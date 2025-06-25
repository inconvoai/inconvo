import { AzureChatOpenAI } from "@langchain/openai";
import {
  HumanMessage,
  type AIMessage,
  type BaseMessage,
  isAIMessage,
} from "@langchain/core/messages";
import { ToolNode } from "@langchain/langgraph/prebuilt";
import {
  StateGraph,
  type MemorySaver,
  Annotation,
  END,
  START,
} from "@langchain/langgraph";
import {
  type DynamicTool,
  tool,
  type StructuredToolInterface,
} from "@langchain/core/tools";
import { z } from "zod";
import type { PostgresSaver } from "@langchain/langgraph-checkpoint-postgres";
import type { Schema } from "~/server/db/schema";
import { getPrompt } from "../utils/getPrompt";
import { databaseRetrieverAgent } from "../database";
import { Calculator } from "@langchain/community/tools/calculator";
import type { RunnableToolLike } from "@langchain/core/runnables";
import { buildTableSchemaStringFromTableSchema } from "../database/utils/schemaFormatters";
import { stringArrayToZodEnum } from "../utils/zodHelpers";

interface Chart {
  type: "bar" | "line";
  data: {
    label: string;
    value: number;
  }[];
  title: string;
  xLabel: string;
  yLabel: string;
}

type Table = {
  headers: string[];
  rows: [][];
};

interface Answer {
  type: string;
  message: string;
  chart?: Chart;
  table?: Table;
}

interface QuestionAgentParams {
  organisationName: string;
  schema: Schema;
  connectorUrl: string;
  connectorSigningKey: string;
  checkpointer: PostgresSaver | MemorySaver;
}

const messageReducer = (
  x: BaseMessage[] | null,
  y: BaseMessage[] | null
): BaseMessage[] => {
  const emptyArray: BaseMessage[] = [];
  if (!y) {
    return emptyArray;
  } else if (!x) {
    return emptyArray.concat(y);
  }
  if (x && y) {
    return x.concat(y);
  }
  return emptyArray;
};

export async function inconvoAgent(params: QuestionAgentParams) {
  const AgentState = Annotation.Root({
    userQuestion: Annotation<HumanMessage>({
      reducer: (x, y) => y,
      default: () => new HumanMessage(""),
    }),
    // Messages in the current run
    messages: Annotation<BaseMessage[] | null>({
      reducer: (x, y) => messageReducer(x, y),
      default: () => [],
    }),
    // The QA message pairs from previous runs
    chatHistory: Annotation<BaseMessage[]>({
      reducer: (x, y) => y,
      default: () => [],
    }),
    answer: Annotation<Answer>({
      reducer: (x, y) => y,
      default: () => ({
        type: "text",
        message: "",
      }),
    }),
    requestContext: Annotation<Record<string, string | number>>({
      reducer: (x, y) => y,
    }),
    error: Annotation<Record<string, unknown> | undefined>({
      reducer: (x, y) => y,
      default: () => undefined,
    }),
  });

  const tools: (StructuredToolInterface | DynamicTool | RunnableToolLike)[] =
    [];
  const toolNode = new ToolNode(tools);

  const model = new AzureChatOpenAI({
    model: "gpt-4.1",
    deploymentName: "gpt-4.1",
    temperature: 0,
  });

  const tableContext = params.schema
    .filter((table) => table.context)
    .map((table) => `Name: ${table.name},\n Context: ${table.context}`)
    .join("\n");

  function resetState(_state: typeof AgentState.State) {
    // We only have to preserve the chat history from the checkpointer
    // The user question will be overwritten by start node because it the graph
    // Is invoked with a user question
    return {
      messages: null,
      error: undefined,
      answer: {
        type: "text",
        message: "",
      },
    };
  }

  function buildTools(state: typeof AgentState.State) {
    const tables = params.schema.map((table) => table.name);

    const databaseRetriever = tool(
      async (input: { question: string }) => {
        const databaseRetrieverResponse = await (
          await databaseRetrieverAgent({
            userQuestion: input.question,
            organisationName: params.organisationName,
            schema: params.schema,
            requestContext: state.requestContext,
            connectorUrl: params.connectorUrl,
            connectorSigningKey: params.connectorSigningKey,
          })
        ).invoke({});
        const data = databaseRetrieverResponse.databaseResponse;
        return { queries: data };
      },
      {
        name: "databaseRetriever",
        description: "Call this tool to retrieve data from the database.",
        schema: z.object({
          question: z.string().describe("The question to ask the database"),
        }),
      }
    );
    tools.push(databaseRetriever);

    const calculator = new Calculator();
    tools.push(calculator);

    const getSchemasForTables = tool(
      async (input: { tables: string[] }) => {
        const schemaStrings = input.tables.map((tableName) => {
          const table = params.schema.find((s) => s.name === tableName);
          if (!table) {
            throw new Error(`Table ${tableName} not found`);
          }
          return buildTableSchemaStringFromTableSchema(table);
        });
        return schemaStrings.join("\n\n---\n\n");
      },
      {
        name: "getSchemasForTables",
        description: "Get the schemas for a list of tables (one or more).",
        schema: z.object({
          tables: z.array(
            stringArrayToZodEnum(tables).describe("The name of the table")
          ),
        }),
      }
    );
    tools.push(getSchemasForTables);

    return {};
  }

  const shouldContinue = (state: typeof AgentState.State) => {
    const { messages } = state;
    const lastMessage = messages?.at(-1) as AIMessage;
    if (
      isAIMessage(lastMessage) &&
      (!lastMessage.tool_calls || lastMessage.tool_calls.length === 0)
    ) {
      return END;
    } else {
      return "continue";
    }
  };

  async function callModel(state: typeof AgentState.State) {
    const prompt = await getPrompt("question_agent");
    const tables = params.schema.map((table) => table.name);
    const response = await prompt.pipe(model.bindTools(tools)).invoke({
      tables,
      tableContext: tableContext,
      chatHistory: state.chatHistory,
      date: new Date().toISOString(),
      requestContext: state.requestContext,
      userQuestion: state.userQuestion,
      messages: state.messages,
    });
    return { messages: [response] };
  }

  async function formatResponse(state: typeof AgentState.State) {
    const prompt = await getPrompt("question_agent");
    const tables = params.schema.map((table) => table.name);
    let selectedType = "text";
    const outputTypeSelectSchema = model.withStructuredOutput(
      z
        .object({
          type: z.enum(["text", "chart", "table"]),
        })
        .describe("The type of output to display"),
      {
        strict: false,
      }
    );

    const selectedTypeResponse = await prompt
      .pipe(outputTypeSelectSchema)
      .invoke({
        tables,
        tableContext: tableContext,
        chatHistory: state.chatHistory,
        date: new Date().toISOString(),
        requestContext: state.requestContext,
        userQuestion: state.userQuestion,
        messages: state.messages,
      });

    selectedType = selectedTypeResponse.type;
    let outputFormatterSchema = model.withStructuredOutput(
      z
        .object({
          type: z.literal(selectedType),
          message: z
            .string()
            .describe(
              "The message to display. When including durations, convert them to an easy to read human format."
            ),
        })
        .describe("The formatted output"),
      {
        strict: true,
        method: "function_calling",
      }
    );
    if (selectedType === "chart") {
      outputFormatterSchema = model.withStructuredOutput(
        z
          .object({
            type: z.literal(selectedType),
            message: z
              .string()
              .describe(
                "The message to display. When including durations, convert them to an easy to read human format."
              ),
            chart: z
              .object({
                type: z.enum(["bar", "line"]).describe("The type of chart"),
                data: z.array(
                  z.object({
                    label: z.string().describe("The label for the x axis"),
                    value: z.number().describe("The value for the y axis"),
                  })
                ),
                title: z.string().describe("The title of the chart"),
                xLabel: z.string().describe("The label for the x axis"),
                yLabel: z.string().describe("The label for the y axis"),
              })
              .describe(`Data used to display chart if type is bar_chart.`),
          })
          .describe("The formatted output"),
        {
          strict: true,
          method: "function_calling",
        }
      );
    } else if (selectedType === "table") {
      outputFormatterSchema = model.withStructuredOutput(
        z
          .object({
            type: z.literal(selectedType),
            message: z
              .string()
              .describe(
                "The message to display. When including durations, convert them to an easy to read human format."
              ),
            table: z
              .object({
                head: z.array(z.string()).describe("The table column names"),
                body: z
                  .array(z.array(z.string()))
                  .describe(
                    "The table rows, each row is an array of strings, the item in the row array much be the same length as the head array"
                  ),
              })
              .describe(`Data used to display table if type is table.`),
          })
          .describe("The formatted output"),
        {
          strict: true,
          method: "function_calling",
        }
      );
    }
    const response = await prompt.pipe(outputFormatterSchema).invoke({
      tables,
      tableContext: tableContext,
      chatHistory: state.chatHistory,
      date: new Date().toISOString(),
      requestContext: state.requestContext,
      userQuestion: state.userQuestion,
      messages: state.messages,
    });
    return {
      answer: response,
      chatHistory: [...state.chatHistory, ...(state.messages ?? [])],
    };
  }

  const workflow = new StateGraph(AgentState)
    .addNode("reset_state", resetState)
    .addNode("build_tools", buildTools)
    .addNode("question_agent", callModel)
    .addNode("question_agent_tools", toolNode)
    .addNode("format_response", formatResponse)
    .addEdge(START, "reset_state")
    .addEdge("reset_state", "build_tools")
    .addEdge("build_tools", "question_agent")
    .addEdge("question_agent_tools", "question_agent")
    .addConditionalEdges("question_agent", shouldContinue, {
      continue: "question_agent_tools",
      [END]: "format_response",
    });

  const graph = workflow.compile({ checkpointer: params.checkpointer });

  return { graph };
}
