import { z } from "zod";
import { UserDatabaseConnector } from "~/server/userDatabaseConnector";
import { createReactAgent } from "@langchain/langgraph/prebuilt";
import { tool } from "@langchain/core/tools";
import {
  querySchema,
  questionConditionsSchema,
} from "~/server/userDatabaseConnector/types";
import { formatAllConditions } from "../index";
import type { DBQuery, Operation } from "../types";
import type {
  TableConditions,
  DateCondition,
  QuestionConditions,
} from "~/server/userDatabaseConnector/types";
import type { Schema } from "~/server/db/schema";
import type { JsonColumnSchema } from "~/server/userDatabaseConnector/types";
import { tryCatch } from "~/server/api/utils/tryCatch";
import { getAIModel } from "~/server/agents/utils/getAIModel";
import { stringArrayToZodEnum } from "../../utils/zodHelpers";
import { whereConditionDocs } from "../utils/whereDocs";
import { buildTableSchemaStringFromTableSchema } from "../utils/schemaFormatters";
import { generateJoinedTables } from "../utils/tableRelations";
import { getPrompt } from "../../utils/getPrompt";
import type { BaseMessage, ToolMessage } from "@langchain/core/messages";
import assert from "assert";
import { buildConditionsForTable } from "../utils/buildConditionsForTable";

interface RequestParams {
  question: string;
  tableName: string;
  operation: Operation;
  operationParams: Record<string, unknown>;
  schema: Schema;
  tableSchema: Schema[number];
  dateCondition: DateCondition;
  tableConditions: TableConditions;
  organisationName: string;
  requestContext: Record<string, string | number>;
  connectorUrl: string;
  connectorSigningKey: string;
}

export const scalarCond = (
  column: string,
  operator: string,
  value:
    | string
    | number
    | boolean
    | string[]
    | number[]
    | null
    | Record<string, never>
) => ({
  [column]: { [operator]: value },
});

export const relationCond = (
  relation: string,
  filterOption: string,
  column: string,
  operator: string,
  value: string | number | boolean | string[] | number[] | null
) => ({
  [relation]: { [filterOption]: { [column]: { [operator]: value } } },
});

async function getDistinctValuesByEditDistance({
  columnName,
  tableName,
  targetString,
  tableConditions,
  jsonColumnSchema,
  dateCondition,
  connectorUrl,
  connectorSigningKey,
}: {
  columnName: string;
  tableName: string;
  targetString: string;
  tableConditions: TableConditions;
  jsonColumnSchema: JsonColumnSchema | null;
  dateCondition: DateCondition;
  connectorUrl: string;
  connectorSigningKey: string;
}) {
  const query: DBQuery = {
    table: tableName,
    operation: "findDistinctByEditDistance",
    operationParameters: {
      column: columnName,
      compareString: targetString,
    },
  };

  const queryWithConditions = formatAllConditions(
    query,
    tableConditions,
    null,
    dateCondition
  );

  if (jsonColumnSchema && jsonColumnSchema.length > 0) {
    queryWithConditions.jsonColumnSchema = jsonColumnSchema;
  }

  const parsedQuery = querySchema.parse(queryWithConditions);

  const userDatabaseConnector = new UserDatabaseConnector({
    baseURL: connectorUrl,
    signingSecret: connectorSigningKey,
  });

  const { data: queryResponse } = await tryCatch(
    userDatabaseConnector.query(parsedQuery)
  );
  // We ignore the error here and later just let the LLM use any string
  // Query will error if more than 500 distinct values are returned
  if (!queryResponse) {
    return false;
  }

  const responseType = z.array(z.string());
  const parsedResponse = responseType.parse(queryResponse.data);
  const distinctValues = parsedResponse;

  return distinctValues;
}

interface GetCorrectedValueForStringColumnInput {
  columnName: string;
  value: string;
  tableName: string;
  tableConditions: TableConditions;
  jsonColumnSchema: JsonColumnSchema | null;
  dateCondition: DateCondition;
  connectorUrl: string;
  connectorSigningKey: string;
  question: string;
}

async function getCorrectedValueForStringColumn({
  columnName,
  value,
  tableName,
  tableConditions,
  jsonColumnSchema,
  dateCondition,
  connectorUrl,
  connectorSigningKey,
  question,
}: GetCorrectedValueForStringColumnInput) {
  const closeMatches = await getDistinctValuesByEditDistance({
    columnName,
    tableName,
    targetString: value,
    tableConditions,
    jsonColumnSchema,
    dateCondition,
    connectorUrl,
    connectorSigningKey,
  });

  // If the query fails, we ignore the error and just let the LLM use any string
  if (closeMatches === false) {
    return value;
  }

  const caseInsensitiveMatch = closeMatches.find(
    (match) => match.toLowerCase() === value.toLowerCase()
  );
  if (caseInsensitiveMatch) {
    return caseInsensitiveMatch;
  }

  const model = getAIModel("azure:gpt-4.1");
  const prompt = await getPrompt("string_value_selector");
  const correctedStringSchema = model.withStructuredOutput(
    z.object({
      correctedValue: stringArrayToZodEnum(closeMatches),
    }),
    {
      method: "jsonSchema",
      strict: true,
    }
  );
  const { correctedValue } = await prompt.pipe(correctedStringSchema).invoke({
    question: question,
    columnName: columnName,
    value: value,
  });
  return correctedValue;
}

export async function questionWhereConditionAgent(params: RequestParams) {
  const llm = getAIModel("azure:gpt-4.1");

  /************* 1. shared array for filters ****************************/
  const filters: unknown[] = [];

  /************* 2. Data Prep *****************************************/
  const tableConditionColumns =
    params.tableConditions?.map((condition) => condition.column) ?? [];
  const columns = params.tableSchema.columns;
  const relations = params.tableSchema?.outwardRelations ?? [];
  const computedColumnNames =
    params.tableSchema?.computedColumns.map((col) => col.name) ?? [];
  const numericalColumnNames = columns
    .filter((column) => ["number"].includes(column.type))
    .map((column) => column.name)
    .concat(computedColumnNames)
    .filter((name) => !tableConditionColumns.includes(name));
  const stringColumnNames = columns
    .filter((column) => ["string"].includes(column.type))
    .map((column) => column.name)
    .filter((name) => !tableConditionColumns.includes(name));
  const booleanColumnNames = columns
    .filter((column) => ["boolean"].includes(column.type))
    .map((column) => column.name)
    .filter((name) => !tableConditionColumns.includes(name));
  const dateColumnNames = columns
    .filter((column) => ["DateTime"].includes(column.type))
    .map((column) => column.name)
    .filter((name) => !tableConditionColumns.includes(name));
  const relationToOneNames = relations
    .filter((relation) => relation.isList === false)
    .map((relation) => relation.name);
  const relationToManyNames = relations
    .filter((relation) => relation.isList === true)
    .map((relation) => relation.name);

  const relatedTables = generateJoinedTables(
    params.schema,
    params.tableName,
    1
  ).uniqueTableNames;
  const relatedTablesSchemasString = relatedTables
    .filter((tableName) => tableName !== params.tableName) // Exclude the starting table
    .map((tableName) => {
      const tableSchema = params.schema.find(
        (table) => table.name === tableName
      );
      if (!tableSchema) {
        throw new Error(`Table ${tableName} not found in schema`);
      }
      return buildTableSchemaStringFromTableSchema(tableSchema);
    })
    .join(",\n");

  /************* 2. BUILDER TOOLS ************************************/
  const buildTools = () => {
    const tools = [];

    if (columns.length > 0) {
      const generateNullCondition = tool(
        async (input: {
          column: string;
          operation: "equals" | "not";
          value: null;
        }) => {
          const { column, operation, value } = input;
          const filter = scalarCond(column, operation, value);
          filters.push(filter);
          return filter;
        },
        {
          name: "generateNullCondition",
          description: "Add a null predicate on a column.",
          schema: z.object({
            column: stringArrayToZodEnum(columns.map((col) => col.name)),
            operation: z.enum(["equals", "not"]),
            value: z.null(),
          }),
        }
      );
      tools.push(generateNullCondition);
    }

    if (dateColumnNames.length > 0) {
      const generateDateCondition = tool(
        async (input: {
          column: string;
          operator: "equals" | "lte" | "gte" | "lt" | "gt";
          value: string;
        }) => {
          const { column, operator, value } = input;
          const filter = scalarCond(column, operator, value);
          filters.push(filter);
          return filter;
        },
        {
          name: "generateDateCondition",
          description: "Add a date predicate on a column of type date.",
          schema: z.object({
            column: stringArrayToZodEnum(dateColumnNames),
            operator: z.enum(["gte", "lte", "gt", "lt", "equals"]),
            value: z
              .string()
              .describe(
                "The value to filter by (ISO 8601) date. YYYY-MM-DDTHH:mm:ss.sssZ"
              ),
          }),
        }
      );
      tools.push(generateDateCondition);
    }

    if (numericalColumnNames.length > 0) {
      const generateNumericalCondition = tool(
        async (input: {
          column: string;
          operator: "equals" | "not" | "lt" | "lte" | "gt" | "gte" | "in";
          value: number | number[];
        }) => {
          const { column, operator, value } = input;
          if (Array.isArray(value)) {
            const filter = scalarCond(column, operator, value);
            filters.push(filter);
            return filter;
          }
          const filter = scalarCond(column, operator, value);
          filters.push(filter);
          return filter;
        },
        {
          name: "generateNumericalCondition",
          description: "Add a number predicate on a column of type number.",
          schema: z.object({
            column: stringArrayToZodEnum(numericalColumnNames),
            operator: z.enum(["equals", "not", "lt", "lte", "gt", "gte", "in"]),
            value: z.union([z.number(), z.array(z.number())]),
          }),
        }
      );
      tools.push(generateNumericalCondition);
    }

    if (booleanColumnNames.length > 0) {
      const generateBooleanCondition = tool(
        async (input: {
          column: string;
          operator: "equals" | "not";
          value: boolean;
        }) => {
          const { column, operator, value } = input;
          const filter = scalarCond(column, operator, value);
          filters.push(filter);
          return filter;
        },
        {
          name: "generateBooleanCondition",
          description: "Add a boolean predicate on a column of type boolean.",
          schema: z.object({
            column: stringArrayToZodEnum(booleanColumnNames),
            operator: z.enum(["equals", "not"]),
            value: z.boolean(),
          }),
        }
      );
      tools.push(generateBooleanCondition);
    }

    if (stringColumnNames.length > 0) {
      const generateStringCondition = tool(
        async (input: {
          column: string;
          operator:
            | "equals"
            | "not"
            | "in"
            | "contains"
            | "contains_insensitive";
          value: string | string[];
        }) => {
          const { column, operator, value } = input;
          if (
            Array.isArray(value) ||
            operator === "contains" ||
            operator === "contains_insensitive"
          ) {
            const filter = scalarCond(column, operator, value);
            filters.push(filter);
            return filter;
          }
          const correctedValue = await getCorrectedValueForStringColumn({
            columnName: column,
            value: value,
            tableName: params.tableName,
            tableConditions: params.tableConditions,
            jsonColumnSchema: null,
            dateCondition: params.dateCondition,
            connectorUrl: params.connectorUrl,
            connectorSigningKey: params.connectorSigningKey,
            question: params.question,
          });
          const filter = scalarCond(column, operator, correctedValue);
          filters.push(filter);
          return filter;
        },
        {
          name: "generateStringCondition",
          description: "Add a string predicate on a column of type string.",
          schema: z.object({
            column: stringArrayToZodEnum(stringColumnNames),
            operator: z.enum([
              "equals",
              "not",
              "in",
              "contains",
              "contains_insensitive",
            ]),
            value: z.union([z.string(), z.array(z.string())]),
          }),
        }
      );
      tools.push(generateStringCondition);
    }

    if (relationToOneNames.length > 0) {
      const generateRelationToOneCondition = tool(
        async (input: {
          relation: string;
          filterOption: "is" | "isNot";
          column: string;
          operator:
            | "equals"
            | "not"
            | "lt"
            | "lte"
            | "gt"
            | "gte"
            | "in"
            | "contains"
            | "contains_insensitive";
          value: string | number | boolean | null;
        }) => {
          const { relation, filterOption, column, operator, value } = input;
          if (
            Array.isArray(value) ||
            operator === "contains" ||
            operator === "contains_insensitive"
          ) {
            const filter = relationCond(
              relation,
              filterOption,
              column,
              operator,
              value
            );
            filters.push(filter);
            return filter;
          }
          const relatedTableName = params.tableSchema.outwardRelations.find(
            (rel) => rel.name === relation
          )?.targetTable.name;
          assert(relatedTableName, "Related table name is not defined");
          const relationSchema = params.schema.find(
            (table) => table.name === relatedTableName
          );
          assert(
            relationSchema,
            `Table ${relatedTableName} not found in schema`
          );
          const relationColumn = relationSchema.columns.find(
            (col) => col.name === column
          );
          assert(
            relationColumn,
            `Column ${column} not found in relation ${relation} schema`
          );
          if (relationColumn.type === "string") {
            const correctedValue = await getCorrectedValueForStringColumn({
              columnName: column,
              value: value as string,
              tableName: relatedTableName,
              tableConditions: buildConditionsForTable(
                relationSchema,
                params.requestContext
              ),
              jsonColumnSchema: null,
              dateCondition: null,
              connectorUrl: params.connectorUrl,
              connectorSigningKey: params.connectorSigningKey,
              question: params.question,
            });
            const filter = relationCond(
              relation,
              filterOption,
              column,
              operator,
              correctedValue
            );
            filters.push(filter);
            return filter;
          }

          const filter = relationCond(
            relation,
            filterOption,
            column,
            operator,
            value
          );
          filters.push(filter);
          return filter;
        },
        {
          name: "generateRelationToOneCondition",
          description:
            "Add a nested relation predicate such as { post: { is: { likes: { gt: 100 } } } }",
          schema: z.object({
            relation: stringArrayToZodEnum(relationToOneNames),
            filterOption: z.enum(["is", "isNot"]),
            column: z.string(),
            operator: z.enum([
              "equals",
              "not",
              "lt",
              "lte",
              "gt",
              "gte",
              "in",
              "contains",
              "contains_insensitive",
            ]),
            value: z.union([
              z.string(),
              z.number(),
              z.boolean(),
              z.array(z.string()),
              z.array(z.number()),
              z.null(),
            ]),
          }),
        }
      );
      tools.push(generateRelationToOneCondition);
      const generateRelationToOneAbsentCondition = tool(
        async (input: {
          relation: string;
          filterOption: "is";
          value: Record<string, never>;
        }) => {
          const { relation, filterOption, value } = input;
          const filter = scalarCond(relation, filterOption, value);
          filters.push(filter);
          return filter;
        },
        {
          name: "generateRelationToOneAbsentCondition",
          description:
            "Add a nested relation predicate such as { post: { is: {} } }",
          schema: z.object({
            relation: stringArrayToZodEnum(relationToOneNames),
            filterOption: z.enum(["is"]),
            value: z.object({}),
          }),
        }
      );
      tools.push(generateRelationToOneAbsentCondition);
    }

    if (relationToManyNames.length > 0) {
      const generateRelationToManyCondition = tool(
        async (input: {
          relation: string;
          filterOption: "some" | "every" | "none";
          column: string;
          operator:
            | "equals"
            | "not"
            | "lt"
            | "lte"
            | "gt"
            | "gte"
            | "in"
            | "contains"
            | "contains_insensitive";
          value: string | number | boolean;
        }) => {
          const { relation, filterOption, column, operator, value } = input;
          if (
            Array.isArray(value) ||
            operator === "contains" ||
            operator === "contains_insensitive"
          ) {
            const filter = relationCond(
              relation,
              filterOption,
              column,
              operator,
              value
            );
            filters.push(filter);
            return filter;
          }
          const relatedTableName = params.tableSchema.outwardRelations.find(
            (rel) => rel.name === relation
          )?.targetTable.name;
          assert(relatedTableName, "Related table name is not defined");
          const relationSchema = params.schema.find(
            (table) => table.name === relatedTableName
          );
          assert(
            relationSchema,
            `Table ${relatedTableName} not found in schema`
          );
          const relationColumn = relationSchema.columns.find(
            (col) => col.name === column
          );
          assert(
            relationColumn,
            `Column ${column} not found in relation ${relation} schema`
          );
          if (relationColumn.type === "string") {
            const correctedValue = await getCorrectedValueForStringColumn({
              columnName: column,
              value: value as string,
              tableName: relatedTableName,
              tableConditions: buildConditionsForTable(
                relationSchema,
                params.requestContext
              ),
              jsonColumnSchema: null,
              dateCondition: null,
              connectorUrl: params.connectorUrl,
              connectorSigningKey: params.connectorSigningKey,
              question: params.question,
            });
            const filter = relationCond(
              relation,
              filterOption,
              column,
              operator,
              correctedValue
            );
            filters.push(filter);
            return filter;
          }

          const filter = relationCond(
            relation,
            filterOption,
            column,
            operator,
            value
          );
          filters.push(filter);
          return filter;
        },
        {
          name: "generateRelationToManyCondition",
          description:
            "Add a nested relation predicate such as { orders: { every: { revenue: { lt: 25 } } } }",
          schema: z.object({
            relation: stringArrayToZodEnum(relationToManyNames),
            filterOption: z.enum(["some", "every", "none"]),
            column: z.string(),
            operator: z.enum([
              "equals",
              "not",
              "lt",
              "lte",
              "gt",
              "gte",
              "in",
              "contains",
              "contains_insensitive",
            ]),
            value: z.union([
              z.string(),
              z.number(),
              z.boolean(),
              z.array(z.string()),
              z.array(z.number()),
              z.null(),
            ]),
          }),
        }
      );
      tools.push(generateRelationToManyCondition);
      const generateRelationToManyAbsentCondition = tool(
        async (input: {
          relation: string;
          filterOption: "none";
          value: Record<string, never>;
        }) => {
          const { relation, filterOption, value } = input;
          const filter = scalarCond(relation, filterOption, value);
          filters.push(filter);
          return filter;
        },
        {
          name: "generateRelationToManyAbsentCondition",
          description:
            "Add a nested relation predicate such as { orders: { none: {} } }",
          schema: z.object({
            relation: stringArrayToZodEnum(relationToManyNames),
            filterOption: z.enum(["none"]),
            value: z.object({}),
          }),
        }
      );
      tools.push(generateRelationToManyAbsentCondition);
    }

    return tools;
  };

  const finalFilterGenerator = tool(
    async () => {
      if (filters.length === 0) {
        return null;
      } else {
        const prompt = await getPrompt("where_condition_agent_formatter");
        const response = llm.withStructuredOutput(
          z.object({
            AND: z.array(z.object({}).passthrough()),
          }),
          {
            method: "function_calling",
            strict: false,
          }
        );
        const result = await prompt.pipe(response).invoke({
          filters: filters
            .map((filter) => JSON.stringify(filter, null, 2))
            .join(",\n"),
          question: params.question,
        });
        return [result, result];
      }
    },
    {
      name: "finalFilterGenerator",
      description:
        "Call this to END. Combines all individual conditions into a valid filter object or returns null.",
      schema: z.object({}),
      returnDirect: true,
      responseFormat: "content_and_artifact",
    }
  );

  /************* 3. THE AGENT ****************************************/

  const agentPrompt = await getPrompt("where_condition_agent");

  const agentPromptFormatted = (await agentPrompt.invoke({
    whereConditionDocs: whereConditionDocs,
    organisationName: params.organisationName,
    tableName: params.tableName,
    operation: params.operation,
    operationParams: JSON.stringify(params.operationParams, null, 2),
    date: new Date().toISOString(),
    tableConditions: JSON.stringify(params.tableConditions, null, 2),
    tableSchema: buildTableSchemaStringFromTableSchema(params.tableSchema),
    relatedTablesSchemas: relatedTablesSchemasString,
  })) as Record<"messages", BaseMessage[]>;

  const agent = createReactAgent({
    llm,
    tools: [...buildTools(), finalFilterGenerator],
    prompt: agentPromptFormatted.messages[0],
  });

  const result = await agent.invoke({
    messages: [{ role: "user", content: params.question }],
  });

  const finalToolMessage = result.messages.at(-1) as ToolMessage;

  const finalMessageContent =
    (finalToolMessage?.artifact as QuestionConditions) ?? null;

  const parsedQuestionConditions =
    questionConditionsSchema.parse(finalMessageContent);

  return parsedQuestionConditions;
}
