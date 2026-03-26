import { describe, expect, it } from "vitest";
import { AIMessage, HumanMessage, ToolMessage } from "@langchain/core/messages";
import {
  extractToolTurnMessages,
  findMostRecentDatabaseRetrieverValidationError,
  formatDatabaseRetrieverValidationFeedback,
  formatDatabaseContext,
} from "./index";

describe("extractToolTurnMessages", () => {
  it("keeps all tool outputs for a selected assistant tool turn", () => {
    const assistantMessage = new AIMessage({
      content: "Fetching schema and querying the database.",
      tool_calls: [
        {
          id: "call_schema",
          name: "getSchemasForTables",
          args: { database: "shop", tables: ["orders"] },
          type: "tool_call",
        },
        {
          id: "call_db",
          name: "databaseRetriever",
          args: {
            database: "shop",
            query: {
              table: "orders",
              operation: "count",
              operationParameters: {
                joins: null,
                count: ["orders.id"],
                countDistinct: null,
              },
              questionConditions: null,
            },
          },
          type: "tool_call",
        },
      ],
    });

    const messages = [
      new HumanMessage("How many orders do we have?"),
      assistantMessage,
      new ToolMessage({
        name: "getSchemasForTables",
        content: "orders schema",
        tool_call_id: "call_schema",
      }),
      new ToolMessage({
        name: "databaseRetriever",
        content: JSON.stringify({ query: { sql: "select count(*)", params: [] } }),
        tool_call_id: "call_db",
      }),
    ];

    expect(extractToolTurnMessages(messages, ["databaseRetriever"])).toEqual([
      assistantMessage,
      messages[2],
      messages[3],
    ]);
  });

  it("does not keep invalid tool calls with no executed tool output", () => {
    const invalidAssistantMessage = new AIMessage({
      content: "Trying a malformed query draft.",
      invalid_tool_calls: [
        {
          id: "call_invalid",
          name: "databaseRetriever",
          args: "{not-json",
          error: "Invalid JSON arguments",
          type: "invalid_tool_call",
        },
      ],
    });

    const messages = [
      new HumanMessage("How many orders do we have?"),
      invalidAssistantMessage,
    ];

    expect(extractToolTurnMessages(messages, ["databaseRetriever"])).toEqual([]);
  });
});

describe("findMostRecentDatabaseRetrieverValidationError", () => {
  it("returns the latest databaseRetriever validation error", () => {
    const messages = [
      new HumanMessage("Show sales by product category."),
      new ToolMessage({
        name: "databaseRetriever",
        tool_call_id: "call_1",
        content: JSON.stringify({
          error: {
            type: "validation",
            stage: "operationParameters",
            message: "Repair the grouped key and retry.",
            issues: [
              {
                path: "groupBy.0",
                code: "invalid_group_by_key",
                message: "Use product.category.",
              },
            ],
          },
          database: "Eval Database",
        }),
      }),
    ];

    expect(findMostRecentDatabaseRetrieverValidationError(messages)).toEqual({
      type: "validation",
      stage: "operationParameters",
      message: "Repair the grouped key and retry.",
      issues: [
        {
          path: "groupBy.0",
          code: "invalid_group_by_key",
          message: "Use product.category.",
        },
      ],
    });
  });

  it("ignores non-validation databaseRetriever tool outputs", () => {
    const messages = [
      new ToolMessage({
        name: "databaseRetriever",
        tool_call_id: "call_1",
        content: JSON.stringify({
          error: {
            type: "execution",
            stage: "execution",
            message: "connector failed",
          },
          database: "Eval Database",
        }),
      }),
    ];

    expect(findMostRecentDatabaseRetrieverValidationError(messages)).toBeNull();
  });
});

describe("formatDatabaseRetrieverValidationFeedback", () => {
  it("keeps repair feedback concise and avoids duplicated retry boilerplate", () => {
    const feedback = formatDatabaseRetrieverValidationFeedback(
      {
        type: "validation",
        stage: "operationParameters",
        message: "Invalid operation parameters.",
        issues: [
          {
            path: "groupBy.1",
            code: "invalid_group_by_key",
            message:
              "Column product.name is not a valid groupBy column. Use exact schema column names only. Valid groupBy columns for product: product.id, product.title, product.category.",
          },
        ],
      },
      2,
    );

    expect(feedback).toContain("Guidance: Invalid operation parameters.");
    expect(feedback).toContain(
      "Do not answer the user yet. Repair the structured query draft and retry `databaseRetriever`. This was failed attempt 2 of 3.",
    );
    expect(feedback.match(/Repair the structured query draft/g)).toHaveLength(1);
    expect(feedback).not.toContain(
      "A validation error is not a final answer.",
    );
  });
});

describe("formatDatabaseContext", () => {
  it("renders connection descriptions with per-table summary lines", () => {
    expect(
      formatDatabaseContext([
        {
          friendlyName: "Eval Database",
          context: "Commerce data for the storefront.",
          connector: {} as never,
          schema: [
            {
              name: "orders",
              schema: "public",
              access: "QUERYABLE",
              context: null,
              summary: "Order records with totals and timestamps.",
              columns: [],
              computedColumns: [],
              outwardRelations: [],
              condition: null,
              accessPolicy: null,
            },
            {
              name: "products",
              schema: "public",
              access: "QUERYABLE",
              context: null,
              summary: null,
              columns: [],
              computedColumns: [],
              outwardRelations: [],
              condition: null,
              accessPolicy: null,
            },
          ],
        },
      ]),
    ).toContain("    - orders: Order records with totals and timestamps.");
    expect(
      formatDatabaseContext([
        {
          friendlyName: "Eval Database",
          context: "Commerce data for the storefront.",
          connector: {} as never,
          schema: [
            {
              name: "products",
              schema: "public",
              access: "QUERYABLE",
              context: null,
              summary: null,
              columns: [],
              computedColumns: [],
              outwardRelations: [],
              condition: null,
              accessPolicy: null,
            },
          ],
        },
      ]),
    ).toContain("    - products");
  });
});
