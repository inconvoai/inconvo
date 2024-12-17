import { z } from "zod";

const columnSchema = z.object({
  name: z.string(),
  type: z.string(),
});

const relationSchema = z.object({
  name: z.string(),
  isList: z.boolean(),
  targetTable: z.string(),
  relationName: z.string().optional(),
  sourceColumns: z.array(z.string()).optional(),
  targetColumns: z.array(z.string()).optional(),
});

const tableSchema = z
  .object({
    name: z.string(),
    columns: z.array(columnSchema),
    relations: z.array(relationSchema).optional(),
  })
  .strict();

const SchemaResponseSchema = z.object({
  tables: z.array(tableSchema),
});

export type SchemaResponse = z.infer<typeof SchemaResponseSchema>;

const answerSchema = z.object({
  type: z.enum(["text", "bar_chart", "line_chart"]),
  message: z.string().describe("The message to display"),
  chartData: z
    .object({
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
    .describe(`Data used to display chart if type is bar_chart.`)
    .optional(),
});

export type answer = z.infer<typeof answerSchema>;
