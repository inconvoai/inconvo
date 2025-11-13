import { z } from "zod";
import type { FindDistinctQuery } from "~/server/userDatabaseConnector/types";

export interface FindDistinctValidatorContext {
  baseTable: string;
  selectableColumns: string[];
}

export interface FindDistinctValidResult {
  status: "valid";
  result: FindDistinctQuery["operationParameters"];
}

export interface FindDistinctInvalidIssue {
  path: string;
  message: string;
  code: string;
}

export interface FindDistinctInvalidResult {
  status: "invalid";
  issues: FindDistinctInvalidIssue[];
}

export type FindDistinctValidationResult =
  | FindDistinctValidResult
  | FindDistinctInvalidResult;

export function buildFindDistinctToolZodSchema() {
  return z.object({
    column: z
      .string()
      .min(1)
      .describe(
        "Fully qualified column (table.column) to return distinct values for. Only columns from the base table are allowed; include computed columns using the same format."
      ),
  });
}

export function validateFindDistinctCandidate(
  candidate: unknown,
  ctx: FindDistinctValidatorContext
): FindDistinctValidationResult {
  const schema = buildFindDistinctToolZodSchema();
  const parsed = schema.safeParse(candidate);

  if (!parsed.success) {
    return {
      status: "invalid",
      issues: parsed.error.issues.map((issue) => ({
        path: issue.path.join(".") || "<root>",
        message: issue.message,
        code: issue.code,
      })),
    };
  }

  const column = parsed.data.column;
  const [table, columnName] = splitQualifiedColumn(column);

  if (!table || !columnName) {
    return {
      status: "invalid",
      issues: [
        {
          path: "column",
          message: `Column ${column} must be formatted as table.column.`,
          code: "invalid_format",
        },
      ],
    };
  }

  if (table !== ctx.baseTable) {
    return {
      status: "invalid",
      issues: [
        {
          path: "column",
          message: `Column ${column} must reference the base table ${ctx.baseTable}.`,
          code: "invalid_table",
        },
      ],
    };
  }

  const allowed = ctx.selectableColumns.includes(column);
  if (!allowed) {
    return {
      status: "invalid",
      issues: [
        {
          path: "column",
          message: `Column ${column} is not selectable for findDistinct.`,
          code: "invalid_column",
        },
      ],
    };
  }

  return {
    status: "valid",
    result: {
      column,
    },
  };
}

function splitQualifiedColumn(column: string): [string | null, string | null] {
  const separatorIndex = column.indexOf(".");
  if (separatorIndex === -1) {
    return [null, null];
  }
  const tablePart = column.slice(0, separatorIndex);
  const columnPart = column.slice(separatorIndex + 1);
  if (!tablePart || !columnPart) {
    return [null, null];
  }
  return [tablePart, columnPart];
}
