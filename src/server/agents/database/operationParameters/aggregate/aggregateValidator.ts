import { z } from "zod";
import {
  joinDescriptorSchema,
  type AggregateQuery,
  type JoinPathHop,
} from "~/server/userDatabaseConnector/types";
import type { GeneratedJoinOption } from "../../utils/tableRelations";

const aggregateJoinSchema = joinDescriptorSchema.strip();

type AggregateJoinInput = z.infer<typeof aggregateJoinSchema>;

export interface AggregateColumnMetadata {
  isNumeric: boolean;
  isTemporal: boolean;
  isCountable: boolean;
}

export type AggregateColumnCatalog = Record<
  string,
  Record<string, AggregateColumnMetadata>
>;

export interface AggregateValidatorContext {
  baseTable: string;
  joinOptions: GeneratedJoinOption[];
  columnCatalog: AggregateColumnCatalog;
}

export interface AggregateValidResult {
  status: "valid";
  result: AggregateQuery["operationParameters"];
}

export interface AggregateInvalidIssue {
  path: string;
  message: string;
  code: string;
}

export interface AggregateInvalidResult {
  status: "invalid";
  issues: AggregateInvalidIssue[];
}

export type AggregateValidationResult =
  | AggregateValidResult
  | AggregateInvalidResult;

export function buildAggregateToolZodSchema(ctx: AggregateValidatorContext) {
  const joinDescription =
    ctx.joinOptions.length > 0
      ? [
          "Array of join descriptors required when referencing related-table columns.",
          "Available joins:",
          ...ctx.joinOptions.map(
            (option) =>
              `${option.name} (${option.table}) path=${formatJoinPath(
                option.path
              )}`
          ),
        ].join("\n")
      : "Array of join descriptors (no joins available from this table).";

  return z.object({
    joins: z.array(aggregateJoinSchema).nullable().describe(joinDescription),
    avg: buildAggregateArraySchema(
      "Columns to average. Use fully-qualified names (alias.column) and ensure the alias is present in the joins array when referencing related tables. Set to null when unused."
    ),
    sum: buildAggregateArraySchema(
      "Columns to sum. Use fully-qualified names (alias.column); numeric columns only. Set to null when unused."
    ),
    min: buildAggregateArraySchema(
      "Columns to take MIN over. Numeric and temporal columns are supported. Use fully-qualified names; include joins when referencing related tables."
    ),
    max: buildAggregateArraySchema(
      "Columns to take MAX over. Numeric and temporal columns are supported. Use fully-qualified names; include joins when referencing related tables."
    ),
    count: buildAggregateArraySchema(
      "Columns to count non-null rows for. Use fully-qualified names. Set to null when unused."
    ),
    median: buildAggregateArraySchema(
      "Columns to compute median for (numeric only). Use fully-qualified names. Set to null when unused."
    ),
  });
}

export function validateAggregateCandidate(
  candidate: unknown,
  ctx: AggregateValidatorContext
): AggregateValidationResult {
  const schema = buildAggregateToolZodSchema(ctx);
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

  const data = parsed.data;
  const issues: AggregateInvalidIssue[] = [];

  const validatedJoins = validateJoins(data.joins ?? null, ctx, issues);

  const joinAliasSet = new Set<string>([ctx.baseTable]);
  validatedJoins?.forEach((join) => {
    if (join.name) {
      joinAliasSet.add(join.name);
    }
    joinAliasSet.add(join.table);
  });

  const checkColumns = (
    columns: string[] | null | undefined,
    pathPrefix: string,
    predicate: (metadata: AggregateColumnMetadata) => boolean,
    errorCode: string,
    errorMessage: (column: string) => string
  ) => {
    if (!columns) return;

    columns.forEach((column, index) => {
      const resolution = resolveColumnReference(column, ctx);
      if (!resolution.success) {
        issues.push({
          path: `${pathPrefix}.${index}`,
          message: resolution.error.message,
          code: resolution.error.code,
        });
        return;
      }

      const reference = resolution.reference;
      if (reference.scope === "join" && !joinAliasSet.has(reference.alias)) {
        issues.push({
          path: `${pathPrefix}.${index}`,
          message: `Join alias ${reference.alias} must appear in the joins array.`,
          code: "missing_join",
        });
      }

      if (!predicate(reference.metadata)) {
        issues.push({
          path: `${pathPrefix}.${index}`,
          message: errorMessage(column),
          code: errorCode,
        });
      }
    });
  };

  const isNumeric = (meta: AggregateColumnMetadata) => meta.isNumeric;
  const isMinMaxSupported = (meta: AggregateColumnMetadata) =>
    meta.isNumeric || meta.isTemporal;
  const isCountable = (meta: AggregateColumnMetadata) => meta.isCountable;

  checkColumns(
    data.avg ?? null,
    "avg",
    isNumeric,
    "non_numeric",
    (column) => `Column ${column} is not numeric and cannot be averaged.`
  );
  checkColumns(
    data.sum ?? null,
    "sum",
    isNumeric,
    "non_numeric",
    (column) => `Column ${column} is not numeric and cannot be summed.`
  );
  checkColumns(
    data.median ?? null,
    "median",
    isNumeric,
    "non_numeric",
    (column) =>
      `Column ${column} is not numeric and cannot have a median calculated.`
  );
  checkColumns(
    data.min ?? null,
    "min",
    isMinMaxSupported,
    "unsupported_min_column",
    (column) =>
      `Column ${column} is not numeric or temporal and cannot be used for MIN.`
  );
  checkColumns(
    data.max ?? null,
    "max",
    isMinMaxSupported,
    "unsupported_max_column",
    (column) =>
      `Column ${column} is not numeric or temporal and cannot be used for MAX.`
  );
  checkColumns(
    data.count ?? null,
    "count",
    isCountable,
    "unsupported_count_column",
    (column) => `Column ${column} cannot be counted.`
  );

  if (issues.length > 0) {
    return {
      status: "invalid",
      issues,
    };
  }

  return {
    status: "valid",
    result: {
      joins: validatedJoins ?? null,
      avg: normalizeList(data.avg),
      sum: normalizeList(data.sum),
      min: normalizeList(data.min),
      max: normalizeList(data.max),
      count: normalizeList(data.count),
      median: normalizeList(data.median),
    },
  };
}

function normalizeList(list: string[] | null | undefined) {
  if (!list || list.length === 0) {
    return null;
  }
  return list;
}

function buildAggregateArraySchema(description: string) {
  return z.array(z.string().min(1)).min(1).nullable().describe(description);
}

interface ResolvedColumnReference {
  scope: "base" | "join";
  alias: string;
  column: string;
  metadata: AggregateColumnMetadata;
}

interface ColumnReferenceError {
  code: string;
  message: string;
}

type ColumnReferenceResolution =
  | { success: true; reference: ResolvedColumnReference }
  | { success: false; error: ColumnReferenceError };

function resolveColumnReference(
  rawColumn: string,
  ctx: AggregateValidatorContext
): ColumnReferenceResolution {
  const [aliasPart, columnPart] = splitAliasAndColumn(rawColumn);
  if (!aliasPart || !columnPart) {
    return {
      success: false,
      error: {
        code: "invalid_format",
        message: `Column ${rawColumn} must be qualified as alias.column.`,
      },
    };
  }

  const aliasCatalog = ctx.columnCatalog[aliasPart];
  if (!aliasCatalog) {
    return {
      success: false,
      error: {
        code: aliasPart.includes(".") ? "invalid_alias_chain" : "invalid_alias",
        message: aliasPart.includes(".")
          ? `Alias ${aliasPart} refers to an unsupported nested path. Use the join alias supplied in the joins array.`
          : `Alias ${aliasPart} is not valid for aggregation.`,
      },
    };
  }

  const metadata = aliasCatalog[columnPart];
  if (!metadata) {
    return {
      success: false,
      error: {
        code: "invalid_column",
        message: `Column ${rawColumn} is not available for aggregation.`,
      },
    };
  }

  return {
    success: true,
    reference: {
      scope: aliasPart === ctx.baseTable ? "base" : "join",
      alias: aliasPart,
      column: columnPart,
      metadata,
    },
  };
}

function splitAliasAndColumn(column: string): [string, string | null] {
  const lastDot = column.lastIndexOf(".");
  if (lastDot === -1) {
    return [column, null];
  }
  const alias = column.slice(0, lastDot);
  const col = column.slice(lastDot + 1);
  return [alias, col || null];
}

function validateJoins(
  joins: AggregateJoinInput[] | null,
  ctx: AggregateValidatorContext,
  issues: AggregateInvalidIssue[]
): AggregateQuery["operationParameters"]["joins"] {
  if (!joins || joins.length === 0) {
    return undefined;
  }

  const optionsByKey = new Map(
    ctx.joinOptions.map((option) => [joinPathKey(option.path), option])
  );
  const seenAliases = new Set<string>([ctx.baseTable]);

  const validated = joins
    .map((join, index) => {
      const key = joinPathKey(join.path);
      const option = optionsByKey.get(key);
      if (!option) {
        issues.push({
          path: `joins.${index}.path`,
          message: "Join path does not match any available relation path.",
          code: "invalid_join_path",
        });
        return null;
      }

      const alias = join.name ?? option.name;
      if (seenAliases.has(alias)) {
        issues.push({
          path: `joins.${index}.name`,
          message:
            alias === ctx.baseTable
              ? `Join alias ${alias} conflicts with the base table alias.`
              : `Join alias ${alias} already provided.`,
          code: "duplicate_join_alias",
        });
        return null;
      }

      if (join.table !== option.table) {
        issues.push({
          path: `joins.${index}.table`,
          message: `Join table must be ${option.table} for this path.`,
          code: "invalid_join_table",
        });
        return null;
      }

      seenAliases.add(alias);
      seenAliases.add(option.table);

      return {
        table: option.table,
        name: alias,
        path: option.path,
        joinType: join.joinType,
      } satisfies NonNullable<
        AggregateQuery["operationParameters"]["joins"]
      >[number];
    })
    .filter((entry): entry is NonNullable<typeof entry> => entry !== null);

  return validated.length > 0 ? validated : undefined;
}

function joinPathKey(path: JoinPathHop[]) {
  return path
    .map((hop) => `${hop.source.join(",")}=>${hop.target.join(",")}`)
    .join("|");
}

function formatJoinPath(path: JoinPathHop[]) {
  return path
    .map((hop) => `[${hop.source.join(", ")}] -> [${hop.target.join(", ")}]`)
    .join(" | ");
}
