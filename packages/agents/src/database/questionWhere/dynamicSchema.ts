import { z } from "zod";
import type { Schema } from "@repo/types";
import {
  NUMERIC_LOGICAL_TYPES,
  isActiveEnumColumn,
  type SchemaColumnValueEnum,
} from "@repo/types";
import type { QuestionConditions } from "@repo/types";

type SchemaColumn = Schema[number]["columns"][number];

type EnumResolution = {
  map: Map<string, string | number>;
  allowedTokens: string[];
};

function normalizeEnumToken(value: unknown): string | null {
  if (typeof value === "number" && Number.isFinite(value)) {
    return String(value);
  }
  if (typeof value === "string") {
    const trimmed = value.trim();
    if (trimmed.length === 0) return null;
    return trimmed.toLowerCase();
  }
  return null;
}

function buildEnumResolution(valueEnum: SchemaColumnValueEnum): EnumResolution {
  const map = new Map<string, string | number>();
  const allowedLabels = new Set<string>();

  valueEnum.entries
    .filter((entry) => entry.selected !== false)
    .forEach((entry) => {
      // Map both value and label tokens to the canonical value for resolution,
      // but only expose labels in error messages — values are hidden from the agent.
      // Prefix keys to avoid collisions between a value matching another entry's label.
      const valueToken = normalizeEnumToken(entry.value);
      if (valueToken) {
        map.set(`v:${valueToken}`, entry.value);
      }

      const labelToken = normalizeEnumToken(entry.label);
      if (labelToken) {
        map.set(`l:${labelToken}`, entry.value);
        allowedLabels.add(entry.label);
      }
    });

  return {
    map,
    allowedTokens: Array.from(allowedLabels),
  };
}

function buildEnumValueSchema(valueEnum: SchemaColumnValueEnum) {
  const resolution = buildEnumResolution(valueEnum);

  return z.any().transform((input, ctx): string | number => {
    const token = normalizeEnumToken(input);
    if (!token) {
      ctx.addIssue({
        code: z.ZodIssueCode.custom,
        message: `Expected one of: ${resolution.allowedTokens.join(", ")}`,
      });
      return z.NEVER;
    }

    // Try label first (agent typically sends labels), then fall back to raw value
    const resolved = resolution.map.get(`l:${token}`) ?? resolution.map.get(`v:${token}`);
    if (resolved === undefined) {
      ctx.addIssue({
        code: z.ZodIssueCode.custom,
        message: `Value \"${String(input)}\" is not in allowed values`,
      });
      return z.NEVER;
    }

    return resolved;
  });
}

function buildStringColumnCondition(col: string) {
  return z
    .object({
      [col]: z.union([
        z.object({ equals: z.string().or(z.null()) }).strict(),
        z.object({ not: z.string().or(z.null()) }).strict(),
        z.object({ in: z.array(z.string()).min(1) }).strict(),
        z.object({ contains: z.string() }).strict(),
        z.object({ contains_insensitive: z.string() }).strict(),
      ]),
    })
    .strict();
}

function buildNumberColumnCondition(col: string) {
  return z
    .object({
      [col]: z
        .object({
          equals: z.number().or(z.null()).optional(),
          not: z.number().or(z.null()).optional(),
          lt: z.number().optional(),
          lte: z.number().optional(),
          gt: z.number().optional(),
          gte: z.number().optional(),
          in: z.array(z.number()).min(1).optional(),
        })
        .strict()
        .refine(
          (obj) => Object.keys(obj).length > 0,
          "At least one operator required",
        ),
    })
    .strict();
}

function buildBooleanColumnCondition(col: string) {
  return z
    .object({
      [col]: z.union([
        z.object({ equals: z.boolean().or(z.null()) }).strict(),
        z.object({ not: z.boolean().or(z.null()) }).strict(),
      ]),
    })
    .strict();
}

function buildDateColumnCondition(col: string) {
  const isoDate = z
    .string()
    .refine(
      (v) => !Number.isNaN(Date.parse(v)),
      "Expected valid ISO 8601 date string",
    );

  return z
    .object({
      [col]: z
        .object({
          equals: isoDate.or(z.null()).optional(),
          not: isoDate.or(z.null()).optional(),
          lt: isoDate.optional(),
          lte: isoDate.optional(),
          gt: isoDate.optional(),
          gte: isoDate.optional(),
        })
        .strict()
        .refine(
          (obj) => Object.keys(obj).length > 0,
          "At least one operator required",
        ),
    })
    .strict();
}

function buildEnumColumnCondition(col: string, valueEnum: SchemaColumnValueEnum) {
  const enumValueSchema = buildEnumValueSchema(valueEnum);
  return z
    .object({
      [col]: z.union([
        z.object({ equals: z.union([enumValueSchema, z.null()]) }).strict(),
        z.object({ not: z.union([enumValueSchema, z.null()]) }).strict(),
        z.object({ in: z.array(enumValueSchema).min(1) }).strict(),
      ]),
    })
    .strict();
}

function buildColumnConditionSchema(
  columnKey: string,
  columnType: string,
  valueEnum: SchemaColumn["valueEnum"],
) {
  if (isActiveEnumColumn(valueEnum)) {
    return buildEnumColumnCondition(columnKey, valueEnum);
  }
  if (columnType === "string") {
    return buildStringColumnCondition(columnKey);
  }
  if (NUMERIC_LOGICAL_TYPES.has(columnType)) {
    return buildNumberColumnCondition(columnKey);
  }
  if (columnType === "boolean") {
    return buildBooleanColumnCondition(columnKey);
  }
  if (columnType === "DateTime") {
    return buildDateColumnCondition(columnKey);
  }
  return null;
}

function buildScalarSchemasForTable(params: {
  table: Schema[number];
  keyBuilder: (columnName: string) => string;
  includeComputedColumns?: boolean;
}): z.ZodTypeAny[] {
  const schemas: z.ZodTypeAny[] = [];

  (params.table.columns ?? []).forEach((column) => {
    const key = params.keyBuilder(column.name);
    const effectiveType = column.effectiveType ?? column.type;
    const schema = buildColumnConditionSchema(key, effectiveType, column.valueEnum ?? null);
    if (schema) {
      schemas.push(schema);
    }
  });

  if (params.includeComputedColumns !== false) {
    (params.table.computedColumns ?? []).forEach((computedColumn) => {
      const key = params.keyBuilder(computedColumn.name);
      // Computed columns are currently treated as numeric for filtering.
      const schema = buildNumberColumnCondition(key);
      schemas.push(schema);
    });
  }

  return schemas;
}

function unionSchemas(schemas: z.ZodTypeAny[]): z.ZodTypeAny {
  if (schemas.length === 0) {
    return z.never();
  }
  if (schemas.length === 1) {
    return schemas[0]!;
  }
  return z.union(schemas as [z.ZodTypeAny, z.ZodTypeAny, ...z.ZodTypeAny[]]);
}

/**
 * Builds a Zod schema for validating the final question WHERE conditions object
 * produced directly by the model (without per-condition tool calls).
 */
export function createQuestionConditionsDynamicSchema(
  tableSchema: Schema[number],
  fullSchema: Schema,
  tableName: string,
  joinedTableNames?: string[],
): z.ZodType<QuestionConditions> {
  const outwardRelations = tableSchema.outwardRelations ?? [];

  const scalarConditionSchemas = buildScalarSchemasForTable({
    table: tableSchema,
    keyBuilder: (columnName) => `${tableName}.${columnName}`,
  });

  function buildInnerRelationFilterSchema(targetTableName: string) {
    const target = fullSchema.find((table) => table.name === targetTableName);
    if (!target) {
      return z.never();
    }

    const innerSchemas = buildScalarSchemasForTable({
      table: target,
      keyBuilder: (columnName) => columnName,
    });

    return unionSchemas(innerSchemas);
  }

  const relationConditionSchemas: z.ZodTypeAny[] = [];

  outwardRelations.forEach((relation) => {
    const relationName = relation.name;
    const targetTableName = relation.targetTable?.name;
    if (!targetTableName) return;
    const inner = buildInnerRelationFilterSchema(targetTableName);

    if (!relation.isList) {
      relationConditionSchemas.push(
        z.union([
          z
            .object({
              [relationName]: z.object({ is: inner }).strict(),
            })
            .strict(),
          z
            .object({
              [relationName]: z.object({ isNot: inner }).strict(),
            })
            .strict(),
          z
            .object({
              [relationName]: z.object({ is: z.object({}).strict() }).strict(),
            })
            .strict(),
        ]),
      );
      return;
    }

    relationConditionSchemas.push(
      z.union([
        z
          .object({
            [relationName]: z.object({ some: inner }).strict(),
          })
          .strict(),
        z
          .object({
            [relationName]: z.object({ every: inner }).strict(),
          })
          .strict(),
        z
          .object({
            [relationName]: z.object({ none: inner }).strict(),
          })
          .strict(),
        z
          .object({
            [relationName]: z.object({ none: z.object({}).strict() }).strict(),
          })
          .strict(),
      ]),
    );
  });

  const qualifiedColumnSchemas: z.ZodTypeAny[] = [];
  if (joinedTableNames?.length) {
    joinedTableNames.forEach((joinedTableName) => {
      const joinedTable = fullSchema.find((table) => table.name === joinedTableName);
      if (!joinedTable) {
        return;
      }
      qualifiedColumnSchemas.push(
        ...buildScalarSchemasForTable({
          table: joinedTable,
          keyBuilder: (columnName) => `${joinedTableName}.${columnName}`,
        }),
      );
    });
  }

  const filterObjectUnionParts: z.ZodTypeAny[] = [];
  if (scalarConditionSchemas.length) {
    filterObjectUnionParts.push(unionSchemas(scalarConditionSchemas));
  }
  if (relationConditionSchemas.length) {
    filterObjectUnionParts.push(unionSchemas(relationConditionSchemas));
  }
  if (qualifiedColumnSchemas.length) {
    filterObjectUnionParts.push(unionSchemas(qualifiedColumnSchemas));
  }

  const filterUnion = unionSchemas(filterObjectUnionParts);

  const node: z.ZodTypeAny = z.lazy(() => {
    const logicalGroup = z.union([
      z.object({ AND: z.array(node).min(1) }).strict(),
      z.object({ OR: z.array(node).min(1) }).strict(),
    ]);
    return z.union([filterUnion, logicalGroup]);
  });

  return z.union([
    z.null(),
    z.object({ AND: z.array(node).min(1) }).strict(),
  ]) as unknown as z.ZodType<QuestionConditions>;
}

/**
 * Helper to validate a candidate output (unknown) using the dynamic schema.
 * Returns { success: true, data } or { success: false, error } similar to safeParse.
 */
export function validateQuestionConditions(
  candidate: unknown,
  tableSchema: Schema[number],
  fullSchema: Schema,
  tableName: string,
  joinedTableNames?: string[],
) {
  const schema = createQuestionConditionsDynamicSchema(
    tableSchema,
    fullSchema,
    tableName,
    joinedTableNames,
  );
  return schema.safeParse(candidate);
}
