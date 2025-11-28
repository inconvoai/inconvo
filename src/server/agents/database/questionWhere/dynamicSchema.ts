import { z } from "zod";
import type { Schema } from "~/server/db/schema";
import type { QuestionConditions } from "~/server/userDatabaseConnector/types";

/**
 * Builds a Zod schema for validating the final question WHERE conditions object
 * produced directly by the model (without per-condition tool calls).
 *
 * The historical tool-driven architecture generated ONE filter object at a time
 * and then aggregated them into: { AND: [ filterObj, ... ] } | null.
 * Each filter object had exactly one top-level key (a scalar column OR a relation name).
 *
 * Supported filter object shapes (single-key objects):
 *  1. Scalar column condition:
 *       { columnName: { <operator>: <value> } }
 *     where operators depend on the column type:
 *       - string:  equals | not | in | contains | contains_insensitive
 *       - number:  equals | not | lt | lte | gt | gte | in
 *       - boolean: equals | not
 *       - DateTime: equals | lt | lte | gt | gte  (ISO 8601 string)
 *     Additionally, ANY column may use null with operators equals | not
 *       (historic null tool allowed null for any column) — e.g. { col: { equals: null } }.
 *
 *  2. To-one relation condition:
 *       { relationName: { is:   { columnName: { <operator>: <value> } } } }
 *       { relationName: { isNot:{ columnName: { <operator>: <value> } } } }
 *       { relationName: { is: {} } }   // absence of related record
 *
 *  3. To-many relation condition:
 *       { relationName: { some: { columnName: { <operator>: <value> } } } }
 *       { relationName: { every:{ columnName: { <operator>: <value> } } } }
 *       { relationName: { none: { columnName: { <operator>: <value> } } } }
 *       { relationName: { none: {} } }  // absence (zero related records)
 *
 *  Final shape returned / expected from the model:
 *     null  (if no filters)  OR
 *     { AND: [ <filterObject>, ... ] }
 *
 * This dynamic validator enforces:
 *  - Only known columns / relations are allowed.
 *  - Only one operator per column condition object.
 *  - Operator/value type compatibility (including null handling rules).
 *  - Relation filterOptions limited to allowed sets (is|isNot / some|every|none).
 *  - Inner relation filter uses target table's columns & rules.
 */
export function createQuestionConditionsDynamicSchema(
  tableSchema: Schema[number],
  fullSchema: Schema
): z.ZodType<QuestionConditions> {
  const columns = tableSchema.columns ?? [];
  const computedColumns = tableSchema.computedColumns ?? [];
  const outwardRelations = tableSchema.outwardRelations ?? [];

  // Collect scalar columns by type.
  const stringColumns = columns
    .filter((c) => c.type === "string")
    .map((c) => c.name);
  const numberColumns = columns
    .filter((c) => c.type === "number")
    .map((c) => c.name)
    // historical logic: numeric operators also allowed on computed columns
    .concat(computedColumns.map((c) => c.name));
  const booleanColumns = columns
    .filter((c) => c.type === "boolean")
    .map((c) => c.name);
  const dateColumns = columns
    .filter((c) => c.type === "DateTime")
    .map((c) => c.name);

  /** Utility: ensure ISO date */
  const isoDate = z
    .string()
    .refine(
      (v) => !isNaN(Date.parse(v)),
      "Expected valid ISO 8601 date string"
    );

  /** Build operator union for a single string column */
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

  /** Number column */
  function buildNumberColumnCondition(col: string) {
    return z
      .object({
        [col]: z.union([
          z.object({ equals: z.number().or(z.null()) }).strict(),
          z.object({ not: z.number().or(z.null()) }).strict(),
          z.object({ lt: z.number() }).strict(),
          z.object({ lte: z.number() }).strict(),
          z.object({ gt: z.number() }).strict(),
          z.object({ gte: z.number() }).strict(),
          z.object({ in: z.array(z.number()).min(1) }).strict(),
        ]),
      })
      .strict();
  }

  /** Boolean column */
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

  /** DateTime column */
  function buildDateColumnCondition(col: string) {
    return z
      .object({
        [col]: z.union([
          z.object({ equals: isoDate.or(z.null()) }).strict(),
          z.object({ not: isoDate.or(z.null()) }).strict(),
          z.object({ lt: isoDate }).strict(),
          z.object({ lte: isoDate }).strict(),
          z.object({ gt: isoDate }).strict(),
          z.object({ gte: isoDate }).strict(),
        ]),
      })
      .strict();
  }

  /** Combine all scalar column schemas as a union (each filter object targets exactly one column). */
  const scalarConditionSchemas: z.ZodTypeAny[] = [];
  stringColumns.forEach((c) =>
    scalarConditionSchemas.push(buildStringColumnCondition(c))
  );
  numberColumns.forEach((c) =>
    scalarConditionSchemas.push(buildNumberColumnCondition(c))
  );
  booleanColumns.forEach((c) =>
    scalarConditionSchemas.push(buildBooleanColumnCondition(c))
  );
  dateColumns.forEach((c) =>
    scalarConditionSchemas.push(buildDateColumnCondition(c))
  );

  const scalarConditionUnion = scalarConditionSchemas.length
    ? z.union(
        scalarConditionSchemas as [
          z.ZodTypeAny,
          z.ZodTypeAny,
          ...z.ZodTypeAny[],
        ]
      )
    : z.never();

  /** Build inner (single-column) condition union for a relation's target table */
  function buildInnerRelationFilterSchema(targetTableName: string) {
    const target = fullSchema.find((t) => t.name === targetTableName);
    if (!target) {
      // Fallback – no columns => impossible to match; disallow.
      return z.never();
    }
    const tCols = target.columns ?? [];
    const tComputed = target.computedColumns ?? [];
    const tString = tCols.filter((c) => c.type === "string").map((c) => c.name);
    const tNumber = tCols
      .filter((c) => c.type === "number")
      .map((c) => c.name)
      .concat(tComputed.map((c) => c.name));
    const tBoolean = tCols
      .filter((c) => c.type === "boolean")
      .map((c) => c.name);
    const tDate = tCols.filter((c) => c.type === "DateTime").map((c) => c.name);

    const innerSchemas: z.ZodTypeAny[] = [];
    tString.forEach((c) => innerSchemas.push(buildStringColumnCondition(c)));
    tNumber.forEach((c) => innerSchemas.push(buildNumberColumnCondition(c)));
    tBoolean.forEach((c) => innerSchemas.push(buildBooleanColumnCondition(c)));
    tDate.forEach((c) => innerSchemas.push(buildDateColumnCondition(c)));

    if (!innerSchemas.length) return z.never();
    return z.union(
      innerSchemas as [z.ZodTypeAny, z.ZodTypeAny, ...z.ZodTypeAny[]]
    );
  }

  /** Relation condition schemas (to-one + to-many) */
  const relationConditionSchemas: z.ZodTypeAny[] = [];

  outwardRelations.forEach((rel) => {
    const relationName = rel.name;
    const targetTableName = rel.targetTable?.name;
    if (!targetTableName) return;
    const inner = buildInnerRelationFilterSchema(targetTableName);

    if (rel.isList === false) {
      // to-one
      const toOneUnion = z.union([
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
            [relationName]: z.object({ is: z.object({}).strict() }).strict(), // absence
          })
          .strict(),
      ]);
      relationConditionSchemas.push(toOneUnion);
    } else {
      // to-many
      const toManyUnion = z.union([
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
            [relationName]: z.object({ none: z.object({}).strict() }).strict(), // absence
          })
          .strict(),
      ]);
      relationConditionSchemas.push(toManyUnion);
    }
  });

  const filterObjectUnionParts: z.ZodTypeAny[] = [];
  if (scalarConditionSchemas.length)
    filterObjectUnionParts.push(scalarConditionUnion);
  if (relationConditionSchemas.length)
    filterObjectUnionParts.push(
      relationConditionSchemas.length === 1
        ? relationConditionSchemas[0]!
        : (z.union(
            relationConditionSchemas as [
              z.ZodTypeAny,
              z.ZodTypeAny,
              ...z.ZodTypeAny[],
            ]
          ) as z.ZodTypeAny)
    );

  const filterUnion: z.ZodTypeAny = filterObjectUnionParts.length
    ? filterObjectUnionParts.length === 1
      ? filterObjectUnionParts[0]!
      : (z.union(
          filterObjectUnionParts as [
            z.ZodTypeAny,
            z.ZodTypeAny,
            ...z.ZodTypeAny[],
          ]
        ) as z.ZodTypeAny)
    : (z.never() as z.ZodTypeAny);

  // Recursive logical grouping: allow nested { AND: [...] } or { OR: [...] } within the main AND array.
  // We'll keep the root contract: null OR { AND: [...] } for compatibility, but each element inside can itself be a group.
  const node: z.ZodTypeAny = z.lazy(() => {
    const logicalGroup = z.union([
      z.object({ AND: z.array(node).min(1) }).strict(),
      z.object({ OR: z.array(node).min(1) }).strict(),
    ]);
    return z.union([filterUnion, logicalGroup]);
  });

  const root = z.union([
    z.null(),
    z.object({ AND: z.array(node).min(1) }).strict(),
  ]);

  return root as unknown as z.ZodType<QuestionConditions>;
}

/**
 * Helper to validate a candidate output (unknown) using the dynamic schema.
 * Returns { success: true, data } or { success: false, error } similar to safeParse.
 */
export function validateQuestionConditions(
  candidate: unknown,
  tableSchema: Schema[number],
  fullSchema: Schema
) {
  const schema = createQuestionConditionsDynamicSchema(tableSchema, fullSchema);
  return schema.safeParse(candidate);
}
