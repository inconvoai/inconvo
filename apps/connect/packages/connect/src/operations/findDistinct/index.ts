import { type Query } from "~/types/querySchema";
import { getTableColumns, sql } from "drizzle-orm";
import { parsePrismaWhere } from "~/operations/utils/prismaToDrizzleWhereConditions";
import { loadDrizzleSchema } from "~/util/loadDrizzleSchema";
import assert from "assert";

export async function findDistinct(db: any, query: Query) {
  assert(query.operation === "findDistinct", "Invalid inconvo operation");
  const {
    table,
    whereAndArray,
    operationParameters,
    jsonColumnSchema,
    computedColumns,
  } = query;

  const drizzleSchema = await loadDrizzleSchema();

  const jsonSchemaForTable = jsonColumnSchema?.find(
    (jsonCol) => jsonCol.tableName === table
  );
  const jsonCols = jsonSchemaForTable?.jsonSchema.map((col) => col.name) || [];
  const jsonColumnName = jsonSchemaForTable?.jsonColumnName;
  const tableAlias = db.$with(`${table}Alias`).as(
    db
      .select({
        ...getTableColumns(drizzleSchema[table]),
        ...jsonCols.reduce((acc: Record<string, unknown>, col) => {
          acc[col] = sql
            .raw(
              `cast((${jsonColumnName}->>'${col}') as ${
                jsonSchemaForTable?.jsonSchema.find((jCol) => jCol.name === col)
                  ? "Text"
                  : "Numeric"
              })`
            )
            .as(col);
          return acc;
        }, {}),
      })
      .from(drizzleSchema[table])
      .where((columns: Record<string, unknown>) =>
        parsePrismaWhere({
          drizzleSchema,
          tableName: table,
          where: whereAndArray,
          columns,
          computedColumns: computedColumns,
        })
      )
  );

  const response = await db
    .with(tableAlias)
    .selectDistinct({
      [operationParameters.column]: tableAlias[operationParameters.column],
    })
    .from(tableAlias)
    .limit(250);

  if (response.length > 249) {
    throw new Error("Find Distinct limit hit at 250");
  }

  return response;
}
