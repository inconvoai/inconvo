import { type Query } from "~/types/querySchema";
import { sql } from "drizzle-orm";
import { parsePrismaWhere } from "~/operations/utils/prismaToDrizzleWhereConditions";
import { loadDrizzleSchema } from "~/util/loadDrizzleSchema";
import assert from "assert";

export async function count(db: any, query: Query) {
  assert(query.operation === "count", "Invalid inconvo operation");
  const { table, whereAndArray, operationParameters, jsonColumnSchema } = query;

  const tables = await loadDrizzleSchema();
  const drizzleWhere = parsePrismaWhere({
    tableSchemas: tables,
    tableName: table,
    where: whereAndArray,
  });

  const columnNames = operationParameters.columns;

  const jsonSchemaForTable = jsonColumnSchema?.find(
    (jsonCol) => jsonCol.tableName === table
  );

  const selectQuery: Record<string, any> = {};
  for (const name of columnNames) {
    const jsonData = jsonSchemaForTable?.jsonSchema.find(
      (x) => x.name === name
    );
    if (tables[table][name]) {
      selectQuery[name] = tables[table][name];
    } else if (jsonData) {
      const castType = jsonData.type === "number" ? "Numeric" : "Text";
      selectQuery[name] = sql
        .raw(
          `cast((${jsonSchemaForTable?.jsonColumnName}->>'${name}') as ${castType})`
        )
        .as(name);
    }
  }

  const tmpTable = db
    .$with("tmpTable")
    .as(db.select(selectQuery).from(tables[table]).where(drizzleWhere));

  const aggregateSelect = columnNames.reduce((acc, column) => {
    const colSelect = {
      [`count_${column}`]:
        sql<number>`cast(count(${tmpTable[column]}) as Int)`.as(
          `count_${column}`
        ),
    };
    return { ...acc, ...colSelect };
  }, {});

  const response = (await db
    .with(tmpTable)
    .select(aggregateSelect)
    .from(tmpTable)) as any;

  const formattedResponse = response[0]
    ? Object.keys(response[0]).reduce(
        (
          acc: {
            _count: { [key: string]: any };
          },
          key
        ) => {
          if (key.startsWith("count_"))
            acc._count[key.replace("count_", "")] = response[0][key];
          return acc;
        },
        { _count: {} }
      )
    : {};

  return formattedResponse;
}
