import { type Query } from "~/types/querySchema";
import { sql, count as dCount } from "drizzle-orm";
import { parsePrismaWhere } from "~/operations/utils/prismaToDrizzleWhereConditions";
import { loadDrizzleSchema } from "~/util/loadDrizzleSchema";
import assert from "assert";
import {
  getColumnFromCTE,
  getColumnFromTable,
} from "../utils/getColumnFromTable";

export async function count(db: any, query: Query) {
  assert(query.operation === "count", "Invalid inconvo operation");
  const { table, whereAndArray, operationParameters, computedColumns } = query;

  const drizzleSchema = await loadDrizzleSchema();

  const columnNames = operationParameters.columns;

  const selectQuery: Record<string, any> = {};
  for (const name of columnNames) {
    selectQuery[name] = sql`${getColumnFromTable({
      columnName: name,
      tableName: table,
      drizzleSchema,
      computedColumns,
    })}`.as(name);
  }

  const tmpTable = db.$with("tmpTable").as(
    db
      .select(selectQuery)
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

  const aggregateSelect = columnNames.reduce((acc, column) => {
    const colSelect = {
      [`count_${column}`]: dCount(
        getColumnFromCTE({ cte: tmpTable, columnName: column })
      ).as(`count_${column}`),
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
