import { sql, SQL } from "drizzle-orm";
import { env } from "~/env";

export function buildJsonObjectSelect(columnNameValue: [string, unknown][]) {
  const params: SQL[] = [];
  columnNameValue.forEach(([name, value]) => {
    params.push(sql.raw(`'${name}'`), sql`${value}`);
  });
  if (env.DATABASE_DIALECT === "postgresql") {
    return sql`json_build_object(${sql.join(params, sql`, `)})`;
  } else if (env.DATABASE_DIALECT === "mysql") {
    return sql`json_object(${sql.join(params, sql`, `)})`;
  }
  throw new Error(`Unsupported database dialect: ${env.DATABASE_DIALECT}`);
}

export function jsonAggregate(jsonObject: any) {
  if (env.DATABASE_DIALECT === "postgresql") {
    return sql`COALESCE(json_agg(${jsonObject}), '[]')`;
  } else if (env.DATABASE_DIALECT === "mysql") {
    return sql`COALESCE(json_arrayagg(${jsonObject}), CAST('[]' AS JSON))`;
  }
  throw new Error(`Unsupported database dialect: ${env.DATABASE_DIALECT}`);
}
