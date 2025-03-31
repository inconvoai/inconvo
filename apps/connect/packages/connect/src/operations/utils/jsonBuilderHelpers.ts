import { Column, Param, sql, SQL } from "drizzle-orm";
import { env } from "~/env";

export function buildJsonObjectSelect(columnNameValue: [string, unknown][]) {
  if (env.DATABASE_DIALECT === "postgresql") {
    return sql.raw(
      `json_build_object(${columnNameValue
        .flatMap(([name, value]) => [name, value])
        .join(", ")})`
    );
  } else if (env.DATABASE_DIALECT === "mysql") {
    return sql.raw(
      `json_object(${columnNameValue
        .flatMap(([name, value]) => [name, value])
        .join(", ")})`
    );
  }
  throw new Error(`Unsupported database dialect: ${env.DATABASE_DIALECT}`);
}

export function jsonAggregate(jsonObject: any) {
  if (env.DATABASE_DIALECT === "postgresql") {
    return sql`json_agg(${jsonObject})`;
  } else if (env.DATABASE_DIALECT === "mysql") {
    return sql`json_arrayagg(${jsonObject})`;
  }
  throw new Error(`Unsupported database dialect: ${env.DATABASE_DIALECT}`);
}
