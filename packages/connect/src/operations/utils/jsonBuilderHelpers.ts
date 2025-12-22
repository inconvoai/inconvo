import { sql } from "kysely";
import { env } from "../../env";

const getDialect = () => process.env.DATABASE_DIALECT || env.DATABASE_DIALECT;

export function buildJsonObject(fields: [string, any][]): any {
  if (fields.length === 0) return null;

  const params: any[] = [];
  fields.forEach(([name, value]) => {
    params.push(sql.lit(name), value);
  });

  const dialect = getDialect();

  if (dialect === "postgresql") {
    return sql`json_build_object(${sql.join(params, sql`, `)})`;
  } else if (dialect === "redshift") {
    return sql`OBJECT(${sql.join(params, sql`, `)})`;
  } else if (dialect === "mysql") {
    return sql`json_object(${sql.join(params, sql`, `)})`;
  } else if (dialect === "mssql") {
    // For MSSQL, we build JSON using FOR JSON PATH
    // We need to select each field with an alias that becomes the JSON key
    const selections = fields.map(
      ([name, value]) => sql`${value} AS ${sql.id(name)}`,
    );
    return sql`(SELECT ${sql.join(selections, sql`, `)} FOR JSON PATH, WITHOUT_ARRAY_WRAPPER)`;
  } else if (dialect === "bigquery") {
    const kvPairs: any[] = [];
    fields.forEach(([name, value]) => {
      kvPairs.push(sql.lit(name), value);
    });
    return sql`JSON_OBJECT(${sql.join(kvPairs, sql`, `)})`;
  }
  throw new Error(`Unsupported database dialect: ${env.DATABASE_DIALECT}`);
}

export function jsonAggregate(jsonObject: any): any {
  const dialect = getDialect();

  if (dialect === "postgresql") {
    return sql`COALESCE(json_agg(${jsonObject}), '[]')`;
  } else if (dialect === "redshift") {
    return sql`COALESCE(ARRAY_AGG(${jsonObject}), JSON_PARSE('[]'))`;
  } else if (dialect === "mysql") {
    return sql`COALESCE(json_arrayagg(${jsonObject}), CAST('[]' AS JSON))`;
  } else if (dialect === "mssql") {
    return sql`COALESCE(CONCAT('[', STRING_AGG(CAST(${jsonObject} AS NVARCHAR(MAX)), ','), ']'), '[]')`;
  } else if (dialect === "bigquery") {
    return sql`IFNULL(TO_JSON(ARRAY_AGG(${jsonObject})), JSON '[]')`;
  }
  throw new Error(`Unsupported database dialect: ${env.DATABASE_DIALECT}`);
}

export function buildJsonObjectSelect(fields: [string, any][]): any {
  return buildJsonObject(fields);
}
