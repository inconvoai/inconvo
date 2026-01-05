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

/**
 * Check if we should use flat aggregate columns instead of JSON wrapping.
 * MSSQL needs flat columns because the FOR JSON PATH subquery doesn't work
 * properly in GROUP BY contexts.
 */
export function shouldUseFlatAggregates(): boolean {
  return getDialect() === "mssql";
}

/**
 * Build flat aggregate selections for MSSQL.
 * Returns an array of [alias, expression] pairs that can be selected directly.
 * The alias format is "prefix.fieldName" (e.g., "_sum.inconvo_InvoiceItem.Total")
 */
export function buildFlatAggregateSelections(
  prefix: string,
  fields: [string, any][] | null | undefined,
): [string, any][] {
  if (!fields || fields.length === 0) return [];
  return fields.map(([name, value]) => [`${prefix}.${name}`, value]);
}

/**
 * Reconstruct nested objects from flat column names.
 * Converts { "_sum.col1": 10, "_sum.col2": 20, "other": "x" }
 * to { "_sum": { "col1": 10, "col2": 20 }, "other": "x" }
 */
export function reconstructNestedFromFlat(
  row: Record<string, unknown>,
  prefixes: string[],
): Record<string, unknown> {
  const result: Record<string, unknown> = {};
  const nestedObjects: Record<string, Record<string, unknown>> = {};

  for (const [key, value] of Object.entries(row)) {
    let matched = false;
    for (const prefix of prefixes) {
      if (key.startsWith(`${prefix}.`)) {
        const nestedKey = key.slice(prefix.length + 1);
        if (!nestedObjects[prefix]) {
          nestedObjects[prefix] = {};
        }
        nestedObjects[prefix][nestedKey] = value;
        matched = true;
        break;
      }
    }
    if (!matched) {
      result[key] = value;
    }
  }

  // Add nested objects to result
  for (const prefix of prefixes) {
    if (nestedObjects[prefix] && Object.keys(nestedObjects[prefix]).length > 0) {
      result[prefix] = nestedObjects[prefix];
    }
  }

  return result;
}
