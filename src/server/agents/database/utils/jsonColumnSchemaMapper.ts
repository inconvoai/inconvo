import assert from "assert";
import type { Schema } from "~/server/db/schema";
import type { JsonColumnSchema } from "~/server/userDatabaseConnector/types";

type JsonColumnData = Record<string, string | number>;
type JsonExample = Record<string, JsonColumnData>;
type ColumnSchema = Schema[number]["columns"][number];

export function mapJsonToSchema(
  obj: JsonExample,
  tableName: string
): JsonColumnSchema[number] {
  const columnName = Object.keys(obj)[0];
  if (!columnName) {
    throw new Error("JSONColConversion: Column Name key is undefined");
  }

  const columnData = obj[columnName];
  if (!columnData) {
    throw new Error("JSONColConversion: Column data is undefined");
  }

  const childKeys = Object.keys(columnData);

  const masqueradeColumns: ColumnSchema[] = childKeys.map((key) => {
    const value = columnData[key];
    assert(
      typeof value === "string" || typeof value === "number",
      `JSONColConversion: Unsupported type for key ${key}`
    );
    const type = typeof value === "string" ? "String" : "Number";
    return {
      name: key,
      dbName: key,
      rename: null,
      notes: null,
      relation: [],
      type: type,
      effectiveType: type,
      conversion: null,
      unit: null,
    };
  });

  const jsonColumnSchema = {
    tableName,
    jsonColumnName: columnName,
    jsonSchema: masqueradeColumns.map((col) => {
      return {
        name: col.name,
        relation: null,
        type: col.type,
        unit: col.unit,
      };
    }),
  };

  return jsonColumnSchema;
}
