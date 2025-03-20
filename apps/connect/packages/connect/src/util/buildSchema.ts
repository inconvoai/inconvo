import { SchemaResponse } from "~/types/types";
import {
  Column,
  createTableRelationsHelpers,
  getTableName,
  getTableUniqueName,
  is,
  isTable,
  Relations,
  Table,
} from "drizzle-orm";
import { loadDrizzleSchema } from "~/util/loadDrizzleSchema";

function getColumnType(column: any): string {
  const columnType = column["columnType"].toLowerCase();
  if (columnType.includes("date")) {
    return "DateTime";
  } else if (columnType.includes("timestamp")) {
    return "DateTime";
  }

  return column["dataType"];
}

function getTableColumnNames(table: Table) {
  //@ts-expect-error
  const columns = table[Table.Symbol.Columns];

  const columnNamesWithTypes = Object.entries(columns).map(
    ([columnName, columnObj]) => {
      const column = columns[columnName];
      const columnType = getColumnType(column);
      return {
        // @ts-expect-error
        name: columnObj.name,
        type: columnType,
      };
    }
  );
  return columnNamesWithTypes;
}

function buildTableSchema(table: Table) {
  return {
    name: getTableName(table),
    columns: getTableColumnNames(table),
  };
}

function buildTableRelations(relations: Relations) {
  let schemaRelations: SchemaResponse["tables"][number]["relations"] = [];
  for (const [key, value] of Object.entries(relations)) {
    const relation = {
      name: value.fieldName,
      isList: !value.config,
      targetTable: value.referencedTableName,
      relationName: value.relationName,
      sourceColumns:
        value?.config?.fields?.map((field: Column) => field.name) ?? undefined,
      targetColumns:
        value?.config?.references?.map((field: Column) => field.name) ??
        undefined,
    };
    schemaRelations.push(relation);
  }
  return schemaRelations;
}

export function buildSchema(): SchemaResponse {
  const drizzleSchema = loadDrizzleSchema();

  const tableNamesMap: Record<string, string> = {};
  let tmpTables: Record<string, any> = {};
  for (const [key, value] of Object.entries(drizzleSchema)) {
    if (isTable(value)) {
      const dbName = getTableUniqueName(value);
      tableNamesMap[dbName] = key;
      const ts = buildTableSchema(value);
      tmpTables[key] = ts;
    } else if (is(value, Relations)) {
      const dbName = getTableUniqueName(value.table);
      const tableName = tableNamesMap[dbName];
      const relations = value.config(
        createTableRelationsHelpers(drizzleSchema[tableName])
      ) as unknown as Relations;
      const tableRelations = buildTableRelations(relations);
      tmpTables[tableName]["relations"] = tableRelations;
    }
  }

  const tablesArr = Object.values(tmpTables);

  let tables: SchemaResponse["tables"] = tablesArr as SchemaResponse["tables"];
  const schema: SchemaResponse = { tables };

  return schema;
}
