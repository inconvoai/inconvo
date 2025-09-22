// import { SchemaResponse } from "~/types/types";
// import {
//   Column,
//   getTableName,
//   getTableUniqueName,
//   is,
//   isTable,
//   Relation,
//   Table,
// } from "drizzle-orm";
// import { loadDrizzleSchema } from "~/util/loadDrizzleSchema";

// function getColumnType(column: Column): string {
//   const columnType = column.columnType.toLocaleLowerCase();
//   if (columnType.includes("date")) {
//     return "DateTime";
//   } else if (columnType.includes("timestamp")) {
//     return "DateTime";
//   } else if (columnType.includes("decimal")) {
//     return "number";
//   }
//   return column.dataType;
// }

// function getTableColumnNames(table: Table) {
//   // @ts-expect-error
//   const columns = table[Table.Symbol.Columns] as typeof table._.columns;
//   const columnNamesWithTypes = Object.entries(columns).map(
//     ([columnName, columnObj]) => {
//       const column = columns[columnName];
//       const columnType = getColumnType(column);
//       return {
//         name: columnObj.name,
//         type: columnType,
//       };
//     }
//   );
//   return columnNamesWithTypes;
// }

// function buildTableSchema(table: Table) {
//   return {
//     name: getTableName(table),
//     columns: getTableColumnNames(table),
//   };
// }

// export async function buildSchema(): Promise<SchemaResponse> {
//   const drizzleSchema = await loadDrizzleSchema();

//   const tableNamesMap: Record<string, string> = {};
//   const tmpTables: Record<string, any> = {};
//   for (const [key, value] of Object.entries(drizzleSchema)) {
//     if (isTable(value)) {
//       const dbName = getTableUniqueName(value);
//       tableNamesMap[dbName] = key;
//       const ts = buildTableSchema(value);
//       tmpTables[key] = ts;
//     } else if (is(value, Relation)) {
//       try {
//         const relConfig = value.config;
//         const relTables = value.tables || value.schema;
//         if (relConfig && relTables) {
//           for (const [tableName, relationsObj] of Object.entries(
//             relConfig as Record<string, Record<string, any>>
//           )) {
//             const tableEntry = tmpTables[tableName];
//             if (!tableEntry) continue; // skip tables we didn't record (unlikely)
//             const tableRelations: SchemaResponse["tables"][number]["relations"] =
//               [];
//             for (const [relName, rel] of Object.entries(relationsObj)) {
//               const isMany = rel.constructor?.name === "Many";
//               const sourceCols = (rel.sourceColumns || []).map(
//                 (c: Column) => c.name
//               );
//               const targetCols = (rel.targetColumns || []).map(
//                 (c: Column) => c.name
//               );
//               const targetTable = rel.targetTable
//                 ? getTableName(rel.targetTable)
//                 : undefined;
//               tableRelations.push({
//                 name: rel.fieldName || relName,
//                 isList: isMany,
//                 targetTable: targetTable,
//                 relationName: relName,
//                 sourceColumns: sourceCols.length ? sourceCols : undefined,
//                 targetColumns: targetCols.length ? targetCols : undefined,
//               });
//             }
//             if (tableRelations.length) {
//               tmpTables[tableName]["relations"] = tableRelations;
//             }
//           }
//         }
//       } catch (e) {
//         console.warn("Failed to process new relations shape", e);
//       }
//     } else {
//       console.log("Not a table or relation", key, value);
//     }
//   }

//   const tablesArr = Object.values(tmpTables);

//   const tables: SchemaResponse["tables"] =
//     tablesArr as SchemaResponse["tables"];
//   const schema: SchemaResponse = { tables };

//   return schema;
// }
