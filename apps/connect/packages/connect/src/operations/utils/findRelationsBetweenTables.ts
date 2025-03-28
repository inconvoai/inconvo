import { Column } from "drizzle-orm";
import { getRelationsForTable } from "./drizzleSchemaHelpers";
export function findRelationsBetweenTables(
  sourceTableName: string,
  targetTableName: string,
  relationName: string,
  drizzleTables: Record<string, any>
): [string, string, boolean] {
  const sourceTableRelations = getRelationsForTable(
    sourceTableName,
    drizzleTables
  );
  const targetTableRelations = getRelationsForTable(
    targetTableName,
    drizzleTables
  );

  // 1) Check if tableA references tableB
  for (const [key, value] of Object.entries(sourceTableRelations)) {
    if (value.fieldName === relationName) {
      if (!value.config) {
        continue;
      }
      return [
        value?.config?.fields?.map((field: Column) => field.name)[0] ??
          undefined,
        value?.config?.references?.map((field: Column) => field.name)[0] ??
          undefined,
        false,
      ];
    }
  }

  // Check if tableB references tableA
  for (const [key, value] of Object.entries(targetTableRelations)) {
    if (
      value.fieldName === relationName ||
      value.fieldName === sourceTableName
    ) {
      return [
        value?.config?.fields?.map((field: Column) => field.name)[0],
        value?.config?.references?.map((field: Column) => field.name)[0],
        true,
      ];
    }
  }

  throw new Error(
    `Could not findRelationsBetweenTables ${sourceTableName}, ${targetTableName}`
  );
}
