import { getAugmentedSchema } from "~/util/augmentedSchemaCache";

export async function findRelationsBetweenTables(
  sourceTableName: string,
  targetTableName: string,
  relationName: string
): Promise<[string, string, boolean]> {
  const schema = await getAugmentedSchema();
  const sourceTable = schema.tables.find((t: any) => t.name === sourceTableName);
  const targetTable = schema.tables.find((t: any) => t.name === targetTableName);

  if (!sourceTable) {
    throw new Error(`Source table ${sourceTableName} not found in schema`);
  }
  if (!targetTable) {
    throw new Error(`Target table ${targetTableName} not found in schema`);
  }

  // Check if sourceTable references targetTable (one-to-many from source perspective)
  const sourceRelation = sourceTable.relations?.find(
    (r: any) => r.name === relationName
  );

  if (sourceRelation) {
    if (sourceRelation.sourceColumns?.length && sourceRelation.targetColumns?.length) {
      return [
        sourceRelation.sourceColumns[0],
        sourceRelation.targetColumns[0],
        false // No grouping needed for direct reference
      ];
    }
  }

  // Check if targetTable references sourceTable (many-to-one from source perspective)
  const targetRelation = targetTable.relations?.find(
    (r: any) => r.targetTable === sourceTableName
  );

  if (targetRelation) {
    if (targetRelation.sourceColumns?.length && targetRelation.targetColumns?.length) {
      return [
        targetRelation.targetColumns[0],
        targetRelation.sourceColumns[0],
        true // Group by is needed for reverse relation (one-to-many)
      ];
    }
  }

  throw new Error(
    `Could not find relation between ${sourceTableName} and ${targetTableName}`
  );
}
