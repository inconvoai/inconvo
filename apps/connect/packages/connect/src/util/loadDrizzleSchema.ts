import * as dSchema from "~/../drizzle/schema";
import * as dRelations from "~/../drizzle/relations";

const drizzleSchema = {
  ...dSchema,
  ...dRelations,
};

export function loadDrizzleSchema(): Record<string, any> {
  return drizzleSchema;
}

// export async function loadDrizzleTables() {
//   try {
//     const schema = `../../drizzle/schema.ts`;
//     const relations = `../../drizzle/relations.ts`;
//     const schemaModule = await import(schema);
//     const relationsModule = await import(relations);
//     const tablesModule = { ...schemaModule, ...relationsModule };
//     return tablesModule as Record<string, any>;
//   } catch (error) {
//     console.error("Failed to load Drizzle tables:", error);
//     throw error;
//   }
// }
