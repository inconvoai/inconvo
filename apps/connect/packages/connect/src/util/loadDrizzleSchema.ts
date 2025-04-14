import path from "path";

export async function loadDrizzleSchema(): Promise<Record<string, any>> {
  try {
    // For testing use one less ../
    const basePath = path.resolve(__dirname, "../../../drizzle");

    const schemaJsPath = path.join(basePath, "schema.js");
    const relationsJsPath = path.join(basePath, "relations.js");

    const schemaModule = await import(schemaJsPath);
    const relationsModule = await import(relationsJsPath);

    // Extract the actual exports, handling both default and named exports
    const schema = schemaModule.default || schemaModule;
    const relations = relationsModule.default || relationsModule;

    const tablesModule = {
      ...schema,
      ...relations,
    };

    return tablesModule as Record<string, any>;
  } catch (error) {
    console.error("Failed to load Drizzle schema:", error);
    throw error;
  }
}
