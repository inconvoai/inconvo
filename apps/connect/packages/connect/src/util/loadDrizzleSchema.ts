import path from "path";
import fs from "fs";

export async function loadDrizzleSchema(): Promise<Record<string, any>> {
  const candidateBasePaths = [
    // Original expected location when running inside the connect package
    path.resolve(__dirname, "../../../drizzle"),
    // Fallback when ts-jest transpilation changes relative depth (observed in parity-tests)
    path.resolve(__dirname, "../../../../packages/connect/drizzle"),
  ];

  let lastError: unknown;
  for (const basePath of candidateBasePaths) {
    try {
      const schemaJsPath = path.join(basePath, "schema.js");
      const relationsJsPath = path.join(basePath, "relations.js");

      if (!fs.existsSync(schemaJsPath) || !fs.existsSync(relationsJsPath)) {
        continue; // try next candidate
      }

      const schemaModule = await import(schemaJsPath);
      const relationsModule = await import(relationsJsPath);

      const schema = (schemaModule as any).default || schemaModule;
      const relations = (relationsModule as any).default || relationsModule;

      return {
        ...schema,
        ...relations,
      } as Record<string, any>;
    } catch (err) {
      lastError = err;
      continue;
    }
  }

  console.error(
    "Failed to load Drizzle schema after trying candidates:",
    candidateBasePaths,
    lastError
  );
  throw lastError;
}
