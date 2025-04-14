import { execSync } from "child_process";
import path from "path";
import dotenv from "dotenv";
import { Project, Node, SourceFile, SyntaxKind } from "ts-morph";
import fs from "fs";

dotenv.config({ path: ".env.test" });

function getDrizzlePath(): string | null {
  try {
    const drizzleKit = require.resolve("@ten-dev/inconvo/express");
    return path.resolve(drizzleKit, "../../../");
  } catch (e) {
    console.error("Drizzle kit package not found");
    console.error(e);
  }
  return null;
}

function runDrizzleCommand(command: string, drizzlePath: string): undefined {
  try {
    const configPath = path.join(drizzlePath, "drizzle.config.js");
    execSync(`npx drizzle-kit ${command} --config=${configPath}`, {
      env: process.env,
      cwd: drizzlePath,
      stdio: "inherit",
    });
  } catch (error) {
    throw new Error(`Failed to run command "${command}": ${error}`);
  }
}

function parseSchema(drizzlePath: string): void {
  const project = new Project({
    useInMemoryFileSystem: false,
    compilerOptions: {
      allowJs: true,
      target: 99, // Latest ESNext
      module: 1, // CommonJS
    },
  });

  const drizzleDir = path.join(drizzlePath, "../drizzle");
  const schemaFilePath = path.join(drizzleDir, "schema.ts");
  const relationsFilePath = path.join(drizzleDir, "relations.ts");

  const filePaths = [schemaFilePath, relationsFilePath];

  function fixBrokenVariableDeclarations(sourceFile: SourceFile): void {
    const statements = sourceFile.getStatements();

    // Iterate through the statements (keep in mind the list shrinks as we remove nodes)
    for (let i = 0; i < statements.length; i++) {
      const stmt = statements[i];
      // Look for a VariableStatement (which should carry the export/const etc)
      if (Node.isVariableStatement(stmt)) {
        // Get the declaration list inside the variable statement.
        const declList = stmt.getDeclarationList();
        const declarations = declList.getDeclarations();

        // If the declaration list is empty, it's likely that the variable name ended up elsewhere.
        if (declarations.length === 0 && i < statements.length - 1) {
          const nextStmt = statements[i + 1];
          // If the next statement is an ExpressionStatement...
          if (Node.isExpressionStatement(nextStmt)) {
            // ...check if its expression is a BinaryExpression.
            const expr = nextStmt.getExpression();
            if (Node.isBinaryExpression(expr)) {
              const left = expr.getLeft();
              // Look for the left-hand side being a StringLiteral (like "test-able")
              if (Node.isStringLiteral(left)) {
                // Get the text without quotes and sanitize it
                const literalText = left.getLiteralValue();
                const validIdentifier = literalText.replace(
                  /[^a-zA-Z0-9_$]/g,
                  "_"
                );
                // The right-hand side should be the initializer
                const initializerText = expr.getRight().getText();

                // Capture any export modifier if present
                const exportPrefix = stmt.hasModifier(SyntaxKind.ExportKeyword)
                  ? "export "
                  : "";
                // Get the declaration kind ("const", "let" or "var")
                const declarationKind = declList.getDeclarationKind();
                // Build new valid variable statement text.
                const newVarStmtText = `${exportPrefix}${declarationKind} ${validIdentifier} = ${initializerText};`;

                // Replace the original variable statement node with the new text…
                stmt.replaceWithText(newVarStmtText);
                // …and remove the following (now redundant) expression statement.
                nextStmt.remove();
                // Skip the next statement in our loop since we just removed it.
                i++;
              }
            }
          }
        }
      }
    }
  }

  for (const fullPath of filePaths) {
    const content = fs.readFileSync(fullPath, "utf8");
    const sourceFile = project.createSourceFile(fullPath, content, {
      overwrite: true,
    });
    fixBrokenVariableDeclarations(sourceFile);
    sourceFile.saveSync();
  }
}

function compileSchemas(drizzlePath: string): boolean {
  try {
    console.log("Compiling Drizzle schemas to JavaScript...");
    const drizzleDir = path.join(drizzlePath, "../drizzle");
    execSync(
      `npx tsc ${path.join(drizzleDir, "schema.ts")} ${path.join(
        drizzleDir,
        "relations.ts"
      )} --skipLibCheck --outDir ${drizzleDir}`,
      {
        stdio: "inherit",
      }
    );
    console.log("Schema compilation completed successfully.");
    return true;
  } catch (error) {
    console.error("Failed to compile schemas:", error);
    return false;
  }
}

module.exports = async () => {
  try {
    const drizzlePath = getDrizzlePath();
    if (!drizzlePath) {
      console.error("Drizzle path or schema path not found");
      process.exit(1);
    }
    console.log("Drizzle path", drizzlePath);
    runDrizzleCommand("pull", drizzlePath);
    console.log("Schema pulled successfully.");
    console.log("Parsing Drizzle schemas...");
    parseSchema(drizzlePath);
    console.log("Compiling TypeScript schemas...");
    const compiled = compileSchemas(drizzlePath);
    if (!compiled) {
      console.warn(
        "Schema compilation failed. The TypeScript schemas will still be available."
      );
    }
  } catch (error) {
    console.error("An error occurred while syncing DB:", error);
    process.exit(1);
  }
};
