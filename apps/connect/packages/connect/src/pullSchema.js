#!/usr/bin/env node
const { execSync } = require("child_process");
const path = require("path");
const dotenv = require("dotenv");
const { Project, SyntaxKind } = require("ts-morph");
const fs = require("fs");
const {
  logger,
  processPullOutput,
  logCommandError,
  logCompilationResult,
} = require("./src/drizzleOutputParser");

const userProjectDir = process.cwd();

// Load the .env file
dotenv.config({ path: path.join(userProjectDir, ".env") });

function getDrizzlePath() {
  try {
    const drizzleKit = require.resolve("@ten-dev/inconvo/express");
    return path.resolve(drizzleKit, "../../../");
  } catch (e) {
    logger.error("Schema pull package not found");
    logger.debug({ err: e }, "Error details");
  }
  return null;
}

function runDrizzleCommand(command, drizzlePath) {
  try {
    const configPath = path.join(drizzlePath, "drizzle.config.js");
    logger.debug(`Running drizzle-kit ${command} with config: ${configPath}`);

    const output = execSync(
      `npx drizzle-kit ${command} --config=${configPath} 2>&1`,
      {
        env: process.env,
        cwd: drizzlePath,
        stdio: "pipe",
        encoding: "utf8",
      }
    );

    if (command === "pull") {
      processPullOutput(output);
    } else if (output) {
      // For other commands, show full output
      console.log(output);
    }

    return output;
  } catch (error) {
    logCommandError(error, command);
    throw error;
  }
}

function parseSchema(drizzlePath) {
  const project = new Project({
    useInMemoryFileSystem: false, // Use real file system
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

  function fixBrokenVariableDeclarations(sourceFile) {
    const statements = sourceFile.getStatements();

    // Iterate through the statements (keep in mind the list shrinks as we remove nodes)
    for (let i = 0; i < statements.length; i++) {
      const stmt = statements[i];
      // Look for a VariableStatement (which should carry the export/const etc)
      if (stmt.getKind() === SyntaxKind.VariableStatement) {
        // Get the declaration list inside the variable statement.
        const declList = stmt.getDeclarationList();
        const declarations = declList.getDeclarations();

        // If the declaration list is empty, it’s likely that the variable name ended up elsewhere.
        if (declarations.length === 0 && i < statements.length - 1) {
          const nextStmt = statements[i + 1];
          // If the next statement is an ExpressionStatement...
          if (nextStmt.getKind() === SyntaxKind.ExpressionStatement) {
            // ...check if its expression is a BinaryExpression.
            const expr = nextStmt.getExpression();
            if (expr.getKind() === SyntaxKind.BinaryExpression) {
              const binaryExpr = expr;
              const left = binaryExpr.getLeft();
              // Look for the left-hand side being a StringLiteral (like "test-able")
              if (left.getKind() === SyntaxKind.StringLiteral) {
                // Get the text without quotes and sanitize it (replace any non-valid identifier character with "_")
                const literalText = left.getLiteralValue();

                const validIdentifier = literalText.replace(
                  /[^a-zA-Z0-9_$]/g,
                  "_"
                );
                // The right-hand side should be the initializer (e.g. pgTable("test-table", { … }))
                const initializerText = binaryExpr.getRight().getText();

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

function compileSchemas(drizzlePath) {
  try {
    logger.debug("Compiling Drizzle schemas to JavaScript...");
    const drizzleDir = path.join(drizzlePath, "../drizzle");
    const output = execSync(
      `npx tsc ${path.join(drizzleDir, "schema.ts")} ${path.join(
        drizzleDir,
        "relations.ts"
      )} --skipLibCheck --outDir ${drizzleDir}`,
      {
        stdio: "pipe",
        encoding: "utf8",
      }
    );

    return logCompilationResult(output, null);
  } catch (error) {
    return logCompilationResult(null, error);
  }
}

(async () => {
  try {
    const drizzlePath = getDrizzlePath();
    if (!drizzlePath) {
      logger.error("Drizzle path or schema path not found");
      process.exit(1);
    }

    logger.info("Reading database schema...");
    runDrizzleCommand("pull", drizzlePath);

    logger.info("Validating schema...");
    parseSchema(drizzlePath);
    logger.info("Schema validation completed successfully.");

    const compiled = compileSchemas(drizzlePath);
    if (!compiled) {
      logger.warn(
        "Schema compilation failed. The TypeScript schemas will still be available."
      );
    }
  } catch (error) {
    // Error has already been logged by runDrizzleCommand
    logger.error("An error occurred during schema pull and compilation.");
    logger.info(error);
    process.exit(1);
  }
})();
