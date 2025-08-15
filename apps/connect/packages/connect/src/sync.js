#!/usr/bin/env node
const { execSync } = require("child_process");
const path = require("path");
const dotenv = require("dotenv");
const { Project, SyntaxKind } = require("ts-morph");
const fs = require("fs");
const pino = require("pino");

const logger = pino({
  level: process.env.LOG_LEVEL || "info",
  transport: {
    target: "pino-pretty",
    options: {
      translateTime: "HH:MM:ss",
      ignore: "pid,hostname,time,level",
      messageFormat: "[{level}]:{msg}",
    },
  },
  formatters: {
    level: (label) => {
      return { level: label.toUpperCase() };
    },
  },
});

const userProjectDir = process.cwd();

// Load the .env file
dotenv.config({ path: path.join(userProjectDir, ".env") });

function getDrizzlePath() {
  try {
    const drizzleKit = require.resolve("@ten-dev/inconvo/express");
    return path.resolve(drizzleKit, "../../../");
  } catch (e) {
    logger.error("[DRIZZLE]:Drizzle kit package not found");
    logger.error({ err: e }, "[DRIZZLE]:Error details");
  }
  return null;
}

function parseDrizzlePullOutput(output) {
  const lines = output.toString().split("\n");
  const finalStateMap = new Map();
  let driverInfo = null;

  // Process lines to find the last occurrence of each type
  lines.forEach((line) => {
    if (line.includes("[✓]") && line.includes("fetched")) {
      // Extract the type (tables, columns, enums, etc.)
      const match = line.match(/(\d+\s+\w+)\s+fetched/);
      if (match) {
        finalStateMap.set(match[1].trim().split(/\s+/).pop(), line);
      }
    }
  });

  const result = [];

  // Add fetch results in a consistent order
  const order = [
    "tables",
    "columns",
    "enums",
    "indexes",
    "keys",
    "policies",
    "constraints",
    "views",
  ];

  order.forEach((key) => {
    if (finalStateMap.has(key)) {
      result.push(finalStateMap.get(key));
    }
  });

  return result;
}

function runDrizzleCommand(command, drizzlePath) {
  try {
    const configPath = path.join(drizzlePath, "drizzle.config.js");
    const output = execSync(
      `npx drizzle-kit ${command} --config=${configPath}`,
      {
        env: process.env,
        cwd: drizzlePath,
        stdio: "pipe",
        encoding: "utf8",
      }
    );

    if (output && command === "pull") {
      // Parse and display cleaned output for pull command
      const parsedLines = parseDrizzlePullOutput(output);
      if (parsedLines.length > 0) {
        logger.info("Database introspection completed:");
        parsedLines.forEach((line) => {
          const match = line.match(/\[✓\]\s+(\d+\s+\w+\s+fetched)/);
          if (match) {
            logger.info(`[✓] ${match[1]}`);
          } else {
            logger.info(line);
          }
        });
      }
    } else if (output) {
      // For other commands, show full output
      console.log(output);
    }

    return output;
  } catch (error) {
    // Log stderr if available
    if (error.stderr) {
      error.stderr
        .toString()
        .split("\n")
        .forEach((line) => {
          if (line.trim()) {
            logger.error(`[DRIZZLE]:${line.trim()}`);
          }
        });
    }
    throw new Error(`Failed to run command "${command}": ${error.message}`);
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
    logger.info("Compiling Drizzle schemas to JavaScript...");
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

    if (output) {
      // Split output by lines and log each non-empty line
      output
        .toString()
        .split("\n")
        .forEach((line) => {
          if (line.trim()) {
            logger.info(`${line.trim()}`);
          }
        });
    }

    logger.info("Schema compilation completed successfully.");
    return true;
  } catch (error) {
    // Log stderr if available
    if (error.stderr) {
      error.stderr
        .toString()
        .split("\n")
        .forEach((line) => {
          if (line.trim()) {
            logger.error(`${line.trim()}`);
          }
        });
    }
    logger.error({ err: error }, "Failed to compile schemas");
    return false;
  }
}

(async () => {
  try {
    const drizzlePath = getDrizzlePath();
    if (!drizzlePath) {
      logger.error("Drizzle path or schema path not found");
      process.exit(1);
    }
    logger.info("Reading live database schema...");
    runDrizzleCommand("pull", drizzlePath);
    logger.info("Updating local schema representation...");

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
    logger.error({ err: error }, "An error occurred while syncing DB");
    process.exit(1);
  }
})();
