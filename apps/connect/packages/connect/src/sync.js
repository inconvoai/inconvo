#!/usr/bin/env node
const { execSync } = require("child_process");
const path = require("path");
const dotenv = require("dotenv");

const userProjectDir = process.cwd();
console.log("User project dir", userProjectDir);

// Load the .env file
dotenv.config({ path: path.join(userProjectDir, ".env") });

function getDrizzlePath() {
  try {
    const drizzleKit = require.resolve("@ten-dev/inconvo/express");
    return path.resolve(drizzleKit, "../../../");
  } catch (e) {
    console.error("Drizzle kit package not found");
    console.error(e);
  }
  return null;
}

function runDrizzleCommand(command, drizzlePath) {
  try {
    const configPath = path.join(drizzlePath, "drizzle.config.js");
    return execSync(`npx drizzle-kit ${command} --config=${configPath}`, {
      env: process.env,
      cwd: drizzlePath,
      stdio: "inherit",
    });
  } catch (error) {
    throw new Error(`Failed to run command "${command}": ${error}`);
  }
}

function compileSchemas(drizzlePath) {
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

(async () => {
  try {
    const drizzlePath = getDrizzlePath();
    if (!drizzlePath) {
      console.error("Drizzle path or schema path not found");
      process.exit(1);
    }
    console.log("Drizzle path", drizzlePath);
    // Run drizzle-kit pull to generate the schema
    runDrizzleCommand("pull", drizzlePath);
    console.log("Schema pulled successfully.");
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
})();
