#!/usr/bin/env node
const { exec } = require("child_process");
const path = require("path");
const dotenv = require("dotenv");

const userProjectDir = process.cwd();
console.log("User project dir", userProjectDir);

// Load the .env file
dotenv.config({ path: path.join(userProjectDir, ".env") });

function getPrismaPath() {
  try {
    const prismaPath = require.resolve("prisma/package.json");
    return path.resolve(prismaPath, "../../../");
  } catch (e) {
    console.error("Prisma package not found");
    console.error(e);
  }
  return null;
}

function getPrismaSchemaPath() {
  try {
    const inconvoPath = require.resolve("@ten-dev/inconvo/express");
    return path.resolve(inconvoPath, "../../../prisma/schema");
  } catch (e) {}
  return null;
}

function runPrismaCommand(command, prismaPath, schemaPath) {
  return new Promise((resolve, reject) => {
    exec(
      `npx prisma ${command} --schema ${schemaPath}`,
      { env: process.env, cwd: prismaPath },
      (error) => {
        if (error) {
          return reject(error);
        }
        resolve();
      }
    );
  });
}

function getDrizzleSchemaPath() {
  try {
    const inconvoPath = require.resolve("@ten-dev/inconvo/express");
    return path.resolve(inconvoPath, "../../../drizzle/schema.ts");
  } catch (e) {}
  return null;
}
function getDrizzleSchemaJsPath() {
  try {
    const inconvoPath = require.resolve("@ten-dev/inconvo/express");
    return path.resolve(inconvoPath, "../../../drizzle");
  } catch (e) {}
  return null;
}

function compileDrizzleSchema() {
  const tsPath = getDrizzleSchemaPath();
  const jsPath = getDrizzleSchemaJsPath();
  if (process.env.DRIZZLE === "FALSE") {
    console.log("Skipping drizzle schema compilation");
    // make a empty schema file
    const fs = require("fs");
    const emptySchema = `export const schema = {};`;
    const emptySchemaPath = path.join(jsPath, "schema.js");
    fs.writeFileSync(emptySchemaPath, emptySchema);
    console.log("Empty drizzle schema created at", emptySchemaPath);
    return Promise.resolve();
  }
  console.log("Compiling drizzle schema", tsPath, jsPath);
  return new Promise((resolve, reject) => {
    exec(
      `npx tsc ${tsPath} --outDir ${jsPath} --skipLibCheck `,
      { env: process.env, cwd: userProjectDir },
      (error) => {
        if (error) {
          return reject(error);
        }
        resolve();
      }
    );
  });
}

(async () => {
  try {
    const prismaPath = getPrismaPath();
    const prismaSchemaPath = getPrismaSchemaPath();
    console.log("Prisma schema path", prismaSchemaPath);
    if (!prismaPath || !prismaSchemaPath) {
      console.log(prismaPath, prismaSchemaPath);
      console.error("Inconvo not found in the project");
      process.exit(1);
    }
    await runPrismaCommand("db pull", prismaPath, prismaSchemaPath);
    await runPrismaCommand("generate", prismaPath, prismaSchemaPath);
    console.log("Schema pulled successfully.");
    await compileDrizzleSchema();
  } catch (error) {
    console.error("An error occurred while syncing DB:", error);
    process.exit(1);
  }
})();
