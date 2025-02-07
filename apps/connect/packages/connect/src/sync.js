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
    const packagePath = require.resolve("prisma/package.json");
    if (packagePath) {
      const prismaPath = require.resolve("prisma");
      return path.resolve(prismaPath, "../../../");
    }
  } catch (e) {}
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

(async () => {
  try {
    const prismaPath = getPrismaPath();
    const prismaSchemaPath = getPrismaSchemaPath();
    console.log("Prisma schema path", prismaSchemaPath);
    if (!prismaPath || !prismaSchemaPath) {
      console.error("Inconvo not found in the project");
      process.exit(1);
    }
    await runPrismaCommand("db pull", prismaPath, prismaSchemaPath);
    await runPrismaCommand("generate", prismaPath, prismaSchemaPath);
    console.log(" Schema pulled successfully.");
  } catch (error) {
    console.error("An error occurred while syncing DB:", error);
    process.exit(1);
  }
})();
