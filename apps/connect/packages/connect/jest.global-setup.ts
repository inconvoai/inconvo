import dotenv from "dotenv";
import { exec } from "child_process";
import path from "path";
import { promisify } from "util";

dotenv.config({ path: ".env.test" });

const execPromise = promisify(exec);

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

function getPrismaSchemaPath(): string | null {
  try {
    const inconvoPath = require.resolve("@ten-dev/inconvo/express");
    return path.resolve(inconvoPath, "../../../prisma/schema");
  } catch (e) {
    console.error("Inconvo schema not found");
  }
  return null;
}

async function runPrismaCommand(
  command: string,
  prismaPath: string,
  schemaPath: string
): Promise<void> {
  try {
    await execPromise(`npx prisma ${command} --schema ${schemaPath}`, {
      env: process.env,
      cwd: prismaPath,
    });
  } catch (error) {
    throw new Error(`Failed to run prisma command: ${command}`);
  }
}

module.exports = async () => {
  try {
    const prismaPath = getPrismaPath();
    const prismaSchemaPath = getPrismaSchemaPath();
    if (!prismaPath || !prismaSchemaPath) {
      console.error("Inconvo not found in the project");
      process.exit(1);
    }
    await runPrismaCommand("db pull", prismaPath, prismaSchemaPath);
    await runPrismaCommand("generate", prismaPath, prismaSchemaPath);
    console.log("Schema pulled successfully.");
  } catch (error) {
    console.error("An error occurred while syncing DB:", error);
    process.exit(1);
  }
};
