import dotenv from "dotenv";
import { exec } from "child_process";
import path from "path";
import { promisify } from "util";

dotenv.config({ path: ".env.test" });

const execPromise = promisify(exec);

function getDrizzlePath() {
  try {
    const drizzleKit = require.resolve("drizzle-kit");
    return path.resolve(drizzleKit, "../../../packages/connect");
  } catch (e) {
    console.error("Drizzle kit package not found");
    console.error(e);
  }
  return null;
}

async function runDrizzleCommand(
  command: string,
  drizzlePath: string
): Promise<void> {
  try {
    await execPromise(`npx drizzle-kit ${command}`, {
      env: process.env,
      cwd: drizzlePath,
    });
  } catch (error) {
    throw new Error(`Failed to run command "${command}": ${error}`);
  }
}

module.exports = async () => {
  try {
    const drizzlePath = getDrizzlePath();
    if (!drizzlePath) {
      console.error("Drizzle path or schema path not found");
      process.exit(1);
    }
    await runDrizzleCommand("pull", drizzlePath);
    console.log("Schema pulled successfully.");
  } catch (error) {
    console.error("An error occurred while syncing DB:", error);
    process.exit(1);
  }
};
