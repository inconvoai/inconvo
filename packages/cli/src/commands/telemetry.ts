import { Command } from "commander";
import * as fs from "fs/promises";
import { getEnvPath, envExists, readEnvFile, ensureInconvoDir } from "../wizard/setup.js";
import { logInfo, logGray } from "../process/output.js";
import { runCliAction } from "./_shared/command-runtime.js";

async function updateTelemetrySetting(enabled: boolean): Promise<void> {
  const envPath = getEnvPath();

  if (!(await envExists())) {
    throw new Error(
      "Configuration not found. Run 'inconvo dev configure' first.",
    );
  }

  const content = await fs.readFile(envPath, "utf-8");
  const lines = content.split("\n");
  const newLines: string[] = [];
  let found = false;

  for (const line of lines) {
    if (line.startsWith("DISABLE_TELEMETRY=")) {
      newLines.push(`DISABLE_TELEMETRY=${enabled ? "false" : "true"}`);
      found = true;
    } else {
      newLines.push(line);
    }
  }

  // If not found, add it
  if (!found) {
    // Find a good place to insert (before the last empty line if any)
    const lastNonEmpty = newLines.findLastIndex((l) => l.trim() !== "");
    if (lastNonEmpty >= 0) {
      newLines.splice(
        lastNonEmpty + 1,
        0,
        "",
        "# Telemetry (anonymous usage data to help improve Inconvo)",
        `DISABLE_TELEMETRY=${enabled ? "false" : "true"}`
      );
    } else {
      newLines.push(`DISABLE_TELEMETRY=${enabled ? "false" : "true"}`);
    }
  }

  await fs.writeFile(envPath, newLines.join("\n"));
}

export const telemetryCommand = new Command("telemetry")
  .description("Manage anonymous telemetry settings")
  .argument("[action]", "Action to perform: on, off, or status")
  .action((action?: string) =>
    runCliAction(async () => {
      await ensureInconvoDir();

      if (!action || action === "status") {
        if (!(await envExists())) {
          logGray("Telemetry: enabled (default)");
          logGray("Run 'inconvo dev configure' to set up Inconvo.");
          return;
        }

        const config = await readEnvFile();
        const disabled = config.DISABLE_TELEMETRY === "true";

        if (disabled) {
          logInfo("Telemetry: disabled");
        } else {
          logInfo("Telemetry: enabled");
        }

        logGray("");
        logGray("Inconvo collects anonymous usage data to help improve the product.");
        logGray("No personal information, queries, or database content is collected.");
        logGray("");
        logGray("Run 'inconvo telemetry off' to disable or 'inconvo telemetry on' to enable.");
        return;
      }

      if (action === "on") {
        await updateTelemetrySetting(true);
        logInfo("Telemetry enabled");
        logGray("Thank you for helping improve Inconvo!");
        return;
      }

      if (action === "off") {
        await updateTelemetrySetting(false);
        logInfo("Telemetry disabled");
        return;
      }

      throw new Error(`Unknown action: ${action}. Usage: inconvo telemetry [on|off|status]`);
    }),
  );
