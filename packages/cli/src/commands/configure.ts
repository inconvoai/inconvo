import { Command } from "commander";
import { runSetupWizard } from "../wizard/setup.js";
import { findMonorepoRoot } from "../process/spawner.js";
import { logError } from "../process/output.js";

export const configureCommand = new Command("configure")
  .description("Configure Inconvo settings interactively")
  .action(async () => {
    const monorepoRoot = findMonorepoRoot();
    if (!monorepoRoot) {
      logError(
        "Could not find monorepo root. Make sure you are running this command from within the Inconvo repository."
      );
      process.exit(1);
    }

    await runSetupWizard(monorepoRoot);
  });
