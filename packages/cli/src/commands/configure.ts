import { Command } from "commander";
import { runSetupWizard } from "../wizard/setup.js";

export const configureCommand = new Command("configure")
  .description("Configure Inconvo settings interactively")
  .action(async () => {
    await runSetupWizard();
  });
