#!/usr/bin/env node
import { runSetupWizard } from "./setup-wizard.js";

runSetupWizard().catch((error) => {
  console.error("Setup failed:", error);
  process.exit(1);
});
