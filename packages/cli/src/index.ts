#!/usr/bin/env node
import { Command } from "commander";
import { createRequire } from "module";
import { devCommand } from "./commands/dev.js";
import { telemetryCommand } from "./commands/telemetry.js";
import { modelCommand } from "./commands/model.js";
import { connectionCommand } from "./commands/connection.js";

const require = createRequire(import.meta.url);
const pkg = require("../package.json") as { version: string };

const program = new Command()
  .name("inconvo")
  .description("CLI for Inconvo development environment")
  .version(pkg.version);

program.addCommand(devCommand);
program.addCommand(telemetryCommand);
program.addCommand(modelCommand);
program.addCommand(connectionCommand);

program.parse();
