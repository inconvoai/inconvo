#!/usr/bin/env node
import { Command } from "commander";
import { createRequire } from "module";
import { devCommand } from "./commands/dev.js";
import { configureCommand } from "./commands/configure.js";
import { telemetryCommand } from "./commands/telemetry.js";

const require = createRequire(import.meta.url);
const pkg = require("../package.json") as { version: string };

const program = new Command()
  .name("inconvo")
  .description("CLI for Inconvo development environment")
  .version(pkg.version);

program.addCommand(devCommand);
program.addCommand(configureCommand);
program.addCommand(telemetryCommand);

program.parse();
