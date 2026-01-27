#!/usr/bin/env node
import { Command } from "commander";
import { devCommand } from "./commands/dev.js";
import { configureCommand } from "./commands/configure.js";

const program = new Command()
  .name("inconvo")
  .description("CLI for Inconvo development environment")
  .version("0.1.0");

program.addCommand(devCommand);
program.addCommand(configureCommand);

program.parse();
