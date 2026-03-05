import { Command } from "commander";
import { connectionListCommand } from "./connection/list.js";
import { connectionListShareableCommand } from "./connection/list-shareable.js";
import { connectionLinkCommand } from "./connection/link.js";
import { connectionUnlinkCommand } from "./connection/unlink.js";
import { connectionSyncCommand } from "./connection/sync.js";

export const connectionCommand = new Command("connection")
  .description("Manage agent database connections")
  .addCommand(connectionListCommand)
  .addCommand(connectionListShareableCommand)
  .addCommand(connectionLinkCommand)
  .addCommand(connectionUnlinkCommand)
  .addCommand(connectionSyncCommand);
