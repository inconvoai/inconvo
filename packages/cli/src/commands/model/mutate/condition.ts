import { Command } from "commander";
import { runCliAction } from "../../_shared/command-runtime.js";
import {
  resolveColumn,
  resolveTable,
  resolveUserContextField,
} from "../../../model/resolution.js";
import { addCommonOptions, loadSnapshot, runMutation } from "./_shared.js";

const conditionCommand = new Command("condition").description(
  "Table condition mutations",
);

addCommonOptions(
  conditionCommand
    .command("set")
    .description("Set table condition")
    .requiredOption("--table <table>", "Table id or name")
    .requiredOption("--column <column>", "Column id or name")
    .requiredOption("--field <field>", "User-context field id or key")
    .action((options) =>
      runCliAction(async () => {
        await runMutation({
          options,
          action: "condition.set",
          requireConnection: true,
          buildPayload: async (context) => {
            const [snapshot, userContext] = await Promise.all([
              loadSnapshot(context),
              context.client.getAgentUserContext(context.agentId),
            ]);
            const table = resolveTable(snapshot.tables, options.table);
            const column = resolveColumn(table, options.column);
            const field = resolveUserContextField(userContext, options.field);
            return {
              tableId: table.id,
              columnId: column.id,
              userContextFieldId: field.id,
            };
          },
          syncConnection: (context) => context.connectionId,
        });
      }),
    ),
  true,
);

addCommonOptions(
  conditionCommand
    .command("clear")
    .description("Clear table condition")
    .requiredOption("--table <table>", "Table id or name")
    .action((options) =>
      runCliAction(async () => {
        await runMutation({
          options,
          action: "condition.clear",
          requireConnection: true,
          buildPayload: async (context) => {
            const snapshot = await loadSnapshot(context);
            const table = resolveTable(snapshot.tables, options.table);
            return {
              tableId: table.id,
            };
          },
          syncConnection: (context) => context.connectionId,
        });
      }),
    ),
  true,
);

export { conditionCommand };
