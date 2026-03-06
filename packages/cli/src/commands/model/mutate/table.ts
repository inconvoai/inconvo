import { Command } from "commander";
import { runCliAction } from "../../_shared/command-runtime.js";
import { resolveTable } from "../../../model/resolution.js";
import { addCommonOptions, loadSnapshot, runMutation } from "./_shared.js";

const tableCommand = new Command("table").description("Table-level mutations");

addCommonOptions(
  tableCommand
    .command("set-access")
    .description("Set table access mode")
    .requiredOption("--table <table>", "Table id or name")
    .requiredOption("--access <access>", "QUERYABLE|JOINABLE|OFF")
    .action((options) =>
      runCliAction(async () => {
        await runMutation({
          options,
          action: "table.setAccess",
          requireConnection: true,
          buildPayload: async (context) => {
            const snapshot = await loadSnapshot(context);
            const table = resolveTable(snapshot.tables, options.table);
            return {
              tableId: table.id,
              access: options.access,
            };
          },
          syncConnection: (context) => context.connectionId,
        });
      }),
    ),
  true,
);

addCommonOptions(
  tableCommand
    .command("set-context")
    .description("Set or clear table context")
    .requiredOption("--table <table>", "Table id or name")
    .option("--context <context>", "Context text")
    .option("--clear", "Clear context (set to null)")
    .action((options) =>
      runCliAction(async () => {
        if (!options.context && options.clear !== true) {
          throw new Error("Provide --context or --clear.");
        }
        await runMutation({
          options,
          action: "table.setContext",
          requireConnection: true,
          buildPayload: async (context) => {
            const snapshot = await loadSnapshot(context);
            const table = resolveTable(snapshot.tables, options.table);
            return {
              tableId: table.id,
              context: options.clear === true ? null : options.context,
            };
          },
          syncConnection: (context) => context.connectionId,
        });
      }),
    ),
  true,
);

export { tableCommand };
