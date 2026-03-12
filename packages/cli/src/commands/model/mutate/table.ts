import { Command, Option } from "commander";
import { runCliAction } from "../../_shared/command-runtime.js";
import { resolveTable } from "../../../model/resolution.js";
import { addCommonOptions, loadSnapshot, runMutation } from "./_shared.js";

const tableCommand = new Command("table").description("Table-level mutations");

addCommonOptions(
  tableCommand
    .command("set-access")
    .description("Set table access mode")
    .requiredOption("--table <table>", "Table id or name")
    .addOption(
      new Option("--access <access>", "Table access mode")
        .choices(["QUERYABLE", "JOINABLE", "OFF"])
        .makeOptionMandatory(),
    )
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
          syncScope: "connection",
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
          syncScope: "connection",
        });
      }),
    ),
  true,
);

export { tableCommand };
