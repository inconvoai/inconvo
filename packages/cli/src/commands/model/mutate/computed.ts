import { Command } from "commander";
import { runCliAction } from "../../_shared/command-runtime.js";
import {
  resolveComputedColumn,
  resolveTable,
} from "../../../model/resolution.js";
import { parseBooleanArg, parseJsonOption } from "../mutate-args.js";
import { addCommonOptions, loadSnapshot, runMutation } from "./_shared.js";

const computedCommand = new Command("computed").description(
  "Computed column mutations",
);

addCommonOptions(
  computedCommand
    .command("create")
    .description("Create computed column")
    .requiredOption("--table <table>", "Table id or name")
    .requiredOption("--name <name>", "Computed column name")
    .requiredOption("--ast <json>", "Computed column AST JSON")
    .option("--unit <unit>", "Optional unit")
    .action((options) =>
      runCliAction(async () => {
        await runMutation({
          options,
          action: "computed.create",
          requireConnection: true,
          buildPayload: async (context) => {
            const snapshot = await loadSnapshot(context);
            const table = resolveTable(snapshot.tables, options.table);
            return {
              tableId: table.id,
              tableName: table.name,
              name: options.name,
              ast: parseJsonOption(options.ast, "--ast"),
              ...(options.unit !== undefined ? { unit: options.unit } : {}),
            };
          },
          syncConnection: (context) => context.connectionId,
        });
      }),
    ),
  true,
);

addCommonOptions(
  computedCommand
    .command("update")
    .description("Update computed column fields")
    .requiredOption("--table <table>", "Table id or name")
    .requiredOption("--computed <computed>", "Computed column id or name")
    .option("--ast <json>", "Computed column AST JSON")
    .option("--name <name>", "New name")
    .option("--selected <bool>", "Selected state", parseBooleanArg)
    .option("--notes <notes>", "Notes value")
    .option("--clear-notes", "Clear notes")
    .option("--unit <unit>", "Unit value")
    .option("--clear-unit", "Clear unit")
    .action((options) =>
      runCliAction(async () => {
        if (
          options.ast === undefined &&
          options.name === undefined &&
          options.selected === undefined &&
          options.notes === undefined &&
          options.clearNotes !== true &&
          options.unit === undefined &&
          options.clearUnit !== true
        ) {
          throw new Error(
            "Provide at least one of --ast, --name, --selected, --notes, --clear-notes, --unit, --clear-unit.",
          );
        }
        await runMutation({
          options,
          action: "computed.update",
          requireConnection: true,
          buildPayload: async (context) => {
            const snapshot = await loadSnapshot(context);
            const table = resolveTable(snapshot.tables, options.table);
            const computed = resolveComputedColumn(table, options.computed);
            return {
              computedColumnId: computed.id,
              ...(options.ast !== undefined
                ? { ast: parseJsonOption(options.ast, "--ast") }
                : {}),
              ...(options.name !== undefined ? { name: options.name } : {}),
              ...(options.selected !== undefined
                ? { selected: options.selected }
                : {}),
              ...(options.notes !== undefined
                ? { notes: options.notes }
                : options.clearNotes === true
                  ? { notes: null }
                  : {}),
              ...(options.unit !== undefined
                ? { unit: options.unit }
                : options.clearUnit === true
                  ? { unit: null }
                  : {}),
            };
          },
          syncConnection: (context) => context.connectionId,
        });
      }),
    ),
  true,
);

addCommonOptions(
  computedCommand
    .command("delete")
    .description("Delete computed column")
    .requiredOption("--table <table>", "Table id or name")
    .requiredOption("--computed <computed>", "Computed column id or name")
    .action((options) =>
      runCliAction(async () => {
        await runMutation({
          options,
          action: "computed.delete",
          requireConnection: true,
          buildPayload: async (context) => {
            const snapshot = await loadSnapshot(context);
            const table = resolveTable(snapshot.tables, options.table);
            const computed = resolveComputedColumn(table, options.computed);
            return {
              computedColumnId: computed.id,
            };
          },
          syncConnection: (context) => context.connectionId,
        });
      }),
    ),
  true,
);

addCommonOptions(
  computedCommand
    .command("set-unit")
    .description("Set or clear computed column unit")
    .requiredOption("--table <table>", "Table id or name")
    .requiredOption("--computed <computed>", "Computed column id or name")
    .option("--unit <unit>", "Unit value")
    .option("--clear", "Clear unit")
    .action((options) =>
      runCliAction(async () => {
        if (options.unit === undefined && options.clear !== true) {
          throw new Error("Provide --unit or --clear.");
        }
        await runMutation({
          options,
          action: "computed.setUnit",
          requireConnection: true,
          buildPayload: async (context) => {
            const snapshot = await loadSnapshot(context);
            const table = resolveTable(snapshot.tables, options.table);
            const computed = resolveComputedColumn(table, options.computed);
            return {
              computedColumnId: computed.id,
              unit: options.clear === true ? null : options.unit,
            };
          },
          syncConnection: (context) => context.connectionId,
        });
      }),
    ),
  true,
);

export { computedCommand };
