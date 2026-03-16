import { Command } from "commander";
import { runCliAction } from "../../_shared/command-runtime.js";
import { resolveColumn, resolveTable } from "../../../model/resolution.js";
import { parseBooleanArg, parseJsonOption } from "../mutate-args.js";
import {
  addCommonOptions,
  loadSnapshot,
  runActionOnly,
  runMutation,
} from "./_shared.js";

const columnCommand = new Command("column").description(
  "Column-level mutations",
);

addCommonOptions(
  columnCommand
    .command("update")
    .description("Update column selected/rename/notes")
    .requiredOption("--table <table>", "Table id or name")
    .requiredOption("--column <column>", "Column id or name")
    .option("--selected <bool>", "Column selected state", parseBooleanArg)
    .option("--rename <name>", "Column rename value")
    .option("--clear-rename", "Clear rename")
    .option("--notes <notes>", "Column notes value")
    .option("--clear-notes", "Clear notes")
    .action((options) =>
      runCliAction(async () => {
        if (
          options.selected === undefined &&
          options.rename === undefined &&
          options.clearRename !== true &&
          options.notes === undefined &&
          options.clearNotes !== true
        ) {
          throw new Error(
            "Provide at least one of --selected, --rename, --clear-rename, --notes, --clear-notes.",
          );
        }
        await runMutation({
          options,
          action: "column.update",
          requireConnection: true,
          buildPayload: async (context) => {
            const snapshot = await loadSnapshot(context);
            const table = resolveTable(snapshot.tables, options.table);
            const column = resolveColumn(table, options.column);
            return {
              columnId: column.id,
              ...(options.selected !== undefined
                ? { selected: options.selected }
                : {}),
              ...(options.rename !== undefined
                ? { rename: options.rename }
                : options.clearRename === true
                  ? { rename: null }
                  : {}),
              ...(options.notes !== undefined
                ? { notes: options.notes }
                : options.clearNotes === true
                  ? { notes: null }
                  : {}),
            };
          },
          syncScope: "connection",
        });
      }),
    ),
  true,
);

addCommonOptions(
  columnCommand
    .command("set-unit")
    .description("Set or clear column unit")
    .requiredOption("--table <table>", "Table id or name")
    .requiredOption("--column <column>", "Column id or name")
    .option("--unit <unit>", "Unit value")
    .option("--clear", "Clear unit (set to null)")
    .action((options) =>
      runCliAction(async () => {
        if (options.unit === undefined && options.clear !== true) {
          throw new Error("Provide --unit or --clear.");
        }
        await runMutation({
          options,
          action: "column.setUnit",
          requireConnection: true,
          buildPayload: async (context) => {
            const snapshot = await loadSnapshot(context);
            const table = resolveTable(snapshot.tables, options.table);
            const column = resolveColumn(table, options.column);
            return {
              tableId: table.id,
              columnName: column.name,
              unit: options.clear === true ? null : options.unit,
            };
          },
          syncScope: "connection",
        });
      }),
    ),
  true,
);

const columnConversionCommand = new Command("conversion").description(
  "Column conversion augmentation",
);

addCommonOptions(
  columnConversionCommand
    .command("create")
    .description("Create conversion augmentation")
    .requiredOption("--table <table>", "Table id or name")
    .requiredOption("--column <column>", "Column id or name")
    .requiredOption("--ast <json>", "Conversion AST JSON")
    .option("--type <type>", "Optional conversion logical type")
    .option("--selected <bool>", "Selected state", parseBooleanArg)
    .action((options) =>
      runCliAction(async () => {
        await runMutation({
          options,
          action: "column.conversion.create",
          requireConnection: true,
          buildPayload: async (context) => {
            const snapshot = await loadSnapshot(context);
            const table = resolveTable(snapshot.tables, options.table);
            const column = resolveColumn(table, options.column);
            return {
              tableId: table.id,
              columnId: column.id,
              ast: parseJsonOption(options.ast, "--ast"),
              ...(options.type !== undefined ? { type: options.type } : {}),
              ...(options.selected !== undefined
                ? { selected: options.selected }
                : {}),
            };
          },
          syncScope: "connection",
        });
      }),
    ),
  true,
);

addCommonOptions(
  columnConversionCommand
    .command("update")
    .description("Update conversion augmentation")
    .requiredOption("--table <table>", "Table id or name")
    .requiredOption("--column <column>", "Column id or name")
    .option("--ast <json>", "Conversion AST JSON")
    .option("--type <type>", "Conversion logical type")
    .option("--clear-type", "Clear conversion type")
    .option("--selected <bool>", "Selected state", parseBooleanArg)
    .action((options) =>
      runCliAction(async () => {
        if (
          options.ast === undefined &&
          options.type === undefined &&
          options.clearType !== true &&
          options.selected === undefined
        ) {
          throw new Error(
            "Provide at least one of --ast, --type, --clear-type, --selected.",
          );
        }
        await runMutation({
          options,
          action: "column.conversion.update",
          requireConnection: true,
          buildPayload: async (context) => {
            const snapshot = await loadSnapshot(context);
            const table = resolveTable(snapshot.tables, options.table);
            const column = resolveColumn(table, options.column);
            return {
              columnId: column.id,
              ...(options.ast !== undefined
                ? { ast: parseJsonOption(options.ast, "--ast") }
                : {}),
              ...(options.type !== undefined
                ? { type: options.type }
                : options.clearType === true
                  ? { type: null }
                  : {}),
              ...(options.selected !== undefined
                ? { selected: options.selected }
                : {}),
            };
          },
          syncScope: "connection",
        });
      }),
    ),
  true,
);

addCommonOptions(
  columnConversionCommand
    .command("delete")
    .description("Delete conversion augmentation")
    .requiredOption("--table <table>", "Table id or name")
    .requiredOption("--column <column>", "Column id or name")
    .action((options) =>
      runCliAction(async () => {
        await runMutation({
          options,
          action: "column.conversion.delete",
          requireConnection: true,
          buildPayload: async (context) => {
            const snapshot = await loadSnapshot(context);
            const table = resolveTable(snapshot.tables, options.table);
            const column = resolveColumn(table, options.column);
            return {
              columnId: column.id,
            };
          },
          syncScope: "connection",
        });
      }),
    ),
  true,
);

const columnEnumCommand = new Command("enum").description(
  "Column enum augmentation",
);

addCommonOptions(
  columnEnumCommand
    .command("create-static")
    .description("Create static enum augmentation")
    .requiredOption("--table <table>", "Table id or name")
    .requiredOption("--column <column>", "Column id or name")
    .requiredOption(
      "--entries <json>",
      "JSON array of enum entries [{value,label,selected?,position?}]",
    )
    .option("--selected <bool>", "Selected state", parseBooleanArg)
    .action((options) =>
      runCliAction(async () => {
        await runMutation({
          options,
          action: "column.enum.createStatic",
          requireConnection: true,
          buildPayload: async (context) => {
            const snapshot = await loadSnapshot(context);
            const table = resolveTable(snapshot.tables, options.table);
            const column = resolveColumn(table, options.column);
            return {
              tableId: table.id,
              columnId: column.id,
              entries: parseJsonOption(options.entries, "--entries"),
              ...(options.selected !== undefined
                ? { selected: options.selected }
                : {}),
            };
          },
          syncScope: "connection",
        });
      }),
    ),
  true,
);

addCommonOptions(
  columnEnumCommand
    .command("create-dynamic")
    .description("Create dynamic enum augmentation")
    .requiredOption("--table <table>", "Table id or name")
    .requiredOption("--column <column>", "Column id or name")
    .option("--selected <bool>", "Selected state", parseBooleanArg)
    .action((options) =>
      runCliAction(async () => {
        await runMutation({
          options,
          action: "column.enum.createDynamic",
          requireConnection: true,
          buildPayload: async (context) => {
            const snapshot = await loadSnapshot(context);
            const table = resolveTable(snapshot.tables, options.table);
            const column = resolveColumn(table, options.column);
            return {
              tableId: table.id,
              columnId: column.id,
              ...(options.selected !== undefined
                ? { selected: options.selected }
                : {}),
            };
          },
          syncScope: "connection",
        });
      }),
    ),
  true,
);

addCommonOptions(
  columnEnumCommand
    .command("update")
    .description("Update enum augmentation")
    .requiredOption("--table <table>", "Table id or name")
    .requiredOption("--column <column>", "Column id or name")
    .option("--mode <mode>", "Expected current mode (STATIC|DYNAMIC)")
    .option("--entries <json>", "JSON array of enum entries")
    .option("--selected <bool>", "Selected state", parseBooleanArg)
    .action((options) =>
      runCliAction(async () => {
        if (options.entries === undefined && options.selected === undefined) {
          throw new Error("Provide at least one of --entries or --selected.");
        }
        await runMutation({
          options,
          action: "column.enum.update",
          requireConnection: true,
          buildPayload: async (context) => {
            const snapshot = await loadSnapshot(context);
            const table = resolveTable(snapshot.tables, options.table);
            const column = resolveColumn(table, options.column);
            const columnRecord = table.columns.find(
              (candidate) =>
                typeof candidate?.id === "string" && candidate.id === column.id,
            ) as { enumMode?: unknown } | undefined;
            const currentMode = columnRecord?.enumMode;
            if (currentMode !== "STATIC" && currentMode !== "DYNAMIC") {
              throw new Error(
                "Column does not have an enum augmentation. Use create-static or create-dynamic first.",
              );
            }
            if (
              options.mode !== undefined &&
              options.mode !== "STATIC" &&
              options.mode !== "DYNAMIC"
            ) {
              throw new Error("--mode must be STATIC or DYNAMIC.");
            }
            if (options.mode !== undefined && options.mode !== currentMode) {
              throw new Error(
                "Changing enum mode in-place is not supported. Delete and recreate the augmentation instead.",
              );
            }
            if (options.entries !== undefined && currentMode !== "STATIC") {
              throw new Error(
                "Dynamic enums do not accept --entries. Delete and recreate the augmentation to switch modes.",
              );
            }
            return {
              columnId: column.id,
              mode: currentMode,
              ...(options.entries !== undefined
                ? { entries: parseJsonOption(options.entries, "--entries") }
                : {}),
              ...(options.selected !== undefined
                ? { selected: options.selected }
                : {}),
            };
          },
          syncScope: "connection",
        });
      }),
    ),
  true,
);

addCommonOptions(
  columnEnumCommand
    .command("delete")
    .description("Delete enum augmentation")
    .requiredOption("--table <table>", "Table id or name")
    .requiredOption("--column <column>", "Column id or name")
    .action((options) =>
      runCliAction(async () => {
        await runMutation({
          options,
          action: "column.enum.delete",
          requireConnection: true,
          buildPayload: async (context) => {
            const snapshot = await loadSnapshot(context);
            const table = resolveTable(snapshot.tables, options.table);
            const column = resolveColumn(table, options.column);
            return {
              columnId: column.id,
            };
          },
          syncScope: "connection",
        });
      }),
    ),
  true,
);

addCommonOptions(
  columnEnumCommand
    .command("autofill")
    .description("Fetch suggested enum entries from distinct values")
    .requiredOption("--table <table>", "Table id or name")
    .requiredOption("--column <column>", "Column id or name")
    .action((options) =>
      runCliAction(async () => {
        await runActionOnly({
          options,
          action: "column.enum.autofill",
          requireConnection: true,
          buildPayload: async (context) => {
            const snapshot = await loadSnapshot(context);
            const table = resolveTable(snapshot.tables, options.table);
            const column = resolveColumn(table, options.column);
            return {
              tableId: table.id,
              columnId: column.id,
            };
          },
        });
      }),
    ),
  true,
);

columnCommand.addCommand(columnConversionCommand);
columnCommand.addCommand(columnEnumCommand);

export { columnCommand };
