import { Command } from "commander";
import { runCliAction } from "../../_shared/command-runtime.js";
import { resolveRelation, resolveTable } from "../../../model/resolution.js";
import { collectPair, parseBooleanArg, parseColumnPairs } from "../mutate-args.js";
import { addCommonOptions, loadSnapshot, runMutation } from "./_shared.js";

const relationCommand = new Command("relation").description("Relation mutations");

addCommonOptions(
  relationCommand
    .command("toggle")
    .description("Toggle relation selected state")
    .requiredOption("--table <table>", "Source table id or name")
    .requiredOption("--relation <relation>", "Relation id or name")
    .requiredOption("--selected <bool>", "Selected state", parseBooleanArg)
    .action((options) =>
      runCliAction(async () => {
        await runMutation({
          options,
          action: "relation.toggle",
          requireConnection: true,
          buildPayload: async (context) => {
            const snapshot = await loadSnapshot(context);
            const sourceTable = resolveTable(snapshot.tables, options.table);
            const relation = resolveRelation(sourceTable, options.relation);
            return {
              relationId: relation.id,
              selected: options.selected,
            };
          },
          syncScope: "connection",
        });
      }),
    ),
  true,
);

const manualRelationCommand = new Command("manual").description(
  "Manual relation lifecycle",
);

addCommonOptions(
  manualRelationCommand
    .command("create")
    .description("Create a manual relation")
    .requiredOption("--source-table <table>", "Source table id or name")
    .requiredOption("--target-table <table>", "Target table id or name")
    .requiredOption("--name <name>", "Relation name")
    .requiredOption("--is-list <bool>", "Whether relation is list", parseBooleanArg)
    .requiredOption(
      "--pair <source:target>",
      "Column pair mapping, repeatable",
      collectPair,
      [],
    )
    .action((options) =>
      runCliAction(async () => {
        const columnPairs = parseColumnPairs(options.pair);
        await runMutation({
          options,
          action: "relation.manual.create",
          requireConnection: true,
          buildPayload: async (context) => {
            const snapshot = await loadSnapshot(context);
            const sourceTable = resolveTable(
              snapshot.tables,
              options.sourceTable,
            );
            const targetTable = resolveTable(
              snapshot.tables,
              options.targetTable,
            );
            return {
              sourceTableId: sourceTable.id,
              targetTableId: targetTable.id,
              name: options.name,
              isList: options.isList,
              columnPairs,
            };
          },
          syncScope: "connection",
        });
      }),
    ),
  true,
);

addCommonOptions(
  manualRelationCommand
    .command("update")
    .description("Update a manual relation")
    .requiredOption("--source-table <table>", "Source table id or name")
    .requiredOption("--relation <relation>", "Relation id or name")
    .option("--name <name>", "Relation name")
    .option("--is-list <bool>", "Whether relation is list", parseBooleanArg)
    .option("--target-table <table>", "Target table id or name")
    .option(
      "--pair <source:target>",
      "Column pair mapping, repeatable",
      collectPair,
      [],
    )
    .action((options) =>
      runCliAction(async () => {
        if (
          options.name === undefined &&
          options.isList === undefined &&
          options.targetTable === undefined &&
          (!Array.isArray(options.pair) || options.pair.length === 0)
        ) {
          throw new Error(
            "Provide at least one of --name, --is-list, --target-table, --pair.",
          );
        }

        await runMutation({
          options,
          action: "relation.manual.update",
          requireConnection: true,
          buildPayload: async (context) => {
            const snapshot = await loadSnapshot(context);
            const sourceTable = resolveTable(
              snapshot.tables,
              options.sourceTable,
            );
            const relation = resolveRelation(sourceTable, options.relation);
            const payload: Record<string, unknown> = {
              relationId: relation.id,
            };

            if (options.name !== undefined) {
              payload.name = options.name;
            }
            if (options.isList !== undefined) {
              payload.isList = options.isList;
            }
            if (options.targetTable !== undefined) {
              const targetTable = resolveTable(
                snapshot.tables,
                options.targetTable,
              );
              payload.targetTableId = targetTable.id;
            }
            if (Array.isArray(options.pair) && options.pair.length > 0) {
              payload.columnPairs = parseColumnPairs(options.pair);
            }

            return payload;
          },
          syncScope: "connection",
        });
      }),
    ),
  true,
);

addCommonOptions(
  manualRelationCommand
    .command("delete")
    .description("Delete a manual relation")
    .requiredOption("--source-table <table>", "Source table id or name")
    .requiredOption("--relation <relation>", "Relation id or name")
    .action((options) =>
      runCliAction(async () => {
        await runMutation({
          options,
          action: "relation.manual.delete",
          requireConnection: true,
          buildPayload: async (context) => {
            const snapshot = await loadSnapshot(context);
            const sourceTable = resolveTable(
              snapshot.tables,
              options.sourceTable,
            );
            const relation = resolveRelation(sourceTable, options.relation);
            return {
              relationId: relation.id,
            };
          },
          syncScope: "connection",
        });
      }),
    ),
  true,
);

relationCommand.addCommand(manualRelationCommand);

export { relationCommand };
