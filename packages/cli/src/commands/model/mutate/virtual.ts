import { Command } from "commander";
import { runCliAction } from "../../_shared/command-runtime.js";
import { resolveTable } from "../../../model/resolution.js";
import { parseIntArg } from "../mutate-args.js";
import { addCommonOptions, loadSnapshot, runActionOnly, runMutation } from "./_shared.js";

const virtualCommand = new Command("virtual").description(
  "Virtual table mutations",
);

addCommonOptions(
  virtualCommand
    .command("validate-sql")
    .description("Validate virtual table SQL")
    .requiredOption("--sql <sql>", "SQL query")
    .option("--dialect <dialect>", "postgresql|mysql|redshift|bigquery|mssql")
    .option("--preview-limit <n>", "Preview limit", parseIntArg)
    .action((options) =>
      runCliAction(async () => {
        await runActionOnly({
          options,
          action: "virtual.validateSql",
          requireConnection: true,
          buildPayload: async (context) => ({
            connectionId: context.connectionId,
            sql: options.sql,
            ...(options.dialect !== undefined ? { dialect: options.dialect } : {}),
            ...(options.previewLimit !== undefined
              ? { previewLimit: options.previewLimit }
              : {}),
          }),
        });
      }),
    ),
  true,
);

addCommonOptions(
  virtualCommand
    .command("create")
    .description("Create virtual table")
    .requiredOption("--name <name>", "Virtual table name")
    .requiredOption("--sql <sql>", "SQL query")
    .option("--dialect <dialect>", "postgresql|mysql|redshift|bigquery|mssql")
    .option("--access <access>", "QUERYABLE|JOINABLE|OFF")
    .option("--context <context>", "Table context")
    .action((options) =>
      runCliAction(async () => {
        await runMutation({
          options,
          action: "virtual.create",
          requireConnection: true,
          buildPayload: async (context) => ({
            connectionId: context.connectionId,
            name: options.name,
            sql: options.sql,
            ...(options.dialect !== undefined ? { dialect: options.dialect } : {}),
            ...(options.access !== undefined ? { access: options.access } : {}),
            ...(options.context !== undefined ? { context: options.context } : {}),
          }),
          syncConnection: (context) => context.connectionId,
        });
      }),
    ),
  true,
);

addCommonOptions(
  virtualCommand
    .command("update-sql")
    .description("Update virtual table SQL")
    .requiredOption("--table <table>", "Virtual table id or name")
    .requiredOption("--sql <sql>", "SQL query")
    .option("--dialect <dialect>", "postgresql|mysql|redshift|bigquery|mssql")
    .action((options) =>
      runCliAction(async () => {
        await runMutation({
          options,
          action: "virtual.updateSql",
          requireConnection: true,
          buildPayload: async (context) => {
            const snapshot = await loadSnapshot(context);
            const table = resolveTable(snapshot.tables, options.table);
            return {
              tableId: table.id,
              sql: options.sql,
              ...(options.dialect !== undefined ? { dialect: options.dialect } : {}),
            };
          },
          syncConnection: (context) => context.connectionId,
        });
      }),
    ),
  true,
);

addCommonOptions(
  virtualCommand
    .command("refresh-columns")
    .description("Refresh virtual table columns from SQL")
    .requiredOption("--table <table>", "Virtual table id or name")
    .option("--preview-limit <n>", "Preview limit", parseIntArg)
    .action((options) =>
      runCliAction(async () => {
        await runMutation({
          options,
          action: "virtual.refreshColumns",
          requireConnection: true,
          buildPayload: async (context) => {
            const snapshot = await loadSnapshot(context);
            const table = resolveTable(snapshot.tables, options.table);
            return {
              tableId: table.id,
              ...(options.previewLimit !== undefined
                ? { previewLimit: options.previewLimit }
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
  virtualCommand
    .command("delete")
    .description("Delete virtual table")
    .requiredOption("--table <table>", "Virtual table id or name")
    .action((options) =>
      runCliAction(async () => {
        await runMutation({
          options,
          action: "virtual.delete",
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

export { virtualCommand };
