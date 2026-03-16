import { Command } from "commander";
import { runCliAction } from "../../_shared/command-runtime.js";
import { resolveTable, resolveUserContextField } from "../../../model/resolution.js";
import { addCommonOptions, loadSnapshot, runMutation } from "./_shared.js";

const policyCommand = new Command("policy").description(
  "Table access-policy mutations",
);

addCommonOptions(
  policyCommand
    .command("set")
    .description("Set table access policy")
    .requiredOption("--table <table>", "Table id or name")
    .requiredOption("--field <field>", "User-context field id or key")
    .action((options) =>
      runCliAction(async () => {
        await runMutation({
          options,
          action: "policy.set",
          requireConnection: true,
          buildPayload: async (context) => {
            const [snapshot, userContext] = await Promise.all([
              loadSnapshot(context),
              context.client.getAgentUserContext(context.agentId),
            ]);
            const table = resolveTable(snapshot.tables, options.table);
            const field = resolveUserContextField(userContext, options.field);
            return {
              tableId: table.id,
              userContextFieldId: field.id,
            };
          },
          syncScope: "connection",
        });
      }),
    ),
  true,
);

addCommonOptions(
  policyCommand
    .command("clear")
    .description("Clear table access policy")
    .requiredOption("--table <table>", "Table id or name")
    .action((options) =>
      runCliAction(async () => {
        await runMutation({
          options,
          action: "policy.clear",
          requireConnection: true,
          buildPayload: async (context) => {
            const snapshot = await loadSnapshot(context);
            const table = resolveTable(snapshot.tables, options.table);
            return {
              tableId: table.id,
            };
          },
          syncScope: "connection",
        });
      }),
    ),
  true,
);

export { policyCommand };
