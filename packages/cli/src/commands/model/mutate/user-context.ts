import { Command, Option } from "commander";
import { runCliAction } from "../../_shared/command-runtime.js";
import { resolveUserContextField } from "../../../model/resolution.js";
import { addCommonOptions, runMutation } from "./_shared.js";

const userContextCommand = new Command("user-context").description(
  "User-context mutations",
);

addCommonOptions(
  userContextCommand
    .command("add-field")
    .description("Add user-context field")
    .requiredOption("--key <key>", "Field key")
    .addOption(
      new Option("--type <type>", "Field type")
        .choices(["STRING", "NUMBER", "BOOLEAN"])
        .makeOptionMandatory(),
    )
    .action((options) =>
      runCliAction(async () => {
        await runMutation({
          options,
          action: "userContext.addField",
          requireConnection: false,
          buildPayload: async () => ({
            key: options.key,
            type: options.type,
          }),
          syncScope: "user-context",
        });
      }),
    ),
  false,
);

addCommonOptions(
  userContextCommand
    .command("delete-field")
    .description("Delete user-context field")
    .requiredOption("--field <field>", "Field id or key")
    .action((options) =>
      runCliAction(async () => {
        await runMutation({
          options,
          action: "userContext.deleteField",
          requireConnection: false,
          buildPayload: async (context) => {
            const userContext = await context.client.getAgentUserContext(
              context.agentId,
            );
            const field = resolveUserContextField(userContext, options.field);
            return {
              id: field.id,
            };
          },
          syncScope: "user-context",
        });
      }),
    ),
  false,
);

addCommonOptions(
  userContextCommand
    .command("set-status")
    .description("Set user-context status")
    .addOption(
      new Option("--status <status>", "User-context status")
        .choices(["ENABLED", "DISABLED"])
        .makeOptionMandatory(),
    )
    .action((options) =>
      runCliAction(async () => {
        await runMutation({
          options,
          action: "userContext.setStatus",
          requireConnection: false,
          buildPayload: async () => ({
            status: options.status,
          }),
          syncScope: "user-context",
        });
      }),
    ),
  false,
);

export { userContextCommand };
