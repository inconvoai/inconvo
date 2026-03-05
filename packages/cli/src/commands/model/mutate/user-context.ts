import { Command } from "commander";
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
    .requiredOption("--type <type>", "STRING|NUMBER|BOOLEAN")
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
          syncConnection: () => undefined,
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
          syncConnection: () => undefined,
        });
      }),
    ),
  false,
);

addCommonOptions(
  userContextCommand
    .command("set-status")
    .description("Set user-context status")
    .requiredOption("--status <status>", "ENABLED|DISABLED")
    .action((options) =>
      runCliAction(async () => {
        await runMutation({
          options,
          action: "userContext.setStatus",
          requireConnection: false,
          buildPayload: async () => ({
            status: options.status,
          }),
          syncConnection: () => undefined,
        });
      }),
    ),
  false,
);

export { userContextCommand };
