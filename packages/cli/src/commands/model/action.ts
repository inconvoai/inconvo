import { Command } from "commander";
import { runCliAction } from "../_shared/command-runtime.js";
import { parseJsonOption } from "./mutate-args.js";
import {
  printDryRunOutput,
  printMutationOutput,
  resolveMutationContext,
  runActionAndSync,
} from "./mutate/_shared.js";
import {
  MODEL_ACTION_DEFINITIONS,
  resolveActionDefinition,
} from "../../model/action-registry.js";
import { DEFAULT_API_BASE_URL } from "../../model/cli-options.js";
import type { ModelActionType } from "../../model/types.js";

const actionRunCommand = new Command("run")
  .description("Run a model action from raw JSON payload")
  .requiredOption("--agent <agentId>", "Target agent id")
  .requiredOption("--action <action>", "Action type")
  .requiredOption("--payload <json>", "Action payload JSON")
  .option("--connection <connectionId>", "Optional connection id for sync scope")
  .option("--api-key <apiKey>", "API key override (otherwise INCONVO_API_KEY)")
  .option(
    "--api-base-url <url>",
    `API base URL override (default: ${DEFAULT_API_BASE_URL})`,
  )
  .option("--dry-run", "Resolve and print action payload without applying")
  .option("--json", "Print JSON output")
  .option("--no-sync", "Skip post-mutation sync")
  .action((options) =>
    runCliAction(async () => {
      const actionDefinition = resolveActionDefinition(options.action);
      if (!actionDefinition) {
        throw new Error(
          `Unknown action "${options.action}". Run 'inconvo model action schema --json' for supported actions.`,
        );
      }

      const context = await resolveMutationContext({
        options,
        requireConnection: false,
      });
      if (actionDefinition.requiresConnection && !context.connectionId) {
        throw new Error(
          `Action "${actionDefinition.action}" requires --connection.`,
        );
      }

      const payload = parseJsonOption(options.payload, "--payload");
      const syncScope = actionDefinition.requiresConnection ? "connection" as const : undefined;
      if (context.dryRun) {
        printDryRunOutput({
          context,
          action: actionDefinition.action,
          payload,
          syncScope,
        });
        return;
      }

      const response = await runActionAndSync({
        context,
        action: actionDefinition.action as ModelActionType,
        payload,
        syncScope,
      });
      printMutationOutput({
        context,
        action: actionDefinition.action,
        result: response.actionResult,
        sync: response.sync,
      });
    }),
  );

const actionSchemaCommand = new Command("schema")
  .description("List supported model actions and metadata")
  .option("--json", "Print JSON output")
  .action((options: { json?: boolean }) => {
    if (options.json) {
      console.log(
        JSON.stringify(
          {
            actions: MODEL_ACTION_DEFINITIONS,
          },
          null,
          2,
        ),
      );
      return;
    }

    console.log("Supported model actions:\n");
    for (const definition of MODEL_ACTION_DEFINITIONS) {
      const connectionHint = definition.requiresConnection
        ? " (requires --connection)"
        : "";
      console.log(`  - ${definition.action}${connectionHint}`);
      console.log(`    ${definition.description}`);
    }
  });

export const modelActionCommand = new Command("action")
  .description("Run generic model actions and inspect action schema")
  .addCommand(actionRunCommand)
  .addCommand(actionSchemaCommand);
