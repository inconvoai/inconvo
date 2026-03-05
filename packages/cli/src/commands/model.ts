import { Command } from "commander";
import { modelPullCommand } from "./model/pull.js";
import { modelMutationCommands } from "./model/mutate.js";
import { modelActionCommand } from "./model/action.js";
import { modelAgentCommand } from "./model/agent.js";

export const modelCommand = new Command("model")
  .description("Manage agent semantic models in this repository")
  .addCommand(modelAgentCommand)
  .addCommand(modelPullCommand)
  .addCommand(modelActionCommand);

for (const command of modelMutationCommands) {
  modelCommand.addCommand(command);
}
