import { tableCommand } from "./mutate/table.js";
import { columnCommand } from "./mutate/column.js";
import { computedCommand } from "./mutate/computed.js";
import { relationCommand } from "./mutate/relation.js";
import { conditionCommand } from "./mutate/condition.js";
import { policyCommand } from "./mutate/policy.js";
import { virtualCommand } from "./mutate/virtual.js";
import { userContextCommand } from "./mutate/user-context.js";

export const modelMutationCommands = [
  tableCommand,
  columnCommand,
  computedCommand,
  relationCommand,
  conditionCommand,
  policyCommand,
  virtualCommand,
  userContextCommand,
];
