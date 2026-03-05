import {
  MODEL_ACTION_TYPES,
  type ModelActionType,
} from "./types.js";

export interface ModelActionDefinition {
  action: ModelActionType;
  description: string;
  requiresConnection: boolean;
}

const ACTION_DESCRIPTION_MAP: Record<ModelActionType, string> = {
  "table.setAccess": "Set table access mode",
  "table.setContext": "Set or clear table context",
  "column.update": "Update column fields (selected/rename/notes)",
  "column.setUnit": "Set or clear a column unit",
  "column.conversion.create": "Create conversion augmentation",
  "column.conversion.update": "Update conversion augmentation",
  "column.conversion.delete": "Delete conversion augmentation",
  "column.enum.createStatic": "Create static enum augmentation",
  "column.enum.createDynamic": "Create dynamic enum augmentation",
  "column.enum.update": "Update enum augmentation",
  "column.enum.delete": "Delete enum augmentation",
  "column.enum.autofill": "Fetch distinct values for enum autofill",
  "computed.create": "Create computed column",
  "computed.update": "Update computed column fields",
  "computed.delete": "Delete computed column",
  "computed.setUnit": "Set or clear computed column unit",
  "relation.toggle": "Toggle relation selected state",
  "relation.manual.create": "Create manual relation",
  "relation.manual.update": "Update manual relation",
  "relation.manual.delete": "Delete manual relation",
  "condition.set": "Set table condition",
  "condition.clear": "Clear table condition",
  "policy.set": "Set table access policy",
  "policy.clear": "Clear table access policy",
  "virtual.validateSql": "Validate virtual table SQL",
  "virtual.create": "Create virtual table",
  "virtual.updateSql": "Update virtual table SQL",
  "virtual.refreshColumns": "Refresh virtual table columns",
  "virtual.delete": "Delete virtual table",
  "userContext.addField": "Add user-context field",
  "userContext.deleteField": "Delete user-context field",
  "userContext.setStatus": "Set user-context status",
};

const ACTIONS_REQUIRING_CONNECTION = new Set<ModelActionType>([
  "table.setAccess",
  "table.setContext",
  "column.update",
  "column.setUnit",
  "column.conversion.create",
  "column.conversion.update",
  "column.conversion.delete",
  "column.enum.createStatic",
  "column.enum.createDynamic",
  "column.enum.update",
  "column.enum.delete",
  "column.enum.autofill",
  "computed.create",
  "computed.update",
  "computed.delete",
  "computed.setUnit",
  "relation.toggle",
  "relation.manual.create",
  "relation.manual.update",
  "relation.manual.delete",
  "condition.set",
  "condition.clear",
  "policy.set",
  "policy.clear",
  "virtual.validateSql",
  "virtual.create",
  "virtual.updateSql",
  "virtual.refreshColumns",
  "virtual.delete",
]);

export const MODEL_ACTION_DEFINITIONS: ModelActionDefinition[] =
  MODEL_ACTION_TYPES.map((action) => ({
    action,
    description: ACTION_DESCRIPTION_MAP[action],
    requiresConnection: ACTIONS_REQUIRING_CONNECTION.has(action),
  }));

const ACTION_LOOKUP = new Map(
  MODEL_ACTION_DEFINITIONS.map((definition) => [definition.action, definition]),
);

export function resolveActionDefinition(
  action: string,
): ModelActionDefinition | undefined {
  return ACTION_LOOKUP.get(action as ModelActionType);
}
