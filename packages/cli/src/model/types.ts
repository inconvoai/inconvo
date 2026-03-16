export type UserContextFieldType = "STRING" | "NUMBER" | "BOOLEAN";
export type UserContextStatus = "UNSET" | "ENABLED" | "DISABLED";

export interface OrgInfo {
  id: string;
  name: string;
}

export interface AgentInfo {
  id: string;
  name: string;
}

export interface ConnectionInfo {
  id: string;
  name: string;
  description: string | null;
  status: string | null;
  isShared: boolean;
  ownerAgentName: string;
}

export interface ConnectionDetails extends ConnectionInfo {
  canEdit: boolean;
}

export interface ShareableConnectionInfo {
  id: string;
  name: string;
  description: string | null;
  databaseType: string | null;
  ownerAgent: { id: string; name: string };
}

export interface ConnectionLinkResponse {
  success: boolean;
  connectionId: string;
}

export interface ConnectionDescriptionUpdateResponse {
  id: string;
  name: string;
  description: string | null;
}

export interface UserContextField {
  id?: string;
  key: string;
  type: UserContextFieldType;
}

export interface UserContextData {
  status: UserContextStatus;
  fields: UserContextField[];
}

export interface UserContextResponse {
  userContext: UserContextData;
  hash: string;
}

export const MODEL_ACTION_TYPES = [
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
  "userContext.addField",
  "userContext.deleteField",
  "userContext.setStatus",
] as const;

export type ModelActionType = (typeof MODEL_ACTION_TYPES)[number];

export type TableSource = "PHYSICAL" | "VIRTUAL";
export type TableAccess = "QUERYABLE" | "JOINABLE" | "OFF";
export type VirtualTableDialect =
  | "postgresql"
  | "mysql"
  | "redshift"
  | "bigquery"
  | "mssql";

export interface ConnectionSemanticModelCondition {
  column: { id: string };
  userContextField: { id: string };
}

export interface ConnectionSemanticModelAccessPolicy {
  userContextField: { id: string };
}

export interface ConnectionSemanticModelVirtualTableConfig {
  sql?: string;
  dialect?: VirtualTableDialect | null;
}

export interface ConnectionSemanticModelTable
  extends Record<string, unknown> {
  id: string;
  name: string;
  access: TableAccess;
  columns: Array<Record<string, unknown>>;
  computedColumns: Array<Record<string, unknown>>;
  outwardRelations: Array<Record<string, unknown>>;
  source?: TableSource;
  context?: string | null;
  condition?: ConnectionSemanticModelCondition | null;
  accessPolicy?: ConnectionSemanticModelAccessPolicy | null;
  virtualTableConfig?: ConnectionSemanticModelVirtualTableConfig | null;
}

export interface ConnectionSemanticModelResponse {
  connectionId: string;
  tables: ConnectionSemanticModelTable[];
  hash: string;
}

export interface ModelActionResponse {
  action: ModelActionType;
  result: unknown;
}
