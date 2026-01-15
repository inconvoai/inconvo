export interface ConnectionInfo {
  id: string;
  name: string;
}

export interface TableConditionRef {
  id: string;
  tableName: string;
  columnName: string;
}

export interface ContextField {
  id: string;
  key: string;
  type: "STRING" | "NUMBER";
  tableConditions?: TableConditionRef[];
}

export interface TableCondition {
  id: string;
  table: {
    id: string;
    name: string;
    connection?: { id: string; name: string } | null;
  };
  column: { id: string; name: string };
  userContextField: { id: string; key: string };
}

export interface TableInfo {
  id: string;
  name: string;
  columns: Array<{ id: string; name: string; type: string }>;
}
