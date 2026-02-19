// Re-export core types from @repo/types that can be used directly
export type {
  TableAccess,
  RelationSource,
  RelationStatus,
  LogicalCastType,
  SQLComputedColumnAst,
  SQLCastExpressionAst,
  UserContextStatus,
} from "@repo/types";

// Import for internal use
import type {
  TableAccess,
  RelationSource,
  RelationStatus,
  LogicalCastType,
} from "@repo/types";

// =============================================================================
// Column Types
// =============================================================================

/**
 * Column conversion (type cast) configuration
 */
export interface ColumnConversion {
  id: string;
  ast: unknown;
  type: string | null;
  selected: boolean;
}

export interface ColumnValueEnumEntry {
  value: string | number;
  label: string;
  selected: boolean;
  position: number;
}

export interface ColumnValueEnum {
  id: string;
  selected: boolean;
  entries: ColumnValueEnumEntry[];
}

export interface ColumnValueEnumEntryInput {
  value: string | number;
  label: string;
  selected?: boolean;
  position?: number;
}

export type ColumnValueEnumMode = "STATIC" | "DYNAMIC";

/**
 * Relation mapping info shown on a column (for display purposes)
 */
export interface ColumnRelationMapping {
  relation: {
    targetTable: { name: string | null };
  };
  targetColumn: { name: string | null };
}

/**
 * Full column representation for the UI
 */
export interface Column {
  id: string;
  name: string;
  rename: string | null;
  notes: string | null;
  type: string;
  effectiveType: string;
  selected: boolean;
  unit: string | null;
  conversion: ColumnConversion | null;
  enumMode: ColumnValueEnumMode | null;
  valueEnum?: ColumnValueEnum | null;
  relation: ColumnRelationMapping[];
}

// =============================================================================
// Computed Column Types
// =============================================================================

/**
 * Computed column (virtual column defined by expression)
 */
export interface ComputedColumn {
  id: string;
  name: string;
  ast: unknown;
  selected: boolean;
  type: string;
  unit: string | null;
  notes: string | null;
}

// =============================================================================
// Relation Types
// =============================================================================

/**
 * Column mapping in a relation (source -> target)
 */
export interface RelationColumnMapping {
  id: string;
  position: number;
  sourceColumnName: string;
  targetColumnName: string;
}

/**
 * Target table info for a relation
 */
export interface RelationTargetTable {
  access: TableAccess;
  name: string;
}

/**
 * Outward relation from a table
 */
export interface Relation {
  id: string;
  name: string;
  relationId: string | null;
  targetTable: RelationTargetTable;
  targetTableId: string;
  isList: boolean;
  selected: boolean;
  source: RelationSource;
  status: RelationStatus;
  errorTag: string | null;
  columnMappings: RelationColumnMapping[];
}

// =============================================================================
// Table Condition (Row-Level Access Constraints)
// =============================================================================

/**
 * Row-level access constraint on a table
 */
export interface TableCondition {
  column: { id: string; name: string };
  userContextField: { id: string; key: string };
}

/**
 * Table-level access policy based on user context.
 */
export interface TableAccessPolicy {
  userContextField: { id: string; key: string };
}

// =============================================================================
// Table Types
// =============================================================================

/**
 * Full table schema with all details (for detail view)
 */
export interface TableSchema {
  id: string;
  name: string;
  access: TableAccess;
  context: string | null;
  columns: Column[];
  computedColumns: ComputedColumn[];
  relations: Relation[];
  condition: TableCondition | null;
  accessPolicy: TableAccessPolicy | null;
}

/**
 * Table summary for list view (lightweight)
 */
export interface TableSummary {
  id: string;
  name: string;
  access: TableAccess;
  columnCount: number;
  selectedColumnCount: number;
  computedColumnCount: number;
  relationCount: number;
  hasCondition: boolean;
  hasAccessPolicy: boolean;
}

// =============================================================================
// Supporting Types (for forms)
// =============================================================================

/**
 * User context field (for context filter form)
 */
export interface UserContextField {
  id: string;
  key: string;
  type: "STRING" | "NUMBER" | "BOOLEAN";
}

/**
 * Table with columns (for manual relation form - target table picker)
 */
export interface TableWithColumns {
  id: string;
  name: string;
  columns: Array<{ id: string; name: string; type?: string }>;
}

// =============================================================================
// Callback Types
// =============================================================================

/**
 * Column update payload
 */
export interface ColumnUpdatePayload {
  selected?: boolean;
  rename?: string | null;
  notes?: string | null;
}

/**
 * Computed column create payload
 */
export interface ComputedColumnCreatePayload {
  name: string;
  ast: unknown;
  unit: string | null;
}

/**
 * Computed column update payload
 */
export interface ComputedColumnUpdatePayload {
  selected?: boolean;
  name?: string;
  notes?: string | null;
  unit?: string | null;
}

/**
 * Manual relation create payload
 */
export interface ManualRelationCreatePayload {
  name: string;
  isList: boolean;
  targetTableId: string;
  columnPairs: Array<{
    sourceColumnName: string;
    targetColumnName: string;
  }>;
}

/**
 * Manual relation update payload
 */
export interface ManualRelationUpdatePayload {
  name: string;
  isList: boolean;
  targetTableId: string;
  columnPairs: Array<{
    sourceColumnName: string;
    targetColumnName: string;
  }>;
}

/**
 * Context filter upsert payload
 */
export interface ContextFilterPayload {
  columnId: string;
  userContextFieldId: string;
}

/**
 * Table access policy upsert payload.
 */
export interface TableAccessPolicyPayload {
  userContextFieldId: string;
}

/**
 * Column unit payload (for regular columns)
 */
export interface ColumnUnitPayload {
  columnName: string;
  unit: string;
}

/**
 * Computed column unit payload
 */
export interface ComputedColumnUnitPayload {
  computedColumnId: string;
  unit: string | null;
}

/**
 * Column conversion create payload
 */
export interface ColumnConversionCreatePayload {
  columnId: string;
  type: LogicalCastType;
  ast: unknown;
  selected: boolean;
}

/**
 * Column conversion update payload
 */
export interface ColumnConversionUpdatePayload {
  type: LogicalCastType;
  ast: unknown;
  selected: boolean;
}

/**
 * Column value enum create payload
 */
export type ColumnValueEnumCreatePayload =
  | {
      mode: "STATIC";
      entries: ColumnValueEnumEntryInput[];
      selected?: boolean;
    }
  | {
      mode: "DYNAMIC";
      selected?: boolean;
    };

/**
 * Column value enum update payload
 */
export type ColumnValueEnumUpdatePayload =
  | {
      mode: "STATIC";
      entries?: ColumnValueEnumEntryInput[];
      selected?: boolean;
    }
  | {
      mode: "DYNAMIC";
      selected?: boolean;
    };

/**
 * Table update payload
 */
export interface UpdateTablePayload {
  access?: TableAccess;
  context?: string | null;
}

// =============================================================================
// Type Aliases (for consistency with component props)
// =============================================================================

export type UpdateColumnPayload = ColumnUpdatePayload;
export type CreateComputedColumnPayload = ComputedColumnCreatePayload;
export type UpdateComputedColumnPayload = ComputedColumnUpdatePayload;
export type CreateManualRelationPayload = ManualRelationCreatePayload;
export type UpdateManualRelationPayload = ManualRelationUpdatePayload;
export type UpsertContextFilterPayload = ContextFilterPayload;
export type UpsertTableAccessPolicyPayload = TableAccessPolicyPayload;
export type UnitColumnPayload = ColumnUnitPayload;
