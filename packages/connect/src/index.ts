export { buildSchema } from "./util/buildSchema";
export { getDb } from "./dbConnection";
export {
  readUnifiedAugmentation,
  writeUnifiedAugmentation,
  computeAugmentationsHash,
} from "./util/schemaAugmentationStore";
export { clearAugmentedSchemaCache } from "./util/augmentedSchemaCache";
// Note: Express middleware exported separately from "@repo/connect/express"
export { BigQueryDialect } from "./dialects/bigquery";
export type { BigQueryDialectConfig } from "./dialects/bigquery";

// Export all operations
export { aggregate } from "./operations/aggregate";
export { findMany } from "./operations/findMany";
export { count } from "./operations/count";
export { countRelations } from "./operations/countRelations";
export { groupBy } from "./operations/groupBy";
export { aggregateGroups } from "./operations/aggregateGroups";
export { findDistinct } from "./operations/findDistinct";
export { findDistinctByEditDistance } from "./operations/findDistinctByEditDistance";

// Export types
export type {
  SchemaColumn,
  SchemaRelation,
  SchemaTable,
  SchemaResponse,
  SchemaComputedColumn,
  DatabaseDialect,
  ConnectionConfig,
} from "./types/types";

// Export query types and schemas
export type {
  Query,
  WhereConditions,
  QuestionConditions,
  JoinPathHop,
  GroupByKey,
  GroupByOrderBy,
  GroupByHaving,
  ComputedColumn,
  SQLComputedColumnAst,
} from "./types/querySchema";

export {
  QuerySchema,
  questionConditionsSchema,
  whereAndArraySchema,
  joinDescriptorSchema,
  joinPathHopSchema,
  groupByKeySchema,
  SQLComputedColumnAstSchema,
} from "./types/querySchema";
