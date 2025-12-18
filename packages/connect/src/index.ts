export { buildSchema } from './util/buildSchema';
export { getDb } from './dbConnection';
export { inconvo } from './express';
export { BigQueryDialect } from './dialects/bigquery';
export type { BigQueryDialectConfig } from './dialects/bigquery';

// Export all operations
export { aggregate } from './operations/aggregate';
export { findMany } from './operations/findMany';
export { count } from './operations/count';
export { countRelations } from './operations/countRelations';
export { groupBy } from './operations/groupBy';
export { aggregateGroups } from './operations/aggregateGroups';
export { findDistinct } from './operations/findDistinct';
export { findDistinctByEditDistance } from './operations/findDistinctByEditDistance';

// Export types
export type {
  SchemaColumn,
  SchemaRelation,
  SchemaTable,
  SchemaResponse,
  SchemaComputedColumn,
  DatabaseDialect,
  ConnectionConfig,
} from './types/types';

export type { Query } from './types/querySchema';
