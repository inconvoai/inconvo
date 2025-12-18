import {
  BigQuery,
  type BigQueryOptions,
  type JobResponse,
} from "@google-cloud/bigquery";
import {
  CompiledQuery,
  type DatabaseConnection,
  type DatabaseIntrospector,
  type DatabaseMetadata,
  type DatabaseMetadataOptions,
  type Dialect,
  type Driver,
  type Kysely,
  type QueryCompiler,
  type QueryResult,
  MysqlAdapter,
  MysqlQueryCompiler,
  type TableMetadata,
  type SchemaMetadata,
  type ColumnMetadata,
  type TransactionSettings,
} from "kysely";
interface BigQueryCredentials {
  client_email?: string;
  private_key?: string;
  [key: string]: unknown;
}

export interface BigQueryDialectConfig {
  /**
   * The project ID where the dataset lives.
   * Used for defaultDataset and INFORMATION_SCHEMA queries.
   */
  projectId: string;
  /**
   * The project ID where jobs should be created.
   * If not specified, uses the project_id from credentials JSON,
   * or falls back to projectId.
   * This is typically the project associated with the service account.
   */
  jobProjectId?: string;
  dataset: string;
  location?: string;
  credentials?: BigQueryCredentials;
  keyFilename?: string;
  maximumBytesBilled?: number | string;
  client?: BigQuery;
  options?: BigQueryOptions;
}

export interface StructFieldMetadata {
  tableName: string;
  parentColumn: string;
  fieldPath: string;
  dataType: string;
}

export class BigQueryDialect implements Dialect {
  constructor(private readonly config: BigQueryDialectConfig) {}

  createDriver(): Driver {
    return new BigQueryDriver(this.config);
  }

  createAdapter() {
    return new MysqlAdapter();
  }

  createQueryCompiler(): QueryCompiler {
    return new MysqlQueryCompiler();
  }

  createIntrospector(_db: Kysely<any>): DatabaseIntrospector {
    return new BigQueryIntrospector(this.config);
  }
}

class BigQueryDriver implements Driver {
  private client: BigQuery | null = null;

  constructor(private readonly config: BigQueryDialectConfig) {}

  async init() {
    // The BigQuery client's projectId determines where jobs are created.
    // This should be the project from the service account (jobProjectId),
    // NOT the dataset's project (projectId).
    // If jobProjectId is not explicitly set, let the BigQuery client
    // infer it from the credentials.
    this.client =
      this.config.client ??
      new BigQuery({
        projectId: this.config.jobProjectId,
        credentials: this.config.credentials,
        keyFilename: this.config.keyFilename,
        ...this.config.options,
      });
  }

  async acquireConnection(): Promise<DatabaseConnection> {
    if (!this.client) {
      throw new Error("BigQuery driver not initialized");
    }
    return new BigQueryConnection(this.client, this.config);
  }

  async beginTransaction(
    _connection: DatabaseConnection,
    _settings: TransactionSettings
  ) {
    throw new Error("BigQuery does not support transactions");
  }

  async commitTransaction(_connection: DatabaseConnection) {
    throw new Error("BigQuery does not support transactions");
  }

  async rollbackTransaction(_connection: DatabaseConnection) {
    throw new Error("BigQuery does not support transactions");
  }

  async releaseConnection(_connection: DatabaseConnection) {
    // No persistent connections to release.
  }

  async destroy() {
    this.client = null;
  }
}

class BigQueryConnection implements DatabaseConnection {
  constructor(
    private readonly client: BigQuery,
    private readonly config: BigQueryDialectConfig
  ) {}

  async executeQuery<O>(compiledQuery: CompiledQuery): Promise<QueryResult<O>> {
    const maxBytes =
      this.config.maximumBytesBilled !== undefined
        ? String(this.config.maximumBytesBilled)
        : undefined;

    const [job] = (await this.client.createQueryJob({
      query: compiledQuery.sql,
      params: compiledQuery.parameters as unknown[],
      useLegacySql: false,
      location: this.config.location,
      maximumBytesBilled: maxBytes,
      defaultDataset: {
        projectId: this.config.projectId,
        datasetId: this.config.dataset,
      },
    })) as unknown as JobResponse;

    const [rows] = await job.getQueryResults();

    const affectedRowsValue =
      job.metadata?.statistics?.query?.numDmlAffectedRows ?? undefined;
    const normalizedAffectedRows =
      affectedRowsValue !== undefined && affectedRowsValue !== null
        ? BigInt(affectedRowsValue)
        : undefined;

    return {
      rows: rows as O[],
      numAffectedRows: normalizedAffectedRows,
    };
  }

  async *streamQuery<R>(
    compiledQuery: CompiledQuery,
    chunkSize = 1000
  ): AsyncIterableIterator<QueryResult<R>> {
    const maxBytes =
      this.config.maximumBytesBilled !== undefined
        ? String(this.config.maximumBytesBilled)
        : undefined;

    const stream = this.client.createQueryStream({
      query: compiledQuery.sql,
      params: compiledQuery.parameters as unknown[],
      useLegacySql: false,
      location: this.config.location,
      maximumBytesBilled: maxBytes,
      defaultDataset: {
        projectId: this.config.projectId,
        datasetId: this.config.dataset,
      },
    });

    let buffer: R[] = [];
    for await (const row of stream) {
      buffer.push(row as R);
      if (buffer.length >= chunkSize) {
        yield { rows: buffer };
        buffer = [];
      }
    }

    if (buffer.length > 0) {
      yield { rows: buffer };
    }
  }
}

export class BigQueryIntrospector implements DatabaseIntrospector {
  private readonly client: BigQuery;

  constructor(private readonly config: BigQueryDialectConfig) {
    // Use jobProjectId for the BigQuery client (where jobs are created).
    // If not set, the client will infer from credentials.
    this.client =
      config.client ??
      new BigQuery({
        projectId: config.jobProjectId,
        credentials: config.credentials,
        keyFilename: config.keyFilename,
        ...config.options,
      });
  }

  async getSchemas(): Promise<SchemaMetadata[]> {
    return [{ name: this.config.dataset }];
  }

  async getTables(
    _options: DatabaseMetadataOptions = { withInternalKyselyTables: false }
  ): Promise<TableMetadata[]> {
    const datasetPath = `\`${this.config.projectId}.${this.config.dataset}\``;
    const tablesQuery = `
      SELECT table_name, table_type
      FROM ${datasetPath}.INFORMATION_SCHEMA.TABLES
    `;
    const columnsQuery = `
      SELECT table_name, column_name, data_type, is_nullable
      FROM ${datasetPath}.INFORMATION_SCHEMA.COLUMNS
    `;

    const [tables] = await this.client.query({
      query: tablesQuery,
      useLegacySql: false,
      location: this.config.location,
    });

    const [columns] = await this.client.query({
      query: columnsQuery,
      useLegacySql: false,
      location: this.config.location,
    });

    const columnsByTable = new Map<string, ColumnMetadata[]>();

    for (const col of columns as any[]) {
      const columnMetadata: ColumnMetadata = {
        name: col.column_name,
        dataType: col.data_type,
        isNullable: col.is_nullable === "YES",
        isAutoIncrementing: false,
        hasDefaultValue: false,
      };
      const existing = columnsByTable.get(col.table_name) ?? [];
      existing.push(columnMetadata);
      columnsByTable.set(col.table_name, existing);
    }

    return (tables as any[]).map((table) => ({
      name: table.table_name,
      isView: table.table_type === "VIEW",
      schema: this.config.dataset,
      columns: columnsByTable.get(table.table_name) ?? [],
    }));
  }

  async getMetadata(
    options?: DatabaseMetadataOptions
  ): Promise<DatabaseMetadata> {
    return {
      tables: await this.getTables(options),
    };
  }

  /**
   * Fetches STRUCT field paths from BigQuery's INFORMATION_SCHEMA.COLUMN_FIELD_PATHS.
   * This returns metadata about nested fields within STRUCT columns.
   * Only returns fields that are nested (contain a dot in field_path).
   */
  async getStructFieldPaths(): Promise<StructFieldMetadata[]> {
    const datasetPath = `\`${this.config.projectId}.${this.config.dataset}\``;

    // COLUMN_FIELD_PATHS contains all fields including nested STRUCT fields
    // field_path contains the full path like "properties.payment_method"
    // We filter for paths containing a dot to get only nested fields
    const structFieldsQuery = `
      SELECT
        table_name,
        field_path,
        data_type
      FROM ${datasetPath}.INFORMATION_SCHEMA.COLUMN_FIELD_PATHS
      WHERE field_path LIKE '%.%'
        AND data_type NOT LIKE 'STRUCT%'
        AND data_type NOT LIKE 'ARRAY%'
    `;

    const [rows] = await this.client.query({
      query: structFieldsQuery,
      useLegacySql: false,
      location: this.config.location,
    });

    const structFields: StructFieldMetadata[] = [];

    for (const row of rows as any[]) {
      const fieldPath = row.field_path as string;
      // Split by first dot to get parent column and nested field path
      const firstDotIndex = fieldPath.indexOf('.');
      if (firstDotIndex === -1) {
        console.warn(
          `[BigQueryIntrospector] Unexpected field_path without dot: "${fieldPath}"`
        );
        continue;
      }

      const parentColumn = fieldPath.slice(0, firstDotIndex);
      const nestedPath = fieldPath.slice(firstDotIndex + 1);

      structFields.push({
        tableName: row.table_name,
        parentColumn,
        fieldPath: nestedPath,
        dataType: row.data_type,
      });
    }

    return structFields;
  }
}
