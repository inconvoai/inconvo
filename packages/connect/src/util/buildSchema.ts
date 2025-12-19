import {
  SchemaResponse,
  SchemaTable,
  SchemaColumn,
  SchemaRelation,
} from "~/types/types";
import { getDb } from "~/dbConnection";
import { env } from "~/env";
import { sql } from "kysely";
import { logger } from "~/util/logger";
import { BigQueryIntrospector, StructFieldMetadata } from "~/dialects/bigquery";

interface NormalizedForeignKeyRow {
  sourceTable: string;
  sourceColumn: string;
  targetTable: string | null;
  targetColumn: string | null;
  constraintName: string;
  constraintSchema: string | null;
  sourceSchema: string | null;
  targetSchema: string | null;
  sourceOrdinal: number | null;
  targetOrdinal: number | null;
}

function normalizeForeignKeyRow(row: any): NormalizedForeignKeyRow {
  const toNumberOrNull = (value: unknown): number | null => {
    if (value === null || value === undefined) {
      return null;
    }
    const numeric = Number(value);
    return Number.isNaN(numeric) ? null : numeric;
  };

  return {
    sourceTable: row.source_table,
    sourceColumn: row.source_column,
    targetTable: row.target_table ?? null,
    targetColumn: row.target_column ?? null,
    constraintName: row.constraint_name,
    constraintSchema:
      row.constraint_schema ?? row.source_constraint_schema ?? null,
    sourceSchema: row.source_schema ?? null,
    targetSchema: row.target_schema ?? null,
    sourceOrdinal: toNumberOrNull(row.source_ordinal_position),
    targetOrdinal: toNumberOrNull(row.target_ordinal_position),
  };
}

function mapKyselyTypeToSimpleType(
  dataType: string,
  columnType?: string,
): string {
  const type = (dataType || columnType || "").toLowerCase();

  if (
    type.includes("int") ||
    type.includes("serial") ||
    type.includes("numeric") ||
    type.includes("decimal") ||
    type.includes("float") ||
    type.includes("double") ||
    type.includes("real")
  ) {
    return "number";
  }

  if (type.includes("bool")) {
    return "boolean";
  }

  if (type.includes("date") || type.includes("time")) {
    return "DateTime";
  }

  if (type.includes("json")) {
    return "json";
  }

  if (type.includes("uuid")) {
    return "string";
  }

  if (
    type.includes("char") ||
    type.includes("text") ||
    type.includes("varchar")
  ) {
    return "string";
  }

  return "string";
}

// Helper to singularize table names for relation naming
function singularize(tableName: string): string {
  // Simple singularization rules
  if (tableName.endsWith("ies")) {
    return tableName.slice(0, -3) + "y";
  }
  if (tableName.endsWith("es")) {
    // Check for special cases like "addresses" -> "address"
    if (
      tableName.endsWith("sses") ||
      tableName.endsWith("xes") ||
      tableName.endsWith("ches") ||
      tableName.endsWith("shes")
    ) {
      return tableName.slice(0, -2);
    }
    return tableName.slice(0, -1);
  }
  if (tableName.endsWith("s")) {
    return tableName.slice(0, -1);
  }
  return tableName;
}

// Helper to pluralize table names for relation naming
function pluralize(tableName: string): string {
  // Check if already plural - if it ends with 's' (but not 'ss'), it's likely already plural
  // Common patterns: users, orders, items, products, categories
  if (tableName.endsWith("s") && !tableName.endsWith("ss")) {
    return tableName; // Already plural
  }

  // Simple pluralization rules for singular words
  if (
    tableName.endsWith("y") &&
    !["ay", "ey", "iy", "oy", "uy"].includes(tableName.slice(-2))
  ) {
    return tableName.slice(0, -1) + "ies";
  }
  if (
    tableName.endsWith("ss") ||
    tableName.endsWith("x") ||
    tableName.endsWith("ch") ||
    tableName.endsWith("sh")
  ) {
    return tableName + "es";
  }
  return tableName + "s";
}

interface ForeignKeyInfo {
  sourceTable: string;
  targetTable: string;
  sourceColumns: string[];
  targetColumns: string[];
  constraintName: string;
}

async function extractForeignKeys(
  targetSchema?: string | null,
  allowedTables?: Set<string>,
): Promise<Map<string, SchemaRelation[]>> {
  const relationsMap = new Map<string, SchemaRelation[]>();
  const db = await getDb();

  try {
    let rows: any[] = [];
    let fallbackUsed = false;
    let skippedForeignKeys = 0;

    if (
      env.DATABASE_DIALECT === "postgresql" ||
      env.DATABASE_DIALECT === "redshift"
    ) {
      const isRedshift = env.DATABASE_DIALECT === "redshift";
      const targetOrdinalExpression = isRedshift
        ? sql`CAST(NULL AS INT)`
        : sql`kcu.position_in_unique_constraint`;

      const query = sql`
        SELECT
          tc.table_name AS source_table,
          kcu.column_name AS source_column,
          ccu.table_name AS target_table,
          ccu.column_name AS target_column,
          kcu.ordinal_position AS source_ordinal_position,
          ${targetOrdinalExpression} AS target_ordinal_position,
          tc.constraint_name,
          tc.constraint_schema,
          tc.table_schema AS source_schema,
          ccu.table_schema AS target_schema
        FROM
          information_schema.table_constraints AS tc
          JOIN information_schema.key_column_usage AS kcu
            ON tc.constraint_name = kcu.constraint_name
            AND tc.constraint_schema = kcu.constraint_schema
          JOIN information_schema.constraint_column_usage AS ccu
            ON ccu.constraint_name = tc.constraint_name
            AND ccu.constraint_schema = tc.constraint_schema
        WHERE tc.constraint_type = 'FOREIGN KEY'
        ${targetSchema ? sql`AND tc.table_schema = ${targetSchema}` : sql``}
      `;

      const result = await query.execute(db);
      rows = result.rows as any[];

      if (rows.length === 0) {
        const fallbackQuery = sql`
          SELECT
            tc.table_schema AS source_schema,
            tc.table_name AS source_table,
            kcu.column_name AS source_column,
            kcu.ordinal_position AS source_ordinal_position,
            ${targetOrdinalExpression} AS target_ordinal_position,
            tc.constraint_name,
            tc.constraint_schema AS constraint_schema
          FROM
            information_schema.table_constraints AS tc
            JOIN information_schema.key_column_usage AS kcu
              ON tc.constraint_name = kcu.constraint_name
              AND tc.constraint_schema = kcu.constraint_schema
          WHERE tc.constraint_type = 'FOREIGN KEY'
          ${targetSchema ? sql`AND tc.table_schema = ${targetSchema}` : sql``}
        `;

        const fallbackResult = await fallbackQuery.execute(db);
        const fallbackRows = fallbackResult.rows as any[];

        if (fallbackRows.length > 0) {
          fallbackUsed = true;

          const constraintNames = Array.from(
            new Set(
              fallbackRows
                .map((row) => row.constraint_name)
                .filter((name): name is string => Boolean(name)),
            ),
          );

          const constraintMeta = new Map<
            string,
            {
              target_table: string | null;
              target_schema: string | null;
              targetColumns: Map<number, string>;
            }
          >();

          if (constraintNames.length > 0) {
            const nameList = sql.join(
              constraintNames.map((name) => sql`${name}`),
              sql`, `,
            );

            const metaQuery = sql`
              SELECT
                rc.constraint_name,
                rc.constraint_schema,
                tc.table_name AS target_table,
                tc.table_schema AS target_schema,
                ref_kcu.column_name AS target_column,
                ref_kcu.ordinal_position AS target_ordinal_position
              FROM
                information_schema.referential_constraints AS rc
                JOIN information_schema.table_constraints AS tc
                  ON rc.unique_constraint_name = tc.constraint_name
                  AND rc.unique_constraint_schema = tc.constraint_schema
                LEFT JOIN information_schema.key_column_usage AS ref_kcu
                  ON ref_kcu.constraint_name = rc.unique_constraint_name
                  AND ref_kcu.constraint_schema = rc.unique_constraint_schema
              WHERE rc.constraint_name IN (${nameList})
            `;

            const metaResult = await metaQuery.execute(db);

            for (const metaRow of metaResult.rows as any[]) {
              const key = `${metaRow.constraint_schema ?? ""}|${
                metaRow.constraint_name
              }`;
              const entry = constraintMeta.get(key) ?? {
                target_table: metaRow.target_table ?? null,
                target_schema: metaRow.target_schema ?? null,
                targetColumns: new Map<number, string>(),
              };

              if (!entry.target_table && metaRow.target_table) {
                entry.target_table = metaRow.target_table;
              }

              if (!entry.target_schema && metaRow.target_schema) {
                entry.target_schema = metaRow.target_schema;
              }

              if (
                metaRow.target_ordinal_position != null &&
                metaRow.target_column
              ) {
                const ordinal = Number(metaRow.target_ordinal_position);
                if (!Number.isNaN(ordinal)) {
                  entry.targetColumns.set(ordinal, metaRow.target_column);
                }
              }

              constraintMeta.set(key, entry);
            }
          }

          rows = fallbackRows.map((row) => {
            const key = `${row.constraint_schema ?? ""}|${row.constraint_name}`;
            const meta = constraintMeta.get(key);
            const targetColumnsMap =
              meta?.targetColumns ?? new Map<number, string>();
            const targetOrdinalRaw = row.target_ordinal_position;
            const targetOrdinal =
              targetOrdinalRaw !== undefined && targetOrdinalRaw !== null
                ? Number(targetOrdinalRaw)
                : null;

            let targetColumn: string | null = null;
            if (targetOrdinal !== null && targetColumnsMap.has(targetOrdinal)) {
              targetColumn = targetColumnsMap.get(targetOrdinal) ?? null;
            } else if (targetColumnsMap.size === 1) {
              targetColumn = Array.from(targetColumnsMap.values())[0] ?? null;
            }

            return {
              ...row,
              target_table: meta?.target_table ?? null,
              target_column: targetColumn,
              target_schema: meta?.target_schema ?? null,
            };
          });
        }
      }
    } else if (env.DATABASE_DIALECT === "mysql") {
      const query = sql`
        SELECT
          TABLE_NAME as source_table,
          COLUMN_NAME as source_column,
          REFERENCED_TABLE_NAME as target_table,
          REFERENCED_COLUMN_NAME as target_column,
          CONSTRAINT_NAME as constraint_name
        FROM
          INFORMATION_SCHEMA.KEY_COLUMN_USAGE
        WHERE
          REFERENCED_TABLE_NAME IS NOT NULL
          ${
            targetSchema
              ? sql`AND TABLE_SCHEMA = ${targetSchema}`
              : sql`AND TABLE_SCHEMA = DATABASE()`
          }
      `;

      const result = await query.execute(db);
      rows = result.rows as any[];
    } else if (env.DATABASE_DIALECT === "mssql") {
      const query = sql`
        SELECT
          OBJECT_NAME(f.parent_object_id) AS source_table,
          COL_NAME(fc.parent_object_id, fc.parent_column_id) AS source_column,
          OBJECT_NAME(f.referenced_object_id) AS target_table,
          COL_NAME(fc.referenced_object_id, fc.referenced_column_id) AS target_column,
          f.name AS constraint_name,
          OBJECT_SCHEMA_NAME(f.parent_object_id) AS source_schema,
          OBJECT_SCHEMA_NAME(f.referenced_object_id) AS target_schema
        FROM
          sys.foreign_keys AS f
        INNER JOIN
          sys.foreign_key_columns AS fc
          ON f.object_id = fc.constraint_object_id
        ${
          targetSchema
            ? sql`WHERE OBJECT_SCHEMA_NAME(f.parent_object_id) = ${targetSchema}`
            : sql``
        }
      `;

      const result = await query.execute(db);
      rows = result.rows as any[];
    } else if (env.DATABASE_DIALECT === "bigquery") {
      const projectId = env.INCONVO_BIGQUERY_PROJECT_ID;
      const datasetName =
        targetSchema ?? env.INCONVO_BIGQUERY_DATASET ?? undefined;

      if (!projectId || !datasetName) {
        logger.warn(
          {
            projectIdPresent: !!projectId,
            datasetPresent: !!datasetName,
          },
          "Schema Introspection - BigQuery missing project or dataset for FK extraction",
        );
        rows = [];
      } else {
        const buildInfoSchemaTable = (tableName: string) =>
          sql.raw(
            `\`${projectId}.${datasetName}.INFORMATION_SCHEMA.${tableName}\``,
          );

        const tableConstraintsTable = buildInfoSchemaTable("TABLE_CONSTRAINTS");
        const keyColumnUsageTable = buildInfoSchemaTable("KEY_COLUMN_USAGE");
        const constraintColumnUsageTable = buildInfoSchemaTable(
          "CONSTRAINT_COLUMN_USAGE",
        );

        const schemaFilter =
          targetSchema && targetSchema.length > 0 ? targetSchema : datasetName;
        const constraintSchemaFilterClause = schemaFilter
          ? sql`AND constraint_schema = ${schemaFilter}`
          : sql``;
        const tableSchemaFilterClause = schemaFilter
          ? sql`WHERE k.table_schema = ${schemaFilter}`
          : sql``;
        const targetTableSchemaFilterClause = schemaFilter
          ? sql`WHERE c.table_schema = ${schemaFilter}`
          : sql``;

        const fkQuery = sql`
          WITH fk_constraints AS (
            SELECT
              constraint_name,
              constraint_schema
            FROM ${tableConstraintsTable}
            WHERE constraint_type = 'FOREIGN KEY'
            ${constraintSchemaFilterClause}
          ),
          source_cols AS (
            SELECT
              k.constraint_name,
              k.constraint_schema,
              k.table_schema AS source_schema,
              k.table_name AS source_table,
              k.column_name AS source_column,
              k.ordinal_position,
              ROW_NUMBER() OVER (
                PARTITION BY k.constraint_name
                ORDER BY k.ordinal_position
              ) AS rn
            FROM ${keyColumnUsageTable} k
            JOIN fk_constraints fk
              ON fk.constraint_name = k.constraint_name
              AND fk.constraint_schema = k.constraint_schema
            ${tableSchemaFilterClause}
          ),
          target_cols AS (
            SELECT
              c.constraint_name,
              c.constraint_schema,
              c.table_schema AS target_schema,
              c.table_name AS target_table,
              c.column_name AS target_column,
              ROW_NUMBER() OVER (
                PARTITION BY c.constraint_name
                ORDER BY c.column_name
              ) AS rn
            FROM ${constraintColumnUsageTable} c
            JOIN fk_constraints fk
              ON fk.constraint_name = c.constraint_name
              AND fk.constraint_schema = c.constraint_schema
            ${targetTableSchemaFilterClause}
          )
          SELECT
            src.source_table,
            src.source_column,
            tgt.target_table,
            tgt.target_column,
            src.constraint_name,
            src.constraint_schema,
            src.source_schema,
            tgt.target_schema,
            src.ordinal_position AS source_ordinal_position,
            tgt.rn AS target_ordinal_position
          FROM source_cols src
          JOIN target_cols tgt
            ON src.constraint_name = tgt.constraint_name
            AND src.constraint_schema = tgt.constraint_schema
            AND src.rn = tgt.rn
          ORDER BY src.constraint_name, src.ordinal_position
        `;

        try {
          const result = await fkQuery.execute(db);
          rows = result.rows as any[];
        } catch (error) {
          logger.warn(
            {
              path: `${projectId}.${datasetName}.INFORMATION_SCHEMA`,
              error,
            },
            "Schema Introspection - BigQuery information schema query failed",
          );
          rows = [];
        }
      }
    } else {
      return relationsMap;
    }

    // Group foreign keys by constraint to handle composite keys
    const foreignKeys: ForeignKeyInfo[] = [];
    const constraintGroups = new Map<string, NormalizedForeignKeyRow[]>();

    for (const row of rows) {
      const normalized = normalizeForeignKeyRow(row);
      const key = `${normalized.sourceTable}|${normalized.constraintName}`;
      if (!constraintGroups.has(key)) {
        constraintGroups.set(key, []);
      }
      constraintGroups.get(key)!.push(normalized);
    }

    const orderValue = (row: NormalizedForeignKeyRow): number => {
      if (row.targetOrdinal !== null) {
        return row.targetOrdinal;
      }
      if (row.sourceOrdinal !== null) {
        return row.sourceOrdinal;
      }
      return Number.MAX_SAFE_INTEGER;
    };

    // Convert groups to ForeignKeyInfo
    for (const group of constraintGroups.values()) {
      const sortedGroup = [...group].sort(
        (a, b) => orderValue(a) - orderValue(b),
      );

      const sourceColumns = sortedGroup
        .map((r) => r.sourceColumn)
        .filter((column): column is string => Boolean(column));
      const targetColumns = sortedGroup
        .map((r) => r.targetColumn)
        .filter((column): column is string => Boolean(column));
      const targetTable =
        sortedGroup.find((r) => Boolean(r.targetTable))?.targetTable ?? null;

      if (
        !targetTable ||
        sourceColumns.length === 0 ||
        targetColumns.length !== sourceColumns.length
      ) {
        skippedForeignKeys += 1;
        continue;
      }

      foreignKeys.push({
        sourceTable: sortedGroup[0].sourceTable,
        targetTable,
        sourceColumns,
        targetColumns,
        constraintName: sortedGroup[0].constraintName,
      });
    }

    if (fallbackUsed) {
      logger.info(
        {
          databaseSchema: targetSchema ?? null,
        },
        "Schema Introspection - used fallback foreign key query",
      );
    }

    if (skippedForeignKeys > 0) {
      logger.warn(
        {
          skippedForeignKeys,
          databaseSchema: targetSchema ?? null,
        },
        "Schema Introspection - skipped foreign keys due to incomplete metadata",
      );
    }

    // Track relation names to ensure uniqueness
    const relationNamesPerTable = new Map<string, Map<string, number>>();

    // First pass: create forward relations (many-to-one)
    for (const fk of foreignKeys) {
      const { sourceTable, targetTable, sourceColumns, targetColumns } = fk;

      if (
        allowedTables &&
        (!allowedTables.has(sourceTable) || !allowedTables.has(targetTable))
      ) {
        continue;
      }

      if (!relationsMap.has(sourceTable)) {
        relationsMap.set(sourceTable, []);
      }
      if (!relationNamesPerTable.has(sourceTable)) {
        relationNamesPerTable.set(sourceTable, new Map());
      }

      // Generate a clean relation name
      let relationName = singularize(targetTable);

      // Handle duplicates
      const nameCount =
        relationNamesPerTable.get(sourceTable)!.get(relationName) || 0;
      if (nameCount > 0) {
        // Remove common suffixes like "_id" from column name
        const columnBase = sourceColumns[0].replace(/_id$/, "");
        // Only add suffix if it's different from the target table name
        if (
          columnBase !== targetTable &&
          columnBase !== singularize(targetTable)
        ) {
          relationName = `${relationName}_via_${columnBase}`;
        } else {
          // If still duplicate, add a number
          relationName = `${relationName}_${nameCount + 1}`;
        }
      }
      relationNamesPerTable.get(sourceTable)!.set(relationName, nameCount + 1);

      const relation: SchemaRelation = {
        name: relationName,
        isList: false,
        targetTable: targetTable,
        sourceColumns: sourceColumns,
        targetColumns: targetColumns,
      };

      relationsMap.get(sourceTable)!.push(relation);
    }

    // Second pass: create reverse relations (one-to-many)
    for (const fk of foreignKeys) {
      const { sourceTable, targetTable, sourceColumns, targetColumns } = fk;

      if (
        allowedTables &&
        (!allowedTables.has(sourceTable) || !allowedTables.has(targetTable))
      ) {
        continue;
      }

      if (!relationsMap.has(targetTable)) {
        relationsMap.set(targetTable, []);
      }
      if (!relationNamesPerTable.has(targetTable)) {
        relationNamesPerTable.set(targetTable, new Map());
      }

      // Generate a clean reverse relation name
      let relationName = pluralize(sourceTable);

      // Handle duplicates
      const nameCount =
        relationNamesPerTable.get(targetTable)!.get(relationName) || 0;
      if (nameCount > 0) {
        // Remove common suffixes like "_id" from column name
        const columnBase = sourceColumns[0].replace(/_id$/, "");
        // Only add suffix if it's meaningful
        if (
          columnBase !== targetTable &&
          columnBase !== singularize(targetTable)
        ) {
          relationName = `${relationName}_via_${columnBase}`;
        } else {
          // If still duplicate, add a number
          relationName = `${relationName}_${nameCount + 1}`;
        }
      }
      relationNamesPerTable.get(targetTable)!.set(relationName, nameCount + 1);

      const reverseRelation: SchemaRelation = {
        name: relationName,
        isList: true,
        targetTable: sourceTable,
        // For reverse relations, swap source and target columns
        sourceColumns: targetColumns,
        targetColumns: sourceColumns,
      };

      // Only add if it provides useful information (not all one-to-many need columns)
      // But keep them for now for compatibility
      relationsMap.get(targetTable)!.push(reverseRelation);
    }
  } catch (error) {
    logger.warn(
      { error },
      "Schema Introspection - Could not extract foreign key relationships",
    );
  }

  return relationsMap;
}

export async function buildSchema(): Promise<SchemaResponse> {
  const startTime = Date.now();

  const db = await getDb();
  const introspector = db.introspection;
  const configuredSchema =
    env.DATABASE_DIALECT === "bigquery"
      ? (env.INCONVO_BIGQUERY_DATASET ?? env.INCONVO_DATABASE_SCHEMA)?.trim()
      : env.INCONVO_DATABASE_SCHEMA?.trim();
  const targetSchema = configuredSchema ? configuredSchema : undefined;

  const schemaLabel = targetSchema ? `schema "${targetSchema}"` : "all schemas";

  logger.info(
    {
      dialect: env.DATABASE_DIALECT,
      targetSchema: targetSchema ?? null,
    },
    `Schema introspection starting for ${env.DATABASE_DIALECT} (${schemaLabel})`,
  );

  const tablesFetchStart = Date.now();
  const allTables = await introspector.getTables();
  const tablesFetchDuration = Date.now() - tablesFetchStart;

  logger.info(
    {
      duration: tablesFetchDuration,
      tables: allTables.length,
      databaseSchema: targetSchema ?? null,
    },
    `Schema introspection - fetched ${allTables.length} tables in ${tablesFetchDuration}ms`,
  );

  const filteredTables = targetSchema
    ? allTables.filter((table) => {
        const tableSchema = (table as any).schema ?? null;
        return tableSchema === targetSchema;
      })
    : allTables;

  logger.info(
    {
      filteredTables: filteredTables.length,
      filterSchema: targetSchema ?? null,
    },
    targetSchema
      ? `Schema introspection - filtered to ${filteredTables.length} tables in schema "${targetSchema}"`
      : `Schema introspection - no schema filter applied (${filteredTables.length} tables kept)`,
  );

  const allowedTables = new Set(filteredTables.map((table) => table.name));

  const relationsStart = Date.now();
  const relationsMap = await extractForeignKeys(targetSchema, allowedTables);
  const relationsDuration = Date.now() - relationsStart;

  let totalRelations = 0;
  let totalColumns = 0;
  relationsMap.forEach((relations) => {
    totalRelations += relations.length;
  });

  logger.info(
    {
      tablesWithRelations: relationsMap.size,
      relations: totalRelations,
      duration: relationsDuration,
    },
    `Schema introspection - extracted ${totalRelations} foreign keys across ${relationsMap.size} tables in ${relationsDuration}ms`,
  );

  // Fetch STRUCT field paths for BigQuery
  const structFieldsByTable = new Map<string, StructFieldMetadata[]>();
  if (env.DATABASE_DIALECT === "bigquery") {
    const structStart = Date.now();
    try {
      // Create a fresh BigQueryIntrospector to access getStructFieldPaths
      // We can't use db.introspection directly as Kysely wraps it
      const projectId = env.INCONVO_BIGQUERY_PROJECT_ID;
      const dataset = env.INCONVO_BIGQUERY_DATASET;
      const location = env.INCONVO_BIGQUERY_LOCATION;

      if (projectId && dataset) {
        // Parse credentials from JSON string if provided
        let credentials: Record<string, unknown> | undefined;
        if (env.INCONVO_BIGQUERY_CREDENTIALS_JSON) {
          try {
            credentials = JSON.parse(env.INCONVO_BIGQUERY_CREDENTIALS_JSON);
          } catch (e) {
            logger.warn(
              { error: e },
              "Failed to parse INCONVO_BIGQUERY_CREDENTIALS_JSON",
            );
          }
        }

        const bqIntrospector = new BigQueryIntrospector({
          projectId,
          dataset,
          location: location || undefined,
          keyFilename: env.INCONVO_BIGQUERY_KEYFILE,
          credentials,
        });

        const structFields = await bqIntrospector.getStructFieldPaths();

        // Group by table name
        for (const field of structFields) {
          if (!structFieldsByTable.has(field.tableName)) {
            structFieldsByTable.set(field.tableName, []);
          }
          structFieldsByTable.get(field.tableName)!.push(field);
        }

        const structDuration = Date.now() - structStart;
        const totalStructFields = structFields.length;
        logger.info(
          {
            structFields: totalStructFields,
            tablesWithStructs: structFieldsByTable.size,
            duration: structDuration,
          },
          `Schema introspection - extracted ${totalStructFields} STRUCT fields across ${structFieldsByTable.size} tables in ${structDuration}ms`,
        );
      } else {
        logger.warn(
          { projectId: !!projectId, dataset: !!dataset },
          "Schema introspection - BigQuery project/dataset not configured for STRUCT field extraction",
        );
      }
    } catch (error) {
      logger.warn(
        { error },
        "Schema introspection - Could not extract STRUCT field paths",
      );
    }
  }

  const schemaTables: SchemaTable[] = [];

  for (const table of filteredTables) {
    const columns: SchemaColumn[] = [];

    for (const column of table.columns) {
      if (
        env.DATABASE_DIALECT === "bigquery" &&
        typeof column.dataType === "string" &&
        /STRUCT|ARRAY|JSON/i.test(column.dataType)
      ) {
        continue;
      }
      columns.push({
        name: column.name,
        type: mapKyselyTypeToSimpleType(column.dataType, column.dataType),
      });
    }

    // Add STRUCT field columns for BigQuery (with # separator)
    const tableStructFields = structFieldsByTable.get(table.name);
    if (tableStructFields) {
      for (const field of tableStructFields) {
        // Use # as separator to distinguish from table.column notation
        // field.fieldPath may contain dots for deeply nested structs
        // e.g., fieldPath "a.b" with parentColumn "props" becomes "props#a#b"
        const columnName = `${field.parentColumn}#${field.fieldPath.replace(/\./g, "#")}`;

        columns.push({
          name: columnName,
          type: mapKyselyTypeToSimpleType(field.dataType, field.dataType),
          isStructField: true,
          structParent: field.parentColumn,
          structFieldPath: field.fieldPath,
        });
      }
    }

    totalColumns += columns.length;

    const schemaTable: SchemaTable = {
      name: table.name,
      schema: (table as any).schema ?? targetSchema,
      columns,
      relations: relationsMap.get(table.name),
      // Leave a mutable slot so buildAugmentedSchema can merge the synced
      // computed-column augmentations without mutating the base schema cache.
      computedColumns: [],
    };

    schemaTables.push(schemaTable);
  }

  // Sort tables alphabetically for consistency
  schemaTables.sort((a, b) => a.name.localeCompare(b.name));

  const duration = Date.now() - startTime;

  // Single info summary at the end
  logger.info(
    {
      duration,
      tables: schemaTables.length,
      columns: totalColumns,
      relations: totalRelations,
      databaseSchema: targetSchema ?? null,
    },
    `Schema introspection complete – ${schemaTables.length} tables, ${totalColumns} columns, ${totalRelations} relations (${schemaLabel}) in ${duration}ms`,
  );

  return {
    tables: schemaTables,
    databaseSchema: targetSchema ?? null,
  };
}
