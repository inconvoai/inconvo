import { Kysely, sql } from "kysely";
import { Query } from "~/types/querySchema";
import { buildWhereConditions } from "~/operations/utils/whereConditionBuilder";
import { getAugmentedSchema } from "~/util/augmentedSchemaCache";
import { getColumnFromTable } from "~/operations/utils/computedColumns";
import { applyLimit } from "~/operations/utils/queryHelpers";
import { getRelatedTableNameFromPath } from "~/operations/utils/schemaHelpers";
import {
  buildJsonObjectSelect,
  jsonAggregate,
} from "~/operations/utils/jsonBuilderHelpers";
import { env } from "~/env";
import assert from "assert";
import {
  resolveJoinDescriptor,
  aliasDepth,
} from "../utils/joinDescriptorHelpers";
import { parseJsonStrings } from "../utils/jsonParsing";
import type { SchemaTable } from "~/types/types";

export async function findMany(db: Kysely<any>, query: Query) {
  assert(query.operation === "findMany", "Invalid operation");
  const { table, whereAndArray, operationParameters, jsonColumnSchema } = query;
  const { select, orderBy, limit } = operationParameters;

  const schema = await getAugmentedSchema();
  const schemaTables = schema.tables;

  const resolvedJoins = (operationParameters.joins ?? [])
    .map((join) =>
      resolveJoinDescriptor({
        alias: join.name ?? join.table,
        tableName: join.table,
        path: join.path,
      })
    )
    .sort((a, b) => aliasDepth(b.alias) - aliasDepth(a.alias));

  const selectMap: Record<string, string[] | undefined> = {
    ...select,
  };

  if (!selectMap[table]) {
    selectMap[table] = [];
  }

  const aliasColumnMap = new Map<string, string>();

  const joinDescriptorMap = new Map(
    resolvedJoins.map((join) => [join.alias, join])
  );

  // Parse columns per table
  // Create table aliases for JSON columns
  const tableAliasMapper: Record<string, any> = {};
  const tableAliases: any[] = [];
  const tablesToAlias =
    jsonColumnSchema?.map((jsonCol) => jsonCol.tableName) || [];

  for (const tableName of tablesToAlias) {
    const jsonSchemaForTable = jsonColumnSchema?.find(
      (jsonCol) => jsonCol.tableName === tableName
    );
    const jsonCols =
      jsonSchemaForTable?.jsonSchema.map((col) => col.name) || [];
    if (jsonCols.length === 0) {
      continue;
    }
    const jsonColumnName = jsonSchemaForTable?.jsonColumnName;

    // Create a CTE for the table with JSON extraction
    const cteSelections: any[] = [`${tableName}.*`];

    for (const col of jsonCols) {
      const colType = jsonSchemaForTable?.jsonSchema.find(
        (jCol) => jCol.name === col
      )?.type;

      let extraction;
      if (env.DATABASE_DIALECT === "postgresql") {
        extraction =
          colType === "String"
            ? sql`(${sql.ref(`${jsonColumnName!}`)}->>${sql.lit(col)})::text`
            : sql`(${sql.ref(`${jsonColumnName!}`)}->>${sql.lit(
                col
              )})::numeric`;
      } else if (env.DATABASE_DIALECT === "mysql") {
        extraction =
          colType === "String"
            ? sql`CAST(JSON_EXTRACT(${sql.ref(jsonColumnName!)}, ${sql.lit(
                "$." + col
              )}) AS CHAR)`
            : sql`CAST(JSON_EXTRACT(${sql.ref(jsonColumnName!)}, ${sql.lit(
                "$." + col
              )}) AS DECIMAL)`;
      } else if (env.DATABASE_DIALECT === "mssql") {
        extraction =
          colType === "String"
            ? sql`JSON_VALUE(${sql.ref(jsonColumnName!)}, ${sql.lit(
                "$." + col
              )})`
            : sql`CAST(JSON_VALUE(${sql.ref(jsonColumnName!)}, ${sql.lit(
                "$." + col
              )}) AS DECIMAL)`;
      } else if (env.DATABASE_DIALECT === "bigquery") {
        extraction =
          colType === "String"
            ? sql`JSON_VALUE(${sql.ref(jsonColumnName!)}, ${sql.lit(
                "$." + col
              )})`
            : sql`SAFE_CAST(JSON_VALUE(${sql.ref(jsonColumnName!)}, ${sql.lit(
                "$." + col
              )}) AS NUMERIC)`;
      } else {
        extraction = sql`NULL`;
      }

      cteSelections.push(sql`${extraction} as ${sql.ref(col)}`);
    }

    const tableAliasCte = db.selectFrom(tableName).select(cteSelections);

    tableAliasMapper[tableName] = `${tableName}Alias`;
    tableAliases.push({ name: `${tableName}Alias`, query: tableAliasCte });
  }

  // Check if we need CTEs for relations
  const needCtes =
    Object.keys(selectMap).filter((t) => t !== query.table).length > 0;

  // Parse table paths for relations
  const tablePaths = Object.keys(selectMap)
    .filter((t) => t !== query.table)
    .map((t) => t.split("."));

  // Remove redundant paths (if a longer path contains a shorter one)
  const dedupedTablePaths = tablePaths.filter(
    (arr, _index, self) =>
      !self.some(
        (other) =>
          other.length > arr.length &&
          other.slice(0, arr.length).every((v, i) => v === arr[i])
      )
  );

  const nestedJsonCtes: any[][] = [];
  const outerTableLinks: string[][][] = [];

  if (needCtes) {
    for (const [outerIndex, tablePath] of dedupedTablePaths.entries()) {
      const jsonCtes: any[] = [];
      const tableLinks: string[][] = [];
      const reverseTablePath = tablePath.slice().reverse();
      const aliasKey = tablePath.join(".");
      const joinDescriptor = joinDescriptorMap.get(aliasKey);

      for (const [index, tableRelationName] of reverseTablePath.entries()) {
        if (tableRelationName === query.table) {
          continue;
        }

        let targetTableName: string | undefined;
        let sourceTableName: string | undefined;
        let currentTableKey: string | undefined;
        let relatedTableKey: string | undefined;
        let groupBy = false;

        const hopMetadata =
          joinDescriptor?.hops[
            Math.max(0, (joinDescriptor?.hops.length ?? 1) - 1 - index)
          ];

        if (
          hopMetadata &&
          hopMetadata.source.length > 0 &&
          hopMetadata.target.length > 0
        ) {
          targetTableName = hopMetadata.target[0]?.tableName;
          sourceTableName = hopMetadata.source[0]?.tableName;
          currentTableKey = hopMetadata.target[0]?.columnName;
          relatedTableKey = hopMetadata.source[0]?.columnName;
          groupBy =
            relationIsList(
              schemaTables,
              sourceTableName,
              targetTableName,
              hopMetadata.source.map((col) => col.columnName),
              hopMetadata.target.map((col) => col.columnName)
            ) ?? false;
        }

        if (
          !targetTableName ||
          !sourceTableName ||
          !currentTableKey ||
          !relatedTableKey
        ) {
          const pathToTableName = tablePath.slice(
            0,
            tablePath.indexOf(tableRelationName) + 1
          );
          targetTableName = await getRelatedTableNameFromPath(pathToTableName);

          const pathToRelatedTable = tablePath.slice(
            0,
            tablePath.indexOf(tableRelationName)
          );
          sourceTableName = await getRelatedTableNameFromPath(
            pathToRelatedTable
          );

          const fallback = getRelationKeysFromSchema(
            schemaTables,
            sourceTableName,
            targetTableName
          );

          relatedTableKey = fallback.sourceColumn ?? relatedTableKey;
          currentTableKey = fallback.targetColumn ?? currentTableKey;
          groupBy =
            fallback.isList ??
            relationIsList(
              schemaTables,
              sourceTableName,
              targetTableName,
              relatedTableKey ? [relatedTableKey] : [],
              currentTableKey ? [currentTableKey] : []
            ) ??
            false;
        }

        if (
          !targetTableName ||
          !sourceTableName ||
          !currentTableKey ||
          !relatedTableKey
        ) {
          throw new Error(
            `Unable to resolve join metadata for alias ${aliasKey} (segment ${tableRelationName}).`
          );
        }

        tableLinks.push([currentTableKey, relatedTableKey]);

        const tableSource =
          tableAliasMapper[targetTableName] || targetTableName;
        const aliasForSegment = tablePath
          .slice(0, tablePath.length - index)
          .join(".");
        const selectedColumns = selectMap[aliasForSegment] ?? [];
        const jsonFields: [string, any][] = selectedColumns.map((col) => {
          const columnParam = getColumnFromTable({
            columnName: col,
            tableName: targetTableName!,
            schema,
          });
          return [col, columnParam];
        });

        if (index === 0) {
          const cteName = `cteInitial${targetTableName}${index}${outerIndex}${
            groupBy ? "_" : ""
          }`;
          const columnExpr = sql`${getColumnFromTable({
            columnName: currentTableKey,
            tableName: targetTableName,
            schema,
          })}`;
          const jsonExpr = buildJsonObjectSelect(jsonFields);

          const cteQuery = db
            .selectFrom(tableSource)
            .select([
              columnExpr.as(currentTableKey),
              jsonExpr.as("json_data"),
            ] as any);

          jsonCtes.push({ name: cteName, query: cteQuery });

          if (groupBy) {
            const groupedCteName = `cte${targetTableName}${index}${outerIndex}`;
            const groupedCteQuery = db
              .selectFrom(cteName)
              .select([
                sql.ref(currentTableKey),
                jsonAggregate(sql.ref("json_data")).as("json_data"),
              ] as any)
              .groupBy(currentTableKey);

            jsonCtes.push({ name: groupedCteName, query: groupedCteQuery });
          }
        } else {
          const previousTableLinks = tableLinks[index - 1];
          const previousCteName = jsonCtes[jsonCtes.length - 1].name;
          const previousTableName = tablePath.slice().reverse()[index - 1];

          const extendedJsonFields = jsonFields.concat([
            [previousTableName, sql.ref(`${previousCteName}.json_data`)],
          ]);

          const cteName = `cteSubs${targetTableName}${index}${outerIndex}${
            groupBy ? "_" : ""
          }`;
          const columnExpr = sql`${getColumnFromTable({
            columnName: currentTableKey,
            tableName: targetTableName,
            schema,
          })}`;
          const jsonExpr = buildJsonObjectSelect(extendedJsonFields);

          let cteQuery: any = db
            .selectFrom(tableSource)
            .leftJoin(previousCteName, (join: any) =>
              join.onRef(
                `${tableSource}.${previousTableLinks[1]}`,
                "=",
                `${previousCteName}.${previousTableLinks[0]}`
              )
            );

          cteQuery = cteQuery
            .select(columnExpr.as(currentTableKey))
            .select(jsonExpr.as("json_data"));

          jsonCtes.push({ name: cteName, query: cteQuery });

          if (groupBy) {
            const groupedCteName = `cte${targetTableName}${index}${outerIndex}`;
            const groupedCteQuery = db
              .selectFrom(cteName)
              .select([
                sql.ref(currentTableKey),
                jsonAggregate(sql.ref("json_data")).as("json_data"),
              ] as any)
              .groupBy(currentTableKey);

            jsonCtes.push({ name: groupedCteName, query: groupedCteQuery });
          }
        }
      }

      nestedJsonCtes.push(jsonCtes);
      outerTableLinks.push(tableLinks);
    }
  }

  // Build the main query
  const tableSource = tableAliasMapper[table] || table;

  // Start building the query with CTEs
  let dbQuery: any = db;

  // Add all CTEs
  for (const alias of tableAliases) {
    dbQuery = dbQuery.with(alias.name, () => alias.query);
  }
  for (const cteGroup of nestedJsonCtes) {
    for (const cte of cteGroup) {
      dbQuery = dbQuery.with(cte.name, () => cte.query);
    }
  }

  // Build root selections
  const rootSelections: any[] = [];
  const mainTableColumns = selectMap[table];

  if (mainTableColumns && mainTableColumns.length > 0) {
    for (const columnName of mainTableColumns) {
      const columnRef = getColumnFromTable({
        columnName,
        tableName: table,
        schema,
      });
      rootSelections.push(columnRef.as(columnName));
    }
  }

  // Add nested relation selections
  dedupedTablePaths.forEach((tablePath, index) => {
    const tableCteGroup = nestedJsonCtes[index];
    const tableCte = tableCteGroup?.at(-1);
    if (!tableCte) {
      return;
    }
    const tableAlias = tablePath.join(".");
    const safeAlias = sanitizeAlias(tableAlias, aliasColumnMap);
    rootSelections.push(
      sql`${sql.ref(`${tableCte.name}.json_data`)}`.as(safeAlias)
    );
  });

  // Handle JSON columns for the main table
  if (jsonColumnSchema && jsonColumnSchema.length > 0) {
    for (const jsonCol of jsonColumnSchema) {
      if (jsonCol.tableName === table) {
        const jsonColumnName = jsonCol.jsonColumnName;
        for (const field of jsonCol.jsonSchema) {
          let extraction;
          if (env.DATABASE_DIALECT === "postgresql") {
            extraction =
              field.type === "String"
                ? sql`(${sql.ref(
                    `${tableSource}.${jsonColumnName}`
                  )}->>${sql.lit(field.name)})::text`
                : sql`(${sql.ref(
                    `${tableSource}.${jsonColumnName}`
                  )}->>${sql.lit(field.name)})::numeric`;
          } else if (env.DATABASE_DIALECT === "mysql") {
            extraction =
              field.type === "String"
                ? sql`CAST(JSON_EXTRACT(${sql.ref(
                    `${tableSource}.${jsonColumnName}`
                  )}, ${sql.lit("$." + field.name)}) AS CHAR)`
                : sql`CAST(JSON_EXTRACT(${sql.ref(
                    `${tableSource}.${jsonColumnName}`
                  )}, ${sql.lit("$." + field.name)}) AS DECIMAL)`;
          } else if (env.DATABASE_DIALECT === "mssql") {
            extraction =
              field.type === "String"
                ? sql`JSON_VALUE(${sql.ref(tableSource)}.${sql.ref(
                    jsonColumnName
                  )}, ${sql.lit("$." + field.name)})`
                : sql`CAST(JSON_VALUE(${sql.ref(tableSource)}.${sql.ref(
                    jsonColumnName
                  )}, ${sql.lit("$." + field.name)}) AS DECIMAL)`;
          } else if (env.DATABASE_DIALECT === "bigquery") {
            extraction =
              field.type === "String"
                ? sql`JSON_VALUE(${sql.ref(
                    `${tableSource}.${jsonColumnName}`
                  )}, ${sql.lit("$." + field.name)})`
                : sql`SAFE_CAST(JSON_VALUE(${sql.ref(
                    `${tableSource}.${jsonColumnName}`
                  )}, ${sql.lit("$." + field.name)}) AS NUMERIC)`;
          } else {
            extraction = sql`NULL`;
          }
          rootSelections.push(extraction.as(field.name));
        }
      }
    }
  }

  // Select from main table
  let selectQuery = dbQuery.selectFrom(tableSource);

  // Add joins for CTEs
  if (needCtes) {
    dedupedTablePaths.forEach((tablePath, pathIndex) => {
      const tableCteGroup = nestedJsonCtes[pathIndex];
      const tableCte = tableCteGroup?.at(-1);
      if (!tableCte) {
        return;
      }
      const finalLink =
        outerTableLinks[pathIndex][outerTableLinks[pathIndex].length - 1];

      selectQuery = selectQuery.leftJoin(tableCte.name, (join: any) =>
        join.onRef(
          `${tableSource}.${finalLink[1]}`,
          "=",
          `${tableCte.name}.${finalLink[0]}`
        )
      );
    });
  }

  // Apply selections
  if (rootSelections.length > 0) {
    selectQuery = selectQuery.select(rootSelections);
  } else {
    selectQuery = selectQuery.selectAll();
  }

  // Add where conditions
  const whereCondition = buildWhereConditions(
    whereAndArray,
    table,
    schema
  );
  if (whereCondition) {
    selectQuery = selectQuery.where(whereCondition);
  }

  // Handle ordering
  if (orderBy) {
    const columnRef = getColumnFromTable({
      columnName: orderBy.column,
      tableName: table,
      schema,
    });

    if (orderBy.direction === "desc") {
      selectQuery = selectQuery.orderBy(columnRef, "desc");
    } else {
      selectQuery = selectQuery.orderBy(columnRef, "asc");
    }
  }

  // Handle limit
  selectQuery = applyLimit(selectQuery, limit);

  const compiled = selectQuery.compile();
  const result = await selectQuery.execute();

  const remappedRows = result.map((row: any) => {
    const output: Record<string, any> = {};
    Object.entries(row).forEach(([key, value]) => {
      const original = aliasColumnMap.get(key);
      if (original) {
        output[original] = value;
      } else {
        output[key] = value;
      }
    });
    return output;
  });

  const nestedAliases = dedupedTablePaths.map((path) => path.join("."));
  const normalized = remappedRows.map((row: any) =>
    normalizeNestedAliases(row, nestedAliases)
  );

  if (env.DATABASE_DIALECT === "mssql") {
    return {
      query: { sql: compiled.sql, params: compiled.parameters },
      data: normalized.map((row: any) => parseJsonStrings(row)),
    };
  }

  return {
    query: { sql: compiled.sql, params: compiled.parameters },
    data: normalized,
  };
}

/**
 * BigQuery requires column names to be alphanumeric/underscore and start with a letter,
 * so we normalize aliases by replacing invalid characters with `_`, collapsing duplicate
 * underscores, trimming leading underscores, defaulting to `col`, and prefixing `col_`
 * when an alias would start with a digit. The `aliasColumnMap` preserves the mapping
 * from the generated safe alias back to its original name; if a collision occurs we
 * append an incrementing suffix until the alias is unique, ensuring result rows can be
 * remapped to their requested columns.
 */
function sanitizeAlias(
  alias: string,
  aliasColumnMap: Map<string, string>
): string {
  let sanitized = alias
    .replace(/[^a-zA-Z0-9_]/g, "__")
    .replace(/_{3,}/g, "__")
    .replace(/^_+/, "");

  if (!sanitized) {
    sanitized = "col";
  }

  if (/^[0-9]/.test(sanitized)) {
    sanitized = `col_${sanitized}`;
  }

  let candidate = sanitized;
  let counter = 1;
  while (
    aliasColumnMap.has(candidate) &&
    aliasColumnMap.get(candidate) !== alias
  ) {
    candidate = `${sanitized}_${counter++}`;
  }

  aliasColumnMap.set(candidate, alias);
  return candidate;
}

function relationIsList(
  tables: SchemaTable[],
  sourceTableName: string | undefined,
  targetTableName: string | undefined,
  sourceColumns: string[],
  targetColumns: string[]
) {
  if (!sourceTableName || !targetTableName) return false;
  const table = tables.find((t) => t.name === sourceTableName);
  if (!table?.relations) return false;
  const relation = table.relations.find((rel) => {
    if (rel.targetTable !== targetTableName) return false;
    const relSource = rel.sourceColumns ?? [];
    const relTarget = rel.targetColumns ?? [];
    return (
      arraysEqual(relSource, sourceColumns) &&
      arraysEqual(relTarget, targetColumns)
    );
  });
  return relation?.isList ?? false;
}

function getRelationKeysFromSchema(
  tables: SchemaTable[],
  sourceTableName: string | undefined,
  targetTableName: string | undefined
) {
  if (!sourceTableName || !targetTableName) {
    return {
      sourceColumn: undefined,
      targetColumn: undefined,
      isList: undefined,
    };
  }
  const table = tables.find((t) => t.name === sourceTableName);
  const relation = table?.relations?.find(
    (rel) => rel.targetTable === targetTableName
  );
  return {
    sourceColumn: relation?.sourceColumns?.[0],
    targetColumn: relation?.targetColumns?.[0],
    isList: relation?.isList,
  };
}

function normalizeNestedAliases(row: any, aliases: string[]) {
  const output: Record<string, any> = { ...row };
  aliases.forEach((alias) => {
    const normalized = coerceToArray(output[alias]);
    const leafKey = alias.split(".").at(-1);
    if (leafKey) {
      normalized.forEach((entry: any) => {
        if (entry && typeof entry === "object" && leafKey in entry) {
          entry[leafKey] = coerceToArray(entry[leafKey]);
        }
      });
    }
    output[alias] = normalized;
  });
  return output;
}

function coerceToArray(value: any) {
  if (value == null) return [];
  if (Array.isArray(value)) return value;
  if (typeof value === "string") {
    try {
      const parsed = JSON.parse(value);
      const normalized = parseJsonStrings(parsed);
      if (Array.isArray(normalized)) return normalized;
      if (normalized && typeof normalized === "object") return [normalized];
    } catch {
      return [value];
    }
  }
  if (typeof value === "object") {
    if (Array.isArray((value as any).data)) return (value as any).data;
    if (Array.isArray((value as any).json)) return (value as any).json;
    return [value];
  }
  return [value];
}

function arraysEqual(a: string[], b: string[]) {
  if (a.length !== b.length) return false;
  return a.every((val, index) => val === b[index]);
}
