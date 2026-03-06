import type {
  ConnectionSemanticModelTable,
  UserContextResponse,
} from "./types.js";

type ResolvableEntity = {
  id?: string;
  name: string;
};

function normalize(value: string): string {
  return value.trim();
}

function formatCandidates(
  entities: ResolvableEntity[],
  kind: string,
): string {
  if (entities.length === 0) {
    return `No ${kind} candidates are available.`;
  }
  const candidates = entities
    .slice(0, 10)
    .map((entity) =>
      entity.id ? `${entity.name} (${entity.id})` : entity.name,
    );
  const suffix = entities.length > 10 ? ", ..." : "";
  return `Available ${kind}: ${candidates.join(", ")}${suffix}`;
}

export function resolveByIdOrName<T extends ResolvableEntity>(
  entities: T[],
  rawIdentifier: string,
  kind: string,
): T {
  const identifier = normalize(rawIdentifier);
  if (identifier.length === 0) {
    throw new Error(`Missing ${kind} identifier.`);
  }

  const byId = entities.find((entity) => entity.id === identifier);
  if (byId) {
    return byId;
  }

  const exactMatches = entities.filter((entity) => entity.name === identifier);
  if (exactMatches.length === 1) {
    return exactMatches[0]!;
  }
  if (exactMatches.length > 1) {
    throw new Error(
      `Ambiguous ${kind} "${identifier}" (multiple exact matches). ${formatCandidates(exactMatches, kind)}`,
    );
  }

  const lower = identifier.toLowerCase();
  const caseInsensitiveMatches = entities.filter(
    (entity) => entity.name.toLowerCase() === lower,
  );
  if (caseInsensitiveMatches.length === 1) {
    return caseInsensitiveMatches[0]!;
  }
  if (caseInsensitiveMatches.length > 1) {
    throw new Error(
      `Ambiguous ${kind} "${identifier}" (case-insensitive matches). ${formatCandidates(caseInsensitiveMatches, kind)}`,
    );
  }

  throw new Error(
    `${kind} "${identifier}" not found. ${formatCandidates(entities, kind)}`,
  );
}

function tableColumns(table: ConnectionSemanticModelTable): ResolvableEntity[] {
  if (!Array.isArray(table.columns)) {
    return [];
  }

  return table.columns
    .map((column) => {
      const id =
        typeof column.id === "string" && column.id.length > 0
          ? column.id
          : undefined;
      const name = typeof column.name === "string" ? column.name : "";
      return { id, name };
    })
    .filter((column) => column.name.length > 0);
}

function tableComputedColumns(
  table: ConnectionSemanticModelTable,
): ResolvableEntity[] {
  if (!Array.isArray(table.computedColumns)) {
    return [];
  }

  return table.computedColumns
    .map((computed) => {
      const id =
        typeof computed.id === "string" && computed.id.length > 0
          ? computed.id
          : undefined;
      const name = typeof computed.name === "string" ? computed.name : "";
      return { id, name };
    })
    .filter((computed) => computed.name.length > 0);
}

function tableRelations(table: ConnectionSemanticModelTable): ResolvableEntity[] {
  if (!Array.isArray(table.outwardRelations)) {
    return [];
  }

  return table.outwardRelations
    .map((relation) => {
      const id =
        typeof relation.id === "string" && relation.id.length > 0
          ? relation.id
          : undefined;
      const name = typeof relation.name === "string" ? relation.name : "";
      return { id, name };
    })
    .filter((relation) => relation.name.length > 0 || relation.id !== undefined);
}

export function resolveTable(
  tables: ConnectionSemanticModelTable[],
  identifier: string,
): ConnectionSemanticModelTable {
  const entities = tables
    .map((table) => ({
      id: table.id,
      name: table.name,
      table,
    }))
    .filter((entity) => entity.name.length > 0 && entity.id.length > 0);

  const resolved = resolveByIdOrName(entities, identifier, "table");
  return resolved.table;
}

export function resolveColumn(
  table: ConnectionSemanticModelTable,
  identifier: string,
): { id: string; name: string } {
  const resolved = resolveByIdOrName(tableColumns(table), identifier, "column");
  if (!resolved.id) {
    throw new Error(`Resolved column "${resolved.name}" does not expose an id.`);
  }
  return {
    id: resolved.id,
    name: resolved.name,
  };
}

export function resolveComputedColumn(
  table: ConnectionSemanticModelTable,
  identifier: string,
): { id: string; name: string } {
  const resolved = resolveByIdOrName(
    tableComputedColumns(table),
    identifier,
    "computed column",
  );
  if (!resolved.id) {
    throw new Error(
      `Resolved computed column "${resolved.name}" does not expose an id.`,
    );
  }
  return {
    id: resolved.id,
    name: resolved.name,
  };
}

export function resolveRelation(
  table: ConnectionSemanticModelTable,
  identifier: string,
): { id: string; name: string } {
  const resolved = resolveByIdOrName(tableRelations(table), identifier, "relation");
  if (!resolved.id) {
    throw new Error(`Resolved relation "${resolved.name}" does not expose an id.`);
  }
  return {
    id: resolved.id,
    name: resolved.name,
  };
}

export function resolveUserContextField(
  response: UserContextResponse,
  identifier: string,
): { id: string; key: string } {
  const entities = response.userContext.fields.map((field) => ({
    id: field.id,
    name: field.key,
  }));
  const resolved = resolveByIdOrName(entities, identifier, "user-context field");
  if (!resolved.id) {
    throw new Error(
      `Resolved user-context field "${resolved.name}" does not expose an id.`,
    );
  }
  return {
    id: resolved.id,
    key: resolved.name,
  };
}
