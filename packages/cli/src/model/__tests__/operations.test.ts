import { test, type TestContext } from "node:test";
import assert from "node:assert/strict";
import * as fs from "fs/promises";
import * as os from "os";
import * as path from "path";
import YAML from "yaml";
import {
  pullAgentsToWorkspace,
  slugify,
  syncSingleAgentToWorkspace,
} from "../operations.js";
import type { PlatformApiClient } from "../api-client.js";

const ORG_ID = "org_test";
const AGENT_ID = "agt_test";

async function withTempRepo(t: TestContext): Promise<string> {
  const repoRoot = await fs.mkdtemp(path.join(os.tmpdir(), "inconvo-ops-test-"));
  t.after(async () => {
    await fs.rm(repoRoot, { recursive: true, force: true });
  });
  return repoRoot;
}

function failMethod(name: string) {
  return async (..._args: unknown[]) => {
    throw new Error(`${name} should not be called`);
  };
}

function createClientStub(
  overrides: Partial<PlatformApiClient>,
): PlatformApiClient {
  return {
    getOrg: failMethod("getOrg"),
    listOrgAgents: failMethod("listOrgAgents"),
    listAgentConnections: failMethod("listAgentConnections"),
    getAgentUserContext: failMethod("getAgentUserContext"),
    getConnectionSemanticModel: failMethod("getConnectionSemanticModel"),
    getConnection: failMethod("getConnection"),
    runModelAction: failMethod("runModelAction"),
    listShareableConnections: failMethod("listShareableConnections"),
    linkConnection: failMethod("linkConnection"),
    syncConnection: failMethod("syncConnection"),
    unlinkConnection: failMethod("unlinkConnection"),
    updateConnectionDescription: failMethod("updateConnectionDescription"),
    ...overrides,
  } as unknown as PlatformApiClient;
}

test("pull writes deterministic files without lock/timestamp artifacts", async (t) => {
  const repoRoot = await withTempRepo(t);

  const client = createClientStub({
    getAgentUserContext: async () => ({
      userContext: {
        status: "ENABLED",
        fields: [
          { id: "uc_2", key: "zeta", type: "NUMBER" as const },
          { id: "uc_1", key: "alpha", type: "STRING" as const },
        ],
      },
      hash: "uc_hash",
    }),
    listAgentConnections: async () => [
      {
        id: "con_b",
        name: "Second",
        description: "Primary warehouse",
        status: null,
        isShared: false,
        ownerAgentName: "Owner",
      },
    ],
    getConnectionSemanticModel: async () => ({
      connectionId: "con_b",
      tables: [
        {
          id: "tbl_2",
          name: "b",
          access: "OFF" as const,
          columns: [],
          computedColumns: [],
          outwardRelations: [],
        },
        {
          id: "tbl_1",
          name: "a",
          access: "OFF" as const,
          columns: [],
          computedColumns: [],
          outwardRelations: [],
        },
      ],
      hash: "hash_b",
    }),
    listShareableConnections: async () => [
      {
        id: "con_shareable",
        name: "Shared Finance",
        description: "Finance reporting source",
        databaseType: "POSTGRES",
        ownerAgent: {
          id: "agt_owner",
          name: "Owner",
        },
      },
    ],
  });

  await pullAgentsToWorkspace({
    client,
    repoRoot,
    org: { id: ORG_ID, name: "Test Org" },
    selectedAgents: [{ id: AGENT_ID, name: "Test Agent" }],
  });

  const agentDir = path.join(
    repoRoot,
    ".inconvo",
    "agents",
    "test-agent",
  );

  const agentDoc = YAML.parse(
    await fs.readFile(path.join(agentDir, "agent.yaml"), "utf8"),
  ) as Record<string, unknown>;
  assert.equal(agentDoc.id, AGENT_ID);
  assert.equal(agentDoc.name, "Test Agent");
  assert.equal(agentDoc.orgId, ORG_ID);
  assert.equal(agentDoc.orgName, "Test Org");
  assert.equal("pulledAt" in agentDoc, false);

  const userContextDoc = YAML.parse(
    await fs.readFile(path.join(agentDir, "user-context.yaml"), "utf8"),
  ) as {
    fields: Array<{ key: string }>;
  };
  assert.deepEqual(
    userContextDoc.fields.map((field) => field.key),
    ["alpha", "zeta"],
  );

  // Connection tables should be stored once in canonical snapshots.
  const tableDir = path.join(
    repoRoot,
    ".inconvo",
    "connections",
    "second",
    "tables",
  );
  const tableFiles = await fs.readdir(tableDir);
  assert.deepEqual(tableFiles.sort(), [".slug-map.yaml", "a.yaml", "b.yaml"]);
  const agentConnectionDoc = YAML.parse(
    await fs.readFile(
      path.join(agentDir, "connections", "second", "connection.yaml"),
      "utf8",
    ),
  ) as { snapshotPath?: string; description?: string };
  assert.equal(agentConnectionDoc.snapshotPath, ".inconvo/connections/second");
  assert.equal(agentConnectionDoc.description, "Primary warehouse");
  const canonicalConnectionDoc = YAML.parse(
    await fs.readFile(
      path.join(repoRoot, ".inconvo", "connections", "second", "connection.yaml"),
      "utf8",
    ),
  ) as { description?: string };
  assert.equal(canonicalConnectionDoc.description, "Primary warehouse");
  const shareableConnectionsDoc = YAML.parse(
    await fs.readFile(path.join(agentDir, "shareable-connections.yaml"), "utf8"),
  ) as Array<{ description?: string }>;
  assert.equal(shareableConnectionsDoc[0]?.description, "Finance reporting source");
  await assert.rejects(
    fs.access(path.join(agentDir, "connections", "second", "tables")),
    /ENOENT/,
  );

  await assert.rejects(
    fs.access(path.join(agentDir, "state.lock.yaml")),
    /ENOENT/,
  );
});

test("pull with selectedConnectionId fetches only requested connection", async (t) => {
  const repoRoot = await withTempRepo(t);
  const fetchedConnections: string[] = [];

  const client = createClientStub({
    getAgentUserContext: async () => ({
      userContext: { status: "ENABLED", fields: [] },
      hash: "uc_hash",
    }),
    listAgentConnections: async () => [
      {
        id: "con_a",
        name: "A",
        description: null,
        status: null,
        isShared: false,
        ownerAgentName: "Owner",
      },
      {
        id: "con_b",
        name: "B",
        description: null,
        status: null,
        isShared: false,
        ownerAgentName: "Owner",
      },
    ],
    getConnectionSemanticModel: async (_agentId: string, connectionId: string) => {
      fetchedConnections.push(connectionId);
      return {
        connectionId,
        tables: [],
        hash: `hash_${connectionId}`,
      };
    },
    listShareableConnections: async () => [],
  });

  const result = await pullAgentsToWorkspace({
    client,
    repoRoot,
    org: { id: ORG_ID, name: "Test Org" },
    selectedAgents: [{ id: AGENT_ID, name: "Test Agent" }],
    selectedConnectionId: "con_b",
  });

  assert.equal(result.pulledConnections, 1);
  assert.deepEqual(fetchedConnections, ["con_b"]);
});

test("syncSingleAgentToWorkspace fails if agent is missing from org listing", async (t) => {
  const repoRoot = await withTempRepo(t);

  const client = createClientStub({
    getOrg: async () => ({ id: ORG_ID, name: "Test Org" }),
    listOrgAgents: async () => [{ id: "agt_other", name: "Other" }],
  });

  await assert.rejects(
    () =>
      syncSingleAgentToWorkspace({
        client,
        repoRoot,
        agentId: AGENT_ID,
      }),
    /Agent agt_test not found in organization org_test/,
  );
});

test("syncSingleAgentToWorkspace preserves unrelated local agent directories", async (t) => {
  const repoRoot = await withTempRepo(t);
  const otherAgentDir = path.join(
    repoRoot,
    ".inconvo",
    "agents",
    "other-agent",
  );
  await fs.mkdir(otherAgentDir, { recursive: true });
  await fs.writeFile(path.join(otherAgentDir, "agent.yaml"), "id: agt_other\n");

  const client = createClientStub({
    getOrg: async () => ({ id: ORG_ID, name: "Test Org" }),
    listOrgAgents: async () => [{ id: AGENT_ID, name: "Test Agent" }],
    getAgentUserContext: async () => ({
      userContext: { status: "ENABLED", fields: [] },
      hash: "uc_hash",
    }),
    listAgentConnections: async () => [
      {
        id: "con_a",
        name: "A",
        description: null,
        status: null,
        isShared: false,
        ownerAgentName: "Owner",
      },
    ],
    getConnectionSemanticModel: async () => ({
      connectionId: "con_a",
      tables: [],
      hash: "hash_con_a",
    }),
    listShareableConnections: async () => [],
  });

  await syncSingleAgentToWorkspace({
    client,
    repoRoot,
    agentId: AGENT_ID,
  });

  await fs.access(path.join(otherAgentDir, "agent.yaml"));
});

test("pull writes collision-safe slugs without IDs in folder/file names", async (t) => {
  const repoRoot = await withTempRepo(t);

  const client = createClientStub({
    getAgentUserContext: async () => ({
      userContext: { status: "ENABLED", fields: [] },
      hash: "uc_hash",
    }),
    listAgentConnections: async () => [
      {
        id: "con_1",
        name: "Sales Data",
        description: null,
        status: null,
        isShared: false,
        ownerAgentName: "Owner",
      },
      {
        id: "con_2",
        name: "sales-data",
        description: null,
        status: null,
        isShared: false,
        ownerAgentName: "Owner",
      },
    ],
    getConnectionSemanticModel: async (_agentId: string, connectionId: string) => ({
      connectionId,
      tables:
        connectionId === "con_1"
          ? [
              {
                id: "tbl_1",
                name: "Orders",
                access: "OFF" as const,
                columns: [],
                computedColumns: [],
                outwardRelations: [],
              },
              {
                id: "tbl_2",
                name: "orders",
                access: "OFF" as const,
                columns: [],
                computedColumns: [],
                outwardRelations: [],
              },
            ]
          : [],
      hash: `hash_${connectionId}`,
    }),
    listShareableConnections: async () => [],
  });

  await pullAgentsToWorkspace({
    client,
    repoRoot,
    org: { id: ORG_ID, name: "Test Org" },
    selectedAgents: [{ id: AGENT_ID, name: "Test Agent" }],
  });

  const agentConnectionsDir = path.join(
    repoRoot,
    ".inconvo",
    "agents",
    "test-agent",
    "connections",
  );
  const connectionEntries = await fs.readdir(agentConnectionsDir);
  assert.equal(connectionEntries.includes("sales-data"), true);
  assert.equal(connectionEntries.includes("sales-data-2"), true);
  assert.equal(connectionEntries.includes("con_1"), false);
  assert.equal(connectionEntries.includes("con_2"), false);

  const canonicalConnectionsDir = path.join(
    repoRoot,
    ".inconvo",
    "connections",
  );
  const connectionSlugMap = YAML.parse(
    await fs.readFile(
      path.join(canonicalConnectionsDir, ".slug-map.yaml"),
      "utf8",
    ),
  ) as Record<string, string>;
  assert.equal(connectionSlugMap.con_1, "sales-data");
  assert.equal(connectionSlugMap.con_2, "sales-data-2");

  const tablesDir = path.join(canonicalConnectionsDir, "sales-data", "tables");
  const tableEntries = await fs.readdir(tablesDir);
  assert.equal(tableEntries.includes("orders.yaml"), true);
  assert.equal(tableEntries.includes("orders-2.yaml"), true);
  assert.equal(
    tableEntries.some((entry) => entry.includes("tbl_")),
    false,
  );

  const tableSlugMap = YAML.parse(
    await fs.readFile(path.join(tablesDir, ".slug-map.yaml"), "utf8"),
  ) as Record<string, string>;
  const assignedTableSlugs = new Set(Object.values(tableSlugMap));
  assert.equal(assignedTableSlugs.has(slugify("Orders")), true);
  assert.equal(assignedTableSlugs.has(`${slugify("Orders")}-2`), true);
});

test("pull ignores unsafe persisted slugs from slug-map files", async (t) => {
  const repoRoot = await withTempRepo(t);
  const connectionsDir = path.join(repoRoot, ".inconvo", "connections");
  const safeConnectionSlug = slugify("Safe Name");
  const safeTableSlug = slugify("Orders");
  const seededTableDir = path.join(connectionsDir, safeConnectionSlug, "tables");

  await fs.mkdir(seededTableDir, { recursive: true });
  await fs.writeFile(
    path.join(connectionsDir, ".slug-map.yaml"),
    YAML.stringify({ con_safe: "../../escaped-connection" }),
  );
  await fs.writeFile(
    path.join(seededTableDir, ".slug-map.yaml"),
    YAML.stringify({ tbl_1: "../../../escaped-table" }),
  );

  const client = createClientStub({
    getAgentUserContext: async () => ({
      userContext: { status: "ENABLED", fields: [] },
      hash: "uc_hash",
    }),
    listAgentConnections: async () => [
      {
        id: "con_safe",
        name: "Safe Name",
        description: null,
        status: null,
        isShared: false,
        ownerAgentName: "Owner",
      },
    ],
    getConnectionSemanticModel: async () => ({
      connectionId: "con_safe",
      tables: [
        {
          id: "tbl_1",
          name: "Orders",
          access: "OFF" as const,
          columns: [],
          computedColumns: [],
          outwardRelations: [],
        },
      ],
      hash: "hash_safe",
    }),
    listShareableConnections: async () => [],
  });

  await pullAgentsToWorkspace({
    client,
    repoRoot,
    org: { id: ORG_ID, name: "Test Org" },
    selectedAgents: [{ id: AGENT_ID, name: "Test Agent" }],
  });

  const connectionSlugMap = YAML.parse(
    await fs.readFile(path.join(connectionsDir, ".slug-map.yaml"), "utf8"),
  ) as Record<string, string>;
  assert.equal(connectionSlugMap.con_safe, safeConnectionSlug);

  const tableSlugMap = YAML.parse(
    await fs.readFile(path.join(seededTableDir, ".slug-map.yaml"), "utf8"),
  ) as Record<string, string>;
  assert.equal(tableSlugMap.tbl_1, safeTableSlug);

  await fs.access(path.join(seededTableDir, `${safeTableSlug}.yaml`));
  await assert.rejects(fs.access(path.join(repoRoot, "escaped-connection")), /ENOENT/);
  await assert.rejects(
    fs.access(path.join(repoRoot, ".inconvo", "escaped-table.yaml")),
    /ENOENT/,
  );
});

test("linked connection is stored once and referenced by multiple agents", async (t) => {
  const repoRoot = await withTempRepo(t);

  const ownerAgent = { id: "agt_owner", name: "Owner" };
  const consumerAgent = { id: "agt_consumer", name: "Consumer" };
  const sharedConnection = {
    id: "con_shared",
    name: "Shared Warehouse",
    description: "Shared analytics warehouse",
    status: "CONNECTED",
    ownerAgentName: "Owner",
  };

  const client = createClientStub({
    getAgentUserContext: async () => ({
      userContext: { status: "ENABLED", fields: [] },
      hash: "uc_hash",
    }),
    listAgentConnections: async (agentId: string) => {
      if (agentId === ownerAgent.id) {
        return [{ ...sharedConnection, isShared: false }];
      }
      if (agentId === consumerAgent.id) {
        return [{ ...sharedConnection, isShared: true }];
      }
      return [];
    },
    getConnectionSemanticModel: async () => ({
      connectionId: sharedConnection.id,
      tables: [],
      hash: "hash_shared",
    }),
    listShareableConnections: async () => [],
  });

  await pullAgentsToWorkspace({
    client,
    repoRoot,
    org: { id: ORG_ID, name: "Test Org" },
    selectedAgents: [ownerAgent, consumerAgent],
  });

  const canonicalConnectionsDir = path.join(repoRoot, ".inconvo", "connections");
  const canonicalEntries = await fs.readdir(canonicalConnectionsDir);
  assert.equal(canonicalEntries.includes("shared-warehouse"), true);

  const ownerRef = YAML.parse(
    await fs.readFile(
      path.join(
        repoRoot,
        ".inconvo",
        "agents",
        "owner",
        "connections",
        "shared-warehouse",
        "connection.yaml",
      ),
      "utf8",
    ),
  ) as { snapshotPath?: string };
  const consumerRef = YAML.parse(
    await fs.readFile(
      path.join(
        repoRoot,
        ".inconvo",
        "agents",
        "consumer",
        "connections",
        "shared-warehouse",
        "connection.yaml",
      ),
      "utf8",
    ),
  ) as { snapshotPath?: string };
  assert.equal(ownerRef.snapshotPath, ".inconvo/connections/shared-warehouse");
  assert.equal(consumerRef.snapshotPath, ".inconvo/connections/shared-warehouse");
});
