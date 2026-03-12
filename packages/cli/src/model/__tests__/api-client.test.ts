import { afterEach, test } from "node:test";
import assert from "node:assert/strict";
import { PlatformApiClient, PlatformApiError } from "../api-client.js";

type MockResponseInit = {
  ok: boolean;
  status: number;
  body: unknown;
};

function jsonResponse(init: MockResponseInit): Response {
  return {
    ok: init.ok,
    status: init.status,
    text: async () => JSON.stringify(init.body),
  } as unknown as Response;
}

const originalFetch = globalThis.fetch;

afterEach(() => {
  globalThis.fetch = originalFetch;
});

test("listOrgAgents accepts wrapped payload", async () => {
  globalThis.fetch = async () =>
    jsonResponse({
      ok: true,
      status: 200,
      body: {
        agents: [{ id: "agt_1", name: "Agent One" }],
      },
    });

  const client = new PlatformApiClient({
    baseUrl: "https://example.com",
    apiKey: "test-key",
  });

  const agents = await client.listOrgAgents();
  assert.deepEqual(agents, [{ id: "agt_1", name: "Agent One" }]);
});

test("listOrgAgents accepts legacy bare-array payload", async () => {
  globalThis.fetch = async () =>
    jsonResponse({
      ok: true,
      status: 200,
      body: [{ id: "agt_2", name: "Agent Two" }],
    });

  const client = new PlatformApiClient({
    baseUrl: "https://example.com",
    apiKey: "test-key",
  });

  const agents = await client.listOrgAgents();
  assert.deepEqual(agents, [{ id: "agt_2", name: "Agent Two" }]);
});

test("listOrgAgents throws clear error on invalid payload", async () => {
  globalThis.fetch = async () =>
    jsonResponse({
      ok: true,
      status: 200,
      body: { data: [] },
    });

  const client = new PlatformApiClient({
    baseUrl: "https://example.com",
    apiKey: "test-key",
  });

  await assert.rejects(
    () => client.listOrgAgents(),
    /Unexpected response from \/org\/agents/,
  );
});

test("runModelAction posts action payload to model/actions endpoint", async () => {
  let requestUrl = "";
  let requestInit: RequestInit | undefined;
  globalThis.fetch = async (url, init) => {
    requestUrl = String(url);
    requestInit = init;
    return jsonResponse({
      ok: true,
      status: 200,
      body: {
        action: "table.setAccess",
        result: { tableId: "tbl_1", access: "QUERYABLE" },
      },
    });
  };

  const client = new PlatformApiClient({
    baseUrl: "https://example.com",
    apiKey: "test-key",
  });

  const response = await client.runModelAction("agt_1", {
    action: "table.setAccess",
    payload: { tableId: "tbl_1", access: "QUERYABLE" },
  });

  assert.equal(
    requestUrl,
    "https://example.com/api/v1/agents/agt_1/model/actions",
  );
  assert.equal(requestInit?.method, "POST");
  assert.equal(
    requestInit?.body,
    JSON.stringify({
      action: "table.setAccess",
      payload: { tableId: "tbl_1", access: "QUERYABLE" },
    }),
  );
  assert.deepEqual(response, {
    action: "table.setAccess",
    result: { tableId: "tbl_1", access: "QUERYABLE" },
  });
});

test("request fallback uses HTTP status when error payload is empty string", async () => {
  globalThis.fetch = async () =>
    jsonResponse({
      ok: false,
      status: 500,
      body: { error: "" },
    });

  const client = new PlatformApiClient({
    baseUrl: "https://example.com",
    apiKey: "test-key",
  });

  await assert.rejects(
    () => client.listOrgAgents(),
    (error: unknown) => {
      assert.ok(error instanceof PlatformApiError);
      assert.equal(error.message, "HTTP 500");
      return true;
    },
  );
});

test("listShareableConnections falls back to legacy context", async () => {
  globalThis.fetch = async () =>
    jsonResponse({
      ok: true,
      status: 200,
      body: {
        connections: [
          {
            id: "con_2",
            name: "Finance",
            context: "Legacy context field",
            databaseType: "POSTGRES",
            ownerAgent: {
              id: "agt_owner",
              name: "Owner",
            },
          },
        ],
      },
    });

  const client = new PlatformApiClient({
    baseUrl: "https://example.com",
    apiKey: "test-key",
  });

  const connections = await client.listShareableConnections("agt_1");
  assert.equal(connections[0]?.description, "Legacy context field");
});

test("updateConnectionDescription patches the connection endpoint", async () => {
  let requestUrl = "";
  let requestInit: RequestInit | undefined;
  globalThis.fetch = async (url, init) => {
    requestUrl = String(url);
    requestInit = init;
    return jsonResponse({
      ok: true,
      status: 200,
      body: { id: "con_4", name: "ERP", description: null },
    });
  };

  const client = new PlatformApiClient({
    baseUrl: "https://example.com",
    apiKey: "test-key",
  });

  await client.updateConnectionDescription("agt_1", "con_4", null);

  assert.equal(
    requestUrl,
    "https://example.com/api/v1/agents/agt_1/connections/con_4",
  );
  assert.equal(requestInit?.method, "PATCH");
  assert.equal(requestInit?.body, JSON.stringify({ description: null }));
});
