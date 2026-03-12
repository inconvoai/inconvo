import { afterEach, test } from "node:test";
import assert from "node:assert/strict";
import * as fs from "fs/promises";
import * as os from "os";
import * as path from "path";
import { createApiClientFromOptions } from "../cli-options.js";

const originalFetch = globalThis.fetch;
const originalApiKey = process.env.INCONVO_API_KEY;
const originalApiBaseUrl = process.env.INCONVO_API_BASE_URL;

const tempDirs: string[] = [];

afterEach(async () => {
  globalThis.fetch = originalFetch;

  if (originalApiKey === undefined) {
    delete process.env.INCONVO_API_KEY;
  } else {
    process.env.INCONVO_API_KEY = originalApiKey;
  }

  if (originalApiBaseUrl === undefined) {
    delete process.env.INCONVO_API_BASE_URL;
  } else {
    process.env.INCONVO_API_BASE_URL = originalApiBaseUrl;
  }

  for (const dir of tempDirs) {
    await fs.rm(dir, { recursive: true, force: true });
  }
  tempDirs.length = 0;
});

async function makeRepo(): Promise<string> {
  const dir = await fs.mkdtemp(path.join(os.tmpdir(), "inconvo-cli-options-"));
  tempDirs.push(dir);
  return dir;
}

test("createApiClientFromOptions reads credentials from repo .env", async () => {
  const repoRoot = await makeRepo();
  await fs.writeFile(
    path.join(repoRoot, ".env"),
    [
      "INCONVO_API_KEY=repo-env-key",
      "INCONVO_API_BASE_URL=https://repo-env.example",
      "",
    ].join("\n"),
  );

  let requestUrl = "";
  let authHeader = "";
  globalThis.fetch = async (url, init) => {
    requestUrl = String(url);
    authHeader = String((init?.headers as Record<string, string>).Authorization);
    return {
      ok: true,
      status: 200,
      text: async () => JSON.stringify({ agents: [] }),
    } as Response;
  };

  const client = await createApiClientFromOptions({ repoRoot });
  await client.listOrgAgents();

  assert.equal(requestUrl, "https://repo-env.example/api/v1/org/agents");
  assert.equal(authHeader, "Bearer repo-env-key");
});

test("createApiClientFromOptions prefers process env over repo .env", async () => {
  const repoRoot = await makeRepo();
  await fs.writeFile(
    path.join(repoRoot, ".env"),
    [
      "INCONVO_API_KEY=repo-env-key",
      "INCONVO_API_BASE_URL=https://repo-env.example",
      "",
    ].join("\n"),
  );
  process.env.INCONVO_API_KEY = "shell-key";
  process.env.INCONVO_API_BASE_URL = "https://shell.example";

  let requestUrl = "";
  let authHeader = "";
  globalThis.fetch = async (url, init) => {
    requestUrl = String(url);
    authHeader = String((init?.headers as Record<string, string>).Authorization);
    return {
      ok: true,
      status: 200,
      text: async () => JSON.stringify({ agents: [] }),
    } as Response;
  };

  const client = await createApiClientFromOptions({ repoRoot });
  await client.listOrgAgents();

  assert.equal(requestUrl, "https://shell.example/api/v1/org/agents");
  assert.equal(authHeader, "Bearer shell-key");
});
