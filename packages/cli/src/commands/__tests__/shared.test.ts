import { test } from "node:test";
import assert from "node:assert/strict";
import {
  parseConnectionCommandOptions,
  parseModelPullCommandOptions,
  requireConnectionInNonInteractiveMode,
} from "../_shared/options.js";
import {
  resolveConnectionAgentContext,
  resolveConnectionTargetContext,
} from "../_shared/connection-command-factory.js";
import { resolveModelPullContext } from "../_shared/model-command-factory.js";
import { runCliAction } from "../_shared/command-runtime.js";

test("parseModelPullCommandOptions normalizes repeatable agents and api settings", () => {
  const parsed = parseModelPullCommandOptions({
    agent: [" agt_1 ", "", "agt_2"],
    allAgents: true,
    connection: " con_1 ",
    apiBaseUrl: " https://example.com ",
    apiKey: " test_key ",
  });

  assert.deepEqual(parsed, {
    agentIds: ["agt_1", "agt_2"],
    allAgents: true,
    connectionId: "con_1",
    json: false,
    apiBaseUrl: "https://example.com",
    apiKey: "test_key",
  });
});

test("parseConnectionCommandOptions trims required/optional connection command fields", () => {
  const parsed = parseConnectionCommandOptions({
    agent: " agt_3 ",
    connection: " con_3 ",
    apiKey: " api_key ",
  });

  assert.deepEqual(parsed, {
    agentId: "agt_3",
    connectionId: "con_3",
    json: false,
    apiBaseUrl: undefined,
    apiKey: "api_key",
  });
});

test("requireConnectionInNonInteractiveMode enforces non-interactive constraint", () => {
  assert.equal(
    requireConnectionInNonInteractiveMode({
      connectionId: "con_5",
      interactive: false,
    }),
    "con_5",
  );

  assert.equal(
    requireConnectionInNonInteractiveMode({
      connectionId: undefined,
      interactive: true,
    }),
    undefined,
  );

  assert.throws(
    () =>
      requireConnectionInNonInteractiveMode({
        connectionId: undefined,
        interactive: false,
      }),
    /--connection is required in non-interactive mode/,
  );
});

test("resolveConnectionAgentContext narrows agent and creates client", async () => {
  const context = await resolveConnectionAgentContext({
    agent: " agt_1 ",
    apiKey: "test_key",
  });

  assert.equal(context.agentId, "agt_1");
  assert.ok(context.client);
});

test("resolveConnectionTargetContext enforces non-interactive rules", () => {
  assert.equal(
    resolveConnectionTargetContext({
      connectionId: "con_1",
      interactive: false,
    }).connectionId,
    "con_1",
  );

  assert.equal(
    resolveConnectionTargetContext({
      connectionId: undefined,
      interactive: true,
    }).connectionId,
    undefined,
  );
});

test("resolveModelPullContext normalizes repeatable agents", async () => {
  const context = await resolveModelPullContext({
    agent: [" agt_1 ", "agt_1", "", "agt_2"],
    apiKey: "test_key",
  });

  assert.deepEqual(context.agentIds, ["agt_1", "agt_2"]);
  assert.equal(context.parsedOptions.connectionId, undefined);
});

test("resolveModelPullContext preserves pull flag validation errors", async () => {
  await assert.rejects(
    () =>
      resolveModelPullContext({
        allAgents: true,
        connection: "con_1",
        apiKey: "test_key",
      }),
    /--connection cannot be combined with --all-agents\./,
  );
});

test("runCliAction exits with code 1 when action throws", async () => {
  const originalExit = process.exit;
  const originalConsoleError = console.error;
  let exitCode: number | undefined;

  try {
    console.error = () => undefined;
    process.exit = ((code?: number) => {
      exitCode = code;
      throw new Error(`process_exit_${code}`);
    }) as typeof process.exit;

    await assert.rejects(
      () =>
        runCliAction(async () => {
          throw new Error("boom");
        }),
      /process_exit_1/,
    );

    assert.equal(exitCode, 1);
  } finally {
    process.exit = originalExit;
    console.error = originalConsoleError;
  }
});
