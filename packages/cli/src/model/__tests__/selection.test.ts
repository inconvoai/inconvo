import { test } from "node:test";
import assert from "node:assert/strict";
import {
  resolvePullTargetMode,
  validatePullFlags,
} from "../selection.js";

test("pull flags: connection requires exactly one agent", () => {
  const error = validatePullFlags({
    agentIds: ["a1", "a2"],
    allAgents: false,
    connectionId: "c1",
  });
  assert.equal(error, "--connection requires exactly one --agent.");
});

test("pull flags: connection cannot be combined with all-agents", () => {
  const error = validatePullFlags({
    agentIds: [],
    allAgents: true,
    connectionId: "c1",
  });
  assert.equal(error, "--connection cannot be combined with --all-agents.");
});

test("pull flags: all-agents cannot be combined with explicit agent ids", () => {
  const error = validatePullFlags({
    agentIds: ["a1"],
    allAgents: true,
    connectionId: undefined,
  });
  assert.equal(error, "--all-agents cannot be combined with --agent.");
});

test("pull mode: explicit agent ids are resolved first", () => {
  const mode = resolvePullTargetMode({
    input: {
      agentIds: ["a1"],
      allAgents: false,
      connectionId: undefined,
    },
    interactive: true,
  });

  assert.equal(mode.kind, "explicit_agents");
  if (mode.kind === "explicit_agents") {
    assert.deepEqual(mode.agentIds, ["a1"]);
  }
});

test("pull mode: all-agents is resolved when provided", () => {
  const mode = resolvePullTargetMode({
    input: {
      agentIds: [],
      allAgents: true,
      connectionId: undefined,
    },
    interactive: false,
  });

  assert.equal(mode.kind, "all_agents");
});

test("pull mode: no flags errors in non-interactive mode", () => {
  const mode = resolvePullTargetMode({
    input: {
      agentIds: [],
      allAgents: false,
      connectionId: undefined,
    },
    interactive: false,
  });
  assert.equal(mode.kind, "error");
});

test("pull mode: interactive fallback when no explicit selector provided", () => {
  const mode = resolvePullTargetMode({
    input: {
      agentIds: [],
      allAgents: false,
      connectionId: undefined,
    },
    interactive: true,
  });
  assert.equal(mode.kind, "interactive");
});
