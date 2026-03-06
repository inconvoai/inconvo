import { test } from "node:test";
import assert from "node:assert/strict";
import {
  MODEL_ACTION_DEFINITIONS,
  resolveActionDefinition,
} from "../action-registry.js";
import { MODEL_ACTION_TYPES } from "../types.js";

test("action registry includes all model action types", () => {
  assert.equal(MODEL_ACTION_DEFINITIONS.length, MODEL_ACTION_TYPES.length);
  assert.deepEqual(
    MODEL_ACTION_DEFINITIONS.map((definition) => definition.action),
    [...MODEL_ACTION_TYPES],
  );
});

test("resolveActionDefinition returns metadata for known action", () => {
  const definition = resolveActionDefinition("table.setAccess");
  assert.equal(definition?.action, "table.setAccess");
  assert.equal(definition?.requiresConnection, true);
});

test("resolveActionDefinition returns undefined for unknown action", () => {
  const definition = resolveActionDefinition("unknown.action");
  assert.equal(definition, undefined);
});
