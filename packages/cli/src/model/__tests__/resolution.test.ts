import { test } from "node:test";
import assert from "node:assert/strict";
import {
  resolveByIdOrName,
  resolveTable,
  resolveUserContextField,
} from "../resolution.js";

test("resolveByIdOrName prefers id over name", () => {
  const resolved = resolveByIdOrName(
    [
      { id: "id_1", name: "orders" },
      { id: "id_2", name: "id_1" },
    ],
    "id_1",
    "table",
  );

  assert.equal(resolved.id, "id_1");
  assert.equal(resolved.name, "orders");
});

test("resolveByIdOrName matches case-insensitive unique names", () => {
  const resolved = resolveByIdOrName(
    [{ id: "id_1", name: "Orders" }],
    "orders",
    "table",
  );
  assert.equal(resolved.id, "id_1");
});

test("resolveTable throws with candidate list when missing", () => {
  assert.throws(
    () =>
      resolveTable(
        [
          {
            id: "tbl_1",
            name: "orders",
            access: "OFF",
            columns: [],
            computedColumns: [],
            outwardRelations: [],
          },
        ],
        "missing_table",
      ),
    /Available table: orders \(tbl_1\)/,
  );
});

test("resolveUserContextField resolves by key", () => {
  const resolved = resolveUserContextField(
    {
      userContext: {
        status: "ENABLED",
        fields: [
          {
            id: "ucf_1",
            key: "org_id",
            type: "STRING",
          },
        ],
      },
      hash: "hash_1",
    },
    "org_id",
  );

  assert.equal(resolved.id, "ucf_1");
  assert.equal(resolved.key, "org_id");
});
