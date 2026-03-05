import { test } from "node:test";
import assert from "node:assert/strict";
import { parseColumnPairs, parseIntArg } from "../mutate-args.js";

test("parseColumnPairs accepts valid source:target pairs", () => {
  assert.deepEqual(parseColumnPairs(["source:target"]), [
    {
      sourceColumnName: "source",
      targetColumnName: "target",
    },
  ]);
});

test("parseColumnPairs rejects malformed values with extra delimiters", () => {
  assert.throws(
    () => parseColumnPairs(["source:target:extra"]),
    /Invalid pair "source:target:extra"/,
  );
});

test("parseColumnPairs rejects empty source or target", () => {
  assert.throws(() => parseColumnPairs(["source:"]), /Invalid pair "source:"/);
  assert.throws(() => parseColumnPairs([":target"]), /Invalid pair ":target"/);
});

test("parseIntArg accepts canonical integer input", () => {
  assert.equal(parseIntArg("5"), 5);
});

test("parseIntArg rejects decimal or mixed-string input", () => {
  assert.throws(() => parseIntArg("1.5"), /Invalid integer value "1.5"/);
  assert.throws(() => parseIntArg("10abc"), /Invalid integer value "10abc"/);
});
