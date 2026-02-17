import {
  ENUM_FETCH_LIMIT,
  MAX_ENUM_ENTRIES,
  NUMERIC_OR_STRING_LOGICAL_TYPES,
  normalizeStaticEnumEntries,
  type ValueEnumEntryInput,
} from "@repo/schema-utils";

export {
  ENUM_FETCH_LIMIT,
  MAX_ENUM_ENTRIES,
  NUMERIC_OR_STRING_LOGICAL_TYPES,
  type ValueEnumEntryInput,
};

export const normalizeEntries = normalizeStaticEnumEntries;
