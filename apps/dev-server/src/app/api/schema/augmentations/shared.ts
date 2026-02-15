import { NUMERIC_LOGICAL_TYPES } from "@repo/types";

export const MAX_ENUM_ENTRIES = 50;
export const ENUM_FETCH_LIMIT = MAX_ENUM_ENTRIES + 1;
export const NUMERIC_OR_STRING_LOGICAL_TYPES = new Set([
  "string",
  ...NUMERIC_LOGICAL_TYPES,
]);

export type ValueEnumEntryInput = {
  value: string | number;
  label: string;
  selected?: boolean;
  position?: number;
};

export function normalizeEntries(params: {
  entries: ValueEnumEntryInput[];
  effectiveType: string;
}) {
  const { entries, effectiveType } = params;
  const isNumeric = NUMERIC_LOGICAL_TYPES.has(effectiveType);
  if (!entries.length) {
    throw new Error("Enum must include at least one entry.");
  }

  const normalized = entries.map((entry, index) => ({
    value: entry.value,
    label: entry.label?.trim(),
    selected: entry.selected ?? true,
    position: entry.position ?? index,
  }));

  const valueKeys = new Set<string>();
  const labelKeys = new Set<string>();

  return normalized
    .map((entry) => {
      if (!entry.label || entry.label.length === 0) {
        throw new Error("Enum labels must not be empty.");
      }

      let canonicalValue: string | number;
      if (isNumeric) {
        if (typeof entry.value === "number" && Number.isFinite(entry.value)) {
          canonicalValue = entry.value;
        } else if (
          typeof entry.value === "string" &&
          Number.isFinite(Number(entry.value))
        ) {
          canonicalValue = Number(entry.value);
        } else {
          throw new Error("Enum values must be numeric for numeric columns.");
        }
      } else {
        if (
          typeof entry.value !== "string" ||
          entry.value.trim().length === 0
        ) {
          throw new Error(
            "Enum values must be non-empty strings for string columns.",
          );
        }
        canonicalValue = entry.value.trim();
      }

      const valueKey =
        typeof canonicalValue === "number"
          ? `n:${canonicalValue}`
          : `s:${canonicalValue.toLowerCase()}`;
      if (valueKeys.has(valueKey)) {
        throw new Error(`Duplicate enum value "${String(canonicalValue)}".`);
      }
      valueKeys.add(valueKey);

      const labelKey = `l:${entry.label.toLowerCase()}`;
      if (labelKeys.has(labelKey)) {
        throw new Error(`Duplicate enum label "${entry.label}".`);
      }
      labelKeys.add(labelKey);

      return {
        value: canonicalValue,
        label: entry.label,
        selected: entry.selected,
        position: entry.position,
      };
    })
    .sort((a, b) => a.position - b.position);
}
