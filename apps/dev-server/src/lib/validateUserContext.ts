import { z } from "zod";

export interface UserContextField {
  key: string;
  type: "STRING" | "NUMBER" | "BOOLEAN";
}

/**
 * Safe patterns for context keys and values to prevent path traversal
 * and conflicts with storage path delimiters.
 */
const SAFE_KEY_PATTERN = /^[a-zA-Z0-9_\-.]+$/;
const SAFE_VALUE_PATTERN = /^[a-zA-Z0-9_\-.@]+$/;

function isValidContextKey(key: string): boolean {
  return SAFE_KEY_PATTERN.test(key);
}

function isValidContextValue(value: string): boolean {
  return SAFE_VALUE_PATTERN.test(value);
}

/**
 * Validate userContext against configured schema.
 * Mirrors platform behavior (allows unknown keys to be stripped).
 */
export function validateUserContextAgainstSchema(
  userContext: Record<string, unknown>,
  fields: UserContextField[],
): Record<string, string | number | boolean> {
  if (fields.length === 0) {
    for (const [key, value] of Object.entries(userContext)) {
      if (!isValidContextKey(key)) {
        throw new Error(
          `Invalid userContext key "${key}". Keys can only contain alphanumeric characters, underscore, hyphen, and period.`,
        );
      }
      if (typeof value === "string" && !isValidContextValue(value)) {
        throw new Error(
          `Invalid userContext value for "${key}". String values can only contain alphanumeric characters, underscore, hyphen, period, and @ symbol.`,
        );
      }
    }
    return userContext as Record<string, string | number | boolean>;
  }

  const schemaShape = fields.reduce(
    (acc, field) => {
      if (!isValidContextKey(field.key)) {
        throw new Error(
          `Invalid field key "${field.key}" in configuration.`,
        );
      }
      acc[field.key] =
        field.type === "NUMBER"
          ? z.number()
          : field.type === "BOOLEAN"
            ? z.boolean()
            : z.string();
      return acc;
    },
    {} as Record<string, z.ZodNumber | z.ZodString | z.ZodBoolean>,
  );

  const schema = z.object(schemaShape);
  const result = schema.safeParse(userContext);
  if (!result.success) {
    const issues = result.error.issues
      .map((i) => `${i.path.join(".")}: ${i.message}`)
      .join(", ");
    throw new Error(`Invalid userContext: ${issues}`);
  }

  for (const [key, value] of Object.entries(result.data)) {
    if (typeof value === "string" && !isValidContextValue(value)) {
      throw new Error(
        `Invalid userContext value for "${key}". String values can only contain alphanumeric characters, underscore, hyphen, period, and @ symbol.`,
      );
    }
  }

  return result.data;
}
