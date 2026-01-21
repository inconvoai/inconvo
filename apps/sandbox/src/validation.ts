/**
 * Validation utilities for path-safe context keys, values, and userIdentifiers.
 * These ensure values can be safely used in R2 storage paths without
 * path traversal vulnerabilities or delimiter conflicts.
 */

/**
 * Pattern for safe context keys.
 * Allows: alphanumeric, underscore, hyphen, period
 * Rejects: colons (path delimiter), slashes, spaces, etc.
 */
const SAFE_KEY_PATTERN = /^[a-zA-Z0-9_\-.]+$/;

/**
 * Pattern for safe context values.
 * Allows: alphanumeric, underscore, hyphen, period, @ symbol
 * Rejects: colons (path delimiter), slashes, spaces, etc.
 */
const SAFE_VALUE_PATTERN = /^[a-zA-Z0-9_\-.@]+$/;

export function isValidContextKey(key: string): boolean {
  return key.length > 0 && key.length <= 256 && SAFE_KEY_PATTERN.test(key);
}

export function isValidContextValue(value: string): boolean {
  return value.length > 0 && value.length <= 256 && SAFE_VALUE_PATTERN.test(value);
}

export function isValidUserIdentifier(userIdentifier: string): boolean {
  return (
    userIdentifier.length > 0 &&
    userIdentifier.length <= 256 &&
    SAFE_VALUE_PATTERN.test(userIdentifier)
  );
}

export interface ValidationError {
  error: string;
}

/**
 * Validates a context key for use in storage paths.
 * Returns an error message if invalid, null if valid.
 */
export function validateContextKey(key: string): string | null {
  if (!key) {
    return "contextKey is required";
  }
  if (key.length > 256) {
    return "contextKey must be at most 256 characters";
  }
  if (!SAFE_KEY_PATTERN.test(key)) {
    return "contextKey can only contain alphanumeric characters, underscore, hyphen, and period";
  }
  return null;
}

/**
 * Validates a context value for use in storage paths.
 * Returns an error message if invalid, null if valid.
 */
export function validateContextValue(value: string): string | null {
  if (!value) {
    return "contextValue is required";
  }
  if (value.length > 256) {
    return "contextValue must be at most 256 characters";
  }
  if (!SAFE_VALUE_PATTERN.test(value)) {
    return "contextValue can only contain alphanumeric characters, underscore, hyphen, period, and @ symbol";
  }
  return null;
}

/**
 * Validates a userIdentifier for use in storage paths.
 * Returns an error message if invalid, null if valid.
 */
export function validateUserIdentifier(userIdentifier: string): string | null {
  if (!userIdentifier) {
    return "userIdentifier is required";
  }
  if (userIdentifier.length > 256) {
    return "userIdentifier must be at most 256 characters";
  }
  if (!SAFE_VALUE_PATTERN.test(userIdentifier)) {
    return "userIdentifier can only contain alphanumeric characters, underscore, hyphen, period, and @ symbol";
  }
  return null;
}
