export type UserContextValues =
  | Record<string, string | number>
  | null
  | undefined;

const DISALLOWED_CHARS = /[:\\/,]/;

/**
 * Validates that a key or value does not contain disallowed characters.
 * Throws if invalid.
 */
const validateContextPart = (part: string, label: string): void => {
  if (DISALLOWED_CHARS.test(part)) {
    throw new Error(
      `User context ${label} contains disallowed characters (: / \\ ,): "${part}"`,
    );
  }
};

/**
 * Builds a flat string representation of user context values in the format
 * "key:value, otherkey:othervalue".
 * Keys are sorted alphabetically to ensure consistent paths.
 */
export const buildUserContextPath = (
  userContext: UserContextValues,
): string => {
  if (!userContext) return "";

  return Object.entries(userContext)
    .sort(([a], [b]) => a.localeCompare(b))
    .map(([key, value]) => {
      const strValue = String(value);
      validateContextPart(key, "key");
      validateContextPart(strValue, "value");
      return `${key}:${strValue}`;
    })
    .join(", ");
};

/**
 * Builds a bucket path of the form "bucket-name:/orgId/agentId/<userContext>/"
 * where userContext is optional.
 */
export const buildBucketPath = (
  bucketName: string,
  organisationId: string,
  agentId: string,
  userContextFlatString: string,
): string => {
  const userContextSuffix = userContextFlatString
    ? `${userContextFlatString}/`
    : "";
  return `${bucketName}:/${organisationId}/${agentId}/${userContextSuffix}`;
};

export const getBucketBaseNames = () => {
  return {
    conversationData: "customer-conversation-data-dev",
    datasets: "customer-datasets-dev",
  };
};
