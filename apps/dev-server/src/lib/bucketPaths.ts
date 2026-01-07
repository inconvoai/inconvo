export type RequestContextValues =
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
      `Request context ${label} contains disallowed characters (: / \\ ,): "${part}"`,
    );
  }
};

/**
 * Builds a flat string representation of request context values in the format
 * "key:value, otherkey:othervalue".
 * Keys are sorted alphabetically to ensure consistent paths.
 */
export const buildRequestContextPath = (
  requestContext: RequestContextValues,
): string => {
  if (!requestContext) return "";

  return Object.entries(requestContext)
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
 * Builds a bucket path of the form "bucket-name:/orgId/agentId/<requestContext>/"
 * where requestContext is optional.
 */
export const buildBucketPath = (
  bucketName: string,
  organisationId: string,
  agentId: string,
  requestContextFlatString: string,
): string => {
  const requestContextSuffix = requestContextFlatString
    ? `${requestContextFlatString}/`
    : "";
  return `${bucketName}:/${organisationId}/${agentId}/${requestContextSuffix}`;
};

export const getBucketBaseNames = () => {
  return {
    conversationData: "customer-conversation-data-dev",
    datasets: "customer-datasets-dev",
  };
};
