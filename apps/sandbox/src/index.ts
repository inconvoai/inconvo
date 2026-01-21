import { Hono } from "hono";
import type { Context, MiddlewareHandler } from "hono";
import { HTTPException } from "hono/http-exception";
import { getSandbox } from "@cloudflare/sandbox";
import { zValidator } from "@hono/zod-validator";
export { Sandbox } from "@cloudflare/sandbox";

import {
  datasetsListQuerySchema,
  datasetsDeleteByPathQuerySchema,
  conversationDataUploadBodySchema,
  sandboxParamsSchema,
  executeBodySchema,
  type SandboxParams,
} from "./schemas";
import {
  validateContextKey,
  validateContextValue,
  validateUserIdentifier,
} from "./validation";

// Internal mount configuration (not exposed to callers)
interface DatasetMount {
  type: "user" | "context";
  key?: string;
  mountPath: string;
  prefix: string;
}

const CONVERSATION_DATA_MOUNT_PATH = "/conversation_data";
const DATASETS_MOUNT_PATH = "/datasets";
const DATASETS_USER_MOUNT_PATH = "/datasets/user";
const MAX_FILE_SIZE_BYTES = 10 * 1024 * 1024; // 10MB

// Context type for routes that use requireOrgAndAgent middleware
type AuthedContext = Context<{
  Bindings: Env;
  Variables: { orgId: string; agentId: string };
}>;

const BUCKET_OPTIONS = (c: AuthedContext, prefix: string) => ({
  endpoint: "https://54b7b8f961038aedf6044dd0c242e67d.r2.cloudflarestorage.com",
  provider: "r2" as const,
  readOnly: true,
  credentials: {
    accessKeyId: c.env.R2_ACCESS_KEY_ID,
    secretAccessKey: c.env.R2_SECRET_ACCESS_KEY,
  },
  s3fsOptions: ["use_path_request_style", "stat_cache_expire=1"],
  prefix,
});

const isUnsafeFileName = (name: string) =>
  name.includes("..") || name.includes("/") || name.includes("\\");

const isSupportedFileExtension = (name: string) => {
  const lower = name.toLowerCase();
  return lower.endsWith(".json") || lower.endsWith(".csv");
};

/**
 * Extract schema from file content for metadata storage.
 * For CSV: returns column names from first row
 * For JSON: returns top-level keys (or array item keys)
 */
function extractSchema(content: Uint8Array, fileName: string): string[] | null {
  try {
    const text = new TextDecoder().decode(content);

    if (fileName.toLowerCase().endsWith(".csv")) {
      // Parse first line of CSV for column names
      const firstLine = text.split(/\r?\n/)[0];
      if (!firstLine) return null;

      // Simple CSV parsing - handle quoted fields
      const columns: string[] = [];
      let current = "";
      let inQuotes = false;

      for (const char of firstLine) {
        if (char === '"') {
          inQuotes = !inQuotes;
        } else if (char === "," && !inQuotes) {
          columns.push(current.trim().replace(/^"|"$/g, ""));
          current = "";
        } else {
          current += char;
        }
      }
      columns.push(current.trim().replace(/^"|"$/g, ""));

      return columns.length > 0 ? columns : null;
    }

    if (fileName.toLowerCase().endsWith(".json")) {
      const data = JSON.parse(text);

      if (
        Array.isArray(data) &&
        data.length > 0 &&
        typeof data[0] === "object"
      ) {
        // Array of objects - get keys from first item
        return Object.keys(data[0] as Record<string, unknown>);
      } else if (typeof data === "object" && data !== null) {
        // Single object - get top-level keys
        return Object.keys(data as Record<string, unknown>);
      }
    }

    return null;
  } catch {
    return null;
  }
}

const app = new Hono<{ Bindings: Env }>();

// Middleware: require API key
const requireApiKey: MiddlewareHandler = async (c, next) => {
  const authHeaderRaw = c.req.header("authorization");

  if (typeof authHeaderRaw !== "string") {
    return c.json({ error: "Unauthorized" }, 401);
  }

  const authHeader = authHeaderRaw as string;

  if (!authHeader.startsWith("Bearer ")) {
    return c.json({ error: "Unauthorized" }, 401);
  }

  const providedKey = authHeader.slice("Bearer ".length).trim();

  if (!providedKey || providedKey !== c.env.INTERNAL_API_KEY) {
    return c.json({ error: "Unauthorized" }, 401);
  }

  return next();
};

// Middleware: extract orgId and agentId from headers
const requireOrgAndAgent: MiddlewareHandler<{
  Bindings: Env;
  Variables: { orgId: string; agentId: string };
}> = async (c, next) => {
  const orgId = c.req.header("x-org-id");
  const agentId = c.req.header("x-agent-id");

  if (!orgId || !agentId) {
    return c.json(
      { error: "Missing required headers: x-org-id and x-agent-id" },
      400,
    );
  }

  c.set("orgId", orgId);
  c.set("agentId", agentId);

  return next();
};

app.use("*", requireApiKey);

// ============================================================================
// Helper functions
// ============================================================================

/** Build prefix for conversation data: "/orgId/agentId/userIdentifier/conversationId/" */
const buildConversationDataPrefix = (
  orgId: string,
  agentId: string,
  userIdentifier: string,
  conversationId: string,
) => `/${orgId}/${agentId}/${userIdentifier}/${conversationId}/`;

/** Build prefix for user-scoped datasets: "/orgId/agentId/userIdentifier/{value}/" */
const buildUserIdentifierPrefix = (
  orgId: string,
  agentId: string,
  userIdentifier: string,
) => `/${orgId}/${agentId}/userIdentifier/${userIdentifier}/`;

const mountBucketIfNeeded = async (
  sandbox: Awaited<ReturnType<typeof getSandbox>>,
  bucketName: string,
  mountPath: string,
  options: ReturnType<typeof BUCKET_OPTIONS>,
) => {
  // Skip mountpoint check - just try to mount directly.
  // The check adds ~2s on cold start waiting for container.
  // If already mounted, we catch the "already in use" error.
  try {
    await sandbox.mountBucket(bucketName, mountPath, options);
  } catch (error) {
    const message = error instanceof Error ? error.message : String(error);
    // If the mount path is already in use, treat it as mounted and continue.
    if (!message.includes("already in use")) {
      throw error;
    }
  }
};

/**
 * Copy files from R2 bucket to sandbox filesystem (for local dev without S3 mounting).
 * This is used when SKIP_BUCKET_MOUNT is enabled because mountBucket() requires
 * real S3 credentials that don't work in local wrangler dev.
 */
const copyBucketFilesToSandbox = async (
  sandbox: Awaited<ReturnType<typeof getSandbox>>,
  bucket: R2Bucket,
  prefix: string,
  mountPath: string,
) => {
  // Ensure mount directory exists and get list of existing files in one command
  const existingResult = await sandbox.exec(
    `mkdir -p ${mountPath} && find ${mountPath} -type f 2>/dev/null || true`,
  );
  const existingFiles = new Set(
    existingResult.stdout
      .split("\n")
      .map((p) => p.trim())
      .filter((p) => p.length > 0)
      .map((p) => p.slice(mountPath.length + 1)), // Remove mountPath prefix to get relative path
  );

  // List and copy files from R2 that don't already exist in sandbox
  let cursor: string | undefined;

  do {
    const listed = await bucket.list({ prefix, cursor, limit: 1000 });

    for (const object of listed.objects) {
      // Skip if it's a "folder" (ends with /)
      if (object.key.endsWith("/")) continue;

      // Get relative path by removing the prefix
      const relativePath = object.key.slice(prefix.length);
      if (!relativePath) continue;

      // Skip if file already exists in sandbox
      if (existingFiles.has(relativePath)) continue;

      try {
        const obj = await bucket.get(object.key);
        if (!obj) continue;

        const destPath = `${mountPath}/${relativePath}`;

        // Create parent directories if the file is nested
        const lastSlash = relativePath.lastIndexOf("/");
        if (lastSlash > 0) {
          const parentDir = `${mountPath}/${relativePath.slice(0, lastSlash)}`;
          await sandbox.exec(`mkdir -p ${parentDir}`);
        }

        const content = await obj.text();
        await sandbox.writeFile(destPath, content);
      } catch (err) {
        console.error(`[copy] Failed to copy ${object.key}:`, err);
      }
    }

    cursor = listed.truncated ? listed.cursor : undefined;
  } while (cursor);
};

/**
 * Build dataset mounts based on userIdentifier and userContext.
 * - Always includes user-scoped datasets mount
 * - Includes context-scoped mounts for contexts that exist AND match userContext
 */
const buildDatasetMounts = async (
  c: AuthedContext,
  orgId: string,
  agentId: string,
  userIdentifier: string,
  userContext?: Record<string, string | number>,
): Promise<DatasetMount[]> => {
  const mounts: DatasetMount[] = [];

  // 1. Always add user-scoped datasets mount
  mounts.push({
    type: "user",
    mountPath: DATASETS_USER_MOUNT_PATH,
    prefix: buildUserIdentifierPrefix(orgId, agentId, userIdentifier),
  });

  // 2. Add context-scoped mounts if userContext is provided
  if (userContext && typeof userContext === "object") {
    // List existing context folders from R2
    const contextPrefix = `${orgId}/${agentId}/userContext/`;
    const existingContexts: { key: string; value: string }[] = [];

    let cursor: string | undefined;
    do {
      const result = await c.env.CUSTOMER_DATASETS.list({
        prefix: contextPrefix,
        delimiter: "/",
        cursor,
        limit: 1000,
      });

      for (const delimitedPrefix of result.delimitedPrefixes ?? []) {
        const folderPath = delimitedPrefix.slice(contextPrefix.length);
        const folderName = folderPath.replace(/\/$/, "");
        const colonIndex = folderName.indexOf(":");
        if (colonIndex > 0) {
          const key = folderName.slice(0, colonIndex);
          const value = folderName.slice(colonIndex + 1);
          if (key && value) {
            existingContexts.push({ key, value });
          }
        }
      }
      cursor = result.truncated ? result.cursor : undefined;
    } while (cursor);

    // Filter to contexts that match userContext
    const matchingContexts = existingContexts.filter(
      (ctx) =>
        ctx.key in userContext && String(userContext[ctx.key]) === ctx.value,
    );

    // Add mounts for matching contexts
    for (const { key, value } of matchingContexts) {
      mounts.push({
        type: "context",
        key,
        mountPath: `${DATASETS_MOUNT_PATH}/context_${key}`,
        prefix: `/${orgId}/${agentId}/userContext/${key}:${value}/`,
      });
    }
  }

  return mounts;
};

/** Get or create sandbox with mounted buckets (or copy files in dev mode) */
const getSandboxWithContext = async (
  c: AuthedContext,
  params: SandboxParams,
) => {
  const orgId = c.get("orgId");
  const agentId = c.get("agentId");

  // Use runId for sandbox identification (message-scoped)
  // Data paths still use conversationId for persistence across runs
  const sandbox = getSandbox(c.env.Sandbox, params.runId, {
    sleepAfter: "5m",
    keepAlive: false,
  });

  // In dev mode (SKIP_BUCKET_MOUNT=true), copy files from R2 into sandbox
  // instead of mounting. This is needed because mountBucket() requires real
  // S3 credentials that don't work with wrangler's local R2 emulation.
  const skipBucketMount = c.env.SKIP_BUCKET_MOUNT === "true";

  // Build dataset mount configurations based on userIdentifier and userContext
  const datasetMounts = await buildDatasetMounts(
    c,
    orgId,
    agentId,
    params.userIdentifier,
    params.userContext,
  );

  if (skipBucketMount) {
    // Dev mode: copy files from R2 into sandbox
    const conversationPrefix = `${orgId}/${agentId}/${params.userIdentifier}/${params.conversationId}/`;

    // Copy conversation data
    await copyBucketFilesToSandbox(
      sandbox,
      c.env.CUSTOMER_CONVERSATION_DATA,
      conversationPrefix,
      CONVERSATION_DATA_MOUNT_PATH,
    );

    // Copy each dataset mount
    for (const mount of datasetMounts) {
      // For dev mode, need to build the R2 prefix without leading slash
      const r2Prefix = mount.prefix.replace(/^\//, "");
      await copyBucketFilesToSandbox(
        sandbox,
        c.env.CUSTOMER_DATASETS,
        r2Prefix,
        mount.mountPath,
      );
    }
  } else {
    // Production mode: mount buckets via S3 with prefix
    const conversationDataPrefix = buildConversationDataPrefix(
      orgId,
      agentId,
      params.userIdentifier,
      params.conversationId,
    );

    // Mount conversation data
    await mountBucketIfNeeded(
      sandbox,
      c.env.CONVERSATION_DATA_BUCKET,
      CONVERSATION_DATA_MOUNT_PATH,
      BUCKET_OPTIONS(c, conversationDataPrefix),
    );

    // Mount each dataset mount in parallel
    await Promise.all(
      datasetMounts.map((mount) =>
        mountBucketIfNeeded(
          sandbox,
          c.env.DATASETS_BUCKET,
          mount.mountPath,
          BUCKET_OPTIONS(c, mount.prefix),
        ),
      ),
    );
  }

  return sandbox;
};

// ============================================================================
// RESTful Dataset endpoints - GET/POST/DELETE /datasets
// ============================================================================

// GET /datasets - List datasets for admin UI (folder navigation)
// Uses R2 delimiter to list only one level at a time (lazy folder loading)
app.get(
  "/datasets",
  requireOrgAndAgent,
  zValidator("query", datasetsListQuerySchema, (result, c) => {
    if (!result.success) {
      const message =
        result.error.issues[0]?.message ?? "Invalid query parameters.";
      return c.json({ error: message }, 400);
    }
  }),
  async (c) => {
    const orgId = c.get("orgId");
    const agentId = c.get("agentId");
    const { path: subPath } = c.req.valid("query");

    // Build the prefix for listing
    // Base: orgId/agentId/
    // With path: orgId/agentId/subPath/
    let prefix = `${orgId}/${agentId}/`;
    if (subPath) {
      // Normalize: remove leading/trailing slashes
      const normalizedPath = subPath.replace(/^\/+|\/+$/g, "");
      if (normalizedPath) {
        prefix += `${normalizedPath}/`;
      }
    }

    try {
      const files: {
        name: string;
        targetPath: string;
        success: boolean;
        error?: string;
        schema?: string[];
        notes?: string;
      }[] = [];
      const folders: string[] = [];

      let cursor: string | undefined;

      // Use delimiter to list only one level at a time
      do {
        const result = await c.env.CUSTOMER_DATASETS.list({
          prefix,
          delimiter: "/",
          cursor,
          limit: 1000,
        });

        // Folders come from delimitedPrefixes (common prefixes ending with delimiter)
        for (const delimitedPrefix of result.delimitedPrefixes ?? []) {
          // Extract folder name from prefix (remove the base prefix and trailing slash)
          const folderPath = delimitedPrefix.slice(prefix.length);
          const folderName = folderPath.replace(/\/$/, "");
          if (folderName) {
            folders.push(folderName);
          }
        }

        // Files at this level
        for (const object of result.objects ?? []) {
          if (!isSupportedFileExtension(object.key)) continue;
          const name = object.key.split("/").pop() ?? object.key;
          files.push({
            name,
            targetPath: `${DATASETS_MOUNT_PATH}/${name}`,
            success: true,
          });
        }

        cursor = result.truncated ? result.cursor : undefined;
      } while (cursor);

      // Fetch metadata for files
      if (files.length > 0) {
        await Promise.all(
          files.map(async (file) => {
            const key = `${prefix}${file.name}`;
            try {
              const headResult = await c.env.CUSTOMER_DATASETS.head(key);
              const customMetadata = headResult?.customMetadata;

              if (customMetadata?.schema) {
                try {
                  file.schema = JSON.parse(customMetadata.schema) as string[];
                } catch {
                  // Invalid schema JSON, ignore
                }
              }
              if (customMetadata?.notes) {
                file.notes = customMetadata.notes;
              }
            } catch {
              // If head fails, continue without metadata
            }
          }),
        );
      }

      return c.json({ files, folders });
    } catch (error) {
      const message = error instanceof Error ? error.message : String(error);
      console.error("Failed to list dataset files:", error);
      return c.json(
        {
          files: [],
          folders: [],
          error: message,
        },
        500,
      );
    }
  },
);

// GET /datasets/user/:userIdentifier - List datasets for a specific user (flat list for agent)
// Returns all files under the user's folder without folder structure
app.get("/datasets/user/:userIdentifier", requireOrgAndAgent, async (c) => {
  const orgId = c.get("orgId");
  const agentId = c.get("agentId");
  const userIdentifier = c.req.param("userIdentifier");

  const userIdError = validateUserIdentifier(userIdentifier ?? "");
  if (userIdError) {
    return c.json({ error: userIdError }, 400);
  }

  // New prefix: orgId/agentId/userIdentifier/{value}/
  const prefix = `${orgId}/${agentId}/userIdentifier/${userIdentifier}/`;

  try {
    const files: {
      name: string;
      targetPath: string;
      success: boolean;
      error?: string;
      schema?: string[];
      notes?: string;
    }[] = [];

    let cursor: string | undefined;

    // List all files under user's folder (no delimiter = recursive)
    do {
      const result = await c.env.CUSTOMER_DATASETS.list({
        prefix,
        cursor,
        limit: 1000,
      });

      for (const object of result.objects ?? []) {
        if (!isSupportedFileExtension(object.key)) continue;
        const name = object.key.split("/").pop() ?? object.key;
        files.push({
          name,
          targetPath: `${DATASETS_USER_MOUNT_PATH}/${name}`,
          success: true,
        });
      }

      cursor = result.truncated ? result.cursor : undefined;
    } while (cursor);

    // Fetch metadata for files
    if (files.length > 0) {
      await Promise.all(
        files.map(async (file) => {
          const key = `${prefix}${file.name}`;
          try {
            const headResult = await c.env.CUSTOMER_DATASETS.head(key);
            const customMetadata = headResult?.customMetadata;

            if (customMetadata?.schema) {
              try {
                file.schema = JSON.parse(customMetadata.schema) as string[];
              } catch {
                // Invalid schema JSON, ignore
              }
            }
            if (customMetadata?.notes) {
              file.notes = customMetadata.notes;
            }
          } catch {
            // If head fails, continue without metadata
          }
        }),
      );
    }

    return c.json({ files });
  } catch (error) {
    const message = error instanceof Error ? error.message : String(error);
    console.error("Failed to list user dataset files:", error);
    return c.json(
      {
        files: [],
        error: message,
      },
      500,
    );
  }
});

const MAX_CONTEXT_SCOPES = 3;

// GET /datasets/contexts - List all unique context scopes for an agent
// Returns list of {key, value} pairs representing existing context folders
app.get("/datasets/contexts", requireOrgAndAgent, async (c) => {
  const orgId = c.get("orgId");
  const agentId = c.get("agentId");

  // Prefix for context folders: orgId/agentId/userContext/
  const prefix = `${orgId}/${agentId}/userContext/`;

  try {
    const contexts: { key: string; value: string }[] = [];

    let cursor: string | undefined;

    // Use delimiter to list only context folders (not files inside them)
    do {
      const result = await c.env.CUSTOMER_DATASETS.list({
        prefix,
        delimiter: "/",
        cursor,
        limit: 1000,
      });

      // Folders come from delimitedPrefixes (common prefixes ending with delimiter)
      for (const delimitedPrefix of result.delimitedPrefixes ?? []) {
        // Extract folder name from prefix: "orgId/agentId/userContext/key:value/"
        const folderPath = delimitedPrefix.slice(prefix.length);
        const folderName = folderPath.replace(/\/$/, "");

        // Parse "key:value" format
        const colonIndex = folderName.indexOf(":");
        if (colonIndex > 0) {
          const key = folderName.slice(0, colonIndex);
          const value = folderName.slice(colonIndex + 1);
          if (key && value) {
            contexts.push({ key, value });
          }
        }
      }

      cursor = result.truncated ? result.cursor : undefined;
    } while (cursor);

    return c.json({ contexts, limit: MAX_CONTEXT_SCOPES });
  } catch (error) {
    const message = error instanceof Error ? error.message : String(error);
    console.error("Failed to list context scopes:", error);
    return c.json({ contexts: [], limit: MAX_CONTEXT_SCOPES, error: message }, 500);
  }
});

// GET /datasets/context/:contextKey/:contextValue - List datasets for a context scope
// Returns all files under the context folder
app.get(
  "/datasets/context/:contextKey/:contextValue",
  requireOrgAndAgent,
  async (c) => {
    const orgId = c.get("orgId");
    const agentId = c.get("agentId");
    const contextKey = c.req.param("contextKey");
    const contextValue = c.req.param("contextValue");

    const keyError = validateContextKey(contextKey ?? "");
    if (keyError) {
      return c.json({ error: keyError }, 400);
    }
    const valueError = validateContextValue(contextValue ?? "");
    if (valueError) {
      return c.json({ error: valueError }, 400);
    }

    // New prefix: orgId/agentId/userContext/{key}:{value}/
    const prefix = `${orgId}/${agentId}/userContext/${contextKey}:${contextValue}/`;

    try {
      const files: {
        name: string;
        targetPath: string;
        success: boolean;
        error?: string;
        schema?: string[];
        notes?: string;
      }[] = [];

      let cursor: string | undefined;

      // List all files under context folder (no delimiter = recursive)
      do {
        const result = await c.env.CUSTOMER_DATASETS.list({
          prefix,
          cursor,
          limit: 1000,
        });

        for (const object of result.objects ?? []) {
          if (!isSupportedFileExtension(object.key)) continue;
          const name = object.key.split("/").pop() ?? object.key;
          files.push({
            name,
            targetPath: `${DATASETS_MOUNT_PATH}/context_${contextKey}/${name}`,
            success: true,
          });
        }

        cursor = result.truncated ? result.cursor : undefined;
      } while (cursor);

      // Fetch metadata for files
      if (files.length > 0) {
        await Promise.all(
          files.map(async (file) => {
            const key = `${prefix}${file.name}`;
            try {
              const headResult = await c.env.CUSTOMER_DATASETS.head(key);
              const customMetadata = headResult?.customMetadata;

              if (customMetadata?.schema) {
                try {
                  file.schema = JSON.parse(customMetadata.schema) as string[];
                } catch {
                  // Invalid schema JSON, ignore
                }
              }
              if (customMetadata?.notes) {
                file.notes = customMetadata.notes;
              }
            } catch {
              // If head fails, continue without metadata
            }
          }),
        );
      }

      return c.json({ files });
    } catch (error) {
      const message = error instanceof Error ? error.message : String(error);
      console.error("Failed to list context dataset files:", error);
      return c.json(
        {
          files: [],
          error: message,
        },
        500,
      );
    }
  },
);

// POST /datasets/user/:userIdentifier - Upload single dataset file for user (multipart/form-data)
app.post("/datasets/user/:userIdentifier", requireOrgAndAgent, async (c) => {
  const orgId = c.get("orgId");
  const agentId = c.get("agentId");
  const userIdentifier = c.req.param("userIdentifier");

  const userIdError = validateUserIdentifier(userIdentifier ?? "");
  if (userIdError) {
    return c.json({ error: userIdError }, 400);
  }

  let formData: FormData;
  try {
    formData = await c.req.formData();
  } catch {
    return c.json({ error: "Invalid multipart/form-data request" }, 400);
  }

  const file = formData.get("file") as File | null;
  const contentType = formData.get("contentType") as string | null;
  const notes = formData.get("notes") as string | null;

  if (!file) {
    return c.json({ error: "File is required" }, 400);
  }

  // New path: orgId/agentId/userIdentifier/{value}/
  const basePrefix = `${orgId}/${agentId}/userIdentifier/${userIdentifier}`;

  // Sanitize file name: trim and replace spaces with underscores
  const fileName = file.name.trim().replace(/ /g, "_");

  if (isUnsafeFileName(fileName)) {
    return c.json(
      {
        file: {
          name: fileName,
          path: "",
          size: 0,
          error: "File name contains invalid characters",
        },
      },
      400,
    );
  }

  if (!isSupportedFileExtension(fileName)) {
    return c.json(
      {
        file: {
          name: fileName,
          path: "",
          size: 0,
          error: "File must be .json or .csv",
        },
      },
      400,
    );
  }

  try {
    // Get raw bytes directly from file - no base64 decode needed
    const content = new Uint8Array(await file.arrayBuffer());

    if (content.length > MAX_FILE_SIZE_BYTES) {
      return c.json(
        {
          file: {
            name: fileName,
            path: "",
            size: content.length,
            error: `File exceeds ${MAX_FILE_SIZE_BYTES / 1024 / 1024}MB limit`,
          },
        },
        400,
      );
    }

    const key = `${basePrefix}/${fileName}`;

    // Extract schema from file content
    const schema = extractSchema(content, fileName);

    // Build custom metadata
    const customMetadata: Record<string, string> = {};
    if (schema) {
      customMetadata.schema = JSON.stringify(schema);
    }
    if (notes) {
      customMetadata.notes = notes;
    }

    // Use contentType from form field, or fall back to file.type
    const finalContentType =
      contentType || file.type || "application/octet-stream";

    await c.env.CUSTOMER_DATASETS.put(key, content, {
      httpMetadata: { contentType: finalContentType },
      customMetadata:
        Object.keys(customMetadata).length > 0 ? customMetadata : undefined,
    });

    return c.json({
      file: {
        name: fileName,
        path: key,
        size: content.length,
      },
    });
  } catch (error) {
    const message = error instanceof Error ? error.message : String(error);
    return c.json(
      {
        file: {
          name: fileName,
          path: "",
          size: 0,
          error: message,
        },
      },
      500,
    );
  }
});

// POST /datasets/context/:contextKey/:contextValue - Upload single dataset file for context (multipart/form-data)
app.post(
  "/datasets/context/:contextKey/:contextValue",
  requireOrgAndAgent,
  async (c) => {
    const orgId = c.get("orgId");
    const agentId = c.get("agentId");
    const contextKey = c.req.param("contextKey");
    const contextValue = c.req.param("contextValue");

    const keyError = validateContextKey(contextKey ?? "");
    if (keyError) {
      return c.json({ error: keyError }, 400);
    }
    const valueError = validateContextValue(contextValue ?? "");
    if (valueError) {
      return c.json({ error: valueError }, 400);
    }

    // Check if we're at the context scope limit
    const contextPrefix = `${orgId}/${agentId}/userContext/`;
    const existingContexts: string[] = [];

    let cursor: string | undefined;
    do {
      const result = await c.env.CUSTOMER_DATASETS.list({
        prefix: contextPrefix,
        delimiter: "/",
        cursor,
        limit: 1000,
      });

      for (const delimitedPrefix of result.delimitedPrefixes ?? []) {
        const folderPath = delimitedPrefix.slice(contextPrefix.length);
        const folderName = folderPath.replace(/\/$/, "");
        if (folderName) {
          existingContexts.push(folderName);
        }
      }
      cursor = result.truncated ? result.cursor : undefined;
    } while (cursor);

    // Check if this context already exists or if we're under the limit
    const requestedContext = `${contextKey}:${contextValue}`;
    const contextExists = existingContexts.includes(requestedContext);

    if (!contextExists && existingContexts.length >= MAX_CONTEXT_SCOPES) {
      return c.json(
        {
          error: `Maximum context scopes limit reached (${MAX_CONTEXT_SCOPES}). Cannot create new context scope '${contextKey}:${contextValue}'. Existing scopes: ${existingContexts.join(", ")}`,
        },
        400,
      );
    }

    let formData: FormData;
    try {
      formData = await c.req.formData();
    } catch {
      return c.json({ error: "Invalid multipart/form-data request" }, 400);
    }

    const file = formData.get("file") as File | null;
    const contentType = formData.get("contentType") as string | null;
    const notes = formData.get("notes") as string | null;

    if (!file) {
      return c.json({ error: "File is required" }, 400);
    }

    // New path: orgId/agentId/userContext/{key}:{value}/
    const basePrefix = `${orgId}/${agentId}/userContext/${contextKey}:${contextValue}`;

    // Sanitize file name: trim and replace spaces with underscores
    const fileName = file.name.trim().replace(/ /g, "_");

    if (isUnsafeFileName(fileName)) {
      return c.json(
        {
          file: {
            name: fileName,
            path: "",
            size: 0,
            error: "File name contains invalid characters",
          },
        },
        400,
      );
    }

    if (!isSupportedFileExtension(fileName)) {
      return c.json(
        {
          file: {
            name: fileName,
            path: "",
            size: 0,
            error: "File must be .json or .csv",
          },
        },
        400,
      );
    }

    try {
      // Get raw bytes directly from file - no base64 decode needed
      const content = new Uint8Array(await file.arrayBuffer());

      if (content.length > MAX_FILE_SIZE_BYTES) {
        return c.json(
          {
            file: {
              name: fileName,
              path: "",
              size: content.length,
              error: `File exceeds ${MAX_FILE_SIZE_BYTES / 1024 / 1024}MB limit`,
            },
          },
          400,
        );
      }

      const key = `${basePrefix}/${fileName}`;

      // Extract schema from file content
      const schema = extractSchema(content, fileName);

      // Build custom metadata
      const customMetadata: Record<string, string> = {};
      if (schema) {
        customMetadata.schema = JSON.stringify(schema);
      }
      if (notes) {
        customMetadata.notes = notes;
      }

      // Use contentType from form field, or fall back to file.type
      const finalContentType =
        contentType || file.type || "application/octet-stream";

      await c.env.CUSTOMER_DATASETS.put(key, content, {
        httpMetadata: { contentType: finalContentType },
        customMetadata:
          Object.keys(customMetadata).length > 0 ? customMetadata : undefined,
      });

      return c.json({
        file: {
          name: fileName,
          path: key,
          size: content.length,
        },
      });
    } catch (error) {
      const message = error instanceof Error ? error.message : String(error);
      return c.json(
        {
          file: {
            name: fileName,
            path: "",
            size: 0,
            error: message,
          },
        },
        500,
      );
    }
  },
);

// DELETE /datasets/user/:userIdentifier/:filename - Delete single user-scoped dataset file
app.delete(
  "/datasets/user/:userIdentifier/:filename",
  requireOrgAndAgent,
  async (c) => {
    const orgId = c.get("orgId");
    const agentId = c.get("agentId");
    const userIdentifier = c.req.param("userIdentifier");
    const filename = c.req.param("filename");

    const userIdError = validateUserIdentifier(userIdentifier ?? "");
    if (userIdError) {
      return c.json({ error: userIdError }, 400);
    }

    const decodedFilename = decodeURIComponent(filename);
    const trimmedName = decodedFilename.trim();

    if (isUnsafeFileName(trimmedName)) {
      return c.json(
        { file: decodedFilename, success: false, error: "Invalid filename" },
        400,
      );
    }

    // New path: orgId/agentId/userIdentifier/{value}/
    const key = `${orgId}/${agentId}/userIdentifier/${userIdentifier}/${trimmedName}`;

    try {
      await c.env.CUSTOMER_DATASETS.delete(key);
      return c.json({ file: decodedFilename, success: true });
    } catch (error) {
      const message = error instanceof Error ? error.message : String(error);
      return c.json(
        { file: decodedFilename, success: false, error: message },
        500,
      );
    }
  },
);

// DELETE /datasets/context/:contextKey/:contextValue/:filename - Delete single context-scoped dataset file
app.delete(
  "/datasets/context/:contextKey/:contextValue/:filename",
  requireOrgAndAgent,
  async (c) => {
    const orgId = c.get("orgId");
    const agentId = c.get("agentId");
    const contextKey = c.req.param("contextKey");
    const contextValue = c.req.param("contextValue");
    const filename = c.req.param("filename");

    const keyError = validateContextKey(contextKey ?? "");
    if (keyError) {
      return c.json({ error: keyError }, 400);
    }
    const valueError = validateContextValue(contextValue ?? "");
    if (valueError) {
      return c.json({ error: valueError }, 400);
    }

    const decodedFilename = decodeURIComponent(filename);
    const trimmedName = decodedFilename.trim();

    if (isUnsafeFileName(trimmedName)) {
      return c.json(
        { file: decodedFilename, success: false, error: "Invalid filename" },
        400,
      );
    }

    // New path: orgId/agentId/userContext/{key}:{value}/
    const key = `${orgId}/${agentId}/userContext/${contextKey}:${contextValue}/${trimmedName}`;

    try {
      await c.env.CUSTOMER_DATASETS.delete(key);
      return c.json({ file: decodedFilename, success: true });
    } catch (error) {
      const message = error instanceof Error ? error.message : String(error);
      return c.json(
        { file: decodedFilename, success: false, error: message },
        500,
      );
    }
  },
);

// DELETE /datasets - Delete by full path (Admin API)
// Uses ?path=... query param for full path relative to orgId/agentId
app.delete(
  "/datasets",
  requireOrgAndAgent,
  zValidator("query", datasetsDeleteByPathQuerySchema, (result, c) => {
    if (!result.success) {
      const message = result.error.issues[0]?.message ?? "Path is required.";
      return c.json({ error: message }, 400);
    }
  }),
  async (c) => {
    const orgId = c.get("orgId");
    const agentId = c.get("agentId");
    const { path: fullPath } = c.req.valid("query");

    const normalizedPath = fullPath.replace(/^\/+|\/+$/g, "");
    if (normalizedPath.includes("..")) {
      return c.json(
        { file: fullPath, success: false, error: "Invalid path" },
        400,
      );
    }

    const key = `${orgId}/${agentId}/${normalizedPath}`;

    try {
      await c.env.CUSTOMER_DATASETS.delete(key);
      return c.json({ file: fullPath, success: true });
    } catch (error) {
      const message = error instanceof Error ? error.message : String(error);
      return c.json({ file: fullPath, success: false, error: message }, 500);
    }
  },
);

// ============================================================================
// RESTful Conversation Data endpoints - POST /conversation-data
// ============================================================================

app.post(
  "/conversation-data",
  requireOrgAndAgent,
  zValidator("json", conversationDataUploadBodySchema, (result, c) => {
    if (!result.success) {
      const message =
        result.error.issues[0]?.message ?? "Invalid upload request.";
      return c.json({ error: message }, 400);
    }
  }),
  async (c) => {
    const orgId = c.get("orgId");
    const agentId = c.get("agentId");
    const { conversationId, userIdentifier, files } = c.req.valid("json");
    const pathPrefix = `${orgId}/${agentId}/${userIdentifier}/${conversationId}`;

    const storedFiles = await Promise.all(
      files.map(async (file) => {
        const fileName = file.name.trim();

        if (isUnsafeFileName(fileName)) {
          return {
            name: fileName,
            path: "",
            size: 0,
            error: "File name contains invalid characters",
          };
        }

        if (!isSupportedFileExtension(fileName)) {
          return {
            name: fileName,
            path: "",
            size: 0,
            error: "File must be .json or .csv",
          };
        }

        try {
          const content = Uint8Array.from(atob(file.content), (c) =>
            c.charCodeAt(0),
          );

          if (content.length > MAX_FILE_SIZE_BYTES) {
            return {
              name: fileName,
              path: "",
              size: content.length,
              error: `File exceeds ${MAX_FILE_SIZE_BYTES / 1024 / 1024}MB limit`,
            };
          }

          const key = `${pathPrefix}/${fileName}`;
          await c.env.CUSTOMER_CONVERSATION_DATA.put(key, content, {
            httpMetadata: { contentType: file.contentType },
          });
          return {
            name: fileName,
            path: key,
            size: content.length,
          };
        } catch (error) {
          const message =
            error instanceof Error ? error.message : String(error);
          return {
            name: fileName,
            path: "",
            size: 0,
            error: message,
          };
        }
      }),
    );

    return c.json({ files: storedFiles });
  },
);

// DELETE /conversation-data - Delete all conversation data for agent
app.delete("/conversation-data", requireOrgAndAgent, async (c) => {
  const orgId = c.get("orgId");
  const agentId = c.get("agentId");
  const prefix = `${orgId}/${agentId}/`;

  try {
    const keysToDelete: string[] = [];
    let cursor: string | undefined;

    // Collect all object keys under the org/agent prefix
    do {
      const result = await c.env.CUSTOMER_CONVERSATION_DATA.list({
        prefix,
        cursor,
        limit: 1000,
      });

      for (const object of result.objects ?? []) {
        keysToDelete.push(object.key);
      }

      cursor = result.truncated ? result.cursor : undefined;
    } while (cursor);

    if (keysToDelete.length === 0) {
      return c.json({ deleted: 0, message: "No conversation data found" });
    }

    // Delete in batches (R2 delete supports up to 1000 keys per call)
    const batchSize = 1000;
    for (let i = 0; i < keysToDelete.length; i += batchSize) {
      const batch = keysToDelete.slice(i, i + batchSize);
      await c.env.CUSTOMER_CONVERSATION_DATA.delete(batch);
    }

    return c.json({
      deleted: keysToDelete.length,
      message: `Deleted ${keysToDelete.length} files`,
    });
  } catch (error) {
    const message = error instanceof Error ? error.message : String(error);
    console.error("Failed to delete conversation data:", error);
    return c.json({ error: message }, 500);
  }
});

// ============================================================================
// Sandbox endpoints - /sandbox/*
// ============================================================================

app.post(
  "/sandbox",
  requireOrgAndAgent,
  zValidator("json", sandboxParamsSchema, (result, c) => {
    if (!result.success) {
      const message =
        result.error.issues[0]?.message ?? "Invalid request body.";
      return c.json({ error: message }, 400);
    }
  }),
  async (c) => {
    const params = c.req.valid("json");
    await getSandboxWithContext(c, params);
    return c.json({ success: true });
  },
);

// POST /sandbox/execute - Execute Python code
app.post(
  "/sandbox/execute",
  requireOrgAndAgent,
  zValidator("json", executeBodySchema, (result, c) => {
    if (!result.success) {
      const message =
        result.error.issues[0]?.message ?? "Invalid execute request.";
      return c.json({ error: message }, 400);
    }
  }),
  async (c) => {
    const { code, ...sandboxParams } = c.req.valid("json");

    const sandbox = await getSandboxWithContext(c, sandboxParams);

    // inconvo.py is pre-installed in container via Dockerfile at /workspace/
    await sandbox.writeFile("/workspace/analyze.py", code);

    const result = await sandbox.exec("cd /workspace && python3 analyze.py");

    return c.json({
      output: result.stdout,
      error: result.stderr,
      exitCode: result.exitCode,
      success: result.success,
    });
  },
);

app.delete("/sandbox", requireOrgAndAgent, async (c) => {
  const body = await c.req.json();
  const result = sandboxParamsSchema.safeParse(body);
  if (!result.success) {
    const message = result.error.issues[0]?.message ?? "Invalid request body.";
    throw new HTTPException(400, { message });
  }
  const params = result.data;

  const sandbox = await getSandboxWithContext(c, params);
  try {
    await sandbox.destroy();
    return c.json({ success: true });
  } catch (error) {
    console.error("Failed to destroy sandbox:", error);
    const message = error instanceof Error ? error.message : "Unknown error";
    return c.json({ error: message }, 500);
  }
});

app.notFound((c) => c.json({ message: "Not Found!" }, 404));

app.onError((err, c) => {
  console.error("Unhandled error:", err);
  if (err instanceof HTTPException) {
    return err.getResponse();
  }
  const message = err instanceof Error ? err.message : "Unknown error";
  return c.json({ error: message }, 500);
});

export default app;
