/**
 * Inconvo Sandbox SDK - Unified client for all sandbox operations.
 *
 * This SDK provides a clean interface for:
 * - Dataset operations (list, upload, delete) - both user-scoped and context-scoped
 * - Sandbox operations (start, execute, destroy)
 * - Conversation data operations (upload)
 *
 * All operations use X-Org-Id and X-Agent-Id headers for authentication.
 * The server constructs bucket paths from these headers + request params.
 *
 * Bucket structure:
 * - User-scoped: /{orgId}/{agentId}/userIdentifier/{userIdentifier}/
 * - Context-scoped: /{orgId}/{agentId}/userContext/{contextKey}:{contextValue}/
 *
 * Usage:
 * ```ts
 * import { SandboxClient } from "./sandbox";
 *
 * // Create client with org/agent context
 * const client = new SandboxClient({
 *   baseUrl: "https://sandbox.example.com",
 *   apiKey: "secret",
 *   orgId: "org-123",
 *   agentId: "agent-456",
 * });
 *
 * // Fetch available datasets for agent context
 * const { datasets } = await client.datasets.fetchAvailable({
 *   userIdentifier: "user_123",
 *   userContext: { orgId: "org_456" },
 * });
 *
 * // List datasets (admin - folder navigation)
 * const allDatasets = await client.datasets.list();
 *
 * // Upload a user-scoped dataset file
 * await client.datasets.uploadForUser({
 *   userIdentifier: "user_123",
 *   file: { name: "data.csv", content: Buffer.from(fileContent) },
 * });
 *
 * // Upload a context-scoped dataset file
 * await client.datasets.uploadForContext({
 *   contextKey: "orgId",
 *   contextValue: "org_456",
 *   file: { name: "shared_data.csv", content: Buffer.from(fileContent) },
 * });
 *
 * // Start sandbox (mounting is handled internally based on userIdentifier/userContext)
 * const sandbox = client.sandbox({
 *   conversationId: "conv-123",
 *   runId: "run-456",
 *   userIdentifier: "user_123",
 *   userContext: { orgId: "org_456" },
 * });
 *
 * await sandbox.start();
 * const result = await sandbox.executeCode("print('hello')");
 * await sandbox.destroy();
 * ```
 */

// ============================================================================
// Types
// ============================================================================

export interface SandboxClientOptions {
  baseUrl: string;
  apiKey: string;
  orgId: string;
  agentId: string;
  timeoutMs?: number;
}

export interface DatasetFile {
  name: string;
  targetPath: string;
  success: boolean;
  error?: string;
  schema?: string[];
  notes?: string;
}

export interface ListDatasetsResponse {
  files: DatasetFile[];
  folders: string[];
}

export interface UploadDatasetFile {
  name: string;
  content: Buffer | Uint8Array; // raw bytes
  contentType?: string;
  notes?: string;
}

export interface UploadDatasetsParams {
  userIdentifier: string;
  file: UploadDatasetFile;
}

export interface UploadedFile {
  name: string;
  path: string;
  size: number;
  error?: string;
}

export interface UploadDatasetsResponse {
  file: UploadedFile;
}

export interface DeleteDatasetsParams {
  userIdentifier: string;
  file: string;
}

export interface ContextScope {
  key: string;
  value: string;
}

export interface ListContextsResponse {
  contexts: ContextScope[];
  limit: number;
  error?: string;
}

export interface DeleteDatasetByPathParams {
  path: string; // Full path relative to orgId/agentId (e.g., "userId:123/data.csv")
}

export interface DeleteDatasetsResponse {
  file: string;
  success: boolean;
  error?: string;
}

// Context-scoped dataset params
export interface ContextDatasetParams {
  contextKey: string;
  contextValue: string;
}

export interface UploadContextDatasetParams extends ContextDatasetParams {
  file: UploadDatasetFile;
}

export interface DeleteContextDatasetParams extends ContextDatasetParams {
  file: string;
}

export interface SandboxParams {
  conversationId: string;
  /** Unique identifier for this run/message. Used to scope the sandbox instance. */
  runId: string;
  /** User identifier for scoping datasets and conversation data. */
  userIdentifier: string;
  /** Optional user context for context-scoped dataset mounting. */
  userContext?: Record<string, string | number | boolean>;
}

export interface ConversationDataUploadParams {
  conversationId: string;
  userIdentifier: string;
  files: {
    name: string;
    content: string; // base64 encoded
    contentType?: string;
  }[];
}

export interface ExecuteResponse {
  output: string;
  error: string;
  exitCode: number;
  success: boolean;
}

// Available dataset info returned to consumers (hides internal storage details)
export interface AvailableDataset {
  name: string;
  targetPath: string;
  schema?: string[];
  notes?: string;
}

export interface FetchAvailableDatasetsParams {
  userIdentifier: string;
  userContext?: Record<string, string | number | boolean> | null;
}

export interface FetchAvailableDatasetsResponse {
  datasets: AvailableDataset[];
}

export class SandboxSDKError extends Error {
  readonly status: number;
  readonly details?: unknown;

  constructor(message: string, status: number, details?: unknown) {
    super(message);
    this.name = "SandboxSDKError";
    this.status = status;
    this.details = details;
  }
}

// ============================================================================
// Dataset Client (for org-level dataset operations)
// ============================================================================

const DATASETS_MOUNT_PATH = "/datasets";

class DatasetClient {
  constructor(
    private readonly baseUrl: string,
    private readonly headers: Headers,
    private readonly orgId: string,
    private readonly agentId: string,
    private readonly timeoutMs?: number,
  ) {}

  /**
   * List datasets for admin UI (folder navigation).
   * @param options.path - Subfolder path to navigate (e.g., "user_123/folder1")
   */
  async list(options?: { path?: string }): Promise<ListDatasetsResponse> {
    const url = new URL(`${this.baseUrl}/datasets`);
    if (options?.path) {
      url.searchParams.set("path", options.path);
    }

    return this.request<ListDatasetsResponse>(url.toString(), {
      method: "GET",
    });
  }

  /**
   * List datasets for a specific user (flat list for agent).
   * @param options.userIdentifier - User identifier (required)
   */
  async listForUser(options: {
    userIdentifier: string;
  }): Promise<ListDatasetsResponse> {
    const url = `${this.baseUrl}/datasets/user/${encodeURIComponent(options.userIdentifier)}`;

    return this.request<ListDatasetsResponse>(url, {
      method: "GET",
    });
  }

  /**
   * List datasets for a specific context (flat list for agent).
   * @param options.contextKey - Context key (e.g., "orgId")
   * @param options.contextValue - Context value (e.g., "org_123")
   */
  async listForContext(options: {
    contextKey: string;
    contextValue: string;
  }): Promise<ListDatasetsResponse> {
    const url = `${this.baseUrl}/datasets/context/${encodeURIComponent(options.contextKey)}/${encodeURIComponent(options.contextValue)}`;

    return this.request<ListDatasetsResponse>(url, {
      method: "GET",
    });
  }

  /**
   * List all unique context scopes for this agent.
   * Returns the list of {key, value} pairs representing existing context folders,
   * and the maximum limit allowed.
   */
  async listContexts(): Promise<ListContextsResponse> {
    return this.request<ListContextsResponse>(`${this.baseUrl}/datasets/contexts`, {
      method: "GET",
    });
  }

  /**
   * Fetch all available datasets for injection into agent context.
   *
   * Handles both user-scoped and context-scoped datasets:
   * - User-scoped: datasets uploaded for this specific userIdentifier
   * - Context-scoped: datasets uploaded for context values matching the user's userContext
   *
   * Returns dataset metadata including file paths where the sandbox will mount them.
   * The actual mounting is handled by the sandbox based on userIdentifier and userContext.
   */
  async fetchAvailable(
    params: FetchAvailableDatasetsParams,
  ): Promise<FetchAvailableDatasetsResponse> {
    const { userIdentifier, userContext } = params;
    const allDatasets: AvailableDataset[] = [];

    try {
      // 1. Fetch user-scoped datasets
      const userResult = await this.listForUser({ userIdentifier });
      const userFiles = userResult.files
        .filter((f) => f.success && f.targetPath)
        .map((f) => ({
          name: f.name,
          targetPath: f.targetPath,
          schema: f.schema,
          notes: f.notes,
        }));

      allDatasets.push(...userFiles);

      // 2. Fetch context-scoped datasets based on existing contexts
      if (userContext && typeof userContext === "object") {
        // Get all existing context scopes for this agent
        const { contexts: existingContexts } = await this.listContexts();

        // Filter to contexts that match the user's userContext
        const matchingContexts = existingContexts.filter(
          (ctx) =>
            ctx.key in userContext &&
            String(userContext[ctx.key]) === ctx.value,
        );

        await Promise.all(
          matchingContexts.map(async ({ key, value }) => {
            try {
              const contextResult = await this.listForContext({
                contextKey: key,
                contextValue: value,
              });

              const contextFiles = contextResult.files
                .filter((f) => f.success && f.targetPath)
                .map((f) => ({
                  name: f.name,
                  // Context datasets are mounted at /datasets/context_{key}/
                  targetPath: `${DATASETS_MOUNT_PATH}/context_${key}/${f.name}`,
                  schema: f.schema,
                  notes: f.notes,
                }));

              allDatasets.push(...contextFiles);
            } catch (error) {
              console.error(
                `Error fetching context datasets for ${key}=${value}:`,
                error,
              );
            }
          }),
        );
      }
    } catch (error) {
      console.error("Error fetching datasets:", error);
    }

    return { datasets: allDatasets };
  }

  /**
   * Upload dataset file for a user using multipart/form-data.
   */
  async uploadForUser(
    params: UploadDatasetsParams,
  ): Promise<UploadDatasetsResponse> {
    const formData = new FormData();

    // Convert Buffer/Uint8Array to a new Uint8Array with its own ArrayBuffer
    // This ensures proper BlobPart compatibility (Buffer's underlying buffer is ArrayBufferLike)
    const bytes = Uint8Array.from(params.file.content);

    // Create a Blob from the raw bytes
    const blob = new Blob([bytes], {
      type: params.file.contentType || "application/octet-stream",
    });
    formData.append("file", blob, params.file.name);

    if (params.file.contentType) {
      formData.append("contentType", params.file.contentType);
    }
    if (params.file.notes) {
      formData.append("notes", params.file.notes);
    }

    return this.request<UploadDatasetsResponse>(
      `${this.baseUrl}/datasets/user/${encodeURIComponent(params.userIdentifier)}`,
      {
        method: "POST",
        // Don't set Content-Type header - let fetch set multipart boundary automatically
        body: formData,
      },
    );
  }

  /**
   * Upload dataset file for a context using multipart/form-data.
   */
  async uploadForContext(
    params: UploadContextDatasetParams,
  ): Promise<UploadDatasetsResponse> {
    const formData = new FormData();

    // Convert Buffer/Uint8Array to a new Uint8Array with its own ArrayBuffer
    const bytes = Uint8Array.from(params.file.content);

    // Create a Blob from the raw bytes
    const blob = new Blob([bytes], {
      type: params.file.contentType || "application/octet-stream",
    });
    formData.append("file", blob, params.file.name);

    if (params.file.contentType) {
      formData.append("contentType", params.file.contentType);
    }
    if (params.file.notes) {
      formData.append("notes", params.file.notes);
    }

    return this.request<UploadDatasetsResponse>(
      `${this.baseUrl}/datasets/context/${encodeURIComponent(params.contextKey)}/${encodeURIComponent(params.contextValue)}`,
      {
        method: "POST",
        body: formData,
      },
    );
  }

  /**
   * @deprecated Use uploadForUser instead
   */
  async upload(params: UploadDatasetsParams): Promise<UploadDatasetsResponse> {
    return this.uploadForUser(params);
  }

  /**
   * Delete a single user-scoped dataset file.
   */
  async deleteForUser(
    params: DeleteDatasetsParams,
  ): Promise<DeleteDatasetsResponse> {
    const url = `${this.baseUrl}/datasets/user/${encodeURIComponent(params.userIdentifier)}/${encodeURIComponent(params.file)}`;

    return this.request<DeleteDatasetsResponse>(url, {
      method: "DELETE",
    });
  }

  /**
   * Delete a single context-scoped dataset file.
   */
  async deleteForContext(
    params: DeleteContextDatasetParams,
  ): Promise<DeleteDatasetsResponse> {
    const url = `${this.baseUrl}/datasets/context/${encodeURIComponent(params.contextKey)}/${encodeURIComponent(params.contextValue)}/${encodeURIComponent(params.file)}`;

    return this.request<DeleteDatasetsResponse>(url, {
      method: "DELETE",
    });
  }

  /**
   * @deprecated Use deleteForUser instead
   */
  async delete(params: DeleteDatasetsParams): Promise<DeleteDatasetsResponse> {
    return this.deleteForUser(params);
  }

  /**
   * Delete a dataset file by full path (admin API - no userIdentifier validation).
   * Path is relative to orgId/agentId (e.g., "user_123/data.csv").
   */
  async deleteByPath(
    params: DeleteDatasetByPathParams,
  ): Promise<DeleteDatasetsResponse> {
    const url = new URL(`${this.baseUrl}/datasets`);
    url.searchParams.set("path", params.path);

    return this.request<DeleteDatasetsResponse>(url.toString(), {
      method: "DELETE",
    });
  }

  private async request<T>(url: string, init: RequestInit): Promise<T> {
    const controller =
      typeof this.timeoutMs === "number" ? new AbortController() : undefined;

    if (controller && this.timeoutMs && this.timeoutMs > 0) {
      setTimeout(() => controller.abort(), this.timeoutMs).unref?.();
    }

    const headers = new Headers(this.headers);
    // Merge custom headers, but skip Content-Type for FormData
    // (FormData needs fetch to set the Content-Type with correct boundary)
    if (init.headers) {
      const initHeaders = new Headers(init.headers);
      initHeaders.forEach((value, key) => {
        if (
          !(
            init.body instanceof FormData &&
            key.toLowerCase() === "content-type"
          )
        ) {
          headers.set(key, value);
        }
      });
    }

    const response = await fetch(url, {
      ...init,
      headers,
      signal: controller?.signal,
    }).catch((error: unknown) => {
      throw new SandboxSDKError(
        `Failed to reach sandbox worker: ${
          error instanceof Error ? error.message : String(error)
        }`,
        0,
        error,
      );
    });

    if (!response.ok) {
      let details: unknown;
      try {
        details = await response.clone().json();
      } catch {
        details = await response.text();
      }

      throw new SandboxSDKError(
        `Sandbox worker responded with ${response.status}`,
        response.status,
        details,
      );
    }

    return (await response.json()) as T;
  }
}

// ============================================================================
// Sandbox Session (for sandbox-specific operations)
// ============================================================================

class SandboxSession {
  constructor(
    private readonly baseUrl: string,
    private readonly headers: Headers,
    private readonly params: SandboxParams,
    private readonly timeoutMs?: number,
  ) {}

  /**
   * Starts/initializes the sandbox and mounts buckets.
   */
  async start(): Promise<{ success: boolean }> {
    return this.request<{ success: boolean }>(`${this.baseUrl}/sandbox`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(this.params),
    });
  }

  /**
   * Executes arbitrary Python code in the sandbox.
   */
  async executeCode(code: string): Promise<ExecuteResponse> {
    if (!code.trim()) {
      throw new Error("executeCode requires non-empty code.");
    }

    return this.request<ExecuteResponse>(`${this.baseUrl}/sandbox/execute`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ ...this.params, code }),
    });
  }

  /**
   * Destroys the sandbox.
   */
  async destroy(): Promise<void> {
    await this.request(
      `${this.baseUrl}/sandbox`,
      {
        method: "DELETE",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify(this.params),
      },
      { expectJson: false },
    );
  }

  private async request<T = unknown>(
    url: string,
    init: RequestInit,
    options: { expectJson?: boolean } = { expectJson: true },
  ): Promise<T> {
    const controller =
      typeof this.timeoutMs === "number" ? new AbortController() : undefined;

    if (controller && this.timeoutMs && this.timeoutMs > 0) {
      setTimeout(() => controller.abort(), this.timeoutMs).unref?.();
    }

    const headers = new Headers(this.headers);
    if (init.headers) {
      const initHeaders = new Headers(init.headers);
      initHeaders.forEach((value, key) => headers.set(key, value));
    }

    const response = await fetch(url, {
      ...init,
      headers,
      signal: controller?.signal,
    }).catch((error: unknown) => {
      throw new SandboxSDKError(
        `Failed to reach sandbox worker: ${
          error instanceof Error ? error.message : String(error)
        }`,
        0,
        error,
      );
    });

    if (!response.ok) {
      let details: unknown;
      if (options.expectJson !== false) {
        try {
          details = await response.clone().json();
        } catch {
          details = await response.text();
        }
      } else {
        details = await response.text();
      }

      throw new SandboxSDKError(
        `Sandbox worker responded with ${response.status}`,
        response.status,
        details,
      );
    }

    if (options.expectJson === false || response.status === 204) {
      // @ts-expect-error - caller indicated no JSON body is expected.
      return undefined;
    }

    return (await response.json()) as T;
  }
}

// ============================================================================
// Main Client
// ============================================================================

export class SandboxClient {
  private readonly baseUrl: string;
  private readonly headers: Headers;
  private readonly timeoutMs?: number;

  /**
   * Dataset operations (list, upload, delete).
   * Uses X-Org-Id and X-Agent-Id headers for authentication.
   */
  readonly datasets: DatasetClient;

  constructor(options: SandboxClientOptions) {
    if (!options.baseUrl) {
      throw new Error("SandboxClient requires a baseUrl.");
    }
    if (!options.apiKey) {
      throw new Error("SandboxClient requires an apiKey.");
    }
    if (!options.orgId) {
      throw new Error("SandboxClient requires an orgId.");
    }
    if (!options.agentId) {
      throw new Error("SandboxClient requires an agentId.");
    }

    this.baseUrl = options.baseUrl.replace(/\/+$/, "");
    this.timeoutMs = options.timeoutMs;

    // Build headers with auth
    this.headers = new Headers({
      authorization: `Bearer ${options.apiKey}`,
      "x-org-id": options.orgId,
      "x-agent-id": options.agentId,
    });

    // Initialize dataset client
    this.datasets = new DatasetClient(
      this.baseUrl,
      this.headers,
      options.orgId,
      options.agentId,
      this.timeoutMs,
    );
  }

  /**
   * Create a sandbox session for a specific conversation.
   * Returns a SandboxSession that can start, execute code, and destroy.
   *
   * @param params.conversationId - The conversation ID (required)
   * @param params.runId - Unique run/message ID (required, scopes the sandbox instance)
   * @param params.userIdentifier - User context for mounting datasets (optional)
   */
  sandbox(params: SandboxParams): SandboxSession {
    if (!params.conversationId) {
      throw new Error("sandbox() requires a conversationId.");
    }
    if (!params.runId) {
      throw new Error("sandbox() requires a runId.");
    }

    return new SandboxSession(
      this.baseUrl,
      this.headers,
      params,
      this.timeoutMs,
    );
  }

  /**
   * Upload files to conversation data bucket.
   * This is a standalone operation not tied to a sandbox session.
   */
  async uploadConversationData(
    params: ConversationDataUploadParams,
  ): Promise<UploadDatasetsResponse> {
    if (params.files.length === 0) {
      throw new Error("uploadConversationData requires at least one file.");
    }

    return this.request<UploadDatasetsResponse>(
      `${this.baseUrl}/conversation-data`,
      {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify(params),
      },
    );
  }

  private async request<T>(url: string, init: RequestInit): Promise<T> {
    const controller =
      typeof this.timeoutMs === "number" ? new AbortController() : undefined;

    if (controller && this.timeoutMs && this.timeoutMs > 0) {
      setTimeout(() => controller.abort(), this.timeoutMs).unref?.();
    }

    const headers = new Headers(this.headers);
    if (init.headers) {
      const initHeaders = new Headers(init.headers);
      initHeaders.forEach((value, key) => headers.set(key, value));
    }

    const response = await fetch(url, {
      ...init,
      headers,
      signal: controller?.signal,
    }).catch((error: unknown) => {
      throw new SandboxSDKError(
        `Failed to reach sandbox worker: ${
          error instanceof Error ? error.message : String(error)
        }`,
        0,
        error,
      );
    });

    if (!response.ok) {
      let details: unknown;
      try {
        details = await response.clone().json();
      } catch {
        details = await response.text();
      }

      throw new SandboxSDKError(
        `Sandbox worker responded with ${response.status}`,
        response.status,
        details,
      );
    }

    return (await response.json()) as T;
  }
}

// ============================================================================
// Factory functions
// ============================================================================

/**
 * Create a SandboxClient using environment variables for baseUrl and apiKey.
 * Useful for server-side code where env vars are available.
 */
export function createSandboxClientFromEnv(options: {
  orgId: string;
  agentId: string;
  timeoutMs?: number;
}): SandboxClient {
  const baseUrl = process.env.INCONVO_SANDBOX_BASE_URL;
  const apiKey = process.env.INCONVO_SANDBOX_API_KEY;

  if (!baseUrl) {
    throw new Error(
      "INCONVO_SANDBOX_BASE_URL environment variable is required.",
    );
  }
  if (!apiKey) {
    throw new Error(
      "INCONVO_SANDBOX_API_KEY environment variable is required.",
    );
  }

  return new SandboxClient({
    baseUrl,
    apiKey,
    orgId: options.orgId,
    agentId: options.agentId,
    timeoutMs: options.timeoutMs,
  });
}

// Types are already exported via `export interface` declarations above
