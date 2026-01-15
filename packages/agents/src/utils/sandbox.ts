/**
 * Inconvo Sandbox SDK - Unified client for all sandbox operations.
 *
 * This SDK provides a clean interface for:
 * - Dataset operations (list, upload, delete)
 * - Sandbox operations (start, execute, destroy)
 * - Conversation data operations (upload)
 *
 * All operations use X-Org-Id and X-Agent-Id headers for authentication.
 * The server constructs bucket paths from these headers + request params.
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
 * // List datasets
 * const datasets = await client.datasets.list();
 * const filtered = await client.datasets.list({ context: "organisationId=1" });
 *
 * // Upload a dataset file
 * await client.datasets.upload({
 *   userContextPath: "organisationId=1",
 *   file: { name: "data.csv", content: Buffer.from(fileContent) },
 * });
 *
 * // Delete a dataset file
 * await client.datasets.delete({
 *   userContextPath: "organisationId=1",
 *   file: "data.csv",
 * });
 *
 * // Start sandbox and execute code
 * const sandbox = client.sandbox({
 *   conversationId: "conv-123",
 *   userContextPath: "organisationId=1", // optional, for mounting datasets
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
  userContextPath: string;
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
  userContextPath: string;
  file: string;
}

export interface DeleteDatasetByPathParams {
  path: string; // Full path relative to orgId/agentId (e.g., "userId:123/data.csv")
}

export interface DeleteDatasetsResponse {
  file: string;
  success: boolean;
  error?: string;
}

export interface SandboxParams {
  conversationId: string;
  /** Unique identifier for this run/message. Used to scope the sandbox instance. */
  runId: string;
  /** User context path for mounting datasets bucket (e.g., "organisationId=1"). Can be empty string for root. */
  userContextPath: string;
}

export interface ConversationDataUploadParams {
  conversationId: string;
  userContextPath: string;
  files: {
    name: string;
    content: string; // base64 encoded
    contentType?: string;
  }[];
}

export interface DescribeFileResult {
  name: string;
  targetPath: string;
  sql: string;
  dataSummary: string;
  error: string;
  success: boolean;
}

export interface DescribeFilesResponse {
  files: DescribeFileResult[];
}

export interface ExecuteResponse {
  output: string;
  error: string;
  exitCode: number;
  success: boolean;
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

class DatasetClient {
  constructor(
    private readonly baseUrl: string,
    private readonly headers: Headers,
    private readonly timeoutMs?: number,
  ) {}

  /**
   * List datasets. Optionally filter by request context and navigate folders.
   * @param options.context - User context path to filter by (e.g., "organisationId=1")
   * @param options.path - Subfolder path within the context (e.g., "folder1/folder2")
   */
  async list(options?: {
    context?: string;
    path?: string;
  }): Promise<ListDatasetsResponse> {
    const url = new URL(`${this.baseUrl}/datasets`);
    if (options?.context) {
      url.searchParams.set("context", options.context);
    }
    if (options?.path) {
      url.searchParams.set("path", options.path);
    }

    return this.request<ListDatasetsResponse>(url.toString(), {
      method: "GET",
    });
  }

  /**
   * Upload dataset files using multipart/form-data.
   */
  async upload(params: UploadDatasetsParams): Promise<UploadDatasetsResponse> {
    const formData = new FormData();

    // Convert Buffer/Uint8Array to a new Uint8Array with its own ArrayBuffer
    // This ensures proper BlobPart compatibility (Buffer's underlying buffer is ArrayBufferLike)
    const bytes = Uint8Array.from(params.file.content);

    // Create a Blob from the raw bytes
    const blob = new Blob([bytes], {
      type: params.file.contentType || "application/octet-stream",
    });
    formData.append("file", blob, params.file.name);
    formData.append("userContextPath", params.userContextPath);

    if (params.file.contentType) {
      formData.append("contentType", params.file.contentType);
    }
    if (params.file.notes) {
      formData.append("notes", params.file.notes);
    }

    return this.request<UploadDatasetsResponse>(`${this.baseUrl}/datasets`, {
      method: "POST",
      // Don't set Content-Type header - let fetch set multipart boundary automatically
      body: formData,
    });
  }

  /**
   * Delete a single dataset file (public API - requires context).
   */
  async delete(params: DeleteDatasetsParams): Promise<DeleteDatasetsResponse> {
    const url = new URL(
      `${this.baseUrl}/datasets/${encodeURIComponent(params.file)}`,
    );
    url.searchParams.set("context", params.userContextPath);

    return this.request<DeleteDatasetsResponse>(url.toString(), {
      method: "DELETE",
    });
  }

  /**
   * Delete a dataset file by full path (admin API - no context validation).
   * Path is relative to orgId/agentId (e.g., "userId:123/data.csv").
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
        if (!(init.body instanceof FormData && key.toLowerCase() === "content-type")) {
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
   * Analyzes all JSON/CSV files in the mounted buckets.
   */
  async describeFiles(): Promise<DescribeFilesResponse> {
    const url = new URL(`${this.baseUrl}/sandbox/files`);
    url.searchParams.set("conversationId", this.params.conversationId);
    url.searchParams.set("userContextPath", this.params.userContextPath);
    return this.request<DescribeFilesResponse>(url.toString(), {
      method: "GET",
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
      this.timeoutMs,
    );
  }

  /**
   * Create a sandbox session for a specific conversation.
   * Returns a SandboxSession that can start, execute code, and destroy.
   *
   * @param params.conversationId - The conversation ID (required)
   * @param params.runId - Unique run/message ID (required, scopes the sandbox instance)
   * @param params.userContextPath - User context for mounting datasets (optional)
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
