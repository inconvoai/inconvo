import { env } from "~/env.js";

/**
 * Inconvo Sandbox client SDK for server-side environments.
 *
 * Usage:
 * ```ts
 * import { InconvoSandbox } from "./sandbox";
 *
 * const sandbox = new InconvoSandbox({
 *   sandboxId: "conversation-or-message-id",
 * });
 *
 * await sandbox.uploadFiles([
 *   { name: "sample.json", content: JSON.stringify({ hello: "world" }) },
 * ]);
 *
 * const description = await sandbox.describeFiles(["sample.json"]);
 * const execution = await sandbox.executeCode("print('hello from sandbox')");
 * await sandbox.destroySandbox();
 * ```
 */

export interface SandboxSDKOptions {
  sandboxId: string;
  baseUrl?: string;
  apiKey?: string;
  timeoutMs?: number;
}

export type UploadContent = string | ArrayBuffer | ArrayBufferView | Blob;

export interface UploadFile {
  name: string;
  content: UploadContent;
}

export interface UploadedFileMetadata {
  name: string;
  path: string;
  size: number;
}

export interface UploadResponse {
  files: UploadedFileMetadata[];
}

export interface DescribeFileResult {
  name: string;
  sql: string;
  dataSummary: string;
  error: string;
  success: boolean;
}

export interface DescribeResponse {
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

const SANDBOX_ID_HEADER = "x-inconvo-message-id";

export class InconvoSandbox {
  private readonly baseUrl: string;
  private readonly sandboxId: string;
  private readonly apiKey?: string;
  private readonly timeoutMs?: number;

  constructor(options: SandboxSDKOptions) {
    if (!options.sandboxId) {
      throw new Error("InconvoSandbox requires a sandboxId.");
    }

    const baseUrl = options.baseUrl ?? env.INCONVO_SANDBOX_BASE_URL;
    if (!baseUrl) {
      throw new Error("InconvoSandbox requires a baseUrl (either via options or INCONVO_SANDBOX_BASE_URL environment variable).");
    }

    this.baseUrl = baseUrl.replace(/\/+$/, "");
    this.sandboxId = options.sandboxId;
    this.apiKey = options.apiKey ?? env.INCONVO_SANDBOX_API_KEY;
    this.timeoutMs = options.timeoutMs;
  }

  /**
   * Uploads one or more JSON files into the sandbox workspace.
   */

  async uploadFiles(files: UploadFile[]): Promise<UploadResponse> {
    if (files.length === 0) {
      throw new Error("uploadFiles requires at least one file.");
    }

    const form = new FormData();
    for (const file of files) {
      form.append("files", this.toBlob(file.content), file.name);
    }

    return this.request<UploadResponse>("/sandbox/files", {
      method: "POST",
      body: form,
    });
  }

  /**
   * Runs the built-in JSON analyzer for the provided file names.
   */
  async describeFiles(fileNames: string[]): Promise<DescribeResponse> {
    if (fileNames.length === 0) {
      throw new Error("describeFiles requires at least one file name.");
    }

    return this.request<DescribeResponse>("/sandbox/files/describe", {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ files: fileNames }),
    });
  }

  /**
   * Executes arbitrary Python code in the sandbox.
   */
  async executeCode(code: string): Promise<ExecuteResponse> {
    if (!code.trim()) {
      throw new Error("executeCode requires non-empty code.");
    }

    return this.request<ExecuteResponse>("/sandbox/execute", {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ code }),
    });
  }

  /**
   * Destroys the sandbox associated with the provided sandbox ID.
   */
  async destroySandbox(): Promise<void> {
    await this.request("/sandbox", { method: "DELETE" }, { expectJson: false });
  }

  private toBlob(content: UploadContent): Blob {
    if (content instanceof Blob) {
      return content;
    }

    if (typeof content === "string") {
      return new Blob([content], { type: "application/json" });
    }

    if (content instanceof ArrayBuffer) {
      return new Blob([content]);
    }

    // ArrayBufferView covers Uint8Array, Buffer (Node.js), etc.
    // Cast to BlobPart to satisfy TypeScript's strict typing
    return new Blob([content as BlobPart]);
  }

  private buildHeaders(initHeaders?: HeadersInit): Headers {
    const headers = new Headers(initHeaders ?? {});
    headers.set(SANDBOX_ID_HEADER, this.sandboxId);

    if (this.apiKey) {
      headers.set("authorization", `Bearer ${this.apiKey}`);
    }

    return headers;
  }

  private async request<T = unknown>(
    path: string,
    init: RequestInit,
    options: { expectJson?: boolean } = { expectJson: true }
  ): Promise<T> {
    const controller =
      typeof this.timeoutMs === "number" ? new AbortController() : undefined;

    if (controller && this.timeoutMs && this.timeoutMs > 0) {
      setTimeout(() => controller.abort(), this.timeoutMs).unref?.();
    }

    const headers = this.buildHeaders(init.headers);

    const response = await fetch(`${this.baseUrl}${path}`, {
      ...init,
      headers,
      signal: controller?.signal,
    }).catch((error: unknown) => {
      throw new SandboxSDKError(
        `Failed to reach sandbox worker: ${
          error instanceof Error ? error.message : String(error)
        }`,
        0,
        error
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
        details
      );
    }

    if (options.expectJson === false || response.status === 204) {
      // @ts-expect-error - caller indicated no JSON body is expected.
      return undefined;
    }

    return (await response.json()) as T;
  }
}
