import type {
  AgentInfo,
  ConnectionDescriptionUpdateResponse,
  ConnectionDetails,
  ConnectionInfo,
  ConnectionLinkResponse,
  ModelActionType,
  ModelActionResponse,
  ConnectionSemanticModelResponse,
  OrgInfo,
  ShareableConnectionInfo,
  UserContextResponse,
} from "./types.js";

type PayloadWithContext = { description?: string | null; context?: string | null };

export class PlatformApiError extends Error {
  readonly status: number;
  readonly payload: unknown;

  constructor(params: { message: string; status: number; payload: unknown }) {
    super(params.message);
    this.name = "PlatformApiError";
    this.status = params.status;
    this.payload = params.payload;
  }
}

function normalizeBaseUrl(rawBaseUrl: string): string {
  const trimmed = rawBaseUrl.trim().replace(/\/+$/, "");
  if (trimmed.endsWith("/api/v1")) {
    return trimmed;
  }
  return `${trimmed}/api/v1`;
}

function isRecord(value: unknown): value is Record<string, unknown> {
  return typeof value === "object" && value !== null && !Array.isArray(value);
}

function nonEmptyString(value: unknown): string | undefined {
  if (typeof value !== "string") {
    return undefined;
  }
  const trimmed = value.trim();
  return trimmed.length > 0 ? trimmed : undefined;
}

/** Resolve `description` from response, falling back to legacy `context` field. */
function resolveDescription(payload: PayloadWithContext): string | null {
  return payload.description ?? payload.context ?? null;
}

function extractListFromPayload<T>(params: {
  payload: unknown;
  listKey: string;
  endpoint: string;
}): T[] {
  const { payload, listKey, endpoint } = params;

  if (Array.isArray(payload)) {
    // Backward compatibility: older endpoints may return a bare array.
    return payload as T[];
  }

  if (isRecord(payload) && Array.isArray(payload[listKey])) {
    return payload[listKey] as T[];
  }

  throw new Error(
    `Unexpected response from ${endpoint}. Expected an array or object with '${listKey}' array.`,
  );
}

async function parseJsonSafe(response: Response): Promise<unknown> {
  const text = await response.text();
  if (!text) {
    return {};
  }
  try {
    return JSON.parse(text) as unknown;
  } catch {
    return { error: text };
  }
}

export class PlatformApiClient {
  private readonly baseUrl: string;
  private readonly apiKey: string;

  constructor(options: { baseUrl: string; apiKey: string }) {
    this.baseUrl = normalizeBaseUrl(options.baseUrl);
    this.apiKey = options.apiKey;
  }

  private async request<T>(path: string, init?: RequestInit): Promise<T> {
    const response = await fetch(`${this.baseUrl}${path}`, {
      ...init,
      headers: {
        Authorization: `Bearer ${this.apiKey}`,
        "Content-Type": "application/json",
        ...(init?.headers ?? {}),
      },
    });

    const payload = await parseJsonSafe(response);

    if (!response.ok) {
      const payloadError =
        isRecord(payload) && "error" in payload
          ? nonEmptyString((payload as { error?: unknown }).error)
          : undefined;
      const message = payloadError ?? `HTTP ${response.status}`;
      throw new PlatformApiError({
        message,
        status: response.status,
        payload,
      });
    }

    return payload as T;
  }

  async getOrg(): Promise<OrgInfo> {
    const payload = await this.request<unknown>("/org");
    if (
      isRecord(payload) &&
      typeof payload.id === "string" &&
      typeof payload.name === "string"
    ) {
      return { id: payload.id, name: payload.name };
    }

    throw new Error(
      "Unexpected response from /org. Expected an object with 'id' and 'name'.",
    );
  }

  async listOrgAgents(): Promise<AgentInfo[]> {
    const payload = await this.request<unknown>("/org/agents");
    return extractListFromPayload<AgentInfo>({
      payload,
      listKey: "agents",
      endpoint: "/org/agents",
    });
  }

  async listAgentConnections(agentId: string): Promise<ConnectionInfo[]> {
    const endpoint = `/agents/${encodeURIComponent(agentId)}/connections`;
    const payload = await this.request<unknown>(endpoint);
    return extractListFromPayload<ConnectionInfo & PayloadWithContext>({
      payload,
      listKey: "connections",
      endpoint,
    }).map((c) => ({ ...c, description: resolveDescription(c) }));
  }

  async getAgentUserContext(agentId: string): Promise<UserContextResponse> {
    return this.request<UserContextResponse>(
      `/agents/${encodeURIComponent(agentId)}/user-context`,
    );
  }

  async getConnectionSemanticModel(
    agentId: string,
    connectionId: string,
  ): Promise<ConnectionSemanticModelResponse> {
    return this.request<ConnectionSemanticModelResponse>(
      `/agents/${encodeURIComponent(agentId)}/connections/${encodeURIComponent(connectionId)}/semantic-model`,
    );
  }

  async runModelAction(
    agentId: string,
    payload: {
      action: ModelActionType;
      payload: unknown;
    },
  ): Promise<ModelActionResponse> {
    return this.request<ModelActionResponse>(
      `/agents/${encodeURIComponent(agentId)}/model/actions`,
      {
        method: "POST",
        body: JSON.stringify(payload),
      },
    );
  }

  async listShareableConnections(
    agentId: string,
  ): Promise<ShareableConnectionInfo[]> {
    const endpoint = `/agents/${encodeURIComponent(agentId)}/connections/shareable`;
    const payload = await this.request<unknown>(endpoint);
    return extractListFromPayload<ShareableConnectionInfo & PayloadWithContext>({
      payload,
      listKey: "connections",
      endpoint,
    }).map((c) => ({ ...c, description: resolveDescription(c) }));
  }

  async getConnection(
    agentId: string,
    connectionId: string,
  ): Promise<ConnectionDetails> {
    return this.request<ConnectionDetails>(
      `/agents/${encodeURIComponent(agentId)}/connections/${encodeURIComponent(connectionId)}`,
    );
  }

  async updateConnectionDescription(
    agentId: string,
    connectionId: string,
    description: string | null,
  ): Promise<ConnectionDescriptionUpdateResponse> {
    return this.request<ConnectionDescriptionUpdateResponse>(
      `/agents/${encodeURIComponent(agentId)}/connections/${encodeURIComponent(connectionId)}`,
      { method: "PATCH", body: JSON.stringify({ description }) },
    );
  }

  async linkConnection(
    agentId: string,
    connectionId: string,
  ): Promise<ConnectionLinkResponse> {
    return this.request<ConnectionLinkResponse>(
      `/agents/${encodeURIComponent(agentId)}/connections/link`,
      {
        method: "POST",
        body: JSON.stringify({ connectionId }),
      },
    );
  }

  async unlinkConnection(
    agentId: string,
    connectionId: string,
  ): Promise<ConnectionLinkResponse> {
    return this.request<ConnectionLinkResponse>(
      `/agents/${encodeURIComponent(agentId)}/connections/unlink`,
      {
        method: "POST",
        body: JSON.stringify({ connectionId }),
      },
    );
  }

  async syncConnection(
    agentId: string,
    connectionId: string,
  ): Promise<{ message: string }> {
    return this.request<{ message: string }>(
      `/agents/${encodeURIComponent(agentId)}/connections/${encodeURIComponent(connectionId)}/sync`,
      { method: "POST" },
    );
  }
}
