import type { DatabaseConnector, Query } from "@repo/types";

/**
 * LocalDatabaseConnector
 *
 * A DatabaseConnector implementation that calls the local /api/connect endpoint.
 * This allows us to reuse the connect package's operation logic without HTTP overhead.
 */
export class LocalDatabaseConnector implements DatabaseConnector {
  private baseUrl: string;

  constructor(baseUrl = "http://localhost:26686") {
    this.baseUrl = baseUrl;
  }

  async query(
    query: Query,
  ): Promise<{ data: unknown; query: { sql: string; params: unknown[] } }> {
    const response = await fetch(`${this.baseUrl}/api/connect`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(query),
    });

    if (!response.ok) {
      const errorBody = (await response.json().catch(() => ({}))) as Record<
        string,
        unknown
      >;
      throw new Error(
        `Query failed: ${response.status} ${response.statusText} - ${JSON.stringify(errorBody)}`,
      );
    }

    return response.json() as Promise<{
      data: unknown;
      query: { sql: string; params: unknown[] };
    }>;
  }
}

// Singleton instance
let connectorInstance: LocalDatabaseConnector | null = null;

export function getConnector(): DatabaseConnector {
  if (!connectorInstance) {
    // Use the PORT env var if available, otherwise default to 26686
    const port = process.env.PORT ?? "26686";
    connectorInstance = new LocalDatabaseConnector(`http://localhost:${port}`);
  }
  return connectorInstance;
}
