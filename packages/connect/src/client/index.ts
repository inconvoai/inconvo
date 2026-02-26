import axios, { type AxiosInstance } from "axios";
import { randomUUID } from "crypto";
import { ZodError } from "zod";
import { generateHmac, generateMessage } from "./hmac";
import {
  SchemaResponseSchema,
  InconvoOptionsSchema,
  querySchema,
  type SchemaResponse,
  type InconvoOptions,
  type Query,
  type QueryResponse,
  queryResponseSchema,
  queryErrorResponseSchema,
  type QueryExecutionErrorDetails,
  unifiedAugmentationsSyncSchema,
  type UnifiedAugmentationsSync,
  validateVirtualTableRequestSchema,
  validateVirtualTableResponseSchema,
  type ValidateVirtualTableRequest,
  type ValidateVirtualTableResponse,
} from "@repo/types";

export class ConnectQueryError extends Error {
  details: QueryExecutionErrorDetails;

  constructor(details: QueryExecutionErrorDetails) {
    super(details.message);
    this.name = "ConnectQueryError";
    this.details = details;
  }
}

export class UserDatabaseConnector {
  private client: AxiosInstance;
  private signingSecret: string;
  private augmentationsHash?: string;
  private connectionId: string;
  private connectionVersion?: number;

  constructor(options: InconvoOptions) {
    const parsedOptions = InconvoOptionsSchema.parse(options);
    this.client = axios.create({
      baseURL: parsedOptions.baseURL,
      headers: {
        "Content-Type": "application/json",
      },
    });
    this.signingSecret = parsedOptions.signingSecret;
    this.augmentationsHash = parsedOptions.augmentationsHash;
    this.connectionId = parsedOptions.connectionId;
    this.connectionVersion = parsedOptions.connectionVersion;
  }

  private generateHeaders(
    method: string,
    requestUrl: string,
    body?: object,
  ): Record<string, string> {
    const timestamp = Math.floor(Date.now() / 1000).toString();
    const random = randomUUID();
    const message = generateMessage({
      method,
      requestUrl,
      timestamp,
      random,
      body,
    });
    const signature = generateHmac(message, this.signingSecret);
    const headers: Record<string, string> = {
      "inconvo-signature": signature,
      "inconvo-timestamp": timestamp,
      "inconvo-random": random,
      "inconvo-connection-id": this.connectionId,
    };
    // Include augmentations hash on POST requests (queries)
    if (method === "POST" && this.augmentationsHash) {
      headers["inconvo-augmentations-hash"] = this.augmentationsHash;
    }
    if (this.connectionVersion !== undefined) {
      headers["inconvo-connection-version"] = String(this.connectionVersion);
    }
    return headers;
  }

  public async getSchema(): Promise<SchemaResponse> {
    const requestUrl = "/";
    const headers = this.generateHeaders("GET", requestUrl);
    try {
      const response = await this.client.get(requestUrl, { headers });
      return SchemaResponseSchema.parse(response.data);
    } catch (error) {
      if (error instanceof ZodError) {
        throw new Error(`Schema validation error: ${error.message}`);
      }
      throw error;
    }
  }

  public async query(query: Query): Promise<QueryResponse> {
    const parsedQuery = querySchema.parse(query);
    const requestUrl = "/";
    const headers = this.generateHeaders("POST", requestUrl, parsedQuery);
    try {
      const response = await this.client.post(requestUrl, parsedQuery, {
        headers,
      });
      const parsedResponse = queryResponseSchema.parse(response.data);
      return parsedResponse;
    } catch (error) {
      if (axios.isAxiosError(error) && error.response?.data) {
        const parsedError = queryErrorResponseSchema.safeParse(
          error.response.data,
        );
        if (parsedError.success) {
          throw new ConnectQueryError(parsedError.data.error);
        }
      }
      if (error instanceof ZodError) {
        throw new Error(`Validation error for Query: ${error.message}`);
      }
      throw error;
    }
  }

  public async syncAugmentations(
    payload: UnifiedAugmentationsSync,
  ): Promise<void> {
    const body = unifiedAugmentationsSyncSchema.parse(payload);
    const requestUrl = "/sync/augmentations";
    const headers = this.generateHeaders("POST", requestUrl, body);
    await this.client.post(requestUrl, body, { headers });
  }

  public async clearCache(): Promise<void> {
    const body = {};
    const requestUrl = "/cache/clear";
    const headers = this.generateHeaders("POST", requestUrl, body);
    await this.client.post(requestUrl, body, { headers });
  }

  /**
   * Sync schema to Lambda storage (S3).
   * Called by Platform after introspecting the database schema.
   * Only needed for Lambda-based connectors, not AppRunner.
   */
  public async syncSchema(schema: SchemaResponse): Promise<void> {
    const body = SchemaResponseSchema.parse(schema);
    const requestUrl = "/sync/schema";
    const headers = this.generateHeaders("POST", requestUrl, body);
    await this.client.post(requestUrl, body, { headers });
  }

  public async validateVirtualTable(
    payload: ValidateVirtualTableRequest,
  ): Promise<ValidateVirtualTableResponse> {
    const body = validateVirtualTableRequestSchema.parse(payload);
    const requestUrl = "/validate/virtual-table";
    const headers = this.generateHeaders("POST", requestUrl, body);
    const response = await this.client.post(requestUrl, body, { headers });
    return validateVirtualTableResponseSchema.parse(response.data);
  }
}

// Re-export utilities
export { computedColumnToString } from "./utils/computedColumnToString";
