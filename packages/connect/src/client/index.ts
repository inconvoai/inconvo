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
  manualRelationsSyncSchema,
  computedColumnsSyncSchema,
  type ManualRelationsSync,
  type ComputedColumnsSync,
  columnConversionsSyncSchema,
  type ColumnConversionsSync,
} from "@repo/types";

export class UserDatabaseConnector {
  private client: AxiosInstance;
  private signingSecret: string;

  constructor(options: InconvoOptions) {
    const parsedOptions = InconvoOptionsSchema.parse(options);
    this.client = axios.create({
      baseURL: parsedOptions.baseURL,
      headers: {
        "Content-Type": "application/json",
      },
    });
    this.signingSecret = parsedOptions.signingSecret;
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
    return {
      "inconvo-signature": signature,
      "inconvo-timestamp": timestamp,
      "inconvo-random": random,
    };
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
      if (error instanceof ZodError) {
        throw new Error(`Validation error for Query: ${error.message}`);
      }
      throw error;
    }
  }

  public async syncCustomRelations(
    payload: ManualRelationsSync,
  ): Promise<void> {
    const body = manualRelationsSyncSchema.parse(payload);
    const requestUrl = "/sync/custom-relations";
    const headers = this.generateHeaders("POST", requestUrl, body);
    await this.client.post(requestUrl, body, { headers });
  }

  public async syncComputedColumns(
    payload: ComputedColumnsSync,
  ): Promise<void> {
    const body = computedColumnsSyncSchema.parse(payload);
    const requestUrl = "/sync/computed-columns";
    const headers = this.generateHeaders("POST", requestUrl, body);
    await this.client.post(requestUrl, body, { headers });
  }

  public async syncColumnConversions(
    payload: ColumnConversionsSync,
  ): Promise<void> {
    const body = columnConversionsSyncSchema.parse(payload);
    const requestUrl = "/sync/column-conversions";
    const headers = this.generateHeaders("POST", requestUrl, body);
    await this.client.post(requestUrl, body, { headers });
  }
}

// Re-export utilities
export { computedColumnToString } from "./utils/computedColumnToString";
