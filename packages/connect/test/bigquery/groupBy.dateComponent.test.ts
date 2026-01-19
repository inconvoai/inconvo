// @ts-nocheck
import type { Kysely } from "kysely";
import { loadTestEnv } from "../loadTestEnv";

const DAY_ORDER = [
  "Monday",
  "Tuesday",
  "Wednesday",
  "Thursday",
  "Friday",
  "Saturday",
  "Sunday",
];

const MONTH_ORDER = [
  "January",
  "February",
  "March",
  "April",
  "May",
  "June",
  "July",
  "August",
  "September",
  "October",
  "November",
  "December",
];

describe("BigQuery groupBy dateComponent buckets", () => {
  let db: Kysely<any>;
  let QuerySchema: (typeof import("~/types/querySchema"))["QuerySchema"];
  let groupBy: (typeof import("~/operations/groupBy"))["groupBy"];

  beforeAll(async () => {
    jest.setTimeout(120000);
    loadTestEnv("bigquery");

    jest.resetModules();
    delete (globalThis as any).__INCONVO_KYSELY_DB__;

    QuerySchema = (await import("~/types/querySchema")).QuerySchema;
    groupBy = (await import("~/operations/groupBy")).groupBy;
    const { getDb } = await import("~/dbConnection");
    db = await getDb();
  });

  afterAll(async () => {
    await db?.destroy?.();
  });

  test("groups orders by day of week with canonical ordering", async () => {
    const iql = {
      table: "orders",
      tableConditions: null,
      whereAndArray: [],
      operation: "groupBy" as const,
      operationParameters: {
        joins: null,
        groupBy: [
          {
            type: "dateComponent",
            column: "orders.created_at",
            component: "dayOfWeek" as const,
            alias: "day_bucket",
          },
        ],
        count: null,
        countDistinct: null,
        sum: null,
        min: null,
        max: null,
        avg: null,
        orderBy: {
          type: "groupKey" as const,
          key: "day_bucket",
          direction: "asc" as const,
        },
        limit: 7,
      },
    };

    const parsed = QuerySchema.parse(iql);
    const response = await groupBy(db, parsed);

    const rows = "data" in response ? response.data : response;
    expect(Array.isArray(rows)).toBe(true);
    expect(rows.length).toBeGreaterThan(0);
    expect(rows.length).toBeLessThanOrEqual(7);

    const seenOrder = rows.map((row: any) => row.day_bucket) as string[];
    const orderIndexes = seenOrder.map((label) => DAY_ORDER.indexOf(label));
    expect(orderIndexes).not.toContain(-1);
    expect([...orderIndexes]).toEqual(
      [...orderIndexes].slice().sort((a, b) => a - b),
    );
  });

  test("groups orders by month with canonical ordering", async () => {
    const iql = {
      table: "orders",
      tableConditions: null,
      whereAndArray: [],
      operation: "groupBy" as const,
      operationParameters: {
        joins: null,
        groupBy: [
          {
            type: "dateComponent",
            column: "orders.created_at",
            component: "monthOfYear" as const,
            alias: "month_bucket",
          },
        ],
        count: null,
        countDistinct: null,
        sum: null,
        min: null,
        max: null,
        avg: null,
        orderBy: {
          type: "groupKey" as const,
          key: "month_bucket",
          direction: "asc" as const,
        },
        limit: 12,
      },
    };

    const parsed = QuerySchema.parse(iql);
    const response = await groupBy(db, parsed);

    const rows = "data" in response ? response.data : response;
    expect(Array.isArray(rows)).toBe(true);
    expect(rows.length).toBeGreaterThan(0);
    expect(rows.length).toBeLessThanOrEqual(12);

    const seenOrder = rows.map((row: any) => row.month_bucket) as string[];
    const orderIndexes = seenOrder.map((label) => MONTH_ORDER.indexOf(label));
    expect(orderIndexes).not.toContain(-1);
    expect([...orderIndexes]).toEqual(
      [...orderIndexes].slice().sort((a, b) => a - b),
    );
  });

  test("groups orders by quarter of year", async () => {
    const iql = {
      table: "orders",
      tableConditions: null,
      whereAndArray: [],
      operation: "groupBy" as const,
      operationParameters: {
        joins: null,
        groupBy: [
          {
            type: "dateComponent",
            column: "orders.created_at",
            component: "quarterOfYear" as const,
            alias: "quarter_bucket",
          },
        ],
        count: null,
        countDistinct: null,
        sum: null,
        min: null,
        max: null,
        avg: null,
        orderBy: {
          type: "groupKey" as const,
          key: "quarter_bucket",
          direction: "asc" as const,
        },
        limit: 4,
      },
    };

    const parsed = QuerySchema.parse(iql);
    const response = await groupBy(db, parsed);

    const rows = "data" in response ? response.data : response;
    expect(Array.isArray(rows)).toBe(true);
    expect(rows.length).toBeGreaterThan(0);
    expect(rows.length).toBeLessThanOrEqual(4);

    const seenOrder = rows.map((row: any) => row.quarter_bucket) as string[];
    const quarterOrder = ["Q1", "Q2", "Q3", "Q4"];
    const orderIndexes = seenOrder.map((label) => quarterOrder.indexOf(label));
    expect(orderIndexes).not.toContain(-1);
    expect([...orderIndexes]).toEqual(
      [...orderIndexes].slice().sort((a, b) => a - b),
    );
  });

  test("filters by monthOfYear using HAVING with numeric value", async () => {
    const iql = {
      table: "orders",
      tableConditions: null,
      whereAndArray: [],
      operation: "groupBy" as const,
      operationParameters: {
        joins: null,
        groupBy: [
          {
            type: "dateComponent",
            column: "orders.created_at",
            component: "monthOfYear" as const,
            alias: "month_bucket",
          },
        ],
        count: ["orders.id"],
        countDistinct: null,
        sum: null,
        min: null,
        max: null,
        avg: null,
        orderBy: {
          type: "groupKey" as const,
          key: "month_bucket",
          direction: "asc" as const,
        },
        having: [
          {
            type: "groupKey" as const,
            key: "month_bucket",
            operator: "in" as const,
            value: [1, 2, 3], // Q1 months (January, February, March)
          },
        ],
        limit: 12,
      },
    };

    const parsed = QuerySchema.parse(iql);
    const response = await groupBy(db, parsed);

    const rows = "data" in response ? response.data : response;
    expect(Array.isArray(rows)).toBe(true);
    expect(rows.length).toBeLessThanOrEqual(3);

    // All returned months should be Q1 months
    const seenMonths = rows.map((row: any) => row.month_bucket) as string[];
    const q1Months = ["January", "February", "March"];
    seenMonths.forEach((month) => {
      expect(q1Months).toContain(month);
    });

    // Ensure hidden __order column is not in results
    rows.forEach((row: any) => {
      expect(row).not.toHaveProperty("month_bucket__order");
    });
  });
});
