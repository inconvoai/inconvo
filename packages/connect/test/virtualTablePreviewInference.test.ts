// @ts-nocheck
import {
  inferColumnsFromPreviewRows,
  inferVirtualTableType,
} from "~/util/virtualTablePreviewInference";

describe("virtual table preview inference", () => {
  test("treats purely numeric strings as number (pg returns bigint/numeric as strings)", () => {
    expect(inferVirtualTableType("123")).toBe("number");
    expect(inferVirtualTableType("456.78")).toBe("number");
    expect(inferVirtualTableType("-99")).toBe("number");
    expect(inferVirtualTableType("-12.5")).toBe("number");
  });

  test("treats non-numeric strings as string", () => {
    expect(inferVirtualTableType("abc")).toBe("string");
    expect(inferVirtualTableType("hello world")).toBe("string");
    expect(inferVirtualTableType("123abc")).toBe("string");
    expect(inferVirtualTableType("")).toBe("string");
  });

  test("four-digit year-like strings that are not ISO dates are treated as number", () => {
    // "2024" alone is not a date — it's a number. Dates need YYYY-MM-DD.
    expect(inferVirtualTableType("2024")).toBe("number");
  });

  test("keeps ISO-like date/timestamp strings as DateTime", () => {
    expect(inferVirtualTableType("2024-01-01")).toBe("DateTime");
    expect(inferVirtualTableType("2024-01-01T12:30:00Z")).toBe("DateTime");
  });

  test("infers preview column types from mixed row data", () => {
    const inferred = inferColumnsFromPreviewRows([
      {
        name: "test",
        created_at: "2024-01-01T12:30:00Z",
        score: 42.5,
        active: true,
      },
    ]);

    expect(inferred).toEqual([
      { sourceName: "name", type: "string", nullable: false },
      { sourceName: "created_at", type: "DateTime", nullable: false },
      { sourceName: "score", type: "number", nullable: false },
      { sourceName: "active", type: "boolean", nullable: false },
    ]);
  });

  test("infers pg bigint/numeric string values as number", () => {
    // Simulates what node-postgres returns for COUNT/SUM/AVG columns
    const inferred = inferColumnsFromPreviewRows([
      { count: "42", total_revenue: "1234.56", avg_score: "78.90" },
    ]);

    expect(inferred).toEqual([
      { sourceName: "count", type: "number", nullable: false },
      { sourceName: "total_revenue", type: "number", nullable: false },
      { sourceName: "avg_score", type: "number", nullable: false },
    ]);
  });
});
