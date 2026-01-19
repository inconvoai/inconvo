import { describe, test, expect } from "vitest";
import { extractJoinedTableNames } from "./extractJoinedTableNames";

describe("extractJoinedTableNames", () => {
  test("returns empty array for null input", () => {
    expect(extractJoinedTableNames(null)).toEqual([]);
  });

  test("returns empty array for undefined input", () => {
    expect(extractJoinedTableNames(undefined)).toEqual([]);
  });

  test("returns empty array for empty joins array", () => {
    expect(extractJoinedTableNames([])).toEqual([]);
  });

  test("extracts single table from simple join", () => {
    const joins = [
      {
        table: "products",
        path: [{ source: ["orders.product_id"], target: ["products.id"] }],
      },
    ];
    expect(extractJoinedTableNames(joins)).toEqual(["products", "orders"]);
  });

  test("extracts tables from multi-hop join path", () => {
    // users -> orders -> products
    const joins = [
      {
        table: "products",
        path: [
          { source: ["users.id"], target: ["orders.user_id"] },
          { source: ["orders.product_id"], target: ["products.id"] },
        ],
      },
    ];
    const result = extractJoinedTableNames(joins);
    expect(result).toContain("users");
    expect(result).toContain("orders");
    expect(result).toContain("products");
    expect(result).toHaveLength(3);
  });

  test("extracts tables from multiple joins", () => {
    const joins = [
      {
        table: "products",
        path: [{ source: ["orders.product_id"], target: ["products.id"] }],
      },
      {
        table: "customers",
        path: [{ source: ["orders.customer_id"], target: ["customers.id"] }],
      },
    ];
    const result = extractJoinedTableNames(joins);
    expect(result).toContain("orders");
    expect(result).toContain("products");
    expect(result).toContain("customers");
  });

  test("deduplicates table names across joins", () => {
    // Two joins that both reference 'orders' table
    const joins = [
      {
        table: "products",
        path: [{ source: ["orders.product_id"], target: ["products.id"] }],
      },
      {
        table: "customers",
        path: [{ source: ["orders.customer_id"], target: ["customers.id"] }],
      },
    ];
    const result = extractJoinedTableNames(joins);
    // 'orders' should only appear once
    expect(result.filter((t) => t === "orders")).toHaveLength(1);
  });

  test("handles multiple joins to same table with different aliases", () => {
    // This is the "buyer/seller" use case - joining users twice
    const joins = [
      {
        table: "users",
        name: "orders.buyer",
        path: [{ source: ["orders.buyer_id"], target: ["users.id"] }],
      },
      {
        table: "users",
        name: "orders.seller",
        path: [{ source: ["orders.seller_id"], target: ["users.id"] }],
      },
    ];
    const result = extractJoinedTableNames(joins);
    // 'users' should appear once (deduplicated)
    expect(result).toContain("users");
    expect(result).toContain("orders");
    // Even though we join users twice, it should only be in the list once for RLS purposes
    expect(result.filter((t) => t === "users")).toHaveLength(1);
  });

  test("handles composite key joins", () => {
    const joins = [
      {
        table: "order_items",
        path: [
          {
            source: ["orders.id", "orders.version"],
            target: ["order_items.order_id", "order_items.order_version"],
          },
        ],
      },
    ];
    const result = extractJoinedTableNames(joins);
    expect(result).toContain("orders");
    expect(result).toContain("order_items");
  });

  test("handles deeply nested multi-hop joins", () => {
    // categories -> products -> order_items -> orders -> customers
    const joins = [
      {
        table: "customers",
        path: [
          { source: ["categories.id"], target: ["products.category_id"] },
          { source: ["products.id"], target: ["order_items.product_id"] },
          { source: ["order_items.order_id"], target: ["orders.id"] },
          { source: ["orders.customer_id"], target: ["customers.id"] },
        ],
      },
    ];
    const result = extractJoinedTableNames(joins);
    expect(result).toContain("categories");
    expect(result).toContain("products");
    expect(result).toContain("order_items");
    expect(result).toContain("orders");
    expect(result).toContain("customers");
    expect(result).toHaveLength(5);
  });
});
