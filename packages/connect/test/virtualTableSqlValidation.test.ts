// @ts-nocheck
import { validateVirtualTableSqlStatic } from "~/util/virtualTableSqlValidation";

describe("validateVirtualTableSqlStatic", () => {
  test("accepts simple SELECT", () => {
    expect(
      validateVirtualTableSqlStatic("SELECT 1", "postgresql"),
    ).toBeNull();
  });

  test("accepts WITH ... SELECT", () => {
    expect(
      validateVirtualTableSqlStatic(
        "WITH x AS (SELECT 1 AS id) SELECT * FROM x",
        "postgresql",
      ),
    ).toBeNull();
  });

  test("rejects SELECT INTO as non-read-only", () => {
    expect(
      validateVirtualTableSqlStatic(
        "SELECT id INTO tmp_table FROM users",
        "mssql",
      ),
    ).toContain("SELECT INTO");
  });
});

