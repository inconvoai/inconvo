// @ts-nocheck
import { sql } from "kysely";
import { createVirtualTableJoinProjectionTests } from "../helpers/virtualTableJoinTestFactory";

createVirtualTableJoinProjectionTests({
  dialect: "mssql",
  expectedSqlBuilder: (db) =>
    db
      .selectFrom("orders as o")
      .innerJoin("organisations as org", "org.id", "o.organisation_id")
      .select([
        sql<number>`CAST(COUNT(o.id) AS INT)`.as("total_rows"),
        sql<number>`CAST(COUNT(DISTINCT org.name) AS INT)`.as(
          "distinct_org_names",
        ),
      ]),
});
