// @ts-nocheck
import { sql } from "kysely";
import { createVirtualTableJoinProjectionTests } from "../helpers/virtualTableJoinTestFactory";

createVirtualTableJoinProjectionTests({
  dialect: "postgresql",
  expectedSqlBuilder: (db) =>
    db
      .selectFrom("orders as o")
      .innerJoin("organisations as org", "org.id", "o.organisation_id")
      .select([
        sql<number>`COUNT(o.id)::int`.as("total_rows"),
        sql<number>`COUNT(DISTINCT org.name)::int`.as("distinct_org_names"),
      ]),
});
