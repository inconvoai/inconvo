import { sql } from "kysely";
import { env } from "~/env";

type SupportedInterval = "day" | "week" | "month" | "quarter" | "year" | "hour";

export function buildDateIntervalExpression(column: any, interval: SupportedInterval) {
  if (env.DATABASE_DIALECT === "postgresql") {
    switch (interval) {
      case "day":
        return sql`to_char(${column}::date, 'YYYY-MM-DD')`;
      case "week":
        return sql`EXTRACT(YEAR FROM ${column}) || '-' || EXTRACT(WEEK FROM ${column})`;
      case "month":
        return sql`to_char(${column}::date, 'YYYY-MM')`;
      case "quarter":
        return sql`to_char(${column}::date, 'YYYY') || '-Q' || EXTRACT(QUARTER FROM ${column})`;
      case "hour":
        return sql`to_char(date_trunc('hour', ${column}), 'YYYY-MM-DD HH24:00')`;
      case "year":
        return sql`to_char(${column}::date, 'YYYY')`;
    }
  } else if (env.DATABASE_DIALECT === "mysql") {
    switch (interval) {
      case "day":
        return sql`DATE_FORMAT(${column}, '%Y-%m-%d')`;
      case "week":
        return sql`YEARWEEK(${column})`;
      case "month":
        return sql`DATE_FORMAT(${column}, '%Y-%m')`;
      case "quarter":
        return sql`CONCAT(YEAR(${column}), '-Q', QUARTER(${column}))`;
      case "hour":
        return sql`DATE_FORMAT(${column}, '%Y-%m-%d %H:00')`;
      case "year":
        return sql`YEAR(${column})`;
    }
  } else if (env.DATABASE_DIALECT === "mssql") {
    switch (interval) {
      case "day":
        return sql`FORMAT(${column}, 'yyyy-MM-dd')`;
      case "week":
        return sql`CONCAT(YEAR(${column}), '-W', DATEPART(WEEK, ${column}))`;
      case "month":
        return sql`FORMAT(${column}, 'yyyy-MM')`;
      case "quarter":
        return sql`CONCAT(YEAR(${column}), '-Q', DATEPART(QUARTER, ${column}))`;
      case "hour":
        return sql`FORMAT(${column}, 'yyyy-MM-dd HH:00')`;
      case "year":
        return sql`YEAR(${column})`;
    }
  } else if (env.DATABASE_DIALECT === "bigquery") {
    switch (interval) {
      case "day":
        return sql`FORMAT_TIMESTAMP('%Y-%m-%d', TIMESTAMP(${column}))`;
      case "week":
        return sql`FORMAT_TIMESTAMP('%G-W%V', TIMESTAMP(${column}))`;
      case "month":
        return sql`FORMAT_TIMESTAMP('%Y-%m', TIMESTAMP(${column}))`;
      case "quarter":
        return sql`CONCAT(FORMAT_TIMESTAMP('%Y', TIMESTAMP(${column})), '-Q', CAST(EXTRACT(QUARTER FROM TIMESTAMP(${column})) AS STRING))`;
      case "hour":
        return sql`FORMAT_TIMESTAMP('%Y-%m-%d %H:00', TIMESTAMP(${column}))`;
      case "year":
        return sql`FORMAT_TIMESTAMP('%Y', TIMESTAMP(${column}))`;
    }
  }

  throw new Error(
    "Unsupported database provider. URL must start with 'mysql', 'postgres', 'mssql', or 'bigquery'"
  );
}
