import { sql } from "drizzle-orm";
import { env } from "~/env";

type SupportedInterval = "day" | "week" | "month" | "quarter" | "year" | "hour";

export function buildDateIntervalExpression(
  column: unknown,
  interval: SupportedInterval
) {
  if (env.DATABASE_DIALECT === "mysql") {
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
  } else if (env.DATABASE_DIALECT === "postgresql") {
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
  }

  throw new Error(
    "Unsupported database provider. URL must start with 'mysql' or 'postgres'"
  );
}
