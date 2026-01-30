import { sql } from "kysely";
import type { DatabaseDialect } from "../types";

type SupportedInterval = "day" | "week" | "month" | "quarter" | "year" | "hour";

export function buildDateIntervalExpression(
  column: any,
  interval: SupportedInterval,
  dialect: DatabaseDialect,
) {
  if (dialect === "postgresql" || dialect === "redshift") {
    switch (interval) {
      case "day":
        return sql`to_char(${column}::date, 'YYYY-MM-DD')`;
      case "week":
        // IYYY = ISO 8601 week-numbering year, IW = ISO week (01-53)
        return sql`to_char(${column}::date, 'IYYY-"W"IW')`;
      case "month":
        return sql`to_char(${column}::date, 'YYYY-MM')`;
      case "quarter":
        return sql`to_char(${column}::date, 'YYYY') || '-Q' || EXTRACT(QUARTER FROM ${column})`;
      case "hour":
        return sql`to_char(date_trunc('hour', ${column}), 'YYYY-MM-DD HH24:00')`;
      case "year":
        return sql`to_char(${column}::date, 'YYYY')`;
    }
  } else if (dialect === "mysql") {
    switch (interval) {
      case "day":
        return sql`DATE_FORMAT(${column}, '%Y-%m-%d')`;
      case "week":
        // %x = ISO 8601 week-numbering year, %v = ISO week (01-53)
        return sql`DATE_FORMAT(${column}, '%x-W%v')`;
      case "month":
        return sql`DATE_FORMAT(${column}, '%Y-%m')`;
      case "quarter":
        return sql`CONCAT(YEAR(${column}), '-Q', QUARTER(${column}))`;
      case "hour":
        return sql`DATE_FORMAT(${column}, '%Y-%m-%d %H:00')`;
      case "year":
        return sql`YEAR(${column})`;
    }
  } else if (dialect === "mssql") {
    switch (interval) {
      case "day":
        // Style 23 = yyyy-mm-dd
        return sql`CONVERT(VARCHAR(10), ${column}, 23)`;
      case "week":
        // MSSQL lacks ISOYEAR, so compute it by shifting date towards mid-year (week 26)
        // ISO_WEEK gives ISO week number; zero-pad for consistent YYYY-WNN format
        return sql`CONCAT(YEAR(DATEADD(day, 26 - DATEPART(ISO_WEEK, ${column}), ${column})), '-W', RIGHT('0' + CAST(DATEPART(ISO_WEEK, ${column}) AS VARCHAR), 2))`;
      case "month":
        // Style 23 = yyyy-mm-dd, take first 7 chars for yyyy-mm
        return sql`LEFT(CONVERT(VARCHAR(10), ${column}, 23), 7)`;
      case "quarter":
        return sql`CONCAT(YEAR(${column}), '-Q', DATEPART(QUARTER, ${column}))`;
      case "hour":
        // Style 120 = yyyy-mm-dd hh:mi:ss, take first 13 chars + ':00' for yyyy-mm-dd hh:00
        return sql`CONCAT(LEFT(CONVERT(VARCHAR(19), ${column}, 120), 13), ':00')`;
      case "year":
        return sql`YEAR(${column})`;
    }
  } else if (dialect === "bigquery") {
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
    `Unsupported database dialect: ${dialect}`,
  );
}
