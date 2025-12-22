import { sql } from "kysely";
import { env } from "../../env";

export type SupportedDateComponent =
  | "dayOfWeek"
  | "monthOfYear"
  | "quarterOfYear";

export function buildDateComponentExpressions(
  column: any,
  component: SupportedDateComponent,
) {
  if (env.DATABASE_DIALECT === "postgresql") {
    switch (component) {
      case "dayOfWeek": {
        const order = sql`(((EXTRACT(DOW FROM ${column})::int + 6) % 7) + 1)`;
        const select = sql`to_char(${column}, 'FMDay')`;
        return { select, order };
      }
      case "monthOfYear": {
        const order = sql`EXTRACT(MONTH FROM ${column})::int`;
        const select = sql`to_char(${column}, 'FMMonth')`;
        return { select, order };
      }
      case "quarterOfYear": {
        const order = sql`EXTRACT(QUARTER FROM ${column})::int`;
        const select = sql`'Q' || to_char(${column}, 'Q')`;
        return { select, order };
      }
      default:
        throw new Error(`Unsupported date component: ${component}`);
    }
  }

  if (env.DATABASE_DIALECT === "mysql") {
    switch (component) {
      case "dayOfWeek": {
        const order = sql`((DAYOFWEEK(${column}) + 5) % 7) + 1`;
        const select = sql`ELT(((DAYOFWEEK(${column}) + 5) % 7) + 1, 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')`;
        return { select, order };
      }
      case "monthOfYear": {
        const order = sql`MONTH(${column})`;
        const select = sql`ELT(MONTH(${column}), 'January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December')`;
        return { select, order };
      }
      case "quarterOfYear": {
        const order = sql`QUARTER(${column})`;
        const select = sql`CONCAT('Q', QUARTER(${column}))`;
        return { select, order };
      }
      default:
        throw new Error(`Unsupported date component: ${component}`);
    }
  }

  if (env.DATABASE_DIALECT === "mssql") {
    switch (component) {
      case "dayOfWeek": {
        const order = sql`((DATEPART(weekday, ${column}) + 5) % 7) + 1`;
        const select = sql`CHOOSE(((DATEPART(weekday, ${column}) + 5) % 7) + 1, 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')`;
        return { select, order };
      }
      case "monthOfYear": {
        const order = sql`DATEPART(month, ${column})`;
        const select = sql`DATENAME(month, ${column})`;
        return { select, order };
      }
      case "quarterOfYear": {
        const order = sql`DATEPART(quarter, ${column})`;
        const select = sql`'Q' + CAST(DATEPART(quarter, ${column}) AS varchar(1))`;
        return { select, order };
      }
      default:
        throw new Error(`Unsupported date component: ${component}`);
    }
  }

  if (env.DATABASE_DIALECT === "bigquery") {
    switch (component) {
      case "dayOfWeek": {
        const order = sql`MOD(EXTRACT(DAYOFWEEK FROM TIMESTAMP(${column})) + 5, 7) + 1`;
        const select = sql`FORMAT_TIMESTAMP('%A', TIMESTAMP(${column}))`;
        return { select, order };
      }
      case "monthOfYear": {
        const order = sql`EXTRACT(MONTH FROM TIMESTAMP(${column}))`;
        const select = sql`FORMAT_TIMESTAMP('%B', TIMESTAMP(${column}))`;
        return { select, order };
      }
      case "quarterOfYear": {
        const order = sql`EXTRACT(QUARTER FROM TIMESTAMP(${column}))`;
        const select = sql`CONCAT('Q', CAST(EXTRACT(QUARTER FROM TIMESTAMP(${column})) AS STRING))`;
        return { select, order };
      }
      default:
        throw new Error(`Unsupported date component: ${component}`);
    }
  }

  throw new Error(
    "Unsupported database provider. URL must start with  'postgresql', 'mysql', 'mssql', or 'bigquery'",
  );
}
