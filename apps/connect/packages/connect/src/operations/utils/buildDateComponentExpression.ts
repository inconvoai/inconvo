import type { SQL } from "drizzle-orm";
import { sql } from "drizzle-orm";
import { env } from "~/env";

export type SupportedDateComponent =
  | "dayOfWeek"
  | "monthOfYear"
  | "quarterOfYear";

interface DateComponentExpressions {
  select: SQL;
  order: SQL;
}

export function buildDateComponentExpressions(
  column: unknown,
  component: SupportedDateComponent
): DateComponentExpressions {
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

  throw new Error(
    "Unsupported database provider. URL must start with 'mysql' or 'postgres'"
  );
}
