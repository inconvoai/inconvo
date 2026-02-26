import { type NextRequest, NextResponse } from "next/server";
import { ZodError } from "zod";
import { validateVirtualTableRequestSchema } from "@repo/types";

// Dynamic import to defer connect package loading until runtime
// This prevents env validation from running during build
async function getConnectModule() {
  return await import("@repo/connect");
}

function safeJsonStringify(value: unknown): string {
  return JSON.stringify(value, (_key, val: unknown) => {
    if (typeof val === "bigint") return val.toString();
    return val;
  });
}

/**
 * POST /api/schema/virtual-tables/validate
 * Validate a virtual table SQL query against the configured database.
 *
 * Body: { sql, dialect?, previewLimit? }
 * Returns: ValidateVirtualTableResponse
 */
export async function POST(request: NextRequest) {
  try {
    const body = await request.json();
    const parsed = validateVirtualTableRequestSchema.parse(body);

    const {
      env,
      getDb,
      validateVirtualTableCore,
      QueryExecutionError,
    } = await getConnectModule();

    const db = await getDb();

    const response = await validateVirtualTableCore({
      sql: parsed.sql,
      dialect: env.DATABASE_DIALECT,
      requestDialect: parsed.dialect,
      previewLimit: parsed.previewLimit ?? 1,
      db,
    });

    return new NextResponse(safeJsonStringify(response), {
      status: 200,
      headers: { "Content-Type": "application/json" },
    });
  } catch (error) {
    if (error instanceof ZodError) {
      return NextResponse.json({ error: error.issues }, { status: 400 });
    }

    // Re-import to use in catch (module is cached so this is cheap)
    const { QueryExecutionError } = await getConnectModule();

    if (error instanceof QueryExecutionError) {
      const response = {
        ok: false,
        error: {
          message: error.details.message,
          sql: error.details.sql,
          code: error.details.code,
          detail: error.details.detail,
          hint: error.details.hint,
        },
      };
      return new NextResponse(safeJsonStringify(response), {
        status: 200,
        headers: { "Content-Type": "application/json" },
      });
    }

    const message = error instanceof Error ? error.message : "Unknown error";
    console.error("[/api/schema/virtual-tables/validate POST]", error);
    return new NextResponse(
      safeJsonStringify({ ok: false, error: { message } }),
      {
        status: 200,
        headers: { "Content-Type": "application/json" },
      },
    );
  }
}
