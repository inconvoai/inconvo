import type { NextRequest } from "next/server";
import { NextResponse } from "next/server";
import { ZodError } from "zod";
import { getPostHogClient } from "~/lib/posthog-server";
import { trackQueryPerformance } from "~/lib/telemetry";

// Dynamic import to defer connect package loading until runtime
// This prevents env validation from running during build
async function getConnectModule() {
  return await import("@repo/connect");
}

// GET /api/connect - Return database schema
export async function GET() {
  try {
    const { buildSchema } = await getConnectModule();
    const schema = await buildSchema();
    return NextResponse.json(schema);
  } catch (error) {
    console.error("[/api/connect GET] Schema introspection failed:", error);
    return NextResponse.json(
      { error: "Failed to introspect schema" },
      { status: 500 },
    );
  }
}

// Helper to safely stringify JSON with bigint support
function safeJsonStringify(value: unknown): string {
  return JSON.stringify(value, (_key, val: unknown) => {
    if (typeof val === "bigint") {
      return val.toString();
    }
    return val;
  });
}

// POST /api/connect - Execute query
export async function POST(request: NextRequest) {
  const startTime = Date.now();

  try {
    const body = (await request.json()) as Record<string, unknown>;

    const connect = await getConnectModule();
    const {
      QuerySchema,
      getDb,
      getAugmentedSchema,
      env,
      findMany,
      aggregate,
      count,
      countRelations,
      groupBy,
      aggregateGroups,
      findDistinct,
      findDistinctByEditDistance,
    } = connect;

    const parsedQuery = QuerySchema.parse(body);
    const { operation } = parsedQuery;

    console.log(`[/api/connect POST] Executing operation: ${operation}`);

    const db = await getDb();
    const schema = await getAugmentedSchema();
    const ctx = { schema, dialect: env.DATABASE_DIALECT };

    let response;
    switch (operation) {
      case "aggregate":
        response = await aggregate(db, parsedQuery, ctx);
        break;
      case "count":
        response = await count(db, parsedQuery, ctx);
        break;
      case "countRelations":
        response = await countRelations(db, parsedQuery, ctx);
        break;
      case "findDistinct":
        response = await findDistinct(db, parsedQuery, ctx);
        break;
      case "findDistinctByEditDistance":
        response = await findDistinctByEditDistance(db, parsedQuery, ctx);
        break;
      case "findMany":
        response = await findMany(db, parsedQuery, ctx);
        break;
      case "aggregateGroups":
        response = await aggregateGroups(db, parsedQuery, ctx);
        break;
      case "groupBy":
        response = await groupBy(db, parsedQuery, ctx);
        break;
      default: {
        const _exhaustiveCheck: never = operation;
        throw new Error(`Invalid operation: ${String(_exhaustiveCheck)}`);
      }
    }

    const duration = Date.now() - startTime;
    console.log(`[/api/connect POST] Operation completed in ${duration}ms`);

    // Track server-side query execution
    const posthog = getPostHogClient();
    trackQueryPerformance(posthog, {
      success: true,
      duration_ms: duration,
    });

    return new NextResponse(safeJsonStringify(response), {
      headers: { "Content-Type": "application/json" },
    });
  } catch (error) {
    const duration = Date.now() - startTime;

    // Track server-side query error
    const posthog = getPostHogClient();
    trackQueryPerformance(posthog, {
      success: false,
      duration_ms: duration,
      error_type: error instanceof ZodError ? "validation" : "execution",
    });

    if (error instanceof ZodError) {
      console.error(
        `[/api/connect POST] Validation error after ${duration}ms:`,
        error.issues,
      );
      return NextResponse.json(
        { error: "Invalid query", issues: error.issues },
        { status: 400 },
      );
    }

    console.error(
      `[/api/connect POST] Operation failed after ${duration}ms:`,
      error,
    );
    return NextResponse.json(
      { error: "Failed to execute query" },
      { status: 500 },
    );
  }
}
