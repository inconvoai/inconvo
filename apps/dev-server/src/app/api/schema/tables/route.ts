import { NextResponse, type NextRequest } from "next/server";
import { getTableIds, getTableCount } from "~/lib/schema";

type TableAccess = "QUERYABLE" | "JOINABLE" | "OFF";

/**
 * GET /api/schema/tables
 * Returns list of tables with filtering and pagination support.
 *
 * Query params:
 * - search: string - filter by table name
 * - access: string - comma-separated access levels (e.g., "QUERYABLE,JOINABLE")
 * - page: number - page number (1-indexed)
 * - perPage: number - items per page (default 20)
 */
export async function GET(request: NextRequest) {
  try {
    const { searchParams } = new URL(request.url);

    // Parse query params
    const search = searchParams.get("search") ?? undefined;
    const accessParam = searchParams.get("access");
    const page = parseInt(searchParams.get("page") ?? "1", 10);
    const perPage = parseInt(searchParams.get("perPage") ?? "20", 10);

    // Parse access filter
    let access: TableAccess[] | undefined;
    if (accessParam) {
      access = accessParam
        .split(",")
        .filter((a): a is TableAccess =>
          ["QUERYABLE", "JOINABLE", "OFF"].includes(a),
        );
    }

    // Get tables and count in parallel
    const [tables, totalCount] = await Promise.all([
      getTableIds({
        access,
        searchQuery: search,
        pagination: { page, perPage },
      }),
      getTableCount({
        access,
        searchQuery: search,
      }),
    ]);

    return NextResponse.json({ tables, totalCount });
  } catch (error) {
    console.error("Failed to get tables:", error);
    return NextResponse.json(
      {
        error: error instanceof Error ? error.message : "Failed to get tables",
      },
      { status: 500 },
    );
  }
}
