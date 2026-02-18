import { type NextRequest, NextResponse } from "next/server";
import { prisma } from "~/lib/prisma";
import { syncAugmentationsToFile } from "~/lib/schema";
import { resolveEffectiveColumnType } from "@repo/types";
import {
  isFindDistinctLimitHitError,
  normalizeDistinctEntriesForEnum,
} from "@repo/schema-utils";
import {
  ENUM_FETCH_LIMIT,
  MAX_ENUM_ENTRIES,
  NUMERIC_OR_STRING_LOGICAL_TYPES,
  normalizeEntries,
  type ValueEnumEntryInput,
} from "./shared";

// Dynamic import to defer connect package loading until runtime.
async function getConnectModule() {
  return await import("@repo/connect");
}

type CreateBody = {
  tableId: string;
  columnId: string;
  kind: "CONVERSION" | "STATIC_ENUM" | "DYNAMIC_ENUM";
  selected?: boolean;
  conversionConfig?: {
    ast: unknown;
    type?: string;
  };
  staticEnumConfig?: {
    entries: ValueEnumEntryInput[];
  };
};

/**
 * GET /api/schema/augmentations?tableId=...&columnId=...
 * Return enum suggestions from distinct column values.
 */
export async function GET(request: NextRequest) {
  try {
    const tableId = request.nextUrl.searchParams.get("tableId");
    const columnId = request.nextUrl.searchParams.get("columnId");

    if (!tableId || !columnId) {
      return NextResponse.json(
        { error: "tableId and columnId are required" },
        { status: 400 },
      );
    }

    const column = await prisma.column.findFirst({
      where: { id: columnId, tableId },
      select: {
        name: true,
        type: true,
        augmentation: {
          select: {
            kind: true,
            selected: true,
            conversionConfig: {
              select: {
                type: true,
              },
            },
          },
        },
        table: {
          select: {
            name: true,
            schema: true,
          },
        },
      },
    });

    if (!column) {
      return NextResponse.json({ error: "Column not found" }, { status: 404 });
    }

    const conversionLike =
      column.augmentation?.kind === "CONVERSION"
        ? {
            selected: column.augmentation.selected,
            type: column.augmentation.conversionConfig?.type ?? null,
          }
        : null;
    const effectiveType = resolveEffectiveColumnType(column.type, conversionLike);
    if (
      !NUMERIC_OR_STRING_LOGICAL_TYPES.has(effectiveType)
    ) {
      return NextResponse.json(
        {
          error:
            "Distinct enum suggestions are only supported for string or numeric columns",
        },
        { status: 400 },
      );
    }

    const connect = await getConnectModule();
    const { QuerySchema, getDb, getAugmentedSchema, env, findDistinct } = connect;
    const query = QuerySchema.parse({
      table: column.table.name,
      tableSchema: column.table.schema ?? null,
      whereAndArray: [],
      tableConditions: null,
      operation: "findDistinct",
      operationParameters: {
        column: `${column.table.name}.${column.name}`,
        limit: ENUM_FETCH_LIMIT,
      },
    });

    const db = await getDb();
    const schema = await getAugmentedSchema();
    let queryResponse: Awaited<ReturnType<typeof findDistinct>>;
    try {
      queryResponse = await findDistinct(db, query, {
        schema,
        dialect: env.DATABASE_DIALECT,
      });
    } catch (error) {
      if (isFindDistinctLimitHitError(error, ENUM_FETCH_LIMIT)) {
        return NextResponse.json(
          { error: "Sorry, enum is currently limited to 50 distinct values." },
          { status: 400 },
        );
      }
      throw error;
    }

    const entries = normalizeDistinctEntriesForEnum({
      rows: queryResponse.data,
      tableName: column.table.name,
      columnName: column.name,
      effectiveType,
      fetchLimit: ENUM_FETCH_LIMIT,
    });
    if (entries.length >= ENUM_FETCH_LIMIT) {
      return NextResponse.json(
        {
          error: `Sorry, enum is currently limited to ${MAX_ENUM_ENTRIES} distinct values.`,
        },
        { status: 400 },
      );
    }

    return NextResponse.json({ entries });
  } catch (error) {
    console.error("Failed to fetch distinct enum values:", error);
    return NextResponse.json(
      {
        error:
          error instanceof Error
            ? error.message
            : "Failed to fetch distinct enum values",
      },
      { status: 500 },
    );
  }
}

/**
 * POST /api/schema/augmentations
 * Create column augmentation metadata.
 */
export async function POST(request: NextRequest) {
  try {
    const body = (await request.json()) as CreateBody;
    const { tableId, columnId, kind, selected = true } = body;

    if (!tableId || !columnId || !kind) {
      return NextResponse.json(
        { error: "tableId, columnId, and kind are required" },
        { status: 400 },
      );
    }
    if (
      kind !== "CONVERSION" &&
      kind !== "STATIC_ENUM" &&
      kind !== "DYNAMIC_ENUM"
    ) {
      return NextResponse.json(
        { error: "Unsupported augmentation kind" },
        { status: 400 },
      );
    }

    if (kind === "CONVERSION") {
      if (!body.conversionConfig?.ast) {
        return NextResponse.json(
          { error: "conversionConfig.ast is required for CONVERSION" },
          { status: 400 },
        );
      }
      if (body.staticEnumConfig) {
        return NextResponse.json(
          { error: "staticEnumConfig is not allowed for CONVERSION" },
          { status: 400 },
        );
      }
    }

    if (kind === "STATIC_ENUM") {
      if (!body.staticEnumConfig?.entries) {
        return NextResponse.json(
          { error: "staticEnumConfig.entries is required for STATIC_ENUM" },
          { status: 400 },
        );
      }
      if (body.conversionConfig) {
        return NextResponse.json(
          { error: "conversionConfig is not allowed for STATIC_ENUM" },
          { status: 400 },
        );
      }
    }

    if (kind === "DYNAMIC_ENUM") {
      if (body.conversionConfig) {
        return NextResponse.json(
          { error: "conversionConfig is not allowed for DYNAMIC_ENUM" },
          { status: 400 },
        );
      }
      if (body.staticEnumConfig) {
        return NextResponse.json(
          { error: "staticEnumConfig is not allowed for DYNAMIC_ENUM" },
          { status: 400 },
        );
      }
    }

    const column = await prisma.column.findFirst({
      where: {
        id: columnId,
        tableId,
      },
      select: {
        id: true,
        type: true,
        tableId: true,
        augmentation: {
          select: {
            id: true,
            kind: true,
          },
        },
      },
    });

    if (!column) {
      return NextResponse.json({ error: "Column not found" }, { status: 404 });
    }

    const rawType = column.type.toLowerCase();
    if (kind === "CONVERSION" && rawType !== "string") {
      return NextResponse.json(
        { error: "Column conversions are only supported on string columns" },
        { status: 400 },
      );
    }
    if (
      (kind === "STATIC_ENUM" || kind === "DYNAMIC_ENUM") &&
      !NUMERIC_OR_STRING_LOGICAL_TYPES.has(rawType)
    ) {
      return NextResponse.json(
        { error: "Enums are only supported for string or numeric columns" },
        { status: 400 },
      );
    }

    if (column.augmentation?.kind === kind) {
      return NextResponse.json(
        { error: `An augmentation of kind ${kind} already exists for this column` },
        { status: 409 },
      );
    }

    let normalizedEntries:
      | ReturnType<typeof normalizeEntries>
      | undefined;
    if (kind === "STATIC_ENUM") {
      try {
        normalizedEntries = normalizeEntries({
          entries: body.staticEnumConfig?.entries ?? [],
          effectiveType: rawType,
        });
      } catch (error) {
        return NextResponse.json(
          {
            error:
              error instanceof Error ? error.message : "Invalid enum entries",
          },
          { status: 400 },
        );
      }
    }

    if (
      kind === "STATIC_ENUM" &&
      normalizedEntries &&
      normalizedEntries.length > MAX_ENUM_ENTRIES
    ) {
      return NextResponse.json(
        { error: `Enum is limited to ${MAX_ENUM_ENTRIES} entries` },
        { status: 400 },
      );
    }

    const augmentation = await prisma.$transaction(async (tx) => {
      if (column.augmentation) {
        await tx.columnAugmentation.delete({
          where: { id: column.augmentation.id },
        });
      }

      return tx.columnAugmentation.create({
        data: {
          kind,
          selected,
          columnId: column.id,
          tableId: column.tableId,
          ...(kind === "CONVERSION" && body.conversionConfig
            ? {
                conversionConfig: {
                  create: {
                    ast: JSON.stringify(body.conversionConfig.ast),
                    type: body.conversionConfig.type ?? null,
                  },
                },
              }
            : {}),
          ...(kind === "STATIC_ENUM" && normalizedEntries
            ? {
                staticEnumConfig: {
                  create: {
                    entries: JSON.stringify(normalizedEntries),
                  },
                },
              }
            : {}),
          ...(kind === "DYNAMIC_ENUM"
            ? {
                dynamicEnumConfig: {
                  create: {},
                },
              }
            : {}),
        },
        include: {
          conversionConfig: true,
          staticEnumConfig: true,
          dynamicEnumConfig: true,
        },
      });
    });

    if (kind === "CONVERSION" || column.augmentation?.kind === "CONVERSION") {
      await syncAugmentationsToFile();
    }

    return NextResponse.json({ augmentation });
  } catch (error) {
    console.error("Failed to create column augmentation:", error);
    return NextResponse.json(
      {
        error:
          error instanceof Error
            ? error.message
            : "Failed to create column augmentation",
      },
      { status: 500 },
    );
  }
}
