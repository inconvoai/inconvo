import { type NextRequest, NextResponse } from "next/server";
import { prisma } from "~/lib/prisma";
import { syncAugmentationsToFile } from "~/lib/schema";
import {
  MAX_ENUM_ENTRIES,
  NUMERIC_OR_STRING_LOGICAL_TYPES,
  normalizeEntries,
} from "../shared";

interface RouteParams {
  params: Promise<{ columnId: string }>;
}

type PatchBody = {
  kind: "CONVERSION" | "STATIC_ENUM" | "DYNAMIC_ENUM";
  selected?: boolean;
  conversionConfig?: {
    ast?: unknown;
    type?: string;
  };
  staticEnumConfig?: {
    entries: Array<{
      value: string | number;
      label: string;
      selected?: boolean;
      position?: number;
    }>;
  };
};

/**
 * PATCH /api/schema/augmentations/[columnId]
 * Update augmentation metadata for a column.
 */
export async function PATCH(request: NextRequest, { params }: RouteParams) {
  const { columnId } = await params;

  try {
    const body = (await request.json()) as PatchBody;
    const { kind, selected, conversionConfig, staticEnumConfig } = body;

    if (
      !kind ||
      (kind !== "CONVERSION" &&
        kind !== "STATIC_ENUM" &&
        kind !== "DYNAMIC_ENUM")
    ) {
      return NextResponse.json(
        { error: "kind must be CONVERSION, STATIC_ENUM, or DYNAMIC_ENUM" },
        { status: 400 },
      );
    }

    if (
      selected === undefined &&
      conversionConfig === undefined &&
      staticEnumConfig === undefined
    ) {
      return NextResponse.json(
        { error: "At least one field must be provided to update augmentation" },
        { status: 400 },
      );
    }

    if (kind === "CONVERSION") {
      if (staticEnumConfig) {
        return NextResponse.json(
          { error: "staticEnumConfig is not allowed for CONVERSION" },
          { status: 400 },
        );
      }
      if (
        conversionConfig &&
        conversionConfig.type !== undefined &&
        conversionConfig.ast === undefined
      ) {
        return NextResponse.json(
          { error: "Updating conversion type requires providing a new ast" },
          { status: 400 },
        );
      }
      if (
        conversionConfig &&
        conversionConfig.type === undefined &&
        conversionConfig.ast === undefined &&
        selected === undefined
      ) {
        return NextResponse.json(
          { error: "No conversion fields provided to update" },
          { status: 400 },
        );
      }
    } else if (kind === "STATIC_ENUM" && conversionConfig) {
      return NextResponse.json(
        { error: "conversionConfig is not allowed for STATIC_ENUM" },
        { status: 400 },
      );
    }

    if (kind === "DYNAMIC_ENUM") {
      if (conversionConfig) {
        return NextResponse.json(
          { error: "conversionConfig is not allowed for DYNAMIC_ENUM" },
          { status: 400 },
        );
      }
      if (staticEnumConfig) {
        return NextResponse.json(
          { error: "staticEnumConfig is not allowed for DYNAMIC_ENUM" },
          { status: 400 },
        );
      }
    }

    const existing = await prisma.columnAugmentation.findUnique({
      where: { columnId },
      select: {
        id: true,
        kind: true,
        columnId: true,
        selected: true,
        column: {
          select: {
            type: true,
          },
        },
      },
    });

    if (!existing) {
      return NextResponse.json(
        { error: "Column augmentation not found" },
        { status: 404 },
      );
    }

    if (existing.kind !== kind) {
      return NextResponse.json(
        {
          error:
            "Kind switch is not allowed in update endpoint. Delete and recreate instead.",
        },
        { status: 400 },
      );
    }

    const rawType = existing.column.type.toLowerCase();
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

    const updateData: {
      selected?: boolean;
      conversionConfig?: {
        update: {
          ast?: string;
          type?: string | null;
        };
      };
      staticEnumConfig?: {
        update: {
          entries: string;
        };
      };
    } = {};

    if (selected !== undefined) {
      updateData.selected = selected;
    }

    if (kind === "CONVERSION" && conversionConfig) {
      const conversionUpdate: {
        ast?: string;
        type?: string | null;
      } = {};
      if (conversionConfig.ast !== undefined) {
        conversionUpdate.ast = JSON.stringify(conversionConfig.ast);
      }
      if (conversionConfig.type !== undefined) {
        conversionUpdate.type = conversionConfig.type ?? null;
      }
      if (Object.keys(conversionUpdate).length > 0) {
        updateData.conversionConfig = {
          update: conversionUpdate,
        };
      }
    }

    if (kind === "STATIC_ENUM" && staticEnumConfig) {
      let normalizedEntries: ReturnType<typeof normalizeEntries>;
      try {
        normalizedEntries = normalizeEntries({
          entries: staticEnumConfig.entries,
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
      if (normalizedEntries.length > MAX_ENUM_ENTRIES) {
        return NextResponse.json(
          { error: `Enum is limited to ${MAX_ENUM_ENTRIES} entries` },
          { status: 400 },
        );
      }
      updateData.staticEnumConfig = {
        update: {
          entries: JSON.stringify(normalizedEntries),
        },
      };
    }

    const updated = await prisma.columnAugmentation.update({
      where: { id: existing.id },
      data: updateData,
      include: {
        conversionConfig: true,
        staticEnumConfig: true,
        dynamicEnumConfig: true,
      },
    });

    if (updated.kind === "CONVERSION") {
      await syncAugmentationsToFile();
    }

    return NextResponse.json({ augmentation: updated });
  } catch (error) {
    console.error("Failed to update column augmentation:", error);
    return NextResponse.json(
      {
        error:
          error instanceof Error
            ? error.message
            : "Failed to update column augmentation",
      },
      { status: 500 },
    );
  }
}

/**
 * DELETE /api/schema/augmentations/[columnId]
 * Delete augmentation metadata for a column.
 */
export async function DELETE(_request: NextRequest, { params }: RouteParams) {
  const { columnId } = await params;

  try {
    const existing = await prisma.columnAugmentation.findUnique({
      where: { columnId },
      select: {
        id: true,
        kind: true,
      },
    });

    if (!existing) {
      return NextResponse.json(
        { error: "Column augmentation not found" },
        { status: 404 },
      );
    }

    await prisma.columnAugmentation.delete({
      where: { id: existing.id },
    });

    if (existing.kind === "CONVERSION") {
      await syncAugmentationsToFile();
    }

    return NextResponse.json({ success: true });
  } catch (error) {
    console.error("Failed to delete column augmentation:", error);
    return NextResponse.json(
      {
        error:
          error instanceof Error
            ? error.message
            : "Failed to delete column augmentation",
      },
      { status: 500 },
    );
  }
}
