import { NextResponse } from "next/server";
import { prisma } from "~/lib/prisma";
import { syncAugmentationsToFile, syncSchema } from "~/lib/schema";

/**
 * POST /api/demo/setup
 * Configure the SQLite database for demo mode.
 * Sets up user context, computed columns, table conditions, and enables all tables.
 */
export async function POST() {
  try {
    // First, sync the schema from the demo database
    await syncSchema();

    // 1. Create user context field for organisation filtering
    const userContextField = await prisma.userContextField.upsert({
      where: { key: "organisationId" },
      update: { type: "NUMBER" },
      create: { key: "organisationId", type: "NUMBER" },
    });

    // 2. Get tables we need to configure
    const ordersTable = await prisma.table.findUnique({
      where: { name: "orders" },
      include: { columns: true },
    });

    const usersTable = await prisma.table.findUnique({
      where: { name: "users" },
      include: { columns: true },
    });

    const productsTable = await prisma.table.findUnique({
      where: { name: "products" },
      include: { columns: true },
    });

    const reviewsTable = await prisma.table.findUnique({
      where: { name: "reviews" },
      include: { columns: true },
    });

    const organisationsTable = await prisma.table.findUnique({
      where: { name: "organisations" },
      include: { columns: true },
    });

    // 3. Create computed column 'total' on orders table (subtotal - discount + tax)
    if (ordersTable) {
      const subtotalCol = ordersTable.columns.find((c) => c.name === "subtotal");
      const discountCol = ordersTable.columns.find((c) => c.name === "discount");
      const taxCol = ordersTable.columns.find((c) => c.name === "tax");

      if (subtotalCol && discountCol && taxCol) {
        // AST for: subtotal - discount + tax
        const totalAst = {
          type: "operation",
          operator: "+",
          operands: [
            {
              type: "operation",
              operator: "-",
              operands: [
                { type: "column", name: "subtotal" },
                { type: "column", name: "discount" },
              ],
            },
            { type: "column", name: "tax" },
          ],
        };

        await prisma.computedColumn.upsert({
          where: {
            tableId_name: { tableId: ordersTable.id, name: "total" },
          },
          update: {
            ast: JSON.stringify(totalAst),
            type: "number",
            selected: true,
          },
          create: {
            name: "total",
            tableId: ordersTable.id,
            ast: JSON.stringify(totalAst),
            type: "number",
            selected: true,
          },
        });
      }
    }

    // 4. Set table contexts (semantic descriptions) for all tables
    if (organisationsTable) {
      await prisma.table.update({
        where: { id: organisationsTable.id },
        data: { context: "Contains tenant/organization information. Each organization has its own users, products, orders, and reviews." },
      });
    }

    if (usersTable) {
      await prisma.table.update({
        where: { id: usersTable.id },
        data: { context: "Contains customer information including name, email, address, and location data. Each user belongs to an organization." },
      });
    }

    if (productsTable) {
      await prisma.table.update({
        where: { id: productsTable.id },
        data: { context: "Contains product catalog with title, category, price, and stock levels. Each product belongs to an organization." },
      });
    }

    if (ordersTable) {
      await prisma.table.update({
        where: { id: ordersTable.id },
        data: { context: "Contains order transactions with subtotal, tax, discount, and quantity. Links users to products they purchased." },
      });
    }

    if (reviewsTable) {
      await prisma.table.update({
        where: { id: reviewsTable.id },
        data: { context: "Contains product reviews with ratings (1-5) and comments written by users." },
      });
    }

    // 5. Set table conditions (row-level filtering) for multi-tenant tables
    const tablesWithOrgId = [
      ordersTable,
      usersTable,
      productsTable,
      reviewsTable,
    ].filter(Boolean) as NonNullable<typeof ordersTable>[];

    for (const table of tablesWithOrgId) {
      const orgIdColumn = table.columns.find(
        (c) => c.name === "organisation_id",
      );
      if (orgIdColumn) {
        await prisma.tableCondition.upsert({
          where: { tableId: table.id },
          update: {
            columnId: orgIdColumn.id,
            userContextFieldId: userContextField.id,
          },
          create: {
            tableId: table.id,
            columnId: orgIdColumn.id,
            userContextFieldId: userContextField.id,
          },
        });
      }
    }

    // 6. Enable all tables as QUERYABLE
    await prisma.table.updateMany({
      data: { access: "QUERYABLE" },
    });

    // 7. Sync augmentations to file
    await syncAugmentationsToFile();

    return NextResponse.json({
      success: true,
      message: "Demo database configured successfully",
      configured: {
        userContextField: userContextField.key,
        computedColumns: ordersTable ? ["orders.total"] : [],
        tableConditions: tablesWithOrgId.map((t) => t.name),
        tablesEnabled: await prisma.table.count(),
      },
    });
  } catch (error) {
    console.error("Failed to configure demo database:", error);
    return NextResponse.json(
      {
        error:
          error instanceof Error
            ? error.message
            : "Failed to configure demo database",
      },
      { status: 500 },
    );
  }
}
