import type { SchemaResponse } from "../types/types";
import type { SQLCastExpressionAst } from "../types/querySchema";

export type ColumnConversion = {
  table: { name: string };
  column: string;
  ast: SQLCastExpressionAst;
  type?: string | null;
};

export function getSchemaColumnConversions(
  schema: SchemaResponse,
): ColumnConversion[] | undefined {
  const conversions: ColumnConversion[] = [];

  for (const table of schema.tables) {
    if (!table.columnConversions?.length) {
      continue;
    }

    for (const conversion of table.columnConversions) {
      conversions.push({
        table: { name: table.name },
        column: conversion.column,
        ast: conversion.ast,
        type: conversion.type,
      });
    }
  }

  return conversions.length > 0 ? conversions : undefined;
}
