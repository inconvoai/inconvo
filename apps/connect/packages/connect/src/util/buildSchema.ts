import { Prisma } from "@prisma/client";
import { SchemaResponse } from "~/types/types";

export function buildSchema(): SchemaResponse {
  const schema: SchemaResponse = {
    tables: Prisma.dmmf.datamodel.models.map((model) => ({
      name: model.name,
      columns: model.fields
        .filter((field: { kind: string }) => field.kind !== "object")
        .map((field) => {
          const column = {
            name: field.name,
            type: field.type,
          };
          return column;
        }),
      relations: model.fields
        .filter((field: { kind: string }) => field.kind === "object")
        .map((field) => {
          const relation = {
            name: field.name,
            isList: field.isList,
            targetTable: field.type,
            relationName: field.relationName,
            sourceColumns: field.relationFromFields as string[] | undefined,
            targetColumns: field.relationToFields as string[] | undefined,
          };
          return relation;
        }),
    })),
  };

  return schema;
}
