import { Prisma } from "@prisma/client";

export function isPrismaFieldNullable(
  field: string,
  prismaModel: string
): boolean {
  return (
    Prisma.dmmf.datamodel.models
      .find((model) => model.name === prismaModel)
      ?.fields.find((fieldObj) => fieldObj.name === field)?.isRequired === false
  );
}
