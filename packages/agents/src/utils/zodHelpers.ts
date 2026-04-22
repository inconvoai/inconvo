import { z } from "zod";

export const stringArrayToZodEnum = (strings: string[]) => {
  if (strings.length === 0) {
    throw new Error("stringArrayToZodEnum requires at least one value");
  }
  return z.enum([strings[0]!, ...strings.slice(1)]);
};
