import { z } from "zod";

export const stringArrayToZodEnum = (strings: string[]) =>
  z.enum([strings[0]!, ...strings.slice(1)]);
