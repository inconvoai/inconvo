import { Request, Response, Router } from "express";
import { authenticated } from "./middlewares";
import { QuerySchema } from "../types/querySchema";
import { ZodError } from "zod";
import { getPrismaClient } from "../prismaClient";
import { count } from "~/operations/count/index";
import { groupBy } from "~/operations/groupBy";
import { findMany } from "~/operations/findMany";
import { countByDateInterval } from "~/operations/countByDateInterval";
import { countByTemporalComponent } from "~/operations/countByTemporalComponent";
import { averageDurationBetweenTwoDates } from "~/operations/averageDurationBetweenTwoDates";
import { buildSchema } from "~/util/buildSchema";
import { aggregate } from "~/operations/aggregate/index";
import { countRelations } from "~/operations/countRelations/index";
import { findDistinct } from "~/operations/findDistinct/index";
import packageJson from "../../package.json";

export function inconvo() {
  const router = Router();
  router.use(authenticated);

  router.get("/", (req: Request, res: Response) => {
    try {
      const schema = buildSchema();
      res.json(schema);
    } catch (error) {
      console.log(error);
      res.status(500).json({ error: "Failed to fetch schema" });
    }
  });

  router.get("/version", (req: Request, res: Response) => {
    res.json({ version: packageJson.version });
  });

  router.post("/", async (req: Request, res: Response) => {
    try {
      const prisma = getPrismaClient();
      const parsedQuery = QuerySchema.parse(req.body);
      const { operation } = parsedQuery;

      if (operation === "findMany") {
        const response = await findMany(prisma, parsedQuery);
        return res.json(response);
      }

      if (operation === "findDistinct") {
        const response = await findDistinct(prisma, parsedQuery);
        return res.json(response);
      }

      if (operation === "count") {
        const response = await count(prisma, parsedQuery);
        return res.json(response);
      }

      if (operation === "countRelations") {
        const response = await countRelations(prisma, parsedQuery);
        return res.json(response);
      }

      if (operation === "aggregate") {
        const response = await aggregate(prisma, parsedQuery);
        return res.json(response);
      }

      if (operation === "groupBy") {
        const response = await groupBy(prisma, parsedQuery);
        return res.json(response);
      }

      if (operation === "countByDateInterval") {
        const response = await countByDateInterval(prisma, parsedQuery);
        return res.json(response);
      }

      if (operation === "countByTemporalComponent") {
        const response = await countByTemporalComponent(prisma, parsedQuery);
        return res.json(response);
      }

      if (operation === "averageDurationBetweenTwoDates") {
        const response = await averageDurationBetweenTwoDates(
          prisma,
          parsedQuery
        );
        return res.json(response);
      }

      return res.status(400).json({ error: "Invalid operation" });
    } catch (error) {
      if (error instanceof ZodError) {
        return res.status(400).json({ error: error.errors });
      }
      console.error(error);
      res.status(500).json({ error: "Failed to execute query" });
    }
  });

  return router;
}
