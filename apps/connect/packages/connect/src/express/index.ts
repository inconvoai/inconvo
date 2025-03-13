import { Request, Response, Router } from "express";
import { authenticated } from "./middlewares";
import { QuerySchema } from "../types/querySchema";
import { ZodError } from "zod";
import { getPrismaClient } from "../prismaClient";
import { count } from "~/operations/count/index";
import { groupBy } from "~/operations/groupBy";
import { findMany } from "~/operations/findMany";
import { aggregateByDateInterval } from "~/operations/aggregateByDateInterval";
import { countByTemporalComponent } from "~/operations/countByTemporalComponent";
import { averageDurationBetweenTwoDates } from "~/operations/averageDurationBetweenTwoDates";
import { buildSchema } from "~/util/buildSchema";
import { aggregate } from "~/operations/aggregate/index";
import { countRelations } from "~/operations/countRelations/index";
import { findDistinct } from "~/operations/findDistinct/index";
import packageJson from "../../package.json";

function safeJsonStringify(value: unknown): string {
  return JSON.stringify(value, (key, val) =>
    typeof val === "bigint" ? val.toString() : val
  );
}

export function inconvo() {
  const router = Router();
  router.use(authenticated);

  router.get("/", (req: Request, res: Response) => {
    try {
      const schema = buildSchema();
      res.setHeader("Content-Type", "application/json");
      res.send(safeJsonStringify(schema));
    } catch (error) {
      console.log(error);
      res.status(500).setHeader("Content-Type", "application/json");
      res.send(safeJsonStringify({ error: "Failed to fetch schema" }));
    }
  });

  router.get("/version", (req: Request, res: Response) => {
    res.setHeader("Content-Type", "application/json");
    res.send(safeJsonStringify({ version: packageJson.version }));
  });

  router.post("/", async (req: Request, res: Response) => {
    try {
      const prisma = getPrismaClient();
      const parsedQuery = QuerySchema.parse(req.body);
      const { operation } = parsedQuery;

      let response;

      if (operation === "findMany") {
        response = await findMany(prisma, parsedQuery);
      } else if (operation === "findDistinct") {
        response = await findDistinct(prisma, parsedQuery);
      } else if (operation === "count") {
        response = await count(prisma, parsedQuery);
      } else if (operation === "countRelations") {
        response = await countRelations(prisma, parsedQuery);
      } else if (operation === "aggregate") {
        response = await aggregate(prisma, parsedQuery);
      } else if (operation === "groupBy") {
        response = await groupBy(prisma, parsedQuery);
      } else if (operation === "aggregateByDateInterval") {
        response = await aggregateByDateInterval(prisma, parsedQuery);
      } else if (operation === "countByTemporalComponent") {
        response = await countByTemporalComponent(prisma, parsedQuery);
      } else if (operation === "averageDurationBetweenTwoDates") {
        response = await averageDurationBetweenTwoDates(prisma, parsedQuery);
      } else {
        return res
          .status(400)
          .setHeader("Content-Type", "application/json")
          .send(safeJsonStringify({ error: "Invalid operation" }));
      }

      res.setHeader("Content-Type", "application/json");
      return res.send(safeJsonStringify(response));
    } catch (error) {
      if (error instanceof ZodError) {
        return res
          .status(400)
          .setHeader("Content-Type", "application/json")
          .send(safeJsonStringify({ error: error.errors }));
      }
      console.error(error);
      res
        .status(500)
        .setHeader("Content-Type", "application/json")
        .send(safeJsonStringify({ error: "Failed to execute query" }));
    }
  });

  return router;
}
