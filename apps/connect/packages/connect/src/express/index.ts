import { Request, Response, Router } from "express";
import { authenticated } from "~/express/middlewares";
import { QuerySchema } from "~/types/querySchema";
import { ZodError } from "zod";
import { buildSchema } from "~/util/buildSchema";
import { aggregate } from "~/operations/aggregate/index";
import { findMany } from "~/operations/findMany/index";
import { count } from "~/operations/count";
import { countRelations } from "~/operations/countRelations";
import { aggregateByDateInterval } from "~/operations/aggregateByDateInterval";
import { countByTemporalComponent } from "~/operations/countByTemporalComponent";
import { groupBy } from "~/operations/groupBy";
import { findDistinct } from "~/operations/findDistinct";
import { getDb } from "~/dbConnection";
import { countWithJoin } from "~/operations/countWithJoin";
import packageJson from "~/../../package.json";

function safeJsonStringify(value: unknown): string {
  return JSON.stringify(value, (key, val) =>
    typeof val === "bigint" ? val.toString() : val
  );
}

export async function inconvo() {
  const router = Router();
  router.use(authenticated);

  router.get("/", async (req: Request, res: Response) => {
    try {
      const schema = await buildSchema();
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
      const parsedQuery = QuerySchema.parse(req.body);
      const { operation } = parsedQuery;
      const db = await getDb();

      let response;
      switch (operation) {
        case "aggregate":
          response = await aggregate(db, parsedQuery);
          break;
        case "aggregateByDateInterval":
          response = await aggregateByDateInterval(db, parsedQuery);
          break;
        case "count":
          response = await count(db, parsedQuery);
          break;
        case "countWithJoin":
          response = await countWithJoin(db, parsedQuery);
          break;
        case "countByTemporalComponent":
          response = await countByTemporalComponent(db, parsedQuery);
          break;
        case "countRelations":
          response = await countRelations(db, parsedQuery);
          break;
        case "findDistinct":
          response = await findDistinct(db, parsedQuery);
          break;
        case "findMany":
          response = await findMany(db, parsedQuery);
          break;
        case "groupBy":
          response = await groupBy(db, parsedQuery);
          break;
        default:
          throw new Error("Invalid inconvo operation");
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
