import { Request, Response, Router } from "express";
import { authenticated } from "./middlewares";
import { QuerySchema } from "../types/querySchema";
import { ZodError } from "zod";
import { db } from "~/dbConnection";
import { buildSchema } from "~/util/buildSchema";
import { aggregate } from "~/operations/aggregate/index";
import packageJson from "../../package.json";

function safeJsonStringify(value: unknown): string {
  return JSON.stringify(value, (key, val) =>
    typeof val === "bigint" ? val.toString() : val
  );
}

export function inconvo() {
  const router = Router();
  // router.use(authenticated);

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
      const parsedQuery = QuerySchema.parse(req.body);
      const { operation } = parsedQuery;

      let response;

      if (operation === "aggregate") {
        response = await aggregate(parsedQuery);
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
