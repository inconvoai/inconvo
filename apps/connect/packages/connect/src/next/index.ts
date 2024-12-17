import assert from "assert";
import { NextRequest, NextResponse } from "next/server";
import { getPrismaClient } from "../prismaClient";
import { QuerySchema } from "~/types/querySchema";
import { ZodError } from "zod";
import { generateHmac, generateMessage } from "~/util/hmac";
import { findMany } from "~/operations/findMany";
import { groupBy } from "~/operations/groupBy";
import { countByDateInterval } from "~/operations/countByDateInterval";
import { countByTemporalComponent } from "~/operations/countByTemporalComponent";
import { count } from "~/operations/count";
import { buildSchema } from "~/util/buildSchema";
import { averageDurationBetweenTwoDates } from "~/operations/averageDurationBetweenTwoDates";
import { aggregate } from "~/operations/aggregate";
import { countRelations } from "~/operations/countRelations";
import { findDistinct } from "~/operations/findDistinct";

const SECRET_KEY = process.env.INCONVO_SECRET_KEY;
assert(SECRET_KEY, "Inconvo secret key is not set");
const INCONVO_DATABASE_URL = process.env.INCONVO_DATABASE_URL;
assert(INCONVO_DATABASE_URL, "Inconvo database URL is not set");

async function handleGetRequest(request: NextRequest) {
  try {
    const timestamp = request.headers.get("inconvo-timestamp");
    const random = request.headers.get("inconvo-random");
    const signature = request.headers.get("inconvo-signature");

    if (!timestamp || !random || !signature) {
      return NextResponse.json({ error: "Invalid Request" }, { status: 400 });
    }

    const message = generateMessage({
      method: "GET",
      requestUrl: "/",
      timestamp,
      random,
    });

    const generatedSignature = generateHmac(message);
    const requestTimestamp = Number(timestamp);
    const currentTimestamp = Math.floor(Date.now() / 1000);

    if (Math.abs(requestTimestamp - currentTimestamp) > 300) {
      return NextResponse.json(
        { message: "Unauthorized - timestamp is too old or too new" },
        { status: 401 }
      );
    }

    if (generatedSignature !== signature) {
      return NextResponse.json({ message: "Unauthorized" }, { status: 401 });
    }

    const schema = buildSchema();
    return NextResponse.json(schema, { status: 200 });
  } catch (error) {
    console.error("Error fetching schema", error);
    return NextResponse.json(
      { error: "Failed to fetch schema" },
      { status: 500 }
    );
  }
}

async function handlePostRequest(request: NextRequest) {
  try {
    const prisma = getPrismaClient();
    const timestamp = request.headers.get("inconvo-timestamp");
    const random = request.headers.get("inconvo-random");
    const signature = request.headers.get("inconvo-signature");
    const body = await request.json();

    if (!timestamp || !random || !signature) {
      return NextResponse.json({ error: "Invalid Request" }, { status: 400 });
    }

    const message = generateMessage({
      method: "POST",
      body,
      requestUrl: "/",
      timestamp,
      random,
    });

    const generatedSignature = generateHmac(message);
    const requestTimestamp = Number(timestamp);
    const currentTimestamp = Math.floor(Date.now() / 1000);

    if (Math.abs(requestTimestamp - currentTimestamp) > 300) {
      return NextResponse.json(
        { message: "Unauthorized - timestamp is too old or too new" },
        { status: 401 }
      );
    }

    if (generatedSignature !== signature) {
      return NextResponse.json({ message: "Unauthorized" }, { status: 401 });
    }

    const parsedQuery = QuerySchema.parse(body);
    const { operation } = parsedQuery;

    if (operation === "findMany") {
      const response = await findMany(prisma, parsedQuery);
      return NextResponse.json(response, { status: 200 });
    }

    if (operation === "findDistinct") {
      const response = await findDistinct(prisma, parsedQuery);
      return NextResponse.json(response, { status: 200 });
    }

    if (operation === "count") {
      const response = await count(prisma, parsedQuery);
      return NextResponse.json(response, { status: 200 });
    }

    if (operation === "countRelations") {
      const response = await countRelations(prisma, parsedQuery);
      return NextResponse.json(response, { status: 200 });
    }

    if (operation === "aggregate") {
      const response = await aggregate(prisma, parsedQuery);
      return NextResponse.json(response, { status: 200 });
    }

    if (operation === "groupBy") {
      const response = await groupBy(prisma, parsedQuery);
      return NextResponse.json(response, { status: 200 });
    }

    if (operation === "averageDurationBetweenTwoDates") {
      const response = await averageDurationBetweenTwoDates(
        prisma,
        parsedQuery
      );
      return NextResponse.json(response, { status: 200 });
    }

    if (operation === "countByDateInterval") {
      const response = await countByDateInterval(prisma, parsedQuery);
      return NextResponse.json(response, { status: 200 });
    }

    if (operation === "countByTemporalComponent") {
      const response = await countByTemporalComponent(prisma, parsedQuery);
      return NextResponse.json(response, { status: 200 });
    }

    return NextResponse.json({ error: "Invalid query" }, { status: 400 });
  } catch (error) {
    console.error("Error executing query", error);
    if (error instanceof ZodError) {
      return NextResponse.json({ error: error.errors }, { status: 400 });
    }
    return NextResponse.json(
      { error: "Failed to execute query" },
      { status: 500 }
    );
  }
}

export function serve() {
  return {
    GET: (request: NextRequest) => handleGetRequest(request),
    POST: (request: NextRequest) => handlePostRequest(request),
  };
}
