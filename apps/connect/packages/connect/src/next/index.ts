import { NextRequest, NextResponse } from "next/server";
import { QuerySchema } from "~/types/querySchema";
import { ZodError } from "zod";
import { generateHmac, generateMessage } from "~/util/hmac";
import { buildSchema } from "~/util/buildSchema";
import { aggregate } from "~/operations/aggregate";
import { getDb } from "~/dbConnection";

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
    const db = getDb();

    if (operation === "aggregate") {
      const response = await aggregate(db, parsedQuery);
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
