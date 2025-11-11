import crypto from "crypto";
import { NextRequest, NextResponse } from "next/server";
import { QuerySchema } from "~/types/querySchema";
import { ZodError } from "zod";
import { generateHmac, generateMessage } from "~/util/hmac";
import { buildSchema } from "~/util/buildSchema";
import { aggregate } from "~/operations/aggregate";
import { getDb } from "~/dbConnection";
import { registerNonce } from "~/util/replayProtection";

const hexPattern = /^[0-9a-f]+$/i;

function authenticateRequest({
  method,
  requestUrl,
  signature,
  timestamp,
  random,
  body,
}: {
  method: "GET" | "POST";
  requestUrl: string;
  signature: string | null;
  timestamp: string | null;
  random: string | null;
  body?: object;
}) {
  if (!timestamp || !random || !signature) {
    return NextResponse.json({ error: "Invalid Request" }, { status: 400 });
  }

  const requestTimestamp = Number(timestamp);

  if (!Number.isFinite(requestTimestamp)) {
    return NextResponse.json({ message: "Unauthorized" }, { status: 401 });
  }

  const currentTimestamp = Math.floor(Date.now() / 1000);

  if (Math.abs(requestTimestamp - currentTimestamp) > 300) {
    return NextResponse.json(
      { message: "Unauthorized - timestamp is too old or too new" },
      { status: 401 }
    );
  }

  const message = generateMessage({
    method,
    body,
    requestUrl,
    timestamp,
    random,
  });

  const generatedSignature = generateHmac(message);
  const expectedLength = generatedSignature.length;

  if (signature.length !== expectedLength || !hexPattern.test(signature)) {
    return NextResponse.json({ message: "Unauthorized" }, { status: 401 });
  }

  const generatedBuffer = Buffer.from(generatedSignature, "hex");
  const receivedBuffer = Buffer.from(signature, "hex");

  if (!crypto.timingSafeEqual(generatedBuffer, receivedBuffer)) {
    return NextResponse.json({ message: "Unauthorized" }, { status: 401 });
  }

  const nonceAccepted = registerNonce(random, requestTimestamp, {
    nowSeconds: currentTimestamp,
  });

  if (!nonceAccepted) {
    return NextResponse.json(
      { message: "Unauthorized - replay detected" },
      { status: 401 }
    );
  }

  return null;
}

async function handleGetRequest(request: NextRequest) {
  try {
    const timestamp = request.headers.get("inconvo-timestamp");
    const random = request.headers.get("inconvo-random");
    const signature = request.headers.get("inconvo-signature");

    const authError = authenticateRequest({
      method: "GET",
      requestUrl: "/",
      signature,
      timestamp,
      random,
    });

    if (authError) {
      return authError;
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

    const authError = authenticateRequest({
      method: "POST",
      requestUrl: "/",
      signature,
      timestamp,
      random,
      body,
    });

    if (authError) {
      return authError;
    }

    const parsedQuery = QuerySchema.parse(body);
    const { operation } = parsedQuery;
    const db = await getDb();

    if (operation === "aggregate") {
      const response = await aggregate(db, parsedQuery);
      return NextResponse.json(response, { status: 200 });
    }

    return NextResponse.json({ error: "Invalid query" }, { status: 400 });
  } catch (error) {
    console.error("Error executing query", error);
    if (error instanceof ZodError) {
      return NextResponse.json({ error: error }, { status: 400 });
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
