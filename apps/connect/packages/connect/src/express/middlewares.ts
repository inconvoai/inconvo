import crypto from "crypto";
import { NextFunction, Request, Response } from "express";
import { generateHmac, generateMessage } from "~/util/hmac";
import { logger } from "~/util/logger";
import { registerNonce } from "~/util/replayProtection";

export function authenticated(req: Request, res: Response, next: NextFunction) {
  const signature = req.headers["inconvo-signature"] as string | undefined;
  const timestamp = req.headers["inconvo-timestamp"] as string | undefined;
  const random = req.headers["inconvo-random"] as string | undefined;

  if (!signature || !timestamp || !random) {
    logger.error({
      hasSignature: !!signature,
      hasTimestamp: !!timestamp,
      hasRandom: !!random
    }, "Auth - Invalid Request - Missing required headers");
    res.status(401).json({ message: "Invalid Request" });
    return;
  }

  const message = generateMessage({
    method: req.method,
    body: req.body,
    requestUrl: req.url,
    timestamp,
    random,
  });

  const generatedSignature = generateHmac(message);
  const requestTimestamp = Number(timestamp);
  const currentTimestamp = Math.floor(Date.now() / 1000);

  if (!Number.isFinite(requestTimestamp)) {
    logger.error(
      { timestamp },
      "Auth - Unauthorized - Invalid timestamp value"
    );
    res.status(401).json({ message: "Unauthorized" });
    return;
  }

  if (Math.abs(requestTimestamp - currentTimestamp) > 300) {
    const timeDiff = requestTimestamp - currentTimestamp;
    logger.error({ timeDiff, maxDiff: 300 }, "Auth - Unauthorized - timestamp out of range");
    res
      .status(401)
      .json({ message: "Unauthorized - timestamp is too old or too new" });
    return;
  }

  const expectedLength = generatedSignature.length;
  const hexPattern = /^[0-9a-f]+$/i;

  if (signature.length !== expectedLength || !hexPattern.test(signature)) {
    logger.error(
      { signatureLength: signature.length, expectedLength },
      "Auth - Unauthorized - Invalid signature format"
    );
    res.status(401).json({ message: "Unauthorized" });
    return;
  }

  const generatedBuffer = Buffer.from(generatedSignature, "hex");
  const receivedBuffer = Buffer.from(signature, "hex");

  if (!crypto.timingSafeEqual(generatedBuffer, receivedBuffer)) {
    logger.error("Auth - Unauthorized - Invalid signature");
    res.status(401).json({ message: "Unauthorized" });
    return;
  }

  const nonceAccepted = registerNonce(random, requestTimestamp, {
    nowSeconds: currentTimestamp,
  });

  if (!nonceAccepted) {
    logger.error(
      { nonce: random },
      "Auth - Unauthorized - Replay detected"
    );
    res.status(401).json({ message: "Unauthorized - replay detected" });
    return;
  }

  next();
}
