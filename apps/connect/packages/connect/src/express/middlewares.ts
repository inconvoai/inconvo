import { NextFunction, Request, Response } from "express";
import { generateHmac, generateMessage } from "~/util/hmac";

export function authenticated(req: Request, res: Response, next: NextFunction) {
  const signature = req.headers["inconvo-signature"] as string | undefined;
  const timestamp = req.headers["inconvo-timestamp"] as string | undefined;
  const random = req.headers["inconvo-random"] as string | undefined;

  if (!signature || !timestamp || !random) {
    console.error("Invalid Request");
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

  if (Math.abs(requestTimestamp - currentTimestamp) > 300) {
    console.error("Unauthorized - timestamp is too old or too new");
    res
      .status(401)
      .json({ message: "Unauthorized - timestamp is too old or too new" });
    return;
  }

  if (generatedSignature === signature) {
    next();
  } else {
    console.error("Unauthorized");
    res.status(401).json({ message: "Unauthorized" });
  }
}
