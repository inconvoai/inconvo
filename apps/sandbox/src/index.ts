import { Hono } from "hono";
import type { Context, MiddlewareHandler } from "hono";
import { HTTPException } from "hono/http-exception";
import { getSandbox, proxyToSandbox, type Sandbox } from "@cloudflare/sandbox";
import { zValidator } from "@hono/zod-validator";
import { z } from "zod";
export { Sandbox } from "@cloudflare/sandbox";

const SANDBOX_ID_HEADER = "x-inconvo-message-id";

const ANALYZE_JSON_SCRIPT = `import pandas as pd
import json
import sys

def analyze_json_file(file_path):
    """
    Analyze a JSON file and display its structure using pandas.
    Returns the first 5 rows of the normalized DataFrame.
    """
    with open(file_path, 'r') as f:
        data = json.load(f)

    # Normalize JSON data - handle different structures
    if 'data' in data:
        df = pd.json_normalize(data['data'])
    else:
        df = pd.json_normalize(data if isinstance(data, list) else [data])

    # Print first 5 rows
    print(df.head(5).to_string())

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: python analyze_json.py <file_path>", file=sys.stderr)
        sys.exit(1)

    file_path = sys.argv[1]
    analyze_json_file(file_path)
`;

const fileFieldSchema = z.union([
  z.instanceof(File),
  z.array(z.instanceof(File)),
]);

const isUnsafeFileName = (name: string) =>
  name.includes("..") || name.includes("/") || name.includes("\\");

const ensureJsonExtension = (name: string) =>
  name.toLowerCase().endsWith(".json");

const toFileArray = (value?: File | File[]) =>
  value ? (Array.isArray(value) ? value : [value]) : [];

const uploadFormSchema = z
  .object({
    file: fileFieldSchema.optional(),
    files: fileFieldSchema.optional(),
    ["files[]"]: fileFieldSchema.optional(),
  })
  .transform((data) => {
    const files = [
      ...toFileArray(data.file),
      ...toFileArray(data.files),
      ...toFileArray(data["files[]"]),
    ];
    return { files };
  })
  .refine((data) => data.files.length > 0, {
    message: "At least one file upload is required.",
    path: ["files"],
  })
  .superRefine((data, ctx) => {
    data.files.forEach((file, index) => {
      const fileName = file.name.trim();
      if (isUnsafeFileName(fileName)) {
        ctx.addIssue({
          code: "custom",
          message:
            "File name must not contain directory paths or traversal sequences.",
          path: ["files", index],
        });
      }
      if (!ensureJsonExtension(fileName)) {
        ctx.addIssue({
          code: "custom",
          message: "Uploaded file must be a .json file.",
          path: ["files", index],
        });
      }
    });
  });

const describeRequestSchema = z
  .object({
    files: z
      .array(z.string().trim())
      .min(1, "At least one file name is required."),
  })
  .superRefine((data, ctx) => {
    data.files.forEach((fileName, index) => {
      if (isUnsafeFileName(fileName)) {
        ctx.addIssue({
          code: "custom",
          message:
            "File name must not contain directory paths or traversal sequences.",
          path: ["files", index],
        });
      }
      if (!ensureJsonExtension(fileName)) {
        ctx.addIssue({
          code: "custom",
          message: "Only .json files can be described.",
          path: ["files", index],
        });
      }
    });
  });

const executeRequestSchema = z.object({
  code: z
    .string()
    .trim()
    .min(1, "Code is required.")
    .max(20_000, "Code must be 20,000 characters or less."),
});

const workspacePathFor = (fileName: string) => `/workspace/${fileName}`;

type AppContext = Context<{ Bindings: Env }>;

const app = new Hono<{ Bindings: Env }>();

const requireApiKey: MiddlewareHandler = async (c: AppContext, next) => {
  const authHeaderRaw = c.req.header("authorization");

  if (typeof authHeaderRaw !== "string") {
    return c.json({ error: "Unauthorized" }, 401);
  }

  const authHeader = authHeaderRaw as string;

  if (!authHeader.startsWith("Bearer ")) {
    return c.json({ error: "Unauthorized" }, 401);
  }

  const providedKey = authHeader.slice("Bearer ".length).trim();

  if (!providedKey || providedKey !== c.env.INTERNAL_API_KEY) {
    return c.json({ error: "Unauthorized" }, 401);
  }

  return next();
};

app.use("*", requireApiKey);
// Apparently not needed, saw it was removed for the example here
// https://github.com/cloudflare/sandbox-sdk/pull/128/files
// app.use("*", async (c, next) => {
//   const proxyResponse = await proxyToSandbox(c.req.raw, c.env);
//   if (proxyResponse) {
//     return proxyResponse;
//   }
//   return next();
// });

const getSandboxIdFromRequest = (c: AppContext) => {
  const sandboxId = c.req.header(SANDBOX_ID_HEADER)?.trim();
  if (!sandboxId) {
    throw new HTTPException(400, {
      message: `Missing ${SANDBOX_ID_HEADER} header.`,
    });
  }
  return sandboxId;
};

const getSandboxForRequest = (c: AppContext) =>
  getSandbox(c.env.Sandbox, getSandboxIdFromRequest(c));

app.post(
  "/sandbox/files",
  zValidator("form", uploadFormSchema, (result, c) => {
    if (!result.success) {
      const message =
        result.error.issues[0]?.message ?? "Invalid upload request.";
      return c.json({ error: message }, 400);
    }
  }),
  async (c) => {
    const { files } = c.req.valid("form");
    const sandbox = getSandboxForRequest(c);
    // wait 1 second here to make sure the sandbox is good to go
    await new Promise((r) => setTimeout(r, 1000));

    const storedFiles = await Promise.all(
      files.map(async (file) => {
        const content = await file.text();
        const targetPath = workspacePathFor(file.name.trim());
        await sandbox.writeFile(targetPath, content);
        return {
          name: file.name.trim(),
          path: targetPath,
          size: file.size,
        };
      }),
    );

    return c.json({ files: storedFiles });
  },
);

app.post(
  "/sandbox/files/describe",
  zValidator("json", describeRequestSchema, (result, c) => {
    if (!result.success) {
      const message =
        result.error.issues[0]?.message ?? "Invalid describe request.";
      return c.json({ error: message }, 400);
    }
  }),
  async (c) => {
    const { files } = c.req.valid("json");
    const sandbox = getSandboxForRequest(c);
    await sandbox.writeFile("/workspace/analyze_json.py", ANALYZE_JSON_SCRIPT);
    // First, read and parse all files in parallel
    const fileData = await Promise.all(
      files.map(async (fileName) => {
        const trimmedName = fileName.trim();
        const targetPath = workspacePathFor(trimmedName);

        try {
          const readResult = await sandbox.readFile(targetPath, {
            encoding: "utf-8",
          });

          if (!readResult.success) {
            return {
              name: trimmedName,
              targetPath,
              sql: "",
              error: `Failed to read file: ${trimmedName}`,
              success: false,
            };
          }

          const fileContent = readResult.content ?? "";
          let fileSQL = "";
          try {
            const parsed = JSON.parse(fileContent);
            const sql = parsed?.query?.sql;
            if (typeof sql === "string") {
              fileSQL = sql;
            }
          } catch (error) {
            console.error(`Failed to extract SQL from ${trimmedName}:`, error);
          }

          return {
            name: trimmedName,
            targetPath,
            sql: fileSQL,
            error: "",
            success: true,
          };
        } catch (error) {
          const message =
            error instanceof Error ? error.message : "Unknown error";
          console.error(`Describe failed for ${trimmedName}:`, error);
          return {
            name: trimmedName,
            targetPath,
            sql: "",
            error: message,
            success: false,
          };
        }
      }),
    );

    // Then, run all exec commands in parallel
    const fileDescriptions = await Promise.all(
      fileData.map(async (file) => {
        if (!file.success) {
          return {
            name: file.name,
            sql: file.sql,
            dataSummary: "",
            error: file.error,
            success: false,
          };
        }

        try {
          const structure = await sandbox.exec(
            `python3 /workspace/analyze_json.py ${JSON.stringify(
              file.targetPath,
            )}`,
          );

          return {
            name: file.name,
            sql: file.sql,
            dataSummary: structure.stdout,
            error: structure.stderr,
            success: structure.success,
          };
        } catch (error) {
          const message =
            error instanceof Error ? error.message : "Unknown error";
          console.error(`Exec failed for ${file.name}:`, error);
          return {
            name: file.name,
            sql: file.sql,
            dataSummary: "",
            error: message,
            success: false,
          };
        }
      }),
    );

    return c.json({ files: fileDescriptions });
  },
);

app.post(
  "/sandbox/execute",
  zValidator("json", executeRequestSchema, (result, c) => {
    if (!result.success) {
      const message =
        result.error.issues[0]?.message ?? "Invalid execute request.";
      return c.json({ error: message }, 400);
    }
  }),
  async (c) => {
    const { code } = c.req.valid("json");
    const sandbox = getSandboxForRequest(c);

    await sandbox.writeFile("/workspace/analyze.py", code);
    const result = await sandbox.exec("python3 /workspace/analyze.py");

    return c.json({
      output: result.stdout,
      error: result.stderr,
      exitCode: result.exitCode,
      success: result.success,
    });
  },
);

app.delete("/sandbox", async (c) => {
  const sandbox = getSandboxForRequest(c);
  try {
    await sandbox.destroy();
    return c.json({ success: true });
  } catch (error) {
    console.error("Failed to destroy sandbox:", error);
    const message = error instanceof Error ? error.message : "Unknown error";
    return c.json({ error: message }, 500);
  }
});

app.notFound((c) => c.json({ message: "Not Found!" }, 404));

app.onError((err, c) => {
  console.error("Unhandled error:", err);
  if (err instanceof HTTPException) {
    return err.getResponse();
  }
  const message = err instanceof Error ? err.message : "Unknown error";
  return c.json({ error: message }, 500);
});

export default app;
