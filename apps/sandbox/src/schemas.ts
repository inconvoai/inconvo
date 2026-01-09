import { z } from "zod";

// Query schema for GET /datasets with optional context filtering and path navigation
export const datasetsQuerySchema = z.object({
  context: z.string().optional(), // requestContextPath for filtering
  path: z.string().optional(), // subfolder path within the context (e.g., "folder1/folder2")
});

// Body schema for DELETE /datasets
export const datasetsDeleteBodySchema = z.object({
  paths: z
    .array(z.string().trim().min(1))
    .min(1, "At least one path is required."),
});

// Body schema for POST /datasets (upload)
export const datasetsUploadBodySchema = z.object({
  requestContextPath: z.string(), // Can be empty for root
  files: z
    .array(
      z.object({
        name: z.string().min(1),
        content: z.string().min(1), // base64 encoded
        contentType: z.string().optional(),
        notes: z.string().optional(),
      }),
    )
    .min(1, "At least one file is required"),
});

// Body schema for POST /conversation-data (upload)
// Uses X-Org-Id and X-Agent-Id headers for bucket path construction
export const conversationDataUploadBodySchema = z.object({
  conversationId: z.string().min(1, "conversationId is required"),
  requestContextPath: z.string(), // Can be empty for root
  files: z
    .array(
      z.object({
        name: z.string().min(1),
        content: z.string().min(1), // base64 encoded
        contentType: z.string().optional(),
      }),
    )
    .min(1, "At least one file is required"),
});

// Body schema for sandbox endpoints
// Uses X-Org-Id and X-Agent-Id headers for bucket path construction
export const sandboxParamsSchema = z.object({
  conversationId: z.string().min(1, "conversationId is required"),
  runId: z.string().min(1, "runId is required"), // Scopes the sandbox instance per run/message
  requestContextPath: z.string(), // For mounting datasets bucket (can be empty string for root)
});

export type SandboxParams = z.infer<typeof sandboxParamsSchema>;

// Body schema for POST /sandbox/execute
export const executeBodySchema = sandboxParamsSchema.extend({
  code: z
    .string()
    .trim()
    .min(1, "Code is required.")
    .max(20_000, "Code must be 20,000 characters or less."),
});
