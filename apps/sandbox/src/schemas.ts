import { z } from "zod";

// Query schema for GET /datasets with optional context filtering and path navigation
export const datasetsQuerySchema = z.object({
  context: z.string().optional(), // userContextPath for filtering
  path: z.string().optional(), // subfolder path within the context (e.g., "folder1/folder2")
});

// Query schema for DELETE /datasets/:filename (Public API)
export const datasetsDeleteQuerySchema = z.object({
  context: z.string().min(1, "Context is required"),
});

// Query schema for DELETE /datasets (Admin API - delete by path)
export const datasetsDeleteByPathQuerySchema = z.object({
  path: z.string().min(1, "Path is required"),
});

// Body schema for POST /datasets (upload single file)
export const datasetsUploadBodySchema = z.object({
  userContextPath: z.string(), // Can be empty for root
  file: z.object({
    name: z.string().min(1),
    content: z.string().min(1), // base64 encoded
    contentType: z.string().optional(),
    notes: z.string().optional(),
  }),
});

// Body schema for POST /conversation-data (upload)
// Uses X-Org-Id and X-Agent-Id headers for bucket path construction
export const conversationDataUploadBodySchema = z.object({
  conversationId: z.string().min(1, "conversationId is required"),
  userContextPath: z.string(), // Can be empty for root
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
  userContextPath: z.string(), // For mounting datasets bucket (can be empty string for root)
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
