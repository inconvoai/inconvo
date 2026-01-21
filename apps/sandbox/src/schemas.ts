import { z } from "zod";

// =============================================================================
// Admin endpoints (folder navigation)
// =============================================================================

// Query schema for GET /datasets (admin - folder navigation)
export const datasetsListQuerySchema = z.object({
  path: z.string().optional(), // subfolder path for navigation (e.g., "userIdentifier/user_123")
});

// Query schema for DELETE /datasets (Admin API - delete by path)
export const datasetsDeleteByPathQuerySchema = z.object({
  path: z.string().min(1, "Path is required"),
});

// =============================================================================
// User-scoped endpoints (identifiers in URL path)
// =============================================================================

// No query schemas needed - userIdentifier comes from URL path param

// =============================================================================
// Context-scoped endpoints (identifiers in URL path)
// =============================================================================

// No query schemas needed - contextKey and contextValue come from URL path params

// =============================================================================
// Conversation data endpoints
// =============================================================================

// Body schema for POST /conversation-data (upload)
// Uses X-Org-Id and X-Agent-Id headers for bucket path construction
export const conversationDataUploadBodySchema = z.object({
  conversationId: z.string().min(1, "conversationId is required"),
  userIdentifier: z.string().min(1, "userIdentifier is required"),
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

// =============================================================================
// Sandbox endpoints
// =============================================================================

// Body schema for POST /sandbox (initialize sandbox)
// Uses X-Org-Id and X-Agent-Id headers for bucket path construction
// Sandbox internally determines what to mount based on userIdentifier and userContext
export const sandboxParamsSchema = z.object({
  conversationId: z.string().min(1, "conversationId is required"),
  runId: z.string().min(1, "runId is required"), // Scopes the sandbox instance per run/message
  userIdentifier: z.string().min(1, "userIdentifier is required"),
  userContext: z.record(z.string(), z.union([z.string(), z.number()])).optional(),
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
