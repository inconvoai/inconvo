import { PostHog } from 'posthog-node';

type FeatureType = 'conversations' | 'schema_editor' | 'user_context';
type ErrorCategory = 'database' | 'llm' | 'schema' | 'network';
type ResponseType = 'text' | 'table' | 'chart';
type ErrorType = 'validation' | 'execution';

// Specific action types for each feature
type ConversationAction = 'created' | 'deleted' | 'selected' | 'message_sent';
type SchemaEditorAction =
  | 'schema_synced'
  | 'table_selected'
  | 'table_access_changed'
  | 'table_prompt_edited'
  | 'column_selected'
  | 'column_renamed'
  | 'column_note_edited'
  | 'computed_column_created'
  | 'computed_column_updated'
  | 'computed_column_deleted'
  | 'relation_created'
  | 'relation_updated'
  | 'relation_deleted';
type UserContextAction = 'context_field_created' | 'context_field_deleted' | 'table_condition_created' | 'table_condition_deleted';

interface FeatureUsageData {
  action: ConversationAction | SchemaEditorAction | UserContextAction;
  count?: number; // Optional count for aggregated events
  session_hash?: string;
}

interface QueryPerformanceData {
  success: boolean;
  duration_ms: number;
  error_type?: ErrorType;
}

interface ResponsePerformanceData {
  success: boolean;
  duration_ms: number;
  response_type?: ResponseType;
}

interface ErrorSummaryData {
  error_category: ErrorCategory;
  count: number;
}

// Check if telemetry is disabled
const isTelemetryDisabled = (): boolean => {
  return process.env.DISABLE_TELEMETRY === 'true';
};

// Sanitize error messages to remove PII
const sanitizeErrorMessage = (error: unknown): string => {
  if (!error) return 'unknown';

  const errorStr = error instanceof Error ? error.message : String(error);

  // Map to generic categories
  if (errorStr.toLowerCase().includes('database') || errorStr.toLowerCase().includes('sql')) {
    return 'database';
  }
  if (errorStr.toLowerCase().includes('llm') || errorStr.toLowerCase().includes('model')) {
    return 'llm';
  }
  if (errorStr.toLowerCase().includes('schema') || errorStr.toLowerCase().includes('table')) {
    return 'schema';
  }
  if (errorStr.toLowerCase().includes('network') || errorStr.toLowerCase().includes('fetch')) {
    return 'network';
  }

  return 'unknown';
};

// Safe wrapper for PostHog calls
const safeCapture = (
  client: PostHog | null,
  eventName: string,
  properties: Record<string, unknown>
): void => {
  if (isTelemetryDisabled()) return;
  if (!client) return;

  try {
    client.capture({
      distinctId: 'anonymous',
      event: eventName,
      properties,
    });
  } catch (error) {
    // Silent failure - never impact app functionality
    // Intentionally not logging to avoid noise
  }
};

// Track aggregated feature usage
export const trackFeatureUsage = (
  client: PostHog | null,
  feature: FeatureType,
  data: FeatureUsageData
): void => {
  if (isTelemetryDisabled()) return;

  safeCapture(client, 'feature_usage', {
    feature,
    action: data.action,
    count: data.count || 1,
    session_hash: data.session_hash || 'anonymous',
  });
};

// Track query performance without operation details
export const trackQueryPerformance = (
  client: PostHog | null,
  data: QueryPerformanceData
): void => {
  if (isTelemetryDisabled()) return;

  safeCapture(client, 'query_performance', {
    success: data.success,
    duration_ms: data.duration_ms,
    error_type: data.error_type,
  });
};

// Track response performance without PII
export const trackResponsePerformance = (
  client: PostHog | null,
  data: ResponsePerformanceData
): void => {
  if (isTelemetryDisabled()) return;

  safeCapture(client, 'response_performance', {
    success: data.success,
    duration_ms: data.duration_ms,
    response_type: data.response_type,
  });
};

// Track error summary without messages
export const trackErrorSummary = (
  client: PostHog | null,
  error: unknown,
  count: number = 1
): void => {
  if (isTelemetryDisabled()) return;

  const category = sanitizeErrorMessage(error) as ErrorCategory;

  safeCapture(client, 'error_summary', {
    error_category: category,
    count,
  });
};

// Client-side helpers that work without PostHog client
export const trackFeatureUsageClient = (
  feature: FeatureType,
  data: FeatureUsageData
): void => {
  if (isTelemetryDisabled()) return;
  if (typeof window === 'undefined') return;

  try {
    // Use window.posthog if available
    const posthog = (window as any).posthog;
    if (!posthog) return;

    posthog.capture('feature_usage', {
      feature,
      action: data.action,
      count: data.count || 1,
      session_hash: data.session_hash || 'anonymous',
    });
  } catch (error) {
    // Silent failure
  }
};
