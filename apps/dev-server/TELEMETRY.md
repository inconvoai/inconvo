# Telemetry & Privacy

Inconvo collects **minimal, anonymized usage metrics** to help improve the product. This document explains exactly what data is collected, what is NOT collected, and how to opt out.

## What Data is Collected

Inconvo tracks **4 types of aggregated, privacy-respecting metrics**:

### 1. Feature Usage (Action-Based)
Tracks specific actions users take within each feature, allowing us to understand which capabilities are being used:

**Conversations:**
- `conversation_created` - New conversation started
- `conversation_deleted` - Conversation removed
- `conversation_selected` - User switched to a conversation
- `message_sent` - User sent a message

**Schema Editor:**
- `schema_synced` - Database schema synchronized
- `table_selected` - User selected a table to configure
- `table_access_changed` - Table access level modified
- `table_prompt_edited` - Table description/context edited
- `column_selected` - Column enabled/disabled for queries
- `column_renamed` - Column display name changed
- `column_note_edited` - Column notes/documentation added
- `computed_column_created` - Custom computed column added
- `computed_column_updated` - Computed column modified
- `computed_column_deleted` - Computed column removed
- `relation_created` - Manual table relationship created
- `relation_updated` - Relationship modified
- `relation_deleted` - Relationship removed

**User Context:**
- `context_field_created` - New context field added
- `context_field_deleted` - Context field removed
- `table_condition_created` - Row-level security condition added
- `table_condition_deleted` - Security condition removed

**Example**: `feature_usage { feature: "schema_editor", action: "column_renamed", count: 1 }`

**Important**: While we track specific action types (like "column_renamed"), we never collect:
- WHAT column was renamed or the new name
- WHO performed the action
- WHEN it happened in relation to other actions (no session tracking)

We only see aggregate counts like "45 columns were renamed this week" across all users.

### 2. Query Performance (Anonymized)
- Success/failure counts
- Average query duration
- Generic error categories (validation vs execution)

**Example**: `query_performance { success: true, duration_ms: 150 }`

### 3. Response Performance (Anonymized)
- Success/failure status
- Response duration
- Response type category (text, table, chart)

**Example**: `response_performance { success: true, duration_ms: 2340, response_type: "table" }`

### 4. Error Summary (Sanitized)
- Generic error category only (database, llm, schema, network)
- Error count

**Example**: `error_summary { error_category: "database", count: 1 }`

## What is NOT Collected

We **never** collect:

- ❌ IP addresses or geolocation data
- ❌ Device identifiers or user agents
- ❌ Usernames, user IDs, or conversation identifiers
- ❌ Database table names, column names, or field keys
- ❌ Full error messages with sensitive data
- ❌ User queries or messages
- ❌ Database query results or content
- ❌ Schema details or structure
- ❌ Session recordings or pageview tracking
- ❌ Any personally identifiable information (PII)

**PostHog Privacy Configuration:**
We explicitly disable PostHog's automatic property collection including:
- No person profiles created (`person_profiles: never`)
- No automatic pageview/pageleave tracking
- No session recordings
- IP addresses and geolocation properties blocked via property denylist

## How to Opt Out

To completely disable telemetry, set the `DISABLE_TELEMETRY` environment variable:

```bash
export DISABLE_TELEMETRY=true
```

Or add it to your `.inconvo.env` file:

```bash
DISABLE_TELEMETRY=true
```

When telemetry is disabled:
- Zero events are sent to PostHog
- No network requests are made to analytics services
- The application functions normally without any degradation
- PostHog proxy routes are not configured

## Silent Failure

Telemetry is designed to **never impact your application**:
- If PostHog fails to initialize, the app continues normally
- If event tracking fails, errors are silently caught
- If the network is unavailable, tracking is skipped
- No console errors or warnings are logged for telemetry issues

## Data Retention

- Events are stored in PostHog for 90 days
- No long-term tracking or profiling
- Data is aggregated for insights, not individual tracking

## Open Source Commitment

This telemetry implementation follows the [1984.vc open source telemetry guidelines](https://1984.vc/docs/founders-handbook/eng/open-source-telemetry):

✅ Easy opt-out via environment variable
✅ No PII or sensitive data collection
✅ Aggregated metrics only
✅ Silent failure mode
✅ Full transparency documentation
✅ Privacy-first design

## Questions or Concerns?

If you have questions about telemetry or want to report privacy concerns:
- Open an issue on GitHub
- Review the telemetry code in `src/lib/telemetry.ts`
- All tracking code is open source and auditable

Thank you for helping us improve Inconvo while respecting your privacy.
