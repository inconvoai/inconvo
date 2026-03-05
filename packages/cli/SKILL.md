---
name: inconvo-cli
description: Use when updating an Inconvo agent semantic model or user-context from code context. Covers the full CLI mutation workflow — tables, columns, relations, conditions, policies, virtual tables, computed columns, and user-context — using `inconvo model` and `inconvo connection` commands.
---

# Inconvo CLI — Semantic Model Skill

Use this skill when you need to update an agent semantic model or user-context
from code context. This repository uses a CLI-only mutation workflow.

## Core Rules

1. Never edit `.inconvo/**` by hand.
2. Never use local YAML as mutation input.
3. Perform every remote change via `inconvo model <group> <command>`.
4. Let the CLI auto-sync snapshots after successful mutations.
5. Use `--dry-run` on mutation commands when planning changes.
6. Use `inconvo model pull` only for explicit refresh or recovery.

## Authentication

The CLI resolves credentials in this priority order (highest first):

1. `--api-key` / `--api-base-url` flags on the command
2. `INCONVO_API_KEY` / `INCONVO_API_BASE_URL` environment variables
3. `.inconvo/config.yaml` in the repo root (gitignored, never committed)

Set up local credentials once with:

```bash
npx inconvo@latest config set
# or non-interactively:
npx inconvo@latest config set --api-key <key> --api-base-url <url>
```

View current stored credentials (key is masked):

```bash
npx inconvo@latest config view
```

The config file lives at `.inconvo/config.yaml` and is automatically excluded from git. Do not commit it.

## Required Inputs

- `agentId` for all mutations (`--agent`).
- `connectionId` for schema mutations (`--connection`).
- Target identifiers from code context (prefer IDs, then exact names).

## Snapshot Layout (Canonical)

After pull/sync, snapshots are deduplicated by connection and referenced from
agents:

```text
.inconvo/
  agents/
    .slug-map.yaml
    <agent-slug>/
      agent.yaml
      user-context.yaml
      shareable-connections.yaml
      connections/
        <connection-slug>/
          connection.yaml      # contains snapshotPath: .inconvo/connections/<connection-slug>
  connections/
    .slug-map.yaml
    <connection-slug>/
      connection.yaml
      tables/
        .slug-map.yaml
        <table-slug>.yaml
```

Notes:
- `agents/*/connections/*/connection.yaml` is a reference, not a duplicate table snapshot.
- Slug maps (`.slug-map.yaml`) keep stable, readable folder/file names without requiring IDs in paths.
- All files under `.inconvo/` are auto-generated — never edit them directly.

## Standard Workflow

1. Read code context to identify the intended mutation.
2. Map the mutation to the corresponding CLI command.
3. Optional: run with `--dry-run` first to inspect payload.
4. Execute one CLI mutation command per logical change.
5. Prefer `--json` for automation/agent workflows.
6. Verify generated files under `.inconvo/` changed as expected.
7. If needed, run `inconvo model pull --agent <agentId>` to recover sync.

## Command Prefix

- Prefer `npx inconvo@latest` in docs, automation, and agent workflows.
- For local development, use `pnpm --dir oss/packages/cli exec tsx src/index.ts`.

## Command Discovery

```bash
npx inconvo@latest config --help
npx inconvo@latest model --help
npx inconvo@latest model agent list --json
npx inconvo@latest model <group> --help
npx inconvo@latest model action schema --json
npx inconvo@latest connection --help
```

## Mutation Mapping (Code -> CLI)

Use this mapping when code references schema/user-context procedures.

| Code Mutation Surface | CLI Command Group |
|---|---|
| `schema.updateTable` | `model table set-access` |
| `schema.updateTableContext` | `model table set-context` |
| `schema.updateColumn` | `model column update` |
| `schema.createColumnUnit` | `model column set-unit` |
| `schema.createColumnAugmentation` (conversion) | `model column conversion create` |
| `schema.updateColumnAugmentation` (conversion) | `model column conversion update` |
| `schema.deleteColumnAugmentation` (conversion) | `model column conversion delete` |
| `schema.createColumnAugmentation` (static enum) | `model column enum create-static` |
| `schema.createColumnAugmentation` (dynamic enum) | `model column enum create-dynamic` |
| `schema.updateColumnAugmentation` (enum) | `model column enum update` |
| `schema.deleteColumnAugmentation` (enum) | `model column enum delete` |
| `schema.getColumnDistinctValues` | `model column enum autofill` |
| `schema.createComputedColumn` | `model computed create` |
| `schema.updateComputedColumn` | `model computed update`, `model computed set-unit` |
| `schema.deleteComputedColumn` | `model computed delete` |
| `schema.updateRelation` | `model relation toggle` |
| `schema.createManualRelation` | `model relation manual create` |
| `schema.updateManualRelation` | `model relation manual update` |
| `schema.deleteManualRelation` | `model relation manual delete` |
| `schema.upsertCondition` | `model condition set` |
| `schema.deleteCondition` | `model condition clear` |
| `schema.upsertTableAccessPolicy` | `model policy set` |
| `schema.deleteTableAccessPolicy` | `model policy clear` |
| `schema.validateVirtualTableSql` | `model virtual validate-sql` |
| `schema.createVirtualTable` | `model virtual create` |
| `schema.updateVirtualTableSql` | `model virtual update-sql` |
| `schema.refreshVirtualTableColumns` | `model virtual refresh-columns` |
| `schema.deleteVirtualTable` | `model virtual delete` |
| `userContext.addField` | `model user-context add-field` |
| `userContext.deleteField` | `model user-context delete-field` |
| `userContext.setStatus` | `model user-context set-status` |

## State Semantics (Important)

### Table access states

- `QUERYABLE`: Table can be selected as a primary query target. Use for tables you want users/agents to ask about directly.
- `JOINABLE`: Table cannot be a primary target, but can be traversed through relations from reachable tables.
- `OFF`: Table is excluded from query planning and traversal.

### Reachability rule

- `JOINABLE` only helps if there is at least one reachable path from a `QUERYABLE` table (possibly through other `JOINABLE` tables).
- If all incoming paths come only from `OFF` tables, the `JOINABLE` table is effectively unreachable.
- Practical check: after changing access, confirm at least one `QUERYABLE -> ... -> JOINABLE` path exists for intended queries.

### User-context state

- `ENABLED`: table/user-context conditions and policies are enforced during query planning.
- `DISABLED`/`UNSET`: user-context-driven filtering/policies are not reliably enforced; conditions may exist but not behave as intended.
- If a table should be tenant-scoped, ensure:
  1. user-context status is `ENABLED`,
  2. required field exists (for example `organisationId`),
  3. table condition/policy is set to that field.

## Semantic Model Feature Guide (When / Why)

| Feature | When to use | Key constraints |
|---|---|---|
| **Table access** (`QUERYABLE`/`JOINABLE`/`OFF`) | `QUERYABLE` for primary query targets; `JOINABLE` for traversal-only helpers; `OFF` to hide entirely. | `JOINABLE` is unreachable unless a path from a `QUERYABLE` table exists. `OFF` tables are detail-read-only in UI — set to `JOINABLE`/`QUERYABLE` first if you need to edit, then revert. |
| **Table context** | Business definitions, synonyms, caveats, rules. | Improves model interpretation quality when domain language is explicit. |
| **Column selection / rename / notes** | Hide sensitive columns (`selected=false`), use business names, add interpretation hints. | Reduces prompt/schema noise. |
| **Column conversions** | When a string column should behave as numeric (cast/coalesce). | Supported only on string columns. |
| **Enums (static vs dynamic)** | Static for curated vocabularies; dynamic for evolving low-cardinality columns. | Enums supported on string/numeric columns only. Dynamic enum falls back when distinct value count is too high. |
| **Units** | Numeric columns/computed columns where unit is meaningful (`USD`, `%`, `kg`). | Disambiguates metric meaning in generated explanations. |
| **Computed columns** | Reusable derived metrics from existing numeric fields. | Expressions reference numeric-compatible columns; operator/function surface is intentionally narrow. |
| **Relations** | Toggle existing FK relations; create manual relations when FKs are missing or semantically wrong. | Broken relations cannot be enabled. Relations to `OFF` tables are effectively disabled for traversal. |
| **Condition vs policy** | Condition (`table.column == userContext.field`) for row-level filtering; policy (`userContext.boolField == true`) for table-level gating. | Both require user-context `ENABLED`. Policy requires a BOOLEAN field. Inherited-only fields cannot be assigned to local conditions/policies. |
| **Virtual tables** | Durable SQL projections/aggregations without changing source schema. | Validate SQL before save. Deletion blocked if active manual inbound relations exist. MySQL not supported in UI. |
| **Demo/read-only agents** | N/A — read only. | Many mutation families are blocked on demo agents and read-only connections. |

## High-Confidence Patterns

### Table and column changes

```bash
npx inconvo@latest model table set-access \
  --agent <agentId> --connection <connectionId> --table <tableIdOrName> \
  --access QUERYABLE

npx inconvo@latest model column update \
  --agent <agentId> --connection <connectionId> --table <tableIdOrName> \
  --column <columnIdOrName> --rename <newName>
```

### User-context changes

```bash
npx inconvo@latest model user-context add-field \
  --agent <agentId> --key <fieldKey> --type STRING

npx inconvo@latest model user-context set-status \
  --agent <agentId> --status ENABLED
```

### Dry-run and JSON-first automation

```bash
npx inconvo@latest model table set-access \
  --agent <agentId> --connection <connectionId> --table <tableIdOrName> \
  --access QUERYABLE --dry-run --json
```

### Generic model actions

```bash
npx inconvo@latest model action schema --json

npx inconvo@latest model action run \
  --agent <agentId> \
  --connection <connectionId> \
  --action table.setAccess \
  --payload '{"tableId":"table_123","access":"QUERYABLE"}' \
  --json
```

### Linked connection workflows

```bash
npx inconvo@latest connection list --agent <agentId> --json
npx inconvo@latest connection list-shareable --agent <agentId> --json
npx inconvo@latest connection link --agent <agentId> --connection <connectionId> --json
npx inconvo@latest connection unlink --agent <agentId> --connection <connectionId> --json
```

### Trigger a database resync

```bash
npx inconvo@latest connection sync --agent <agentId> --connection <connectionId>
```

After the platform sync completes, the CLI automatically pulls an updated local snapshot.

## Resolution Rules

The CLI resolver follows this order for table/column/relation/field lookups:

1. Exact ID
2. Exact name
3. Case-insensitive unique name

If ambiguous or missing, fail fast and pass explicit IDs.

## Pull Behavior

- Non-interactive mode requires one of:
  - `--agent <id>` (repeatable), or
  - `--all-agents`.
- `--connection` requires exactly one `--agent`.
- Interactive pull prompts for target selection (one/many/all).

## Error Recovery

| Symptom | Likely cause | Fix |
|---|---|---|
| Mutation succeeds but CLI warns sync failed | Network issue or transient API error after the remote write | `inconvo model pull --agent <agentId>` |
| `--table` / `--column` not found | Name mismatch or ambiguous (multiple matches) | Pass the exact ID instead of name |
| `UNAUTHORIZED` | Missing or expired API key | Re-run `inconvo config set` or check `INCONVO_API_KEY` |
| `BAD_REQUEST` from user-context mutation | Key already exists in effective context, or trying to disable while inherited fields exist | Check existing fields with `model agent list --json` and inspect `user-context.yaml` |
| Snapshot looks stale after a DB schema change | Platform hasn't re-introspected the live DB | Run `inconvo connection sync --agent <agentId> --connection <connectionId>` first, then pull |

## Output and Automation Notes

- Add `--json` for machine-readable output (`model pull`, mutations, `model action`, and `connection` list/link/sync commands).
- Add `--dry-run` for mutation commands and `model action run`.
- The CLI automatically warns when a mutation succeeds but local sync fails.
- On sync failure, recover with:

```bash
npx inconvo@latest model pull --agent <agentId>
```

- In CI/CD, set `INCONVO_API_KEY` as an environment secret rather than using `.inconvo/config.yaml`.

## Done Criteria

- Remote mutation executed through CLI command.
- Local `.inconvo/` snapshot auto-synced.
- No manual edits to generated YAML files.
- For linked connections, only one canonical connection snapshot exists under `.inconvo/connections/`.
