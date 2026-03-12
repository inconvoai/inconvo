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
4. Let the CLI auto-refresh local `.inconvo/` snapshots after successful mutations.
5. Use `--dry-run` to verify target resolution; it does not validate payload format.
6. Use `--json` for automation/agent workflows.
7. For many independent mutations, use `--no-sync` on each command and run `inconvo model pull` once at the end.
8. Use `inconvo model pull` after DB schema changes or to recover local snapshots.

## Authentication

The CLI resolves credentials in this priority order (highest first):

1. `--api-key` / `--api-base-url` flags on the command
2. `INCONVO_API_KEY` / `INCONVO_API_BASE_URL` environment variables
3. `.inconvo/config.yaml` in the repo root (gitignored, never committed)

```bash
npx inconvo@latest config set
# or non-interactively:
npx inconvo@latest config set --api-key <key> --api-base-url <url>
npx inconvo@latest config view
```

## Required Inputs

- `agentId` for all mutations (`--agent`). Read from `.inconvo/agents/<slug>/agent.yaml`.
- `connectionId` for schema mutations (`--connection`). Read from `.inconvo/connections/<slug>/connection.yaml`.
- Connection snapshots expose the platform connection context as `description`.
- Target identifiers: prefer IDs for stability, names work when unambiguous.

## Snapshot Layout

```text
.inconvo/
  agents/
    .slug-map.yaml
    <agent-slug>/
      agent.yaml                  # contains agentId
      user-context.yaml           # fields + status
      shareable-connections.yaml
      connections/
        <connection-slug>/
          connection.yaml         # reference only — includes description + snapshotPath
  connections/
    .slug-map.yaml
    <connection-slug>/
      connection.yaml             # contains connectionId + description
      tables/
        .slug-map.yaml
        <table-slug>.yaml         # full table snapshot: columns, relations, computed, condition, policy
```

All files are auto-generated. Read them for IDs and current state; never edit directly.

## Sync Terms

- **Post-mutation sync**: automatic refresh of local `.inconvo/` files after a successful mutation.
- **`model pull`**: rebuild local snapshots from the platform.
- **`connection sync`**: ask the platform to re-introspect a connection's database schema. Follow with `model pull` when you need updated local files.

## Standard Workflow

1. Read `.inconvo/` YAML files to understand current state and collect IDs.
2. Read code context (schema, routes, UI) to understand intended semantics.
3. Plan all mutations, group independent ones for parallel execution.
4. Run mutations. Use `--dry-run --json` when target resolution is uncertain. For many independent mutations, add `--no-sync` to each command and run `inconvo model pull` once at the end.
5. Verify the synced YAML files reflect expected changes.
6. If sync fails, run `inconvo model pull --agent <agentId>`.

## Command Discovery

```bash
npx inconvo@latest model --help
npx inconvo@latest model <group> --help
npx inconvo@latest model action schema --json   # lists all valid action names
npx inconvo@latest connection --help
```

## Full Mutation Reference

| What you want to do                | CLI Command                                                                          |
| ---------------------------------- | ------------------------------------------------------------------------------------ |
| Enable/disable a table             | `model table set-access --access QUERYABLE\|JOINABLE\|OFF`                           |
| Set table description              | `model table set-context --context "..."`                                            |
| Rename / add notes / hide a column | `model column update --rename --notes --selected`                                    |
| Set a column unit                  | `model column set-unit --unit USD`                                                   |
| Create a computed column           | `model computed create --name --ast <json> --unit`                                   |
| Update a computed column           | `model computed update` / `model computed set-unit`                                  |
| Delete a computed column           | `model computed delete`                                                              |
| Toggle an FK relation on/off       | `model relation toggle --selected true\|false`                                       |
| Create a manual relation           | `model relation manual create --source-table --target-table --name --is-list --pair` |
| Delete a manual relation           | `model relation manual delete --source-table --relation`                             |
| Set row-level condition            | `model condition set --table --column --field`                                       |
| Clear row-level condition          | `model condition clear --table`                                                      |
| Set table access policy            | `model policy set --table --field`                                                   |
| Clear table access policy          | `model policy clear --table`                                                         |
| Create static enum                 | `model column enum create-static`                                                    |
| Create dynamic enum                | `model column enum create-dynamic`                                                   |
| Add user-context field             | `model user-context add-field --key --type STRING\|NUMBER\|BOOLEAN`                  |
| Delete user-context field          | `model user-context delete-field --key`                                              |
| Enable user-context                | `model user-context set-status --status ENABLED`                                     |
| View connection description        | `connection get --agent <agentId> --connection <connectionId>`                       |
| Update connection description      | `connection update --agent <agentId> --connection <connectionId> --description "..."` |
| Pull latest snapshot               | `model pull --agent <agentId> [--connection <connectionId>]`                         |
| Trigger DB resync                  | `connection sync --agent <agentId> --connection <connectionId>`                      |

## Connection Metadata

Use connection commands for database-level metadata, not semantic-model changes:

```bash
# Read current connection metadata (description maps to platform "context")
npx inconvo@latest connection get \
  --agent <agentId> --connection <connectionId> --json

# Set the description
npx inconvo@latest connection update \
  --agent <agentId> --connection <connectionId> \
  --description "Sales warehouse used for BI reporting" --json

# Clear the description
npx inconvo@latest connection update \
  --agent <agentId> --connection <connectionId> \
  --clear-description --json
```

`connection update` refreshes the local `.inconvo/` snapshot for that connection. It does not perform a remote DB rescan; `connection sync` does that.

## Computed Column AST Format

The `--ast` flag takes a JSON string. Node types and their shapes:

```json
{ "type": "column",    "name": "<columnName>" }
{ "type": "value",     "value": 42 }
{ "type": "function",  "name": "ABS", "arguments": [<node>] }
{ "type": "operation", "operator": "+"|"-"|"*"|"/"|"%", "operands": [<node>, <node>] }
{ "type": "brackets",  "expression": <node> }
```

Example — `(subtotal - discount) + tax`:

```bash
npx inconvo@latest model computed create \
  --agent <agentId> --connection <connectionId> \
  --table orders --name "total" \
  --ast '{"type":"operation","operator":"+","operands":[{"type":"brackets","expression":{"type":"operation","operator":"-","operands":[{"type":"column","name":"subtotal"},{"type":"column","name":"discount"}]}},{"type":"column","name":"tax"}]}' \
  --unit USD --json
```

> **Note:** `--dry-run` does NOT validate the AST schema — it only resolves the target entity and shows what would be sent. The API validates the AST and returns a detailed Zod error listing valid node shapes if it's wrong.

## Relations Workflow

FK relations defined in the database schema are **auto-introspected** by the platform and should appear in table YAML under `outwardRelations` with `source: FK` on the first pull. No manual steps should be required.

**Only create manual relations** when no FK constraint exists in the DB (e.g. soft references, denormalised keys, or cross-database joins).

Check current relation state:

```bash
grep -E "source: MANUAL|source: FK|^  - id: relation_|^    name:" \
  .inconvo/connections/<slug>/tables/<table>.yaml
```

### If FK relations are missing after a pull

Before creating manual relations, ask the user to confirm:

1. **Do the FK constraints actually exist in the database?** Check with the user or inspect the schema — if there are no FK constraints, that explains the absence.
2. **Does the Inconvo DB user have access to `information_schema`?** FK introspection requires read access to `information_schema.key_column_usage` and `information_schema.referential_constraints`. If this permission is missing, run a `connection sync` after granting it and then pull again.

Only fall back to manual relations if FKs genuinely don't exist in the schema.

## Semantic Model Content Guidelines

Prefer fewer, higher-signal semantics over broad coverage. Default to preserving
existing semantics; only mutate entries that are clearly missing, wrong, or
materially improved by the code context.

Before adding or changing table context or column notes, ask: "Does this add
non-obvious business meaning that is not already captured by the schema,
relations, units, renames, computed columns, or existing notes?" If not, skip
it.

Keeping each layer focused prevents duplication and improves model quality:

| Layer                     | What belongs here                                                                                                                                  | What does NOT belong here                                                   |
| ------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------- | --------------------------------------------------------------------------- |
| **Table context**         | What the table represents, business domain meaning, important distinctions (e.g. "accounts vs customers"), which questions to direct at this table | Formulas, thresholds, enum values, per-column behavior, or repeated FK details |
| **Column notes**          | Non-obvious business meaning, valid ranges, business rules, or app-specific interpretation for a field                                             | Restating the name/type, generic timestamps, or FK details already expressed as a relation/condition |
| **Computed columns**      | The single authoritative place for a derived metric formula (e.g. `total = subtotal - discount + tax`)                                             | Don't repeat the formula in the notes of input columns                      |
| **Column rename**         | Business-friendly display name when the DB column name is unclear (`ean` → `EAN (barcode)`)                                                        | —                                                                           |
| **Column selected=false** | Sensitive columns that must never be exposed (passwords, tokens, internal flags)                                                                   | —                                                                           |

- Do not add notes just to fill blank fields. Broad annotation coverage is worse
  than a smaller set of precise semantics.
- Do not restate ownership or tenant scoping in notes or table context when
  relations and row-level conditions already express it.
- Preserve existing good semantics. Change only what is clearly improved by the
  code context.

## Multi-Tenancy Pattern

For apps that scope all data by a tenant/organisation ID:

```bash
# 1. Add the tenant field to user-context
npx inconvo@latest model user-context add-field \
  --agent <agentId> --key organisationId --type NUMBER --no-sync --json

# 2. Enable user-context
npx inconvo@latest model user-context set-status \
  --agent <agentId> --status ENABLED --no-sync --json

# 3. Set condition on every tenant-scoped table (run in parallel)
npx inconvo@latest model condition set \
  --agent <agentId> --connection <connectionId> \
  --table <table> --column organisation_id --field organisationId --no-sync --json

# 4. Pull once at the end
npx inconvo@latest model pull --agent <agentId> --json
```

Conditions require user-context to be `ENABLED` to take effect. The order of add-field / set-status / condition set does not matter as long as status is ENABLED before queries run.

## Table Access Decision Guide

| Table role                                                                      | Access level |
| ------------------------------------------------------------------------------- | ------------ |
| Users ask about it directly (orders, products, users, reviews)                  | `QUERYABLE`  |
| Reference/lookup table only traversed via relations (organisations, categories) | `JOINABLE`   |
| Internal, sensitive, or irrelevant to the agent                                 | `OFF`        |

`JOINABLE` is only useful when there is at least one `QUERYABLE` table with a relation path to it. A `JOINABLE` table with no reachable path is effectively `OFF`.

## High-Confidence Patterns

### Bulk table setup (run in parallel with --no-sync, then pull once)

```bash
npx inconvo@latest model table set-access \
  --agent <agentId> --connection <connectionId> \
  --table <table> --access QUERYABLE --no-sync --json

npx inconvo@latest model table set-context \
  --agent <agentId> --connection <connectionId> \
  --table <table> --context "What this table is and when to use it." --no-sync --json

# After all mutations complete:
npx inconvo@latest model pull --agent <agentId> --json
```

### Column updates

```bash
# Rename + notes in one command
npx inconvo@latest model column update \
  --agent <agentId> --connection <connectionId> \
  --table <table> --column <column> \
  --rename "display name" --notes "What this column means." --json

# Hide sensitive column
npx inconvo@latest model column update \
  --agent <agentId> --connection <connectionId> \
  --table <table> --column password --selected false --json

# Set currency unit
npx inconvo@latest model column set-unit \
  --agent <agentId> --connection <connectionId> \
  --table <table> --column price --unit USD --json
```

### Manual relation (fallback when no FK)

```bash
npx inconvo@latest model relation manual create \
  --agent <agentId> --connection <connectionId> \
  --source-table <table> --target-table <table> \
  --name "relationName" --is-list false \
  --pair "source_col:target_col" --json
```

### Pull for a specific connection

```bash
npx inconvo@latest model pull \
  --agent <agentId> --connection <connectionId> --json
```

## Resolution Rules

The CLI resolves `--table`, `--column`, `--relation`, `--field` in this order:

1. Exact ID match
2. Exact name match
3. Case-insensitive unique name match

If ambiguous (multiple matches) or not found — fail fast and use the explicit ID from the YAML file.

## Error Recovery

| Symptom                                               | Likely cause                                                                            | Fix                                                                                                                           |
| ----------------------------------------------------- | --------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------- |
| Mutation succeeds but sync warning                    | Transient network error after remote write                                              | `model pull --agent <agentId>`                                                                                                |
| "No changes detected (hash unchanged)"               | Remote data hasn't changed since last sync                                              | Expected — the CLI skips redundant disk writes when hashes match                                                              |
| "Sync skipped (--no-sync)"                            | `--no-sync` flag was used                                                               | Run `model pull --agent <agentId>` when ready                                                                                 |
| `--table` / `--column` not found                      | Name mismatch or ambiguous                                                              | Use exact ID from YAML                                                                                                        |
| `UNAUTHORIZED`                                        | Missing or expired API key                                                              | Re-run `inconvo config set`                                                                                                   |
| `BAD_REQUEST` with Zod errors on `computedColumn.ast` | Wrong AST node shape                                                                    | Check the AST Format section above; every node requires a `type` discriminator                                                |
| `BAD_REQUEST` from user-context mutation              | Field key already exists                                                                | Check `user-context.yaml` fields list                                                                                         |
| FK relations missing after pull                       | Inconvo DB user lacks `information_schema` access, or no FK constraints exist in the DB | Confirm FK constraints exist and that the DB user has `information_schema` read access, then `connection sync` + `model pull` |
| Manual relation delete returns "not found"            | Already absorbed into a FK relation after pull                                          | Check `source:` in YAML — if `FK`, it was absorbed; nothing to delete                                                         |

## Done Criteria

- All remote mutations executed via CLI.
- Local `.inconvo/` snapshot auto-synced after each mutation (or synced once at end if `--no-sync` was used).
- No manual edits to generated YAML files.
- Table contexts describe purpose only — no column-level details.
- Column notes describe the specific column only — no table-level details.
- Derived metrics live in computed columns; not repeated in input column notes.
