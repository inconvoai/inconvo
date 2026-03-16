# Inconvo CLI

## Quick Start

```bash
npx inconvo@latest dev
```

This will:
1. Run the setup wizard (first time only)
2. Pull Docker images for the dev server
3. Start the dev server in Docker and the sandbox on your machine

Then open http://localhost:26686 to start chatting with your data.

## Requirements

- Docker (Docker Desktop or Docker daemon running)
- Node.js 20+ (only for running `npx`)

## Commands

Credential priority (highest first): `--api-key` flag → exported `INCONVO_API_KEY` env var → repo `.env` (`INCONVO_API_KEY`).

If you're working from a cloned repo, add your API settings to the repo `.env`:

```bash
INCONVO_API_KEY=your-api-key
INCONVO_API_BASE_URL=https://app.inconvo.ai
```

### `inconvo dev`

Start the Inconvo development environment.

```bash
npx inconvo@latest dev
```

Options:
- `--image-version <version>` - Use a specific Docker image version

### `inconvo dev configure`

Re-run the configuration wizard to update your settings.

```bash
npx inconvo@latest dev configure
```

### `inconvo model pull`

Generate local semantic-model snapshots under `.inconvo/`.

```bash
npx inconvo@latest model pull --agent <agentId>
```

Use `inconvo model agent list --json` to discover agent IDs for automation.

### `inconvo model agent list`

List agents in the current organization (name + id), useful before pull/mutation commands.

```bash
npx inconvo@latest model agent list --json
```

### `inconvo model <group> <command>`

Apply semantic-model or user-context mutations directly via API, then auto-sync
generated `.inconvo/` snapshots.

Examples:

```bash
npx inconvo@latest model table set-access --agent <agentId> --connection <connectionId> --table orders --access QUERYABLE
npx inconvo@latest model column update --agent <agentId> --connection <connectionId> --table orders --column total --rename amount
npx inconvo@latest model user-context add-field --agent <agentId> --key org_id --type STRING
```

For agent/tooling usage:
- Add `--json` for structured output.
- Add `--dry-run` to mutation commands to print the resolved action payload without applying changes.

Available groups:
- `table`
- `column` (`conversion`, `enum`)
- `computed`
- `relation` (`manual`)
- `condition`
- `policy`
- `virtual`
- `user-context`

### `inconvo model action`

Run generic model actions from raw JSON payloads and inspect supported action metadata.

```bash
npx inconvo@latest model action schema --json
npx inconvo@latest model action run --agent <agentId> --connection <connectionId> --action table.setAccess --payload '{"tableId":"table_123","access":"QUERYABLE"}' --json
```

### `inconvo connection sync`

Trigger a full database resync for a connection — the platform re-introspects the live DB schema, then the CLI pulls an updated local snapshot.

```bash
npx inconvo@latest connection sync --agent <agentId> --connection <connectionId>
# or interactively (pick from a list):
npx inconvo@latest connection sync --agent <agentId>
```

### `inconvo connection get`

Show metadata for a connection, including its description. In CLI responses, `description` is the user-facing name for the platform's internal connection `context` field.

```bash
npx inconvo@latest connection get --agent <agentId> --connection <connectionId>
# or interactively:
npx inconvo@latest connection get --agent <agentId>
```

### `inconvo connection update`

Update or clear a connection description without redeploying the connector. After a successful update, the CLI refreshes the local `.inconvo/` snapshot for that connection.

```bash
npx inconvo@latest connection update --agent <agentId> --connection <connectionId> --description "Sales warehouse used for BI reporting"
npx inconvo@latest connection update --agent <agentId> --connection <connectionId> --clear-description
```

## Configuration

On first run, the CLI will prompt you for:

- **Database connection** - PostgreSQL, Redshift, MySQL, SQL Server, or BigQuery
- **OpenAI API key** - For the LLM

For BigQuery connections, the setup wizard also asks for:
- Project ID
- Dataset
- Location (for example `US` or `EU`)
- Service account credentials JSON file path (copied to `~/.inconvo/credentials/` and mounted into Docker)
- Max bytes billed per query (optional, must be an integer greater than `0`)

BigQuery credentials are resolved in this order when multiple values are set:
1. `INCONVO_BIGQUERY_CREDENTIALS_JSON`
2. `INCONVO_BIGQUERY_CREDENTIALS_BASE64`
3. `INCONVO_BIGQUERY_KEYFILE`

Configuration is stored in `~/.inconvo/config.env`.
The CLI stores the dev-server SQLite database at `~/.inconvo/data/inconvo.db`.

> **Note**: When developing locally from a cloned repo, the dev-server uses `.inconvo.env` in the `apps/dev-server/` directory instead. These are separate configurations.

## AI Agent Skill

Install the Inconvo CLI skill to give your AI agent (Claude Code, Cursor, etc.) full knowledge of the semantic model mutation workflow:

```bash
npx skills add inconvoai/inconvo
# or globally across all agents:
npx skills add inconvoai/inconvo --global
```

## For Development

If you want to contribute or develop locally, clone the repository instead:

```bash
git clone https://github.com/inconvoai/inconvo.git
cd inconvo
pnpm install
pnpm dev
```

See the main repository README for development instructions.

## License

Apache-2.0
