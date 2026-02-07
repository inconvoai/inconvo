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
- Node.js 18+ (only for running `npx`)

## Commands

### `inconvo dev`

Start the Inconvo development environment.

```bash
npx inconvo@latest dev
```

Options:
- `--image-version <version>` - Use a specific Docker image version

### `inconvo configure`

Re-run the configuration wizard to update your settings.

```bash
npx inconvo@latest configure
```

## Configuration

On first run, the CLI will prompt you for:

- **Database connection** - PostgreSQL, Redshift, MySQL, SQL Server, or BigQuery
- **OpenAI API key** - For the LLM

For BigQuery connections, the setup wizard also asks for:
- Project ID
- Dataset
- Location (for example `US` or `EU`)
- Service account credentials JSON (via file path or pasted JSON)

Configuration is stored in `~/.inconvo/config.env`.
The CLI stores the dev-server SQLite database at `~/.inconvo/data/inconvo.db`.

> **Note**: When developing locally from a cloned repo, the dev-server uses `.inconvo.env` in the `apps/dev-server/` directory instead. These are separate configurations.

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
