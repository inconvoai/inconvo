# Inconvo CLI

Try out Inconvo without cloning the repository.

## Quick Start

```bash
npx inconvo dev
```

This will:
1. Run the setup wizard (first time only)
2. Pull Docker images for the dev server and sandbox
3. Start the services via Docker Compose

Then open http://localhost:26686 to start chatting with your data.

## Requirements

- Docker (Docker Desktop or Docker daemon running)
- Node.js 18+ (only for running `npx`)

## Commands

### `inconvo dev`

Start the Inconvo development environment.

```bash
npx inconvo dev
```

Options:
- `--version <version>` - Use a specific release version

### `inconvo configure`

Re-run the configuration wizard to update your settings.

```bash
npx inconvo configure
```

## Configuration

On first run, the CLI will prompt you for:

- **Database connection** - PostgreSQL, MySQL, or SQL Server
- **OpenAI API key** - For the LLM
- **LangChain API key** - For prompt management

Configuration is stored in `~/.inconvo/config.env`.

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

MIT
