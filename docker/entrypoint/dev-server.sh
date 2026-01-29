#!/bin/bash
set -e

echo "Initializing database..."

# Ensure data directory exists
mkdir -p /data

# Set database URL for Prisma
# Use INCONVO_LOCAL_DB_PATH from compose, or default to /data/inconvo.db
export DATABASE_URL="file:${INCONVO_LOCAL_DB_PATH:-/data/inconvo.db}"

# Run Prisma db push to create/migrate SQLite database
cd /app/apps/dev-server

# Remove prisma.config.ts if present (it requires modules not in standalone build)
rm -f prisma.config.ts

echo "Running prisma db push..."
prisma db push --schema prisma/schema.prisma --url "$DATABASE_URL" --accept-data-loss || {
    echo "Warning: Database initialization failed"
    echo "Schema location: $(ls -la prisma/ 2>/dev/null || echo 'prisma dir not found')"
}

echo "Starting dev-server..."

# Execute the main command (node server.js)
exec "$@"
