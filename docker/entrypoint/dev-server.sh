#!/bin/bash
set -e

echo "Initializing database..."

# Ensure data directory exists
mkdir -p /data

# Run Prisma db push to create/migrate SQLite database
cd /app/apps/dev-server
prisma db push --skip-generate --accept-data-loss 2>/dev/null || {
    echo "Database initialization failed, but continuing..."
}

echo "Starting dev-server..."

# Execute the main command (node server.js)
exec "$@"
