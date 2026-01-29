#!/bin/bash
set -e

echo "Initializing database..."

# Ensure data directory exists
mkdir -p /data

# Run Prisma db push to create/migrate SQLite database
cd /app/apps/dev-server
echo "Running prisma db push..."
prisma db push --skip-generate --accept-data-loss || {
    echo "Warning: Database initialization failed"
    echo "Schema location: $(ls -la prisma/ 2>/dev/null || echo 'prisma dir not found')"
}

echo "Starting dev-server..."

# Execute the main command (node server.js)
exec "$@"
