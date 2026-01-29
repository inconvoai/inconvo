#!/bin/bash
set -e

# Check if Docker socket is mounted
if [ ! -S /var/run/docker.sock ]; then
    echo "WARNING: Docker socket not mounted at /var/run/docker.sock"
    echo "Container execution features will not work."
    echo "Mount with: -v /var/run/docker.sock:/var/run/docker.sock"
fi

# Ensure data directory exists for local storage
mkdir -p /data

echo "Starting sandbox..."

# Build wrangler args - pass env vars as worker vars
WRANGLER_ARGS="dev --port 8787 --ip 0.0.0.0"
WRANGLER_ARGS="$WRANGLER_ARGS --var SKIP_BUCKET_MOUNT:true"

if [ -n "$INTERNAL_API_KEY" ]; then
    WRANGLER_ARGS="$WRANGLER_ARGS --var INTERNAL_API_KEY:$INTERNAL_API_KEY"
fi

# Execute wrangler with the constructed args
exec wrangler $WRANGLER_ARGS
