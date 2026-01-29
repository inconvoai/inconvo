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

# Execute the main command (wrangler dev)
exec "$@"
