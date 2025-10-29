#!/bin/bash

# Load environment variables from .env.local
if [ -f .env.local ]; then
  export $(grep -v '^#' .env.local | grep 'ARRIVY_\|DATABASE_URL\|POSTGRES_URL' | xargs)

  # If DATABASE_URL exists but POSTGRES_URL doesn't, copy it
  if [ ! -z "$DATABASE_URL" ] && [ -z "$POSTGRES_URL" ]; then
    export POSTGRES_URL="$DATABASE_URL"
  fi
fi

# Run the sync script with all arguments passed through
npx tsx scripts/sync-arrivy-tasks.ts "$@"
