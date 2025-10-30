#!/bin/bash
# Deployment script for RepCard Complete Integration
# This script runs migrations and prepares for deployment

set -e

echo "🚀 RepCard Complete Integration - Deployment Script"
echo "=================================================="
echo ""

# Colors for output
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m' # No Color

# Check if DATABASE_URL is set
if [ -z "$DATABASE_URL" ]; then
    echo -e "${RED}❌ ERROR: DATABASE_URL environment variable is not set${NC}"
    echo "Please set DATABASE_URL before running migrations"
    exit 1
fi

echo -e "${GREEN}✅ DATABASE_URL is set${NC}"
echo ""

# Run migrations
echo "📦 Running database migrations..."
echo ""

echo "1. Running migration 016_repcard_complete_data.sql..."
psql "$DATABASE_URL" -f lib/db/migrations/016_repcard_complete_data.sql
if [ $? -eq 0 ]; then
    echo -e "${GREEN}✅ Migration 016 completed successfully${NC}"
else
    echo -e "${RED}❌ Migration 016 failed${NC}"
    exit 1
fi

echo ""
echo "2. Running migration 017_repcard_settings.sql..."
psql "$DATABASE_URL" -f lib/db/migrations/017_repcard_settings.sql
if [ $? -eq 0 ]; then
    echo -e "${GREEN}✅ Migration 017 completed successfully${NC}"
else
    echo -e "${RED}❌ Migration 017 failed${NC}"
    exit 1
fi

echo ""
echo -e "${GREEN}✅ All migrations completed successfully!${NC}"
echo ""
echo "📋 Next steps:"
echo "1. ✅ Migrations are complete"
echo "2. ⏭️  Commit and push changes to trigger deployment"
echo "3. ⏭️  After deployment, run comprehensive sync:"
echo "   POST /api/admin/repcard/comprehensive-sync"
echo "4. ⏭️  Configure leaderboards in Settings → RepCard Config"

