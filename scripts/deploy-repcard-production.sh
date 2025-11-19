#!/bin/bash
# RepCard Production Deployment Script
# This script runs all necessary steps to deploy RepCard fixes to production

set -e

echo "🚀 RepCard Production Deployment"
echo "================================"
echo ""

# Colors for output
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m' # No Color

# Step 1: Check prerequisites
echo "📋 Step 1: Checking prerequisites..."
if [ -z "$DATABASE_URL" ]; then
    echo -e "${RED}❌ DATABASE_URL environment variable not set${NC}"
    echo "Please set DATABASE_URL before running this script"
    exit 1
fi
echo -e "${GREEN}✅ DATABASE_URL is set${NC}"

if ! command -v psql &> /dev/null && ! command -v npx &> /dev/null; then
    echo -e "${RED}❌ Neither psql nor npx found. Please install PostgreSQL client or Node.js${NC}"
    exit 1
fi
echo -e "${GREEN}✅ Prerequisites met${NC}"
echo ""

# Step 2: Run migrations
echo "📋 Step 2: Running migrations..."
echo -e "${YELLOW}⚠️  This will modify your database schema${NC}"
read -p "Continue? (yes/no): " confirm

if [ "$confirm" != "yes" ]; then
    echo "❌ Cancelled"
    exit 1
fi

echo ""
echo "Running migration 017: Make company_id nullable..."
if command -v npx &> /dev/null; then
    npx tsx scripts/run-repcard-migrations.ts
else
    echo "Using psql directly..."
    psql "$DATABASE_URL" -f lib/db/migrations/017_make_repcard_users_company_id_nullable.sql
    psql "$DATABASE_URL" -f lib/db/migrations/018_normalize_repcard_user_ids_to_integer.sql
fi

if [ $? -eq 0 ]; then
    echo -e "${GREEN}✅ Migrations completed successfully${NC}"
else
    echo -e "${RED}❌ Migrations failed${NC}"
    exit 1
fi
echo ""

# Step 3: Verify migrations
echo "📋 Step 3: Verifying migrations..."
echo "Checking if company_id is nullable..."
COMPANY_ID_CHECK=$(psql "$DATABASE_URL" -t -c "SELECT COUNT(*) FROM information_schema.columns WHERE table_name='repcard_users' AND column_name='company_id' AND is_nullable='YES';" 2>/dev/null || echo "0")

if [ "$COMPANY_ID_CHECK" -gt 0 ]; then
    echo -e "${GREEN}✅ company_id is nullable${NC}"
else
    echo -e "${YELLOW}⚠️  company_id nullable check inconclusive (may need manual verification)${NC}"
fi

echo "Checking if user IDs are INTEGER..."
USER_ID_CHECK=$(psql "$DATABASE_URL" -t -c "SELECT data_type FROM information_schema.columns WHERE table_name='users' AND column_name='repcard_user_id';" 2>/dev/null || echo "")

if echo "$USER_ID_CHECK" | grep -q "integer"; then
    echo -e "${GREEN}✅ repcard_user_id is INTEGER${NC}"
else
    echo -e "${YELLOW}⚠️  repcard_user_id type check inconclusive (may need manual verification)${NC}"
fi
echo ""

# Step 4: Backfill company_id (optional)
echo "📋 Step 4: Backfill company_id (optional)..."
read -p "Run backfill script to populate company_id from offices? (yes/no): " run_backfill

if [ "$run_backfill" = "yes" ]; then
    if command -v npx &> /dev/null; then
        npx tsx scripts/backfill-repcard-users-company-id.ts
        echo -e "${GREEN}✅ Backfill completed${NC}"
    else
        echo -e "${YELLOW}⚠️  npx not found, skipping backfill${NC}"
    fi
else
    echo "Skipping backfill (can run later with: npx tsx scripts/backfill-repcard-users-company-id.ts)"
fi
echo ""

# Step 5: Summary
echo "================================"
echo -e "${GREEN}✅ Deployment Complete!${NC}"
echo ""
echo "Next steps:"
echo "1. ✅ Migrations completed"
echo "2. ⏭️  Code is already deployed (pushed to main)"
echo "3. ⏭️  Go to /admin/repcard-sync and run 'Quick Sync'"
echo "4. ⏭️  Verify analytics at /analytics → RepCard tab"
echo ""
echo "If you need to backfill company_id later:"
echo "  npx tsx scripts/backfill-repcard-users-company-id.ts"
echo ""

