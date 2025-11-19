#!/bin/bash
# Production RepCard Setup Verification Script
#
# This script checks if RepCard is properly configured in production:
# 1. Environment variables (via Vercel CLI)
# 2. Database tables
# 3. Data sync status
# 4. User linking status
#
# Usage: ./scripts/verify-production-setup.sh

set -e

echo "🔍 RepCard Production Setup Verification"
echo "========================================"
echo ""

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Check if Vercel CLI is installed
if ! command -v vercel &> /dev/null; then
    echo -e "${YELLOW}⚠️  Vercel CLI not found. Installing...${NC}"
    npm i -g vercel
fi

# Check if DATABASE_URL is set
if [ -z "$DATABASE_URL" ]; then
    echo -e "${YELLOW}⚠️  DATABASE_URL not set. Pulling from Vercel...${NC}"
    
    if [ ! -f .env.production ]; then
        vercel env pull .env.production --environment=production
    fi
    
    if [ -f .env.production ]; then
        export $(cat .env.production | grep -v '^#' | xargs)
    else
        echo -e "${RED}❌ Could not load DATABASE_URL${NC}"
        echo "Please set DATABASE_URL environment variable or run:"
        echo "  vercel env pull .env.production --environment=production"
        exit 1
    fi
fi

echo "📋 Step 1: Checking Environment Variables"
echo "----------------------------------------"

# Check REPCARD_API_KEY via Vercel
echo "Checking REPCARD_API_KEY in Vercel production..."
if vercel env ls | grep -q "REPCARD_API_KEY.*Production"; then
    echo -e "${GREEN}✅ REPCARD_API_KEY is set in Vercel production${NC}"
else
    echo -e "${RED}❌ REPCARD_API_KEY is NOT set in Vercel production${NC}"
    echo ""
    echo "ACTION REQUIRED:"
    echo "1. Go to: https://vercel.com/[your-project]/settings/environment-variables"
    echo "2. Add: REPCARD_API_KEY = <your-repcard-api-key>"
    echo "3. Select: Production environment"
    echo "4. Save and redeploy"
    echo ""
fi

# Check CRON_SECRET
if vercel env ls | grep -q "CRON_SECRET.*Production"; then
    echo -e "${GREEN}✅ CRON_SECRET is set in Vercel production${NC}"
else
    echo -e "${YELLOW}⚠️  CRON_SECRET is not set (cron jobs may not work)${NC}"
fi

echo ""
echo "📊 Step 2: Checking Database Tables"
echo "------------------------------------"

# Check if psql is available
if ! command -v psql &> /dev/null; then
    echo -e "${YELLOW}⚠️  psql not found. Skipping database checks.${NC}"
    echo "You can manually check by running:"
    echo "  psql \"\$DATABASE_URL\" -f scripts/check-repcard-tables.sql"
else
    # Check tables
    echo "Checking RepCard tables..."
    TABLE_COUNT=$(psql "$DATABASE_URL" -t -c "SELECT COUNT(*) FROM information_schema.tables WHERE table_schema = 'public' AND table_name LIKE 'repcard_%';" 2>/dev/null | xargs)
    
    if [ "$TABLE_COUNT" -gt 0 ]; then
        echo -e "${GREEN}✅ Found $TABLE_COUNT RepCard tables${NC}"
        
        # Check data counts
        echo ""
        echo "Data counts:"
        psql "$DATABASE_URL" -c "
          SELECT 
            (SELECT COUNT(*) FROM repcard_users) as users,
            (SELECT COUNT(*) FROM repcard_customers) as customers,
            (SELECT COUNT(*) FROM repcard_appointments) as appointments,
            (SELECT COUNT(*) FROM repcard_status_logs) as status_logs;
        " 2>/dev/null || echo "Could not query data counts"
    else
        echo -e "${RED}❌ No RepCard tables found!${NC}"
        echo ""
        echo "ACTION REQUIRED: Run migrations"
        echo "  ./scripts/run-migrations-production.sh"
    fi
fi

echo ""
echo "👥 Step 3: Checking User Linking"
echo "--------------------------------"

if command -v psql &> /dev/null; then
    LINKED_COUNT=$(psql "$DATABASE_URL" -t -c "SELECT COUNT(*) FROM users WHERE repcard_user_id IS NOT NULL;" 2>/dev/null | xargs)
    TOTAL_COUNT=$(psql "$DATABASE_URL" -t -c "SELECT COUNT(*) FROM users;" 2>/dev/null | xargs)
    
    if [ "$LINKED_COUNT" -gt 0 ]; then
        PERCENTAGE=$((LINKED_COUNT * 100 / TOTAL_COUNT))
        echo -e "${GREEN}✅ $LINKED_COUNT / $TOTAL_COUNT users linked ($PERCENTAGE%)${NC}"
    else
        echo -e "${RED}❌ No users linked to RepCard!${NC}"
        echo ""
        echo "ACTION REQUIRED: Link users"
        echo "  psql \"\$DATABASE_URL\" -f scripts/link-users-to-repcard.sql"
    fi
else
    echo -e "${YELLOW}⚠️  psql not found. Skipping user linking check.${NC}"
fi

echo ""
echo "========================================"
echo "✅ Verification Complete!"
echo ""
echo "Next Steps:"
echo "1. If REPCARD_API_KEY is missing: Add it to Vercel and redeploy"
echo "2. If tables are missing: Run migrations"
echo "3. If no data: Run sync via admin dashboard"
echo "4. If users not linked: Run link-users-to-repcard.sql"
echo ""

