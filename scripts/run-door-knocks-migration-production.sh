#!/bin/bash
# Run Door Knocks Migration (036) in Production
# Usage: ./scripts/run-door-knocks-migration-production.sh

set -e

echo "🚀 Door Knocks Migration (036) - Production"
echo "============================================"
echo ""
echo "⚠️  WARNING: This will run migrations on PRODUCTION database"
echo ""
read -p "Are you sure you want to continue? (type 'yes' to confirm): " confirm

if [ "$confirm" != "yes" ]; then
  echo "❌ Cancelled"
  exit 1
fi

echo ""
echo "📥 Pulling production environment variables..."

# Check if vercel CLI is installed
if ! command -v vercel &> /dev/null; then
  echo "❌ Vercel CLI not found. Installing..."
  npm i -g vercel
fi

# Pull production env vars
vercel env pull .env.production --environment=production

if [ ! -f .env.production ]; then
  echo "❌ Failed to pull .env.production"
  exit 1
fi

echo "✅ Production environment variables pulled"
echo ""

# Extract DATABASE_URL from .env.production
DATABASE_URL=$(grep "^DATABASE_URL=" .env.production | cut -d '=' -f2- | tr -d '"' | tr -d "'")

if [ -z "$DATABASE_URL" ]; then
  echo "❌ DATABASE_URL not found in .env.production"
  exit 1
fi

# Show database info (without password)
DB_INFO=$(echo "$DATABASE_URL" | sed 's/:[^:@]*@/:***@/g')
echo "📊 Database: $DB_INFO"
echo ""

read -p "Confirm this is your PRODUCTION database (yes/no): " db_confirm

if [ "$db_confirm" != "yes" ]; then
  echo "❌ Cancelled - database not confirmed"
  exit 1
fi

echo ""
echo "🚀 Running door knocks migration with production DATABASE_URL..."
echo ""

# Run migration with production DATABASE_URL
export DATABASE_URL
export POSTGRES_URL="$DATABASE_URL"
npx tsx scripts/run-door-knocks-migration.ts

echo ""
echo "✅ Migration complete!"
echo ""
echo "📋 Next steps:"
echo "1. ✅ Migration complete"
echo "2. ⏭️  Wait for Vercel deployment (if not already deployed)"
echo "3. ⏭️  Run a full sync to extract door knocks from existing customer data"
echo "4. ⏭️  Verify dashboard shows accurate doors/hours metrics"
echo ""

# Cleanup
unset DATABASE_URL
unset POSTGRES_URL
