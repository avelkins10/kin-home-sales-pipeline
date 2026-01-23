#!/bin/bash
# Run Migrations 034 and 035 in Production
# Usage: ./scripts/run-production-migrations.sh [production-url]

set -e

# Get production URL from argument or environment
PROD_URL="${1:-${PRODUCTION_URL}}"

if [ -z "$PROD_URL" ]; then
  echo "❌ Error: Production URL required"
  echo "Usage: $0 <production-url>"
  echo "   or: PRODUCTION_URL=https://... $0"
  exit 1
fi

echo "🚀 Running Migrations 034 and 035 in Production"
echo "📍 URL: $PROD_URL"
echo ""

# Check status first
echo "📋 Checking current migration status..."
STATUS=$(curl -s "$PROD_URL/api/admin/run-migrations-034-035" || echo "{}")

if echo "$STATUS" | grep -q '"migration034":true' && echo "$STATUS" | grep -q '"migration035":true'; then
  echo "✅ Migrations 034 and 035 are already applied!"
  exit 0
fi

echo "⚠️  Migrations not yet applied. Running now..."
echo ""

# Run migrations
echo "🔄 Running migrations..."
RESULT=$(curl -s -X POST "$PROD_URL/api/admin/run-migrations-034-035" \
  -H "Content-Type: application/json" || echo '{"success":false,"error":"Request failed"}')

# Check result
if echo "$RESULT" | grep -q '"success":true'; then
  echo "✅ Migrations completed successfully!"
  echo ""
  echo "📊 Results:"
  echo "$RESULT" | jq '.' 2>/dev/null || echo "$RESULT"
else
  echo "❌ Migration failed!"
  echo ""
  echo "Error details:"
  echo "$RESULT" | jq '.' 2>/dev/null || echo "$RESULT"
  exit 1
fi

# Verify
echo ""
echo "🔍 Verifying migrations..."
VERIFY=$(curl -s "$PROD_URL/api/admin/run-migrations-034-035")
echo "$VERIFY" | jq '.' 2>/dev/null || echo "$VERIFY"
