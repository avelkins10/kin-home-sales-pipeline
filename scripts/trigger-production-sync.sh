#!/bin/bash
# Trigger RepCard Sync in Production
#
# This script triggers a RepCard sync in production via the API.
# You need to be authenticated and have admin access.
#
# Usage: ./scripts/trigger-production-sync.sh [your-domain.com]

set -e

DOMAIN=${1:-"your-domain.com"}

echo "🚀 Triggering RepCard Sync in Production"
echo "=========================================="
echo ""
echo "Domain: https://$DOMAIN"
echo ""

# Check if domain is set
if [ "$DOMAIN" = "your-domain.com" ]; then
    echo "❌ Please provide your production domain:"
    echo "   ./scripts/trigger-production-sync.sh your-actual-domain.com"
    exit 1
fi

echo "⚠️  This will trigger a full RepCard sync in production."
echo "⚠️  This may take 2-5 minutes to complete."
echo ""
read -p "Continue? (yes/no): " confirm

if [ "$confirm" != "yes" ]; then
    echo "❌ Cancelled"
    exit 1
fi

echo ""
echo "📡 Triggering sync..."
echo ""

# Try to trigger sync (requires authentication)
# Note: You'll need to provide auth token or use browser session
SYNC_URL="https://$DOMAIN/api/admin/repcard/sync"

echo "Sync endpoint: $SYNC_URL"
echo ""
echo "To trigger sync, you can:"
echo ""
echo "Option 1: Via Admin Dashboard (Recommended)"
echo "  1. Navigate to: https://$DOMAIN/admin/repcard-sync"
echo "  2. Click 'Run Full Sync' button"
echo ""
echo "Option 2: Via API (requires auth token)"
echo "  curl -X POST $SYNC_URL \\"
echo "    -H 'Authorization: Bearer YOUR_AUTH_TOKEN' \\"
echo "    -H 'Content-Type: application/json'"
echo ""
echo "Option 3: Wait for automatic sync (runs every 5 minutes)"
echo "  The cron job will sync automatically if:"
echo "  - REPCARD_API_KEY is set ✅"
echo "  - CRON_SECRET is set ✅"
echo "  - Cron is enabled in Vercel ✅"
echo ""



