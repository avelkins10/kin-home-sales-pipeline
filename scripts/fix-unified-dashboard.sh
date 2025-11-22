#!/bin/bash
# Quick script to show remaining line numbers that need fixing
echo "Lines with SQL fragments that need fixing:"
grep -n "\${dateFilter}" /Users/austinelkins/Rep_Dashboard/app/api/repcard/unified-dashboard/route.ts
