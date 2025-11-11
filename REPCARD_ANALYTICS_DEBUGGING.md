# RepCard Analytics Debugging Guide

## Problem
RepCard analytics are showing zeros in the analytics dashboard. This guide helps identify and fix the root cause.

## What I've Added

### 1. Diagnostic API Endpoint
**File**: `app/api/repcard/diagnostic/route.ts`

A new diagnostic endpoint that checks:
- ✅ Environment variables (REPCARD_API_KEY)
- ✅ API connection to RepCard
- ✅ Database tables existence
- ✅ Data counts in database
- ✅ User linking status
- ✅ Date range data availability

**Access**: Only Super Admins can access this endpoint
**URL**: `/api/repcard/diagnostic`

### 2. Diagnostic Banner Component
**File**: `components/analytics/RepCardDiagnosticBanner.tsx`

A visual banner that appears at the top of the RepCard analytics tab showing:
- ✅ Green banner when everything is healthy
- ⚠️ Yellow alert with specific issues and recommended fixes
- 🔄 Refresh button to check status again
- 🔗 Link to sync dashboard

**Visibility**: Only visible to Super Admins

### 3. Enhanced Error Messages
**File**: `components/analytics/RepCardOverviewCard.tsx`

Added helpful warning message when all metrics are zero, explaining possible causes:
- No data synced yet
- Users not linked to RepCard
- Data outside selected time range

## How to Use

### Step 1: Check the Diagnostic Banner

1. Navigate to **Analytics** → **RepCard** tab
2. If you're a Super Admin, you'll see a diagnostic banner at the top
3. The banner will show:
   - ✅ **Green**: Everything is working
   - ⚠️ **Yellow**: Issues found with specific recommendations

### Step 2: Review the Recommendations

The diagnostic banner provides specific, actionable recommendations:

#### Common Issue: "REPCARD_API_KEY not set"
**Fix**:
1. Get API key from https://www.repcard.com/settings/api
2. Add to Vercel production: `vercel env add REPCARD_API_KEY production`
3. Redeploy after adding

#### Common Issue: "No data synced from RepCard"
**Fix**:
1. Navigate to `/admin/repcard-sync`
2. Click "Run Full Sync"
3. Wait 2-5 minutes for sync to complete

#### Common Issue: "No users linked to RepCard"
**Fix**:
Run SQL to link users by email:
```sql
UPDATE users u
SET repcard_user_id = ru.repcard_user_id::text
FROM repcard_users ru
WHERE LOWER(u.email) = LOWER(ru.email)
  AND u.repcard_user_id IS NULL;
```

### Step 3: Check API Endpoints Directly

If you want to check the diagnostic programmatically:

```bash
# Get your session token from browser dev tools
curl https://your-domain.com/api/repcard/diagnostic \
  -H "Cookie: next-auth.session-token=YOUR_SESSION_TOKEN"
```

### Step 4: Check Server Logs

The leaderboard API route has extensive logging. Check Vercel logs for:
- `[RepCard Leaderboard]` - Query execution details
- `[RepCard Leaderboard] DEBUG` - Data availability checks
- `[RepCard Leaderboard] ⚠️` - Warnings about filters or data issues

## Common Root Causes

### 1. Missing API Key (Most Common)
**Symptom**: All metrics show 0, diagnostic shows "REPCARD_API_KEY not set"
**Fix**: Add API key to Vercel production environment and redeploy

### 2. No Data Synced
**Symptom**: Diagnostic shows 0 records in database
**Fix**: Run initial sync via `/admin/repcard-sync`

### 3. Users Not Linked
**Symptom**: Diagnostic shows 0 linked users
**Fix**: Link users by email (see SQL above)

### 4. Date Range Issues
**Symptom**: Data exists but shows 0 for current month
**Fix**: Check if data exists in different time range, adjust filters

### 5. Office Filter Too Restrictive
**Symptom**: Leaderboard shows 0 entries but data exists
**Fix**: The API automatically falls back to all offices if office filter returns 0 users

## Testing Checklist

After fixing issues, verify:

- [ ] Diagnostic banner shows green (healthy status)
- [ ] RepCard Overview shows non-zero metrics
- [ ] Leaderboards display data
- [ ] Quality metrics show values
- [ ] No errors in browser console
- [ ] No errors in Vercel function logs

## Next Steps

1. **Check the diagnostic banner** - It will tell you exactly what's wrong
2. **Follow the recommendations** - Each issue has specific fix steps
3. **Refresh the status** - Click refresh to verify fixes worked
4. **Check logs** - If still not working, check Vercel logs for detailed errors

## Files Modified

- ✅ `app/api/repcard/diagnostic/route.ts` - New diagnostic endpoint
- ✅ `components/analytics/RepCardDiagnosticBanner.tsx` - New diagnostic banner
- ✅ `components/analytics/RepCardOverviewCard.tsx` - Enhanced error messages
- ✅ `app/(sales)/analytics/page.tsx` - Added diagnostic banner to RepCard tab

## Need More Help?

If the diagnostic banner doesn't solve the issue:

1. **Check Vercel Logs**: Look for `[RepCard Leaderboard]` messages
2. **Test API Directly**: Use the diagnostic endpoint to get detailed status
3. **Check Database**: Verify tables exist and have data
4. **Verify API Key**: Test RepCard API connection directly

The diagnostic system should identify 90%+ of issues automatically!

