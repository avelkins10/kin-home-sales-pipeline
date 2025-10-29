# RepCard Integration Fixes

## Issues Identified

### 1. ❌ Missing Environment Variables
**Problem:** `REPCARD_API_KEY` and `REPCARD_API_URL` were not documented in `env.example`

**Impact:** Developers didn't know they needed to set these variables, causing API calls to fail silently

**Fix:** Added RepCard configuration section to `env.example` with clear documentation

---

### 2. ❌ Type Mismatch in API Parameters
**Problem:** The `setterIds` parameter in `getAppointments()` expects a string (comma-separated IDs), but was receiving a number from `user.repcard_user_id`

**Location:** `app/api/repcard/users/[userId]/stats/route.ts` line 233

**Impact:** API calls would fail or return incorrect data due to parameter type mismatch

**Fix:** 
- Convert `user.repcard_user_id` to string: `setterIds: String(user.repcard_user_id)`
- Ensure `userId` parameter for `getCustomers()` is properly typed

---

### 3. ❌ Poor Error Handling
**Problem:** API errors were not properly caught or logged, making debugging difficult

**Impact:** 
- Silent failures when RepCard API was unavailable
- No visibility into what was failing
- Infinite loops possible if pagination failed

**Fix:**
- Added try-catch blocks around all API calls
- Added error logging with context (userId, repcardUserId, page number)
- Added safety limits to prevent infinite pagination loops (max 200 pages)
- Graceful degradation - return empty arrays/metrics instead of failing completely

---

### 4. ❌ Missing Error Handling in Frontend
**Problem:** BaseballCard component didn't handle API errors gracefully

**Impact:** Users saw generic error messages or broken UI when API calls failed

**Fix:**
- Improved error parsing to extract meaningful error messages
- Added retry logic (2 retries with 1 second delay)
- Better error logging in console
- Proper handling of `hasRepcardData: false` responses

---

## How to Fix in Your Environment

### Step 1: Set Environment Variables

Add these to your `.env.local` (for local development) or Vercel environment variables (for production):

```bash
# RepCard API Key (obtain from RepCard dashboard)
REPCARD_API_KEY=your_repcard_api_key_here

# RepCard API Base URL (default: https://api.repcard.com)
REPCARD_API_URL=https://api.repcard.com
```

**To get your RepCard API key:**
1. Log into RepCard dashboard
2. Go to Settings → API
3. Generate or copy your API key
4. Set it in your environment

### Step 2: Verify User Mappings

Users need `repcard_user_id` set in the database to fetch RepCard data. Check this with:

```sql
SELECT id, name, email, repcard_user_id 
FROM users 
WHERE repcard_user_id IS NOT NULL;
```

If users are missing `repcard_user_id`, they need to be synced from QuickBase Contacts table or added manually.

### Step 3: Test the Integration

Run the diagnostic script to verify everything is working:

```bash
# Set your API key temporarily for testing
export REPCARD_API_KEY="your-key-here"

# Run diagnostic
npx tsx scripts/diagnose-repcard.ts
```

### Step 4: Check Logs

If issues persist, check:
1. **Server logs** - Look for RepCard API errors
2. **Browser console** - Check for frontend errors
3. **Network tab** - Verify API requests are being made

Common error messages:
- `RepCard API key is required` → Set `REPCARD_API_KEY`
- `401 Unauthorized` → Invalid API key
- `429 Too Many Requests` → Rate limit exceeded, wait and retry
- `User not linked to RepCard` → User missing `repcard_user_id`

---

## Testing Checklist

- [ ] `REPCARD_API_KEY` is set in environment
- [ ] `REPCARD_API_URL` is set (or using default)
- [ ] At least one user has `repcard_user_id` set
- [ ] Diagnostic script runs without errors
- [ ] BaseballCard component loads without errors
- [ ] Metrics display correctly (not all zeros)
- [ ] Error messages are user-friendly when data is missing

---

## Code Changes Summary

### Files Modified:
1. `env.example` - Added RepCard configuration section
2. `app/api/repcard/users/[userId]/stats/route.ts` - Fixed type issues, added error handling
3. `components/rep/BaseballCard.tsx` - Improved error handling and retry logic

### Key Improvements:
- ✅ Proper type conversion for API parameters
- ✅ Comprehensive error handling and logging
- ✅ Safety limits to prevent infinite loops
- ✅ Graceful degradation on API failures
- ✅ Better user-facing error messages
- ✅ Retry logic for transient failures

---

## Next Steps

1. **Set the API key** in your environment (critical!)
2. **Run the diagnostic script** to verify connectivity
3. **Check user mappings** - ensure users have `repcard_user_id`
4. **Test the BaseballCard** component with a real user
5. **Monitor logs** for any remaining issues

If you continue to see issues after setting the API key, check:
- Is the API key valid? (Test with diagnostic script)
- Do users have `repcard_user_id` set?
- Is RepCard API accessible from your server?
- Are there rate limits being hit?

