# RepCard Sync Optimization Plan

**User Preference:** 5-10 minute updates are acceptable (current 5-minute sync is fine)

---

## Current Status

**Sync Setup:**
- ✅ Backend sync: Every 5 minutes via Vercel Cron (`*/5 * * * *`)
- ✅ Incremental sync: Uses `lastSyncTimestamp` to fetch only new records
- ⚠️ Frontend polling: Inconsistent (30s-60s intervals)
- ⚠️ Cache TTLs: 15-30 minutes (too long for 5-minute sync)

**Display Fixes (Already Complete):**
- ✅ Quality metrics showing 4 separate categories (PB, 48h, Both, Neither)
- ✅ Fixed 0% display issues (using database columns)
- ✅ Attribution fixes (setters vs closers)
- ✅ Backfill script created

---

## Optimization Plan (For 5-Minute Sync)

### 1. Sync Frequency
**Status:** ✅ **NO CHANGE NEEDED**
- Current 5-minute sync is acceptable per user preference
- Can optionally increase to 10 minutes if preferred, but 5 minutes is fine

### 2. Reduce Cache TTLs
**Goal:** Ensure cache expires shortly after sync completes

**Files to Update:**
- `app/api/repcard/leaderboard/route.ts` - Reduce from 30 min to 5-6 min
- `app/api/repcard/users/[userId]/stats/route.ts` - Reduce from 15 min to 5-6 min
- `app/api/repcard/unified-dashboard/route.ts` - Add cache with 5-6 min TTL (optional)

**Rationale:** Cache should be slightly longer than sync interval (5 min) to ensure users see updates after sync completes

### 3. Standardize Frontend Polling
**Goal:** Consistent UX - all components check for updates every 30 seconds

**Files to Update:**
- `components/analytics/RepCardUnifiedDashboard.tsx` - Change from 60s to 30s

**Benefit:** Users see updates within 30 seconds after sync completes (instead of waiting up to 60 seconds)

### 4. Optional Enhancements
- Add sync status indicator ("Last synced: X minutes ago")
- Optimize incremental sync queries (use timestamp instead of date)
- Research RepCard webhook support (for instant updates if available)

---

## Expected Results

**Current:**
- Sync: Every 5 minutes ✅
- Cache: 15-30 minutes ⚠️ (too long)
- Frontend polling: 30-60 seconds ⚠️ (inconsistent)
- **Effective delay: 5-30 minutes** (due to long cache)

**After Optimization:**
- Sync: Every 5 minutes ✅ (no change)
- Cache: 5-6 minutes ✅ (matches sync)
- Frontend polling: 30 seconds ✅ (consistent)
- **Effective delay: ~5 minutes** (users see updates within 30 seconds after sync completes)

---

## Files to Modify

1. `app/api/repcard/leaderboard/route.ts` - Reduce cache TTL from 30 min to 5-6 min
2. `app/api/repcard/users/[userId]/stats/route.ts` - Reduce cache TTL from 15 min to 5-6 min
3. `components/analytics/RepCardUnifiedDashboard.tsx` - Reduce polling from 60s to 30s

**Optional:**
4. `app/api/repcard/unified-dashboard/route.ts` - Add cache with 5-6 min TTL
5. `lib/repcard/sync-service.ts` - Optimize incremental sync queries
6. `components/analytics/RepCardUnifiedDashboard.tsx` - Add sync status indicator

---

## Deployment

**Current Status:** Changes are NOT yet deployed (local only)

**To Deploy:**
```bash
git add .
git commit -m "feat: optimize RepCard cache TTLs and frontend polling for 5-minute sync"
git push origin main
```

**Deployment:** Automatic via GitHub Actions when pushed to `main` branch

---

## Summary

**User Preference:** 5-10 minute updates are fine ✅

**Main Work:** Already complete (display fixes, quality metrics)

**Remaining Work:** 
- Reduce cache TTLs to match 5-minute sync
- Standardize frontend polling to 30 seconds
- Optional: Add sync status indicator

**Impact:** Users will see updates within ~30 seconds after each 5-minute sync completes (instead of waiting up to 30 minutes due to long cache)
