# RepCard Real-Time Updates Guide

## Current Status: Near Real-Time (5 minutes)

**Yes, new doors knocked and appointments set will appear in your leaderboard!**

However, there's a **5-minute delay** because:
- Incremental sync runs every 5 minutes via Vercel Cron
- It pulls all changes since the last sync
- Leaderboard automatically reflects the updated data

---

## How It Works

### 1. Scheduled Incremental Sync

**Current Configuration** (`vercel.json`):
```json
{
  "path": "/api/cron/repcard-sync",
  "schedule": "*/5 * * * *"  // Every 5 minutes
}
```

**What gets synced every 5 minutes:**
- ✅ Users/Reps (updated profiles)
- ✅ Offices
- ✅ Customers (new door knocks)
- ✅ Appointments (new appointments set)
- ✅ Status Logs (status changes)
- ⏭️ Attachments (skipped for speed)

**Timeline:**
```
Rep knocks door in RepCard
    ↓
[0-5 minutes wait]
    ↓
Cron runs incremental sync
    ↓
New customer record appears in database
    ↓
Leaderboard updates (reads from database)
    ↓
Rep sees updated stats!
```

---

## Making It More Real-Time

### Option 1: More Frequent Syncs (Current: 5 min)

**Make it every 2 minutes:**
```json
{
  "path": "/api/cron/repcard-sync",
  "schedule": "*/2 * * * *"  // Every 2 minutes
}
```

**Pros:**
- Faster updates (2 min delay)
- Simple to implement

**Cons:**
- More API calls to RepCard
- Higher server costs
- Still not instant

---

### Option 2: Frontend Polling (Recommended)

Add automatic refresh to leaderboard pages:

```typescript
// In your leaderboard component
const { data, refetch } = useQuery({
  queryKey: ['leaderboard', ...],
  queryFn: fetchLeaderboard,
  refetchInterval: 30000, // Check every 30 seconds
  staleTime: 60000, // Consider data stale after 1 minute
});
```

**Benefits:**
- Leaderboard auto-refreshes every 30 seconds
- Users see updates as soon as sync completes
- No code changes needed on backend

**Implementation:**
- Already implemented in some components (`useRealtimeUpdates` hook exists)
- Just needs to be added to leaderboard components

---

### Option 3: RepCard Webhooks (Best for Instant Updates)

If RepCard supports webhooks, this would be **instant**:

1. RepCard sends webhook when door is knocked
2. Your endpoint receives notification
3. Immediately sync that specific record
4. Leaderboard updates instantly

**Check RepCard API docs for webhook support:**
- Look for "webhooks" or "notifications" in API docs
- Common events: `customer.created`, `appointment.created`, `status.changed`

**If webhooks exist, create:**
```
/api/webhooks/repcard/customer-created
/api/webhooks/repcard/appointment-created
```

Then sync immediately on webhook receipt.

---

## Frontend Real-Time Updates

### Add Auto-Refresh to Leaderboard

Update your leaderboard component to poll for updates:

```typescript
// components/analytics/ConfigurableLeaderboard.tsx
import { useQuery } from '@tanstack/react-query';

export function ConfigurableLeaderboard({ ... }) {
  const { data, isLoading } = useQuery({
    queryKey: ['leaderboard', metric, timeRange, ...],
    queryFn: async () => {
      const res = await fetch(`/api/repcard/leaderboard?...`);
      return res.json();
    },
    refetchInterval: 30000, // Auto-refresh every 30 seconds
    staleTime: 60000, // Data is fresh for 1 minute
  });

  // ... rest of component
}
```

**User Experience:**
- Opens leaderboard → sees current data
- After 30 seconds → automatically checks for updates
- If sync completed, sees new data
- If not, keeps showing cached data

---

## Current Behavior Summary

| Action | Delay | How It Works |
|--------|-------|--------------|
| Rep knocks door | 0-5 min | Cron sync runs every 5 min, pulls new customers |
| Rep sets appointment | 0-5 min | Cron sync runs every 5 min, pulls new appointments |
| Status changes | 0-5 min | Cron sync runs every 5 min, pulls status logs |
| Leaderboard displays | Instant | Reads from synced database (fast!) |

**Total Delay:** ~5 minutes maximum (usually less)

---

## Testing Real-Time Updates

### Test Current Sync Speed

1. **Knock a door in RepCard**
2. **Wait 5 minutes**
3. **Check leaderboard** - should see updated count

### Speed Up Testing

Manually trigger sync immediately:

```bash
# Via API (if logged in as super_admin)
curl -X POST "https://your-domain.com/api/admin/repcard/comprehensive-sync?incremental=true" \
  -H "Cookie: your-session-cookie"

# Via script
npx tsx scripts/run-comprehensive-repcard-sync.ts --incremental
```

Then refresh leaderboard to see updates immediately.

---

## Recommendations

### For Near Real-Time (Current Setup)

✅ **Current: 5-minute syncs + frontend polling**
- Good balance of speed vs. API calls
- Users see updates within 5 minutes
- Frontend polling makes it feel faster

### For Faster Updates

**Option A: Increase sync frequency**
```json
"schedule": "*/2 * * * *"  // Every 2 minutes
```

**Option B: Add frontend polling** (recommended)
- Keep 5-minute syncs
- Add 30-second frontend refresh
- Users see updates as soon as sync completes

### For Instant Updates

**Use RepCard webhooks** (if available)
- Instant notifications
- Sync only changed records
- True real-time updates

---

## Monitoring Sync Status

Check sync logs to see when last sync ran:

```sql
SELECT 
  entity_type,
  started_at,
  completed_at,
  status,
  records_inserted,
  records_updated
FROM repcard_sync_log
WHERE entity_type IN ('customers', 'appointments')
ORDER BY started_at DESC
LIMIT 10;
```

Or use the admin endpoint:

```bash
GET /api/admin/repcard/comprehensive-sync
```

---

## Summary

**Yes, your leaderboard will update automatically!**

- ✅ New doors knocked → appear in ~5 minutes
- ✅ New appointments → appear in ~5 minutes  
- ✅ Updates happen automatically via cron
- ✅ No manual sync needed

**To make it feel faster:**
1. Add frontend polling (auto-refresh every 30s)
2. Optionally increase sync frequency to every 2 minutes
3. If RepCard has webhooks, use those for instant updates

The current 5-minute delay is a good balance, but you can make it feel more "live" with frontend polling!

