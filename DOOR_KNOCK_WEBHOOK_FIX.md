# Door Knock Webhook Fix

**Issue:** Door knocks not being stored in `repcard_door_knocks` table (0 rows)

**Root Cause:** Type mismatch in webhook processor - casting INTEGER columns to TEXT

**Fix Applied:** Updated `lib/repcard/webhook-processor.ts` to use INTEGER types instead of TEXT

---

## Changes Made

### 1. Fixed Type Casting in Door Knock Webhook Processor

**Before (Line 753):**
```typescript
WHERE repcard_user_id = ${setterUserId.toString()}::text
```

**After:**
```typescript
const setterUserIdInt = parseInt(setterUserId.toString());
WHERE repcard_user_id = ${setterUserIdInt}
```

**Before (Line 799):**
```typescript
${setterUserId.toString()}::text,
${repcardCustomerId ? repcardCustomerId.toString() : null}::text,
```

**After:**
```typescript
${parseInt(setterUserId.toString())},
${repcardCustomerId ? parseInt(repcardCustomerId.toString()) : null},
```

---

## Why This Matters

The `repcard_door_knocks` table uses INTEGER columns (from migration 018):
- `setter_user_id INTEGER NOT NULL`
- `repcard_customer_id INTEGER`

But the webhook processor was casting to TEXT, causing:
1. Type mismatch errors (silently caught)
2. Failed inserts (webhook returns success but data isn't stored)
3. 0 rows in the table

---

## Verification

After this fix, door knock webhooks should:
1. ✅ Successfully insert into `repcard_door_knocks` table
2. ✅ Link to customers via `repcard_customer_id`
3. ✅ Track office_id from setter
4. ✅ Store all door knock metadata (status, distance, verified, etc.)

---

## Testing

1. **Trigger a door knock in RepCard** (or simulate webhook)
2. **Check database:**
   ```sql
   SELECT COUNT(*) FROM repcard_door_knocks;
   SELECT * FROM repcard_door_knocks ORDER BY created_at DESC LIMIT 5;
   ```
3. **Check logs** for webhook processing:
   - Look for `[RepCard Webhook Processor] Door knock processed`
   - Should show `wasInserted: true`

---

## Next Steps

1. ✅ Fix applied to code
2. ⏭️ Deploy to production
3. ⏭️ Monitor webhook logs for door knock events
4. ⏭️ Verify door knocks are being stored

---

## Related Files

- `lib/repcard/webhook-processor.ts` - Webhook processing logic
- `app/api/webhooks/repcard/route.ts` - Webhook endpoint
- `lib/db/migrations/036_create_repcard_door_knocks_table.sql` - Table schema
- `lib/db/migrations/018_normalize_repcard_user_ids_to_integer.sql` - Type normalization
