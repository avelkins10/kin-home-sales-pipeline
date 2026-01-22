# Customer Sync Instructions

## Current Situation

- **2800 customers** exist in database (from 2020-2025)
- **560 recent appointments** (last 30 days) reference customer IDs that **don't exist** in database
- **0% metrics** because customers are missing (can't calculate `is_within_48_hours` without `customer.created_at`)

## Solution: Run Customer Sync

### Option 1: Admin Dashboard (Easiest) ✅

1. Go to **Settings** → **RepCard** tab
2. Find the **"Fix Missing Customers"** button (large, prominent)
3. Click it - this will:
   - Sync missing customers from RepCard
   - Link appointments to customers
   - Recalculate metrics automatically

### Option 2: API Endpoint

Call the API endpoint directly (requires super_admin authentication):

```bash
POST /api/admin/repcard/fix-missing-customers
```

Or with date range:

```bash
POST /api/admin/repcard/fix-missing-customers?startDate=2025-12-01&endDate=2026-01-22
```

### Option 3: Comprehensive Sync

Run a full comprehensive sync (syncs everything):

```bash
POST /api/admin/repcard/comprehensive-sync?skipAppointments=true&skipStatusLogs=true
```

This will sync customers (and users/offices) but skip appointments since we already have them.

## What Happens After Sync

1. ✅ Missing customers will be synced from RepCard
2. ✅ Appointments will be linked to customers (`customer_id` FK set)
3. ✅ Database triggers will automatically calculate:
   - `is_within_48_hours` (compares `appointment.scheduled_at` to `customer.created_at`)
   - `has_power_bill` (checks for attachments)
4. ✅ Dashboard will show correct percentages

## Expected Results

After sync, you should see:
- **48-Hour Speed**: >0% (appointments scheduled within 48h of customer creation)
- **Power Bill Rate**: >0% (appointments/customers with attachments)
- **High Quality**: >0% (appointments with both PB and 48h)

## Verification

After sync, check:
1. Dashboard metrics should show non-zero percentages
2. Settings → RepCard → "Check Customer Sync" should show 0 missing customers
3. Recent appointments should have `customer_id` set (not NULL)
