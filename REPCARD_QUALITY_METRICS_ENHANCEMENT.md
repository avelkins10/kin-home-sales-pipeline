# RepCard Quality Metrics Enhancement

**Date:** 2025-01-28  
**Status:** ✅ **COMPLETE**

---

## 🎯 Enhancement Overview

Enhanced quality metrics to track **4 separate categories** of appointment quality:

1. **Appointments with Power Bill** (regardless of 48h status)
2. **Appointments within 48 Hours** (regardless of power bill status)
3. **Appointments with BOTH** (power bill AND within 48h) - **HIGH QUALITY**
4. **Appointments with NEITHER** (no power bill AND not within 48h) - **LOW QUALITY**

---

## 📊 Business Value

### For Setters
- **Input Quality**: See how many appointments have power bills (thoroughness)
- **Speed Quality**: See how many appointments are set within 48h (responsiveness)
- **High Quality**: See appointments with both (best practices)
- **Low Quality**: See appointments with neither (improvement opportunities)

### For Closers
- **Cancellation Analysis**: Understand why appointments cancel
  - Low quality appointments (neither PB nor 48h) have higher cancellation risk
  - High quality appointments (both PB and 48h) have higher likelihood to sit
- **Quality Correlation**: See relationship between setter quality and appointment outcomes

---

## ✅ Changes Made

### 1. Unified Dashboard API (`app/api/repcard/unified-dashboard/route.ts`)

**Enhanced Quality Metrics Query:**
```sql
SELECT
  COUNT(DISTINCT a.id) FILTER (WHERE a.is_within_48_hours = TRUE)::int as within_48h,
  COUNT(DISTINCT a.id) FILTER (WHERE a.has_power_bill = TRUE)::int as with_power_bill,
  COUNT(DISTINCT a.id) FILTER (WHERE a.is_within_48_hours = TRUE AND a.has_power_bill = TRUE)::int as both,
  COUNT(DISTINCT a.id) FILTER (WHERE a.is_within_48_hours = FALSE AND a.has_power_bill = FALSE)::int as neither
```

**Response Structure:**
```typescript
{
  totalAppointments: number,
  appointmentSpeed: number, // % within 48h
  powerBillRate: number, // % with power bill
  withBoth: {
    count: number,
    percentage: number
  },
  withNeither: {
    count: number,
    percentage: number
  },
  rescheduleRate: number
}
```

### 2. Quality Tile Component (`components/analytics/tiles/QualityTile.tsx`)

**Added:**
- New "Quality Breakdown" section showing:
  - **High Quality** (green): Appointments with both PB and 48h
  - **Low Quality** (red): Appointments with neither

**Display:**
- Shows count and percentage for each category
- Color-coded for quick visual identification
- Includes helpful context ("Higher likelihood to sit" / "Higher cancellation risk")

### 3. Leaderboard Tile (`components/analytics/tiles/LeaderboardTile.tsx`)

**Enhanced:**
- Added `bothCount` and `neitherCount` to RepData interface
- Display badges showing:
  - "X both" (green badge) - High quality appointments
  - "X neither" (red badge) - Low quality appointments

### 4. Leaderboard Query (`app/api/repcard/unified-dashboard/route.ts`)

**Enhanced:**
- Top appointment setters query now includes:
  - `both_count`: Appointments with both PB and 48h
  - `neither_count`: Appointments with neither

---

## 📋 Metrics Breakdown

### Individual Metrics
- **48-Hour Speed**: % of appointments scheduled within 48 hours of lead creation
- **Power Bill Rate**: % of appointments with power bill attachments

### Combined Metrics
- **High Quality (Both)**: 
  - Count: Appointments with BOTH power bill AND within 48h
  - Percentage: % of total appointments
  - **Impact**: Higher likelihood to sit with closer
  
- **Low Quality (Neither)**:
  - Count: Appointments with NEITHER power bill NOR within 48h
  - Percentage: % of total appointments
  - **Impact**: Higher cancellation risk

---

## 🎨 UI Display

### Quality Tile
```
┌─────────────────────────────────────┐
│ Appointment Quality                 │
│ Based on 465 appointments           │
├─────────────────────────────────────┤
│ [48h Speed] [Reschedule] [PB Rate] │
│                                     │
│ Quality Breakdown                   │
│ ┌─────────────┐ ┌─────────────┐   │
│ │ High Quality│ │ Low Quality │   │
│ │ 150 (32.3%) │ │ 89 (19.1%)  │   │
│ │ Both PB&48h │ │ Neither     │   │
│ └─────────────┘ └─────────────┘   │
└─────────────────────────────────────┘
```

### Leaderboard
```
Appointments Set
1. John Doe: 38
   [12 in 48h] [15 w/PB] [8 both] [5 neither] [45%]
```

---

## 📝 Files Modified

1. ✅ `app/api/repcard/unified-dashboard/route.ts` - Enhanced queries and response
2. ✅ `components/analytics/tiles/QualityTile.tsx` - Added quality breakdown section
3. ✅ `components/analytics/tiles/LeaderboardTile.tsx` - Added both/neither badges

---

## 🚀 Next Steps

1. **Test Display** - Verify all 4 metrics show correctly
2. **Verify Calculations** - Ensure counts and percentages are accurate
3. **Add to User Stats** - Consider adding these metrics to individual user stats
4. **Add to Reports** - Consider including in export/report functionality

---

## 🎉 Summary

**Before:** Only showed individual metrics (48h %, PB %)  
**After:** Shows individual metrics + combined quality breakdown (both/neither)

**Status:** ✅ **COMPLETE - READY FOR TESTING!**
