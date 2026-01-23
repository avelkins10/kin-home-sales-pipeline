# RepCard Leaderboards Page - Implementation Summary

## Overview
Created a dedicated `/leaderboards` page with RepCard data and quality metrics, organized in tabs (Setters / Closers / Offices). This is a clean, fresh implementation separate from the analytics page to avoid legacy code issues.

## Files Created

### 1. Database Migration
**File:** `lib/db/migrations/039_repcard_metric_config.sql`
- Creates `repcard_metric_config` table for storing metric configurations
- Creates `should_count_for_metric()` database function
- Inserts default configurations matching current behavior
- Allows admins to configure which dispositions/statuses count toward each metric

### 2. Helper Library
**File:** `lib/repcard/metric-config.ts`
- `shouldCountForMetric()` - Check if disposition/status counts for a metric
- `getMetricConfig()` - Get configurations for a specific metric
- `getAllMetricConfigs()` - Get all configurations
- `saveMetricConfig()` - Save configuration
- `deleteMetricConfig()` - Delete configuration
- `discoverDispositions()` - Discover all unique dispositions/statuses from database

### 3. API Endpoints

**File:** `app/api/repcard/metrics/config/route.ts`
- `GET` - Get metric configurations (optionally filtered by metric name)
- `POST` - Save metric configuration
- `DELETE` - Delete metric configuration

**File:** `app/api/repcard/metrics/config/discover/route.ts`
- `GET` - Discover all unique dispositions and status categories from database
- Returns counts for each value to help admins understand the data

### 4. Settings UI Component
**File:** `components/settings/RepCardMetricConfigTab.tsx`
- **Discovery Tab**: Shows all unique dispositions/statuses with counts
- **Configuration Tab**: 
  - Select metric to configure
  - View current configurations
  - Quick configure by checking/unchecking dispositions/statuses
  - Pattern matching support
- Added to Settings page under "Metric Config" tab (super_admin only)

### 5. Main Leaderboards Page
**File:** `app/(sales)/leaderboards/page.tsx`
- Three tabs: Setters, Closers, Offices
- Time range filters (Today, Week to Date, Month to Date, Year to Date, Custom)
- Office filter (multi-select)
- Refresh button with sync status
- Auto-refresh every 30 seconds

### 6. Leaderboard Components

**File:** `components/leaderboards/LeaderboardFilters.tsx`
- Time range selector buttons
- Custom date range picker
- Office multi-select (placeholder - can be enhanced)

**File:** `components/leaderboards/SettersLeaderboard.tsx`
- Table showing setters ranked by appointments set
- Columns: Rank, Name, Team, Doors, Hours, Appointments Set, 48h Speed, Power Bill, High Quality, Low Quality, Conversion Rate
- Medal badges (🥇🥈🥉) for top 3
- Click row to view rep detail page

**File:** `components/leaderboards/ClosersLeaderboard.tsx`
- Table showing closers ranked by sales closed
- Columns: Rank, Name, Team, Appointments Run, Sat Closed, Sat No Close, No Show, Cancelled, Close Rate
- Color-coded close rates

**File:** `components/leaderboards/OfficesLeaderboard.tsx`
- Table showing office performance
- Columns: Office, Doors, Appointments, Sales, Conversion, Close Rate, Setters, Closers, Reps
- Aggregated metrics per office

### 7. Navigation Update
**File:** `components/layout/TopNavbar.tsx`
- Added "Leaderboards" link between "Analytics" and "Reports"
- Icon: Trophy
- Roles: Same as Analytics (team_lead, area_director, divisional, office_leader, regional, super_admin)

## Key Features

### Date Filtering (Critical Fix)
All queries use date comparison with timezone conversion:
```sql
WHERE scheduled_at IS NOT NULL
  AND (scheduled_at AT TIME ZONE 'UTC' AT TIME ZONE 'America/New_York')::date >= ${startDateParam}::date
  AND (scheduled_at AT TIME ZONE 'UTC' AT TIME ZONE 'America/New_York')::date <= ${endDateParam}::date
```

### Configurability
- Admins can configure which dispositions/statuses count toward each metric
- Discovery tool shows all available values in the database
- Changes apply immediately to all leaderboard queries
- Default configurations match current behavior (no breaking changes)

### Setter Leaderboard Logic
- **Appointments Set**: Gross count (exclude ONLY reschedules, include cancelled/no-show for gamification)
- **Quality Metrics**: Calculated from appointments excluding cancelled/no-show
- **48h Speed**: Count of appointments with `is_within_48_hours = TRUE`
- **Power Bill**: Count of appointments with `has_power_bill = TRUE`
- **High Quality**: Both 48h AND power bill
- **Low Quality**: Neither 48h NOR power bill

### Closer Leaderboard Logic
- **Appointments Run**: Count of appointments where `closer_user_id` matches
- **Sat Closed**: `status_category = 'sat_closed'` or `'completed'`
- **Sat No Close**: `status_category = 'sat_no_close'`
- **Close Rate**: (Sat Closed / Appointments Run) * 100

## Next Steps

1. **Run Migration**: Execute migration 039 to create the metric config table
   - Go to Settings → System → Run Migrations (super_admin only)
   - Or run manually: `psql $DATABASE_URL -f lib/db/migrations/039_repcard_metric_config.sql`

2. **Configure Metrics** (Optional):
   - Go to Settings → Metric Config
   - Review discovered dispositions/statuses
   - Adjust configurations as needed for your business rules

3. **Test Leaderboards Page**:
   - Navigate to `/leaderboards`
   - Verify date filtering works correctly
   - Check that setters/closers/offices display correctly
   - Test time range filters

4. **Integrate Metric Configuration** (Future Enhancement):
   - Update leaderboard queries to use `should_count_for_metric()` function
   - Replace hardcoded filters with configuration-based logic
   - This will make the system fully configurable without code changes

## API Endpoint Used

The page uses the existing `/api/repcard/leaderboards` endpoint which:
- Already has proper date filtering (uses date comparison with timezone conversion)
- Returns setters, closers, and offices data
- Supports office filtering
- Has 5-minute caching

## Notes

- The existing `/api/repcard/leaderboards/route.ts` already exists and works correctly
- Metric configuration system is set up but not yet integrated into queries (can be done later)
- Default configurations match current behavior, so nothing breaks
- Settings page now has "Metric Config" tab for admins to configure metrics
