# Crew Performance Dashboard Guide

## Overview
The Crew Performance Dashboard provides comprehensive analytics and metrics for field crew members, tracking task completion, efficiency, customer satisfaction, and on-time performance.

## Features

### Metrics Tracked
- **Tasks Completed:** Daily, weekly, and monthly completion counts
- **Average Completion Time:** Mean duration from STARTED to COMPLETE status
- **On-Time Percentage:** Percentage of tasks started on or before scheduled time
- **Customer Ratings:** Average rating from customer feedback (1-5 stars)
- **Active Tasks:** Currently assigned tasks (NOT_STARTED, ENROUTE, STARTED)
- **Exception Rate:** Frequency of EXCEPTION events reported

### Dashboard Sections

1. **Metrics Overview:**
   - 6 metric cards showing team-wide aggregates
   - Real-time updates every 60 seconds
   - Color-coded indicators for performance levels

2. **Top Performers:**
   - Leaderboard showing top 3-5 crew members
   - Ranked by tasks completed, on-time percentage, or customer ratings
   - Trophy/award icons for recognition

3. **Needs Support:**
   - Identifies crew members with performance issues
   - Highlights low on-time percentage, high exceptions, low ratings
   - Provides suggested actions for improvement

4. **Crew Comparison:**
   - Horizontal bar chart comparing all crew members
   - Team average reference line
   - Selectable metrics (tasks, time, on-time %, ratings)
   - Color-coded bars (green above average, red below)

5. **Performance Table:**
   - Sortable table with all crew metrics
   - Click row to filter dashboard to specific crew member
   - Export to CSV functionality

## Filters

### Time Range
- Last 7 Days
- Last 30 Days (default)
- Last 90 Days
- All Time

### Crew Member
- All Crew (default)
- Individual crew member selection

### Task Type
- All Tasks (default)
- Survey
- Install
- Inspection
- Service

## Data Sources

All metrics are calculated from Arrivy data:
- **arrivy_tasks:** Task assignments and scheduling
- **arrivy_task_status:** Status updates with timestamps
- **arrivy_events:** Customer ratings and exceptions
- **arrivy_entities:** Crew member information

## Calculations

### Tasks Completed
```sql
COUNT(*) WHERE status_type = 'COMPLETE' 
AND reported_at >= [date_boundary]
GROUP BY entity_id
```

### Average Completion Time
```sql
AVG(EXTRACT(EPOCH FROM (complete_time - start_time)) / 60)
WHERE status_type IN ('STARTED', 'COMPLETE')
```

### On-Time Percentage
```sql
(COUNT(*) WHERE started_at <= scheduled_start) / COUNT(*) * 100
```

### Customer Rating Average
```sql
AVG(extra_fields->>'rating'::integer)
FROM arrivy_events
WHERE event_type = 'TASK_RATING'
```

## Usage

### Accessing the Dashboard
1. Navigate to Operations → Crew Performance
2. Select desired time range and filters
3. View metrics, charts, and tables

### Exporting Data
1. Click "Export to CSV" button in header
2. CSV file downloads with all crew metrics
3. Filename format: `crew-performance-{timeRange}-{date}.csv`

### Identifying Issues
1. Check "Needs Support" card for crew members with issues
2. Review specific metrics in performance table
3. Use comparison chart to identify outliers
4. Click crew member to view detailed breakdown

## Performance Considerations

- **Caching:** Results cached for 60 seconds to reduce database load
- **Auto-Refresh:** Dashboard updates every 60 seconds
- **Query Optimization:** Uses indexed columns for fast aggregation
- **Pagination:** Performance table shows all crew members (typically < 50)

## Troubleshooting

### No Crew Members Showing
- Verify entities synced from Arrivy: `npm run sync:arrivy:entities`
- Check database: `SELECT COUNT(*) FROM arrivy_entities;`
- Ensure crew members have assigned tasks

### Metrics Show Zero
- Verify tasks exist in selected time range
- Check task status history has COMPLETE statuses
- Confirm crew members are assigned to tasks (assigned_entity_ids)

### Ratings Not Appearing
- Verify TASK_RATING webhook is enabled
- Check arrivy_events table for TASK_RATING events
- Confirm customers are leaving ratings via tracker URLs

## Future Enhancements

- Individual crew detail pages with task history
- Trend charts showing performance over time
- Automated performance reports via email
- Goal setting and tracking per crew member
- Integration with QuickBase for project context



