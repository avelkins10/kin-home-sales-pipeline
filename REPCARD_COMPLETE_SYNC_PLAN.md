# RepCard Complete Data Sync & Settings Configuration Plan

**Date:** 2025-01-28  
**Goal:** Sync everything from RepCard API + Add configurable leaderboard/analytics settings

---

## Part 1: Complete RepCard Data Sync

### Currently Syncing ✅
- ✅ Users (`/users/minimal`, `/users/{id}/details`)
- ✅ Offices (`/offices`)
- ✅ Customers (`/customers`)
- ✅ Appointments (`/appointments`)
- ✅ Status Logs (`/customers/status-logs`)
- ✅ Customer Attachments (`/customers/attachments`)
- ✅ Appointment Attachments (`/appointments/attachments`)

### Missing Endpoints to Sync 🔴

#### 1. Customer Notes (`/customers/notes`)
**Why:** Customer interaction history, notes from reps
**Frequency:** Every sync (incremental)
**Data Structure:**
```typescript
{
  id: number;
  customerId: number;
  userId: number;
  note: string;
  createdAt: string;
  updatedAt: string;
  user?: RepCardUserMinimal;
}
```

#### 2. Customer Status Definitions (`/customers/status`)
**Why:** Status configuration, colors, icons, workflow
**Frequency:** Daily (rarely changes)
**Data Structure:**
```typescript
{
  id: number;
  statusName: string;
  shortName: string;
  type: number;
  colour: string;
  iconName: string;
  statusOrder: number;
  // ... more fields
}
```

#### 3. Calendar/Calendar Events (`/calendar/lists`, `/calendar/{id}`)
**Why:** Calendar configurations, setter/closer assignments
**Frequency:** Daily (rarely changes)
**Data Structure:**
```typescript
{
  id: number;
  name: string;
  setters: RepCardUser[];
  closers: RepCardUser[];
  dispatchers: RepCardUser[];
}
```

#### 4. Custom Fields (`/custom-fields/lead`, `/custom-fields/customer`)
**Why:** Field definitions for custom data, system size, cost, etc.
**Frequency:** Daily (rarely changes)
**Data Structure:**
```typescript
{
  id: number;
  internalName: string;
  displayName: string;
  type: string;
  dataType: string;
  optionValues?: string[];
}
```

#### 5. Leaderboard Data (`/leaderboards`)
**Why:** Pre-aggregated metrics, multiple leaderboard types
**Frequency:** Every sync (for historical snapshots)
**Data Structure:**
```typescript
{
  leaderboard_name: string;
  stats: {
    headers: Array<{
      stat_name: string;
      mapped_field: string;
      short_name: string;
      rank_by: boolean;
    }>;
    stats: Array<{
      item_id: number;
      item_type: 'user';
      customer_count: number;
      door_knocks: number;
      appointment_count: number;
      // ... more metrics
    }>;
  };
}
```

#### 6. Teams (`/offices` → teams)
**Why:** Team structure within offices
**Frequency:** Daily (rarely changes)
**Data Structure:**
```typescript
{
  id: number;
  team_name: string;
  office_id: number;
  // ... more fields
}
```

---

## Part 2: Database Schema for Missing Data

### Migration: `016_repcard_complete_data.sql`

```sql
-- Customer Notes
CREATE TABLE IF NOT EXISTS repcard_customer_notes (
  id TEXT PRIMARY KEY DEFAULT gen_random_uuid()::text,
  repcard_note_id INTEGER UNIQUE NOT NULL,
  customer_id TEXT REFERENCES repcard_customers(id) ON DELETE CASCADE,
  repcard_customer_id INTEGER NOT NULL,
  user_id TEXT REFERENCES users(id),
  repcard_user_id INTEGER NOT NULL,
  note TEXT NOT NULL,
  created_at TIMESTAMP NOT NULL,
  updated_at TIMESTAMP NOT NULL,
  raw_data JSONB,
  synced_at TIMESTAMP DEFAULT NOW()
);

CREATE INDEX idx_repcard_notes_repcard_id ON repcard_customer_notes(repcard_note_id);
CREATE INDEX idx_repcard_notes_customer ON repcard_customer_notes(repcard_customer_id);
CREATE INDEX idx_repcard_notes_user ON repcard_customer_notes(repcard_user_id);
CREATE INDEX idx_repcard_notes_created_at ON repcard_customer_notes(created_at);

-- Customer Status Definitions
CREATE TABLE IF NOT EXISTS repcard_customer_statuses (
  id TEXT PRIMARY KEY DEFAULT gen_random_uuid()::text,
  repcard_status_id INTEGER UNIQUE NOT NULL,
  status_name TEXT NOT NULL,
  short_name TEXT,
  type INTEGER,
  colour TEXT,
  icon_name TEXT,
  icon_url TEXT,
  status_order INTEGER,
  is_global BOOLEAN DEFAULT FALSE,
  is_base_status BOOLEAN DEFAULT FALSE,
  company_id INTEGER,
  raw_data JSONB,
  synced_at TIMESTAMP DEFAULT NOW()
);

CREATE INDEX idx_repcard_statuses_repcard_id ON repcard_customer_statuses(repcard_status_id);
CREATE INDEX idx_repcard_statuses_company ON repcard_customer_statuses(company_id);
CREATE INDEX idx_repcard_statuses_order ON repcard_customer_statuses(status_order);

-- Calendars
CREATE TABLE IF NOT EXISTS repcard_calendars (
  id TEXT PRIMARY KEY DEFAULT gen_random_uuid()::text,
  repcard_calendar_id INTEGER UNIQUE NOT NULL,
  name TEXT NOT NULL,
  company_id INTEGER NOT NULL,
  status TEXT DEFAULT 'active',
  setters INTEGER[], -- Array of RepCard user IDs
  closers INTEGER[], -- Array of RepCard user IDs
  dispatchers INTEGER[], -- Array of RepCard user IDs
  raw_data JSONB,
  synced_at TIMESTAMP DEFAULT NOW()
);

CREATE INDEX idx_repcard_calendars_repcard_id ON repcard_calendars(repcard_calendar_id);
CREATE INDEX idx_repcard_calendars_company ON repcard_calendars(company_id);

-- Custom Fields
CREATE TABLE IF NOT EXISTS repcard_custom_fields (
  id TEXT PRIMARY KEY DEFAULT gen_random_uuid()::text,
  repcard_field_id INTEGER UNIQUE NOT NULL,
  entity_type TEXT NOT NULL, -- 'lead', 'customer', 'recruit', 'other'
  internal_name TEXT NOT NULL,
  display_name TEXT NOT NULL,
  field_type TEXT NOT NULL, -- 'number', 'text', 'date', etc.
  data_type TEXT NOT NULL,
  option_values JSONB, -- Array of options for select fields
  company_id INTEGER,
  raw_data JSONB,
  synced_at TIMESTAMP DEFAULT NOW()
);

CREATE INDEX idx_repcard_fields_repcard_id ON repcard_custom_fields(repcard_field_id);
CREATE INDEX idx_repcard_fields_entity_type ON repcard_custom_fields(entity_type);
CREATE INDEX idx_repcard_fields_company ON repcard_custom_fields(company_id);

-- Leaderboard Snapshots (store historical leaderboard data)
CREATE TABLE IF NOT EXISTS repcard_leaderboard_snapshots (
  id TEXT PRIMARY KEY DEFAULT gen_random_uuid()::text,
  snapshot_date DATE NOT NULL,
  leaderboard_name TEXT NOT NULL,
  leaderboard_id TEXT NOT NULL, -- RepCard leaderboard _id
  company_id INTEGER NOT NULL,
  user_id TEXT REFERENCES users(id),
  repcard_user_id INTEGER NOT NULL,
  -- Metrics from D2D Leaderboard
  customer_count INTEGER DEFAULT 0,
  door_knocks INTEGER DEFAULT 0,
  lead_count INTEGER DEFAULT 0,
  appointment_count INTEGER DEFAULT 0,
  avg_door_knocks_per_day NUMERIC DEFAULT 0,
  avg_distance_per_knocks NUMERIC DEFAULT 0,
  -- Metrics from Overview Leaderboard
  avg_rating NUMERIC DEFAULT 0,
  video_viewed INTEGER DEFAULT 0,
  -- Metrics from Engagements Leaderboard
  review_count INTEGER DEFAULT 0,
  referral_count INTEGER DEFAULT 0,
  engagement_count INTEGER DEFAULT 0,
  card_sent_count INTEGER DEFAULT 0,
  -- Ranking
  item_rank INTEGER,
  office_id INTEGER,
  office_name TEXT,
  team_id INTEGER,
  team_name TEXT,
  raw_data JSONB,
  synced_at TIMESTAMP DEFAULT NOW(),
  UNIQUE(snapshot_date, leaderboard_name, repcard_user_id)
);

CREATE INDEX idx_repcard_lb_snapshots_date ON repcard_leaderboard_snapshots(snapshot_date);
CREATE INDEX idx_repcard_lb_snapshots_leaderboard ON repcard_leaderboard_snapshots(leaderboard_name);
CREATE INDEX idx_repcard_lb_snapshots_user ON repcard_leaderboard_snapshots(repcard_user_id);
CREATE INDEX idx_repcard_lb_snapshots_company ON repcard_leaderboard_snapshots(company_id);

-- Teams (from Offices)
ALTER TABLE repcard_users ADD COLUMN IF NOT EXISTS team_id INTEGER;
ALTER TABLE repcard_users ADD COLUMN IF NOT EXISTS team_name TEXT;

CREATE TABLE IF NOT EXISTS repcard_teams (
  id TEXT PRIMARY KEY DEFAULT gen_random_uuid()::text,
  repcard_team_id INTEGER UNIQUE NOT NULL,
  team_name TEXT NOT NULL,
  office_id INTEGER NOT NULL,
  repcard_office_id INTEGER,
  team_logo TEXT,
  company_id INTEGER,
  raw_data JSONB,
  synced_at TIMESTAMP DEFAULT NOW()
);

CREATE INDEX idx_repcard_teams_repcard_id ON repcard_teams(repcard_team_id);
CREATE INDEX idx_repcard_teams_office ON repcard_teams(office_id);
```

---

## Part 3: Settings System for Leaderboard/Analytics Configuration

### Settings Schema: `017_repcard_settings.sql`

```sql
-- RepCard Leaderboard Configuration
CREATE TABLE IF NOT EXISTS repcard_leaderboard_config (
  id TEXT PRIMARY KEY DEFAULT gen_random_uuid()::text,
  name TEXT NOT NULL UNIQUE, -- e.g., 'Default', 'D2D Focus', 'Closer Performance'
  description TEXT,
  leaderboard_type TEXT NOT NULL, -- 'd2d', 'overview', 'engagements', 'custom'
  enabled_metrics TEXT[] NOT NULL, -- Array of metric keys
  rank_by_metric TEXT NOT NULL, -- Which metric to rank by
  display_order INTEGER DEFAULT 0,
  date_range_default TEXT DEFAULT 'month', -- 'today', 'week', 'month', 'quarter', 'ytd', 'custom'
  roles TEXT[], -- Which roles can see this leaderboard
  office_ids INTEGER[], -- Which offices to include (empty = all)
  is_default BOOLEAN DEFAULT FALSE,
  created_by TEXT REFERENCES users(id),
  created_at TIMESTAMP DEFAULT NOW(),
  updated_at TIMESTAMP DEFAULT NOW()
);

-- RepCard Analytics Configuration
CREATE TABLE IF NOT EXISTS repcard_analytics_config (
  id TEXT PRIMARY KEY DEFAULT gen_random_uuid()::text,
  name TEXT NOT NULL UNIQUE,
  description TEXT,
  widget_type TEXT NOT NULL, -- 'card', 'chart', 'table', 'leaderboard'
  metric_type TEXT NOT NULL, -- 'doors_knocked', 'appointments_set', 'conversion_rate', etc.
  date_range_default TEXT DEFAULT 'month',
  refresh_interval INTEGER DEFAULT 30, -- seconds
  enabled BOOLEAN DEFAULT TRUE,
  display_order INTEGER DEFAULT 0,
  roles TEXT[], -- Which roles can see this widget
  office_ids INTEGER[], -- Which offices to include (empty = all)
  config JSONB, -- Widget-specific configuration
  created_by TEXT REFERENCES users(id),
  created_at TIMESTAMP DEFAULT NOW(),
  updated_at TIMESTAMP DEFAULT NOW()
);

-- Metric Definitions (defines what metrics are available)
CREATE TABLE IF NOT EXISTS repcard_metric_definitions (
  id TEXT PRIMARY KEY DEFAULT gen_random_uuid()::text,
  metric_key TEXT NOT NULL UNIQUE, -- e.g., 'doors_knocked', 'appointment_speed'
  display_name TEXT NOT NULL,
  description TEXT,
  category TEXT NOT NULL, -- 'volume', 'quality', 'revenue', 'engagement'
  data_source TEXT NOT NULL, -- 'repcard_api', 'database', 'calculated'
  unit TEXT, -- 'count', 'percentage', 'currency', 'hours', etc.
  format TEXT DEFAULT 'number', -- 'number', 'percentage', 'currency', 'duration'
  aggregation_type TEXT DEFAULT 'sum', -- 'sum', 'avg', 'count', 'max', 'min'
  enabled BOOLEAN DEFAULT TRUE,
  leaderboard_supported BOOLEAN DEFAULT TRUE,
  analytics_supported BOOLEAN DEFAULT TRUE,
  config JSONB, -- Calculation/formula config if calculated
  created_at TIMESTAMP DEFAULT NOW(),
  updated_at TIMESTAMP DEFAULT NOW()
);

-- Pre-populate metric definitions
INSERT INTO repcard_metric_definitions (metric_key, display_name, description, category, data_source, unit, format, aggregation_type, leaderboard_supported, analytics_supported) VALUES
-- Volume Metrics
('doors_knocked', 'Doors Knocked', 'Total doors knocked/leads created', 'volume', 'repcard_api', 'count', 'number', 'sum', true, true),
('appointments_set', 'Appointments Set', 'Total appointments scheduled', 'volume', 'repcard_api', 'count', 'number', 'sum', true, true),
('lead_count', 'Leads Created', 'Total leads created', 'volume', 'repcard_api', 'count', 'number', 'sum', true, true),
('customer_count', 'Customers Created', 'Total customers created', 'volume', 'repcard_api', 'count', 'number', 'sum', true, true),
('appointment_count', 'Appointments Completed', 'Total appointments completed', 'volume', 'database', 'count', 'number', 'sum', true, true),

-- Quality Metrics
('appointment_speed', 'Appointment Speed', 'Percentage of appointments set within 24 hours', 'quality', 'calculated', 'percentage', 'percentage', 'avg', true, true),
('attachment_rate', 'Power Bill Rate', 'Percentage of customers with power bill attachments', 'quality', 'calculated', 'percentage', 'percentage', 'avg', true, true),
('quality_score', 'Quality Score', 'Composite quality score', 'quality', 'calculated', 'score', 'number', 'avg', true, true),
('reschedule_rate', 'Reschedule Rate', 'Average number of reschedules per appointment', 'quality', 'calculated', 'count', 'number', 'avg', true, true),
('follow_up_consistency', 'Follow-Up Consistency', 'Percentage of appointments with follow-up', 'quality', 'calculated', 'percentage', 'percentage', 'avg', true, true),

-- Revenue Metrics
('sales_closed', 'Sales Closed', 'Total sales closed', 'revenue', 'database', 'count', 'number', 'sum', true, true),
('revenue', 'Revenue', 'Total revenue from closed sales', 'revenue', 'database', 'currency', 'currency', 'sum', true, true),

-- Engagement Metrics
('avg_rating', 'Average Rating', 'Average customer rating', 'engagement', 'repcard_api', 'rating', 'number', 'avg', true, true),
('review_count', 'Reviews', 'Total customer reviews', 'engagement', 'repcard_api', 'count', 'number', 'sum', true, true),
('referral_count', 'Referrals', 'Total referrals received', 'engagement', 'repcard_api', 'count', 'number', 'sum', true, true),
('video_viewed', 'Video Views', 'Total video views', 'engagement', 'repcard_api', 'count', 'number', 'sum', true, true),
('engagement_count', 'Engagements', 'Total customer engagements', 'engagement', 'repcard_api', 'count', 'number', 'sum', true, true),

-- Calculated Metrics
('set_rate', 'Set Rate', 'Appointments set / Doors knocked', 'quality', 'calculated', 'percentage', 'percentage', 'avg', true, true),
('close_rate', 'Close Rate', 'Sales closed / Appointments set', 'quality', 'calculated', 'percentage', 'percentage', 'avg', true, true),
('avg_door_knocks_per_day', 'Avg Doors/Day', 'Average doors knocked per day', 'volume', 'repcard_api', 'count', 'number', 'avg', true, true),
('avg_distance_per_knock', 'Avg Distance', 'Average distance per door knock', 'volume', 'repcard_api', 'distance', 'number', 'avg', true, true)
ON CONFLICT (metric_key) DO NOTHING;

CREATE INDEX idx_repcard_lb_config_enabled ON repcard_leaderboard_config(enabled);
CREATE INDEX idx_repcard_lb_config_default ON repcard_leaderboard_config(is_default);
CREATE INDEX idx_repcard_analytics_config_enabled ON repcard_analytics_config(enabled);
CREATE INDEX idx_repcard_metrics_category ON repcard_metric_definitions(category);
CREATE INDEX idx_repcard_metrics_enabled ON repcard_metric_definitions(enabled);
```

---

## Part 4: Sync Functions for Missing Data

### Add to `lib/repcard/comprehensive-sync.ts`

```typescript
// Add these sync functions:

export async function syncCustomerNotes(options: {
  startDate?: string;
  endDate?: string;
  incremental?: boolean;
} = {}): Promise<SyncEntityResult> {
  // Sync customer notes
  // Use: repcardClient.getCustomerNotes()
}

export async function syncCustomerStatuses(): Promise<SyncEntityResult> {
  // Sync status definitions
  // Use: repcardClient.getCustomerStatuses()
}

export async function syncCalendars(): Promise<SyncEntityResult> {
  // Sync calendar configurations
  // Use: repcardClient.getCalendars()
}

export async function syncCustomFields(): Promise<SyncEntityResult> {
  // Sync custom field definitions
  // Use: repcardClient.getCustomFields()
}

export async function syncLeaderboards(options: {
  startDate?: string;
  endDate?: string;
} = {}): Promise<SyncEntityResult> {
  // Sync leaderboard snapshots
  // Use: repcardClient.getLeaderboard()
  // Store as historical snapshots
}

export async function syncTeams(): Promise<SyncEntityResult> {
  // Sync teams from offices
  // Teams are nested in office data
}
```

### Add to `lib/repcard/client.ts`

```typescript
// Add these methods:

async getCustomerNotes(params?: {
  customerId?: number;
  userId?: number;
  page?: number;
  perPage?: number;
}): Promise<RepCardPaginatedResponse<RepCardCustomerNote>> {
  // Implement customer notes endpoint
}

async getCustomerStatuses(): Promise<RepCardApiResponse<RepCardCustomerStatus[]>> {
  // Implement status definitions endpoint
}

async getCalendars(params?: {
  status?: string;
}): Promise<RepCardApiResponse<RepCardCalendar[]>> {
  // Implement calendars endpoint
}

async getCustomFields(entityType: 'lead' | 'customer' | 'recruit' | 'other'): Promise<RepCardApiResponse<RepCardCustomField[]>> {
  // Implement custom fields endpoint
}

async getLeaderboards(params?: {
  fromDate?: string;
  toDate?: string;
}): Promise<RepCardApiResponse<RepCardLeaderboard[]>> {
  // Already exists, but ensure it's complete
}
```

---

## Part 5: Settings UI Component

### Component: `components/settings/RepCardConfigTab.tsx`

```typescript
'use client';

import { useState } from 'react';
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import { Card, CardContent, CardHeader, CardTitle, CardDescription } from '@/components/ui/card';
import { Button } from '@/components/ui/button';
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs';
import { toast } from 'sonner';

export function RepCardConfigTab() {
  const queryClient = useQueryClient();
  
  // Fetch current configurations
  const { data: leaderboardConfigs } = useQuery({
    queryKey: ['repcard-leaderboard-configs'],
    queryFn: async () => {
      const res = await fetch('/api/repcard/settings/leaderboards');
      return res.json();
    }
  });

  const { data: analyticsConfigs } = useQuery({
    queryKey: ['repcard-analytics-configs'],
    queryFn: async () => {
      const res = await fetch('/api/repcard/settings/analytics');
      return res.json();
  });

  const { data: metrics } = useQuery({
    queryKey: ['repcard-metrics'],
    queryFn: async () => {
      const res = await fetch('/api/repcard/settings/metrics');
      return res.json();
    }
  });

  return (
    <div className="space-y-6">
      <Tabs defaultValue="leaderboards">
        <TabsList>
          <TabsTrigger value="leaderboards">Leaderboards</TabsTrigger>
          <TabsTrigger value="analytics">Analytics Widgets</TabsTrigger>
          <TabsTrigger value="metrics">Available Metrics</TabsTrigger>
        </TabsList>

        <TabsContent value="leaderboards">
          <LeaderboardConfigManager configs={leaderboardConfigs} />
        </TabsContent>

        <TabsContent value="analytics">
          <AnalyticsConfigManager configs={analyticsConfigs} />
        </TabsContent>

        <TabsContent value="metrics">
          <MetricsManager metrics={metrics} />
        </TabsContent>
      </Tabs>
    </div>
  );
}
```

---

## Part 6: API Endpoints for Settings

### `app/api/repcard/settings/leaderboards/route.ts`
- GET: List all leaderboard configurations
- POST: Create new leaderboard configuration
- PUT: Update leaderboard configuration
- DELETE: Delete leaderboard configuration

### `app/api/repcard/settings/analytics/route.ts`
- GET: List all analytics widget configurations
- POST: Create new analytics widget
- PUT: Update analytics widget
- DELETE: Delete analytics widget

### `app/api/repcard/settings/metrics/route.ts`
- GET: List all available metrics
- PUT: Update metric configuration (enable/disable)

---

## Implementation Priority

### Phase 1: Complete Data Sync (Week 1)
1. ✅ Add database migrations for missing tables
2. ✅ Add sync functions for notes, statuses, calendars, custom fields, leaderboards, teams
3. ✅ Update comprehensive sync to include all new endpoints
4. ✅ Test sync with all endpoints

### Phase 2: Settings System (Week 2)
1. ✅ Create settings database schema
2. ✅ Create settings API endpoints
3. ✅ Build settings UI component
4. ✅ Add to settings page

### Phase 3: Configuration Integration (Week 3)
1. ✅ Update leaderboard API to use configurations
2. ✅ Update analytics components to use configurations
3. ✅ Add default configurations
4. ✅ Test end-to-end

---

## Next Steps

1. **Create migration files** for all new tables
2. **Add sync functions** to comprehensive-sync.ts
3. **Add API methods** to client.ts
4. **Create settings UI** component
5. **Update leaderboard API** to use configurable settings

Would you like me to start implementing these changes?

