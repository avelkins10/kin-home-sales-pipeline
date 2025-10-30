# RepCard API Analysis & Revised Architecture Recommendation

**Date:** 2025-01-28  
**Based on:** RepCard API Documentation Postman Collection

---

## Key Discovery: RepCard API Provides Much More Than Expected

After reviewing the API documentation, I was **incorrect** about what RepCard API offers. Here's what's actually available:

### 1. Leaderboard API (`/leaderboards`) ✅ **COMPREHENSIVE**

**Endpoint:** `GET /leaderboards?from_date=YYYY-MM-DD&to_date=YYYY-MM-DD`

**Returns:** Multiple leaderboard configurations with **pre-aggregated metrics**:

#### D2D Leaderboard (Most Relevant):
```json
{
  "stats": [
    {
      "item_id": 667,
      "item_type": "user",
      "customer_count": 0,           // ✅ Sales - Customers Created
      "door_knocks": 4,              // ✅ DOORS KNOCKED (available!)
      "lead_count": 15,              // ✅ Leads Created
      "appointment_count": 3,        // ✅ Appointments Set
      "avg_door_knocks_per_day": 2,  // ✅ Calculated metric
      "avg_distance_per_knocks": 0,  // ✅ Calculated metric
      "office": [...],               // ✅ Office info
      "team": [...],                 // ✅ Team info
      "item_rank": 2
    }
  ]
}
```

#### Overview Leaderboard:
```json
{
  "customer_count": 0,      // ✅ Customers Created
  "appointment_count": 3,   // ✅ Appointments Set
  "lead_count": 15,         // ✅ Leads Created
  "avg_rating": 4            // ✅ Review Ratings
}
```

**Key Features:**
- ✅ Pre-aggregated metrics (no calculation needed)
- ✅ Date range filtering (`from_date`, `to_date`)
- ✅ Includes office/team relationships
- ✅ Returns all users (even with 0 metrics)
- ✅ Multiple leaderboard types (D2D, Overview, Engagements, Recruiting)

### 2. Appointments API (`/appointments`) ✅ **FULLY FEATURED**

**Endpoint:** `GET /appointments?from_date=YYYY-MM-DD&to_date=YYYY-MM-DD&setter_ids=447,478&closer_ids=108727&per_page=100&page=1`

**Returns:**
- Full appointment details
- Setter and closer relationships
- Customer relationships
- Status/disposition
- Dates (scheduled_at, completed_at)

**Filtering Support:**
- ✅ `setter_ids` - Filter by setter
- ✅ `closer_ids` - Filter by closer
- ✅ `customer_ids` - Filter by customer
- ✅ `status_ids` - Filter by status
- ✅ Date range (`from_date`, `to_date`)
- ✅ Pagination (`per_page`, `page`)

### 3. Customers API (`/customers`) ✅ **RELATIONSHIPS INCLUDED**

**Endpoint:** `GET /customers?per_page=100&last_created_from=YYYY-MM-DD&last_created_to=YYYY-MM-DD&withRelations=attachments,user,notes`

**Returns:**
- Customer details
- **Relationships:** attachments, user (setter), notes, auroraProjectLinks
- Date filtering (`last_created_from`, `last_created_to`)
- Status filtering

### 4. Status Logs API (`/customers/status-logs`) ✅ **STATUS HISTORY**

**Endpoint:** `GET /customers/status-logs?from_date=YYYY-MM-DD&to_date=YYYY-MM-DD`

**Returns:**
- Status change history
- Old/new status
- Changed by user
- Timestamp

### 5. Attachments API ✅ **SEPARATE ENDPOINTS**

- `GET /customers/attachments` - Customer attachments
- `GET /appointments/attachments` - Appointment attachments
- Filtering by user_ids, customer_ids, date ranges

---

## Revised Architecture Recommendation

### Option A: Hybrid Approach (RECOMMENDED) ⭐

**Use RepCard API for Display, Database for Joining**

#### For Leaderboards (RepCard-only metrics):
```typescript
// Use RepCard API directly
GET /leaderboards?from_date=2025-01-01&to_date=2025-01-31

// Returns pre-aggregated data:
- doors_knocked ✅
- appointments_set ✅
- customer_count ✅
- lead_count ✅
- avg_door_knocks_per_day ✅
```

**Benefits:**
- ✅ Always fresh data (no sync delay)
- ✅ Pre-aggregated (fast)
- ✅ No calculation needed
- ✅ Returns all users (even with 0 metrics)
- ✅ 1 API call per leaderboard view

**Rate Limit Math:**
- Leaderboard view: 1 API call
- 30-second refresh = 2 calls/minute per user
- 10 concurrent users = 20 calls/minute
- **Well within 100 req/period limit** ✅

#### For Combined Metrics (RepCard + QuickBase):
```typescript
// Still use database for joins
// Sync RepCard data to database for:
1. Joining with QuickBase sales data
2. Custom calculations (quality_score, etc.)
3. Historical queries
4. Complex filtering
```

### Option B: Full API-First Approach

**Use RepCard API for everything, minimal database sync**

**Pros:**
- ✅ Always fresh data
- ✅ No sync complexity
- ✅ Less storage

**Cons:**
- ❌ Can't join with QuickBase easily (requires API calls to both)
- ❌ Rate limits become concern (multiple calls per view)
- ❌ No historical data queries
- ❌ Custom metrics still need calculation

**Not Recommended** because:
- You need to combine RepCard + QuickBase metrics
- Quality metrics require raw appointment data
- Historical queries need stored data

---

## Recommended Implementation Strategy

### Phase 1: Hybrid Leaderboard (Week 1)

**Use RepCard API for RepCard-only leaderboards:**

```typescript
// app/api/repcard/leaderboard/route.ts
export async function GET(request: NextRequest) {
  // If no QuickBase metrics needed, use RepCard API
  if (metric === 'doors_knocked' || metric === 'appointments_set' || metric === 'lead_count') {
    const repcardData = await repcardClient.getLeaderboard({
      fromDate: startDate,
      toDate: endDate
    });
    
    // Map RepCard leaderboard to our format
    return mapRepCardLeaderboard(repcardData, metric);
  }
  
  // For combined metrics or custom calculations, use database
  return queryDatabase(...);
}
```

**Benefits:**
- ✅ Fresh data (no sync delay)
- ✅ Pre-aggregated (fast)
- ✅ 1 API call vs multiple DB queries
- ✅ Less database load

### Phase 2: Keep Database Sync for Joins (Week 2)

**Continue syncing to database for:**
1. **Combined metrics** (RepCard + QuickBase)
2. **Custom calculations** (quality_score, appointment_speed, attachment_rate)
3. **Historical queries** (trends, time-series data)
4. **Complex filtering** (office + role + date combinations)

**Sync Strategy:**
- Sync every 5 minutes (or longer - data freshness less critical)
- Only sync raw data (customers, appointments, status logs)
- Don't sync aggregated leaderboard data (get from API)

### Phase 3: Smart Caching (Week 3)

**Cache RepCard API responses:**
```typescript
// Cache leaderboard API calls for 2-3 minutes
// Reduces API calls while keeping data fresh
const CACHE_TTL = 180000; // 3 minutes
```

---

## Final Recommendation: Hybrid Approach ⭐

### Use RepCard API For:
1. **Leaderboards** (RepCard-only metrics)
   - `doors_knocked` ✅
   - `appointments_set` ✅
   - `lead_count` ✅
   - `customer_count` ✅

2. **Single User Stats** (if not combining with QuickBase)
   - Use `/leaderboards` filtered by user

### Use Database For:
1. **Combined Metrics** (RepCard + QuickBase)
   - Still need database joins
   - Example: RepCard appointments + QuickBase sales

2. **Custom Calculations**
   - Quality metrics (require raw appointment data)
   - Appointment speed (need to calculate time differences)
   - Attachment rates (need to count attachments)

3. **Historical Queries**
   - Time-series data
   - Trends over time
   - Complex date ranges

4. **Relationships**
   - Customer → Appointments → Status Logs
   - Multi-level joins easier in database

### Sync Strategy:
- **Reduce sync frequency** to 10-15 minutes (data freshness less critical)
- **Sync only raw data** (customers, appointments, status logs, attachments)
- **Don't sync aggregated data** (get from API)

---

## Implementation Plan

### Step 1: Update Leaderboard API
```typescript
// Check if metric is available in RepCard API
const REPCARD_API_METRICS = ['doors_knocked', 'appointments_set', 'lead_count', 'customer_count'];

if (REPCARD_API_METRICS.includes(metric) && !needsQuickBaseData) {
  // Use RepCard API
  return await getLeaderboardFromAPI(dateRange);
} else {
  // Use database (for combined metrics or custom calculations)
  return await getLeaderboardFromDatabase(dateRange);
}
```

### Step 2: Map RepCard Leaderboard Response
```typescript
function mapRepCardLeaderboard(repcardData: any, metric: string) {
  // Find D2D leaderboard (has door_knocks)
  const d2dLeaderboard = repcardData.result.find(
    (lb: any) => lb.leaderboard_name === 'D2D Leaderboard'
  );
  
  return d2dLeaderboard.stats.stats.map((stat: any) => ({
    userId: stat.item_id,
    userName: stat.item_title,
    metricValue: stat[mapMetricToField(metric)],
    // ... map other fields
  }));
}
```

### Step 3: Reduce Sync Frequency
- Change from 5 minutes to 10-15 minutes
- Only sync raw data (not aggregated)

---

## Conclusion

**You were right!** RepCard API provides much more than I initially thought:

✅ **Doors Knocked** - Available in D2D Leaderboard  
✅ **Pre-aggregated metrics** - No calculation needed  
✅ **Relationships included** - Offices, teams, users  
✅ **Date filtering** - Supports all date ranges  
✅ **Rate limit friendly** - 1 call per leaderboard view

**Recommended Approach:**
- **Hybrid**: Use RepCard API for display, database for joins
- **Best of both worlds**: Fresh data + ability to combine with QuickBase
- **Simpler sync**: Less frequent, only raw data

This is a **better architecture** than pure database-first or pure API-first!

