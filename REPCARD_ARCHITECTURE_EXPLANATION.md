# RepCard Architecture Explanation

## Overview
**We are using the SAME tables as before webhooks.** Webhooks and API sync both write to the same database tables. The dashboard reads from those same tables.

## Data Flow Architecture

### 1. **Database Tables (Same as Before)**
We have these tables (created by migrations 012-033):
- `repcard_appointments` - Stores all appointments
- `repcard_customers` - Stores all customers/leads
- `repcard_users` - Stores RepCard user data
- `repcard_offices` - Stores office data
- `repcard_appointment_attachments` - Stores appointment attachments
- `repcard_customer_attachments` - Stores customer attachments
- `repcard_metric_audit` - Audit trail for metric calculations (NEW - migration 032)

**No new tables were created for webhooks.** Webhooks write to the existing tables.

### 2. **Data Sources (Two Ways to Populate Tables)**

#### A. **Webhooks (Real-time)**
- **Endpoint**: `/api/webhooks/repcard`
- **Processor**: `lib/repcard/webhook-processor.ts`
- **What it does**:
  - Receives webhook payload from RepCard
  - Parses appointment/customer data directly from payload
  - **INSERTs/UPDATEs into `repcard_appointments` and `repcard_customers` tables**
  - Triggers automatically calculate `is_within_48_hours` and `has_power_bill`
  - Falls back to API sync if payload is incomplete

#### B. **API Sync (Scheduled/Manual)**
- **Endpoints**: 
  - `/api/admin/repcard/sync` (manual)
  - `/api/cron/repcard-sync` (automatic every 5 minutes)
- **Service**: `lib/repcard/sync-service.ts`
- **What it does**:
  - Calls RepCard API to fetch appointments/customers
  - **INSERTs/UPDATEs into `repcard_appointments` and `repcard_customers` tables**
  - Same tables as webhooks!

### 3. **Dashboard Display (Reads from Same Tables)**

#### Dashboard Components
- `RepCardOptimizedDashboard.tsx` - Main dashboard UI
- `RepCardSimpleDashboard.tsx` - Simple view
- `RepCardUnifiedDashboard.tsx` - Unified view

#### API Endpoint
- `/api/repcard/unified-dashboard`
- **Queries**: `repcard_appointments` and `repcard_customers` tables
- **Does NOT care** whether data came from webhooks or API sync
- **Reads**: `is_within_48_hours`, `has_power_bill`, `scheduled_at`, `created_at`, etc.

## Key Points

### ✅ Same Tables
- Webhooks and API sync both write to `repcard_appointments` and `repcard_customers`
- Dashboard reads from the same tables
- No duplicate data or separate tables

### ✅ Unified Data
- Whether an appointment came from a webhook or API sync, it's in the same table
- Metrics (`is_within_48_hours`, `has_power_bill`) are calculated by database triggers
- Dashboard shows all data regardless of source

### ✅ Event-Driven Metrics
- Database triggers automatically calculate metrics when data is inserted/updated
- Works for both webhook data and API sync data
- No difference in how metrics are calculated

## Data Flow Diagram

```
┌─────────────────┐
│   RepCard API   │
└────────┬────────┘
         │
         │ (scheduled sync every 5 min)
         │
         ▼
┌─────────────────────────┐
│  API Sync Service       │
│  (sync-service.ts)      │
└────────┬─────────────────┘
         │
         │ INSERT/UPDATE
         ▼
┌─────────────────────────┐
│  repcard_appointments   │◄──┐
│  repcard_customers      │   │
│  (SAME TABLES)          │   │
└────────┬─────────────────┘   │
         │                      │
         │                      │
         │ (webhook events)      │
         │                      │
┌────────▼─────────────────┐   │
│  Webhook Processor       │   │
│  (webhook-processor.ts)  │   │
└────────┬─────────────────┘   │
         │                      │
         │ INSERT/UPDATE         │
         └──────────────────────┘
         │
         │ Database Triggers
         │ (calculate metrics)
         ▼
┌─────────────────────────┐
│  is_within_48_hours    │
│  has_power_bill        │
│  (auto-calculated)     │
└────────┬─────────────────┘
         │
         │ SELECT queries
         ▼
┌─────────────────────────┐
│  Dashboard API          │
│  (unified-dashboard)   │
└────────┬─────────────────┘
         │
         │ JSON response
         ▼
┌─────────────────────────┐
│  Dashboard UI           │
│  (RepCardOptimized...) │
└─────────────────────────┘
```

## What Changed with Webhooks?

### Before Webhooks:
1. Cron job runs every 5 minutes
2. Calls RepCard API
3. Syncs data to `repcard_appointments` and `repcard_customers`
4. Dashboard reads from those tables
5. **Latency**: Up to 5 minutes for new data

### After Webhooks:
1. **Real-time**: Webhook fires immediately when appointment is set
2. Webhook processor writes directly to `repcard_appointments` and `repcard_customers`
3. Dashboard reads from same tables
4. **Latency**: Near-instant (seconds)
5. **Fallback**: Cron still runs every 5 minutes to catch anything missed

## Summary

- **Tables**: Same tables (`repcard_appointments`, `repcard_customers`)
- **Webhooks**: Write to existing tables (no new tables)
- **API Sync**: Also writes to same tables
- **Dashboard**: Reads from same tables (doesn't care about source)
- **Metrics**: Calculated by database triggers (works for both sources)
- **Result**: Unified data, no duplication, faster updates via webhooks
