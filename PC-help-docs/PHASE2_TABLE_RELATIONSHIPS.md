# PHASE 2: TABLE RELATIONSHIPS & CRITICAL DISCOVERIES

**Analysis Date:** 2025-10-24
**QuickBase Realm:** kin.quickbase.com
**Tables Analyzed:** 54 total (12 known + 42 discovered)

---

## EXECUTIVE SUMMARY

Phase 2 successfully discovered **42 new tables** through relationship field analysis, bringing the total to **54 tables** in the QuickBase app. Most critically, I found the three "missing" tables that were referenced but not yet identified:

1. **Outreach Records** → `btvik5kwi`
2. **Sales Aid Request Records** → `bt3m39fgr`
3. **ZD Outreach Records** → `bt9scdhbj`

Additionally, I mapped **152 table relationships** showing how data flows through the PC workflow.

---

## CRITICAL TABLE DISCOVERIES

### 1. OUTREACH RECORDS TABLE
**Table ID:** `btvik5kwi`
**Purpose:** Customer outreach attempt logging and call tracking

**Referenced By:**
- Projects table, field 1640 "Outreach records" (dblink)
- Projects table, field 1614 "Calls (PC)" (dblink)

**Significance:**
- This is where PCs log all customer contact attempts
- Likely contains: date, time, method (call/text/email), outcome, notes
- Field 2267 "# of Outreach records" in Projects counts records here
- Field 2268 "# of Due Milestone Outreach records" filters this table for overdue items

**Recommended Analysis:**
```javascript
// Query to understand Outreach Records structure
GET /v1/fields?tableId=btvik5kwi

// Get sample outreach data
POST /v1/records/query
{
  "from": "btvik5kwi",
  "select": ["*"],
  "options": { "top": 20 }
}
```

---

### 2. SALES AID REQUEST RECORDS TABLE
**Table ID:** `bt3m39fgr`
**Purpose:** Escalation requests when PCs need help from sales team

**Referenced By:**
- Projects table, field 1792 "Sales Aid Request Records" (dblink)

**Related Fields in Projects:**
- 1912: # of Open Sales Aid records
- 2422: # of Escalated Sales Aid Records ← **DASHBOARD "Pending Escalation"**
- 1908: # of Active Unresponsive Sales Aid Records ← **DASHBOARD "Unresponsive"**
- 2423: # of Pending Closeout Sales Aid
- 2421: # of Sales Aid Adder Records
- 2416: Maximum Sales Aid Date Created
- 2417: Max Record Sales Aid Reason

**Significance:**
- **Drives "Pending Escalation" dashboard section**
- Contains escalation reason codes, priority, assignment
- Tracks resolution status and closure
- Links to Install Communications (field 126) and Task Groups (fields 69, 72)

**Recommended Analysis:**
```javascript
// Get Sales Aid schema
GET /v1/fields?tableId=bt3m39fgr

// Get active escalations
POST /v1/records/query
{
  "from": "bt3m39fgr",
  "select": ["*"],
  "where": "{[status_field].EX.'Open'}",
  "options": { "top": 50 }
}
```

---

### 3. ZD OUTREACH RECORDS TABLE
**Table ID:** `bt9scdhbj`
**Purpose:** Zendesk-integrated outreach tracking (likely for NEM/PTO phase)

**Referenced By:**
- Projects table, field 1936 "ZD Outreach Records" (dblink)
- Projects table, field 1937 "Add ZD Outreach" (URL button)

**Significance:**
- Zendesk integration for customer support tickets
- May be used for NEM/PTO customer communication tracking
- Separate from main outreach records, suggesting different workflow
- Likely read-only sync from Zendesk system

**Recommended Analysis:**
```javascript
// Get ZD Outreach schema
GET /v1/fields?tableId=bt9scdhbj

// Sample ZD outreach data
POST /v1/records/query
{
  "from": "bt9scdhbj",
  "select": ["*"],
  "options": { "top": 10 }
}
```

---

## ADDITIONAL HIGH-VALUE DISCOVERED TABLES

### Communication & Tracking Tables

| Table ID | Purpose | Key Relationships |
|----------|---------|-------------------|
| `bt4a8ypkq` | **Intake Event records** | Linked from Projects (1799), Task Groups (34) |
| `bsbguxz4i` | **Event Calendar / All Project Events** | Linked from Projects (multiple), Installations, Inspections |
| `buk7pibpv` | **Communication records** (Zendesk tickets) | Linked from Projects (2137, 2147) |
| `bstdqwrkg` | **Tickets** (Support/issue tracking) | Linked from Projects (2150), Installations (347) |
| `bu59cg46p` | **Outbound Communication records** | Linked from Projects (2400) |
| `bu5ppgx2q` | **Callpilot Log records** | Linked from Projects (2402) - call center integration |

### Workflow & Task Tables

| Table ID | Purpose | Key Relationships |
|----------|---------|-------------------|
| `br9kwm8q9` | **Tasks** (main task table, different from Sales Tasks) | Linked from Projects, Installations, Permits, etc. |
| `bu36g8j99` | **Sales Task Submission records** | Linked from Tasks (bu36ggiht) field 7 |
| `bvbqgs5yc` | **Arrivy Task Records** (field service tasks) | Linked from Projects, Installations, Inspections |

### Financial & Cost Tables

| Table ID | Purpose | Key Relationships |
|----------|---------|-------------------|
| `btkc3zrxs` | **Receivable records** | Linked from Projects (1492) |
| `bub2ixkr4` | **Funding Event Records** | Linked from Projects (1983) |
| `bt4pxqzx8` | **Outstanding Item records** | Linked from Projects (2008, 1988) |
| `budd3c3ti` | **All Bills** | Linked from Projects (2061) |
| `bu4s5xtja` | **Commission Log Event records** | Linked from Projects (2336) |
| `bu5h3nm6k` | **Sequifi Commissions** | Linked from Projects (2382) |

### Design & Engineering Tables

| Table ID | Purpose | Key Relationships |
|----------|---------|-------------------|
| `bsbhp6zhm` | **Designs** | Linked from Projects (168), Permits (100) |
| `btadqc6ga` | **BOM** (Bill of Materials) | Linked from Projects (1187) |
| `bvgi8955t` | **Material Order BOM records** | Linked from Projects (2579) |
| `bsaycczmf` | **Adders** (Adder Review, MPU Readiness) | Linked from Projects, Installations |
| `bsbkmwvu8` | **Materials** | Linked from Installations (29) |
| `bsg4wcwr5` | **Equipment** | Linked from Installations (60) |

### Administrative & Support Tables

| Table ID | Purpose | Key Relationships |
|----------|---------|-------------------|
| `br9kwm8ke` | **Attachments** (universal file storage) | Linked from all tables |
| `bsikx9yw7` | **Notification records** | Linked from Projects (456, 458) |
| `bsdcnz75g` | **Cancellation Requests** | Linked from Projects, Install Communications |
| `bt26x67sf` | **Request to Cancel records** | Linked from Projects (1781), Install Communications (127) |
| `bt4r9f4vu` | **Service records** (post-install service) | Linked from Projects (1812), Inspections (121) |
| `br9kwm847` | **Milestones** | Linked from Projects (195) |
| `br9kwm87j` | **Project - Organizations** | Linked from Projects (8) |

### Logging & Audit Tables

| Table ID | Purpose | Key Relationships |
|----------|---------|-------------------|
| `bsiripd8r` | **Audit Log Event records** | Linked from Projects (668) |
| `bsy4d7a5h` | **Status Update records** | Linked from Projects (919) |
| `bs274cv7e` | **Contact Updates / Change records** | Linked from Projects (2300, 2302) |

### Customer-Facing Tables

| Table ID | Purpose | Key Relationships |
|----------|---------|-------------------|
| `br9vmrkp8` | **Surveys** | Linked from Projects (142) |
| `bscyuq2i2` | **HOAs** (Homeowner Association) | Linked from Projects (209) |
| `bum8nf7jc` | **Text records** (SMS communication) | Linked from Projects (2354) |
| `bsg48gg8q` | **Calls** | Linked from Projects (374) |

### Integration Tables

| Table ID | Purpose | Key Relationships |
|----------|---------|-------------------|
| `bvf3ydgnq` | **Order Webhook records** | Linked from Projects (2545) |
| `bvg6k4a6t` | **TOA - JOB ID records** | Linked from Projects (2584) |
| `bvdcsga5j` | **AI Observations** | Linked from Projects (2492) |
| `buxcybwum` | **Call to Action records** | Linked from Projects (2269) |

---

## TABLE RELATIONSHIP ANALYSIS

### Projects Table → Outgoing Relationships

The **Projects table** is the central hub with **98 outgoing relationship fields**:

#### By Category:
- **Communications**: 11 links to Install Communications (bsb6bqt3b) for notes by phase
- **Cost Tracking**: 6 links to Cost Records (bsiz6sw8r) by cost type
- **Milestone Tracking**: 3 links to Tasks tables (br9kwm8q9, bu36ggiht)
- **Cancellations**: 3 links to Cancellation tables (bsdcnz75g, bt26x67sf)
- **Events**: 4 links to Event Calendar (bsbguxz4i)
- **Notifications**: 2 links to Notification records (bsikx9yw7)
- **Escalations**: 1 link to Sales Aid Requests (bt3m39fgr)
- **Outreach**: 2 links to Outreach tables (btvik5kwi, bt9scdhbj)

#### Top Connected Tables:
1. **bsb6bqt3b** (Install Communications) - 11 connections
2. **bsiz6sw8r** (Cost Records) - 6 connections
3. **bsbguxz4i** (Event Calendar) - 4 connections
4. **br9kwm8q9** (Tasks) - 3 connections
5. **bsdcnz75g** (Cancellation Requests) - 3 connections

### Projects Table ← Incoming Relationships

The **Projects table** receives data from **3 tables**:

1. **Install Communications** (bsb6bqt3b)
   - Field 51: Project records
   - Field 55: Project records2
   - Field 87: Project records4
   - Purpose: Link communications back to projects

2. **Installations** (bsbkmcgfs)
   - Field 137: Project records
   - Field 212: Install Availability Report
   - Purpose: Link install records to projects

3. **Interconnections** (bseufp79y)
   - Field 184: Project records
   - Purpose: Link NEM/utility records to projects

### Install Communications Table

**Bidirectional Relationships:**
- Links to itself (field 29) for threaded notes
- Links to Projects (3 fields) for project association
- Links to Attachments (br9kwm8ke) for file storage
- Links to Cancel Requests (bt26x67sf) for cancellation tracking

**Key Insight:** Install Communications is a shared table across ALL project phases (Intake, Design, Permit, Install, Inspection, NEM, PTO) as evidenced by Projects table having 11 separate dblink fields filtered by phase.

### Task System Architecture

**Three-Tier Task System:**

1. **Task Groups** (bu36gem4p)
   - Contains: PC info, project link, task count
   - Links to: Sales Tasks (bu36ggiht), Intake Events (bt4a8ypkq)

2. **Sales Tasks** (bu36ggiht)
   - Contains: Individual tasks within groups
   - Links to: Task Submissions (bu36g8j99), Attachments (br9kwm8ke)

3. **Task Submissions** (bu36g8j99 or brh36h9mh)
   - Contains: Customer-submitted documents/responses
   - Note: Two potential tables found

---

## PC WORKFLOW DATA FLOW

### Initial Outreach Flow

```
Projects
  ├─ Field 2268: # of Due Milestone Outreach records
  │  ├─ Queries → Outreach Records (btvik5kwi)
  │  └─ Filters by: due date, incomplete status
  │
  ├─ Field 2287: PC Outreach: Create Check-In? (checkbox)
  └─ Field 2266: PC Outreach: Incomplete Check-In? (checkbox)
       └─ Triggers → Dashboard "Initial Outreach" section
```

### Escalation Flow

```
Projects
  ├─ Field 1792: Sales Aid Request Records (dblink)
  │  └─ Links to → Sales Aid Requests (bt3m39fgr)
  │
  ├─ Field 2422: # of Escalated Sales Aid Records
  │  └─ Filters → Open escalations
  │       └─ Displays in → Dashboard "Pending Escalation"
  │
  └─ Field 1908: # of Active Unresponsive Sales Aid Records
       └─ Filters → Unresponsive customer escalations
            └─ Displays in → Dashboard "Unresponsive Customers"
```

### Communication Flow

```
Projects ←→ Install Communications (bsb6bqt3b)
  │
  ├─ Phase-specific links:
  │  ├─ Field 484: Intake Communications
  │  ├─ Field 483: Design Communications
  │  ├─ Field 493: Permit Communications
  │  ├─ Field 496: Install Communications
  │  ├─ Field 497: PTO Communications
  │  ├─ Field 492: NEM Communications
  │  └─ Field 186: Notes (general)
  │
  └─ Tracking fields update in Projects:
     ├─ Field 2304: Max PC Note Date
     ├─ Field 2264: Maximum PC Outreach Completed Date
     ├─ Field 1731-1734: Last Attempted Contact Date (by phase)
     └─ Field 1708-1711: # of Contact Attempts (by phase)
```

### NEM Blocker Flow

```
Install Communications (bsb6bqt3b)
  ├─ Field 135: NEM Blocker Outreach (checkbox)
  │  └─ When checked → Appears in Dashboard "Blocked NEM"
  │
  └─ Related Project → Projects (br9kwm8na)
       └─ Interconnections (bseufp79y)
            ├─ Field 174: Days Since Last NEM Outreach
            └─ Field 175: Add Outreach Note (URL)
```

---

## RELATIONSHIP STATISTICS

### Most Connected Tables

1. **Projects (br9kwm8na)**: 98 outgoing relationships
2. **Installations (bsbkmcgfs)**: 13 relationships
3. **Permits (bscs3z866)**: 9 relationships
4. **Interconnections (bseufp79y)**: 7 relationships
5. **Install Communications (bsb6bqt3b)**: 7 relationships
6. **Inspections (bsc3v7tdg)**: 7 relationships

### Universal Support Tables

These tables are linked from nearly every other table:

- **br9kwm8ke** (Attachments): Linked from 15+ tables
- **bsb6bqt3b** (Install Communications): Linked from 10+ tables
- **bsiz6sw8r** (Cost Records): Linked from 7+ tables
- **br9kwm8q9** (Tasks): Linked from 6+ tables
- **bsbguxz4i** (Event Calendar): Linked from 5+ tables

### Isolated Tables

- **Cost Records (bsiz6sw8r)**: No outgoing relationships (sink table)
- **Task Submissions (brh36h9mh)**: Access denied (permissions issue)

---

## FIELD IDS TO ADD TO YOUR CODEBASE

Update `/Users/austinelkins/lib/constants/fieldIds.ts` with newly discovered tables:

```typescript
// CRITICAL PC DASHBOARD TABLES
export const TABLES = {
  // Core tables (already known)
  PROJECTS: 'br9kwm8na',
  INSTALL_COMMUNICATIONS: 'bsb6bqt3b',
  TASK_GROUPS: 'bu36gem4p',
  TASKS: 'bu36ggiht',

  // NEWLY DISCOVERED - CRITICAL FOR PC DASHBOARD
  OUTREACH_RECORDS: 'btvik5kwi',           // ← Customer outreach logging
  SALES_AID_REQUESTS: 'bt3m39fgr',         // ← Escalation tracking
  ZD_OUTREACH_RECORDS: 'bt9scdhbj',        // ← Zendesk outreach sync

  // High-value support tables
  INTAKE_EVENTS: 'bt4a8ypkq',
  EVENT_CALENDAR: 'bsbguxz4i',
  COMMUNICATION_RECORDS: 'buk7pibpv',      // Zendesk tickets
  TICKETS: 'bstdqwrkg',
  OUTBOUND_COMMS: 'bu59cg46p',
  CALLPILOT_LOGS: 'bu5ppgx2q',             // Call center integration

  // Task system
  CORE_TASKS: 'br9kwm8q9',                 // Different from Sales Tasks!
  SALES_TASK_SUBMISSIONS: 'bu36g8j99',
  ARRIVY_TASKS: 'bvbqgs5yc',               // Field service tasks

  // Financial
  RECEIVABLES: 'btkc3zrxs',
  FUNDING_EVENTS: 'bub2ixkr4',
  OUTSTANDING_ITEMS: 'bt4pxqzx8',
  BILLS: 'budd3c3ti',
  COMMISSION_LOG: 'bu4s5xtja',
  SEQUIFI_COMMISSIONS: 'bu5h3nm6k',

  // Design & Engineering
  DESIGNS: 'bsbhp6zhm',
  BOM: 'btadqc6ga',
  MATERIAL_ORDER_BOM: 'bvgi8955t',
  ADDERS: 'bsaycczmf',
  MATERIALS: 'bsbkmwvu8',
  EQUIPMENT: 'bsg4wcwr5',

  // Support
  ATTACHMENTS: 'br9kwm8ke',                // Universal file storage
  NOTIFICATIONS: 'bsikx9yw7',
  CANCELLATION_REQUESTS: 'bsdcnz75g',
  CANCEL_REQUESTS: 'bt26x67sf',
  SERVICE_RECORDS: 'bt4r9f4vu',
  MILESTONES: 'br9kwm847',
  ORGANIZATIONS: 'br9kwm87j',

  // Logging
  AUDIT_LOG: 'bsiripd8r',
  STATUS_UPDATES: 'bsy4d7a5h',
  CONTACT_UPDATES: 'bs274cv7e',

  // Customer-facing
  SURVEYS: 'br9vmrkp8',
  HOAS: 'bscyuq2i2',
  TEXT_RECORDS: 'bum8nf7jc',
  CALLS: 'bsg48gg8q',

  // Integrations
  ORDER_WEBHOOKS: 'bvf3ydgnq',
  TOA_JOB_IDS: 'bvg6k4a6t',
  AI_OBSERVATIONS: 'bvdcsga5j',
  CALL_TO_ACTION: 'buxcybwum',
} as const;
```

---

## NEXT PHASE RECOMMENDATIONS

### Phase 3: Analyze Workflow Patterns

Now that we have the table structure, we need to:

1. **Query Outreach Records table** (btvik5kwi)
   - Get field schema
   - Understand outreach attempt structure
   - Find: outcome codes, method tracking, timestamp patterns

2. **Query Sales Aid Requests table** (bt3m39fgr)
   - Get field schema
   - Understand escalation workflow
   - Find: reason codes, priority levels, resolution tracking

3. **Analyze trigger conditions**
   - When does "Initial Outreach" appear?
   - When does "Pending Escalation" trigger?
   - What makes a customer "Unresponsive"?
   - When does "Blocked NEM" flag activate?

4. **Understand calculated fields**
   - How is "Days Overdue" computed?
   - What's the grace period logic?
   - How are priorities assigned?

### Questions for Austin

1. **Access Issue**: Task Submissions table (brh36h9mh) returned 401. Is this a permissions issue or different app?

2. **Confirmation Needed**:
   - Is `btvik5kwi` the main outreach logging table PCs use daily?
   - Is `bt3m39fgr` where PCs submit escalation requests?
   - Should we query ZD Outreach (bt9scdhbj) or is that read-only sync?

3. **Dashboard Clarification**:
   - Are there saved QuickBase reports that power each dashboard section?
   - Or is the dashboard built in your current app querying these tables?

---

## ANALYSIS STATISTICS

- **Starting Tables:** 12 known tables
- **Discovered Tables:** 42 new tables via relationship analysis
- **Total Tables:** 54 tables
- **Relationships Mapped:** 152 table relationships
- **Critical Discoveries:** 3 (Outreach, Sales Aid, ZD Outreach)
- **Execution Time:** ~90 seconds
- **API Calls Made:** 36 (12 field schema queries + relationship parsing)

---

*Generated by Phase 2 Analysis Script*
*Script: `/Users/austinelkins/phase2-map-table-relationships.js`*
