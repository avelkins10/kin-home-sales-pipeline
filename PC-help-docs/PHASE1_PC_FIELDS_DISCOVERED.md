# PHASE 1: PC DASHBOARD FIELDS - DISCOVERY RESULTS

**Analysis Date:** 2025-10-24
**QuickBase Realm:** kin.quickbase.com
**Project Coordinators:** Emma Martin (150 projects), Paige Elkins (150 projects)

---

## EXECUTIVE SUMMARY

Phase 1 successfully discovered **110 fields** across 6 QuickBase tables that support PC dashboard functionality. All critical missing fields have been located except for "overdue" calculations which appear to be dynamically computed.

### Project Load Verification
- **Emma Martin**: 150 projects (31 Active, 53 Complete, 59 Cancelled)
- **Paige Elkins**: 150 projects (35 ROR, 102 Complete, 12 Cancelled)
- Confirms: PCs manage 100+ projects across all lifecycle stages

---

## CRITICAL FIELD DISCOVERIES

### 1. REPCARD ID (Sales Rep Identifier)
**Status:** ✅ FOUND

| Field ID | Label | Table | Type | Usage |
|----------|-------|-------|------|-------|
| 2277 | Related Closer - repcard_id | Projects | text | Primary rep ID |
| 2279 | Related Setter - repcard_id | Projects | text | Setter rep ID |
| 2395 | Related Closer - repcard_id - rc_image | Projects | text | Rep profile image |
| 2390 | Related Setter - repcard_id - rc_office_id | Projects | text | Rep office ID |
| 2393 | Related Setter - repcard_id - rc_team | Projects | text | Rep team name |
| 2392 | Related Setter - repcard_id - rc_team_id | Projects | text | Rep team ID |
| 2389 | Related Setter - repcard_id - Is Rookie? | Projects | checkbox | Rep experience flag |

**Key Insight:** Field 2277 is the primary closer repcard ID. This links to the sales rep's RepCard profile.

---

### 2. OUTREACH METHODS & TRACKING
**Status:** ✅ FOUND

#### Preferred Contact Methods
| Field ID | Label | Table | Type | Values |
|----------|-------|-------|------|--------|
| 2404 | PC Outreach: Preferred Outreach Method | Projects | multitext | Text, Email, Call |
| 2086 | Manual Preferred Outreach | Projects | multitext | Text, Email, Call |
| 2087 | Calculated Preferred Outreach | Projects | multitext | Computed from manual |

#### Outreach Records & Links
| Field ID | Label | Table | Type | Purpose |
|----------|-------|-------|------|---------|
| 1640 | Outreach records | Projects | dblink | Link to outreach table |
| 1641 | Add Outreach | Projects | url | Quick-add button |
| 2267 | # of Outreach records | Projects | numeric | Total count |
| 2268 | # of Due Milestone Outreach records | Projects | numeric | Overdue count |
| 2149 | First Outreach Completed Date | Projects | timestamp | Initial contact |
| 2264 | Maximum PC Outreach Completed Date | Projects | timestamp | Most recent |

#### PC-Specific Outreach Flags
| Field ID | Label | Table | Type | Purpose |
|----------|-------|-------|------|---------|
| 2287 | PC Outreach: Create Check-In? | Projects | checkbox | Needs check-in |
| 2266 | PC Outreach: Incomplete Check-In? | Projects | checkbox | Follow-up needed |
| 1980 | Needs WIP Touchpoint? | Projects | checkbox | WIP follow-up flag |

**Key Insight:** Field 2404 stores customer's preferred contact method. Field 2268 likely drives "Initial Outreach" dashboard count.

---

### 3. CONTACT ATTEMPTS TRACKING
**Status:** ✅ FOUND (Phase-Based)

| Field ID | Label | Table | Type | Phase |
|----------|-------|-------|------|-------|
| 1708 | # of Contact Attempts - Intake | Projects | numeric | Intake |
| 1711 | # of Contact Attempts - Install | Projects | numeric | Install |
| 1709 | # of Contact Attempts - NEM | Projects | numeric | NEM/PTO |
| 1710 | # of Contact Attempts - PTO | Projects | numeric | PTO |
| 2444 | # of Contact Attempts - Intake (source) | Projects | numeric | Intake source |

**Key Insight:** Attempts are tracked per project phase, not globally. For "Unresponsive Customers" dashboard, likely uses field 1909 or counts across phases.

---

### 4. LAST CONTACT TRACKING
**Status:** ✅ FOUND (Phase-Based)

#### Last Contact Dates
| Field ID | Label | Table | Type | Phase |
|----------|-------|-------|------|-------|
| 1731 | Last Attempted Contact Date - Intake | Projects | date | Intake |
| 1734 | Last Attempted Contact Date - Install | Projects | date | Install |
| 1732 | Last Attempted Contact Date - NEM | Projects | date | NEM |
| 1733 | Last Attempted Contact Date - PTO | Projects | date | PTO |

#### Days Since Last Contact
| Field ID | Label | Table | Type | Phase |
|----------|-------|-------|------|-------|
| 1735 | Days Since Last Contact Attempt - Intake | Projects | numeric | Intake |
| 1738 | Days Since Last Contact Attempt - Install | Projects | numeric | Install |
| 1736 | Days Since Last Contact Attempt - NEM | Projects | numeric | NEM |
| 1737 | Days Since Last Contact Attempt - PTO | Projects | numeric | PTO |

#### Last Contact Notes
| Field ID | Label | Table | Type | Phase |
|----------|-------|-------|------|-------|
| 1739 | Last Contact Attempt Note - Intake | Projects | text | Intake |
| 1742 | Last Contact Attempt Note - Install | Projects | text | Install |
| 1740 | Last Contact Attempt Note - NEM | Projects | text | NEM |
| 1741 | Last Contact Attempt Note - PTO | Projects | text | PTO |

#### Last Contact Record IDs
| Field ID | Label | Table | Type | Phase |
|----------|-------|-------|------|-------|
| 1727 | Last Attempted Date Record ID - Intake | Projects | numeric | Intake |
| 1730 | Last Attempted Date Record ID - Install | Projects | numeric | Install |
| 1729 | Last Attempted Date Record ID - NEM | Projects | numeric | NEM |
| 1728 | Last Attempted Date Record ID - PTO | Projects | numeric | PTO |

**Key Insight:** QuickBase stores last contact per phase. PC dashboard likely uses phase-specific fields based on current project status.

---

### 5. ESCALATION & SALES AID
**Status:** ✅ FOUND

#### Core Escalation Fields
| Field ID | Label | Table | Type | Values |
|----------|-------|-------|------|--------|
| 322 | Project Escalation | Projects | text-multi-line | ARC, High, Medium, Low |
| 1909 | Is Unresponsive? | Projects | text | Boolean-like flag |

#### Sales Aid Tracking
| Field ID | Label | Table | Type | Purpose |
|----------|-------|-------|------|---------|
| 1792 | Sales Aid Request Records | Projects | dblink | Link to sales aid table |
| 1793 | Request Sales Aid | Projects | url | Quick-add button |
| 1912 | # of Open Sales Aid records | Projects | numeric | Active requests |
| 2422 | # of Escalated Sales Aid Records | Projects | numeric | **ESCALATION COUNT** |
| 1908 | # of Active Unresponsive Sales Aid Records | Projects | numeric | **UNRESPONSIVE COUNT** |
| 2423 | # of Pending Closeout Sales Aid | Projects | numeric | Ready to close |
| 2421 | # of Sales Aid Adder Records | Projects | numeric | Additional requests |
| 2416 | Maximum Sales Aid Date Created | Projects | timestamp | Most recent |
| 2417 | Max Record Sales Aid Reason | Projects | multitext | Reason codes |

#### Sales Aid in Other Tables
| Field ID | Label | Table | Type |
|----------|-------|-------|------|
| 28 | Notify Escalation Manager | Install Communications | checkbox |
| 126 | Related Sales Aid Request | Install Communications | numeric |
| 69 | Sales Aid Request Note | Task Groups | text |
| 72 | Sales Aid Reason | Task Groups | text |

**Key Insight:** Field 2422 likely drives "Pending Escalation" dashboard count. Field 1908 drives "Unresponsive Customers" count.

---

### 6. DUE DATES & SCHEDULING
**Status:** ✅ FOUND (32 fields)

QuickBase tracks numerous scheduled dates across project lifecycle:

#### Installation Scheduling
- 710: Install Scheduled Date Capture
- 2628: Arrivy Installation: Milestone Scheduled Date
- 2610: Arrivy Installation: Max PV Module Task - Scheduled Date/Time
- 2616: Arrivy Installation: Max Electrical Task - Scheduled Date/Time

#### Inspection Scheduling
- 226: Inspection Scheduled Date
- 1589: First Inspection Scheduled Date/Time
- 1815: Task: Inspection Scheduled Date/Time

#### Survey Scheduling
- 166: Survey Scheduled Date
- 793: Maximum Survey Scheduled Date
- 1010: Site Survey Scheduled Date/Time
- 2526: Site Survey: Arrivy Scheduled Date/Time
- 2210: Site Survey: Calendly Scheduled Date/Time

#### Other Deadlines
- 1137: ARC Resolution Due Date
- 2459: Design SLA Deadline
- 1573: ZD ticket due date
- 2234: MPU Scheduled Date

**Key Insight:** No single "PC Outreach Due Date" field found. Dashboard likely uses milestone-based scheduling or calculates from last contact + grace period.

---

### 7. OVERDUE CALCULATIONS
**Status:** ⚠️ NOT FOUND (Likely Calculated)

No stored "overdue" or "days overdue" fields were found. This data is likely **calculated dynamically** using:

```
Days Overdue = TODAY() - [Last Contact Date] - [Grace Period]
```

Or:
```
Days Overdue = TODAY() - [Scheduled Date]
```

**Recommendation:** Query QuickBase reports/views to find calculation formulas, or implement in new PC platform.

---

### 8. NEM/PTO OUTREACH
**Status:** ✅ FOUND

| Field ID | Label | Table | Type |
|----------|-------|-------|------|
| 1976 | Recent Note - Record ID# (NEM/PTO Outreach) | Projects | numeric |
| 1977 | Recent Note - Record ID# (NEM/PTO Outreach) - Note | Projects | text |
| 1978 | Recent Note - Record ID# (NEM/PTO Outreach) - Days Since Note Created | Projects | text |
| 135 | NEM Blocker Outreach | Install Communications | checkbox |
| 175 | Add Outreach Note | Interconnections | url |
| 174 | Project - Days Since Last NEM Outreach | Interconnections | text |

**Key Insight:** NEM/PTO outreach tracked separately. Field 135 likely drives "Blocked NEM" dashboard section.

---

## PC DASHBOARD SECTION MAPPINGS

Based on field discovery, here's the likely data source for each dashboard section:

### Initial Outreach (19+ items)
**Query Logic:**
```javascript
WHERE:
  {2268.GT.0}  // # of Due Milestone Outreach records > 0
  OR {2287.EX.true}  // PC Outreach: Create Check-In? = true
  OR {2266.EX.true}  // PC Outreach: Incomplete Check-In? = true
```

### Unresponsive Customers
**Query Logic:**
```javascript
WHERE:
  {1909.EX."Yes"}  // Is Unresponsive? = Yes
  OR {1908.GT.0}  // # of Active Unresponsive Sales Aid Records > 0
  OR (Days Since Last Contact > threshold AND Attempts > 3)
```

### Pending Escalation
**Query Logic:**
```javascript
WHERE:
  {2422.GT.0}  // # of Escalated Sales Aid Records > 0
  OR {322.EX."High"}  // Project Escalation = High
  OR {322.EX."ARC"}  // Project Escalation = ARC
```

### Blocked NEM
**Query Logic:**
```javascript
WHERE:
  {135.EX.true}  // NEM Blocker Outreach (from Install Communications)
  AND Status in NEM/PTO phase
```

---

## FIELD ADDITIONS NEEDED IN YOUR CODEBASE

Update `/Users/austinelkins/lib/constants/fieldIds.ts`:

```typescript
// PC DASHBOARD FIELDS - NEWLY DISCOVERED
export const PC_DASHBOARD_FIELDS = {
  // Repcard (Sales Rep)
  CLOSER_REPCARD_ID: 2277,
  SETTER_REPCARD_ID: 2279,
  CLOSER_REPCARD_IMAGE: 2395,
  SETTER_REPCARD_OFFICE_ID: 2390,
  SETTER_REPCARD_TEAM: 2393,
  SETTER_REPCARD_TEAM_ID: 2392,
  SETTER_IS_ROOKIE: 2389,

  // Outreach Methods
  PREFERRED_OUTREACH_METHOD: 2404,
  MANUAL_PREFERRED_OUTREACH: 2086,
  CALCULATED_PREFERRED_OUTREACH: 2087,

  // Outreach Records & Tracking
  OUTREACH_RECORDS: 1640,
  ADD_OUTREACH_URL: 1641,
  NUM_OUTREACH_RECORDS: 2267,
  NUM_DUE_MILESTONE_OUTREACH: 2268,
  FIRST_OUTREACH_COMPLETED_DATE: 2149,
  MAX_PC_OUTREACH_COMPLETED_DATE: 2264,

  // PC Outreach Flags
  PC_CREATE_CHECKIN: 2287,
  PC_INCOMPLETE_CHECKIN: 2266,
  NEEDS_WIP_TOUCHPOINT: 1980,

  // Contact Attempts (by phase)
  CONTACT_ATTEMPTS_INTAKE: 1708,
  CONTACT_ATTEMPTS_INSTALL: 1711,
  CONTACT_ATTEMPTS_NEM: 1709,
  CONTACT_ATTEMPTS_PTO: 1710,
  CONTACT_ATTEMPTS_INTAKE_SOURCE: 2444,

  // Last Contact Dates (by phase)
  LAST_CONTACT_DATE_INTAKE: 1731,
  LAST_CONTACT_DATE_INSTALL: 1734,
  LAST_CONTACT_DATE_NEM: 1732,
  LAST_CONTACT_DATE_PTO: 1733,

  // Days Since Last Contact (by phase)
  DAYS_SINCE_CONTACT_INTAKE: 1735,
  DAYS_SINCE_CONTACT_INSTALL: 1738,
  DAYS_SINCE_CONTACT_NEM: 1736,
  DAYS_SINCE_CONTACT_PTO: 1737,

  // Last Contact Notes (by phase)
  LAST_CONTACT_NOTE_INTAKE: 1739,
  LAST_CONTACT_NOTE_INSTALL: 1742,
  LAST_CONTACT_NOTE_NEM: 1740,
  LAST_CONTACT_NOTE_PTO: 1741,

  // Last Contact Record IDs (by phase)
  LAST_CONTACT_RECORD_ID_INTAKE: 1727,
  LAST_CONTACT_RECORD_ID_INSTALL: 1730,
  LAST_CONTACT_RECORD_ID_NEM: 1729,
  LAST_CONTACT_RECORD_ID_PTO: 1728,

  // Escalation & Sales Aid
  PROJECT_ESCALATION: 322,
  IS_UNRESPONSIVE: 1909,
  SALES_AID_RECORDS: 1792,
  REQUEST_SALES_AID_URL: 1793,
  NUM_OPEN_SALES_AID: 1912,
  NUM_ESCALATED_SALES_AID: 2422,
  NUM_ACTIVE_UNRESPONSIVE_SALES_AID: 1908,
  NUM_PENDING_CLOSEOUT_SALES_AID: 2423,
  NUM_SALES_AID_ADDER: 2421,
  MAX_SALES_AID_DATE_CREATED: 2416,
  MAX_SALES_AID_REASON: 2417,

  // NEM/PTO Outreach
  RECENT_NOTE_NEM_PTO_ID: 1976,
  RECENT_NOTE_NEM_PTO_TEXT: 1977,
  RECENT_NOTE_NEM_PTO_DAYS_SINCE: 1978,

  // Scheduling (selected key fields)
  INSTALL_SCHEDULED_DATE: 710,
  INSPECTION_SCHEDULED_DATE: 226,
  SURVEY_SCHEDULED_DATE: 166,
  ARC_RESOLUTION_DUE_DATE: 1137,
  DESIGN_SLA_DEADLINE: 2459,
} as const;

// Install Communications table fields
export const INSTALL_COMM_FIELDS = {
  NEM_BLOCKER_OUTREACH: 135,
  NOTIFY_ESCALATION_MANAGER: 28,
  RELATED_SALES_AID_REQUEST: 126,
} as const;

// Task Groups table fields
export const TASK_GROUP_FIELDS_ADDITIONS = {
  SALES_AID_REQUEST_NOTE: 69,
  SALES_AID_REASON: 72,
  SMS_OUTREACH_DAYS_SINCE_CREATION: 48,
} as const;

// Interconnections table fields
export const INTERCONNECTION_FIELDS = {
  ADD_OUTREACH_NOTE_URL: 175,
  DAYS_SINCE_LAST_NEM_OUTREACH: 174,
  INSPECTION_SCHEDULED_DATE: 138,
} as const;
```

---

## NEXT STEPS (Phase 2)

1. **Map Table Relationships**
   - Projects → Install Communications (how related?)
   - Projects → Task Groups → Tasks
   - Projects → Interconnections
   - Projects → Sales Aid Requests table (need to find table ID)

2. **Query Outreach Records Table**
   - Field 1640 links to "Outreach records" - need to find this table ID
   - Likely contains detailed outreach attempt logs

3. **Query Sales Aid Requests Table**
   - Field 1792 links to "Sales Aid Request Records" - need table ID
   - Contains escalation details and reason codes

4. **Analyze QB Reports/Views**
   - PC dashboard likely uses saved QB reports
   - Need to extract report IDs and filter logic

5. **Understand Grace Periods**
   - How is "overdue" calculated?
   - What triggers escalation?
   - Different thresholds per project phase?

---

## QUESTIONS FOR AUSTIN

1. **Outreach Records Table**: Do you know the table ID for the "Outreach records" that field 1640 links to? Or is this an external system?

2. **Sales Aid Requests Table**: Field 1792 links to "Sales Aid Request Records" - is this a separate QuickBase table in this app?

3. **Dashboard Report IDs**: Can you share the QuickBase report IDs that power each PC dashboard section? This would show exact filter logic.

4. **Grace Period Calculation**: How do PCs currently determine if a project is "overdue" for outreach? Is it a manual judgment or calculated rule?

5. **NEM Blocker Criteria**: What makes a project appear in "Blocked NEM" section? Just the checkbox in Install Communications, or other factors?

---

## ANALYSIS STATISTICS

- **Tables Searched:** 6 (Projects, Install Communications, Task Groups, Tasks, Installations, Interconnections)
- **Fields Discovered:** 110 relevant fields
- **Missing/Calculated:** 1 category (overdue)
- **New Field IDs to Add:** 70+ fields for your codebase
- **Execution Time:** ~45 seconds
- **API Calls Made:** 24 (field schema + data queries)

---

*Generated by Phase 1 Analysis Script*
*Script: `/Users/austinelkins/phase1-search-missing-pc-fields.js`*
