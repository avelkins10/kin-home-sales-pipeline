# Quickbase Solar Sales Pipeline - Project Brief

## Project Goal

Build a **Rep Dashboard** for solar sales reps to track their pipeline projects from sale through installation, using data from Quickbase.

## What We're Building

A web application that allows sales reps (Closers, Setters) to:
1. View their assigned projects
2. Track project progress through milestones
3. Monitor holds/blockers
4. Manage adders/add-ons
5. See project timelines and SLA deadlines

## Data Source

**Quickbase Database:**
- **Realm:** kin.quickbase.com
- **Main Table:** br9kwm8na (Projects/Pipeline)
- **Child Tables:**
  - br9kwm8q9 (Tasks)
  - bsaycczmf (Adders/Extra Charges)
- **Total Fields:** 2,032 in main table

## Key Entities

### 1. Projects
- Solar installation projects from sale through completion
- Average timeline: 91 days (median) from sale to install
- Current sample: ~300 active projects

### 2. Milestones (9 stages)
1. Intake
2. Survey (Site Visit)
3. Design (CAD/Engineering)
4. NEM (Interconnection Application)
5. Permitting
6. Installation
7. Verification
8. Inspection
9. PTO (Permission to Operate)

### 3. Team Members
- **Closers:** Sales people who close deals
- **Setters:** Lead setters who schedule appointments
- **Project Coordinators:** Manage project execution

### 4. Adders/Add-ons
- Additional products/services sold with solar system
- Examples: Batteries (Encharge 10kW), Perfect Power Box, Insulation, Electrical Upgrades
- 67.8% of projects have adders
- Require multi-stage approval (Sales → Design → Operations)

## Critical Data Points

### Project Status Distribution
- Active: 41.2%
- Active - On Hold: 21.4% ⚠️
- Completed: 18.6%
- Cancelled: 14.6%
- Active - PTO: 2.8%
- Active - Installed: 1.4%

### Key Metrics
- **21% of projects are on hold** - major bottleneck
- **20% cancellation rate** in recent projects
- **Average sale to install:** 135 days (high variance)

## Key Fields Reference

### Core Project Fields
```javascript
{
  recordId: 3,              // Record ID#
  projectId: 11,            // Project ID
  customerName: 145,        // Customer Name
  customerAddress: 146,     // Customer Address
  customerPhone: 148,       // Customer Phone
  salesDate: 522,           // Sales Date
  projectStatus: 255,       // Project Status
  projectPriority: 300,     // Project Priority
  systemSize: 13,           // System Size (kW)
  systemPrice: 133,         // System Price
  projectAge: 438           // Project Age
}
```

### Team Assignment
```javascript
{
  closerId: 516,            // Closer ID
  closerName: 517,          // Closer Name
  setterId: 329,            // Setter ID
  setterName: 330,          // Setter Name
  projectCoordinatorId: 819,// PC ID
  projectCoordinator: 820   // PC Name
}
```

### Hold/Blocker Fields
```javascript
{
  onHold: 231,              // On Hold? (checkbox)
  holdReason: 232,          // Hold Reason
  blockReason: 233,         // Block Reason
  userPlacedOnHold: 234,    // User Placed On Hold
  dateOnHold: 235,          // Date Project Placed On Hold
  holdBlockers: 1389        // Hold/Blockers (description)
}
```

### Key Milestone Dates (frequently filled)
```javascript
{
  // Survey (25-27% usage)
  surveySubmitted: 164,     // Survey Submitted Date
  surveyApproved: 165,      // Survey Approved Date

  // Design (24-27% usage)
  designCompleted: 315,     // Design Completed
  cadDesignApproved: 476,   // CAD Design Approved
  engineeringSubmitted: 477,// Engineering Submitted
  designSLADeadline: 2459,  // Design SLA Deadline

  // Permit (21-24% usage)
  permitSubmitted: 207,     // Permit Submitted
  permitApproved: 208,      // Permit Approved

  // NEM/PTO (20-24% usage)
  nemSubmitted: 326,        // NEM Submitted Date
  nemApproved: 327,         // NEM Approved Date
  ptoSubmitted: 537,        // PTO Submitted Date
  ptoApproved: 538,         // PTO Approved Date

  // Install (50-54% usage) - MOST RELIABLE
  installScheduled: 710,    // Install Scheduled Date Capture
  installStartDate: 178,    // Install Scheduled Start Date
  installCompleted: 534,    // Install Completed Date
  installationCompletedAt: 587 // Installation - Completed At
}
```

### Adder Fields
```javascript
{
  totalAdders: 252,         // Total # of Adders
  totalAdderCost: 2115,     // Total Adder Cost
  salesFacingList: 2286,    // Sales Facing Adder List
  numApproved: 1046,        // # of Adders (Approved)
  numNeedsReview: 2282,     // # of Needs Review Adders
  addersPPW: 2114          // Total Add-on PPW
}
```

### Status Bar (Visual Indicator)
```javascript
{
  statusBar: 301           // Project Status - Color (HTML table)
}
```
- Shows all 9 milestones with colored dots (🟢🟡🔴)
- NO dates embedded - just visual status

## Technical Architecture Insights

### Parent-Child Table Structure
```
Projects (br9kwm8na)
├── Tasks (br9kwm8q9) - 589 fields
│   ├── Task types: Permit, Survey, Pre-Intake, Funding, etc.
│   └── Status: "Not Started", "Pending", etc.
│
└── Adders (bsaycczmf) - 172 fields
    ├── Product Name, Cost, Category
    ├── Approval status: Pending Review (67%), Approved (33%)
    └── Workflow: Sales → Design → Operations
```

### Data Quality Notes
- **Install dates most reliable:** 50-54% of projects have data
- **Scheduled dates rarely filled:** <10% usage
- **Completed/Submitted dates better than approval dates**
- **Recent projects:** Only 60% have complete timeline data

## User Flows

### Rep Login → Dashboard
1. Identify current user (Closer/Setter)
2. Query projects where `closerId = currentUser.id` or `setterId = currentUser.id`
3. Show project list with:
   - Customer name
   - Current status + priority
   - Days since sale
   - Current milestone
   - Holds/blockers

### Project Detail View
1. Show customer info
2. Display 9-milestone progress bar (like Status Bar field)
3. Show actual dates for each milestone (if available)
4. List adders with approval status
5. Show holds/blockers if any
6. Display SLA deadlines (especially Design SLA)

### Adder Management
1. List all adders for project
2. Show approval status
3. Allow sales rep to:
   - View adder details
   - See if design review needed
   - Check ops approval status
   - Track if "Required for Install"

## Key Challenges Identified

### 1. Data Inconsistency
- 40% of projects missing complete milestone dates
- High variance in timeline (1-910 days)
- Many fields exist but aren't populated

### 2. High Hold Rate
- 21% of projects on hold
- Need prominent hold reason display
- Track who placed hold and when

### 3. Adder Approval Bottleneck
- 67% of adders pending review
- 36% adder cancellation rate
- Multi-stage approval process complex

### 4. Milestone Tracking
- Milestone dates scattered across 150+ date fields
- Some milestones tracked in child tables (Tasks)
- Status Bar HTML is visual only, no dates

## Recommended Features

### Phase 1 - Core Dashboard
- [ ] User authentication & role detection
- [ ] Project list filtered by assigned rep
- [ ] Basic project cards with status
- [ ] Hold/blocker alerts
- [ ] Simple milestone timeline

### Phase 2 - Project Details
- [ ] Full 9-milestone progress view
- [ ] Actual date display for each milestone
- [ ] SLA deadline tracking
- [ ] Adder list with approval status
- [ ] Hold/blocker management

### Phase 3 - Analytics
- [ ] Pipeline metrics (projects by status)
- [ ] Average time per milestone
- [ ] Hold reason breakdown
- [ ] Adder approval velocity
- [ ] Personal performance metrics

## API Integration

### Quickbase REST API
- **Endpoint:** https://api.quickbase.com/v1/
- **Authentication:** QB-USER-TOKEN (user tokens)
- **Key Operations:**
  - GET /v1/fields?tableId={tableId} - Get field definitions
  - POST /v1/records/query - Query records with filters
  - POST /v1/records - Create/update records

### Query Examples

**Get projects for a closer:**
```javascript
POST /v1/records/query
{
  "from": "br9kwm8na",
  "select": [3, 11, 145, 255, 522, 534, 2286, 231, 232],
  "where": "{516.EX.123456}",  // closerId equals user ID
  "options": { "top": 100 }
}
```

**Get adders for a project:**
```javascript
POST /v1/records/query
{
  "from": "bsaycczmf",
  "select": [3, 56, 8, 13, 20, 23],
  "where": "{10.EX.358}",  // Related Project equals project ID
  "options": { "top": 50 }
}
```

## Success Metrics

### For Reps
- Reduce time to find project status (from minutes to seconds)
- Clear visibility into holds/blockers
- Easy adder tracking
- SLA awareness (especially Design deadlines)

### For Business
- Reduce hold time (21% → target <10%)
- Improve adder approval speed (67% pending → <30%)
- Better timeline predictability
- Reduced cancellation rate (20% → <15%)

## Next Steps

1. **Design UI mockups** for:
   - Dashboard/project list
   - Project detail view
   - Milestone timeline component
   - Adder management interface

2. **Set up development environment:**
   - Choose framework (React, Vue, etc.)
   - Configure Quickbase API access
   - Set up authentication

3. **Build core features:**
   - User login & role detection
   - Project list query
   - Basic project cards
   - Milestone progress display

4. **Iterate based on feedback:**
   - Test with actual sales reps
   - Refine filters and views
   - Add requested features
