# Phase Alignment Guide - Cursor Implementation vs TRAYCER Spec

**Last Updated**: 2025-01-02
**Status**: Cursor completed Phase 3 (detailed project view) | Dashboard & Projects List pages still needed

---

## Executive Summary

Cursor successfully implemented a **comprehensive project detail page** with 9-milestone tracking, hold management, and deep project analysis. However, we still need to build the **Dashboard** and **Projects List** pages from our original TRAYCER-UI-SPEC.md.

This guide shows:
1. What Cursor built (Phase 3 detail view)
2. What we originally spec'd (Dashboard + Projects List)
3. How to build the missing pieces
4. How to integrate both approaches

---

## 1. What Cursor Built (Phase 3)

### Implemented Components ✅

**Project Detail Page** (`/projects/[id]`)
- **ProjectHeader**: Customer name, back button, status badges, hold alerts
- **CustomerContactCard**: Phone, email, address with action buttons
- **SystemSpecsCard**: System size, pricing, equipment details
- **TeamMembersCard**: Closer, setter, coordinator info
- **AddersCard**: Adder list, costs, approval status with urgency indicators
- **Timeline**: 9-milestone vertical visualization with substeps
  - Intake → Survey → Design → HOA (conditional) → Permit → NEM → Install → Inspection → PTO
- **MilestoneNode**: Individual milestone with status, substeps, warnings, dates
- **MilestoneConnector**: Visual lines between milestones
- **HoldManagementCard**: Place/release/update hold with API integration

### Key Features ✅
- **9 detailed milestones** (vs our spec's 7 compact traffic lights)
- **Vertical timeline** with substeps and warnings
- **Hold management system** with optimistic updates
- **Comprehensive testing** (>90% coverage target)
- **Conditional HOA milestone** rendering
- **Mobile-responsive** horizontal scroll

### What This Gives You
- Deep project analysis for reps managing individual deals
- Detailed milestone tracking with edge cases
- Full CRUD hold management
- Production-ready with tests

---

## 2. What We Originally Spec'd (TRAYCER-UI-SPEC.md)

### Missing Components ❌

**Dashboard Page** (`/`)
- **DashboardMetrics**: 4 key metric cards
  1. Installs This Week
  2. Active Accounts (not on hold)
  3. On Hold (with breakdown: "3 Finance, 2 Roof, 1 Customer")
  4. Monthly Installs
- **UrgentAlerts**: Projects on hold > 7 days (red banner)
- **RecentProjects**: 5 most recent active projects
- Welcome header with user name and role

**Projects List Page** (`/projects`)
- **Horizontal traffic lights** (7 milestones in compact row)
  - Intake → Survey → Design → NEM → Permit → Install → Inspection/PTO
  - Emoji icons, color-coded states, pulse animation
  - Status text below: "NEM: Submitted 12/28 • 5d waiting"
- **Filter chips** (9 views)
  - All | Active | On Hold | Install Ready | Install Scheduled | Install Completed | Pending Cancel | Cancelled | Needs Attention
- **SearchBar**: Debounced search by name or project ID
- **ProjectRow**: Horizontal layout
  - Column 1: Customer info (220px)
  - Column 2: Traffic lights (flexible)
  - Column 3: Metrics (160px) - Size, PPW, Value
  - Column 4: Age indicator (80px)
- **PPWDisplay**: Dual PPW with delta (Sold: $3.20 | Yours: $2.80 ↓ -$0.40)
- **HoldBanner**: Colored banner above row by hold type
- **ProjectAgeIndicator**: Color-coded age warnings

### What This Would Give You
- Quick scanning of all projects at a glance
- Fast filtering and searching
- Horizontal traffic lights for scannable status
- iPad-optimized landscape layout
- Dashboard overview with key metrics

---

## 3. Gap Analysis

| Feature | Cursor Built | TRAYCER Spec | Priority |
|---------|-------------|--------------|----------|
| **Project Detail** | ✅ 9 milestones (vertical) | 6 tabs (Overview, Timeline, System, etc.) | Keep Cursor's ✅ |
| **Hold Management** | ✅ Full CRUD | Not in spec | Keep Cursor's ✅ |
| **Dashboard Page** | ❌ | ✅ 4 metrics + alerts | **BUILD THIS** 🔴 |
| **Projects List** | ❌ | ✅ Horizontal traffic lights | **BUILD THIS** 🔴 |
| **Traffic Lights** | Vertical timeline | Horizontal 7-light row | **BUILD THIS** 🔴 |
| **Filter Chips** | ❌ | ✅ 9 filter views | **BUILD THIS** 🔴 |
| **Search Bar** | ❌ | ✅ Debounced search | **BUILD THIS** 🔴 |
| **PPW Delta** | ❌ | ✅ Sold vs Commission | **BUILD THIS** 🔴 |

---

## 4. Recommended Hybrid Approach

**Use both implementations for different purposes:**

### Quick Scanning (Build from TRAYCER spec)
1. **Dashboard** (`/`) - High-level metrics, urgent alerts, recent projects
2. **Projects List** (`/projects`) - Horizontal traffic lights for fast scanning
   - Quick view of all projects
   - Filter by status, search by name
   - Click row → detail page

### Deep Analysis (Already built by Cursor)
3. **Project Detail** (`/projects/[id]`) - Use Cursor's 9-milestone timeline
   - Vertical timeline with substeps
   - Hold management
   - Full project details

**User Flow:**
```
Dashboard → Projects List (scan all) → Click project → Detail page (deep dive)
    ↓              ↓                           ↓
  Metrics   Horizontal traffic         9-milestone timeline
  Alerts    Filter/search             Hold management
  Recent    Quick metrics             Detailed analysis
```

---

## 5. Implementation Roadmap

### Phase 4A: Build Dashboard Page (2-3 hours)

Follow TRAYCER-INSTRUCTIONS.md Phase 3 prompts.

#### Step 1: Dashboard Metrics Component

**Prompt for Cursor/Traycer:**
```
Create components/dashboard/DashboardMetrics.tsx based on TRAYCER-UI-SPEC.md section 2.2

Requirements:
- Props: userId (string), role (string)
- Fetch data using getDashboardMetrics() query (create in lib/quickbase/queries.ts)
- 4 metric cards in responsive grid (1 col mobile, 2 tablet, 4 desktop)
- Metrics:
  1. Installs This Week - Calendar icon, blue
  2. Active Accounts - TrendingUp icon, green, subtitle "Not on hold"
  3. On Hold - Pause icon, red, subtitle shows breakdown "3 Finance, 2 Roof"
  4. Monthly Installs - CheckCircle icon, purple
- Use Card, CardContent from @components/ui/card
- Use TanStack Query with useQuery hook
- Icons from lucide-react

First implement getDashboardMetrics() in lib/quickbase/queries.ts:

export async function getDashboardMetrics(userId: string, role: string): Promise<{
  installsThisWeek: number
  activeProjects: number
  onHold: number
  holdBreakdown: string
  installsThisMonth: number
}> {
  // Query Quickbase for metrics
  // installsThisWeek: Count where INSTALL_COMPLETED_DATE in current week
  // activeProjects: Count where NOT on hold, NOT completed, NOT cancelled
  // onHold: Count where PROJECT_STATUS contains 'Hold'
  // holdBreakdown: Count by hold type, format "3 Finance, 2 Roof, 1 Customer"
  // installsThisMonth: Count where INSTALL_COMPLETED_DATE in current month
}
```

#### Step 2: Urgent Alerts Component

**Prompt:**
```
Create components/dashboard/UrgentAlerts.tsx based on TRAYCER-UI-SPEC.md section 2.3

Requirements:
- Props: userId, role
- Fetch using getUrgentProjects() query (create in lib/quickbase/queries.ts)
- Red alert banner if projects on hold > 7 days exist
- Show first 3 projects with: customer name, days on hold, hold reason
- "View all X on hold" link if more than 3
- Hide entire component if no urgent projects
- Each project clickable → /projects/{recordId}
- Icons: AlertTriangle, Clock from lucide-react

First implement getUrgentProjects():

export async function getUrgentProjects(userId: string, role: string): Promise<Array<{
  recordId: string
  customerName: string
  daysOnHold: number
  holdReason: string
}>> {
  // Query where PROJECT_STATUS contains 'Hold' AND DATE_ON_HOLD > 7 days ago
  // Calculate daysOnHold from DATE_ON_HOLD
  // Sort by daysOnHold DESC
}
```

#### Step 3: Recent Projects Component

**Prompt:**
```
Create components/dashboard/RecentProjects.tsx based on TRAYCER-UI-SPEC.md section 2.4

Requirements:
- Props: userId, role
- Fetch using getRecentProjects() query (create in queries.ts)
- Show 5 most recent active projects
- Each shows: customer name, project ID, current milestone badge, arrow icon
- "View all" link to /projects
- Clickable → /projects/{recordId}
- Use getCurrentMilestone() from @lib/utils/milestones
- Use Badge from @components/ui/badge

Implementation:

export async function getRecentProjects(userId: string, role: string) {
  // Query most recent 5 active projects
  // Sort by SALES_DATE DESC or PROJECT_AGE DESC
}
```

#### Step 4: Dashboard Page

**Prompt:**
```
Update app/(dashboard)/page.tsx to match TRAYCER-UI-SPEC.md section 2.1

Requirements:
- Server component (async function)
- Get session with getServerSession
- Redirect to /login if no session
- Layout:
  - Header: "Welcome back, {name}" + role subtitle
  - UrgentAlerts (in Suspense with AlertsSkeleton)
  - DashboardMetrics (in Suspense with MetricsSkeleton)
  - RecentProjects (in Suspense with RecentProjectsSkeleton)
- Pass userId and role to all components

Helper function:

function getRoleDisplayName(role: string) {
  switch (role) {
    case 'closer': return 'Closer Dashboard'
    case 'setter': return 'Setter Dashboard'
    case 'office_leader': return 'Office Leader Dashboard'
    case 'regional': return 'Regional Manager Dashboard'
    case 'super_admin': return 'Super Admin Dashboard'
    default: return 'Dashboard'
  }
}
```

### Phase 4B: Build Projects List Page (3-4 hours)

Follow TRAYCER-INSTRUCTIONS.md Phase 4 prompts.

#### Step 1: Shared Components (Build these first)

**PPWDisplay Component:**
```
Create components/projects/PPWDisplay.tsx based on TRAYCER-UI-SPEC.md section 5.2

Requirements:
- Props: soldPPW (number|null), commissionablePPW (number|null), expanded (boolean)
- Shows both Sold PPW and Commissionable PPW
- Delta: commissionablePPW - soldPPW
- Delta color: green if positive, red if negative, gray if zero
- Delta icon: ArrowUp/ArrowDown/Minus
- Compact mode: inline display
- Expanded mode: stacked rows
- Icons from lucide-react

Example output (compact):
Sold: $3.20 | Yours: $2.80 ↓ -$0.40  (red)
```

**ProjectAgeIndicator Component:**
```
Create components/projects/ProjectAgeIndicator.tsx based on TRAYCER-UI-SPEC.md section 5.4

Requirements:
- Props: age (number in days)
- Color coding:
  - < 90 days: gray
  - 90-120 days: amber with "WARNING" label
  - > 120 days: red with "CRITICAL" label
- Show Calendar icon
- Display "{age}d"
- Use cn() for conditional classes
```

**HoldBanner Component:**
```
Create components/projects/HoldBanner.tsx based on TRAYCER-UI-SPEC.md section 5.3

Requirements:
- Props: project, holdType
- Get hold reason from HOLD_REASON or BLOCK_REASON fields
- Calculate days on hold from DATE_ON_HOLD
- Color by hold type:
  - finance: red, roof: orange, customer: amber
  - permit: yellow, hoa: lime, generic: gray
- Show AlertTriangle icon, hold label, reason, days on hold
- Rounded top corners, 4px left border
```

**TrafficLightPipeline Component (KEY COMPONENT):**
```
Create components/projects/TrafficLightPipeline.tsx based on TRAYCER-UI-SPEC.md section 5.1

This is the CENTERPIECE - horizontal traffic lights for list view

Requirements:
- Props: project (QuickbaseProject)
- 7 traffic lights in horizontal row (not 9!)
  - Intake, Survey, Design, NEM, Permit, Install, Inspection/PTO
- Each light: circular 32px with emoji icon
- Colors by state:
  - complete: green-500
  - in-progress: amber-500 with PULSE animation
  - pending: gray-200
  - on-hold: red-500
  - overdue: red-600 with ring
- Connector lines between lights (4px wide, green if complete, gray otherwise)
- Status text below: "NEM: Submitted 12/28 • 5d waiting"
- Use calculateMilestoneState() from @lib/utils/traffic-lights
- Use getMilestoneStatusText() from @lib/utils/traffic-lights

Emojis:
- Intake: 📋, Survey: 📸, Design: 📐
- NEM: 🔌, Permit: 📋, Install: 🔧, Inspection: ✅

NOTE: You'll need to create lib/utils/traffic-lights.ts
(See TRAYCER-UI-SPEC.md section 6.1 for full implementation)
```

#### Step 2: Traffic Light State Logic (CRITICAL)

**Prompt:**
```
Create lib/utils/traffic-lights.ts based on TRAYCER-UI-SPEC.md section 6.1

This is COMPLEX - implement carefully.

Requirements:
- Export types: MilestoneId, MilestoneState
- calculateMilestoneState(project, milestoneId): Returns state for each of 7 milestones
  - States: 'complete', 'in-progress', 'pending', 'on-hold', 'overdue'
  - 7 milestones: intake, survey, design, nem, permit, install, inspection
  - Each has specific completion criteria
  - If project on hold, current milestone shows 'on-hold'
- getMilestoneStatusText(project): Returns text like "NEM: Submitted 12/28 • 5d waiting"

Helper functions (internal):
- calculateIntakeState(project)
- calculateSurveyState(project)
- calculateDesignState(project)
- calculateNEMState(project)
- calculatePermitState(project)
- calculateInstallState(project)
- calculateInspectionState(project)
- getCurrentMilestoneId(project)
- calculateDaysWaiting(submitDate)
- formatDate(dateStr) → MM/DD
- detectHoldStatus(status)

Use PROJECT_FIELDS from @lib/constants/fieldIds
Use hold detection from @lib/utils/hold-detection if available

See TRAYCER-UI-SPEC.md section 6.1 lines 100-300 for complete implementation
```

#### Step 3: Filter Chips & Search

**Prompt:**
```
Create components/projects/ProjectFilterChips.tsx based on TRAYCER-UI-SPEC.md section 3.2

Requirements:
- 9 filter chips in horizontal scrollable row
- Chips: All, Active, On Hold, Install Ready, Install Scheduled, Install Completed, Pending Cancel, Cancelled, Needs Attention
- Active chip: blue background, white text
- Inactive: gray background, hover effect
- Updates URL param ?view=...
- Round pill shape, smooth transitions
```

**Prompt:**
```
Create components/projects/SearchBar.tsx

Requirements:
- Search input with Search icon
- Debounced (300ms) using useDebouncedCallback from 'use-debounce'
- Updates URL param ?search=...
- Placeholder: "Search by name or project ID..."
- Max width 448px

Install: npm install use-debounce
```

#### Step 4: Project Row & Table View

**Prompt:**
```
Create components/projects/ProjectRow.tsx based on TRAYCER-UI-SPEC.md section 3.4

This is COMPLEX - the main list item with horizontal traffic lights

Requirements:
- Props: project
- 4-column grid: [220px customer | flexible traffic lights | 160px metrics | 80px age]
- Hold banner above row (rounded top) if on hold
- Clickable → /projects/{recordId}
- Hover shadow

Column 1 - Customer Info:
- Full name using parseCustomerName()
- Address line 1 & 2
- Phone as tel: link (stop propagation)

Column 2 - Traffic Lights:
- TrafficLightPipeline component

Column 3 - Metrics (3 rows):
- Size: label + formatSystemSize()
- PPW: label + PPWDisplay component
- Value: label + formatCurrency()

Column 4 - Age:
- ProjectAgeIndicator component

Use components: TrafficLightPipeline, PPWDisplay, HoldBanner, ProjectAgeIndicator
Use utilities: parseCustomerName, formatAddress, getProjectAge, detectHoldStatus
```

**Prompt:**
```
Create components/projects/ProjectTableView.tsx based on TRAYCER-UI-SPEC.md section 3.3

Requirements:
- Props: userId, role, view, search
- Fetch using getProjectsForUser(userId, role, view, search)
- TanStack Query with 30-second refetch
- Map projects to ProjectRow
- Loading skeleton
- Empty state: "No projects found"
```

#### Step 5: Update Projects Page

**Prompt:**
```
Update app/(dashboard)/projects/page.tsx based on TRAYCER-UI-SPEC.md section 3.1

Requirements:
- Server component with searchParams
- Get session, redirect if not authenticated
- Layout:
  - ProjectListHeader (title + subtitle)
  - Row with SearchBar and ProjectFilterChips
  - ProjectTableView in Suspense
- Pass searchParams.view and searchParams.search

Create simple ProjectListHeader:
- H1: "Projects"
- Subtitle: "Manage and track all your solar installation projects"
```

#### Step 6: Implement Query Logic (CRITICAL)

**Prompt:**
```
Implement getProjectsForUser() in lib/quickbase/queries.ts

This is the most complex query - implements all 9 filter views

Requirements:
- Parameters: userId, role, view, search
- Build where clause based on:
  - Role filtering (closer, office_leader, regional, etc.)
  - View filtering (active, on-hold, install-ready, etc.)
  - Search filtering (customer name or project ID)
- Return sorted results

Filter views logic:

'active': NOT on hold, NOT completed, NOT cancelled
'on-hold': PROJECT_STATUS contains 'Hold'
'install-ready': NEM_APPROVED='Yes' AND PERMIT_APPROVED='Yes' AND INSTALL_SCHEDULED_DATE_CAPTURE=''
'install-scheduled': INSTALL_SCHEDULED_DATE_CAPTURE!='' AND INSTALL_COMPLETED_DATE=''
'install-completed': INSTALL_COMPLETED_DATE!='' AND PTO_APPROVED=''
'pending-cancel': PROJECT_STATUS contains 'Pending Cancel'
'cancelled': PROJECT_STATUS contains 'Cancel' AND NOT 'Pending'
'needs-attention': PROJECT_AGE>90 OR (ON_HOLD='Yes' AND daysOnHold>7)

Sort order varies by view:
- 'on-hold': Sort by DATE_ON_HOLD ASC (oldest first)
- 'install-scheduled': Sort by INSTALL_SCHEDULED_DATE_CAPTURE ASC
- 'install-completed': Sort by INSTALL_COMPLETED_DATE DESC
- default: Sort by PROJECT_AGE DESC

See CURSOR-INSTRUCTIONS.md for complete filter implementation
```

---

## 6. Integration Points

### Linking Both Approaches

**Dashboard** → **Projects List** → **Project Detail**

1. **Dashboard recent projects** link to detail page:
   ```tsx
   <Link href={`/projects/${recordId}`}>
   ```

2. **Projects List rows** link to detail page:
   ```tsx
   <Link href={`/projects/${recordId}`}>
   ```

3. **Detail page back button** returns to list:
   ```tsx
   <Button onClick={() => router.back()}>Back to Projects</Button>
   ```

### Data Consistency

Both views use the same:
- **Field IDs** from `lib/constants/fieldIds.ts`
- **Project type** from `lib/types/project.ts`
- **Quickbase client** from `lib/quickbase/client.ts`
- **TanStack Query** for caching

---

## 7. Key Differences: 7 vs 9 Milestones

### List View (7 Milestones - Build This)
Horizontal compact view for scanning:
```
📋 Intake → 📸 Survey → 📐 Design → 🔌 NEM → 📋 Permit → 🔧 Install → ✅ PTO
```

### Detail View (9 Milestones - Already Built by Cursor)
Vertical detailed view with substeps:
```
1. Intake
2. Survey
3. Design
4. HOA (conditional)
5. Permit
6. NEM
7. Install
8. Inspection
9. PTO
```

**Why Different?**
- **List view**: Fast scanning, space-constrained
- **Detail view**: Deep analysis, space for substeps

---

## 8. Testing Strategy

### Dashboard Tests
```typescript
// tests/pages/dashboard.test.ts
- Metrics display correct counts
- Urgent alerts show/hide based on data
- Recent projects link correctly
- Welcome message shows user name
```

### Projects List Tests
```typescript
// tests/pages/projects-list.test.ts
- All 9 filters work
- Search filters correctly
- Traffic lights show correct states
- Rows link to detail page
- PPW delta colors are correct
```

### Integration Tests
```typescript
// tests/integration/navigation.test.ts
- Dashboard → Projects List
- Projects List → Project Detail
- Project Detail → Back to list
- All data loads correctly
```

---

## 9. Acceptance Criteria

### Dashboard Page ✅
- [ ] 4 metric cards display correct data
- [ ] Urgent alerts show projects on hold > 7 days
- [ ] Recent projects clickable
- [ ] Welcome header with user name and role

### Projects List Page ✅
- [ ] Horizontal traffic lights (7 milestones)
- [ ] All 7 lights show correct states
- [ ] In-progress lights pulse
- [ ] Status text shows milestone details
- [ ] Customer names split correctly
- [ ] Addresses on two lines
- [ ] Phone numbers are tel: links
- [ ] PPW shows both values with delta
- [ ] Delta colors correct (red/green/gray)
- [ ] Age indicators color-coded
- [ ] Hold banners appear with correct color
- [ ] All 9 filter chips work
- [ ] Search filters by name and ID
- [ ] Responsive on iPad Pro 11" and 13"

### Integration ✅
- [ ] Dashboard links to projects list
- [ ] Projects list links to detail
- [ ] Detail back button works
- [ ] Data consistent across views
- [ ] No TypeScript errors
- [ ] No console errors

---

## 10. Estimated Timeline

| Phase | Task | Time | Tool |
|-------|------|------|------|
| **4A** | Dashboard page (3 components + page) | 2-3 hours | Cursor/Traycer |
| **4B** | Projects list (7 components + queries) | 3-4 hours | Cursor/Traycer |
| **4C** | Testing & integration | 1-2 hours | Manual |
| **Total** | Complete dashboard | **6-9 hours** | - |

With AI assistance (Traycer), this could be **2-3 hours total**.

---

## 11. Next Steps

### Immediate Actions

1. **Choose your tool**: Cursor or Traycer?
   - Cursor: Use prompts above directly
   - Traycer: Load TRAYCER-INSTRUCTIONS.md context

2. **Start with Dashboard** (Phase 4A)
   - Easier, fewer dependencies
   - Build: DashboardMetrics → UrgentAlerts → RecentProjects → Page

3. **Then Projects List** (Phase 4B)
   - More complex (traffic lights, filters)
   - Build: Shared components → Traffic light logic → Row → Table → Page

4. **Test & Integrate** (Phase 4C)
   - Test navigation flows
   - Verify data consistency
   - Run test suite

### Questions to Answer

1. **Do you want to build from scratch or modify existing pages?**
   - The `/app/(dashboard)/page.tsx` currently exists (from Phase 1-2)
   - The `/projects` route doesn't exist yet

2. **Which tool should I use for implementation prompts?**
   - Cursor: Step-by-step manual prompts
   - Traycer: AI-optimized prompts with context loading

3. **Should we keep both traffic light approaches?**
   - Recommended: Yes (7 for list, 9 for detail)
   - Alternative: Standardize on one

---

## 12. Reference Documents

- **TRAYCER-UI-SPEC.md**: Complete UI specification with all components
- **TRAYCER-INSTRUCTIONS.md**: AI-optimized prompts for implementation
- **CURSOR-INSTRUCTIONS.md**: Manual step-by-step guide
- **README.md**: Project overview and current phase status

---

## Summary

**What's Done:** ✅ Detailed project view (9 milestones, hold management, testing)

**What's Needed:** 🔴 Dashboard page + Projects list page with horizontal traffic lights

**Recommended Approach:** Build missing pages from TRAYCER spec, keep Cursor's detail view

**Next Action:** Start Phase 4A (Dashboard) using prompts in Section 5

Ready to build! 🚀
