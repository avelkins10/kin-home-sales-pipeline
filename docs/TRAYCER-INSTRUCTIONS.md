# Traycer AI Implementation Guide - Kin Home Rep Dashboard

This guide provides AI-specific instructions for implementing the Kin Home Rep Dashboard using Traycer (or similar AI coding assistants like Cursor Composer, GitHub Copilot, etc.).

---

## Overview

Traycer is an AI coding assistant that can generate entire features from detailed specifications. This guide shows you how to use Traycer effectively to build the dashboard.

**Total Implementation Time**: 2-3 hours (with AI assistance)

---

## Setup

### 1. Load Context

Before starting, ensure Traycer has access to these files:

```
@docs/TRAYCER-UI-SPEC.md
@lib/constants/fieldIds.ts
@lib/types/project.ts
@lib/quickbase/client.ts
@components/ui/* (all shadcn components)
```

### 2. Set Project Context

Tell Traycer about your project:

```
This is a Next.js 14 dashboard for solar installation sales reps. We use:
- TypeScript
- Tailwind CSS
- shadcn/ui components
- TanStack Query for data fetching
- Quickbase as our backend

We're building 3 pages: Dashboard, Projects List, and Project Detail.
All specifications are in @docs/TRAYCER-UI-SPEC.md
```

---

## Implementation Strategy

### Phase 1: Utilities First (15 minutes)

Build all utility functions first since they have no UI dependencies.

#### Prompt 1.1: Hold Detection

```
Create lib/utils/hold-detection.ts based on section 6.2 of @docs/TRAYCER-UI-SPEC.md

Requirements:
- detectHoldStatus(status) function that checks if PROJECT_STATUS contains hold keywords
- extractHoldType(status) function that returns hold type (finance, roof, customer, etc.)
- Hold keywords: "On Hold", "Finance Hold", "Roof Hold", "Customer Hold", "Permit Hold", "HOA Hold"
- Return types should be properly typed
```

**Verify**: Check that both functions are exported and typed correctly.

---

#### Prompt 1.2: Project Helpers

```
Create lib/utils/project-helpers.ts based on section 6.3 of @docs/TRAYCER-UI-SPEC.md

Requirements:
- parseCustomerName(fullName): Split into firstName and lastName
  - Handle edge cases: single word, empty string
  - Last word is always lastName, everything else is firstName
- formatAddress(address): Split into line1 (street) and line2 (city, state, zip)
  - Split on commas
  - First part is line1, rest joined with commas is line2
- getProjectAge(project): Calculate days since sale
  - Use PROJECT_AGE field if available, otherwise calculate from SALES_DATE
- getInstallDate(project): Get install dates with backup field hierarchy
  - scheduled: Use fields 710, 178, 1124, 835 in order (first non-empty)
  - started: Use field 464
  - completed: Use fields 534, 587, 2006 in order
- getInstallRescheduleHistory(project): Return array of all install date changes
  - Include date and source label
  - Sort by date descending (most recent first)

Use @lib/constants/fieldIds.ts for field references
Use @lib/types/project.ts for QuickbaseProject type
```

**Verify**: Test each function with sample data.

---

#### Prompt 1.3: Traffic Light Logic

```
Create lib/utils/traffic-lights.ts based on section 6.1 of @docs/TRAYCER-UI-SPEC.md

This is the MOST COMPLEX utility - implement carefully.

Requirements:
- Export types: MilestoneId, MilestoneState
- calculateMilestoneState(project, milestoneId): Returns state for each milestone
  - States: 'complete', 'in-progress', 'pending', 'on-hold', 'overdue'
  - 7 milestones: intake, survey, design, nem, permit, install, inspection
  - Each milestone has specific completion criteria (see spec section 6.1)
  - If project is on hold, current milestone shows 'on-hold' state
- getMilestoneStatusText(project): Returns status text like "NEM: Submitted 12/28 • 5d waiting"
  - Format depends on current milestone
  - Show submission dates and days waiting for NEM and Permit
  - Show scheduled/started dates for Install

Helper functions needed:
- calculateIntakeState(project)
- calculateSurveyState(project)
- calculateDesignState(project)
- calculateNEMState(project)
- calculatePermitState(project)
- calculateInstallState(project)
- calculateInspectionState(project)
- getCurrentMilestoneId(project)
- calculateDaysWaiting(submitDate)
- formatDate(dateStr) - format as MM/DD

Use @lib/constants/fieldIds.ts for all field references
Use @lib/utils/hold-detection.ts for detectHoldStatus
```

**Verify**: Test with projects at different stages. Ensure states are correct.

---

### Phase 2: Shared Components (30 minutes)

Build reusable UI components that multiple pages will use.

#### Prompt 2.1: PPW Display

```
Create components/projects/PPWDisplay.tsx based on section 5.2 of @docs/TRAYCER-UI-SPEC.md

Requirements:
- Props: soldPPW (number|null), commissionablePPW (number|null), expanded (boolean, default false)
- Shows both Sold PPW and Commissionable PPW
- Calculate delta: commissionablePPW - soldPPW
- Delta color: green if positive, red if negative, gray if zero
- Delta icon: ArrowUp if positive, ArrowDown if negative, Minus if zero
- Compact mode (default): Shows both values inline with delta
- Expanded mode: Shows values in rows with labels
- Handle null values gracefully (show "N/A")

Use lucide-react icons: ArrowDown, ArrowUp, Minus
```

**Verify**: Test with different PPW combinations (positive delta, negative delta, equal, null values).

---

#### Prompt 2.2: Project Age Indicator

```
Create components/projects/ProjectAgeIndicator.tsx based on section 5.4 of @docs/TRAYCER-UI-SPEC.md

Requirements:
- Props: age (number in days)
- Color coding:
  - < 90 days: gray text
  - 90-120 days: amber text with "WARNING" label
  - > 120 days: red text with "CRITICAL" label
- Show calendar icon from lucide-react
- Display age as "{age}d"
- Labels appear below age number

Use Tailwind classes for colors
Use cn() utility from @/lib/utils for conditional classes
```

**Verify**: Test with ages: 45, 95, 130 to see all color states.

---

#### Prompt 2.3: Hold Banner

```
Create components/projects/HoldBanner.tsx based on section 5.3 of @docs/TRAYCER-UI-SPEC.md

Requirements:
- Props: project (QuickbaseProject), holdType (union of hold types)
- Get hold reason from HOLD_REASON or BLOCK_REASON fields
- Calculate days on hold from DATE_ON_HOLD field
- Color coding by hold type:
  - finance: red background
  - roof: orange background
  - customer: amber background
  - permit: yellow background
  - hoa: lime background
  - generic: gray background
- Show AlertTriangle icon, hold type label, reason, and days on hold
- Banner has rounded-top corners and left border (4px)

Use @lib/constants/fieldIds.ts for field references
Icons: AlertTriangle, Clock from lucide-react
```

**Verify**: Test with different hold types to see color variations.

---

#### Prompt 2.4: Traffic Light Pipeline

```
Create components/projects/TrafficLightPipeline.tsx based on section 5.1 of @docs/TRAYCER-UI-SPEC.md

This is the CENTERPIECE component of the dashboard.

Requirements:
- Props: project (QuickbaseProject)
- 7 traffic lights in horizontal row: Intake, Survey, Design, NEM, Permit, Install, Inspection (PTO)
- Each light is a circular icon (32px) with emoji
- Colors by state:
  - complete: green-500 background
  - in-progress: amber-500 background with PULSE animation
  - pending: gray-200 background
  - on-hold: red-500 background
  - overdue: red-600 background with ring
- Connector lines between lights (4px wide, 16px long)
  - Green if previous light is complete, gray otherwise
- Status text below lights showing current milestone details
- Use calculateMilestoneState() from @lib/utils/traffic-lights.ts
- Use getMilestoneStatusText() from @lib/utils/traffic-lights.ts

Milestone emojis:
- Intake: 📋
- Survey: 📸
- Design: 📐
- NEM: 🔌
- Permit: 📋
- Install: 🔧
- Inspection: ✅

Add Tailwind animation for pulse:
- animate-pulse class on in-progress lights
```

**Verify**: Test with projects at different stages. Ensure pulse animation works on in-progress.

---

### Phase 3: Dashboard Page (30 minutes)

Build the homepage dashboard with metrics and alerts.

#### Prompt 3.1: Dashboard Metrics

```
Create components/dashboard/DashboardMetrics.tsx based on section 2.2 of @docs/TRAYCER-UI-SPEC.md

Requirements:
- Props: userId (string), role (string)
- Fetch metrics using getDashboardMetrics() query (you'll need to create this)
- Display 4 metric cards in grid layout (responsive: 1 col mobile, 2 cols tablet, 4 cols desktop)
- Metrics:
  1. Installs This Week (Calendar icon, blue)
  2. Active Accounts (TrendingUp icon, green) - subtitle: "Not on hold"
  3. On Hold (Pause icon, red) - subtitle shows breakdown like "3 Finance, 2 Roof"
  4. Monthly Installs (CheckCircle icon, purple)
- Each card shows: label, value (large), subtitle (if applicable), colored icon
- Use Card, CardContent from @components/ui/card
- Use TanStack Query with useQuery hook

Icons from lucide-react: Calendar, TrendingUp, Pause, CheckCircle
```

**Note**: You'll need to implement `getDashboardMetrics()` in `lib/quickbase/queries.ts` first. Here's the signature:

```typescript
export async function getDashboardMetrics(userId: string, role: string): Promise<{
  installsThisWeek: number
  activeProjects: number
  onHold: number
  holdBreakdown: string // "3 Finance, 2 Roof, 1 Customer"
  installsThisMonth: number
}>
```

**Verify**: Metrics display correctly and update when data changes.

---

#### Prompt 3.2: Urgent Alerts

```
Create components/dashboard/UrgentAlerts.tsx based on section 2.3 of @docs/TRAYCER-UI-SPEC.md

Requirements:
- Props: userId (string), role (string)
- Fetch urgent projects using getUrgentProjects() query
- Show red alert banner if projects exist on hold > 7 days
- Display first 3 urgent projects with:
  - Customer name
  - Days on hold (with Clock icon)
  - Hold reason
- "View all X on hold" link if more than 3
- Hide entire component if no urgent projects
- Each project clickable, links to /projects/{recordId}

Use @components/ui/card or plain div with red-50 background
Icons: AlertTriangle, Clock from lucide-react
```

**Note**: Implement `getUrgentProjects()` in queries.ts:

```typescript
export async function getUrgentProjects(userId: string, role: string): Promise<Array<{
  recordId: string
  customerName: string
  daysOnHold: number
  holdReason: string
}>>
```

---

#### Prompt 3.3: Recent Projects

```
Create components/dashboard/RecentProjects.tsx based on section 2.4 of @docs/TRAYCER-UI-SPEC.md

Requirements:
- Props: userId (string), role (string)
- Fetch recent projects using getRecentProjects() query
- Show 5 most recent active projects
- Each project shows:
  - Customer name
  - Project ID (smaller, gray text)
  - Current milestone badge
  - Arrow icon
- "View all" link to /projects page
- Each project clickable, links to /projects/{recordId}

Use getCurrentMilestone() from @lib/utils/milestones.ts if available, or from traffic-lights.ts
Use Badge component from @components/ui/badge for milestone
Icon: ArrowRight from lucide-react
```

---

#### Prompt 3.4: Dashboard Page

```
Update app/(dashboard)/page.tsx based on section 2.1 of @docs/TRAYCER-UI-SPEC.md

Requirements:
- Server component (async function)
- Get session with getServerSession
- Redirect to /login if no session
- Layout:
  - Page header: "Welcome back, {name}" and role subtitle
  - UrgentAlerts component (in Suspense)
  - DashboardMetrics component (in Suspense)
  - RecentProjects component (in Suspense)
- Pass userId and role to all components
- Implement getRoleDisplayName() helper function

Skeleton components for loading states:
- AlertsSkeleton: Simple gray rectangle
- MetricsSkeleton: Grid of 4 skeleton cards
- RecentProjectsSkeleton: 3 skeleton rows
```

**Verify**: Visit `/` route and see complete dashboard with metrics, alerts, and recent projects.

---

### Phase 4: Projects List Page (45 minutes)

Build the main projects list with traffic lights and filters.

#### Prompt 4.1: Search Bar

```
Create components/projects/SearchBar.tsx

Requirements:
- Search input with Search icon (lucide-react)
- Debounced search (300ms delay) - use useDebouncedCallback from 'use-debounce'
- Updates URL query param ?search=...
- Default value from current URL search param
- Placeholder: "Search by name or project ID..."
- Max width: 448px (max-w-md)

You may need to install: npm install use-debounce
```

---

#### Prompt 4.2: Filter Chips

```
Create components/projects/ProjectFilterChips.tsx based on section 3.2 of @docs/TRAYCER-UI-SPEC.md

Requirements:
- 9 filter chips: All Projects, Active, On Hold, Install Ready, Install Scheduled, Install Completed, Pending Cancel, Cancelled, Needs Attention
- Horizontal scrollable row
- Active chip: blue background (bg-blue-600), white text
- Inactive chips: gray background (bg-gray-100), gray text, hover gray-200
- Clicking chip updates URL query param ?view=...
- "all" view removes the param
- Round pill shape (rounded-full)
- Smooth transitions

Use useRouter and useSearchParams from next/navigation
```

---

#### Prompt 4.3: Project Row

```
Create components/projects/ProjectRow.tsx based on section 3.4 of @docs/TRAYCER-UI-SPEC.md

This is a COMPLEX component - the main list item.

Requirements:
- Props: project (QuickbaseProject)
- 4-column grid layout: [220px customer | flexible traffic lights | 160px metrics | 80px age]
- Responsive: smaller columns on iPad
- Hold banner above row (rounded top) if on hold
- Main row rounded bottom if hold, full rounded if not on hold
- Clickable entire row → /projects/{recordId}
- Hover shadow effect

Column 1 - Customer Info:
- Full name (firstName lastName) using parseCustomerName()
- Address line 1
- Address line 2 (if exists)
- Phone number as tel: link (stops click propagation)

Column 2 - Traffic Light Pipeline:
- TrafficLightPipeline component
- Flexible width, fills available space

Column 3 - Metrics (3 rows):
- System Size: label + value
- PPW: label + PPWDisplay component
- Value: label + formatted currency

Column 4 - Age:
- ProjectAgeIndicator component
- Right aligned

Use components:
- @components/projects/TrafficLightPipeline
- @components/projects/PPWDisplay
- @components/projects/HoldBanner
- @components/projects/ProjectAgeIndicator

Use utilities:
- @lib/utils/project-helpers: parseCustomerName, formatAddress, getProjectAge
- @lib/utils/hold-detection: detectHoldStatus, extractHoldType
- @lib/utils/formatters: formatSystemSize, formatCurrency
```

**Verify**: View project rows in different states (active, on hold, different ages).

---

#### Prompt 4.4: Project Table View

```
Create components/projects/ProjectTableView.tsx based on section 3.3 of @docs/TRAYCER-UI-SPEC.md

Requirements:
- Props: userId, role, view, search (optional)
- Fetch projects using getProjectsForUser(userId, role, view, search)
- Use TanStack Query with useQuery
- Refetch every 30 seconds (refetchInterval: 30000)
- Map projects to ProjectRow components
- Show loading skeleton while loading
- Show empty state if no projects
- Vertical stack with gap-3

Empty state:
- Centered text: "No projects found"
- White background, rounded, border
```

**Note**: You need to implement `getProjectsForUser()` in queries.ts with filter logic. See CURSOR-INSTRUCTIONS.md for details.

---

#### Prompt 4.5: Projects Page

```
Update app/(dashboard)/projects/page.tsx based on section 3.1 of @docs/TRAYCER-UI-SPEC.md

Requirements:
- Server component with searchParams prop
- Get session, redirect if not authenticated
- Layout:
  - ProjectListHeader component (create simple header with title and subtitle)
  - Row with SearchBar and ProjectFilterChips
  - ProjectTableView in Suspense
- Pass searchParams.view and searchParams.search to ProjectTableView

Create ProjectListHeader component:
- H1: "Projects"
- Subtitle: "Manage and track all your solar installation projects"
```

**Verify**: Visit `/projects` and test:
- All filters work
- Search works
- Traffic lights display correctly
- Can click into projects

---

### Phase 5: Project Detail Page (45 minutes)

Build the detailed project view with tabs.

#### Prompt 5.1: Detail Header

```
Create components/projects/detail/ProjectDetailHeader.tsx based on section 4.2 of @docs/TRAYCER-UI-SPEC.md

Requirements:
- Props: project (QuickbaseProject)
- Back button: "← Back to Projects" → /projects
- Hold banner if on hold (HoldBanner component)
- Hero card with:
  - Large customer name (firstName lastName)
  - Project ID (smaller, monospace)
  - Contact info row: Address, Phone (tel: link), Email (mailto: link)
  - Edit button (top right) - doesn't need functionality yet
- Icons: ArrowLeft, Phone, Mail, MapPin, Edit from lucide-react
- White background, rounded, border
```

---

#### Prompt 5.2: Overview Tab

```
Create components/projects/detail/tabs/OverviewTab.tsx based on section 4.4 of @docs/TRAYCER-UI-SPEC.md

Requirements:
- Props: project (QuickbaseProject)
- 2-column layout (2/3 left, 1/3 right)
- Left column:
  - Project Pipeline card (use ExpandedTrafficLightPipeline - create this)
  - Recent Activity card (use ActivityFeed - create placeholder)
- Right column:
  - Quick Stats card (use QuickStats - create this)
  - Key Contacts card (use KeyContacts - create this)

Create supporting components:

ExpandedTrafficLightPipeline:
- Similar to TrafficLightPipeline but vertical list
- Each milestone shows: icon, title, state, dates
- Show all 7 milestones with completion status

QuickStats:
- Card showing: System Size, Sold PPW, Your PPW (with delta), Total Value, Project Age
- Use PPWDisplay in expanded mode

KeyContacts:
- Card showing: Homeowner, Closer, Setter, Install Crew
- Get from Quickbase fields if available

ActivityFeed:
- Placeholder for now: "Activity feed coming soon"
- TODO: Implement once Quickbase communication schema determined
```

---

#### Prompt 5.3: Timeline Tab

```
Create components/projects/detail/tabs/TimelineTab.tsx based on section 4.5 of @docs/TRAYCER-UI-SPEC.md

Requirements:
- Props: project (QuickbaseProject)
- Vertical timeline showing all 7 milestones
- Each milestone shows:
  - Icon: CheckCircle (complete), Clock (in-progress), Circle (pending)
  - Milestone title
  - Sub-fields with dates (or "Pending")
  - Vertical line connecting milestones
- Special section for Install: Show reschedule history
  - Use getInstallRescheduleHistory() from @lib/utils/project-helpers
  - Display in small gray box below install milestone
  - Show source and date for each reschedule

Icons: CheckCircle, Clock, Circle from lucide-react
```

---

#### Prompt 5.4: System, Financials, Documents, Activity Tabs

```
Create these tabs based on TRAYCER-UI-SPEC.md sections 4.6-4.9:

SystemTab.tsx (section 4.6):
- Card with grid of system fields
- Fields: System Size, Panel Count, Panel Type, Inverter Type, Battery, Roof Type
- 2-column responsive grid
- Each field: label (small gray) + value (medium bold)

FinancialsTab.tsx (section 4.7):
- 2-column grid with 2 cards
- Pricing card: System Price (large), PPW (expanded mode)
- Financing card: Financing Type
- Use PPWDisplay component in expanded mode

DocumentsTab.tsx (section 4.8):
- Placeholder for now
- Show sample documents list with FileText icons
- Download buttons (non-functional)
- TODO: Integrate with Quickbase attachments later

ActivityTab.tsx (section 4.9):
- Placeholder using ActivityFeed component
- Card wrapper around ActivityFeed
```

---

#### Prompt 5.5: Detail Tabs Component

```
Create components/projects/detail/ProjectDetailTabs.tsx based on section 4.3 of @docs/TRAYCER-UI-SPEC.md

Requirements:
- Props: project (QuickbaseProject)
- Use Tabs component from @components/ui/tabs
- 6 tabs: Overview, Timeline, System, Financials, Documents, Activity
- TabsList with 6 TabsTriggers
- TabsContent for each tab with corresponding tab component
- Default to "overview" tab
- Pass project prop to all tab components
```

---

#### Prompt 5.6: Detail Page

```
Create app/(dashboard)/projects/[id]/page.tsx based on section 4.1 of @docs/TRAYCER-UI-SPEC.md

Requirements:
- Server component with params.id
- Get session, redirect if not authenticated
- Fetch project using getProjectById(params.id)
- Show 404 if project not found (use notFound() from next/navigation)
- Layout:
  - ProjectDetailHeader component
  - ProjectDetailTabs component
- Space between sections (space-y-6)

Implement getProjectById() in queries.ts if not exists:
- Fetch single record by RECORD_ID field
- Return QuickbaseProject or null
```

**Verify**: Click into project from list and test:
- Header displays correctly
- All 6 tabs render and switch
- Data displays in each tab
- Back button returns to list

---

## Query Implementation

You'll need to implement several query functions in `lib/quickbase/queries.ts`. Here's a guide:

### getDashboardMetrics

```typescript
export async function getDashboardMetrics(userId: string, role: string) {
  // Query 1: Count installs this week (INSTALL_COMPLETED_DATE in current week)
  // Query 2: Count active projects (NOT on hold, NOT completed, NOT cancelled)
  // Query 3: Count on hold projects + breakdown by type
  // Query 4: Count installs this month

  // Return object with all metrics
}
```

### getUrgentProjects

```typescript
export async function getUrgentProjects(userId: string, role: string) {
  // Query projects where:
  // - PROJECT_STATUS contains 'Hold'
  // - DATE_ON_HOLD > 7 days ago
  // Sort by DATE_ON_HOLD ASC (oldest first)
  // Return: recordId, customerName, daysOnHold, holdReason
}
```

### getProjectsForUser

```typescript
export async function getProjectsForUser(
  userId: string,
  role: string,
  view: string,
  search?: string
) {
  // Build where clause based on:
  // - Role-based filtering (closer, office_leader, regional, etc.)
  // - View filtering (active, on-hold, install-ready, etc.)
  // - Search filtering (by customer name or project ID)

  // See CURSOR-INSTRUCTIONS.md for detailed filter logic
}
```

---

## Testing Strategy

After implementing each phase, test thoroughly:

### Phase 1 Tests (Utilities)
```
Test parseCustomerName with:
- "John Smith" → {firstName: "John", lastName: "Smith"}
- "John David Martinez" → {firstName: "John David", lastName: "Martinez"}
- "Madonna" → {firstName: "Madonna", lastName: ""}

Test traffic light states with projects at different milestones
```

### Phase 2 Tests (Components)
```
Test PPWDisplay with:
- soldPPW: 3.20, commissionablePPW: 2.80 → Shows red delta
- soldPPW: 3.00, commissionablePPW: 3.20 → Shows green delta
- soldPPW: 3.00, commissionablePPW: 3.00 → Shows gray neutral
```

### Phase 3 Tests (Dashboard)
```
Visit / and verify:
- Metrics display correct counts
- Urgent alerts show when projects on hold > 7 days
- Recent projects link to detail pages
```

### Phase 4 Tests (Projects List)
```
Visit /projects and verify:
- All filters work and update URL
- Search filters correctly
- Traffic lights show correct states
- Can click into projects
```

### Phase 5 Tests (Project Detail)
```
Click into project and verify:
- All tabs render
- Timeline shows all dates
- Reschedule history displays
- Back button works
```

---

## Common Prompts

### "Fix TypeScript errors"
```
Fix all TypeScript errors in this file. Ensure:
- All imports are correct
- All props are properly typed
- All functions return correct types
- No 'any' types used
```

### "Make it responsive"
```
Make this component responsive for iPad Pro 11" and 13" in landscape mode.
Use Tailwind breakpoints:
- sm: 640px
- md: 768px
- lg: 1024px
- xl: 1280px
```

### "Add loading states"
```
Add proper loading states with skeleton loaders.
Use Skeleton component from @components/ui/skeleton
Match the layout of the actual content
```

---

## Optimization Tips

1. **Use Traycer's context feature**: Always reference the spec with `@docs/TRAYCER-UI-SPEC.md`
2. **Build incrementally**: Test each component before moving to the next
3. **Use specific prompts**: Don't ask "build the dashboard" - ask for one component at a time
4. **Verify types**: Always check that TypeScript is happy
5. **Test interactivity**: Click everything to ensure links and buttons work

---

## Troubleshooting

### Traycer generates wrong code
- **Solution**: Be more specific in your prompt. Reference exact section numbers from spec.

### TypeScript errors
- **Solution**: Ask Traycer to "Fix all TypeScript errors" and provide the error messages

### Components not rendering
- **Solution**: Check imports. Ask Traycer to "verify all imports are correct"

### Styling looks wrong
- **Solution**: Reference specific Tailwind classes from spec. Ask to "match the mockup exactly"

---

## Final Checklist

After completing all phases:

- [ ] All utilities implemented and tested
- [ ] All shared components render correctly
- [ ] Dashboard page displays with metrics, alerts, and recent projects
- [ ] Projects list page shows traffic lights and filters work
- [ ] Project detail page shows all 6 tabs
- [ ] No TypeScript errors
- [ ] No console errors in browser
- [ ] Responsive on iPad Pro 11" and 13"
- [ ] All links navigate correctly
- [ ] Search and filters update URL and results

---

**Estimated Total Time with AI**: 2-3 hours

Good luck! Let the AI do the heavy lifting. 🤖✨
