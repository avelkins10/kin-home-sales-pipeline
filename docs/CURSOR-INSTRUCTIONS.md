# Cursor Implementation Guide - Kin Home Rep Dashboard

This guide provides step-by-step instructions for implementing the Kin Home Rep Dashboard using Cursor IDE.

---

## Prerequisites

1. **Review the main specification**: Read `/docs/TRAYCER-UI-SPEC.md` thoroughly
2. **Ensure development environment is ready**:
   - Node.js 18+ installed
   - All dependencies installed (`npm install`)
   - Quickbase credentials configured in `.env.local`
   - Development server can run (`npm run dev`)

---

## Implementation Phases

### Phase 1: Utility Functions (30 minutes)

These have no UI dependencies and should be built first.

#### Step 1.1: Hold Detection Utilities

Create `lib/utils/hold-detection.ts`:

```typescript
export function detectHoldStatus(status: string): boolean {
  const holdStatuses = [
    'On Hold',
    'Finance Hold',
    'Roof Hold',
    'Customer Hold',
    'Permit Hold',
    'HOA Hold'
  ]
  return holdStatuses.some((hold) => status.includes(hold))
}

export function extractHoldType(
  status: string
): 'finance' | 'roof' | 'customer' | 'permit' | 'hoa' | 'generic' {
  if (status.includes('Finance Hold')) return 'finance'
  if (status.includes('Roof Hold')) return 'roof'
  if (status.includes('Customer Hold')) return 'customer'
  if (status.includes('Permit Hold')) return 'permit'
  if (status.includes('HOA Hold')) return 'hoa'
  return 'generic'
}
```

**Test**: Create a simple test file or use Cursor's inline testing to verify hold detection works.

#### Step 1.2: Project Helper Functions

Create `lib/utils/project-helpers.ts`:

Copy the full implementation from TRAYCER-UI-SPEC.md section 6.3.

**Key functions**:
- `parseCustomerName(fullName)` - Split first/last name
- `formatAddress(address)` - Split into line1/line2
- `getProjectAge(project)` - Calculate days since sale
- `getInstallDate(project)` - Get install dates with fallbacks
- `getInstallRescheduleHistory(project)` - Get reschedule history

**Test**:
```typescript
// Test in Cursor console
parseCustomerName("John David Martinez")
// Should return: { firstName: "John David", lastName: "Martinez" }

formatAddress("123 Main St, Austin, TX 78701")
// Should return: { line1: "123 Main St", line2: "Austin, TX 78701" }
```

#### Step 1.3: Traffic Light State Logic

Create `lib/utils/traffic-lights.ts`:

Copy the full implementation from TRAYCER-UI-SPEC.md section 6.1.

**Key functions**:
- `calculateMilestoneState(project, milestoneId)` - Returns state for each milestone
- `getMilestoneStatusText(project)` - Returns status text below lights

**Test**: Use a sample project record to verify states are calculated correctly.

#### Step 1.4: Update Quickbase Queries

Update `lib/quickbase/queries.ts` to add new query functions:

```typescript
// Dashboard metrics
export async function getDashboardMetrics(userId: string, role: string) {
  // Query for:
  // - installsThisWeek: Count of installs with INSTALL_COMPLETED_DATE this week
  // - activeProjects: Count where NOT on hold, NOT completed, NOT cancelled
  // - onHold: Count where status contains 'Hold'
  // - holdBreakdown: String like "3 Finance, 2 Roof, 1 Customer"
  // - installsThisMonth: Count of installs this month
}

// Urgent projects (on hold > 7 days)
export async function getUrgentProjects(userId: string, role: string) {
  // Query for projects where:
  // - detectHoldStatus(status) = true
  // - daysOnHold > 7
  // - Sort by daysOnHold DESC
}

// Recent projects (5 most recent active)
export async function getRecentProjects(userId: string, role: string) {
  // Query for recent projects, limit 5
}

// Projects list with filtering
export async function getProjectsForUser(
  userId: string,
  role: string,
  view: string = 'all',
  search?: string
) {
  // Implement filter logic from TRAYCER-UI-SPEC.md section 3.3
  // Support views: all, active, on-hold, install-ready, etc.
}

// Single project by ID
export async function getProjectById(recordId: string) {
  // Fetch single record by RECORD_ID
}
```

**Test**: Run each query in isolation to verify results.

---

### Phase 2: Shared UI Components (1 hour)

Build reusable components that other pages will use.

#### Step 2.1: PPWDisplay Component

Create `components/projects/PPWDisplay.tsx`:

Copy implementation from TRAYCER-UI-SPEC.md section 5.2.

**Features**:
- Shows both Sold PPW and Commissionable PPW
- Delta indicator with up/down arrow
- Color coding (red for negative, green for positive)
- Compact and expanded modes

**Test**:
```tsx
<PPWDisplay soldPPW={3.20} commissionablePPW={2.80} />
// Should show: "Sold: $3.20 | Yours: $2.80 ↓ -$0.40" with red arrow
```

#### Step 2.2: ProjectAgeIndicator Component

Create `components/projects/ProjectAgeIndicator.tsx`:

Copy implementation from TRAYCER-UI-SPEC.md section 5.4.

**Features**:
- Shows project age in days
- Color coding: gray (<90d), amber (90-120d), red (>120d)
- WARNING/CRITICAL labels

**Test**:
```tsx
<ProjectAgeIndicator age={95} />
// Should show amber color with "WARNING"
```

#### Step 2.3: HoldBanner Component

Create `components/projects/HoldBanner.tsx`:

Copy implementation from TRAYCER-UI-SPEC.md section 5.3.

**Features**:
- Colored banner by hold type
- Shows hold reason and days on hold
- Appears above project row

**Test**:
```tsx
<HoldBanner project={sampleProject} holdType="finance" />
// Should show red banner with "Finance Hold" label
```

#### Step 2.4: TrafficLightPipeline Component

Create `components/projects/TrafficLightPipeline.tsx`:

Copy implementation from TRAYCER-UI-SPEC.md section 5.1.

**Features**:
- 7 traffic lights (Intake → Survey → Design → NEM → Permit → Install → Inspection)
- Color coding by state (green/amber/gray/red)
- Pulse animation for in-progress
- Status text below lights
- Connector lines between lights

**Test**: View with projects at different stages to verify all states render correctly.

---

### Phase 3: Dashboard Page (1 hour)

#### Step 3.1: DashboardMetrics Component

Create `components/dashboard/DashboardMetrics.tsx`:

Copy implementation from TRAYCER-UI-SPEC.md section 2.2.

**Features**:
- 4 metric cards in grid layout
- Icons and colors for each metric
- Subtitle for On Hold card showing breakdown

**Test**: Verify all 4 metrics display correctly and update on data change.

#### Step 3.2: UrgentAlerts Component

Create `components/dashboard/UrgentAlerts.tsx`:

Copy implementation from TRAYCER-UI-SPEC.md section 2.3.

**Features**:
- Shows projects on hold > 7 days
- Red alert banner
- Clickable project links
- Shows first 3, link to view all

**Test**: Verify alert shows when urgent projects exist, hidden when none.

#### Step 3.3: RecentProjects Component

Create `components/dashboard/RecentProjects.tsx`:

Copy implementation from TRAYCER-UI-SPEC.md section 2.4.

**Features**:
- Shows 5 most recent projects
- Current milestone badge
- Clickable links to project detail

**Test**: Verify projects display and links work.

#### Step 3.4: Update Dashboard Page

Update `app/(dashboard)/page.tsx`:

Copy implementation from TRAYCER-UI-SPEC.md section 2.1.

**Test**: Visit `/` route and verify:
- Welcome header with user name
- Urgent alerts (if any urgent projects)
- 4 metrics cards
- Recent projects widget

---

### Phase 4: Projects List Page (2 hours)

#### Step 4.1: SearchBar Component

Create `components/projects/SearchBar.tsx`:

```typescript
'use client'

import { useRouter, useSearchParams } from 'next/navigation'
import { Search } from 'lucide-react'
import { Input } from '@/components/ui/input'
import { useDebouncedCallback } from 'use-debounce'

export function SearchBar() {
  const router = useRouter()
  const searchParams = useSearchParams()
  const currentSearch = searchParams.get('search') || ''

  const handleSearch = useDebouncedCallback((value: string) => {
    const params = new URLSearchParams(searchParams)
    if (value) {
      params.set('search', value)
    } else {
      params.delete('search')
    }
    router.push(`/projects?${params.toString()}`)
  }, 300)

  return (
    <div className="relative w-full max-w-md">
      <Search className="absolute left-3 top-1/2 -translate-y-1/2 h-4 w-4 text-gray-400" />
      <Input
        type="search"
        placeholder="Search by name or project ID..."
        defaultValue={currentSearch}
        onChange={(e) => handleSearch(e.target.value)}
        className="pl-10"
      />
    </div>
  )
}
```

Install debounce hook if needed: `npm install use-debounce`

**Test**: Type in search bar and verify URL updates with debounce.

#### Step 4.2: ProjectFilterChips Component

Create `components/projects/ProjectFilterChips.tsx`:

Copy implementation from TRAYCER-UI-SPEC.md section 3.2.

**Features**:
- 9 filter chips (All, Active, On Hold, etc.)
- Active chip highlighted in blue
- Updates URL params on click

**Test**: Click each filter and verify URL updates and projects filter correctly.

#### Step 4.3: ProjectRow Component

Create `components/projects/ProjectRow.tsx`:

Copy implementation from TRAYCER-UI-SPEC.md section 3.4.

**Features**:
- 4-column grid layout (customer info, traffic lights, metrics, age)
- Hold banner above row when on hold
- Clickable row linking to detail page
- Phone number tel: link

**Test**: View with various projects to verify all data displays correctly.

#### Step 4.4: ProjectTableView Component

Create `components/projects/ProjectTableView.tsx`:

Copy implementation from TRAYCER-UI-SPEC.md section 3.3.

**Features**:
- Fetches projects based on view and search
- Maps projects to ProjectRow components
- Loading skeleton
- Empty state

**Test**: Change filters and search to verify data updates.

#### Step 4.5: ProjectListHeader Component

Create `components/projects/ProjectListHeader.tsx`:

```typescript
export function ProjectListHeader() {
  return (
    <div>
      <h1 className="text-3xl font-bold text-gray-900">Projects</h1>
      <p className="text-gray-600 mt-1">
        Manage and track all your solar installation projects
      </p>
    </div>
  )
}
```

#### Step 4.6: Update Projects Page

Update `app/(dashboard)/projects/page.tsx`:

Copy implementation from TRAYCER-UI-SPEC.md section 3.1.

**Test**: Visit `/projects` and verify:
- Page header
- Search bar
- Filter chips
- Project list with traffic lights
- All filters work
- Search works

---

### Phase 5: Project Detail Page (2 hours)

#### Step 5.1: ProjectDetailHeader Component

Create `components/projects/detail/ProjectDetailHeader.tsx`:

Copy implementation from TRAYCER-UI-SPEC.md section 4.2.

**Features**:
- Back to projects link
- Customer name (large header)
- Project ID
- Contact info (address, phone, email)
- Hold banner if on hold
- Edit button

**Test**: Verify all customer info displays and links work.

#### Step 5.2: Tab Components

Create all tab components in `components/projects/detail/tabs/`:

**OverviewTab.tsx**: Copy from TRAYCER-UI-SPEC.md section 4.4
**TimelineTab.tsx**: Copy from TRAYCER-UI-SPEC.md section 4.5
**SystemTab.tsx**: Copy from TRAYCER-UI-SPEC.md section 4.6
**FinancialsTab.tsx**: Copy from TRAYCER-UI-SPEC.md section 4.7
**DocumentsTab.tsx**: Copy from TRAYCER-UI-SPEC.md section 4.8
**ActivityTab.tsx**: Copy from TRAYCER-UI-SPEC.md section 4.9

#### Step 5.3: Supporting Components

Create these components used by the tabs:

**ExpandedTrafficLightPipeline.tsx**:
```typescript
// Similar to TrafficLightPipeline but with date labels
// Shows all 7 milestones in vertical list with completion dates
```

**QuickStats.tsx**:
```typescript
// Card showing: System Size, Sold PPW, Your PPW, Total Value, Project Age
```

**KeyContacts.tsx**:
```typescript
// Card showing: Homeowner, Closer, Setter, Install Crew
```

**ActivityFeed.tsx**:
```typescript
// Timeline of recent activity/status changes
// TODO: Implement once Quickbase communication schema is determined
```

#### Step 5.4: ProjectDetailTabs Component

Create `components/projects/detail/ProjectDetailTabs.tsx`:

Copy implementation from TRAYCER-UI-SPEC.md section 4.3.

#### Step 5.5: Create Project Detail Page

Create `app/(dashboard)/projects/[id]/page.tsx`:

Copy implementation from TRAYCER-UI-SPEC.md section 4.1.

**Test**: Click on a project from the list and verify:
- Hero header displays correctly
- All 6 tabs render
- Can switch between tabs
- Data displays in each tab

---

## Testing Checklist

### Dashboard Page
- [ ] Metrics cards display correct data
- [ ] Urgent alerts show/hide based on data
- [ ] Recent projects clickable and link to detail
- [ ] Page updates when data changes

### Projects List
- [ ] All 7 traffic lights render correctly
- [ ] Traffic light states match project data (complete/in-progress/pending)
- [ ] In-progress lights pulse
- [ ] Status text shows correct milestone info
- [ ] Customer names split into first/last correctly
- [ ] Addresses display on two lines
- [ ] Phone numbers are clickable tel: links
- [ ] PPW displays both values with delta
- [ ] Delta color is correct (red/green/gray)
- [ ] Project age color codes correctly
- [ ] Hold banners appear with correct color
- [ ] All 9 filter chips work
- [ ] Search filters by name and project ID
- [ ] Layout responsive on iPad Pro 11" and 13"

### Project Detail
- [ ] Back button returns to project list
- [ ] Customer info displays correctly
- [ ] Hold banner shows if on hold
- [ ] All 6 tabs switch correctly
- [ ] Overview tab shows pipeline and stats
- [ ] Timeline tab shows all milestone dates
- [ ] Install reschedule history displays
- [ ] System tab shows technical details
- [ ] Financials tab shows pricing
- [ ] Documents tab renders (even if empty)
- [ ] Activity tab renders

---

## Common Issues and Solutions

### Issue: Traffic lights not showing correct state

**Solution**: Check that `calculateMilestoneState()` function has access to all required field IDs. Verify field IDs in `lib/constants/fieldIds.ts` match Quickbase.

### Issue: Hold banner not appearing

**Solution**: Verify `PROJECT_STATUS` field contains exact text like "Finance Hold" not "finance hold". Check case sensitivity in `detectHoldStatus()`.

### Issue: PPW delta showing wrong color

**Solution**: Check that `SOLD_NET_PPW` and `COMMISSIONABLE_PPW` field IDs are correct. Log the values to verify they're numbers not strings.

### Issue: Search not working

**Solution**: Verify debounce is installed (`npm install use-debounce`). Check that search query is properly added to Quickbase where clause.

### Issue: Filters not returning projects

**Solution**: Check `buildWhereClause()` function in queries.ts. Log the generated where clause to verify syntax matches Quickbase API.

### Issue: Responsive layout broken on iPad

**Solution**: Verify Tailwind breakpoints are correct. Check that grid-cols classes are responsive: `grid-cols-[220px_1fr_160px_80px]` should have responsive variants.

---

## Performance Optimization

1. **Enable React Query caching**: Already configured in `queryClient` config
2. **Debounce search**: Implemented with `useDebouncedCallback`
3. **Lazy load tabs**: Consider using `React.lazy()` for tab components if needed
4. **Optimize images**: Use Next.js Image component for any images
5. **Minimize re-renders**: Use `React.memo()` for expensive components

---

## Next Steps After Implementation

1. **Add error boundaries**: Wrap pages in error boundaries to catch errors gracefully
2. **Add loading states**: Implement skeleton loaders for all async content
3. **Add offline support**: Consider using service workers for offline functionality
4. **Add analytics**: Track user interactions for product insights
5. **Add tests**: Write unit tests for utilities and integration tests for pages

---

## Getting Help

1. **Cursor AI Chat**: Use Cmd+L to open chat and ask specific questions
2. **Cursor Composer**: Use Cmd+I to generate code with context
3. **Spec Reference**: Always refer back to TRAYCER-UI-SPEC.md for detailed requirements
4. **Quickbase Docs**: https://developer.quickbase.com/

---

**Estimated Total Time**: 6-7 hours for complete implementation

Good luck! 🚀
