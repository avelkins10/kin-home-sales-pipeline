# UI Code Export - Current State

**Generated**: 2025-01-02
**Purpose**: Share with design-focused Claude for UI improvements
**Project**: Kin Home Rep Dashboard - Projects List Page

---

## Current UI Issues to Address

### Layout Problems
1. **Fixed Grid Layout**: Using `grid-cols-[220px_1fr_160px_80px]` with rigid column widths
2. **Multiple Link Components**: Each section wrapped in separate `<Link>` tags causing poor UX
3. **Traffic Light Sizing**: Fixed 32px circles may not scale well
4. **Emoji Icons**: Using emoji (📋, 📸, 📐, etc.) instead of proper icon library
5. **Generic Colors**: Basic gray/white color scheme lacks visual hierarchy
6. **Spacing Issues**: Inconsistent padding and gaps throughout

### UX Concerns
1. **Clickable Confusion**: Multiple separate links in one row (name, traffic lights, metrics, age) all go to same place
2. **No Hover States**: Limited visual feedback on interactions
3. **Hold Banner**: Disconnected from row (rounded-t-none hack)
4. **Filter Chips**: Basic blue/gray with no active state animation
5. **Sidebar**: Fixed 256px width, no collapse option

### Visual Design
1. **Low Contrast**: Gray-heavy color palette
2. **No Visual Hierarchy**: All text similar weight/size
3. **Flat Design**: Lacks depth, elevation, or modern styling
4. **No Loading Animations**: Basic skeleton states
5. **No Error State Design**: Simple text error messages

---

## File 1: Projects List Page

**Path**: `app/(dashboard)/projects/page.tsx`

```typescript
import { Suspense } from 'react'
import { getServerSession } from 'next-auth'
import { authOptions } from '@/lib/auth/next-auth.config'
import { redirect } from 'next/navigation'
import { ProjectTableView } from '@/components/projects/ProjectTableView'
import { ProjectFilterChips } from '@/components/projects/ProjectFilterChips'
import { SearchBar } from '@/components/projects/SearchBar'
import { Skeleton } from '@/components/ui/skeleton'

interface ProjectsPageProps {
  searchParams: {
    search?: string
    view?: string
  }
}

export default async function ProjectsPage({ searchParams }: ProjectsPageProps) {
  const session = await getServerSession(authOptions)

  if (!session) {
    redirect('/login')
  }

  return (
    <div className="min-h-screen bg-gray-50">
      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
        {/* Page Header */}
        <div className="mb-8">
          <h1 className="text-3xl font-bold text-gray-900">My Projects</h1>
          <p className="mt-2 text-gray-600">Manage and track all your solar installation projects</p>
        </div>

        {/* Controls Section */}
        <div className="mb-8 space-y-4">
          {/* Search Bar */}
          <SearchBar defaultValue={searchParams.search} />

          {/* Filter Chips */}
          <ProjectFilterChips />
        </div>

        {/* Project Table */}
        <Suspense fallback={<ProjectTableSkeleton />}>
          <ProjectTableView
            userId={session.user.quickbaseUserId}
            role={session.user.role}
            view={searchParams.view || 'all'}
            search={searchParams.search || ''}
          />
        </Suspense>
      </div>
    </div>
  )
}

function ProjectTableSkeleton() {
  return (
    <div className="space-y-4">
      {Array.from({ length: 6 }).map((_, i) => (
        <div key={i} className="bg-white rounded-lg border p-4 space-y-3">
          {/* Customer section skeleton */}
          <div className="space-y-2">
            <Skeleton className="h-4 w-32" />
            <Skeleton className="h-3 w-48" />
            <Skeleton className="h-3 w-36" />
          </div>

          {/* Traffic lights skeleton */}
          <div className="flex items-center gap-2">
            {Array.from({ length: 7 }).map((_, j) => (
              <Skeleton key={j} className="h-8 w-8 rounded-full" />
            ))}
          </div>

          {/* Metrics skeleton */}
          <div className="flex justify-between">
            <div className="space-y-1">
              <Skeleton className="h-3 w-16" />
              <Skeleton className="h-3 w-20" />
              <Skeleton className="h-3 w-24" />
            </div>
            <Skeleton className="h-6 w-12" />
          </div>
        </div>
      ))}
    </div>
  )
}
```

---

## File 2: Dashboard Layout with Sidebar

**Path**: `app/(dashboard)/layout.tsx`

```typescript
import { getServerSession } from 'next-auth';
import { authOptions } from '@/lib/auth/next-auth.config';
import { redirect } from 'next/navigation';
import Link from 'next/link';
import { cn } from '@/lib/utils/cn';
import { OfflineIndicator } from '@/components/ui/OfflineIndicator';

export default async function DashboardLayout({
  children,
}: {
  children: React.ReactNode;
}) {
  const session = await getServerSession(authOptions);

  if (!session) {
    redirect('/login');
  }

  // Navigation items with their paths
  const navigationItems = [
    { name: 'Dashboard', href: '/', roles: ['closer', 'setter', 'office_leader', 'regional', 'super_admin'] },
    { name: 'Projects', href: '/projects', roles: ['closer', 'setter', 'office_leader', 'regional', 'super_admin'] },
    { name: 'Holds', href: '/holds', roles: ['closer', 'setter', 'office_leader', 'regional', 'super_admin'] },
    { name: 'Analytics', href: '/analytics', roles: ['office_leader', 'regional', 'super_admin'] },
    { name: 'Settings', href: '/settings', roles: ['closer', 'setter', 'office_leader', 'regional', 'super_admin'] },
  ];

  // Filter navigation items based on user role
  const visibleNavItems = navigationItems.filter(item =>
    item.roles.includes(session.user.role)
  );

  return (
    <div className="min-h-screen bg-gray-50">
      {/* Offline Indicator */}
      <OfflineIndicator />

      {/* Sidebar Navigation */}
      <div className="fixed inset-y-0 left-0 z-50 w-64 bg-white shadow-lg">
        <div className="flex h-full flex-col">
          {/* Logo */}
          <div className="flex h-16 items-center justify-center border-b border-gray-200">
            <h1 className="text-xl font-bold text-gray-900">
              Kin Home Sales
            </h1>
          </div>

          {/* Navigation */}
          <nav className="flex-1 space-y-1 px-4 py-4">
            {visibleNavItems.map((item) => (
              <Link
                key={item.href}
                href={item.href}
                className="flex items-center rounded-md px-3 py-2 text-sm font-medium text-gray-700 hover:bg-gray-100 hover:text-gray-900"
              >
                {item.name}
              </Link>
            ))}
          </nav>

          {/* User Info */}
          <div className="border-t border-gray-200 p-4">
            <div className="flex items-center">
              <div className="flex-shrink-0">
                <div className="h-8 w-8 rounded-full bg-blue-500 flex items-center justify-center">
                  <span className="text-sm font-medium text-white">
                    {session.user.name?.charAt(0).toUpperCase()}
                  </span>
                </div>
              </div>
              <div className="ml-3">
                <p className="text-sm font-medium text-gray-700">
                  {session.user.name}
                </p>
                <p className="text-xs text-gray-500 capitalize">
                  {session.user.role.replace('_', ' ')}
                </p>
              </div>
            </div>
            <form action="/api/auth/signout" method="post" className="mt-2">
              <button
                type="submit"
                className="text-xs text-gray-500 hover:text-gray-700"
              >
                Sign out
              </button>
            </form>
          </div>
        </div>
      </div>

      {/* Main Content */}
      <div className="pl-64">
        <main className="py-6 pt-12">
          <div className="mx-auto max-w-7xl px-4 sm:px-6 lg:px-8">
            {children}
          </div>
        </main>
      </div>
    </div>
  );
}
```

---

## File 3: Project Table View Container

**Path**: `components/projects/ProjectTableView.tsx`

```typescript
'use client';

import { useQuery } from '@tanstack/react-query';
import { getProjectsForUserOffline } from '@/lib/offline/offlineQueries';
import { ProjectRow } from './ProjectRow';
import { Skeleton } from '@/components/ui/skeleton';
import { Button } from '@/components/ui/button';

interface ProjectTableViewProps {
  userId: string;
  role: string;
  view: string;
  search: string;
}

export function ProjectTableView({ userId, role, view, search }: ProjectTableViewProps) {
  const { data: projects, isLoading, error, refetch } = useQuery({
    queryKey: ['projects', userId, role, view, search],
    queryFn: () => getProjectsForUserOffline(userId, role, view, search),
    staleTime: 30000, // 30 seconds
    refetchInterval: 60000, // 1 minute for auto-refresh
  });

  // Loading state
  if (isLoading) {
    return (
      <div className="space-y-4">
        {Array.from({ length: 6 }).map((_, index) => (
          <div key={index} className="bg-white rounded-lg border p-4 space-y-3">
            {/* Customer section skeleton */}
            <div className="space-y-2">
              <Skeleton className="h-4 w-32" />
              <Skeleton className="h-3 w-48" />
              <Skeleton className="h-3 w-36" />
            </div>

            {/* Traffic lights skeleton */}
            <div className="flex items-center gap-2">
              {Array.from({ length: 7 }).map((_, i) => (
                <Skeleton key={i} className="h-8 w-8 rounded-full" />
              ))}
            </div>

            {/* Metrics skeleton */}
            <div className="flex justify-between">
              <div className="space-y-1">
                <Skeleton className="h-3 w-16" />
                <Skeleton className="h-3 w-20" />
                <Skeleton className="h-3 w-24" />
              </div>
              <Skeleton className="h-6 w-12" />
            </div>
          </div>
        ))}
      </div>
    );
  }

  // Error state
  if (error) {
    return (
      <div className="text-center py-8">
        <p className="text-red-600 mb-4">Failed to load projects</p>
        <Button onClick={() => window.location.reload()}>
          Try Again
        </Button>
      </div>
    );
  }

  // Empty state
  if (!projects || projects.length === 0) {
    return (
      <div className="text-center py-8">
        <p className="text-gray-600 mb-4">No projects found matching your filters.</p>
        <Button onClick={() => window.location.href = '/projects'}>
          Clear Filters
        </Button>
      </div>
    );
  }

  // Success state
  return (
    <div className="space-y-4">
      {projects.map((project) => {
        const recordId = project[3]?.value; // RECORD_ID field
        return (
          <ProjectRow key={recordId} project={project} />
        );
      })}
    </div>
  );
}
```

---

## File 4: Individual Project Row

**Path**: `components/projects/ProjectRow.tsx`

**KEY ISSUE**: Multiple `<Link>` components wrapping different sections, all pointing to same destination

```typescript
'use client';

import Link from 'next/link';
import { Phone } from 'lucide-react';
import { PROJECT_FIELDS } from '@/lib/constants/fieldIds';
import { QuickbaseProject } from '@/lib/types/project';
import { TrafficLightPipeline } from './TrafficLightPipeline';
import { PPWDisplay } from './PPWDisplay';
import { ProjectAgeIndicator } from './ProjectAgeIndicator';
import { HoldBanner } from './HoldBanner';
import { parseCustomerName, formatAddress, getProjectAge } from '@/lib/utils/project-helpers';
import { detectHoldStatus, extractHoldType } from '@/lib/utils/hold-detection';
import { formatSystemSize, formatCurrency } from '@/lib/utils/formatters';

interface ProjectRowProps {
  project: QuickbaseProject;
}

export function ProjectRow({ project }: ProjectRowProps) {
  // Extract data from project
  const recordId = project[PROJECT_FIELDS.RECORD_ID]?.value;
  const customerName = project[PROJECT_FIELDS.CUSTOMER_NAME]?.value || '';
  const customerAddress = project[PROJECT_FIELDS.CUSTOMER_ADDRESS]?.value || '';
  const customerPhone = project[PROJECT_FIELDS.CUSTOMER_PHONE]?.value || '';
  const projectStatus = project[PROJECT_FIELDS.PROJECT_STATUS]?.value || '';
  const systemSizeKw = parseFloat(project[PROJECT_FIELDS.SYSTEM_SIZE_KW]?.value || '0');
  const systemPrice = parseFloat(project[PROJECT_FIELDS.SYSTEM_PRICE]?.value || '0');

  // PPW fields - using the field IDs from quickbase-config.json
  const soldPPW = parseFloat(project[2292]?.value || '0') || null; // soldGross
  const commissionablePPW = parseFloat(project[2480]?.value || '0') || null; // commissionable

  // Calculate derived values
  const isOnHold = detectHoldStatus(projectStatus);
  const holdType = extractHoldType(projectStatus);
  const projectAge = getProjectAge(project);
  const parsedName = parseCustomerName(customerName);
  const formattedAddress = formatAddress(customerAddress);

  // Format phone number for tel: link
  const phoneLink = customerPhone ? `tel:${customerPhone.replace(/\D/g, '')}` : '';

  return (
    <div>
      {/* Hold banner (if on hold) */}
      {isOnHold && (
        <HoldBanner project={project} holdType={holdType} />
      )}

      {/* Main row content */}
      <div
        className={`bg-white border rounded-lg hover:shadow-md transition-shadow ${
          isOnHold ? 'rounded-t-none' : ''
        }`}
        data-testid="project-row"
      >
        <div className="grid grid-cols-[220px_1fr_160px_80px] gap-4 p-4">
          {/* Column 1 - Customer Info (220px fixed) */}
          <div className="space-y-1">
            <Link href={`/projects/${recordId}`}>
              <div className="font-semibold text-gray-900 hover:text-blue-600" data-testid="customer-name">
                {parsedName.firstName} {parsedName.lastName}
              </div>
            </Link>
            <div className="text-sm text-gray-600">
              {formattedAddress.line1}
            </div>
            {formattedAddress.line2 && (
              <div className="text-sm text-gray-600">
                {formattedAddress.line2}
              </div>
            )}
            {customerPhone && (
              <a
                href={phoneLink}
                className="text-sm text-blue-600 hover:underline flex items-center gap-1"
                data-testid="customer-phone"
              >
                <Phone className="h-3 w-3" />
                {customerPhone}
              </a>
            )}
          </div>

          {/* Column 2 - Traffic Lights (flexible width) */}
          <Link href={`/projects/${recordId}`} className="flex items-center" data-testid="traffic-lights">
            <TrafficLightPipeline project={project} />
          </Link>

          {/* Column 3 - Metrics (160px fixed) */}
          <Link href={`/projects/${recordId}`} className="space-y-2">
            {/* Size row */}
            <div className="flex items-center justify-between text-sm">
              <span className="text-gray-500">Size:</span>
              <span className="text-gray-900 font-medium">
                {formatSystemSize(systemSizeKw)}
              </span>
            </div>

            {/* PPW row */}
            <div className="flex items-center justify-between text-sm">
              <span className="text-gray-500">PPW:</span>
              <PPWDisplay soldPPW={soldPPW} commissionablePPW={commissionablePPW} />
            </div>

            {/* Value row */}
            <div className="flex items-center justify-between text-sm">
              <span className="text-gray-500">Value:</span>
              <span className="text-gray-900 font-medium">
                {formatCurrency(systemPrice)}
              </span>
            </div>
          </Link>

          {/* Column 4 - Age (80px fixed) */}
          <Link href={`/projects/${recordId}`} className="flex items-center justify-end" data-testid="project-age">
            <ProjectAgeIndicator age={projectAge} />
          </Link>
        </div>
      </div>
    </div>
  );
}
```

---

## File 5: Traffic Light Pipeline Component

**Path**: `components/projects/TrafficLightPipeline.tsx`

**KEY ISSUE**: Using emoji icons instead of proper icon library

```typescript
'use client';

import { cn } from '@/lib/utils/cn';
import { calculateMilestoneState, getMilestoneStatusText, MilestoneState } from '@/lib/utils/traffic-lights';
import { QuickbaseProject } from '@/lib/types/project';

interface TrafficLightPipelineProps {
  project: QuickbaseProject;
}

const milestones = [
  { id: 'intake' as const, label: 'Intake', icon: '📋' },
  { id: 'survey' as const, label: 'Survey', icon: '📸' },
  { id: 'design' as const, label: 'Design', icon: '📐' },
  { id: 'nem' as const, label: 'NEM', icon: '🔌' },
  { id: 'permit' as const, label: 'Permit', icon: '📋' },
  { id: 'install' as const, label: 'Install', icon: '🔧' },
  { id: 'inspection' as const, label: 'PTO', icon: '✅' },
];

function getTrafficLightClass(state: MilestoneState): string {
  switch (state) {
    case 'complete':
      return 'bg-green-500 text-white shadow-sm';
    case 'in-progress':
      return 'bg-amber-500 text-white shadow-lg';
    case 'pending':
      return 'bg-gray-200 text-gray-500';
    case 'on-hold':
      return 'bg-red-500 text-white';
    case 'overdue':
      return 'bg-red-600 text-white ring-2 ring-red-300';
    default:
      return 'bg-gray-200 text-gray-500';
  }
}

export function TrafficLightPipeline({ project }: TrafficLightPipelineProps) {
  return (
    <div className="space-y-2">
      {/* Traffic lights row */}
      <div className="flex items-center gap-2">
        {milestones.map((milestone, index) => {
          const state = calculateMilestoneState(project, milestone.id);
          const isLast = index === milestones.length - 1;

          return (
            <div key={milestone.id} className="flex items-center gap-2">
              {/* Traffic light circle */}
              <div
                className={cn(
                  'w-8 h-8 rounded-full flex items-center justify-center text-xs transition-all',
                  getTrafficLightClass(state),
                  state === 'in-progress' && 'animate-pulse'
                )}
                title={milestone.label}
              >
                {milestone.icon}
              </div>

              {/* Connector line */}
              {!isLast && (
                <div
                  className={cn(
                    'h-0.5 w-4',
                    state === 'complete' ? 'bg-green-500' : 'bg-gray-300'
                  )}
                />
              )}
            </div>
          );
        })}
      </div>

      {/* Status text below lights */}
      <div className="text-xs text-gray-600">
        {getMilestoneStatusText(project)}
      </div>
    </div>
  );
}
```

---

## File 6: Filter Chips Component

**Path**: `components/projects/ProjectFilterChips.tsx`

```typescript
'use client';

import { useRouter, useSearchParams } from 'next/navigation';
import { cn } from '@/lib/utils/cn';

const filterChips = [
  { value: 'all', label: 'All' },
  { value: 'active', label: 'Active' },
  { value: 'on-hold', label: 'On Hold' },
  { value: 'install-ready', label: 'Install Ready' },
  { value: 'install-scheduled', label: 'Install Scheduled' },
  { value: 'install-completed', label: 'Install Completed' },
  { value: 'pending-cancel', label: 'Pending Cancel' },
  { value: 'cancelled', label: 'Cancelled' },
  { value: 'needs-attention', label: 'Needs Attention' },
];

export function ProjectFilterChips() {
  const router = useRouter();
  const searchParams = useSearchParams();

  // Get current view parameter, default to 'all'
  const currentView = searchParams.get('view') || 'all';

  const handleFilterChange = (value: string) => {
    const params = new URLSearchParams(searchParams.toString());

    if (value === 'all') {
      params.delete('view');
    } else {
      params.set('view', value);
    }

    router.push(`/projects?${params.toString()}`);
  };

  return (
    <div className="flex gap-2 overflow-x-auto pb-2">
      {filterChips.map((chip) => {
        const isActive = currentView === chip.value;

        return (
          <button
            key={chip.value}
            onClick={() => handleFilterChange(chip.value)}
            className={cn(
              'px-4 py-2 rounded-full text-sm font-medium transition-colors whitespace-nowrap',
              isActive
                ? 'bg-blue-600 text-white'
                : 'bg-gray-100 text-gray-700 hover:bg-gray-200'
            )}
          >
            {chip.label}
          </button>
        );
      })}
    </div>
  );
}
```

---

## Design Improvement Requests

### Priority 1: Fix UX Issues
1. **Single Link Wrapper**: Entire ProjectRow should be one clickable area, not multiple links
2. **Phone Link**: Should prevent click-through to project detail (stopPropagation)
3. **Hold Banner Integration**: Better visual connection between banner and row
4. **Responsive Grid**: Replace fixed widths with responsive flexbox/grid

### Priority 2: Visual Polish
1. **Modern Color Palette**: Replace generic grays with branded colors
2. **Icon Library**: Replace emoji with Lucide React icons
3. **Micro-interactions**: Add smooth hover states, transitions
4. **Visual Hierarchy**: Better typography sizing and weights
5. **Elevation/Depth**: Add subtle shadows, borders, gradients

### Priority 3: Component Improvements
1. **Filter Chips**: Active state animation, better scroll indicators
2. **Traffic Lights**: More visual feedback, tooltips on hover
3. **Skeleton States**: More sophisticated loading animations
4. **Error States**: Better designed error messages with retry actions
5. **Sidebar**: Collapsible option, active route highlighting

### Priority 4: Accessibility
1. **Focus States**: Clear keyboard navigation indicators
2. **ARIA Labels**: Proper labels for screen readers
3. **Color Contrast**: Ensure WCAG AA compliance
4. **Touch Targets**: Minimum 44px for mobile/tablet

---

## Available UI Libraries

- **shadcn/ui components**: Card, Button, Skeleton, Badge, etc.
- **Lucide React**: Icon library (already imported for Phone icon)
- **Tailwind CSS**: Full utility library available
- **Framer Motion**: Available if needed for animations

---

## Design References

**Target Aesthetic**: Modern SaaS dashboard
- Clean, spacious layouts
- Subtle shadows and borders
- Smooth transitions
- Clear visual hierarchy
- Professional color palette

**Inspiration**: Linear, Vercel Dashboard, Stripe Dashboard

---

## Next Steps for Designer

1. **Review the code** and identify additional issues
2. **Propose a new design** for ProjectRow component
3. **Fix the multiple Link issue** (make entire row clickable)
4. **Replace emoji icons** with Lucide React icons
5. **Create a modern color palette** (primary, secondary, success, warning, danger)
6. **Improve spacing and typography** throughout
7. **Add micro-interactions** (hover, active, focus states)
8. **Design better loading/error states**

Feel free to be creative while maintaining the functional requirements!
