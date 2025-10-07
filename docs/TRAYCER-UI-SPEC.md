# Kin Home Rep Dashboard - UI Implementation Specification

**Target Platform**: iPad Pro 11" (1194x834) & iPad Pro 13" (1366x1024) - Landscape Orientation
**Design Reference**: `/ui-mockup-final.html`
**Framework**: Next.js 14 + TypeScript + Tailwind CSS + shadcn/ui

---

## Table of Contents
1. [Design System](#1-design-system)
2. [Dashboard Page](#2-dashboard-page)
3. [Projects List Page](#3-projects-list-page)
4. [Project Detail Page](#4-project-detail-page)
5. [Shared Components](#5-shared-components)
6. [Data Utilities](#6-data-utilities)
7. [Responsive Layout](#7-responsive-layout)
8. [Implementation Guide](#8-implementation-guide)
9. [Acceptance Criteria](#9-acceptance-criteria)

---

## 1. Design System

### 1.1 Color Palette

```css
/* Traffic Light States */
--traffic-complete: #10B981      /* green-500 */
--traffic-in-progress: #F59E0B   /* amber-500 */
--traffic-pending: #E5E7EB       /* gray-200 */
--traffic-on-hold: #EF4444       /* red-500 */
--traffic-overdue: #DC2626       /* red-600 */

/* PPW Delta Indicators */
--delta-positive: #10B981        /* Commission > Sold */
--delta-negative: #EF4444        /* Commission < Sold */
--delta-neutral: #6B7280         /* Commission = Sold */

/* Hold Banner Colors (by type) */
--hold-finance: #EF4444          /* red-500 */
--hold-roof: #F97316             /* orange-500 */
--hold-customer: #F59E0B         /* amber-500 */
--hold-permit: #EAB308           /* yellow-500 */
--hold-hoa: #84CC16              /* lime-500 */
--hold-generic: #6B7280          /* gray-500 */

/* Project Age Warnings */
--age-normal: #6B7280            /* < 90 days */
--age-warning: #F59E0B           /* 90-120 days */
--age-critical: #EF4444          /* > 120 days */

/* Status Badges */
--badge-active: #3B82F6          /* blue-500 */
--badge-hold: #EF4444            /* red-500 */
--badge-completed: #10B981       /* green-500 */
```

### 1.2 Typography

```css
/* Customer/Project Names */
--name-primary: font-semibold text-base text-gray-900
--name-secondary: font-semibold text-lg text-gray-900

/* Labels & Meta */
--label-small: text-xs text-gray-500
--label-medium: text-sm text-gray-600
--label-large: text-base text-gray-700

/* Values */
--value-small: font-bold text-sm text-gray-900
--value-medium: font-bold text-base text-gray-900
--value-large: font-bold text-2xl text-gray-900

/* Status Text */
--status-text: text-xs text-gray-600
```

---

## 2. Dashboard Page

**Route**: `/app/(dashboard)/page.tsx`

### 2.1 Page Layout

```typescript
import { Suspense } from 'react'
import { getServerSession } from 'next-auth'
import { authOptions } from '@/lib/auth/next-auth.config'
import { redirect } from 'next/navigation'
import { DashboardMetrics } from '@/components/dashboard/DashboardMetrics'
import { UrgentAlerts } from '@/components/dashboard/UrgentAlerts'
import { RecentProjects } from '@/components/dashboard/RecentProjects'

export default async function DashboardPage() {
  const session = await getServerSession(authOptions)
  if (!session) redirect('/login')

  return (
    <div className="space-y-6">
      {/* Page Header */}
      <div className="flex items-center justify-between">
        <div>
          <h1 className="text-3xl font-bold text-gray-900">
            Welcome back, {session.user.name}
          </h1>
          <p className="text-gray-600">
            {getRoleDisplayName(session.user.role)}
          </p>
        </div>
      </div>

      {/* Urgent Alerts */}
      <Suspense fallback={<AlertsSkeleton />}>
        <UrgentAlerts
          userId={session.user.quickbaseUserId}
          role={session.user.role}
        />
      </Suspense>

      {/* 4 Key Metrics */}
      <Suspense fallback={<MetricsSkeleton />}>
        <DashboardMetrics
          userId={session.user.quickbaseUserId}
          role={session.user.role}
        />
      </Suspense>

      {/* Recent Projects */}
      <Suspense fallback={<RecentProjectsSkeleton />}>
        <RecentProjects
          userId={session.user.quickbaseUserId}
          role={session.user.role}
        />
      </Suspense>
    </div>
  )
}
```

### 2.2 Component: DashboardMetrics

**File**: `components/dashboard/DashboardMetrics.tsx`

```typescript
'use client'

import { useQuery } from '@tanstack/react-query'
import { Card, CardContent } from '@/components/ui/card'
import { Calendar, TrendingUp, Pause, CheckCircle } from 'lucide-react'
import { getDashboardMetrics } from '@/lib/quickbase/queries'

interface DashboardMetricsProps {
  userId: string
  role: string
}

export function DashboardMetrics({ userId, role }: DashboardMetricsProps) {
  const { data: metrics, isLoading } = useQuery({
    queryKey: ['dashboard-metrics', userId, role],
    queryFn: () => getDashboardMetrics(userId, role),
  })

  if (isLoading) return <MetricsSkeleton />

  const stats = [
    {
      label: 'Installs This Week',
      value: metrics?.installsThisWeek || 0,
      icon: Calendar,
      color: 'text-blue-600',
      bgColor: 'bg-blue-50',
    },
    {
      label: 'Active Accounts',
      value: metrics?.activeProjects || 0,
      subtitle: 'Not on hold',
      icon: TrendingUp,
      color: 'text-green-600',
      bgColor: 'bg-green-50',
    },
    {
      label: 'On Hold',
      value: metrics?.onHold || 0,
      subtitle: metrics?.holdBreakdown || '',
      icon: Pause,
      color: 'text-red-600',
      bgColor: 'bg-red-50',
    },
    {
      label: 'Monthly Installs',
      value: metrics?.installsThisMonth || 0,
      icon: CheckCircle,
      color: 'text-purple-600',
      bgColor: 'bg-purple-50',
    },
  ]

  return (
    <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6">
      {stats.map((stat) => {
        const Icon = stat.icon
        return (
          <Card key={stat.label} className="hover:shadow-md transition-shadow">
            <CardContent className="p-6">
              <div className="flex items-center justify-between">
                <div className="flex-1">
                  <p className="text-sm text-gray-600 font-medium">{stat.label}</p>
                  <p className="text-3xl font-bold text-gray-900 mt-2">{stat.value}</p>
                  {stat.subtitle && (
                    <p className="text-xs text-gray-500 mt-1">{stat.subtitle}</p>
                  )}
                </div>
                <div className={`p-3 rounded-full ${stat.bgColor}`}>
                  <Icon className={`h-8 w-8 ${stat.color}`} />
                </div>
              </div>
            </CardContent>
          </Card>
        )
      })}
    </div>
  )
}
```

### 2.3 Component: UrgentAlerts

**File**: `components/dashboard/UrgentAlerts.tsx`

Show projects on hold > 7 days:

```typescript
'use client'

import { useQuery } from '@tanstack/react-query'
import { AlertTriangle, Clock } from 'lucide-react'
import Link from 'next/link'
import { getUrgentProjects } from '@/lib/quickbase/queries'

interface UrgentAlertsProps {
  userId: string
  role: string
}

export function UrgentAlerts({ userId, role }: UrgentAlertsProps) {
  const { data: urgentProjects } = useQuery({
    queryKey: ['urgent-projects', userId, role],
    queryFn: () => getUrgentProjects(userId, role),
  })

  if (!urgentProjects || urgentProjects.length === 0) return null

  return (
    <div className="bg-red-50 border-l-4 border-red-500 rounded-lg p-4">
      <div className="flex items-start gap-3">
        <AlertTriangle className="h-5 w-5 text-red-600 mt-0.5" />
        <div className="flex-1">
          <h3 className="font-semibold text-red-900">
            Urgent Attention Required
          </h3>
          <p className="text-sm text-red-700 mt-1">
            {urgentProjects.length} project{urgentProjects.length > 1 ? 's' : ''} on hold for more than 7 days
          </p>
          <div className="mt-3 space-y-2">
            {urgentProjects.slice(0, 3).map((project) => (
              <Link
                key={project.recordId}
                href={`/projects/${project.recordId}`}
                className="block p-2 bg-white rounded hover:bg-red-50 transition-colors"
              >
                <div className="flex items-center justify-between">
                  <span className="text-sm font-medium text-gray-900">
                    {project.customerName}
                  </span>
                  <div className="flex items-center gap-2 text-xs text-red-600">
                    <Clock className="h-3 w-3" />
                    {project.daysOnHold} days
                  </div>
                </div>
                <p className="text-xs text-gray-600 mt-1">{project.holdReason}</p>
              </Link>
            ))}
          </div>
          {urgentProjects.length > 3 && (
            <Link
              href="/projects?view=on-hold"
              className="text-sm text-red-700 font-medium hover:text-red-800 mt-2 inline-block"
            >
              View all {urgentProjects.length} on hold →
            </Link>
          )}
        </div>
      </div>
    </div>
  )
}
```

### 2.4 Component: RecentProjects

**File**: `components/dashboard/RecentProjects.tsx`

Show 5 most recent active projects:

```typescript
'use client'

import { useQuery } from '@tanstack/react-query'
import Link from 'next/link'
import { ArrowRight } from 'lucide-react'
import { getRecentProjects } from '@/lib/quickbase/queries'
import { getCurrentMilestone } from '@/lib/utils/milestones'
import { Badge } from '@/components/ui/badge'

interface RecentProjectsProps {
  userId: string
  role: string
}

export function RecentProjects({ userId, role }: RecentProjectsProps) {
  const { data: projects } = useQuery({
    queryKey: ['recent-projects', userId, role],
    queryFn: () => getRecentProjects(userId, role),
  })

  if (!projects || projects.length === 0) return null

  return (
    <div className="bg-white rounded-lg border border-gray-200 p-6">
      <div className="flex items-center justify-between mb-4">
        <h3 className="text-lg font-semibold text-gray-900">Recent Projects</h3>
        <Link
          href="/projects"
          className="text-sm text-blue-600 hover:text-blue-700 font-medium"
        >
          View all →
        </Link>
      </div>
      <div className="space-y-3">
        {projects.map((project) => {
          const milestone = getCurrentMilestone(project)
          return (
            <Link
              key={project.recordId}
              href={`/projects/${project.recordId}`}
              className="block p-3 rounded-lg hover:bg-gray-50 transition-colors"
            >
              <div className="flex items-center justify-between">
                <div className="flex-1">
                  <p className="font-medium text-gray-900">{project.customerName}</p>
                  <p className="text-sm text-gray-500 mt-0.5">
                    {project.projectId}
                  </p>
                </div>
                <div className="flex items-center gap-3">
                  <Badge variant="secondary">{milestone}</Badge>
                  <ArrowRight className="h-4 w-4 text-gray-400" />
                </div>
              </div>
            </Link>
          )
        })}
      </div>
    </div>
  )
}
```

---

## 3. Projects List Page

**Route**: `/app/(dashboard)/projects/page.tsx`

### 3.1 Page Layout

```typescript
import { Suspense } from 'react'
import { getServerSession } from 'next-auth'
import { authOptions } from '@/lib/auth/next-auth.config'
import { redirect } from 'next/navigation'
import { ProjectListHeader } from '@/components/projects/ProjectListHeader'
import { ProjectFilterChips } from '@/components/projects/ProjectFilterChips'
import { ProjectTableView } from '@/components/projects/ProjectTableView'
import { SearchBar } from '@/components/projects/SearchBar'

export default async function ProjectsPage({
  searchParams,
}: {
  searchParams: {
    view?: string
    search?: string
  }
}) {
  const session = await getServerSession(authOptions)
  if (!session) redirect('/login')

  return (
    <div className="space-y-4">
      {/* Header */}
      <ProjectListHeader />

      {/* Search + Filters */}
      <div className="flex flex-col sm:flex-row items-start sm:items-center justify-between gap-4">
        <SearchBar />
        <ProjectFilterChips />
      </div>

      {/* Project Table */}
      <Suspense fallback={<ProjectTableSkeleton />}>
        <ProjectTableView
          userId={session.user.quickbaseUserId}
          role={session.user.role}
          view={searchParams.view || 'all'}
          search={searchParams.search}
        />
      </Suspense>
    </div>
  )
}
```

### 3.2 Component: ProjectFilterChips

**File**: `components/projects/ProjectFilterChips.tsx`

```typescript
'use client'

import { useRouter, useSearchParams } from 'next/navigation'
import { cn } from '@/lib/utils'

type FilterView =
  | 'all'
  | 'active'
  | 'on-hold'
  | 'install-ready'
  | 'install-scheduled'
  | 'install-completed'
  | 'pending-cancel'
  | 'cancelled'
  | 'needs-attention'

interface FilterChip {
  value: FilterView
  label: string
}

export function ProjectFilterChips() {
  const router = useRouter()
  const searchParams = useSearchParams()
  const currentView = (searchParams.get('view') || 'all') as FilterView

  const filters: FilterChip[] = [
    { value: 'all', label: 'All Projects' },
    { value: 'active', label: 'Active' },
    { value: 'on-hold', label: 'On Hold' },
    { value: 'install-ready', label: 'Install Ready' },
    { value: 'install-scheduled', label: 'Install Scheduled' },
    { value: 'install-completed', label: 'Install Completed' },
    { value: 'pending-cancel', label: 'Pending Cancel' },
    { value: 'cancelled', label: 'Cancelled' },
    { value: 'needs-attention', label: 'Needs Attention' },
  ]

  const handleFilterClick = (view: FilterView) => {
    const params = new URLSearchParams(searchParams)
    if (view === 'all') {
      params.delete('view')
    } else {
      params.set('view', view)
    }
    router.push(`/projects?${params.toString()}`)
  }

  return (
    <div className="flex items-center gap-2 overflow-x-auto pb-2">
      {filters.map((filter) => (
        <button
          key={filter.value}
          onClick={() => handleFilterClick(filter.value)}
          className={cn(
            'px-4 py-2 rounded-full text-sm font-medium whitespace-nowrap transition-colors',
            currentView === filter.value
              ? 'bg-blue-600 text-white shadow-sm'
              : 'bg-gray-100 text-gray-700 hover:bg-gray-200'
          )}
        >
          {filter.label}
        </button>
      ))}
    </div>
  )
}
```

### 3.3 Component: ProjectTableView

**File**: `components/projects/ProjectTableView.tsx`

```typescript
'use client'

import { useQuery } from '@tanstack/react-query'
import { ProjectRow } from './ProjectRow'
import { getProjectsForUser } from '@/lib/quickbase/queries'
import type { QuickbaseProject } from '@/lib/types/project'

interface ProjectTableViewProps {
  userId: string
  role: string
  view: string
  search?: string
}

export function ProjectTableView({ userId, role, view, search }: ProjectTableViewProps) {
  const { data: projects, isLoading } = useQuery({
    queryKey: ['projects', userId, role, view, search],
    queryFn: () => getProjectsForUser(userId, role, view, search),
    refetchInterval: 30000, // Refresh every 30 seconds
  })

  if (isLoading) return <ProjectTableSkeleton />

  if (!projects || projects.length === 0) {
    return (
      <div className="text-center py-12 bg-white rounded-lg border border-gray-200">
        <p className="text-gray-500">No projects found</p>
      </div>
    )
  }

  return (
    <div className="space-y-3">
      {projects.map((project: QuickbaseProject) => (
        <ProjectRow key={project[3]?.value} project={project} />
      ))}
    </div>
  )
}
```

### 3.4 Component: ProjectRow

**File**: `components/projects/ProjectRow.tsx`

```typescript
'use client'

import Link from 'next/link'
import { TrafficLightPipeline } from './TrafficLightPipeline'
import { PPWDisplay } from './PPWDisplay'
import { HoldBanner } from './HoldBanner'
import { ProjectAgeIndicator } from './ProjectAgeIndicator'
import { PROJECT_FIELDS } from '@/lib/constants/fieldIds'
import { formatSystemSize, formatCurrency } from '@/lib/utils/formatters'
import { parseCustomerName, formatAddress, getProjectAge } from '@/lib/utils/project-helpers'
import { detectHoldStatus, extractHoldType } from '@/lib/utils/hold-detection'
import type { QuickbaseProject } from '@/lib/types/project'

interface ProjectRowProps {
  project: QuickbaseProject
}

export function ProjectRow({ project }: ProjectRowProps) {
  // Extract data
  const recordId = project[PROJECT_FIELDS.RECORD_ID]?.value
  const customerName = project[PROJECT_FIELDS.CUSTOMER_NAME]?.value || 'Unknown'
  const customerAddress = project[PROJECT_FIELDS.CUSTOMER_ADDRESS]?.value || ''
  const phoneNumber = project[PROJECT_FIELDS.CUSTOMER_PHONE]?.value || ''
  const projectStatus = project[PROJECT_FIELDS.PROJECT_STATUS]?.value || ''

  // Parse full name
  const { firstName, lastName } = parseCustomerName(customerName)

  // Format address
  const { line1, line2 } = formatAddress(customerAddress)

  // Metrics
  const systemSize = project[PROJECT_FIELDS.SYSTEM_SIZE_KW]?.value
  const soldPPW = project[PROJECT_FIELDS.SOLD_NET_PPW]?.value
  const commissionablePPW = project[PROJECT_FIELDS.COMMISSIONABLE_PPW]?.value
  const systemPrice = project[PROJECT_FIELDS.SYSTEM_PRICE]?.value
  const projectAge = getProjectAge(project)

  // Hold detection
  const isOnHold = detectHoldStatus(projectStatus)
  const holdType = extractHoldType(projectStatus)

  return (
    <div className="group relative">
      {/* Hold Banner - appears above row when on hold */}
      {isOnHold && <HoldBanner project={project} holdType={holdType} />}

      {/* Main Row - Grid Layout */}
      <Link
        href={`/projects/${recordId}`}
        className={cn(
          'block bg-white border border-gray-200 hover:shadow-md transition-shadow',
          isOnHold ? 'rounded-b-lg' : 'rounded-lg'
        )}
      >
        <div className="grid grid-cols-[220px_1fr_160px_80px] gap-4 p-4 items-center">

          {/* Column 1: Customer Info (220px) */}
          <div className="space-y-1">
            <h3 className="font-semibold text-base text-gray-900">
              {firstName} {lastName}
            </h3>
            <p className="text-sm text-gray-600">{line1}</p>
            {line2 && <p className="text-sm text-gray-600">{line2}</p>}
            {phoneNumber && (
              <a
                href={`tel:${phoneNumber}`}
                onClick={(e) => e.stopPropagation()}
                className="text-sm text-blue-600 hover:underline"
              >
                {phoneNumber}
              </a>
            )}
          </div>

          {/* Column 2: Traffic Light Pipeline (flexible) */}
          <div className="flex-1">
            <TrafficLightPipeline project={project} />
          </div>

          {/* Column 3: Metrics (160px) */}
          <div className="space-y-2">
            <div className="flex items-center justify-between">
              <span className="text-xs text-gray-500">Size</span>
              <span className="font-bold text-sm text-gray-900">
                {systemSize ? formatSystemSize(parseFloat(systemSize)) : 'N/A'}
              </span>
            </div>

            <div className="flex items-center justify-between">
              <span className="text-xs text-gray-500">PPW</span>
              <PPWDisplay
                soldPPW={soldPPW ? parseFloat(soldPPW) : null}
                commissionablePPW={commissionablePPW ? parseFloat(commissionablePPW) : null}
              />
            </div>

            <div className="flex items-center justify-between">
              <span className="text-xs text-gray-500">Value</span>
              <span className="font-bold text-sm text-gray-900">
                {systemPrice ? formatCurrency(parseFloat(systemPrice)) : 'N/A'}
              </span>
            </div>
          </div>

          {/* Column 4: Age Indicator (80px) */}
          <div className="text-right">
            <ProjectAgeIndicator age={projectAge} />
          </div>

        </div>
      </Link>
    </div>
  )
}
```

---

## 4. Project Detail Page

**Route**: `/app/(dashboard)/projects/[id]/page.tsx`

### 4.1 Page Layout

```typescript
import { Suspense } from 'react'
import { getServerSession } from 'next-auth'
import { authOptions } from '@/lib/auth/next-auth.config'
import { redirect, notFound } from 'next/navigation'
import { getProjectById } from '@/lib/quickbase/queries'
import { ProjectDetailHeader } from '@/components/projects/detail/ProjectDetailHeader'
import { ProjectDetailTabs } from '@/components/projects/detail/ProjectDetailTabs'

export default async function ProjectDetailPage({
  params,
}: {
  params: { id: string }
}) {
  const session = await getServerSession(authOptions)
  if (!session) redirect('/login')

  const project = await getProjectById(params.id)
  if (!project) notFound()

  return (
    <div className="space-y-6">
      {/* Hero Header */}
      <ProjectDetailHeader project={project} />

      {/* Tabbed Content */}
      <ProjectDetailTabs project={project} />
    </div>
  )
}
```

### 4.2 Component: ProjectDetailHeader

**File**: `components/projects/detail/ProjectDetailHeader.tsx`

```typescript
'use client'

import Link from 'next/link'
import { ArrowLeft, Phone, Mail, MapPin, Edit } from 'lucide-react'
import { Button } from '@/components/ui/button'
import { HoldBanner } from '@/components/projects/HoldBanner'
import { PROJECT_FIELDS } from '@/lib/constants/fieldIds'
import { parseCustomerName, formatAddress } from '@/lib/utils/project-helpers'
import { detectHoldStatus, extractHoldType } from '@/lib/utils/hold-detection'
import type { QuickbaseProject } from '@/lib/types/project'

interface ProjectDetailHeaderProps {
  project: QuickbaseProject
}

export function ProjectDetailHeader({ project }: ProjectDetailHeaderProps) {
  const customerName = project[PROJECT_FIELDS.CUSTOMER_NAME]?.value || 'Unknown'
  const projectId = project[PROJECT_FIELDS.PROJECT_ID]?.value || 'N/A'
  const customerAddress = project[PROJECT_FIELDS.CUSTOMER_ADDRESS]?.value || ''
  const phoneNumber = project[PROJECT_FIELDS.CUSTOMER_PHONE]?.value || ''
  const email = project[PROJECT_FIELDS.CUSTOMER_EMAIL]?.value || ''
  const projectStatus = project[PROJECT_FIELDS.PROJECT_STATUS]?.value || ''

  const { firstName, lastName } = parseCustomerName(customerName)
  const { line1, line2 } = formatAddress(customerAddress)
  const isOnHold = detectHoldStatus(projectStatus)
  const holdType = extractHoldType(projectStatus)

  return (
    <div className="space-y-4">
      {/* Back Button */}
      <Link
        href="/projects"
        className="inline-flex items-center gap-2 text-sm text-gray-600 hover:text-gray-900"
      >
        <ArrowLeft className="h-4 w-4" />
        Back to Projects
      </Link>

      {/* Hero Card */}
      <div className="bg-white rounded-lg border border-gray-200 overflow-hidden">
        {/* Hold Banner */}
        {isOnHold && <HoldBanner project={project} holdType={holdType} />}

        {/* Main Content */}
        <div className="p-6">
          <div className="flex items-start justify-between">
            <div className="flex-1">
              {/* Customer Name */}
              <h1 className="text-3xl font-bold text-gray-900">
                {firstName} {lastName}
              </h1>

              {/* Project ID */}
              <p className="text-sm text-gray-500 mt-1 font-mono">
                Project #{projectId}
              </p>

              {/* Contact Info */}
              <div className="flex flex-wrap items-center gap-4 mt-4">
                {customerAddress && (
                  <div className="flex items-center gap-2 text-sm text-gray-600">
                    <MapPin className="h-4 w-4" />
                    <span>{line1}{line2 ? `, ${line2}` : ''}</span>
                  </div>
                )}
                {phoneNumber && (
                  <a
                    href={`tel:${phoneNumber}`}
                    className="flex items-center gap-2 text-sm text-blue-600 hover:underline"
                  >
                    <Phone className="h-4 w-4" />
                    {phoneNumber}
                  </a>
                )}
                {email && (
                  <a
                    href={`mailto:${email}`}
                    className="flex items-center gap-2 text-sm text-blue-600 hover:underline"
                  >
                    <Mail className="h-4 w-4" />
                    {email}
                  </a>
                )}
              </div>
            </div>

            {/* Action Buttons */}
            <div className="flex items-center gap-2">
              <Button variant="outline" size="sm">
                <Edit className="h-4 w-4 mr-2" />
                Edit
              </Button>
            </div>
          </div>
        </div>
      </div>
    </div>
  )
}
```

### 4.3 Component: ProjectDetailTabs

**File**: `components/projects/detail/ProjectDetailTabs.tsx`

```typescript
'use client'

import { useState } from 'react'
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs'
import { OverviewTab } from './tabs/OverviewTab'
import { TimelineTab } from './tabs/TimelineTab'
import { SystemTab } from './tabs/SystemTab'
import { FinancialsTab } from './tabs/FinancialsTab'
import { DocumentsTab } from './tabs/DocumentsTab'
import { ActivityTab } from './tabs/ActivityTab'
import type { QuickbaseProject } from '@/lib/types/project'

interface ProjectDetailTabsProps {
  project: QuickbaseProject
}

export function ProjectDetailTabs({ project }: ProjectDetailTabsProps) {
  return (
    <Tabs defaultValue="overview" className="w-full">
      <TabsList className="grid w-full grid-cols-6">
        <TabsTrigger value="overview">Overview</TabsTrigger>
        <TabsTrigger value="timeline">Timeline</TabsTrigger>
        <TabsTrigger value="system">System</TabsTrigger>
        <TabsTrigger value="financials">Financials</TabsTrigger>
        <TabsTrigger value="documents">Documents</TabsTrigger>
        <TabsTrigger value="activity">Activity</TabsTrigger>
      </TabsList>

      <TabsContent value="overview" className="mt-6">
        <OverviewTab project={project} />
      </TabsContent>

      <TabsContent value="timeline" className="mt-6">
        <TimelineTab project={project} />
      </TabsContent>

      <TabsContent value="system" className="mt-6">
        <SystemTab project={project} />
      </TabsContent>

      <TabsContent value="financials" className="mt-6">
        <FinancialsTab project={project} />
      </TabsContent>

      <TabsContent value="documents" className="mt-6">
        <DocumentsTab project={project} />
      </TabsContent>

      <TabsContent value="activity" className="mt-6">
        <ActivityTab project={project} />
      </TabsContent>
    </Tabs>
  )
}
```

### 4.4 Tab: Overview

**File**: `components/projects/detail/tabs/OverviewTab.tsx`

```typescript
'use client'

import { Card, CardHeader, CardTitle, CardContent } from '@/components/ui/card'
import { ExpandedTrafficLightPipeline } from '@/components/projects/ExpandedTrafficLightPipeline'
import { QuickStats } from '@/components/projects/QuickStats'
import { KeyContacts } from '@/components/projects/KeyContacts'
import { ActivityFeed } from '@/components/projects/ActivityFeed'
import type { QuickbaseProject } from '@/lib/types/project'

interface OverviewTabProps {
  project: QuickbaseProject
}

export function OverviewTab({ project }: OverviewTabProps) {
  return (
    <div className="grid grid-cols-1 lg:grid-cols-3 gap-6">
      {/* Left Column (2/3) */}
      <div className="lg:col-span-2 space-y-6">
        {/* Expanded Traffic Light Pipeline */}
        <Card>
          <CardHeader>
            <CardTitle>Project Pipeline</CardTitle>
          </CardHeader>
          <CardContent>
            <ExpandedTrafficLightPipeline project={project} />
          </CardContent>
        </Card>

        {/* Recent Activity */}
        <Card>
          <CardHeader>
            <CardTitle>Recent Activity</CardTitle>
          </CardHeader>
          <CardContent>
            <ActivityFeed project={project} limit={5} />
          </CardContent>
        </Card>
      </div>

      {/* Right Column (1/3) */}
      <div className="space-y-6">
        {/* Quick Stats */}
        <QuickStats project={project} />

        {/* Key Contacts */}
        <KeyContacts project={project} />
      </div>
    </div>
  )
}
```

### 4.5 Tab: Timeline

**File**: `components/projects/detail/tabs/TimelineTab.tsx`

```typescript
'use client'

import { Card, CardContent } from '@/components/ui/card'
import { CheckCircle, Clock, Circle } from 'lucide-react'
import { PROJECT_FIELDS } from '@/lib/constants/fieldIds'
import { formatDate } from '@/lib/utils/formatters'
import { getInstallRescheduleHistory } from '@/lib/utils/project-helpers'
import type { QuickbaseProject } from '@/lib/types/project'

interface TimelineTabProps {
  project: QuickbaseProject
}

export function TimelineTab({ project }: TimelineTabProps) {
  const milestones = [
    {
      id: 'intake',
      title: 'Intake',
      fields: [
        { label: 'Sales Date', fieldId: PROJECT_FIELDS.SALES_DATE },
        { label: 'Survey Scheduled', fieldId: PROJECT_FIELDS.SURVEY_SCHEDULED_DATE },
      ],
    },
    {
      id: 'survey',
      title: 'Survey',
      fields: [
        { label: 'Survey Completed', fieldId: PROJECT_FIELDS.SURVEY_COMPLETED },
        { label: 'Survey Approved', fieldId: PROJECT_FIELDS.SURVEY_APPROVED },
      ],
    },
    {
      id: 'design',
      title: 'Design',
      fields: [
        { label: 'Design Started', fieldId: PROJECT_FIELDS.DESIGN_STARTED },
        { label: 'Design Completed', fieldId: PROJECT_FIELDS.DESIGN_COMPLETED },
        { label: 'CAD Design Approved', fieldId: PROJECT_FIELDS.CAD_DESIGN_APPROVED },
      ],
    },
    {
      id: 'nem',
      title: 'NEM (Utility)',
      fields: [
        { label: 'NEM Submitted', fieldId: PROJECT_FIELDS.NEM_SUBMITTED_DATE },
        { label: 'NEM Approved', fieldId: PROJECT_FIELDS.NEM_APPROVED },
      ],
    },
    {
      id: 'permit',
      title: 'Permit (AHJ)',
      fields: [
        { label: 'Permit Submitted', fieldId: PROJECT_FIELDS.PERMIT_SUBMITTED_DATE },
        { label: 'Permit Approved', fieldId: PROJECT_FIELDS.PERMIT_APPROVED },
      ],
    },
    {
      id: 'install',
      title: 'Install',
      fields: [
        { label: 'Install Started', fieldId: PROJECT_FIELDS.INSTALL_STARTED_DATE },
        { label: 'Install Completed', fieldId: PROJECT_FIELDS.INSTALL_COMPLETED_DATE },
      ],
    },
    {
      id: 'inspection',
      title: 'Inspection / PTO',
      fields: [
        { label: 'Passing Inspection', fieldId: PROJECT_FIELDS.PASSING_INSPECTION_COMPLETED },
        { label: 'PTO Approved', fieldId: PROJECT_FIELDS.PTO_APPROVED },
      ],
    },
  ]

  const rescheduleHistory = getInstallRescheduleHistory(project)

  return (
    <div className="space-y-6">
      {/* Timeline */}
      <Card>
        <CardContent className="p-6">
          <div className="space-y-8">
            {milestones.map((milestone, index) => {
              const allFieldsComplete = milestone.fields.every(
                field => project[field.fieldId]?.value
              )
              const anyFieldComplete = milestone.fields.some(
                field => project[field.fieldId]?.value
              )

              return (
                <div key={milestone.id} className="relative">
                  {/* Vertical Line */}
                  {index < milestones.length - 1 && (
                    <div className="absolute left-3 top-8 bottom-0 w-0.5 bg-gray-200" />
                  )}

                  {/* Milestone Header */}
                  <div className="flex items-start gap-4">
                    {/* Icon */}
                    <div className="relative z-10">
                      {allFieldsComplete ? (
                        <CheckCircle className="h-6 w-6 text-green-500" />
                      ) : anyFieldComplete ? (
                        <Clock className="h-6 w-6 text-amber-500" />
                      ) : (
                        <Circle className="h-6 w-6 text-gray-300" />
                      )}
                    </div>

                    {/* Content */}
                    <div className="flex-1 pb-8">
                      <h3 className="font-semibold text-gray-900">{milestone.title}</h3>
                      <div className="mt-2 space-y-1">
                        {milestone.fields.map(field => {
                          const value = project[field.fieldId]?.value
                          return (
                            <div key={field.fieldId} className="flex items-center justify-between text-sm">
                              <span className="text-gray-600">{field.label}</span>
                              <span className={value ? 'text-gray-900 font-medium' : 'text-gray-400'}>
                                {value ? formatDate(value) : 'Pending'}
                              </span>
                            </div>
                          )
                        })}
                      </div>

                      {/* Install Reschedule History */}
                      {milestone.id === 'install' && rescheduleHistory.length > 0 && (
                        <div className="mt-3 p-3 bg-gray-50 rounded-lg">
                          <p className="text-xs font-medium text-gray-700 mb-2">Schedule History</p>
                          <div className="space-y-1">
                            {rescheduleHistory.map((entry, idx) => (
                              <div key={idx} className="flex items-center justify-between text-xs">
                                <span className="text-gray-600">{entry.source}</span>
                                <span className="text-gray-900">{formatDate(entry.date)}</span>
                              </div>
                            ))}
                          </div>
                        </div>
                      )}
                    </div>
                  </div>
                </div>
              )
            })}
          </div>
        </CardContent>
      </Card>
    </div>
  )
}
```

### 4.6 Tab: System

**File**: `components/projects/detail/tabs/SystemTab.tsx`

```typescript
'use client'

import { Card, CardHeader, CardTitle, CardContent } from '@/components/ui/card'
import { PROJECT_FIELDS } from '@/lib/constants/fieldIds'
import { formatSystemSize } from '@/lib/utils/formatters'
import type { QuickbaseProject } from '@/lib/types/project'

interface SystemTabProps {
  project: QuickbaseProject
}

export function SystemTab({ project }: SystemTabProps) {
  const systemFields = [
    { label: 'System Size', fieldId: PROJECT_FIELDS.SYSTEM_SIZE_KW, format: (v: string) => formatSystemSize(parseFloat(v)) },
    { label: 'Panel Count', fieldId: PROJECT_FIELDS.PANEL_COUNT },
    { label: 'Panel Type', fieldId: PROJECT_FIELDS.PANEL_TYPE },
    { label: 'Inverter Type', fieldId: PROJECT_FIELDS.INVERTER_TYPE },
    { label: 'Battery', fieldId: PROJECT_FIELDS.BATTERY },
    { label: 'Roof Type', fieldId: PROJECT_FIELDS.ROOF_TYPE },
  ]

  return (
    <Card>
      <CardHeader>
        <CardTitle>System Details</CardTitle>
      </CardHeader>
      <CardContent>
        <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
          {systemFields.map(field => {
            const value = project[field.fieldId]?.value
            return (
              <div key={field.fieldId} className="flex flex-col">
                <span className="text-sm text-gray-500">{field.label}</span>
                <span className="text-base font-medium text-gray-900 mt-1">
                  {value ? (field.format ? field.format(value) : value) : 'N/A'}
                </span>
              </div>
            )
          })}
        </div>
      </CardContent>
    </Card>
  )
}
```

### 4.7 Tab: Financials

**File**: `components/projects/detail/tabs/FinancialsTab.tsx`

```typescript
'use client'

import { Card, CardHeader, CardTitle, CardContent } from '@/components/ui/card'
import { PPWDisplay } from '@/components/projects/PPWDisplay'
import { PROJECT_FIELDS } from '@/lib/constants/fieldIds'
import { formatCurrency } from '@/lib/utils/formatters'
import type { QuickbaseProject } from '@/lib/types/project'

interface FinancialsTabProps {
  project: QuickbaseProject
}

export function FinancialsTab({ project }: FinancialsTabProps) {
  const systemPrice = project[PROJECT_FIELDS.SYSTEM_PRICE]?.value
  const soldPPW = project[PROJECT_FIELDS.SOLD_NET_PPW]?.value
  const commissionablePPW = project[PROJECT_FIELDS.COMMISSIONABLE_PPW]?.value
  const financingType = project[PROJECT_FIELDS.FINANCING_TYPE]?.value

  return (
    <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
      {/* Pricing Card */}
      <Card>
        <CardHeader>
          <CardTitle>Pricing</CardTitle>
        </CardHeader>
        <CardContent className="space-y-4">
          <div className="flex flex-col">
            <span className="text-sm text-gray-500">Total System Price</span>
            <span className="text-2xl font-bold text-gray-900 mt-1">
              {systemPrice ? formatCurrency(parseFloat(systemPrice)) : 'N/A'}
            </span>
          </div>

          <div className="flex flex-col">
            <span className="text-sm text-gray-500">Price Per Watt</span>
            <div className="mt-2">
              <PPWDisplay
                soldPPW={soldPPW ? parseFloat(soldPPW) : null}
                commissionablePPW={commissionablePPW ? parseFloat(commissionablePPW) : null}
              />
            </div>
          </div>
        </CardContent>
      </Card>

      {/* Financing Card */}
      <Card>
        <CardHeader>
          <CardTitle>Financing</CardTitle>
        </CardHeader>
        <CardContent>
          <div className="flex flex-col">
            <span className="text-sm text-gray-500">Type</span>
            <span className="text-base font-medium text-gray-900 mt-1">
              {financingType || 'N/A'}
            </span>
          </div>
        </CardContent>
      </Card>
    </div>
  )
}
```

### 4.8 Tab: Documents

**File**: `components/projects/detail/tabs/DocumentsTab.tsx`

```typescript
'use client'

import { Card, CardContent } from '@/components/ui/card'
import { FileText, Download } from 'lucide-react'
import { Button } from '@/components/ui/button'

export function DocumentsTab() {
  // TODO: Integrate with Quickbase attachments
  const documents = [
    { name: 'Contract - Signed.pdf', date: '2024-11-15', size: '245 KB' },
    { name: 'HOA Approval.pdf', date: '2024-11-20', size: '180 KB' },
    { name: 'CAD Design.dwg', date: '2024-12-05', size: '1.2 MB' },
  ]

  return (
    <Card>
      <CardContent className="p-6">
        <div className="space-y-3">
          {documents.map((doc, index) => (
            <div
              key={index}
              className="flex items-center justify-between p-4 border border-gray-200 rounded-lg hover:bg-gray-50"
            >
              <div className="flex items-center gap-3">
                <FileText className="h-8 w-8 text-blue-600" />
                <div>
                  <p className="font-medium text-gray-900">{doc.name}</p>
                  <p className="text-sm text-gray-500">{doc.date} • {doc.size}</p>
                </div>
              </div>
              <Button variant="ghost" size="sm">
                <Download className="h-4 w-4" />
              </Button>
            </div>
          ))}
        </div>
      </CardContent>
    </Card>
  )
}
```

### 4.9 Tab: Activity

**File**: `components/projects/detail/tabs/ActivityTab.tsx`

```typescript
'use client'

import { Card, CardContent } from '@/components/ui/card'
import { ActivityFeed } from '@/components/projects/ActivityFeed'
import type { QuickbaseProject } from '@/lib/types/project'

interface ActivityTabProps {
  project: QuickbaseProject
}

export function ActivityTab({ project }: ActivityTabProps) {
  return (
    <Card>
      <CardContent className="p-6">
        <ActivityFeed project={project} />
      </CardContent>
    </Card>
  )
}
```

---

## 5. Shared Components

### 5.1 TrafficLightPipeline

**File**: `components/projects/TrafficLightPipeline.tsx`

```typescript
'use client'

import { cn } from '@/lib/utils'
import { calculateMilestoneState, getMilestoneStatusText } from '@/lib/utils/traffic-lights'
import type { QuickbaseProject } from '@/lib/types/project'

type MilestoneState = 'complete' | 'in-progress' | 'pending' | 'on-hold' | 'overdue'

interface TrafficLightProps {
  project: QuickbaseProject
}

export function TrafficLightPipeline({ project }: TrafficLightProps) {
  const milestones = [
    { id: 'intake', label: 'Intake', icon: '📋' },
    { id: 'survey', label: 'Survey', icon: '📸' },
    { id: 'design', label: 'Design', icon: '📐' },
    { id: 'nem', label: 'NEM', icon: '🔌' },
    { id: 'permit', label: 'Permit', icon: '📋' },
    { id: 'install', label: 'Install', icon: '🔧' },
    { id: 'inspection', label: 'PTO', icon: '✅' },
  ]

  return (
    <div className="space-y-2">
      {/* Traffic Lights Row */}
      <div className="flex items-center gap-2">
        {milestones.map((milestone, index) => {
          const state = calculateMilestoneState(project, milestone.id)

          return (
            <div key={milestone.id} className="flex items-center gap-2">
              {/* Traffic Light Circle */}
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

              {/* Connector Line */}
              {index < milestones.length - 1 && (
                <div
                  className={cn(
                    'h-0.5 w-4',
                    state === 'complete' ? 'bg-green-500' : 'bg-gray-200'
                  )}
                />
              )}
            </div>
          )
        })}
      </div>

      {/* Status Text Below Lights */}
      <div className="text-xs text-gray-600">
        {getMilestoneStatusText(project)}
      </div>
    </div>
  )
}

function getTrafficLightClass(state: MilestoneState): string {
  switch (state) {
    case 'complete':
      return 'bg-green-500 text-white shadow-sm'
    case 'in-progress':
      return 'bg-amber-500 text-white shadow-lg'
    case 'pending':
      return 'bg-gray-200 text-gray-500'
    case 'on-hold':
      return 'bg-red-500 text-white'
    case 'overdue':
      return 'bg-red-600 text-white ring-2 ring-red-300'
    default:
      return 'bg-gray-200 text-gray-500'
  }
}
```

### 5.2 PPWDisplay

**File**: `components/projects/PPWDisplay.tsx`

```typescript
'use client'

import { ArrowDown, ArrowUp, Minus } from 'lucide-react'

interface PPWDisplayProps {
  soldPPW: number | null
  commissionablePPW: number | null
  expanded?: boolean
}

export function PPWDisplay({ soldPPW, commissionablePPW, expanded = false }: PPWDisplayProps) {
  if (!soldPPW && !commissionablePPW) {
    return <span className="text-sm text-gray-400">N/A</span>
  }

  // Calculate delta
  const delta = commissionablePPW && soldPPW ? commissionablePPW - soldPPW : 0

  const deltaColor = delta > 0 ? 'text-green-500' : delta < 0 ? 'text-red-500' : 'text-gray-500'

  const DeltaIcon = delta > 0 ? ArrowUp : delta < 0 ? ArrowDown : Minus

  if (expanded) {
    return (
      <div className="space-y-2">
        <div className="flex items-center justify-between">
          <span className="text-sm text-gray-500">Sold PPW:</span>
          <span className="font-bold text-lg text-gray-900">${soldPPW?.toFixed(2) || 'N/A'}</span>
        </div>
        {commissionablePPW && (
          <>
            <div className="flex items-center justify-between">
              <span className="text-sm text-gray-500">Your PPW:</span>
              <span className="font-bold text-lg text-gray-900">${commissionablePPW.toFixed(2)}</span>
            </div>
            {delta !== 0 && (
              <div className={`flex items-center justify-between ${deltaColor}`}>
                <span className="text-sm">Delta:</span>
                <span className="flex items-center gap-1 font-semibold">
                  <DeltaIcon className="h-4 w-4" />
                  ${Math.abs(delta).toFixed(2)}
                </span>
              </div>
            )}
          </>
        )}
      </div>
    )
  }

  return (
    <div className="flex flex-col items-end gap-0.5">
      {/* Sold PPW */}
      <div className="flex items-center gap-1">
        <span className="text-xs text-gray-500">Sold:</span>
        <span className="font-bold text-sm text-gray-900">${soldPPW?.toFixed(2) || 'N/A'}</span>
      </div>

      {/* Commissionable PPW with Delta */}
      {commissionablePPW && (
        <div className="flex items-center gap-1">
          <span className="text-xs text-gray-500">Yours:</span>
          <span className="font-bold text-sm text-gray-900">${commissionablePPW.toFixed(2)}</span>
          {delta !== 0 && (
            <span className={`flex items-center gap-0.5 text-xs ${deltaColor}`}>
              <DeltaIcon className="h-3 w-3" />
              ${Math.abs(delta).toFixed(2)}
            </span>
          )}
        </div>
      )}
    </div>
  )
}
```

### 5.3 HoldBanner

**File**: `components/projects/HoldBanner.tsx`

```typescript
'use client'

import { AlertTriangle, Clock } from 'lucide-react'
import { PROJECT_FIELDS } from '@/lib/constants/fieldIds'
import type { QuickbaseProject } from '@/lib/types/project'

interface HoldBannerProps {
  project: QuickbaseProject
  holdType: 'finance' | 'roof' | 'customer' | 'permit' | 'hoa' | 'generic'
}

export function HoldBanner({ project, holdType }: HoldBannerProps) {
  const holdReason =
    project[PROJECT_FIELDS.HOLD_REASON]?.value ||
    project[PROJECT_FIELDS.BLOCK_REASON]?.value ||
    'No reason specified'

  const dateOnHold = project[PROJECT_FIELDS.DATE_ON_HOLD]?.value
  const daysOnHold = calculateDaysOnHold(dateOnHold)

  const holdColors = {
    finance: 'bg-red-50 border-red-500 text-red-800',
    roof: 'bg-orange-50 border-orange-500 text-orange-800',
    customer: 'bg-amber-50 border-amber-500 text-amber-800',
    permit: 'bg-yellow-50 border-yellow-500 text-yellow-800',
    hoa: 'bg-lime-50 border-lime-500 text-lime-800',
    generic: 'bg-gray-50 border-gray-500 text-gray-800',
  }

  const holdLabels = {
    finance: 'Finance Hold',
    roof: 'Roof Hold',
    customer: 'Customer Hold',
    permit: 'Permit Hold',
    hoa: 'HOA Hold',
    generic: 'On Hold',
  }

  return (
    <div className={`rounded-t-lg border-l-4 p-3 ${holdColors[holdType]}`}>
      <div className="flex items-center justify-between">
        <div className="flex items-center gap-2">
          <AlertTriangle className="h-4 w-4" />
          <span className="font-semibold text-sm">{holdLabels[holdType]}</span>
          <span className="text-sm">• {holdReason}</span>
        </div>
        {daysOnHold > 0 && (
          <div className="flex items-center gap-1 text-sm">
            <Clock className="h-3 w-3" />
            <span>{daysOnHold} days</span>
          </div>
        )}
      </div>
    </div>
  )
}

function calculateDaysOnHold(dateOnHold?: string): number {
  if (!dateOnHold) return 0
  const holdDate = new Date(dateOnHold)
  const now = new Date()
  const diffTime = now.getTime() - holdDate.getTime()
  return Math.floor(diffTime / (1000 * 60 * 60 * 24))
}
```

### 5.4 ProjectAgeIndicator

**File**: `components/projects/ProjectAgeIndicator.tsx`

```typescript
'use client'

import { Calendar } from 'lucide-react'
import { cn } from '@/lib/utils'

interface ProjectAgeIndicatorProps {
  age: number // days
}

export function ProjectAgeIndicator({ age }: ProjectAgeIndicatorProps) {
  const getAgeColor = (days: number) => {
    if (days > 120) return 'text-red-500 font-bold'
    if (days > 90) return 'text-amber-500 font-semibold'
    return 'text-gray-500'
  }

  const getAgeLabel = (days: number) => {
    if (days > 120) return 'CRITICAL'
    if (days > 90) return 'WARNING'
    return ''
  }

  return (
    <div className="flex flex-col items-end gap-1">
      <div className={cn('flex items-center gap-1', getAgeColor(age))}>
        <Calendar className="h-3 w-3" />
        <span className="text-sm">{age}d</span>
      </div>
      {getAgeLabel(age) && (
        <span className={cn('text-xs font-bold', getAgeColor(age))}>{getAgeLabel(age)}</span>
      )}
    </div>
  )
}
```

---

## 6. Data Utilities

### 6.1 Traffic Light State Logic

**File**: `lib/utils/traffic-lights.ts`

```typescript
import { PROJECT_FIELDS } from '@/lib/constants/fieldIds'
import type { QuickbaseProject } from '@/lib/types/project'

export type MilestoneId = 'intake' | 'survey' | 'design' | 'nem' | 'permit' | 'install' | 'inspection'

export type MilestoneState = 'complete' | 'in-progress' | 'pending' | 'on-hold' | 'overdue'

/**
 * Calculate the state of each milestone
 */
export function calculateMilestoneState(
  project: QuickbaseProject,
  milestoneId: MilestoneId
): MilestoneState {
  const projectStatus = project[PROJECT_FIELDS.PROJECT_STATUS]?.value || ''
  const isOnHold = detectHoldStatus(projectStatus)

  // If project is on hold, mark current milestone as on-hold
  if (isOnHold) {
    const currentMilestone = getCurrentMilestoneId(project)
    if (currentMilestone === milestoneId) return 'on-hold'
  }

  switch (milestoneId) {
    case 'intake':
      return calculateIntakeState(project)
    case 'survey':
      return calculateSurveyState(project)
    case 'design':
      return calculateDesignState(project)
    case 'nem':
      return calculateNEMState(project)
    case 'permit':
      return calculatePermitState(project)
    case 'install':
      return calculateInstallState(project)
    case 'inspection':
      return calculateInspectionState(project)
    default:
      return 'pending'
  }
}

function calculateIntakeState(project: QuickbaseProject): MilestoneState {
  const surveyApproved = project[PROJECT_FIELDS.SURVEY_APPROVED]?.value
  const surveyScheduled = project[PROJECT_FIELDS.SURVEY_SCHEDULED_DATE]?.value

  if (surveyApproved) return 'complete'
  if (surveyScheduled) return 'in-progress'

  const projectAge = parseInt(project[PROJECT_FIELDS.PROJECT_AGE]?.value || '0')
  if (projectAge > 7) return 'overdue'

  return 'in-progress'
}

function calculateSurveyState(project: QuickbaseProject): MilestoneState {
  const surveyApproved = project[PROJECT_FIELDS.SURVEY_APPROVED]?.value
  const surveyCompleted = project[PROJECT_FIELDS.SURVEY_COMPLETED]?.value
  const surveyScheduled = project[PROJECT_FIELDS.SURVEY_SCHEDULED_DATE]?.value

  if (surveyApproved) return 'complete'
  if (surveyCompleted) return 'in-progress'
  if (surveyScheduled) return 'in-progress'

  return 'pending'
}

function calculateDesignState(project: QuickbaseProject): MilestoneState {
  const surveyApproved = project[PROJECT_FIELDS.SURVEY_APPROVED]?.value
  if (!surveyApproved) return 'pending'

  const cadApproved = project[PROJECT_FIELDS.CAD_DESIGN_APPROVED]?.value
  const designCompleted = project[PROJECT_FIELDS.DESIGN_COMPLETED]?.value

  if (cadApproved) return 'complete'
  if (designCompleted) return 'in-progress'

  const projectAge = parseInt(project[PROJECT_FIELDS.PROJECT_AGE]?.value || '0')
  if (projectAge > 21) return 'overdue'

  return 'in-progress'
}

function calculateNEMState(project: QuickbaseProject): MilestoneState {
  const cadApproved = project[PROJECT_FIELDS.CAD_DESIGN_APPROVED]?.value
  if (!cadApproved) return 'pending'

  const nemApproved = project[PROJECT_FIELDS.NEM_APPROVED]?.value
  const nemSubmitted = project[PROJECT_FIELDS.NEM_SUBMITTED]?.value
  const nemSubmitDate = project[PROJECT_FIELDS.NEM_SUBMITTED_DATE]?.value

  if (nemApproved) return 'complete'
  if (nemSubmitted || nemSubmitDate) return 'in-progress'

  return 'in-progress'
}

function calculatePermitState(project: QuickbaseProject): MilestoneState {
  const cadApproved = project[PROJECT_FIELDS.CAD_DESIGN_APPROVED]?.value
  if (!cadApproved) return 'pending'

  const permitApproved = project[PROJECT_FIELDS.PERMIT_APPROVED]?.value
  const permitSubmitted = project[PROJECT_FIELDS.PERMIT_SUBMITTED]?.value
  const permitSubmitDate = project[PROJECT_FIELDS.PERMIT_SUBMITTED_DATE]?.value

  if (permitApproved) return 'complete'
  if (permitSubmitted || permitSubmitDate) return 'in-progress'

  return 'in-progress'
}

function calculateInstallState(project: QuickbaseProject): MilestoneState {
  const nemApproved = project[PROJECT_FIELDS.NEM_APPROVED]?.value
  const permitApproved = project[PROJECT_FIELDS.PERMIT_APPROVED]?.value

  if (!nemApproved || !permitApproved) return 'pending'

  const installCompleted =
    project[PROJECT_FIELDS.INSTALL_COMPLETED_DATE]?.value ||
    project[PROJECT_FIELDS.INSTALLATION_COMPLETED]?.value ||
    project[PROJECT_FIELDS.INSTALL_COMPLETE_IMPORT]?.value

  if (installCompleted) return 'complete'

  const installStarted = project[PROJECT_FIELDS.INSTALL_STARTED_DATE]?.value
  if (installStarted) return 'in-progress'

  const installScheduled =
    project[PROJECT_FIELDS.INSTALL_SCHEDULED_DATE_CAPTURE]?.value ||
    project[PROJECT_FIELDS.INSTALL_SCHEDULED_START_DATE]?.value ||
    project[PROJECT_FIELDS.ESTIMATED_INSTALL_DATE]?.value ||
    project[PROJECT_FIELDS.INSTALL_DATE_IMPORT]?.value

  if (installScheduled) return 'in-progress'

  return 'in-progress'
}

function calculateInspectionState(project: QuickbaseProject): MilestoneState {
  const installCompleted =
    project[PROJECT_FIELDS.INSTALL_COMPLETED_DATE]?.value ||
    project[PROJECT_FIELDS.INSTALLATION_COMPLETED]?.value

  if (!installCompleted) return 'pending'

  const ptoApproved = project[PROJECT_FIELDS.PTO_APPROVED]?.value
  if (ptoApproved) return 'complete'

  const passingInspection = project[PROJECT_FIELDS.PASSING_INSPECTION_COMPLETED]?.value
  if (passingInspection) return 'in-progress'

  return 'in-progress'
}

function getCurrentMilestoneId(project: QuickbaseProject): MilestoneId {
  if (calculateInspectionState(project) === 'in-progress') return 'inspection'
  if (calculateInstallState(project) === 'in-progress') return 'install'
  if (calculatePermitState(project) === 'in-progress') return 'permit'
  if (calculateNEMState(project) === 'in-progress') return 'nem'
  if (calculateDesignState(project) === 'in-progress') return 'design'
  if (calculateSurveyState(project) === 'in-progress') return 'survey'
  return 'intake'
}

export function getMilestoneStatusText(project: QuickbaseProject): string {
  const currentMilestone = getCurrentMilestoneId(project)

  switch (currentMilestone) {
    case 'intake':
      return 'Intake: Scheduling survey'

    case 'survey':
      const surveyScheduled = project[PROJECT_FIELDS.SURVEY_SCHEDULED_DATE]?.value
      if (surveyScheduled) {
        return `Survey: Scheduled for ${formatDate(surveyScheduled)}`
      }
      return 'Survey: Awaiting schedule'

    case 'design':
      return 'Design: CAD in progress'

    case 'nem':
      const nemSubmitDate = project[PROJECT_FIELDS.NEM_SUBMITTED_DATE]?.value
      const nemDaysWaiting = calculateDaysWaiting(nemSubmitDate)
      if (nemSubmitDate && nemDaysWaiting > 0) {
        return `NEM: Submitted ${formatDate(nemSubmitDate)} • ${nemDaysWaiting}d waiting`
      }
      return 'NEM: Preparing submission'

    case 'permit':
      const permitSubmitDate = project[PROJECT_FIELDS.PERMIT_SUBMITTED_DATE]?.value
      const permitDaysWaiting = calculateDaysWaiting(permitSubmitDate)
      if (permitSubmitDate && permitDaysWaiting > 0) {
        return `Permit: Submitted ${formatDate(permitSubmitDate)} • ${permitDaysWaiting}d waiting`
      }
      return 'Permit: Preparing submission'

    case 'install':
      const installScheduled =
        project[PROJECT_FIELDS.INSTALL_SCHEDULED_DATE_CAPTURE]?.value ||
        project[PROJECT_FIELDS.INSTALL_SCHEDULED_START_DATE]?.value
      const installStarted = project[PROJECT_FIELDS.INSTALL_STARTED_DATE]?.value

      if (installStarted) {
        return `Install: Started ${formatDate(installStarted)}`
      }
      if (installScheduled) {
        return `Install: Scheduled for ${formatDate(installScheduled)}`
      }
      return 'Install: Ready to schedule'

    case 'inspection':
      const passingInspection = project[PROJECT_FIELDS.PASSING_INSPECTION_COMPLETED]?.value
      if (passingInspection) {
        return `Inspection: Passed ${formatDate(passingInspection)} • Awaiting PTO`
      }
      return 'Inspection: Pending'

    default:
      return ''
  }
}

function calculateDaysWaiting(submitDate?: string): number {
  if (!submitDate) return 0
  const submit = new Date(submitDate)
  const now = new Date()
  const diffTime = now.getTime() - submit.getTime()
  return Math.floor(diffTime / (1000 * 60 * 60 * 24))
}

function formatDate(dateStr: string): string {
  const date = new Date(dateStr)
  return `${date.getMonth() + 1}/${date.getDate()}`
}

function detectHoldStatus(status: string): boolean {
  const holdStatuses = ['On Hold', 'Finance Hold', 'Roof Hold', 'Customer Hold', 'Permit Hold', 'HOA Hold']
  return holdStatuses.some((hold) => status.includes(hold))
}
```

### 6.2 Hold Detection

**File**: `lib/utils/hold-detection.ts`

```typescript
export function detectHoldStatus(status: string): boolean {
  const holdStatuses = ['On Hold', 'Finance Hold', 'Roof Hold', 'Customer Hold', 'Permit Hold', 'HOA Hold']
  return holdStatuses.some((hold) => status.includes(hold))
}

export function extractHoldType(status: string): 'finance' | 'roof' | 'customer' | 'permit' | 'hoa' | 'generic' {
  if (status.includes('Finance Hold')) return 'finance'
  if (status.includes('Roof Hold')) return 'roof'
  if (status.includes('Customer Hold')) return 'customer'
  if (status.includes('Permit Hold')) return 'permit'
  if (status.includes('HOA Hold')) return 'hoa'
  return 'generic'
}
```

### 6.3 Project Helpers

**File**: `lib/utils/project-helpers.ts`

```typescript
import { PROJECT_FIELDS } from '@/lib/constants/fieldIds'
import type { QuickbaseProject } from '@/lib/types/project'

export function parseCustomerName(fullName: string): { firstName: string; lastName: string } {
  const parts = fullName.trim().split(' ')
  if (parts.length === 0) return { firstName: 'Unknown', lastName: '' }
  if (parts.length === 1) return { firstName: parts[0], lastName: '' }

  const lastName = parts[parts.length - 1]
  const firstName = parts.slice(0, -1).join(' ')

  return { firstName, lastName }
}

export function formatAddress(address: string): { line1: string; line2: string } {
  if (!address) return { line1: '', line2: '' }

  const parts = address.split(',').map((p) => p.trim())

  if (parts.length <= 1) {
    return { line1: parts[0] || '', line2: '' }
  }

  const line1 = parts[0]
  const line2 = parts.slice(1).join(', ')

  return { line1, line2 }
}

export function getProjectAge(project: QuickbaseProject): number {
  const ageField = project[PROJECT_FIELDS.PROJECT_AGE]?.value
  if (ageField) return parseInt(ageField)

  const salesDate = project[PROJECT_FIELDS.SALES_DATE]?.value
  if (!salesDate) return 0

  const sale = new Date(salesDate)
  const now = new Date()
  const diffTime = now.getTime() - sale.getTime()
  return Math.floor(diffTime / (1000 * 60 * 60 * 24))
}

export function getInstallDate(project: QuickbaseProject): {
  scheduled: string | null
  started: string | null
  completed: string | null
} {
  const scheduled =
    project[PROJECT_FIELDS.INSTALL_SCHEDULED_DATE_CAPTURE]?.value ||
    project[PROJECT_FIELDS.INSTALL_SCHEDULED_START_DATE]?.value ||
    project[PROJECT_FIELDS.ESTIMATED_INSTALL_DATE]?.value ||
    project[PROJECT_FIELDS.INSTALL_DATE_IMPORT]?.value ||
    null

  const started = project[PROJECT_FIELDS.INSTALL_STARTED_DATE]?.value || null

  const completed =
    project[PROJECT_FIELDS.INSTALL_COMPLETED_DATE]?.value ||
    project[PROJECT_FIELDS.INSTALLATION_COMPLETED]?.value ||
    project[PROJECT_FIELDS.INSTALL_COMPLETE_IMPORT]?.value ||
    null

  return { scheduled, started, completed }
}

export function getInstallRescheduleHistory(project: QuickbaseProject): Array<{
  date: string
  source: string
}> {
  const history: Array<{ date: string; source: string }> = []

  const fields = [
    { id: PROJECT_FIELDS.INSTALL_SCHEDULED_DATE_CAPTURE, label: 'Scheduled (Primary)' },
    { id: PROJECT_FIELDS.INSTALL_SCHEDULED_START_DATE, label: 'Scheduled (Start)' },
    { id: PROJECT_FIELDS.ESTIMATED_INSTALL_DATE, label: 'Estimated' },
    { id: PROJECT_FIELDS.INSTALL_DATE_IMPORT, label: 'Imported' },
  ]

  fields.forEach((field) => {
    const value = project[field.id]?.value
    if (value) {
      history.push({ date: value, source: field.label })
    }
  })

  return history.sort((a, b) => new Date(b.date).getTime() - new Date(a.date).getTime())
}
```

---

## 7. Responsive Layout

### 7.1 Breakpoints

```css
/* Desktop (> 1400px) */
@media (min-width: 1400px) {
  .project-row {
    grid-template-columns: 220px 1fr 160px 80px;
  }
}

/* iPad Pro 13" (1366 x 1024 landscape) */
@media (min-width: 1024px) and (max-width: 1399px) {
  .project-row {
    grid-template-columns: 200px 1fr 140px 70px;
  }
}

/* iPad Pro 11" (1194 x 834 landscape) */
@media (min-width: 834px) and (max-width: 1023px) {
  .project-row {
    grid-template-columns: 180px 1fr 130px 60px;
  }
}
```

---

## 8. Implementation Guide

### 8.1 File Creation Order

1. **Utilities** (no dependencies)
   - `lib/utils/hold-detection.ts`
   - `lib/utils/project-helpers.ts`
   - `lib/utils/traffic-lights.ts`

2. **Shared Components** (depend on utilities)
   - `components/projects/PPWDisplay.tsx`
   - `components/projects/ProjectAgeIndicator.tsx`
   - `components/projects/HoldBanner.tsx`
   - `components/projects/TrafficLightPipeline.tsx`

3. **List Components**
   - `components/projects/ProjectRow.tsx`
   - `components/projects/ProjectFilterChips.tsx`
   - `components/projects/SearchBar.tsx`
   - `components/projects/ProjectTableView.tsx`

4. **Dashboard Components**
   - `components/dashboard/DashboardMetrics.tsx`
   - `components/dashboard/UrgentAlerts.tsx`
   - `components/dashboard/RecentProjects.tsx`

5. **Detail Components**
   - `components/projects/detail/ProjectDetailHeader.tsx`
   - `components/projects/detail/tabs/*Tab.tsx`
   - `components/projects/detail/ProjectDetailTabs.tsx`

6. **Pages**
   - `app/(dashboard)/page.tsx` (Dashboard)
   - `app/(dashboard)/projects/page.tsx` (Projects List)
   - `app/(dashboard)/projects/[id]/page.tsx` (Project Detail)

### 8.2 Query Updates

Update `lib/quickbase/queries.ts` to add these new functions:

```typescript
export async function getDashboardMetrics(userId: string, role: string)
export async function getUrgentProjects(userId: string, role: string)
export async function getRecentProjects(userId: string, role: string)
export async function getProjectsForUser(userId: string, role: string, view: string, search?: string)
export async function getProjectById(recordId: string)
```

---

## 9. Acceptance Criteria

### Dashboard Page ✅
- [ ] 4 key metrics cards display correctly
- [ ] Urgent alerts show projects on hold > 7 days
- [ ] Recent projects widget shows 5 most recent
- [ ] Metrics update on data refresh

### Projects List Page ✅
- [ ] Horizontal traffic light layout matches mockup
- [ ] 7 traffic lights: Intake, Survey, Design, NEM, Permit, Install, Inspection
- [ ] Traffic light colors: Green (complete), Amber (in-progress), Gray (pending), Red (on-hold/overdue)
- [ ] In-progress lights pulse
- [ ] Status text shows current milestone details
- [ ] Customer names show full first and last name
- [ ] Addresses display on two lines
- [ ] Phone numbers are clickable tel: links
- [ ] PPW displays both Sold and Commissionable with delta
- [ ] Delta shows correct color (red/green/gray)
- [ ] Project age shows with color coding (gray/amber/red)
- [ ] Hold banner appears with correct color by type
- [ ] Filter chips work correctly (9 views)
- [ ] Search filters by name and project ID
- [ ] Responsive on iPad Pro 11" and 13"

### Project Detail Page ✅
- [ ] Hero header shows customer info and hold banner
- [ ] 6 tabs render correctly
- [ ] Overview tab shows expanded pipeline and quick stats
- [ ] Timeline tab shows all milestone dates
- [ ] Install reschedule history displays correctly
- [ ] System tab shows all technical details
- [ ] Financials tab shows pricing and PPW
- [ ] Documents tab ready for attachments
- [ ] Activity tab shows recent activity feed

---

**End of Specification**

This comprehensive specification covers Dashboard, Projects List, and Project Detail pages with all required functionality and UI polish for iPad Pro optimization.
