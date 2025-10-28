'use client';

import { Suspense, useEffect } from 'react'
import { useSession } from 'next-auth/react'
import { useQuery } from '@tanstack/react-query'
import { useRouter } from 'next/navigation'
import { notFound } from 'next/navigation'
import { ProjectHeader } from '@/components/projects/ProjectHeader'
import { ProjectDetailMobileHeader } from '@/components/projects/ProjectDetailMobileHeader'
import { ProjectDetailMobileLayout } from '@/components/projects/ProjectDetailMobileLayout'
import { CustomerContactCard } from '@/components/projects/CustomerContactCard'
import { SystemSpecsCard } from '@/components/projects/SystemSpecsCard'
import { TeamMembersCard } from '@/components/projects/TeamMembersCard'
import { PricingBreakdownCard } from '@/components/projects/PricingBreakdownCard'
import { Timeline } from '@/components/milestones/Timeline'
import { HoldManagementCard } from '@/components/projects/HoldManagementCard'
import { TaskSection } from '@/components/projects/TaskSection'
import { ProjectDetailSkeleton } from '@/components/projects/ProjectDetailSkeleton'
import { OperationsNotesSection } from '@/components/operations/OperationsNotesSection'
import { projectKey } from '@/lib/queryKeys'
import { getBaseUrl } from '@/lib/utils/baseUrl'

export default function OperationsProjectDetailPage({
  params,
}: {
  params: { id: string }
}) {
  const { data: session, status } = useSession()
  const router = useRouter()

  // Parse and validate project ID - keep as string for consistent query keys
  const projectId = params.id
  const projectIdNum = parseInt(projectId)
  if (isNaN(projectIdNum)) {
    notFound()
  }

  // Fetch project data with useQuery - MUST be called before any conditional returns
  const { data: project, isLoading, error } = useQuery({
    queryKey: projectKey(projectId),
    queryFn: async () => {
      const baseUrl = getBaseUrl()
      const response = await fetch(`${baseUrl}/api/projects/${projectId}`)
      if (!response.ok) {
        if (response.status === 404) throw new Error('NOT_FOUND')
        throw new Error('Failed to fetch project')
      }
      return response.json()
    },
    enabled: status === 'authenticated', // Only fetch when authenticated
    staleTime: 300000, // 5 minutes (matches list view cache)
    retry: (failureCount, error) => {
      // Disable retries for NOT_FOUND to avoid slow 404 handling and wasted requests
      if (error?.message === 'NOT_FOUND') {
        return false
      }
      // Keep up to 3 retries for other errors
      return failureCount < 3
    },
  })

  // Redirect if not authenticated - using useEffect to avoid router.push during render
  useEffect(() => {
    if (status === 'unauthenticated') {
      router.replace('/login')
    }
  }, [status, router])

  // Handle hash navigation (e.g., #tasks from notifications)
  useEffect(() => {
    if (typeof window === 'undefined') return;

    const hash = window.location.hash;
    if (hash) {
      // Wait for component to render
      const timeoutId = setTimeout(() => {
        const elementId = hash.replace('#', '');
        const element = document.getElementById(elementId);

        if (element) {
          element.scrollIntoView({ behavior: 'smooth', block: 'start' });

          // Optional: Add a highlight effect
          element.classList.add('ring-2', 'ring-blue-500', 'ring-offset-2');
          setTimeout(() => {
            element.classList.remove('ring-2', 'ring-blue-500', 'ring-offset-2');
          }, 2000);
        }
      }, 300); // Delay to ensure DOM is ready

      return () => clearTimeout(timeoutId);
    }
  }, [projectId, project]); // Re-run when project data loads

  // Handle loading and unauthenticated states
  if (isLoading || status === 'loading' || status === 'unauthenticated') {
    return <ProjectDetailSkeleton />
  }

  // Handle error states
  if (error?.message === 'NOT_FOUND') {
    notFound()
  }

  if (error) {
    return (
      <div className="min-h-screen bg-gray-50 p-6">
        <div className="max-w-7xl mx-auto">
          <div className="bg-white rounded-lg shadow-sm border p-6">
            <h2 className="text-xl font-semibold text-red-600 mb-2">Error Loading Project</h2>
            <p className="text-gray-600">{error.message}</p>
          </div>
        </div>
      </div>
    )
  }

  if (!project) {
    return <ProjectDetailSkeleton />
  }

  return (
    <div className="min-h-screen bg-gray-50">
      {/* Mobile Sticky Header - Only shows on mobile (< 640px) */}
      <ProjectDetailMobileHeader project={project} />

      {/* Mobile Layout - Accordion-based collapsible sections (< 640px) */}
      <ProjectDetailMobileLayout project={project} projectId={projectId} />

      {/* Desktop Layout - Traditional grid (>= 640px) */}
      <div className="hidden mobile:block max-w-7xl mx-auto space-y-6 p-6">
        {/* Project Header */}
        <ProjectHeader project={project} />

        {/* Main Content Grid */}
        <div className="grid grid-cols-1 lg:grid-cols-3 gap-6">
          {/* Left Column - 2/3 width */}
          <div className="lg:col-span-2 space-y-6">
            {/* Action Items */}
            <div id="tasks">
              <TaskSection projectId={projectId} />
            </div>

            {/* Customer Contact */}
            <CustomerContactCard project={project} />

            {/* System Specifications */}
            <SystemSpecsCard project={project} />

            {/* Project Timeline */}
            <div className="bg-white rounded-lg shadow-sm border p-6">
              <h2 className="text-xl font-semibold text-gray-900 mb-4">
                Project Timeline
              </h2>
              <Suspense fallback={<div className="h-32 bg-gray-100 rounded animate-pulse" />}>
                <Timeline project={project} />
              </Suspense>
            </div>

            {/* All Project Notes (Operations view shows ALL notes) */}
            <OperationsNotesSection projectId={projectId} />
          </div>

          {/* Right Column - 1/3 width */}
          <div className="space-y-6">
            {/* Team Members */}
            <TeamMembersCard project={project} />

            {/* Pricing & Adders */}
            <PricingBreakdownCard project={project} />

            {/* Hold Management */}
            <HoldManagementCard project={project} />
          </div>
        </div>
      </div>
    </div>
  )
}
