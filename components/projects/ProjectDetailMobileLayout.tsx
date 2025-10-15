'use client'

import { Suspense } from 'react'
import { AccordionMobile, AccordionSection } from '@/components/ui/accordion-mobile'
import { CustomerContactCard } from '@/components/projects/CustomerContactCard'
import { SystemSpecsCard } from '@/components/projects/SystemSpecsCard'
import { TeamMembersCard } from '@/components/projects/TeamMembersCard'
import { PricingBreakdownCard } from '@/components/projects/PricingBreakdownCard'
import { HoldManagementCard } from '@/components/projects/HoldManagementCard'
import { Timeline } from '@/components/milestones/Timeline'
import { ProjectCommunicationTabs } from '@/components/projects/ProjectCommunicationTabs'
import { QuickbaseProject } from '@/lib/types/project'
import { isProjectOnHold } from '@/lib/utils/milestone-engine'
import { Badge } from '@/components/ui/badge'
import { useIsMobile } from '@/lib/hooks/useMediaQuery'

interface ProjectDetailMobileLayoutProps {
  project: QuickbaseProject
  projectId: string
  // Future: pass unread counts for smart badges
  unreadNotesCount?: number
  unreadMessagesCount?: number
}

/**
 * Mobile-optimized layout for project details page
 * Features:
 * - Accordion sections to reduce vertical scrolling
 * - Smart section ordering (most important first)
 * - Auto-expand based on project state
 * - Compact card styling
 * - Only renders on mobile (< 640px)
 */
export function ProjectDetailMobileLayout({
  project,
  projectId,
  unreadNotesCount = 0,
  unreadMessagesCount = 0
}: ProjectDetailMobileLayoutProps) {
  const isMobile = useIsMobile()
  const onHold = isProjectOnHold(project)
  const hasUnreadComms = unreadNotesCount > 0 || unreadMessagesCount > 0

  // Don't render on desktop
  if (!isMobile) {
    return null
  }

  // Build accordion sections with smart defaults
  const sections: AccordionSection[] = [
    // 1. Timeline - Always visible (highest priority)
    {
      id: 'timeline',
      title: 'Project Timeline',
      defaultOpen: true, // Always open by default
      children: (
        <Suspense fallback={<div className="h-32 bg-gray-100 rounded animate-pulse" />}>
          <Timeline project={project} />
        </Suspense>
      )
    },

    // 2. Hold Management - Only show if on hold (high priority when applicable)
    ...(onHold ? [{
      id: 'hold',
      title: 'Hold Management',
      defaultOpen: true, // Auto-expand if on hold
      badge: (
        <Badge variant="destructive" className="text-xs">
          On Hold
        </Badge>
      ),
      children: <HoldManagementCard project={project} />
    }] : []),

    // 3. Communications - High priority, auto-expand if unread
    {
      id: 'communications',
      title: 'Notes & Messages',
      defaultOpen: hasUnreadComms, // Auto-expand if unread
      badge: hasUnreadComms ? (
        <Badge variant="default" className="text-xs bg-blue-600">
          {unreadNotesCount + unreadMessagesCount}
        </Badge>
      ) : undefined,
      children: <ProjectCommunicationTabs projectId={projectId} />
    },

    // 4. Customer Contact - Important for quick reference
    {
      id: 'contact',
      title: 'Customer Contact',
      defaultOpen: false,
      children: <CustomerContactCard project={project} />
    },

    // 5. Team Members - Medium priority
    {
      id: 'team',
      title: 'Team Members',
      defaultOpen: false,
      children: <TeamMembersCard project={project} />
    },

    // 6. System Specs - Lower priority (details)
    {
      id: 'specs',
      title: 'System Specifications',
      defaultOpen: false,
      children: <SystemSpecsCard project={project} />
    },

    // 7. Pricing & Adders - Lower priority (details)
    {
      id: 'pricing',
      title: 'Pricing & Adders',
      defaultOpen: false,
      children: <PricingBreakdownCard project={project} />
    },

    // 8. Hold Management - If NOT on hold, show at bottom
    ...(!onHold ? [{
      id: 'hold',
      title: 'Hold Management',
      defaultOpen: false,
      children: <HoldManagementCard project={project} />
    }] : [])
  ]

  return (
    <div className="pb-6">
      <AccordionMobile
        sections={sections}
        persistKey={`project-${projectId}`}
        className="px-3"
      />
    </div>
  )
}
