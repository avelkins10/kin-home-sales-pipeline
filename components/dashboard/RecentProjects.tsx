'use client'

import { useQuery } from '@tanstack/react-query'
// fetching via API route
import { getCurrentMilestone, getMilestoneStatus } from '@/lib/utils/milestone-engine'
import { useMilestoneConfig } from '@/lib/hooks/useMilestoneConfig'
import { PROJECT_FIELDS } from '@/lib/constants/fieldIds'
import { Card, CardHeader, CardTitle, CardContent } from '@/components/ui/card'
import { Badge } from '@/components/ui/badge'
import { Skeleton } from '@/components/ui/skeleton'
import { ArrowRight } from 'lucide-react'
import Link from 'next/link'
import type { QuickbaseProject } from '@/lib/types/project'
import { getBaseUrl } from '@/lib/utils/baseUrl'

interface RecentProjectsProps {
  userId: string
  role: string
}

export function RecentProjects({ userId, role }: RecentProjectsProps) {
  // Fetch dynamic milestone configuration
  const { config } = useMilestoneConfig()

  const { data: projects, isLoading, error } = useQuery({
    queryKey: ['recent-projects', userId, role],
    queryFn: async () => {
      const response = await fetch(`${getBaseUrl()}/api/dashboard/recent`)
      if (!response.ok) throw new Error('Failed to fetch recent projects')
      return response.json()
    },
  })

  if (isLoading) {
    return (
      <Card>
        <CardHeader>
          <CardTitle>Recent Projects</CardTitle>
        </CardHeader>
        <CardContent>
          <div className="space-y-3">
            {Array.from({ length: 3 }).map((_, i) => (
              <div key={i} className="flex items-center justify-between p-3">
                <div className="space-y-2">
                  <Skeleton className="h-4 w-48" />
                  <Skeleton className="h-3 w-24" />
                </div>
                <div className="flex items-center gap-2">
                  <Skeleton className="h-6 w-16" />
                  <Skeleton className="h-4 w-4" />
                </div>
              </div>
            ))}
          </div>
        </CardContent>
      </Card>
    )
  }

  if (error) {
    return (
      <Card>
        <CardHeader>
          <CardTitle>Recent Projects</CardTitle>
        </CardHeader>
        <CardContent>
          <p className="text-red-600">Failed to load recent projects</p>
        </CardContent>
      </Card>
    )
  }

  if (!projects || projects.length === 0) {
    return (
      <Card>
        <CardHeader>
          <CardTitle>Recent Projects</CardTitle>
        </CardHeader>
        <CardContent>
          <p className="text-gray-600">No recent projects to display.</p>
        </CardContent>
      </Card>
    )
  }


  return (
    <Card>
      <CardHeader>
        <div className="flex items-center justify-between">
          <CardTitle>Recent Projects</CardTitle>
          <Link 
            href="/projects" 
            className="text-sm text-blue-600 hover:text-blue-700 font-medium"
          >
            View all →
          </Link>
        </div>
      </CardHeader>
      <CardContent>
        <div className="space-y-3">
          {projects.map((project: QuickbaseProject) => {
            const recordId = project[PROJECT_FIELDS.RECORD_ID]?.value
            const customerName = project[PROJECT_FIELDS.CUSTOMER_NAME]?.value || 'Unknown Customer'
            const projectId = recordId || 'N/A' // PROJECT_ID same as RECORD_ID
            const currentMilestoneId = getCurrentMilestone(project, config)
            const milestoneStatus = getMilestoneStatus(project, currentMilestoneId, config)
            const milestone = milestoneStatus.name

            return (
              <Link
                key={recordId}
                href={`/projects/${recordId}`}
                className="block p-3 rounded-lg hover:bg-gray-50 transition-colors"
              >
                <div className="flex items-center justify-between">
                  <div>
                    <p className="font-medium text-gray-900">{customerName}</p>
                    <p className="text-sm text-gray-500 mt-0.5">Project ID: {projectId}</p>
                  </div>
                  <div className="flex items-center gap-2">
                    <Badge variant="secondary">{milestone}</Badge>
                    <ArrowRight className="h-4 w-4 text-gray-400" />
                  </div>
                </div>
              </Link>
            )
          })}
        </div>
      </CardContent>
    </Card>
  )
}
