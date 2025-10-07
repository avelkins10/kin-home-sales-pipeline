'use client'

import { useQuery } from '@tanstack/react-query'
// fetching via API route
import { AlertTriangle, Clock, ArrowRight } from 'lucide-react'
import Link from 'next/link'
import { getBaseUrl } from '@/lib/utils/baseUrl'

interface UrgentAlertsProps {
  userId: string
  role: string
}

export function UrgentAlerts({ userId, role }: UrgentAlertsProps) {
  const { data: urgentProjects } = useQuery({
    queryKey: ['urgent-projects', userId, role],
    queryFn: async () => {
      const response = await fetch(`${getBaseUrl()}/api/dashboard/urgent`)
      if (!response.ok) throw new Error('Failed to fetch urgent projects')
      return response.json()
    },
  })

  if (!urgentProjects || urgentProjects.length === 0) {
    return null
  }

  return (
    <div className="bg-red-50 border-l-4 border-red-500 rounded-lg p-4">
      <div className="flex items-center gap-2 mb-2">
        <AlertTriangle className="h-5 w-5 text-red-600" />
        <h3 className="font-semibold text-red-900">Urgent Attention Required</h3>
      </div>
      <p className="text-sm text-red-700 mb-3">
        {urgentProjects.length} project(s) on hold for more than 7 days
      </p>
      <div className="space-y-2">
        {urgentProjects.slice(0, 3).map((project: any) => (
          <Link
            key={project.recordId}
            href={`/projects/${project.recordId}`}
            className="block p-2 bg-white rounded hover:bg-red-50 transition-colors"
          >
            <div className="flex items-center justify-between">
              <div className="flex-1">
                <p className="text-sm font-medium text-gray-900">{project.customerName}</p>
                <div className="flex items-center gap-1 mt-1">
                  <Clock className="h-3 w-3 text-red-600" />
                  <span className="text-xs text-red-600">{project.daysOnHold} days on hold</span>
                </div>
                <p className="text-xs text-gray-600 mt-1">{project.holdReason}</p>
              </div>
              <ArrowRight className="h-4 w-4 text-gray-400" />
            </div>
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
  )
}
