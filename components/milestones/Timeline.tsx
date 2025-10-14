'use client'

import { MilestoneNode } from './MilestoneNode'
import { MilestoneConnector } from './MilestoneConnector'
import { getAllMilestoneStatuses, isProjectOnHold } from '@/lib/utils/milestone-engine'
import { useMilestoneConfig } from '@/lib/hooks/useMilestoneConfig'
import { QuickbaseProject } from '@/lib/types/project'

interface TimelineProps {
  project: QuickbaseProject
}

// Milestone icon and color mapping (7-milestone system)
const milestoneConfig: Record<string, { icon: string; color: string; calculated?: boolean }> = {
  intake: { icon: '📋', color: 'gray' },
  survey: { icon: '📐', color: 'blue' },
  design: { icon: '🎨', color: 'purple' },
  permitting: { icon: '📄', color: 'indigo' }, // Combines HOA, NEM, and Permit
  install: { icon: '🔧', color: 'green' },
  inspection: { icon: '🔍', color: 'teal' },
  pto: { icon: '🎉', color: 'emerald' }
}

export function Timeline({ project }: TimelineProps) {
  // Fetch dynamic milestone configuration
  const { config } = useMilestoneConfig()

  // Check if project is on hold
  const onHold = isProjectOnHold(project)

  // Get all milestone statuses from the engine
  const allStatuses = getAllMilestoneStatuses(project, config)

  // Build milestones array, filtering out not-applicable ones
  const milestones = allStatuses
    .filter(status => status.state !== 'not-applicable')
    .map(status => {
      const config = milestoneConfig[status.id] || { icon: '•', color: 'gray' }

      return {
        name: status.name,
        icon: config.icon,
        color: config.color,
        calculated: config.calculated,
        status: status.state,
        date: status.date,
        scheduledDate: status.scheduledDate,
        estimatedDate: status.estimatedDate,
        substeps: status.substeps,
        blockedReason: status.blockedReason,
        urgency: status.urgency
      }
    })

  return (
    <div className="space-y-6" data-testid="timeline">
      {/* Timeline */}
      <div className="relative">
        <div className="overflow-x-auto pb-4">
          <div className="flex items-start space-x-4 min-w-max">
            {milestones.map((milestone, index) => (
              <div key={milestone.name} className="flex items-start">
                <MilestoneNode
                  milestone={milestone}
                  isBlocked={onHold && milestone.status === 'in-progress'}
                />
                {index < milestones.length - 1 && (
                  <MilestoneConnector
                    status={milestone.status === 'complete' ? 'complete' : 'pending'}
                  />
                )}
              </div>
            ))}
          </div>
        </div>
      </div>

      {/* Legend */}
      <div className="border-t border-gray-200 pt-4">
        <div className="flex flex-wrap justify-center items-center gap-x-6 gap-y-2">
          <div className="flex items-center gap-2">
            <div className="w-3 h-3 bg-green-500 rounded-full" />
            <span className="text-sm text-gray-600">Complete</span>
          </div>
          <div className="flex items-center gap-2">
            <div className="w-3 h-3 bg-yellow-500 rounded-full" />
            <span className="text-sm text-gray-600">In Progress</span>
          </div>
          <div className="flex items-center gap-2">
            <div className="w-3 h-3 bg-blue-500 rounded-full" />
            <span className="text-sm text-gray-600">Ready to Start</span>
          </div>
          <div className="flex items-center gap-2">
            <div className="w-3 h-3 bg-gray-300 rounded-full" />
            <span className="text-sm text-gray-600">Pending</span>
          </div>
          <div className="flex items-center gap-2">
            <div className="w-3 h-3 bg-red-500 rounded-full" />
            <span className="text-sm text-gray-600">Blocked</span>
          </div>
          <div className="flex items-center gap-2">
            <div className="w-3 h-3 bg-rose-600 rounded-full" />
            <span className="text-sm text-gray-600">Overdue</span>
          </div>
        </div>
      </div>
    </div>
  )
}
