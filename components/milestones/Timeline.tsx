'use client'

import { MilestoneNode } from './MilestoneNode'
import { MilestoneConnector } from './MilestoneConnector'
import { PROJECT_FIELDS } from '@/lib/constants/fieldIds'
import {
  getSurveyStatus,
  getDesignStatus,
  getHOAStatus,
  getPermitStatus,
  getNEMStatus,
  getInstallStatus,
  getVerificationStatus,
  getInspectionStatus,
  getPTOStatus
} from '@/lib/utils/milestoneStatus'
import { QuickbaseProject } from '@/lib/types/project'

interface TimelineProps {
  project: QuickbaseProject
}

export function Timeline({ project }: TimelineProps) {
  // Check if project is on hold
  const onHold = project[PROJECT_FIELDS.ON_HOLD]?.value === 'Yes'

  // Check if project is rejected or cancelled
  const projectStatus = (project[PROJECT_FIELDS.PROJECT_STATUS]?.value || '').toLowerCase()
  const isRejected = projectStatus.includes('reject')
  const isCancelled = projectStatus.includes('cancel')
  const isFailedProject = isRejected || isCancelled

  // Get HOA status to determine if HOA milestone should be shown
  const hoaStatus = getHOAStatus(project)

  // Build milestones array
  const milestones = [
    {
      name: 'Intake',
      icon: '📋',
      color: 'gray',
      // For rejected/cancelled projects, intake was rejected (not approved)
      status: isFailedProject ? ('rejected' as const) : ('complete' as const),
      date: isFailedProject
        ? (project[PROJECT_FIELDS.SALES_DATE]?.value ? new Date(project[PROJECT_FIELDS.SALES_DATE].value) : undefined)
        : (project[PROJECT_FIELDS.FINANCE_INTAKE_APPROVED_DATE]?.value ?
            new Date(project[PROJECT_FIELDS.FINANCE_INTAKE_APPROVED_DATE].value) :
            (project[PROJECT_FIELDS.SALES_DATE]?.value ? new Date(project[PROJECT_FIELDS.SALES_DATE].value) : undefined))
    },
    {
      name: 'Survey',
      icon: '📐',
      color: 'blue',
      ...getSurveyStatus(project)
    },
    {
      name: 'Design',
      icon: '🎨',
      color: 'purple',
      ...getDesignStatus(project)
    },
    // HOA milestone - only include if project requires HOA
    ...(hoaStatus.hasHOA ? [{
      name: 'HOA',
      icon: '🏘️',
      color: 'orange',
      conditional: true,
      ...hoaStatus
    }] : []),
    {
      name: 'NEM',
      icon: '⚡',
      color: 'yellow',
      ...getNEMStatus(project)
    },
    {
      name: 'Permit',
      icon: '📄',
      color: 'indigo',
      ...getPermitStatus(project)
    },
    {
      name: 'Install',
      icon: '🔧',
      color: 'green',
      ...getInstallStatus(project)
    },
    {
      name: 'Verification',
      icon: '✅',
      color: 'cyan',
      calculated: true,
      ...getVerificationStatus(project)
    },
    {
      name: 'Inspection',
      icon: '🔍',
      color: 'teal',
      ...getInspectionStatus(project)
    },
    {
      name: 'PTO',
      icon: '🎉',
      color: 'emerald',
      ...getPTOStatus(project)
    }
  ]

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
        <div className="flex justify-center items-center space-x-6">
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
            <span className="text-sm text-gray-600">Upcoming</span>
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
            <div className="w-3 h-3 bg-rose-500 rounded-full" />
            <span className="text-sm text-gray-600">Rejected</span>
          </div>
        </div>
      </div>
    </div>
  )
}
