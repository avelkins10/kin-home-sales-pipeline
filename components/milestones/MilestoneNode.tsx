'use client'

import { AlertTriangle, Clock } from 'lucide-react'
import { formatDate } from '@/lib/utils/formatters'
import { cn } from '@/lib/utils/cn'
import type { MilestoneState } from '@/lib/utils/milestone-engine'

interface MilestoneNodeProps {
  milestone: {
    name: string
    status: MilestoneState
    date?: Date | null
    scheduledDate?: Date | null
    estimatedDate?: Date | null
    substeps?: Array<{ name: string; date: Date | null; state: string; }>
    warning?: string
    urgent?: boolean
    calculated?: boolean
    conditional?: boolean
    celebration?: boolean
    icon: string
    blockedReason?: string
  }
  isBlocked?: boolean
}

export function MilestoneNode({ milestone, isBlocked }: MilestoneNodeProps) {
  const getStatusColor = () => {
    // Overdue has highest priority for visual feedback
    if (milestone.status === 'overdue') {
      return 'bg-rose-600 border-rose-700'
    }
    if (isBlocked || milestone.status === 'blocked') {
      return 'bg-red-500 border-red-600'
    }
    if (milestone.status === 'complete') {
      return 'bg-green-500 border-green-600'
    }
    if (milestone.status === 'in-progress') {
      return 'bg-yellow-500 border-yellow-600'
    }
    if (milestone.status === 'ready-for') {
      return 'bg-blue-500 border-blue-600'
    }
    if (milestone.status === 'not-applicable') {
      return 'bg-gray-200 border-gray-300'
    }
    return 'bg-gray-300 border-gray-400'
  }

  const getStatusLabel = () => {
    if (milestone.status === 'overdue') return 'Overdue'
    if (isBlocked || milestone.status === 'blocked') return milestone.blockedReason || 'Blocked'
    if (milestone.status === 'complete') return 'Complete'
    if (milestone.status === 'in-progress') return 'In Progress'
    if (milestone.status === 'ready-for') return `Ready for ${milestone.name}`
    if (milestone.status === 'not-applicable') return 'N/A'
    return 'Pending'
  }

  const statusColor = getStatusColor()
  const statusLabel = getStatusLabel()

  return (
    <div className="flex flex-col items-center min-w-[120px]">
      {/* Circle Node */}
      <div className="relative">
        <div
          className={cn(
            'w-16 h-16 rounded-full border-4 flex items-center justify-center text-2xl',
            statusColor,
            milestone.urgent && 'animate-pulse'
          )}
        >
          {milestone.icon}
        </div>
        
        {/* Urgent Indicator */}
        {milestone.urgent && (
          <div className="absolute -top-1 -right-1 w-6 h-6 bg-red-500 rounded-full flex items-center justify-center">
            <AlertTriangle className="w-3 h-3 text-white" />
          </div>
        )}
      </div>

      {/* Text Section */}
      <div className="mt-3 text-center">
        <h3 className="font-semibold text-gray-900 text-sm">{milestone.name}</h3>
        <p className="text-xs text-gray-500 mt-1">
          {statusLabel}
          {milestone.calculated && ' (Est.)'}
          {milestone.conditional && ' (If Req.)'}
        </p>
      </div>

      {/* Date Display */}
      <div className="mt-1 text-center">
        {milestone.date ? (
          <p className="text-xs text-gray-600">
            {formatDate(milestone.date)}
          </p>
        ) : milestone.scheduledDate ? (
          <p className="text-xs text-blue-600 flex items-center justify-center gap-1">
            <Clock className="w-3 h-3" />
            {formatDate(milestone.scheduledDate)}
          </p>
        ) : milestone.estimatedDate ? (
          <p className="text-xs text-gray-400">
            Est. {formatDate(milestone.estimatedDate)}
          </p>
        ) : milestone.status !== 'complete' ? (
          <p className="text-xs text-gray-400">Pending</p>
        ) : null}
      </div>

      {/* Warning Box */}
      {milestone.warning && (
        <div className="mt-2 max-w-[200px]">
          <div className="bg-orange-50 border border-orange-200 rounded px-2 py-1">
            <p className="text-xs text-orange-700">{milestone.warning}</p>
          </div>
        </div>
      )}

      {/* Substeps List */}
      {milestone.substeps && milestone.substeps.length > 0 && (
        <div className="mt-3 space-y-1">
          {milestone.substeps.map((substep, index) => (
            <div key={index} className="flex items-center gap-2">
              <div
                className={cn(
                  'w-2 h-2 rounded-full',
                  substep.state === 'complete' ? 'bg-green-500' :
                  substep.state === 'pending' ? 'bg-gray-300' : 'bg-yellow-500'
                )}
              />
              <div className="text-xs text-gray-700">
                <span>{substep.name}</span>
                {substep.date && (
                  <span className="text-gray-500 ml-1">({formatDate(substep.date)})</span>
                )}
              </div>
            </div>
          ))}
        </div>
      )}
    </div>
  )
}
