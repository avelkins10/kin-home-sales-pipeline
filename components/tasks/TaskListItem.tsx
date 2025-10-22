'use client'

import Link from 'next/link'
import { Clock, FileText, AlertTriangle, CheckCircle } from 'lucide-react'
import { Task } from '@/lib/types/task'
import { formatDaysAgo } from '@/lib/utils/formatters'
import { TaskStatusBadge } from '@/components/projects/TaskStatusBadge'
import { UrgencyBadge } from '@/components/ui/UrgencyBadge'
import { getTaskUrgency } from '@/lib/utils/task-urgency'
import { cn } from '@/lib/utils/cn'

interface TaskListItemProps {
  task: Task & {
    projectId: number
    projectName: string
    projectStatus: string
  }
  showProject?: boolean
}

export function TaskListItem({ task, showProject = true }: TaskListItemProps) {
  const urgency = getTaskUrgency(task, task.projectStatus)
  const hasSubmissions = task.submissions && task.submissions.length > 0
  const latestSubmission = hasSubmissions ? task.submissions[0] : null
  const needsRevision = latestSubmission?.opsDisposition === 'Needs Revision'

  return (
    <Link
      href={`/projects/${task.projectId}#tasks`}
      className={cn(
        'block bg-white border rounded-lg p-4 hover:shadow-md transition-all duration-200',
        urgency.level === 'critical' && 'border-l-4 border-l-red-500',
        urgency.level === 'urgent' && 'border-l-4 border-l-yellow-500'
      )}
    >
      {/* Header */}
      <div className="flex items-start justify-between gap-3 mb-2">
        <div className="flex-1 min-w-0">
          <h3 className="font-semibold text-gray-900 text-sm truncate">
            {task.name}
          </h3>
          {task.category && task.category !== task.name && (
            <p className="text-xs text-gray-500 mt-0.5">{task.category}</p>
          )}
        </div>
        <div className="flex items-center gap-2 flex-shrink-0">
          <UrgencyBadge
            level={urgency.level}
            daysWaiting={urgency.daysWaiting}
            reason={urgency.reason}
            size="small"
          />
          <TaskStatusBadge status={task.status} />
        </div>
      </div>

      {/* Project Context */}
      {showProject && (
        <div className="flex items-center gap-1.5 text-xs text-gray-600 mb-2">
          <FileText className="w-3.5 h-3.5" />
          <span>Project: {task.projectName}</span>
        </div>
      )}

      {/* Task Meta */}
      <div className="flex items-center gap-4 text-xs text-gray-500">
        <div className="flex items-center gap-1">
          <Clock className="w-3.5 h-3.5" />
          <span>{formatDaysAgo(task.dateCreated)}</span>
        </div>
        {hasSubmissions && (
          <div className="flex items-center gap-1">
            {needsRevision ? (
              <>
                <AlertTriangle className="w-3.5 h-3.5 text-orange-500" />
                <span className="text-orange-600 font-medium">Needs Revision</span>
              </>
            ) : (
              <>
                <CheckCircle className="w-3.5 h-3.5 text-green-500" />
                <span>{task.submissions.length} submission{task.submissions.length > 1 ? 's' : ''}</span>
              </>
            )}
          </div>
        )}
      </div>

      {/* Description preview */}
      {task.description && (
        <p className="text-xs text-gray-600 mt-2 line-clamp-2">
          {task.description}
        </p>
      )}

      {/* Ops Review Note */}
      {task.opsReviewNote && (
        <div className="mt-2 bg-purple-50 border border-purple-200 rounded p-2">
          <p className="text-xs text-purple-700 line-clamp-2">
            <span className="font-medium">Ops Note:</span> {task.opsReviewNote}
          </p>
        </div>
      )}
    </Link>
  )
}
