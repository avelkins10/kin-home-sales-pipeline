'use client'

import { Task } from '@/lib/types/task'
import { TaskListItem } from './TaskListItem'
import { PendingCancelTaskCard } from './PendingCancelTaskCard'
import { AlertTriangle, CheckCircle, Clock, ChevronDown, ChevronUp } from 'lucide-react'
import { sortTasksByUrgency } from '@/lib/utils/task-urgency'

interface TaskListProps {
  tasks: Array<Task & {
    projectId: number
    projectName: string
    projectStatus: string
    closerName?: string | null
    salesOffice?: string | null
    isPendingCancel?: boolean
  }>
  groupBy?: 'none' | 'project' | 'urgency' | 'office'
  expandedOffices?: Set<string>
  toggleOffice?: (officeName: string) => void
  showOnlyPendingCancel?: boolean // If true, only show pending cancel cards, filter out regular tasks
}

// Helper: Sort tasks by date (newest first)
function sortByDate(tasks: any[]) {
  return [...tasks].sort((a, b) => {
    const dateA = a.dateCreated ? new Date(a.dateCreated).getTime() : 0
    const dateB = b.dateCreated ? new Date(b.dateCreated).getTime() : 0
    return dateB - dateA // Newest first
  })
}

export function TaskList({ tasks, groupBy = 'none', expandedOffices, toggleOffice, showOnlyPendingCancel = false }: TaskListProps) {
  // If showing only pending cancel, filter to only pending cancel tasks and deduplicate by project
  let displayTasks = tasks
  if (showOnlyPendingCancel) {
    // Filter to only pending cancel tasks
    const pendingCancelTasks = tasks.filter((t: any) => t.isPendingCancel)
    
    // Deduplicate by projectId - show only one card per project
    const projectsSeen = new Set<number>()
    displayTasks = pendingCancelTasks.filter((task: any) => {
      if (projectsSeen.has(task.projectId)) {
        return false // Already showed this project
      }
      projectsSeen.add(task.projectId)
      return true
    })
  }

  if (displayTasks.length === 0) {
    return (
      <div className="text-center py-12 bg-white rounded-lg border">
        <CheckCircle className="w-12 h-12 text-green-500 mx-auto mb-3" />
        <h3 className="text-lg font-semibold text-gray-900 mb-1">
          {showOnlyPendingCancel ? 'No Pending Cancel Projects' : 'No Tasks Pending'}
        </h3>
        <p className="text-sm text-gray-500">
          {showOnlyPendingCancel 
            ? 'All projects are active or cancelled. No pending cancellation actions needed.'
            : 'All your tasks are complete. Great work!'
          }
        </p>
      </div>
    )
  }

  // Group tasks if needed
  if (groupBy === 'project') {
    const tasksByProject = displayTasks.reduce((acc, task) => {
      const projectId = task.projectId
      if (!acc[projectId]) {
        acc[projectId] = {
          projectName: task.projectName,
          tasks: []
        }
      }
      acc[projectId].tasks.push(task)
      return acc
    }, {} as Record<number, { projectName: string; tasks: typeof tasks }>)

    return (
      <div className="space-y-6">
        {Object.entries(tasksByProject).map(([projectId, { projectName, tasks: projectTasks }]) => (
          <div key={projectId}>
            <h3 className="text-sm font-semibold text-gray-900 mb-3 flex items-center gap-2">
              <span>{projectName}</span>
              <span className="text-xs font-normal text-gray-500">
                ({projectTasks.length} {projectTasks.length === 1 ? 'task' : 'tasks'})
              </span>
            </h3>
            <div className="space-y-3">
              {sortByDate(projectTasks).map((task: any) => (
                task.isPendingCancel ? (
                  <PendingCancelTaskCard key={task.recordId} task={task} />
                ) : (
                  <TaskListItem key={task.recordId} task={task} showProject={false} />
                )
              ))}
            </div>
          </div>
        ))}
      </div>
    )
  }

  if (groupBy === 'office') {
    const tasksByOffice = displayTasks.reduce((acc, task) => {
      const officeName = task.salesOffice || 'No Office'
      if (!acc[officeName]) {
        acc[officeName] = []
      }
      acc[officeName].push(task)
      return acc
    }, {} as Record<string, typeof tasks>)

    // Sort offices alphabetically
    const sortedOffices = Object.keys(tasksByOffice).sort()

    return (
      <div className="space-y-4">
        {sortedOffices.map((officeName) => {
          const officeTasks = tasksByOffice[officeName]
          const isExpanded = expandedOffices?.has(officeName) ?? true
          return (
            <div key={officeName} className="bg-white rounded-lg border">
              <button
                onClick={() => toggleOffice?.(officeName)}
                className="w-full px-4 py-3 flex items-center justify-between hover:bg-gray-50 transition-colors"
              >
                <div className="flex items-center gap-2">
                  <h3 className="text-sm font-semibold text-gray-900">
                    {officeName}
                  </h3>
                  <span className="text-xs font-normal text-gray-500">
                    ({officeTasks.length} {officeTasks.length === 1 ? 'task' : 'tasks'})
                  </span>
                </div>
                {isExpanded ? (
                  <ChevronUp className="w-5 h-5 text-gray-500" />
                ) : (
                  <ChevronDown className="w-5 h-5 text-gray-500" />
                )}
              </button>
              {isExpanded && (
                <div className="px-4 pb-4 space-y-3">
                  {sortByDate(officeTasks).map((task: any) => (
                    task.isPendingCancel ? (
                      <PendingCancelTaskCard key={task.recordId} task={task} />
                    ) : (
                      <TaskListItem key={task.recordId} task={task} />
                    )
                  ))}
                </div>
              )}
            </div>
          )
        })}
      </div>
    )
  }

  if (groupBy === 'urgency') {
    const sortedTasks = sortTasksByUrgency(displayTasks)
    const critical = sortByDate(sortedTasks.filter(t => {
      // Exclude approved tasks from critical - they're completed
      const taskStatus = (t.status || '').toLowerCase().trim()
      if (taskStatus === 'approved') {
        return false
      }
      const daysWaiting = t.dateCreated ? Math.ceil((Date.now() - new Date(t.dateCreated).getTime()) / (1000 * 60 * 60 * 24)) : 0
      const isRejected = typeof t.projectStatus === 'string' && t.projectStatus.toLowerCase().includes('reject')
      return daysWaiting > 7 || isRejected
    }))
    const urgent = sortByDate(sortedTasks.filter(t => {
      // Exclude approved tasks from urgent - they're completed
      const taskStatus = (t.status || '').toLowerCase().trim()
      if (taskStatus === 'approved') {
        return false
      }
      const daysWaiting = t.dateCreated ? Math.ceil((Date.now() - new Date(t.dateCreated).getTime()) / (1000 * 60 * 60 * 24)) : 0
      const hasRevision = t.submissions?.[0]?.opsDisposition === 'Needs Revision'
      return (daysWaiting >= 3 && daysWaiting <= 7) || hasRevision
    }).filter(t => !critical.includes(t)))
    const normal = sortByDate(sortedTasks.filter(t => !critical.includes(t) && !urgent.includes(t)))

    return (
      <div className="space-y-6">
        {critical.length > 0 && (
          <div>
            <h3 className="text-sm font-semibold text-red-700 mb-3 flex items-center gap-2">
              <AlertTriangle className="w-4 h-4" />
              <span>Critical ({critical.length})</span>
            </h3>
            <div className="space-y-3">
              {critical.map((task: any) => (
                task.isPendingCancel ? (
                  <PendingCancelTaskCard key={task.recordId} task={task} />
                ) : (
                  <TaskListItem key={task.recordId} task={task} />
                )
              ))}
            </div>
          </div>
        )}

        {urgent.length > 0 && (
          <div>
            <h3 className="text-sm font-semibold text-yellow-700 mb-3 flex items-center gap-2">
              <Clock className="w-4 h-4" />
              <span>Urgent ({urgent.length})</span>
            </h3>
            <div className="space-y-3">
              {urgent.map((task: any) => (
                task.isPendingCancel ? (
                  <PendingCancelTaskCard key={task.recordId} task={task} />
                ) : (
                  <TaskListItem key={task.recordId} task={task} />
                )
              ))}
            </div>
          </div>
        )}

        {normal.length > 0 && (
          <div>
            <h3 className="text-sm font-semibold text-gray-700 mb-3 flex items-center gap-2">
              <CheckCircle className="w-4 h-4" />
              <span>Normal ({normal.length})</span>
            </h3>
            <div className="space-y-3">
              {normal.map((task: any) => (
                task.isPendingCancel ? (
                  <PendingCancelTaskCard key={task.recordId} task={task} />
                ) : (
                  <TaskListItem key={task.recordId} task={task} />
                )
              ))}
            </div>
          </div>
        )}
      </div>
    )
  }

  // No grouping - sort by date first
  const sortedTasks = sortByDate(displayTasks)
  
  // If showing only pending cancel, render them as cards grouped by project
  if (showOnlyPendingCancel) {
    // Group by project
    const tasksByProject = sortedTasks.reduce((acc, task) => {
      const projectId = task.projectId
      if (!acc[projectId]) {
        acc[projectId] = task // Just store the first/only task per project
      }
      return acc
    }, {} as Record<number, typeof sortedTasks[0]>)

    return (
      <div className="space-y-4">
        {Object.values(tasksByProject).map((task: any) => (
          <PendingCancelTaskCard key={`pending-cancel-${task.projectId}`} task={task} />
        ))}
      </div>
    )
  }

  return (
    <div className="space-y-3">
      {sortedTasks.map((task: any) => (
        task.isPendingCancel ? (
          <PendingCancelTaskCard key={task.recordId} task={task} />
        ) : (
          <TaskListItem key={task.recordId} task={task} />
        )
      ))}
    </div>
  )
}
