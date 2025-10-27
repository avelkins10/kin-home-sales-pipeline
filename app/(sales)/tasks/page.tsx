'use client'

import { useSession } from 'next-auth/react'
import { redirect } from 'next/navigation'
import { useQuery } from '@tanstack/react-query'
import { useState } from 'react'
import { AlertTriangle, CheckCircle, Clock, Filter, Grid3x3, List, Building2 } from 'lucide-react'
import { TaskList } from '@/components/tasks/TaskList'
import { Button } from '@/components/ui/button'
import { Skeleton } from '@/components/ui/skeleton'
import { cn } from '@/lib/utils/cn'
import { getTaskUrgency } from '@/lib/utils/task-urgency'

type GroupBy = 'none' | 'project' | 'urgency' | 'office'

export default function TasksPage() {
  const { data: session, status } = useSession()
  const [groupBy, setGroupBy] = useState<GroupBy>('urgency')
  const [selectedCloser, setSelectedCloser] = useState<string>('all')

  // Fetch all tasks for current user
  // IMPORTANT: Must call all hooks before any early returns (Rules of Hooks)
  const { data: tasks = [], isLoading, error } = useQuery({
    queryKey: ['tasks', 'all'],
    queryFn: async () => {
      const response = await fetch('/api/tasks')
      if (!response.ok) {
        throw new Error('Failed to fetch tasks')
      }
      return response.json()
    },
    staleTime: 60000, // 1 minute
    refetchInterval: 60000, // Auto-refresh every minute
    enabled: status !== 'loading' && !!session, // Only run query when session is ready
  })

  if (status === 'loading') {
    return <TasksPageSkeleton />
  }

  if (!session) {
    redirect('/login')
  }

  // Extract unique closers for filter
  const uniqueClosers = Array.from(new Set(
    tasks
      .map((t: any) => t.closerName)
      .filter((name): name is string => !!name)
  )).sort()

  // Filter tasks by selected closer
  const filteredTasks = selectedCloser === 'all'
    ? tasks
    : tasks.filter((t: any) => t.closerName === selectedCloser)

  // Calculate stats from filtered tasks
  const totalTasks = filteredTasks.length
  const criticalCount = filteredTasks.filter((task: any) => {
    const urgency = getTaskUrgency(task, task.projectStatus)
    return urgency.level === 'critical'
  }).length
  const urgentCount = filteredTasks.filter((task: any) => {
    const urgency = getTaskUrgency(task, task.projectStatus)
    return urgency.level === 'urgent'
  }).length
  const waitingOver7Days = filteredTasks.filter((task: any) => {
    const daysWaiting = task.dateCreated
      ? Math.ceil((Date.now() - new Date(task.dateCreated).getTime()) / (1000 * 60 * 60 * 24))
      : 0
    return daysWaiting > 7
  }).length

  return (
    <div className="min-h-screen bg-gray-50">
      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-6">
        {/* Header */}
        <div className="mb-6">
          <h1 className="text-2xl font-bold text-gray-900">My Tasks</h1>
          <p className="mt-1 text-sm text-gray-600">
            All pending tasks across your projects
          </p>
        </div>

        {/* Stats Bar */}
        <div className="grid grid-cols-1 md:grid-cols-4 gap-4 mb-6">
          <div className="bg-white rounded-lg border p-4">
            <div className="flex items-center justify-between">
              <div>
                <p className="text-sm text-gray-500">Total Tasks</p>
                <p className="text-2xl font-bold text-gray-900">{totalTasks}</p>
              </div>
              <List className="w-8 h-8 text-gray-400" />
            </div>
          </div>

          <div className="bg-white rounded-lg border p-4">
            <div className="flex items-center justify-between">
              <div>
                <p className="text-sm text-gray-500">Critical</p>
                <p className="text-2xl font-bold text-red-600">{criticalCount}</p>
              </div>
              <AlertTriangle className="w-8 h-8 text-red-400" />
            </div>
          </div>

          <div className="bg-white rounded-lg border p-4">
            <div className="flex items-center justify-between">
              <div>
                <p className="text-sm text-gray-500">Urgent</p>
                <p className="text-2xl font-bold text-yellow-600">{urgentCount}</p>
              </div>
              <Clock className="w-8 h-8 text-yellow-400" />
            </div>
          </div>

          <div className="bg-white rounded-lg border p-4">
            <div className="flex items-center justify-between">
              <div>
                <p className="text-sm text-gray-500">Waiting &gt;7 days</p>
                <p className="text-2xl font-bold text-orange-600">{waitingOver7Days}</p>
              </div>
              <CheckCircle className="w-8 h-8 text-orange-400" />
            </div>
          </div>
        </div>

        {/* Controls */}
        <div className="flex flex-col sm:flex-row items-start sm:items-center justify-between gap-4 mb-6 bg-white rounded-lg border p-4">
          {/* Filter by Closer */}
          <div className="flex items-center gap-3">
            <div className="flex items-center gap-2">
              <Filter className="w-4 h-4 text-gray-500" />
              <span className="text-sm font-medium text-gray-700">Filter:</span>
            </div>
            <select
              value={selectedCloser}
              onChange={(e) => setSelectedCloser(e.target.value)}
              className="text-sm border border-gray-300 rounded-md px-3 py-1.5 focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-blue-500"
            >
              <option value="all">All Closers ({tasks.length})</option>
              {uniqueClosers.map((closer) => {
                const count = tasks.filter((t: any) => t.closerName === closer).length
                return (
                  <option key={closer} value={closer}>
                    {closer} ({count})
                  </option>
                )
              })}
            </select>
          </div>

          {/* Group by */}
          <div className="flex items-center gap-3">
            <span className="text-sm font-medium text-gray-700">Group by:</span>
            <div className="flex items-center gap-2">
            <Button
              variant={groupBy === 'urgency' ? 'default' : 'outline'}
              size="sm"
              onClick={() => setGroupBy('urgency')}
              className={cn(
                groupBy === 'urgency' && 'bg-blue-600 hover:bg-blue-700'
              )}
            >
              <AlertTriangle className="w-4 h-4 mr-1.5" />
              Urgency
            </Button>
            <Button
              variant={groupBy === 'project' ? 'default' : 'outline'}
              size="sm"
              onClick={() => setGroupBy('project')}
              className={cn(
                groupBy === 'project' && 'bg-blue-600 hover:bg-blue-700'
              )}
            >
              <Grid3x3 className="w-4 h-4 mr-1.5" />
              Project
            </Button>
            <Button
              variant={groupBy === 'office' ? 'default' : 'outline'}
              size="sm"
              onClick={() => setGroupBy('office')}
              className={cn(
                groupBy === 'office' && 'bg-blue-600 hover:bg-blue-700'
              )}
            >
              <Building2 className="w-4 h-4 mr-1.5" />
              Office
            </Button>
            <Button
              variant={groupBy === 'none' ? 'default' : 'outline'}
              size="sm"
              onClick={() => setGroupBy('none')}
              className={cn(
                groupBy === 'none' && 'bg-blue-600 hover:bg-blue-700'
              )}
            >
              <List className="w-4 h-4 mr-1.5" />
              List
            </Button>
          </div>
        </div>

        {/* Task List */}
        {isLoading ? (
          <TaskListSkeleton />
        ) : error ? (
          <div className="bg-white rounded-lg border p-6 text-center">
            <AlertTriangle className="w-12 h-12 text-red-500 mx-auto mb-3" />
            <h3 className="text-lg font-semibold text-gray-900 mb-1">
              Failed to Load Tasks
            </h3>
            <p className="text-sm text-gray-500">
              {(error as Error).message || 'Please try again later.'}
            </p>
          </div>
        ) : (
          <TaskList tasks={filteredTasks} groupBy={groupBy} />
        )}
      </div>
    </div>
  )
}

function TasksPageSkeleton() {
  return (
    <div className="min-h-screen bg-gray-50">
      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-6">
        <Skeleton className="h-8 w-48 mb-2" />
        <Skeleton className="h-4 w-64 mb-6" />

        <div className="grid grid-cols-1 md:grid-cols-4 gap-4 mb-6">
          {[1, 2, 3, 4].map((i) => (
            <Skeleton key={i} className="h-24 w-full" />
          ))}
        </div>

        <Skeleton className="h-16 w-full mb-6" />

        <div className="space-y-3">
          {[1, 2, 3].map((i) => (
            <Skeleton key={i} className="h-32 w-full" />
          ))}
        </div>
      </div>
    </div>
  )
}

function TaskListSkeleton() {
  return (
    <div className="space-y-3">
      {[1, 2, 3, 4, 5].map((i) => (
        <Skeleton key={i} className="h-32 w-full" />
      ))}
    </div>
  )
}
