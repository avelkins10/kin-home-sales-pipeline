'use client'

import { useSession } from 'next-auth/react'
import { redirect } from 'next/navigation'
import { useQuery } from '@tanstack/react-query'
import { useState, useMemo } from 'react'
import { AlertTriangle, CheckCircle, Clock, Filter, Grid3x3, List, Building2, ArrowUpDown, ArrowUp, ArrowDown, Calendar } from 'lucide-react'
import { TaskList } from '@/components/tasks/TaskList'
import { Button } from '@/components/ui/button'
import { Skeleton } from '@/components/ui/skeleton'
import { cn } from '@/lib/utils/cn'
import { getTaskUrgency } from '@/lib/utils/task-urgency'

type GroupBy = 'none' | 'project' | 'urgency' | 'office'
type SortBy = 'urgency' | 'date-newest' | 'date-oldest' | 'name'
type DateRange = 'all' | 'ytd' | '90' | '30' | '7'

export default function TasksPage() {
  const { data: session, status } = useSession()
  const [groupBy, setGroupBy] = useState<GroupBy>('urgency')
  const [sortBy, setSortBy] = useState<SortBy>('urgency')
  const [selectedCloser, setSelectedCloser] = useState<string>('all')
  const [selectedOffice, setSelectedOffice] = useState<string>('all')
  // Default to showing actionable tasks (exclude approved) - most important are not started and pending cancels
  const [selectedStatus, setSelectedStatus] = useState<string>('actionable')
  const [dateRange, setDateRange] = useState<DateRange>('all')
  const [expandedOffices, setExpandedOffices] = useState<Set<string>>(new Set())

  // Tab state: 'open' shows active tasks, 'pending-cancel' shows cancelled/pending cancel tasks
  const [activeTab, setActiveTab] = useState<'open' | 'pending-cancel'>('open')

  // Fetch tasks for current user with date range and actionable filters
  // IMPORTANT: Must call all hooks before any early returns (Rules of Hooks)
  // By default, fetch actionable tasks only (faster initial load) - excludes Approved/Closed by Ops
  // When user selects "All", "Approved", or "Closed by Ops", fetch all tasks
  const shouldFetchAllTasks = selectedStatus === 'all' || selectedStatus === 'approved' || selectedStatus === 'closed_by_ops'
  
  const { data: tasksData, isLoading, error } = useQuery({
    queryKey: ['tasks', dateRange, shouldFetchAllTasks],
    queryFn: async () => {
      const params = new URLSearchParams()
      if (dateRange !== 'all') {
        params.set('range', dateRange)
      }
      // Filter at API level for performance: actionable=true excludes Approved/Closed by Ops
      // This reduces data transfer and speeds up initial load significantly
      if (!shouldFetchAllTasks) {
        params.set('actionable', 'true') // Default: faster, only actionable tasks
      } else {
        params.set('actionable', 'false') // User wants to see all including approved/closed
      }
      
      const url = `/api/tasks${params.toString() ? `?${params.toString()}` : ''}`
      const response = await fetch(url)
      if (!response.ok) {
        throw new Error('Failed to fetch tasks')
      }
      const data = await response.json()
      // Handle both new format (with activeTasks) and old format (array)
      if (data.activeTasks) {
        return data
      }
      // Backward compatibility: if it's an array, treat as activeTasks
      return { activeTasks: data, cancelledAndPendingCancelTasks: [], allTasks: data }
    },
    staleTime: 60000, // 1 minute
    refetchInterval: 60000, // Auto-refresh every minute
    enabled: status !== 'loading' && !!session, // Only run query when session is ready
  })

  // Get tasks based on active tab - memoize to ensure stability
  const tasks = useMemo(() => {
    return activeTab === 'open' 
      ? (tasksData?.activeTasks || [])
      : (tasksData?.cancelledAndPendingCancelTasks || [])
  }, [activeTab, tasksData?.activeTasks, tasksData?.cancelledAndPendingCancelTasks])
  
  // Fallback to allTasks for backward compatibility - ensure it's always an array
  const allTasks = useMemo(() => {
    return Array.isArray(tasksData?.allTasks) 
      ? tasksData.allTasks 
      : Array.isArray(tasksData) 
        ? tasksData 
        : []
  }, [tasksData])
  
  // Ensure tasks is always an array (defensive programming)
  const tasksArray = useMemo(() => {
    return Array.isArray(tasks) ? tasks : []
  }, [tasks])

  if (status === 'loading') {
    return <TasksPageSkeleton />
  }

  if (!session) {
    redirect('/login')
  }

  // Toggle office expansion
  const toggleOffice = (officeName: string) => {
    setExpandedOffices(prev => {
      const next = new Set(prev)
      if (next.has(officeName)) {
        next.delete(officeName)
      } else {
        next.add(officeName)
      }
      return next
    })
  }

  // Extract unique closers and offices for filters (use allTasks for filter options)
  const uniqueClosers = Array.from(new Set(
    allTasks
      .map((t: any) => t.closerName)
      .filter((name): name is string => !!name)
  )).sort()

  const uniqueOffices = Array.from(new Set(
    allTasks
      .map((t: any) => t.salesOffice)
      .filter((office): office is string => !!office)
  )).sort()

  // Calculate status counts for tabs
  const statusCounts = {
    all: tasksArray.length,
    actionable: tasksArray.filter((t: any) => {
      const taskStatus = (t.status || '').toLowerCase().trim()
      return taskStatus !== 'approved' && taskStatus !== 'closed by ops'
    }).length,
    not_started: tasksArray.filter((t: any) => (t.status || '').toLowerCase().trim() === 'not started').length,
    in_progress: tasksArray.filter((t: any) => (t.status || '').toLowerCase().trim() === 'in progress').length,
    approved: tasksArray.filter((t: any) => (t.status || '').toLowerCase().trim() === 'approved').length,
    closed_by_ops: tasksArray.filter((t: any) => (t.status || '').toLowerCase().trim() === 'closed by ops').length,
  }

  // Filter tasks by selected closer, office, and status
  const filteredTasks = useMemo(() => {
    if (!Array.isArray(tasksArray) || tasksArray.length === 0) {
      return []
    }
    
    return tasksArray.filter((t: any) => {
      // Filter by closer
      if (selectedCloser !== 'all' && t.closerName !== selectedCloser) {
        return false
      }
      // Filter by office
      if (selectedOffice !== 'all' && t.salesOffice !== selectedOffice) {
        return false
      }
      // Filter by status - use task.status field, not projectStatus
      const taskStatus = (t.status || '').toLowerCase().trim()
      
      if (selectedStatus === 'actionable') {
        // Show actionable tasks: not started, in progress, pending cancel (not approved/closed)
        // This is the default - prioritize important tasks that need action
        if (taskStatus === 'approved' || taskStatus === 'closed by ops') {
          return false
        }
        // Always include pending cancel tasks (they're action items)
        if (t.isPendingCancel) {
          return true
        }
        return true // Include not started, in progress, etc.
      } else if (selectedStatus !== 'all') {
        if (selectedStatus === 'approved' && taskStatus !== 'approved') {
          return false
        }
        if (selectedStatus === 'not_started' && taskStatus !== 'not started') {
          return false
        }
        if (selectedStatus === 'in_progress' && taskStatus !== 'in progress') {
          return false
        }
        if (selectedStatus === 'closed_by_ops' && taskStatus !== 'closed by ops') {
          return false
        }
      }
      return true
    })
  }, [tasksArray, selectedCloser, selectedOffice, selectedStatus])

  // Apply sorting to filtered tasks
  const sortedTasks = useMemo(() => {
    if (!Array.isArray(filteredTasks) || filteredTasks.length === 0) {
      return []
    }
    
    let sorted = [...filteredTasks]
    
    switch (sortBy) {
      case 'date-newest':
        sorted.sort((a, b) => {
          try {
            const dateA = a?.dateCreated ? new Date(a.dateCreated).getTime() : 0
            const dateB = b?.dateCreated ? new Date(b.dateCreated).getTime() : 0
            if (isNaN(dateA)) return 1 // Invalid dates go to end
            if (isNaN(dateB)) return -1
            return dateB - dateA // Newest first
          } catch {
            return 0 // If date parsing fails, maintain order
          }
        })
        break
      case 'date-oldest':
        sorted.sort((a, b) => {
          try {
            const dateA = a?.dateCreated ? new Date(a.dateCreated).getTime() : 0
            const dateB = b?.dateCreated ? new Date(b.dateCreated).getTime() : 0
            if (isNaN(dateA)) return 1 // Invalid dates go to end
            if (isNaN(dateB)) return -1
            return dateA - dateB // Oldest first
          } catch {
            return 0 // If date parsing fails, maintain order
          }
        })
        break
      case 'name':
        sorted.sort((a, b) => {
          const nameA = (a?.name || '').toLowerCase()
          const nameB = (b?.name || '').toLowerCase()
          return nameA.localeCompare(nameB)
        })
        break
      case 'urgency':
      default:
        // Sort by urgency (handled by TaskList component when groupBy='urgency')
        // For other groupBy modes, use default order
        break
    }
    
    return sorted
  }, [filteredTasks, sortBy])

  // Calculate stats from filtered tasks (only count active tasks for stats)
  // Stats should reflect the current tab's tasks
  const tasksForStats = activeTab === 'open' ? filteredTasks : []
  
  const totalIncompleteTasks = tasksForStats.filter((task: any) => {
    const taskStatus = (task.status || '').toLowerCase().trim()
    return taskStatus !== 'approved' && taskStatus !== 'closed by ops'
  }).length
  
  const totalApprovedTasks = tasksForStats.filter((task: any) => {
    const taskStatus = (task.status || '').toLowerCase().trim()
    return taskStatus === 'approved'
  }).length
  
  // Only count INCOMPLETE tasks waiting >7 days (exclude approved/closed)
  const totalTasksWaitingOver7Days = tasksForStats.filter((task: any) => {
    const taskStatus = (task.status || '').toLowerCase().trim()
    // Exclude approved and closed tasks - they're not "waiting"
    if (taskStatus === 'approved' || taskStatus === 'closed by ops') {
      return false
    }
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
            {activeTab === 'open' 
              ? 'All pending tasks across your active projects'
              : 'Tasks for cancelled or pending cancellation projects'}
          </p>
        </div>

        {/* Tabs */}
        <div className="mb-6 flex gap-2 border-b border-gray-200">
          <button
            onClick={() => setActiveTab('open')}
            className={cn(
              'px-4 py-2 text-sm font-medium border-b-2 transition-colors',
              activeTab === 'open'
                ? 'border-blue-600 text-blue-600'
                : 'border-transparent text-gray-500 hover:text-gray-700'
            )}
          >
            Open Tasks ({tasksData?.activeTasks?.length || 0})
          </button>
          <button
            onClick={() => setActiveTab('pending-cancel')}
            className={cn(
              'px-4 py-2 text-sm font-medium border-b-2 transition-colors',
              activeTab === 'pending-cancel'
                ? 'border-blue-600 text-blue-600'
                : 'border-transparent text-gray-500 hover:text-gray-700'
            )}
          >
            Pending Cancel ({tasksData?.cancelledAndPendingCancelTasks?.length || 0})
          </button>
        </div>

        {/* Stats Bar */}
        <div className="grid grid-cols-1 md:grid-cols-3 gap-4 mb-6">
          <div className="bg-white rounded-lg border p-4">
            <div className="flex items-center justify-between">
              <div>
                <p className="text-sm text-gray-500">Total Incomplete Tasks</p>
                <p className="text-2xl font-bold text-gray-900">{totalIncompleteTasks}</p>
              </div>
              <List className="w-8 h-8 text-gray-400" />
            </div>
          </div>

          <div className="bg-white rounded-lg border p-4">
            <div className="flex items-center justify-between">
              <div>
                <p className="text-sm text-gray-500">Total Approved Tasks</p>
                <p className="text-2xl font-bold text-green-600">{totalApprovedTasks}</p>
              </div>
              <CheckCircle className="w-8 h-8 text-green-400" />
            </div>
          </div>

          <div className="bg-white rounded-lg border p-4">
            <div className="flex items-center justify-between">
              <div>
                <p className="text-sm text-gray-500">Total Tasks Waiting &gt;7 days</p>
                <p className="text-2xl font-bold text-orange-600">{totalTasksWaitingOver7Days}</p>
              </div>
              <Clock className="w-8 h-8 text-orange-400" />
            </div>
          </div>
        </div>

        {/* Controls */}
        <div className="bg-white rounded-lg border p-4 mb-6 space-y-4">
          {/* Date Range Filter - Most Prominent */}
          <div className="flex flex-wrap items-center gap-3 pb-3 border-b border-gray-200">
            <div className="flex items-center gap-2">
              <Calendar className="w-4 h-4 text-gray-500" />
              <span className="text-sm font-medium text-gray-700">Date Range:</span>
            </div>
            <div className="flex flex-wrap gap-2">
              <Button
                variant={dateRange === 'all' ? 'default' : 'outline'}
                size="sm"
                onClick={() => setDateRange('all')}
                className={cn(
                  dateRange === 'all' && 'bg-blue-600 hover:bg-blue-700'
                )}
              >
                All Time
              </Button>
              <Button
                variant={dateRange === 'ytd' ? 'default' : 'outline'}
                size="sm"
                onClick={() => setDateRange('ytd')}
                className={cn(
                  dateRange === 'ytd' && 'bg-blue-600 hover:bg-blue-700'
                )}
              >
                Year to Date
              </Button>
              <Button
                variant={dateRange === '90' ? 'default' : 'outline'}
                size="sm"
                onClick={() => setDateRange('90')}
                className={cn(
                  dateRange === '90' && 'bg-blue-600 hover:bg-blue-700'
                )}
              >
                Last 90 Days
              </Button>
              <Button
                variant={dateRange === '30' ? 'default' : 'outline'}
                size="sm"
                onClick={() => setDateRange('30')}
                className={cn(
                  dateRange === '30' && 'bg-blue-600 hover:bg-blue-700'
                )}
              >
                Last 30 Days
              </Button>
              <Button
                variant={dateRange === '7' ? 'default' : 'outline'}
                size="sm"
                onClick={() => setDateRange('7')}
                className={cn(
                  dateRange === '7' && 'bg-blue-600 hover:bg-blue-700'
                )}
              >
                Last 7 Days
              </Button>
            </div>
          </div>

          {/* Filters */}
          <div className="flex flex-wrap items-center gap-4">
            <div className="flex items-center gap-2">
              <Filter className="w-4 h-4 text-gray-500" />
              <span className="text-sm font-medium text-gray-700">Filters:</span>
            </div>

            {/* Filter by Closer */}
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

            {/* Filter by Office */}
            <select
              value={selectedOffice}
              onChange={(e) => setSelectedOffice(e.target.value)}
              className="text-sm border border-gray-300 rounded-md px-3 py-1.5 focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-blue-500"
            >
              <option value="all">All Offices ({tasks.length})</option>
              {uniqueOffices.map((office) => {
                const count = tasks.filter((t: any) => t.salesOffice === office).length
                return (
                  <option key={office} value={office}>
                    {office} ({count})
                  </option>
                )
              })}
            </select>

          </div>

          {/* Status Tabs */}
          <div className="flex flex-wrap items-center gap-2 border-b border-gray-200 pb-2">
            <span className="text-sm font-medium text-gray-700 mr-2">Status:</span>
            <Button
              variant={selectedStatus === 'actionable' ? 'default' : 'outline'}
              size="sm"
              onClick={() => setSelectedStatus('actionable')}
              className={cn(
                selectedStatus === 'actionable' && 'bg-blue-600 hover:bg-blue-700',
                'rounded-md'
              )}
            >
              Actionable ({statusCounts.actionable})
            </Button>
            <Button
              variant={selectedStatus === 'all' ? 'default' : 'outline'}
              size="sm"
              onClick={() => setSelectedStatus('all')}
              className={cn(
                selectedStatus === 'all' && 'bg-blue-600 hover:bg-blue-700',
                'rounded-md'
              )}
            >
              All ({statusCounts.all})
            </Button>
            <Button
              variant={selectedStatus === 'not_started' ? 'default' : 'outline'}
              size="sm"
              onClick={() => setSelectedStatus('not_started')}
              className={cn(
                selectedStatus === 'not_started' && 'bg-blue-600 hover:bg-blue-700',
                'rounded-md'
              )}
            >
              Not Started ({statusCounts.not_started})
            </Button>
            <Button
              variant={selectedStatus === 'in_progress' ? 'default' : 'outline'}
              size="sm"
              onClick={() => setSelectedStatus('in_progress')}
              className={cn(
                selectedStatus === 'in_progress' && 'bg-blue-600 hover:bg-blue-700',
                'rounded-md'
              )}
            >
              In Progress ({statusCounts.in_progress})
            </Button>
            <Button
              variant={selectedStatus === 'approved' ? 'default' : 'outline'}
              size="sm"
              onClick={() => setSelectedStatus('approved')}
              className={cn(
                selectedStatus === 'approved' && 'bg-blue-600 hover:bg-blue-700',
                'rounded-md'
              )}
            >
              Approved ({statusCounts.approved})
            </Button>
            <Button
              variant={selectedStatus === 'closed_by_ops' ? 'default' : 'outline'}
              size="sm"
              onClick={() => setSelectedStatus('closed_by_ops')}
              className={cn(
                selectedStatus === 'closed_by_ops' && 'bg-blue-600 hover:bg-blue-700',
                'rounded-md'
              )}
            >
              Closed by Ops ({statusCounts.closed_by_ops})
            </Button>
          </div>

          {/* Group by */}
          <div className="flex flex-wrap items-center gap-3">
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

          {/* Sort by */}
          <div className="flex flex-wrap items-center gap-3">
            <span className="text-sm font-medium text-gray-700">Sort by:</span>
            <div className="flex items-center gap-2">
            <Button
              variant={sortBy === 'urgency' ? 'default' : 'outline'}
              size="sm"
              onClick={() => setSortBy('urgency')}
              className={cn(
                sortBy === 'urgency' && 'bg-blue-600 hover:bg-blue-700'
              )}
            >
              <AlertTriangle className="w-4 h-4 mr-1.5" />
              Urgency
            </Button>
            <Button
              variant={sortBy === 'date-newest' ? 'default' : 'outline'}
              size="sm"
              onClick={() => setSortBy('date-newest')}
              className={cn(
                sortBy === 'date-newest' && 'bg-blue-600 hover:bg-blue-700'
              )}
            >
              <ArrowDown className="w-4 h-4 mr-1.5" />
              Newest First
            </Button>
            <Button
              variant={sortBy === 'date-oldest' ? 'default' : 'outline'}
              size="sm"
              onClick={() => setSortBy('date-oldest')}
              className={cn(
                sortBy === 'date-oldest' && 'bg-blue-600 hover:bg-blue-700'
              )}
            >
              <ArrowUp className="w-4 h-4 mr-1.5" />
              Oldest First
            </Button>
            <Button
              variant={sortBy === 'name' ? 'default' : 'outline'}
              size="sm"
              onClick={() => setSortBy('name')}
              className={cn(
                sortBy === 'name' && 'bg-blue-600 hover:bg-blue-700'
              )}
            >
              <ArrowUpDown className="w-4 h-4 mr-1.5" />
              Name
            </Button>
            </div>
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
          <TaskList
            tasks={sortedTasks}
            groupBy={groupBy}
            expandedOffices={expandedOffices}
            toggleOffice={toggleOffice}
            showOnlyPendingCancel={activeTab === 'pending-cancel'}
          />
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
