'use client'

import React, { useEffect, useRef, useState } from 'react'
import { useQuery } from '@tanstack/react-query'
import { AlertTriangle, CheckCircle, RefreshCw, Sparkles } from 'lucide-react'
import { useIsMobile } from '@/lib/hooks/useMediaQuery'
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card'
import { Progress } from '@/components/ui/progress'
import { Badge } from '@/components/ui/badge'
import { Button } from '@/components/ui/button'
import { Skeleton } from '@/components/ui/skeleton'
import { Alert, AlertDescription } from '@/components/ui/alert'
import { TaskCard } from './TaskCard'
import { TaskGroup } from '@/lib/types/task'
import { tasksKey } from '@/lib/queryKeys'

interface TaskSectionProps {
  projectId: string | number
  className?: string
}

export function TaskSection({ projectId, className }: TaskSectionProps) {
  const isMobile = useIsMobile()
  const [showCelebration, setShowCelebration] = useState(false)
  const prevUnapprovedRef = useRef<number | null>(null)
  
  const { data: taskGroups = [], isLoading, error, refetch, isFetching } = useQuery({
    queryKey: tasksKey(projectId),
    queryFn: async () => {
      const response = await fetch(`/api/projects/${projectId}/tasks`)
      if (!response.ok) {
        throw new Error('Failed to fetch tasks')
      }
      return response.json() as Promise<TaskGroup[]>
    },
    enabled: !!projectId,
    staleTime: 5 * 60 * 1000, // 5 minutes
    refetchInterval: 60000, // 60 seconds
    refetchIntervalInBackground: true,
    retry: 3,
  })

  // Celebration trigger effect
  useEffect(() => {
    if (taskGroups.length > 0) {
      const currentUnapproved = taskGroups.reduce((sum, group) => sum + group.unapprovedTasks, 0)
      
      // Trigger celebration when transitioning from pending to all complete
      if (prevUnapprovedRef.current !== null && 
          prevUnapprovedRef.current > 0 && 
          currentUnapproved === 0) {
        setShowCelebration(true)
        // Auto-hide after 5 seconds
        const timer = setTimeout(() => setShowCelebration(false), 5000)
        return () => clearTimeout(timer)
      }
      
      prevUnapprovedRef.current = currentUnapproved
    }
  }, [taskGroups])

  // Loading state
  if (isLoading) {
    return (
      <Card className={className}>
        <CardHeader>
          <CardTitle className="flex items-center gap-2">
            <AlertTriangle className="w-5 h-5 text-orange-500" />
            Action Required: Complete Missing Items
          </CardTitle>
        </CardHeader>
        <CardContent className={isMobile ? "space-y-3" : "space-y-4"} aria-label="Loading tasks" aria-busy="true">
          <Skeleton className="h-4 w-full" />
          <Skeleton className="h-2 w-full" />
          <div className="space-y-3">
            <Skeleton className={isMobile ? "h-16 w-full" : "h-20 w-full"} />
            <Skeleton className={isMobile ? "h-16 w-full" : "h-20 w-full"} />
            <Skeleton className={isMobile ? "h-16 w-full" : "h-20 w-full"} />
          </div>
        </CardContent>
      </Card>
    )
  }

  // Error state
  if (error) {
    return (
      <Card className={className}>
        <CardHeader>
          <CardTitle className="flex items-center gap-2">
            <AlertTriangle className="w-5 h-5 text-red-500" />
            Action Required: Complete Missing Items
          </CardTitle>
        </CardHeader>
        <CardContent>
          <Alert variant="destructive" role="alert" aria-live="assertive">
            <AlertTriangle className="h-4 w-4" />
            <AlertDescription className="flex items-center justify-between">
              <span>{error?.message || 'Failed to load tasks. Please try again.'}</span>
              <Button
                variant="outline"
                size="sm"
                onClick={() => refetch()}
                className="ml-2"
              >
                <RefreshCw className="w-4 h-4 mr-1" />
                Retry
              </Button>
            </AlertDescription>
          </Alert>
        </CardContent>
      </Card>
    )
  }

  // No tasks - show empty state
  if (taskGroups.length === 0) {
    return (
      <Card className={className}>
        <CardContent className="py-8 text-center">
          <CheckCircle className="h-12 w-12 text-green-500 mx-auto mb-3" />
          <p className="text-sm font-medium text-gray-900 mb-1">
            No Tasks Required
          </p>
          <p className="text-xs text-gray-500">
            This project doesn&apos;t have any pending tasks at this time.
          </p>
        </CardContent>
      </Card>
    )
  }

  // Calculate progress
  const totalTasks = taskGroups.reduce((sum, group) => sum + group.totalTasks, 0)
  const totalUnapproved = taskGroups.reduce((sum, group) => sum + group.unapprovedTasks, 0)
  const approvedTasks = totalTasks - totalUnapproved
  const progressPercentage = totalTasks > 0 ? Math.round((approvedTasks / totalTasks) * 100) : 0

  // Get all tasks from all groups
  const allTasks = taskGroups.flatMap(group => group.tasks)

  // Determine progress bar color
  const getProgressColor = (percentage: number) => {
    if (percentage <= 33) return 'bg-red-500'
    if (percentage <= 66) return 'bg-yellow-500'
    return 'bg-green-500'
  }

  return (
    <Card className={className}>
      <CardHeader>
        <div className="flex items-center justify-between">
          <CardTitle className="flex items-center gap-2">
            <AlertTriangle className="w-5 h-5 text-orange-500" />
            Action Required: Complete Missing Items
          </CardTitle>
          <div className="flex items-center gap-2">
            {isFetching && !isLoading && (
              <div className="text-xs text-gray-500 flex items-center gap-1">
                <RefreshCw className="h-3 w-3 animate-spin" />
                <span className="hidden sm:inline">Updating...</span>
              </div>
            )}
            {totalUnapproved > 0 && (
              <Badge variant="destructive">
                {totalUnapproved} pending
              </Badge>
            )}
          </div>
        </div>
      </CardHeader>
      <CardContent className="space-y-4">
        {/* Celebration state */}
        {totalUnapproved === 0 && totalTasks > 0 && (
          <Alert 
            className={`bg-gradient-to-r from-green-50 to-emerald-50 border-green-200 ${
              showCelebration ? 'animate-bounce-in' : ''
            }`}
            role="status"
            aria-live="polite"
          >
            <div className="flex items-center gap-2">
              <Sparkles className="h-5 w-5 text-green-600 animate-celebration-pulse" />
              <CheckCircle className="h-4 w-4 text-green-600" />
            </div>
            <AlertDescription className="text-green-800 font-medium">
              🎉 All tasks complete! Your project is ready for review.
            </AlertDescription>
          </Alert>
        )}

        {/* Progress section */}
        <div 
          className="space-y-2"
          role="status"
          aria-live="polite"
          aria-label={`Task progress: ${approvedTasks} of ${totalTasks} approved`}
        >
          <div className="flex items-center justify-between text-sm">
            <span className="text-gray-600">
              Progress: {approvedTasks} of {totalTasks} approved
            </span>
            <span className="text-gray-500">{progressPercentage}%</span>
          </div>
          <Progress 
            value={progressPercentage} 
            className="h-2"
            indicatorClassName={getProgressColor(progressPercentage)}
          />
        </div>

        {/* Task list */}
        {allTasks.length > 0 && (
          <div className={`space-y-3 overflow-y-auto ${
            isMobile ? 'max-h-80 pb-2' : 'max-h-96'
          }`}>
            {allTasks.map((task) => (
                <TaskCard key={task.recordId} task={task} projectId={projectId} />
            ))}
          </div>
        )}
      </CardContent>
    </Card>
  )
}
