'use client'

import { cn } from '@/lib/utils/cn'

interface TaskProgressProps {
  totalTasks: number
  approvedTasks: number
  size?: 'small' | 'default' | 'large'
  showPercentage?: boolean
  showCounts?: boolean
  className?: string
}

export function TaskProgress({
  totalTasks,
  approvedTasks,
  size = 'default',
  showPercentage = true,
  showCounts = true,
  className
}: TaskProgressProps) {
  const unapprovedTasks = totalTasks - approvedTasks
  const completionPercentage = totalTasks > 0 ? Math.round((approvedTasks / totalTasks) * 100) : 0

  // Color coding based on completion percentage
  const getProgressColor = () => {
    if (completionPercentage === 100) return 'bg-green-500'
    if (completionPercentage >= 67) return 'bg-blue-500'
    if (completionPercentage >= 34) return 'bg-yellow-500'
    return 'bg-orange-500'
  }

  const getBgColor = () => {
    if (completionPercentage === 100) return 'bg-green-50'
    if (completionPercentage >= 67) return 'bg-blue-50'
    if (completionPercentage >= 34) return 'bg-yellow-50'
    return 'bg-orange-50'
  }

  const getTextColor = () => {
    if (completionPercentage === 100) return 'text-green-700'
    if (completionPercentage >= 67) return 'text-blue-700'
    if (completionPercentage >= 34) return 'text-yellow-700'
    return 'text-orange-700'
  }

  // Size variants
  const sizeClasses = {
    small: {
      container: 'gap-1',
      bar: 'h-1.5',
      text: 'text-xs'
    },
    default: {
      container: 'gap-2',
      bar: 'h-2.5',
      text: 'text-sm'
    },
    large: {
      container: 'gap-3',
      bar: 'h-4',
      text: 'text-base'
    }
  }

  const sizes = sizeClasses[size]

  return (
    <div className={cn('flex flex-col', sizes.container, className)}>
      {/* Progress bar */}
      <div className="w-full bg-gray-200 rounded-full overflow-hidden" style={{ height: sizes.bar === 'h-1.5' ? '6px' : sizes.bar === 'h-2.5' ? '10px' : '16px' }}>
        <div
          className={cn('h-full transition-all duration-500 ease-out', getProgressColor())}
          style={{ width: `${completionPercentage}%` }}
          role="progressbar"
          aria-valuenow={completionPercentage}
          aria-valuemin={0}
          aria-valuemax={100}
        />
      </div>

      {/* Stats row */}
      {(showPercentage || showCounts) && (
        <div className={cn('flex items-center justify-between', sizes.text)}>
          {showCounts && (
            <span className={cn('font-medium', getTextColor())}>
              {approvedTasks} of {totalTasks} approved
            </span>
          )}
          {showPercentage && (
            <span className={cn('font-semibold', getTextColor())}>
              {completionPercentage}%
            </span>
          )}
        </div>
      )}

      {/* Status badge for completion */}
      {completionPercentage === 100 && (
        <div className={cn('inline-flex items-center gap-1.5 px-2 py-1 rounded-full w-fit', getBgColor(), sizes.text)}>
          <svg className={cn('w-4 h-4', getTextColor())} fill="none" viewBox="0 0 24 24" stroke="currentColor">
            <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M5 13l4 4L19 7" />
          </svg>
          <span className={cn('font-medium', getTextColor())}>All tasks complete!</span>
        </div>
      )}

      {/* Pending tasks warning */}
      {unapprovedTasks > 0 && completionPercentage < 100 && (
        <div className={cn('text-gray-600', sizes.text)}>
          {unapprovedTasks} {unapprovedTasks === 1 ? 'task' : 'tasks'} pending
        </div>
      )}
    </div>
  )
}
