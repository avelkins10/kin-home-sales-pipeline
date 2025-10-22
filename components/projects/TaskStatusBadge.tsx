'use client'

import React from 'react'
import { AlertCircle, Clock, CheckCircle } from 'lucide-react'
import { cn } from '@/lib/utils/cn'
import { TaskStatus } from '@/lib/types/task'

interface TaskStatusBadgeProps {
  status: TaskStatus
  className?: string
}

function getTaskStatusConfig(status: TaskStatus) {
  switch (status) {
    case 'Not Started':
      return {
        icon: AlertCircle,
        colors: 'bg-red-50 text-red-700 border-red-200',
        label: 'Not Started',
      }
    case 'In Progress':
      return {
        icon: Clock,
        colors: 'bg-yellow-50 text-yellow-700 border-yellow-200',
        label: 'In Progress',
      }
    case 'Complete':
      return {
        icon: CheckCircle,
        colors: 'bg-green-50 text-green-700 border-green-200',
        label: 'Complete',
      }
    default:
      return {
        icon: AlertCircle,
        colors: 'bg-gray-50 text-gray-700 border-gray-200',
        label: status,
      }
  }
}

export function TaskStatusBadge({ status, className }: TaskStatusBadgeProps) {
  const config = getTaskStatusConfig(status)
  const Icon = config.icon

  return (
    <div
      className={cn(
        'inline-flex items-center gap-1.5 px-2.5 py-1 rounded-full border text-xs font-medium',
        config.colors,
        className
      )}
      role="status"
      aria-label={`Task status: ${status}`}
      title={status}
    >
      <Icon className="w-3 h-3" aria-hidden="true" />
      <span className="hidden sm:inline">{config.label}</span>
    </div>
  )
}
