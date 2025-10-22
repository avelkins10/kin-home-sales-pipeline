'use client'

import { AlertTriangle, Clock, CheckCircle } from 'lucide-react'
import { cn } from '@/lib/utils/cn'
import { UrgencyLevel, getUrgencyColors, getUrgencyLabel } from '@/lib/utils/task-urgency'

interface UrgencyBadgeProps {
  level: UrgencyLevel
  reason?: string
  daysWaiting?: number
  showIcon?: boolean
  size?: 'small' | 'default' | 'large'
  className?: string
}

export function UrgencyBadge({
  level,
  reason,
  daysWaiting,
  showIcon = true,
  size = 'default',
  className
}: UrgencyBadgeProps) {
  const colors = getUrgencyColors(level)
  const label = getUrgencyLabel(level)

  const Icon = level === 'critical'
    ? AlertTriangle
    : level === 'urgent'
    ? Clock
    : CheckCircle

  const sizeClasses = {
    small: 'px-2 py-0.5 text-xs',
    default: 'px-2.5 py-1 text-xs',
    large: 'px-3 py-1.5 text-sm'
  }

  const iconSizes = {
    small: 'w-3 h-3',
    default: 'w-3.5 h-3.5',
    large: 'w-4 h-4'
  }

  return (
    <div
      className={cn(
        'inline-flex items-center gap-1.5 rounded-full border font-medium',
        colors.bg,
        colors.text,
        colors.border,
        sizeClasses[size],
        className
      )}
      title={reason}
    >
      {showIcon && <Icon className={iconSizes[size]} />}
      <span>{label}</span>
      {daysWaiting !== undefined && daysWaiting > 0 && (
        <span className="font-normal opacity-75">
          · {daysWaiting}d
        </span>
      )}
    </div>
  )
}
