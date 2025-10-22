'use client'

import React from 'react'
import { Skeleton } from '@/components/ui/skeleton'
import { cn } from '@/lib/utils/cn'
import { useIsMobile } from '@/lib/hooks/useMediaQuery'

interface TaskCardSkeletonProps {
  className?: string
}

export function TaskCardSkeleton({ className }: TaskCardSkeletonProps) {
  const isMobile = useIsMobile()

  return (
    <div className={cn(
      'rounded-lg border bg-white space-y-3',
      isMobile ? 'p-3' : 'p-4',
      className
    )}>
      {/* Header */}
      <div className={cn(
        'flex items-start justify-between',
        isMobile ? 'gap-2' : 'gap-3'
      )}>
        <div className="flex-1 min-w-0 space-y-2">
          <Skeleton className={cn(
            'h-4',
            isMobile ? 'w-3/4' : 'w-1/2'
          )} />
          <Skeleton className={cn(
            'h-3',
            isMobile ? 'w-1/2' : 'w-1/3'
          )} />
        </div>
        <Skeleton className="h-6 w-20 rounded-full" />
      </div>

      {/* Submission History */}
      <div className="space-y-2">
        <Skeleton className="h-3 w-24" />
        <div className="space-y-2">
          <div className="flex items-start gap-2">
            <Skeleton className="h-3 w-16 flex-shrink-0" />
            <div className="flex flex-col gap-1">
              <div className="flex items-center gap-1.5">
                <Skeleton className="h-3 w-3 rounded-full" />
                <Skeleton className="h-3 w-20" />
              </div>
            </div>
          </div>
        </div>
      </div>

      {/* Form Area */}
      <div className="space-y-3">
        <Skeleton className="h-9 w-full rounded-md" />
      </div>
    </div>
  )
}
