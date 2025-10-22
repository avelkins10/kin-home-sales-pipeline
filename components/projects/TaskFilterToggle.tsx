'use client'

import { useRouter, useSearchParams } from 'next/navigation'
import { Button } from '@/components/ui/button'
import { List, AlertTriangle } from 'lucide-react'
import { cn } from '@/lib/utils/cn'

interface TaskFilterToggleProps {
  currentFilter: boolean // true if filtering to show only projects with tasks
}

const taskFilterOptions = [
  { value: false, label: 'All Projects', icon: List },
  { value: true, label: 'With Tasks', icon: AlertTriangle },
]

export function TaskFilterToggle({ currentFilter }: TaskFilterToggleProps) {
  const router = useRouter()
  const searchParams = useSearchParams()

  const handleFilterChange = (showTasksOnly: boolean) => {
    const params = new URLSearchParams(searchParams.toString())
    
    if (showTasksOnly) {
      params.set('withTasks', 'true')
    } else {
      params.delete('withTasks')
    }
    
    router.push(`/projects?${params.toString()}`)
  }

  return (
    <div className="flex items-center gap-1 mb-4">
      <div 
        role="group" 
        aria-label="Filter projects by task status"
        className="flex rounded-lg border border-gray-300 overflow-hidden"
      >
        {taskFilterOptions.map((option, index) => {
          const isActive = currentFilter === option.value
          const Icon = option.icon
          
          return (
            <Button
              key={option.value.toString()}
              variant={isActive ? 'default' : 'outline'}
              size="sm"
              onClick={() => handleFilterChange(option.value)}
              className={cn(
                'flex items-center gap-2 px-3 py-2 text-sm font-medium transition-colors',
                'border-0 rounded-none',
                index === 0 && 'rounded-l-lg',
                index === taskFilterOptions.length - 1 && 'rounded-r-lg',
                index > 0 && 'border-l-0',
                isActive 
                  ? 'bg-orange-600 text-white hover:bg-orange-700' 
                  : 'bg-white text-gray-700 hover:bg-gray-50 border-gray-300'
              )}
              aria-pressed={isActive}
            >
              <Icon className="h-4 w-4" />
              <span className="hidden sm:inline">{option.label}</span>
              <span className="sm:hidden">
                {option.value === false ? 'All' : 'Tasks'}
              </span>
            </Button>
          )
        })}
      </div>
    </div>
  )
}
