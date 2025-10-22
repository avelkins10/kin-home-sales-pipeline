'use client'

import { useRouter, useSearchParams } from 'next/navigation'
import { Button } from '@/components/ui/button'
import { User, Users } from 'lucide-react'
import { cn } from '@/lib/utils/cn'

interface TaskFilterToggleProps {
  currentFilter: boolean // true if filtering to show only projects with tasks
}

const taskFilterOptions = [
  { value: 'my', label: 'My Tasks', icon: User },
  { value: 'team', label: 'Team Tasks', icon: Users },
]

export function TaskFilterToggle({ currentFilter }: TaskFilterToggleProps) {
  const router = useRouter()
  const searchParams = useSearchParams()

  // Determine current task view based on ownership and withTasks params
  const ownership = searchParams.get('ownership') || 'all'
  const withTasks = searchParams.get('withTasks') === 'true'

  let currentTaskView = ''
  if (withTasks && ownership === 'my-projects') {
    currentTaskView = 'my'
  } else if (withTasks && ownership === 'team-projects') {
    currentTaskView = 'team'
  }

  const handleFilterChange = (taskView: string) => {
    const params = new URLSearchParams(searchParams.toString())

    if (taskView === 'my') {
      params.set('withTasks', 'true')
      params.set('ownership', 'my-projects')
    } else if (taskView === 'team') {
      params.set('withTasks', 'true')
      params.set('ownership', 'team-projects')
    }

    router.push(`/projects?${params.toString()}`)
  }

  return (
    <div className="flex items-center gap-1 mb-4">
      <div
        role="group"
        aria-label="Filter projects by task ownership"
        className="flex rounded-lg border border-gray-300 overflow-hidden"
      >
        {taskFilterOptions.map((option, index) => {
          const isActive = currentTaskView === option.value
          const Icon = option.icon

          return (
            <Button
              key={option.value}
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
                {option.value === 'my' ? 'My' : 'Team'}
              </span>
            </Button>
          )
        })}
      </div>
    </div>
  )
}
