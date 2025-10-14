'use client'

import { useRouter, useSearchParams } from 'next/navigation'
import { Button } from '@/components/ui/button'
import { User, Users, List } from 'lucide-react'
import { cn } from '@/lib/utils/cn'
import { isManagerRole } from '@/lib/utils/role-helpers'

interface OwnershipFilterToggleProps {
  currentOwnership: string // 'all' | 'my-projects' | 'team-projects'
  userRole: string // User's role to determine available options
}

const ownershipOptions = [
  { value: 'all', label: 'All Projects', icon: List },
  { value: 'my-projects', label: 'My Projects', icon: User },
  { value: 'team-projects', label: 'Team Projects', icon: Users },
]

export function OwnershipFilterToggle({ currentOwnership, userRole }: OwnershipFilterToggleProps) {
  const router = useRouter()
  const searchParams = useSearchParams()

  const handleOwnershipChange = (ownership: string) => {
    const params = new URLSearchParams(searchParams.toString())
    
    if (ownership === 'all') {
      params.delete('ownership')
    } else {
      params.set('ownership', ownership)
    }
    
    router.push(`/projects?${params.toString()}`)
  }

  // Filter options based on user role
  const availableOptions = ownershipOptions.filter(option => {
    if (option.value === 'team-projects') {
      return isManagerRole(userRole)
    }
    return true
  })

  return (
    <div className="flex items-center gap-1 mb-4">
      <div 
        role="group" 
        aria-label="Filter projects by ownership"
        className="flex rounded-lg border border-gray-300 overflow-hidden"
      >
        {availableOptions.map((option, index) => {
          const isActive = currentOwnership === option.value
          const Icon = option.icon
          
          return (
            <Button
              key={option.value}
              variant={isActive ? 'default' : 'outline'}
              size="sm"
              onClick={() => handleOwnershipChange(option.value)}
              className={cn(
                'flex items-center gap-2 px-3 py-2 text-sm font-medium transition-colors',
                'border-0 rounded-none',
                index === 0 && 'rounded-l-lg',
                index === availableOptions.length - 1 && 'rounded-r-lg',
                index > 0 && 'border-l-0',
                isActive 
                  ? 'bg-blue-600 text-white hover:bg-blue-700' 
                  : 'bg-white text-gray-700 hover:bg-gray-50 border-gray-300'
              )}
              aria-pressed={isActive}
            >
              <Icon className="h-4 w-4" />
              <span className="hidden sm:inline">{option.label}</span>
              <span className="sm:hidden">
                {option.value === 'all' ? 'All' : 
                 option.value === 'my-projects' ? 'Mine' : 'Team'}
              </span>
            </Button>
          )
        })}
      </div>
    </div>
  )
}
