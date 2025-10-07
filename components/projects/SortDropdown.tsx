'use client'

import { useRouter, useSearchParams } from 'next/navigation'
import { ArrowUpDown, Clock, Calendar, AlertCircle, Zap } from 'lucide-react'
import {
  DropdownMenu,
  DropdownMenuContent,
  DropdownMenuItem,
  DropdownMenuTrigger,
} from '@/components/ui/dropdown-menu'
import { Button } from '@/components/ui/button'

const SORT_OPTIONS = [
  { value: 'default', label: 'Default (Priority)', icon: Zap },
  { value: 'newest', label: 'Newest First', icon: Calendar },
  { value: 'oldest', label: 'Oldest First', icon: Clock },
  { value: 'age-desc', label: 'Oldest Projects', icon: AlertCircle },
  { value: 'customer-asc', label: 'Customer A-Z', icon: ArrowUpDown },
  { value: 'customer-desc', label: 'Customer Z-A', icon: ArrowUpDown },
]

export function SortDropdown() {
  const router = useRouter()
  const searchParams = useSearchParams()
  const currentSort = searchParams.get('sort') || 'default'

  const handleSortChange = (sortValue: string) => {
    const params = new URLSearchParams(searchParams.toString())

    if (sortValue === 'default') {
      params.delete('sort')
    } else {
      params.set('sort', sortValue)
    }

    router.push(`/projects?${params.toString()}`)
  }

  const currentOption = SORT_OPTIONS.find(opt => opt.value === currentSort) || SORT_OPTIONS[0]
  const Icon = currentOption.icon

  return (
    <DropdownMenu>
      <DropdownMenuTrigger asChild>
        <Button
          variant="outline"
          className="flex items-center gap-2 bg-white"
        >
          <Icon className="h-4 w-4" />
          <span className="hidden sm:inline">Sort:</span>
          <span>{currentOption.label}</span>
        </Button>
      </DropdownMenuTrigger>
      <DropdownMenuContent align="end" className="w-56">
        {SORT_OPTIONS.map((option) => {
          const OptionIcon = option.icon
          return (
            <DropdownMenuItem
              key={option.value}
              onClick={() => handleSortChange(option.value)}
              className={currentSort === option.value ? 'bg-slate-100' : ''}
            >
              <OptionIcon className="mr-2 h-4 w-4" />
              <span>{option.label}</span>
            </DropdownMenuItem>
          )
        })}
      </DropdownMenuContent>
    </DropdownMenu>
  )
}
