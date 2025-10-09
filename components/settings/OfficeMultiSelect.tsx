'use client'

import { useState, useEffect } from 'react'
import { useQuery } from '@tanstack/react-query'
import { Button } from '@/components/ui/button'
import { Badge } from '@/components/ui/badge'
import {
  Popover,
  PopoverContent,
  PopoverTrigger,
} from '@/components/ui/popover'
import { Input } from '@/components/ui/input'
import { Label } from '@/components/ui/label'
import { Check, ChevronDown, Search, X } from 'lucide-react'
import { getBaseUrl } from '@/lib/utils/baseUrl'
import { cn } from '@/lib/utils/cn'

interface Office {
  id: string
  name: string
  is_active: boolean
  manager_count: number
  region?: string
}

interface OfficeMultiSelectProps {
  value: string[] // Array of office names
  onChange: (offices: string[]) => void
  placeholder?: string
  disabled?: boolean
  maxSelections?: number
}

export function OfficeMultiSelect({
  value = [],
  onChange,
  placeholder = 'Select offices...',
  disabled = false,
  maxSelections,
}: OfficeMultiSelectProps) {
  const [open, setOpen] = useState(false)
  const [searchQuery, setSearchQuery] = useState('')

  // Fetch offices from API
  const { data: offices = [], isLoading } = useQuery({
    queryKey: ['offices'],
    queryFn: async () => {
      const response = await fetch(`${getBaseUrl()}/api/admin/offices`)
      if (!response.ok) throw new Error('Failed to fetch offices')
      return response.json() as Promise<Office[]>
    },
  })

  // Filter offices based on search query
  const filteredOffices = offices.filter((office) =>
    office.name.toLowerCase().includes(searchQuery.toLowerCase())
  )

  const handleToggle = (officeName: string) => {
    const isSelected = value.includes(officeName)
    
    if (isSelected) {
      // Remove office
      onChange(value.filter((name) => name !== officeName))
    } else {
      // Add office (check max selections)
      if (maxSelections && value.length >= maxSelections) {
        return // Don't add if max reached
      }
      onChange([...value, officeName])
    }
  }

  const handleRemove = (officeName: string) => {
    onChange(value.filter((name) => name !== officeName))
  }

  const handleClearAll = () => {
    onChange([])
  }

  return (
    <div className="space-y-2">
      <Popover open={open} onOpenChange={setOpen}>
        <PopoverTrigger asChild>
          <Button
            variant="outline"
            role="combobox"
            aria-expanded={open}
            className="w-full justify-between"
            disabled={disabled}
          >
            <span className="truncate">
              {value.length === 0
                ? placeholder
                : `${value.length} ${value.length === 1 ? 'office' : 'offices'} selected`}
            </span>
            <ChevronDown className="ml-2 h-4 w-4 shrink-0 opacity-50" />
          </Button>
        </PopoverTrigger>
        <PopoverContent className="w-[400px] p-0" align="start">
          <div className="flex flex-col">
            {/* Search input */}
            <div className="border-b p-3">
              <div className="relative">
                <Search className="absolute left-2 top-2.5 h-4 w-4 text-gray-400" />
                <Input
                  placeholder="Search offices..."
                  value={searchQuery}
                  onChange={(e) => setSearchQuery(e.target.value)}
                  className="pl-8"
                />
              </div>
            </div>

            {/* Office list */}
            <div className="max-h-[300px] overflow-y-auto p-2">
              {isLoading ? (
                <div className="py-6 text-center text-sm text-gray-500">
                  Loading offices...
                </div>
              ) : filteredOffices.length === 0 ? (
                <div className="py-6 text-center text-sm text-gray-500">
                  No offices found
                </div>
              ) : (
                filteredOffices.map((office) => {
                  const isSelected = value.includes(office.name)
                  const isDisabled =
                    !isSelected &&
                    maxSelections !== undefined &&
                    value.length >= maxSelections

                  return (
                    <button
                      key={office.id}
                      onClick={() => !isDisabled && handleToggle(office.name)}
                      disabled={isDisabled}
                      className={cn(
                        'flex w-full items-center justify-between rounded-sm px-2 py-2 text-sm hover:bg-gray-100',
                        isSelected && 'bg-gray-50',
                        isDisabled && 'cursor-not-allowed opacity-50'
                      )}
                    >
                      <div className="flex items-center gap-2">
                        <div
                          className={cn(
                            'flex h-4 w-4 items-center justify-center rounded border',
                            isSelected
                              ? 'border-primary bg-primary text-white'
                              : 'border-gray-300'
                          )}
                        >
                          {isSelected && <Check className="h-3 w-3" />}
                        </div>
                        <span>{office.name}</span>
                        {office.region && (
                          <span className="text-xs text-gray-500">({office.region})</span>
                        )}
                      </div>
                      {office.manager_count > 0 && (
                        <Badge variant="outline" className="text-xs">
                          {office.manager_count} {office.manager_count === 1 ? 'manager' : 'managers'}
                        </Badge>
                      )}
                    </button>
                  )
                })
              )}
            </div>

            {/* Footer with clear button */}
            {value.length > 0 && (
              <div className="border-t p-2">
                <Button
                  variant="ghost"
                  size="sm"
                  onClick={handleClearAll}
                  className="w-full"
                >
                  Clear all ({value.length})
                </Button>
              </div>
            )}
          </div>
        </PopoverContent>
      </Popover>

      {/* Selected offices badges */}
      {value.length > 0 && (
        <div className="flex flex-wrap gap-1">
          {value.map((officeName) => (
            <Badge key={officeName} variant="secondary" className="gap-1">
              {officeName}
              <button
                onClick={() => handleRemove(officeName)}
                className="ml-1 rounded-full hover:bg-gray-300"
                disabled={disabled}
              >
                <X className="h-3 w-3" />
              </button>
            </Badge>
          ))}
        </div>
      )}
    </div>
  )
}
