'use client'

import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from '@/components/ui/select'
import { Filter } from 'lucide-react'
import { cn } from '@/lib/utils/cn'

interface CalendarFiltersProps {
  eventType: string
  onEventTypeChange: (value: string) => void
  status: string
  onStatusChange: (value: string) => void
  disabled?: boolean
}

export const CalendarFilters = ({
  eventType,
  onEventTypeChange,
  status,
  onStatusChange,
  disabled = false
}: CalendarFiltersProps) => {
  return (
    <div className="flex flex-col sm:flex-row gap-3 mb-6">
      {/* Event Type Filter */}
      <div className="flex-1">
        <label className="block text-sm font-medium text-slate-700 mb-2">
          Event Type
        </label>
        <Select
          value={eventType}
          onValueChange={onEventTypeChange}
          disabled={disabled}
        >
          <SelectTrigger className="w-full bg-white border-slate-200 shadow-sm">
            <SelectValue placeholder="Select event type" />
          </SelectTrigger>
          <SelectContent>
            <SelectItem value="both">Both</SelectItem>
            <SelectItem value="survey">Surveys</SelectItem>
            <SelectItem value="install">Installs</SelectItem>
          </SelectContent>
        </Select>
      </div>

      {/* Status Filter */}
      <div className="flex-1">
        <label className="block text-sm font-medium text-slate-700 mb-2">
          Status
        </label>
        <Select
          value={status}
          onValueChange={onStatusChange}
          disabled={disabled}
        >
          <SelectTrigger className="w-full bg-white border-slate-200 shadow-sm">
            <SelectValue placeholder="Select status" />
          </SelectTrigger>
          <SelectContent>
            <SelectItem value="all">All Statuses</SelectItem>
            <SelectItem value="scheduled">Scheduled</SelectItem>
            <SelectItem value="completed">Completed</SelectItem>
            <SelectItem value="pending">Pending</SelectItem>
          </SelectContent>
        </Select>
      </div>
    </div>
  )
}
