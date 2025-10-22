"use client"

import * as React from "react"
import { CalendarIcon } from "lucide-react"
import { addDays, format, startOfMonth, startOfQuarter, startOfYear, subDays } from "date-fns"

import { cn } from "@/lib/utils/cn"
import { Button } from "@/components/ui/button"
import { Calendar } from "@/components/ui/calendar"
import {
  Popover,
  PopoverContent,
  PopoverTrigger,
} from "@/components/ui/popover"

export type DateRange = {
  from: Date | undefined
  to?: Date | undefined
}

interface DateRangePickerProps {
  value?: DateRange
  onChange?: (range: DateRange | undefined) => void
  className?: string
}

export function DateRangePicker({
  value,
  onChange,
  className,
}: DateRangePickerProps) {
  const [date, setDate] = React.useState<DateRange | undefined>(value)

  const handleSelect = (newRange: DateRange | undefined) => {
    setDate(newRange)
    onChange?.(newRange)
  }

  const handlePresetClick = (preset: DateRange) => {
    setDate(preset)
    onChange?.(preset)
  }

  const today = new Date()

  // Use midnight dates to avoid timezone issues
  const todayMidnight = new Date(today.getFullYear(), today.getMonth(), today.getDate());
  const monthStart = new Date(today.getFullYear(), today.getMonth(), 1);
  const yearStart = new Date(today.getFullYear(), 0, 1);
  const quarterStart = new Date(today.getFullYear(), Math.floor(today.getMonth() / 3) * 3, 1);

  const presets = [
    {
      label: "Today",
      range: { from: todayMidnight, to: todayMidnight },
    },
    {
      label: "Yesterday",
      range: { from: subDays(todayMidnight, 1), to: subDays(todayMidnight, 1) },
    },
    {
      label: "Last 7 days",
      range: { from: subDays(todayMidnight, 6), to: todayMidnight },
    },
    {
      label: "Last 30 days",
      range: { from: subDays(todayMidnight, 29), to: todayMidnight },
    },
    {
      label: "This month",
      range: { from: monthStart, to: todayMidnight },
    },
    {
      label: "This quarter",
      range: { from: quarterStart, to: todayMidnight },
    },
    {
      label: "This year",
      range: { from: yearStart, to: todayMidnight },
    },
  ]

  const formatDateRange = (range: DateRange | undefined) => {
    if (!range?.from) {
      return <span>Pick a date range</span>
    }

    if (!range.to) {
      return format(range.from, "LLL dd, yyyy")
    }

    return `${format(range.from, "LLL dd, yyyy")} - ${format(range.to, "LLL dd, yyyy")}`
  }

  return (
    <div className={cn("grid gap-2", className)}>
      <Popover>
        <PopoverTrigger asChild>
          <Button
            id="date"
            variant={"outline"}
            className={cn(
              "w-[300px] justify-start text-left font-normal",
              !date && "text-muted-foreground"
            )}
          >
            <CalendarIcon className="mr-2 h-4 w-4" />
            {formatDateRange(date)}
          </Button>
        </PopoverTrigger>
        <PopoverContent className="w-auto p-0" align="start">
          <div className="flex">
            {/* Preset buttons */}
            <div className="flex flex-col gap-1 p-3 border-r">
              <div className="text-sm font-medium mb-1">Presets</div>
              {presets.map((preset) => (
                <Button
                  key={preset.label}
                  variant="ghost"
                  size="sm"
                  className="justify-start text-xs"
                  onClick={() => handlePresetClick(preset.range)}
                >
                  {preset.label}
                </Button>
              ))}
            </div>
            {/* Calendar */}
            <div className="p-3">
              <Calendar
                initialFocus
                mode="range"
                defaultMonth={date?.from}
                selected={date ? { from: date.from, to: date.to } : undefined}
                onSelect={(range) => handleSelect(range ? { from: range.from, to: range.to } : undefined)}
                numberOfMonths={2}
              />
            </div>
          </div>
        </PopoverContent>
      </Popover>
    </div>
  )
}
