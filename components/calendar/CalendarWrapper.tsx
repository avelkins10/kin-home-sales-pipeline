'use client'

import { useState } from 'react'
import { Calendar, dateFnsLocalizer } from 'react-big-calendar'
import { format, parse, startOfWeek, getDay } from 'date-fns'
import { enUS } from 'date-fns/locale'
import { Button } from '@/components/ui/button'
import { cn } from '@/lib/utils/cn'
import { ChevronLeft, ChevronRight, Calendar as CalendarIcon } from 'lucide-react'

// Create localizer outside component to avoid recreation on each render
const localizer = dateFnsLocalizer({
  format,
  parse,
  startOfWeek: () => startOfWeek(new Date(), { weekStartsOn: 0 }), // Sunday
  getDay,
  locales: { 'en-US': enUS },
})

export interface RbcCalendarEvent {
  id: string
  title: string
  start: Date
  end: Date
  resource?: any
}

interface CalendarWrapperProps {
  events: RbcCalendarEvent[]
  onSelectEvent?: (event: RbcCalendarEvent) => void
  eventPropGetter?: (event: RbcCalendarEvent) => { style?: React.CSSProperties; className?: string }
  className?: string
  components?: any
}

interface CustomToolbarProps {
  label: string
  onNavigate: (action: 'PREV' | 'NEXT' | 'TODAY') => void
  onView: (view: 'month' | 'week' | 'day') => void
  views: string[]
  view: string
}

const CustomToolbar = ({ label, onNavigate, onView, view }: CustomToolbarProps) => {
  return (
    <div className="flex items-center justify-between mb-4">
      {/* Left side: Navigation */}
      <div className="flex items-center gap-2">
        <Button
          variant="outline"
          size="sm"
          onClick={() => onNavigate('PREV')}
          className="h-8 w-8 p-0"
          data-testid="calendar-nav-prev"
        >
          <ChevronLeft className="h-4 w-4" />
        </Button>
        <Button
          variant="outline"
          size="sm"
          onClick={() => onNavigate('TODAY')}
          className="h-8 px-3"
          data-testid="calendar-nav-today"
        >
          Today
        </Button>
        <Button
          variant="outline"
          size="sm"
          onClick={() => onNavigate('NEXT')}
          className="h-8 w-8 p-0"
          data-testid="calendar-nav-next"
        >
          <ChevronRight className="h-4 w-4" />
        </Button>
        <div className="ml-4 text-lg font-semibold text-slate-900" data-testid="calendar-current-label">
          {label}
        </div>
      </div>

      {/* Right side: View toggle */}
      <div className="flex items-center gap-1">
        <Button
          variant={view === 'month' ? 'default' : 'outline'}
          size="sm"
          onClick={() => onView('month')}
          className="h-8 px-3"
          data-testid="calendar-view-month"
        >
          Month
        </Button>
        <Button
          variant={view === 'week' ? 'default' : 'outline'}
          size="sm"
          onClick={() => onView('week')}
          className="h-8 px-3"
          data-testid="calendar-view-week"
        >
          Week
        </Button>
        <Button
          variant={view === 'day' ? 'default' : 'outline'}
          size="sm"
          onClick={() => onView('day')}
          className="h-8 px-3"
          data-testid="calendar-view-day"
        >
          Day
        </Button>
      </div>
    </div>
  )
}

export const CalendarWrapper = ({
  events,
  onSelectEvent,
  eventPropGetter,
  className,
  components
}: CalendarWrapperProps) => {
  const [currentView, setCurrentView] = useState<'month' | 'week' | 'day'>('month')

  const handleViewChange = (view: any) => {
    if (view === 'month' || view === 'week' || view === 'day') {
      setCurrentView(view)
    }
  }

  return (
    <div className={cn('bg-white rounded-lg border shadow-sm p-6', className)} data-testid="calendar-wrapper">
      <Calendar
        localizer={localizer}
        events={events}
        startAccessor="start"
        endAccessor="end"
        view={currentView}
        onView={handleViewChange}
        views={['month', 'week', 'day']}
        components={{ toolbar: CustomToolbar, ...(components || {}) }}
        onSelectEvent={onSelectEvent}
        eventPropGetter={eventPropGetter}
        style={{ height: '700px' }}
        className="h-[500px] md:h-[600px] lg:h-[700px]"
      />
    </div>
  )
}
