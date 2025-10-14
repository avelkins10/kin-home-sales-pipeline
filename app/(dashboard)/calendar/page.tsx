'use client'

import { useSession } from 'next-auth/react'
import { redirect } from 'next/navigation'
import { useState, useMemo } from 'react'
import { useQuery } from '@tanstack/react-query'
import { useRouter, useSearchParams } from 'next/navigation'
import { parseISO } from 'date-fns'
import { CalendarWrapper } from '@/components/calendar/CalendarWrapper'
import { CalendarFilters } from '@/components/calendar/CalendarFilters'
import { CalendarSkeleton } from '@/components/calendar/CalendarSkeleton'
import { CalendarEventContent } from '@/components/calendar/CalendarEventContent'
import { calendarEventsKey } from '@/lib/queryKeys'
import { CalendarEvent } from '@/lib/utils/calendar-helpers'
import { Alert, AlertDescription } from '@/components/ui/alert'
import { Button } from '@/components/ui/button'
import { AlertCircle, RefreshCw } from 'lucide-react'
import { eventColorMap, statusColorMap } from '@/lib/constants/designTokens'

interface CalendarPageProps {
  searchParams: {
    eventType?: string
    status?: string
    ownership?: string
  }
}

export default function CalendarPage({ searchParams }: CalendarPageProps) {
  const { data: session, status: authStatus } = useSession()
  const router = useRouter()
  const searchParamsHook = useSearchParams()

  // State management
  const [eventType, setEventType] = useState(searchParams.eventType || 'both')
  const [status, setStatus] = useState(searchParams.status || 'all')
  const ownership = searchParams.ownership || 'all'

  // Data fetching - called before any returns
  const {
    data: events = [],
    isLoading,
    error,
    refetch
  } = useQuery({
    queryKey: calendarEventsKey({ ownership }),
    queryFn: async () => {
      const params = new URLSearchParams()
      if (ownership !== 'all') params.append('ownership', ownership)

      const response = await fetch(`/api/calendar/events?${params.toString()}`)
      if (!response.ok) {
        throw new Error('Failed to fetch calendar events')
      }
      return response.json()
    },
    enabled: !!session,
    staleTime: 60000, // 1 minute
  })

  // Client-side filtering
  const filteredEvents = useMemo(() => {
    return events.filter((event: CalendarEvent) => {
      const matchesEventType = eventType === 'both' || event.type === eventType
      const matchesStatus = status === 'all' || event.status === status
      return matchesEventType && matchesStatus
    })
  }, [events, eventType, status])

  // Transform events for react-big-calendar
  const transformedEvents = useMemo(() => {
    return filteredEvents.map((event: CalendarEvent) => {
      const start = parseISO(event.date)
      return {
        id: event.id,
        title: event.title,
        start: start,
        end: start,
        resource: event
      }
    })
  }, [filteredEvents])

  // Authentication check - after all hooks
  if (authStatus === 'loading') {
    return (
      <div className="min-h-screen bg-slate-50 flex items-center justify-center">
        <div className="text-center">
          <div className="animate-spin rounded-full h-8 w-8 border-b-2 border-blue-600 mx-auto mb-4"></div>
          <p className="text-slate-600">Loading...</p>
        </div>
      </div>
    )
  }

  if (!session) {
    redirect('/login')
  }

  // Event styling - color coordinated by both type (survey/install) and status
  const eventPropGetter = (event: any) => {
    const { resource } = event
    const eventType = resource.type as keyof typeof eventColorMap
    const eventStatus = resource.status as 'scheduled' | 'completed' | 'pending'

    // Get colors based on both type and status
    const colors = eventColorMap[eventType]?.[eventStatus] || statusColorMap.default

    return {
      style: {
        backgroundColor: colors.background,
        borderColor: colors.border,
        color: colors.text,
        border: `2px solid ${colors.border}`,
        borderRadius: '4px',
        fontSize: '12px',
        padding: '2px 4px',
        fontWeight: '500'
      }
    }
  }

  // Event click handler
  const handleEventClick = (event: any) => {
    const recordId = event.resource.project.recordId
    if (recordId) {
      router.push(`/projects/${recordId}`)
    }
  }

  return (
    <div className="min-h-screen bg-slate-50">
      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
        {/* Page header */}
        <div className="mb-8">
          <h1 data-testid="calendar-page-title" className="text-2xl font-bold text-slate-900">Calendar</h1>
          <p data-testid="calendar-page-subtitle" className="text-sm text-slate-600 mt-1">
            View scheduled site surveys and installations
          </p>
        </div>

        {/* Filters */}
        <CalendarFilters
          eventType={eventType}
          onEventTypeChange={setEventType}
          status={status}
          onStatusChange={setStatus}
          disabled={isLoading}
        />

        {/* Color Legend */}
        <div className="mb-4 bg-white rounded-lg border border-slate-200 p-4">
          <h3 className="text-sm font-semibold text-slate-700 mb-3">Color Legend</h3>
          <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
            {/* Surveys */}
            <div>
              <p className="text-xs font-medium text-slate-600 mb-2">Site Surveys</p>
              <div className="flex flex-wrap gap-2">
                <div className="flex items-center gap-2">
                  <div className="w-4 h-4 rounded" style={{ backgroundColor: eventColorMap.survey.scheduled.background, border: `2px solid ${eventColorMap.survey.scheduled.border}` }} />
                  <span className="text-xs text-slate-600">Scheduled</span>
                </div>
                <div className="flex items-center gap-2">
                  <div className="w-4 h-4 rounded" style={{ backgroundColor: eventColorMap.survey.completed.background, border: `2px solid ${eventColorMap.survey.completed.border}` }} />
                  <span className="text-xs text-slate-600">Completed</span>
                </div>
                <div className="flex items-center gap-2">
                  <div className="w-4 h-4 rounded" style={{ backgroundColor: eventColorMap.survey.pending.background, border: `2px solid ${eventColorMap.survey.pending.border}` }} />
                  <span className="text-xs text-slate-600">Pending</span>
                </div>
              </div>
            </div>
            {/* Installs */}
            <div>
              <p className="text-xs font-medium text-slate-600 mb-2">Installations</p>
              <div className="flex flex-wrap gap-2">
                <div className="flex items-center gap-2">
                  <div className="w-4 h-4 rounded" style={{ backgroundColor: eventColorMap.install.scheduled.background, border: `2px solid ${eventColorMap.install.scheduled.border}` }} />
                  <span className="text-xs text-slate-600">Scheduled</span>
                </div>
                <div className="flex items-center gap-2">
                  <div className="w-4 h-4 rounded" style={{ backgroundColor: eventColorMap.install.completed.background, border: `2px solid ${eventColorMap.install.completed.border}` }} />
                  <span className="text-xs text-slate-600">Completed</span>
                </div>
                <div className="flex items-center gap-2">
                  <div className="w-4 h-4 rounded" style={{ backgroundColor: eventColorMap.install.pending.background, border: `2px solid ${eventColorMap.install.pending.border}` }} />
                  <span className="text-xs text-slate-600">Pending</span>
                </div>
              </div>
            </div>
          </div>
        </div>

        {/* Calendar content */}
        {isLoading ? (
          <CalendarSkeleton />
        ) : error ? (
          <Alert variant="destructive" data-testid="calendar-error-alert">
            <AlertCircle className="h-4 w-4" />
            <AlertDescription className="flex items-center justify-between">
              <span>Failed to load calendar events. Please try again.</span>
              <Button
                variant="outline"
                size="sm"
                onClick={() => refetch()}
                className="ml-4"
                data-testid="calendar-retry-button"
              >
                <RefreshCw className="h-4 w-4 mr-2" />
                Retry
              </Button>
            </AlertDescription>
          </Alert>
        ) : (
          <CalendarWrapper
            events={transformedEvents}
            onSelectEvent={handleEventClick}
            eventPropGetter={eventPropGetter}
            components={{ event: CalendarEventContent }}
          />
        )}
      </div>
    </div>
  )
}
