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

interface CalendarPageProps {
  searchParams: {
    eventType?: string
    status?: string
    ownership?: string
  }
}

export default function CalendarPage({ searchParams }: CalendarPageProps) {
  const { data: session, status } = useSession()
  const router = useRouter()
  const searchParamsHook = useSearchParams()
  
  // State management
  const [eventType, setEventType] = useState(searchParams.eventType || 'both')
  const [status, setStatus] = useState(searchParams.status || 'all')
  const ownership = searchParams.ownership || 'all'

  // Authentication check
  if (status === 'loading') {
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

  // Data fetching
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

  // Event styling
  const eventPropGetter = (event: any) => {
    const { resource } = event
    let backgroundColor = ''
    let borderColor = ''
    let color = ''

    switch (resource.status) {
      case 'scheduled':
        backgroundColor = '#dbeafe'
        borderColor = '#3b82f6'
        color = '#1e40af'
        break
      case 'completed':
        backgroundColor = '#dcfce7'
        borderColor = '#22c55e'
        color = '#15803d'
        break
      case 'pending':
        backgroundColor = '#fef3c7'
        borderColor = '#f59e0b'
        color = '#d97706'
        break
      default:
        backgroundColor = '#f1f5f9'
        borderColor = '#64748b'
        color = '#475569'
    }

    return {
      style: {
        backgroundColor,
        borderColor,
        color,
        border: `1px solid ${borderColor}`,
        borderRadius: '4px',
        fontSize: '12px',
        padding: '2px 4px'
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
          <h1 className="text-2xl font-bold text-slate-900">Calendar</h1>
          <p className="text-sm text-slate-600 mt-1">
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

        {/* Calendar content */}
        {isLoading ? (
          <CalendarSkeleton />
        ) : error ? (
          <Alert variant="destructive">
            <AlertCircle className="h-4 w-4" />
            <AlertDescription className="flex items-center justify-between">
              <span>Failed to load calendar events. Please try again.</span>
              <Button
                variant="outline"
                size="sm"
                onClick={() => refetch()}
                className="ml-4"
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
