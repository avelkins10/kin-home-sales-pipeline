'use client';

import { useState, useCallback, useEffect } from 'react';
import { useQuery } from '@tanstack/react-query';
import { ScrollArea } from '@/components/ui/scroll-area';
import { Badge } from '@/components/ui/badge';
import { Button } from '@/components/ui/button';
import { Skeleton } from '@/components/ui/skeleton';
import { Truck, Play, CheckCircle, AlertTriangle, Clock, ExternalLink, RefreshCw } from 'lucide-react';
import type { FieldTrackingEvent, ActivityFeedFilters, ActivityFeedResponse } from '@/lib/types/operations';
import { formatDistanceToNow } from 'date-fns';
import { FieldTrackingActivityFilters } from './FieldTrackingActivityFilters';

interface FieldTrackingActivityFeedProps {
  onTaskClick?: (taskId: string) => void;
  coordinatorEmail?: string;
}

const EVENTS_PER_PAGE = 20;

export function FieldTrackingActivityFeed({ 
  onTaskClick,
  coordinatorEmail,
}: FieldTrackingActivityFeedProps) {
  const [filters, setFilters] = useState<ActivityFeedFilters>({
    eventType: 'all',
    dateRange: null,
    crewMember: 'all',
    taskType: 'all',
    search: '',
  });
  const [offset, setOffset] = useState(0);
  const [showFilters, setShowFilters] = useState(false);
  const [allEvents, setAllEvents] = useState<FieldTrackingEvent[]>([]);

  // Fetch crew members for filter dropdown
  const { data: crewMembers = [] } = useQuery<string[]>({
    queryKey: ['crew-members'],
    queryFn: async () => {
      const response = await fetch('/api/operations/field-tracking/crew-members');
      if (!response.ok) throw new Error('Failed to fetch crew members');
      return response.json();
    },
  });

  // Available task types (hardcoded for now, can be fetched dynamically if needed)
  const availableTaskTypes = ['Survey', 'Install', 'Inspection', 'Service'];

  // Build query params for API call
  const buildQueryParams = useCallback(() => {
    const params = new URLSearchParams();
    
    if (filters.eventType !== 'all') {
      params.append('eventType', filters.eventType);
    }
    
    if (filters.dateRange?.start) {
      params.append('startDate', filters.dateRange.start.toISOString());
      if (filters.dateRange.end) {
        params.append('endDate', filters.dateRange.end.toISOString());
      }
    }
    
    if (filters.crewMember !== 'all') {
      params.append('reporterName', filters.crewMember);
    }
    
    if (filters.taskType !== 'all') {
      params.append('taskType', filters.taskType);
    }
    
    if (filters.search) {
      params.append('search', filters.search);
    }
    
    params.append('limit', EVENTS_PER_PAGE.toString());
    params.append('offset', offset.toString());
    
    return params;
  }, [filters, offset]);

  // Fetch events with React Query
  const { data, isLoading, isFetching, error, refetch } = useQuery<ActivityFeedResponse>({
    queryKey: ['field-tracking-events', filters, offset],
    queryFn: async () => {
      const params = buildQueryParams();
      const response = await fetch(`/api/operations/field-tracking/events?${params}`);
      if (!response.ok) throw new Error('Failed to fetch events');
      return response.json();
    },
    refetchInterval: offset === 0 ? 30000 : false, // Auto-refresh only first page every 30 seconds
    keepPreviousData: true,
  });

  // Combine paginated results
  useEffect(() => {
    if (data?.events) {
      if (offset === 0) {
        // Reset on new filter
        setAllEvents(data.events);
      } else {
        // Append for pagination
        setAllEvents((prev) => [...prev, ...data.events]);
      }
    }
  }, [data, offset]);

  // Filter change handler
  const handleFiltersChange = useCallback((newFilters: ActivityFeedFilters) => {
    setFilters(newFilters);
    setOffset(0); // Reset pagination on filter change
  }, []);

  // Load more handler
  const handleLoadMore = useCallback(() => {
    setOffset((prev) => prev + EVENTS_PER_PAGE);
  }, []);

  // Event click handler
  const handleEventClick = useCallback((event: FieldTrackingEvent) => {
    if (event.arrivy_task_id && onTaskClick) {
      // Use quickbase_project_id if available, otherwise use arrivy_task_id
      const taskId = event.quickbase_project_id || event.arrivy_task_id.toString();
      onTaskClick(taskId);
    }
  }, [onTaskClick]);

  // Get icon for event type
  const getEventIcon = (eventType: string, eventSubType: string | null) => {
    if (eventSubType === 'ENROUTE') return Truck;
    if (eventSubType === 'STARTED') return Play;
    if (eventSubType === 'COMPLETE') return CheckCircle;
    if (eventType === 'LATE' || eventType === 'NOSHOW' || eventType === 'EXCEPTION' || eventType === 'CANCELLED') {
      return AlertTriangle;
    }
    return Clock;
  };

  // Get color for event type
  const getEventColor = (eventType: string, eventSubType: string | null) => {
    if (eventSubType === 'ENROUTE') return 'text-blue-600';
    if (eventSubType === 'STARTED') return 'text-yellow-600';
    if (eventSubType === 'COMPLETE') return 'text-green-600';
    if (eventType === 'LATE' || eventType === 'NOSHOW' || eventType === 'EXCEPTION' || eventType === 'CANCELLED') {
      return 'text-red-600';
    }
    return 'text-gray-600';
  };

  // Check if event is critical
  const isCriticalEvent = (eventType: string): boolean => {
    return ['LATE', 'NOSHOW', 'EXCEPTION', 'CANCELLED'].includes(eventType);
  };

  // Loading skeleton
  if (isLoading && offset === 0) {
    return (
      <div className="space-y-4">
        {[...Array(5)].map((_, i) => (
          <div key={i} className="flex gap-3 pb-4 border-b">
            <Skeleton className="h-5 w-5 rounded" />
            <div className="flex-1 space-y-2">
              <Skeleton className="h-4 w-3/4" />
              <Skeleton className="h-3 w-1/2" />
              <Skeleton className="h-3 w-1/4" />
            </div>
          </div>
        ))}
      </div>
    );
  }

  // Error state
  if (error) {
    return (
      <div className="text-center py-8 space-y-3">
        <p className="text-red-600 text-sm">Failed to load activity feed.</p>
        <Button variant="outline" size="sm" onClick={() => refetch()}>
          Try Again
        </Button>
      </div>
    );
  }

  // Empty state
  if (allEvents.length === 0 && !isLoading) {
    const hasActiveFilters = filters.eventType !== 'all' || filters.crewMember !== 'all' || 
                             filters.taskType !== 'all' || filters.search || filters.dateRange;
    
    return (
      <div>
        <FieldTrackingActivityFilters
          filters={filters}
          onFiltersChange={handleFiltersChange}
          availableCrewMembers={crewMembers}
          availableTaskTypes={availableTaskTypes}
          showFilters={showFilters}
          onToggleFilters={() => setShowFilters(!showFilters)}
        />
        <div className="text-center text-gray-500 py-8 mt-4">
          {hasActiveFilters 
            ? 'No events match your filters. Try adjusting your search criteria.'
            : 'No recent activity. Events will appear here as they occur.'}
        </div>
      </div>
    );
  }

  return (
    <div className="space-y-4">
      {/* Filters */}
      <FieldTrackingActivityFilters
        filters={filters}
        onFiltersChange={handleFiltersChange}
        availableCrewMembers={crewMembers}
        availableTaskTypes={availableTaskTypes}
        showFilters={showFilters}
        onToggleFilters={() => setShowFilters(!showFilters)}
      />

      {/* Auto-refresh indicator */}
      {isFetching && offset === 0 && allEvents.length > 0 && (
        <div className="flex items-center justify-center gap-2 text-xs text-gray-500 py-2">
          <RefreshCw className="h-3 w-3 animate-spin" />
          Refreshing...
        </div>
      )}

      {/* Event list */}
      <ScrollArea className="h-[500px] pr-4">
        <div className="space-y-4">
          {allEvents.map((event) => {
            const Icon = getEventIcon(event.event_type, event.event_sub_type);
            const color = getEventColor(event.event_type, event.event_sub_type);
            const isCritical = isCriticalEvent(event.event_type);
            const hasTaskLink = !!event.arrivy_task_id;

            return (
              <div
                key={event.id}
                onClick={() => hasTaskLink && handleEventClick(event)}
                className={`flex gap-3 pb-4 border-b last:border-0 relative ${
                  hasTaskLink ? 'cursor-pointer hover:bg-gray-50 transition-colors rounded-lg p-2 -m-2' : ''
                } ${isCritical ? 'bg-red-50 border-l-4 border-red-500 pl-4' : ''}`}
                role={hasTaskLink ? 'button' : undefined}
                tabIndex={hasTaskLink ? 0 : undefined}
                onKeyDown={(e) => {
                  if (hasTaskLink && (e.key === 'Enter' || e.key === ' ')) {
                    e.preventDefault();
                    handleEventClick(event);
                  }
                }}
                aria-label={hasTaskLink ? `View task details for ${event.title || event.event_type}` : undefined}
              >
                <div className={`flex-shrink-0 ${color}`}>
                  <Icon className="h-5 w-5" />
                </div>
                <div className="flex-1 min-w-0">
                  <div className="flex items-start justify-between gap-2">
                    <div className="flex-1">
                      <p className="font-medium text-sm">{event.title || event.event_type}</p>
                      {event.message && (
                        <p className="text-sm text-gray-600 mt-1">{event.message}</p>
                      )}
                      {event.reporter_name && (
                        <p className="text-xs text-gray-500 mt-1">
                          By {event.reporter_name}
                        </p>
                      )}
                    </div>
                    <div className="flex items-center gap-2 flex-shrink-0">
                      {event.event_type && (
                        <Badge variant="outline" className="text-xs">
                          {event.event_type}
                        </Badge>
                      )}
                      {event.event_sub_type && (
                        <Badge variant="outline" className="text-xs">
                          {event.event_sub_type}
                        </Badge>
                      )}
                      {hasTaskLink && (
                        <ExternalLink className="h-4 w-4 text-gray-400" title="Click to view task details" />
                      )}
                    </div>
                  </div>
                  <p className="text-xs text-gray-400 mt-1">
                    {formatDistanceToNow(new Date(event.timestamp), { addSuffix: true })}
                  </p>
                </div>
              </div>
            );
          })}
        </div>
      </ScrollArea>

      {/* Pagination controls */}
      {data && (
        <div className="pt-4 border-t space-y-3">
          <div className="text-sm text-gray-600 text-center">
            Showing {allEvents.length} of {data.total} events
          </div>
          {data.hasMore && (
            <Button
              variant="outline"
              onClick={handleLoadMore}
              disabled={isFetching}
              className="w-full"
            >
              {isFetching ? (
                <>
                  <RefreshCw className="mr-2 h-4 w-4 animate-spin" />
                  Loading...
                </>
              ) : (
                'Load More Events'
              )}
            </Button>
          )}
        </div>
      )}
    </div>
  );
}
