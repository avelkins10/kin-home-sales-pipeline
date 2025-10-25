'use client';

import { useState, useMemo } from 'react';
import { useQuery } from '@tanstack/react-query';
import { Card } from '@/components/ui/card';
import { Button } from '@/components/ui/button';
import { Badge } from '@/components/ui/badge';
import { 
  ChevronLeft, 
  ChevronRight, 
  Calendar, 
  Clock, 
  MapPin, 
  Phone,
  ChevronDown
} from 'lucide-react';
import { useMediaQuery } from '@/lib/hooks/useMediaQuery';
import { cn } from '@/lib/utils';
import { ProjectDetailModal } from './ProjectDetailModal';
import type { PCCalendarEvent, PCCalendarView } from '@/lib/types/operations';

interface PCCalendarProps {
  pcEmail: string;
  view?: PCCalendarView;
  onEventClick?: (event: PCCalendarEvent) => void;
}

export function PCCalendar({ 
  pcEmail, 
  view: initialView = 'month',
  onEventClick 
}: PCCalendarProps) {
  const [currentDate, setCurrentDate] = useState(new Date());
  const [selectedView, setSelectedView] = useState<PCCalendarView>(initialView);
  const [selectedEvent, setSelectedEvent] = useState<PCCalendarEvent | null>(null);
  const isMobile = useMediaQuery('(max-width: 768px)');

  // Auto-switch to agenda view on mobile
  const displayView = isMobile && selectedView === 'month' ? 'agenda' : selectedView;

  // Calculate date range for API call
  const { startDate, endDate } = useMemo(() => {
    const start = new Date(currentDate);
    const end = new Date(currentDate);
    
    switch (displayView) {
      case 'month':
        start.setDate(1);
        end.setMonth(end.getMonth() + 1);
        end.setDate(0);
        break;
      case 'week':
        const dayOfWeek = start.getDay();
        start.setDate(start.getDate() - dayOfWeek);
        end.setTime(start.getTime());
        end.setDate(end.getDate() + 6);
        break;
      case 'day':
        end.setDate(start.getDate() + 1);
        break;
      case 'agenda':
        start.setDate(start.getDate() - 7);
        end.setDate(start.getDate() + 30);
        break;
    }
    
    return {
      startDate: start.toISOString().split('T')[0],
      endDate: end.toISOString().split('T')[0]
    };
  }, [currentDate, displayView]);

  // Fetch calendar events
  const { data: events = [], isLoading, error } = useQuery<PCCalendarEvent[]>({
    queryKey: ['pc-calendar', pcEmail, startDate, endDate],
    queryFn: async () => {
      const response = await fetch(
        `/api/operations/calendar?pcEmail=${pcEmail}&startDate=${startDate}&endDate=${endDate}`
      );
      if (!response.ok) throw new Error('Failed to fetch calendar events');
      const data = await response.json();
      
      // Normalize date fields from strings to Date objects
      const normalizedEvents = (data.events || []).map((event: any) => ({
        ...event,
        start: new Date(event.start),
        end: new Date(event.end)
      }));
      
      return normalizedEvents;
    },
    enabled: !!pcEmail
  });

  // Navigation functions
  const goToPrevious = () => {
    const newDate = new Date(currentDate);
    switch (displayView) {
      case 'month':
        newDate.setMonth(newDate.getMonth() - 1);
        break;
      case 'week':
        newDate.setDate(newDate.getDate() - 7);
        break;
      case 'day':
        newDate.setDate(newDate.getDate() - 1);
        break;
    }
    setCurrentDate(newDate);
  };

  const goToNext = () => {
    const newDate = new Date(currentDate);
    switch (displayView) {
      case 'month':
        newDate.setMonth(newDate.getMonth() + 1);
        break;
      case 'week':
        newDate.setDate(newDate.getDate() + 7);
        break;
      case 'day':
        newDate.setDate(newDate.getDate() + 1);
        break;
    }
    setCurrentDate(newDate);
  };

  const goToToday = () => {
    setCurrentDate(new Date());
  };

  // Event handlers
  const handleEventClick = (event: PCCalendarEvent) => {
    setSelectedEvent(event);
    onEventClick?.(event);
  };

  const getEventColor = (event: PCCalendarEvent) => {
    switch (event.type) {
      case 'install':
        return 'bg-green-100 text-green-800 border-green-200';
      case 'survey':
        return 'bg-blue-100 text-blue-800 border-blue-200';
      case 'outreach':
        return 'bg-orange-100 text-orange-800 border-orange-200';
      case 'appointment':
        return 'bg-purple-100 text-purple-800 border-purple-200';
      case 'site_visit':
        return 'bg-yellow-100 text-yellow-800 border-yellow-200';
      default:
        return 'bg-gray-100 text-gray-800 border-gray-200';
    }
  };

  const getStatusColor = (status: string) => {
    switch (status) {
      case 'overdue':
        return 'bg-red-500';
      case 'completed':
        return 'bg-green-500';
      case 'scheduled':
        return 'bg-blue-500';
      default:
        return 'bg-gray-500';
    }
  };

  // Render month view
  const renderMonthView = () => {
    const year = currentDate.getFullYear();
    const month = currentDate.getMonth();
    const firstDay = new Date(year, month, 1);
    const lastDay = new Date(year, month + 1, 0);
    const startDate = new Date(firstDay);
    startDate.setDate(startDate.getDate() - firstDay.getDay());
    
    const days = [];
    const current = new Date(startDate);
    
    for (let i = 0; i < 42; i++) {
      const dayEvents = events.filter(event => {
        const eventDate = new Date(event.start);
        return eventDate.toDateString() === current.toDateString();
      });
      
      days.push(
        <div
          key={i}
          className={cn(
            "min-h-[100px] p-2 border-b border-r border-gray-200",
            current.getMonth() !== month && "text-gray-400 bg-gray-50",
            current.toDateString() === new Date().toDateString() && "bg-blue-50"
          )}
        >
          <div className="text-sm font-medium mb-1">
            {current.getDate()}
          </div>
          <div className="space-y-1">
            {dayEvents.slice(0, 3).map((event, index) => (
              <div
                key={index}
                className={cn(
                  "text-xs p-1 rounded cursor-pointer hover:opacity-80",
                  getEventColor(event)
                )}
                onClick={() => handleEventClick(event)}
              >
                <div className="flex items-center gap-1">
                  <div className={cn("w-2 h-2 rounded-full", getStatusColor(event.status))} />
                  <span className="truncate">{event.title}</span>
                </div>
              </div>
            ))}
            {dayEvents.length > 3 && (
              <div className="text-xs text-gray-500">
                +{dayEvents.length - 3} more
              </div>
            )}
          </div>
        </div>
      );
      current.setDate(current.getDate() + 1);
    }
    
    return (
      <div className="grid grid-cols-7">
        {['Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat'].map(day => (
          <div key={day} className="p-2 text-center font-medium text-gray-600 border-b border-gray-200">
            {day}
          </div>
        ))}
        {days}
      </div>
    );
  };

  // Render week view
  const renderWeekView = () => {
    const startOfWeek = new Date(currentDate);
    const dayOfWeek = startOfWeek.getDay();
    startOfWeek.setDate(startOfWeek.getDate() - dayOfWeek);
    
    const weekDays = [];
    for (let i = 0; i < 7; i++) {
      const day = new Date(startOfWeek);
      day.setDate(startOfWeek.getDate() + i);
      weekDays.push(day);
    }

    return (
      <div className="grid grid-cols-7 min-h-[600px]">
        {weekDays.map((day, index) => {
          const dayEvents = events.filter(event => {
            const eventDate = new Date(event.start);
            return eventDate.toDateString() === day.toDateString();
          });

          return (
            <div key={index} className="border-r border-gray-200 last:border-r-0">
              <div className="p-2 border-b border-gray-200 bg-gray-50">
                <div className="text-sm font-medium text-gray-600">
                  {day.toLocaleDateString('en-US', { weekday: 'short' })}
                </div>
                <div className="text-lg font-semibold">
                  {day.getDate()}
                </div>
              </div>
              <div className="p-2 space-y-1 min-h-[500px]">
                {dayEvents.map((event, eventIndex) => (
                  <div
                    key={eventIndex}
                    className={cn(
                      "text-xs p-2 rounded cursor-pointer hover:opacity-80",
                      getEventColor(event)
                    )}
                    onClick={() => handleEventClick(event)}
                  >
                    <div className="flex items-center gap-1 mb-1">
                      <div className={cn("w-2 h-2 rounded-full", getStatusColor(event.status))} />
                      <span className="font-medium truncate">{event.title}</span>
                    </div>
                    <div className="text-xs opacity-75">
                      {new Date(event.start).toLocaleTimeString([], { hour: '2-digit', minute: '2-digit' })}
                    </div>
                    {event.customerName && (
                      <div className="text-xs opacity-75 truncate">
                        {event.customerName}
                      </div>
                    )}
                  </div>
                ))}
              </div>
            </div>
          );
        })}
      </div>
    );
  };

  // Render day view
  const renderDayView = () => {
    const dayEvents = events.filter(event => {
      const eventDate = new Date(event.start);
      return eventDate.toDateString() === currentDate.toDateString();
    });

    const sortedEvents = dayEvents.sort((a, b) => 
      new Date(a.start).getTime() - new Date(b.start).getTime()
    );

    if (sortedEvents.length === 0) {
      return (
        <div className="text-center py-12">
          <Calendar className="h-12 w-12 text-gray-400 mx-auto mb-4" />
          <h3 className="text-lg font-medium text-gray-900 mb-2">
            No events scheduled
          </h3>
          <p className="text-gray-600">
            No events scheduled for {currentDate.toLocaleDateString()}
          </p>
        </div>
      );
    }

    return (
      <div className="space-y-3">
        {sortedEvents.map((event) => (
          <Card
            key={event.id}
            className="p-4 cursor-pointer hover:shadow-md transition-shadow"
            onClick={() => handleEventClick(event)}
          >
            <div className="flex items-start gap-3">
              <div className={cn("w-3 h-3 rounded-full mt-2", getStatusColor(event.status))} />
              <div className="flex-1 min-w-0">
                <div className="flex items-center gap-2 mb-2">
                  <h4 className="font-medium text-gray-900 truncate">
                    {event.title}
                  </h4>
                  <Badge variant="outline" className={getEventColor(event)}>
                    {event.type}
                  </Badge>
                </div>
                <div className="flex items-center gap-4 text-sm text-gray-600 mb-2">
                  <div className="flex items-center gap-1">
                    <Clock className="h-4 w-4" />
                    {new Date(event.start).toLocaleTimeString([], { hour: '2-digit', minute: '2-digit' })}
                    {event.end && (
                      <span> - {new Date(event.end).toLocaleTimeString([], { hour: '2-digit', minute: '2-digit' })}</span>
                    )}
                  </div>
                  {event.location && (
                    <div className="flex items-center gap-1">
                      <MapPin className="h-4 w-4" />
                      {event.location}
                    </div>
                  )}
                </div>
                {event.customerName && (
                  <div className="text-sm text-gray-600 mb-2">
                    Customer: {event.customerName}
                  </div>
                )}
                {event.notes && (
                  <div className="text-sm text-gray-600">
                    {event.notes}
                  </div>
                )}
              </div>
            </div>
          </Card>
        ))}
      </div>
    );
  };

  // Render agenda view
  const renderAgendaView = () => {
    const sortedEvents = [...events].sort((a, b) => 
      new Date(a.start).getTime() - new Date(b.start).getTime()
    );

    if (sortedEvents.length === 0) {
      return (
        <div className="text-center py-12">
          <Calendar className="h-12 w-12 text-gray-400 mx-auto mb-4" />
          <h3 className="text-lg font-medium text-gray-900 mb-2">
            No scheduled activities
          </h3>
          <p className="text-gray-600">
            No scheduled activities for this period
          </p>
        </div>
      );
    }

    return (
      <div className="space-y-4">
        {sortedEvents.map((event) => (
          <Card
            key={event.id}
            className="p-4 cursor-pointer hover:shadow-md transition-shadow"
            onClick={() => handleEventClick(event)}
          >
            <div className="flex items-start gap-3">
              <div className={cn("w-3 h-3 rounded-full mt-2", getStatusColor(event.status))} />
              <div className="flex-1 min-w-0">
                <div className="flex items-center gap-2 mb-1">
                  <h4 className="font-medium text-gray-900 truncate">
                    {event.title}
                  </h4>
                  <Badge variant="outline" className={getEventColor(event)}>
                    {event.type}
                  </Badge>
                </div>
                <div className="flex items-center gap-4 text-sm text-gray-600">
                  <div className="flex items-center gap-1">
                    <Clock className="h-4 w-4" />
                    {new Date(event.start).toLocaleDateString()} at {new Date(event.start).toLocaleTimeString([], { hour: '2-digit', minute: '2-digit' })}
                  </div>
                  {event.location && (
                    <div className="flex items-center gap-1">
                      <MapPin className="h-4 w-4" />
                      {event.location}
                    </div>
                  )}
                </div>
                {event.customerName && (
                  <div className="mt-2 text-sm text-gray-600">
                    Customer: {event.customerName}
                  </div>
                )}
              </div>
            </div>
          </Card>
        ))}
      </div>
    );
  };

  if (error) {
    return (
      <Card className="p-6">
        <div className="text-center">
          <h3 className="text-lg font-medium text-red-600 mb-2">
            Error loading calendar
          </h3>
          <p className="text-gray-600">
            Failed to load calendar events. Please try again.
          </p>
        </div>
      </Card>
    );
  }

  return (
    <div className="space-y-4">
      {/* Header */}
      <Card className="p-4">
        <div className="flex flex-col mobile:flex-row mobile:items-center mobile:justify-between gap-3">
          <div className="flex items-center gap-2">
            <h2 className="text-lg mobile:text-xl font-semibold">
              {currentDate.toLocaleDateString('en-US', { 
                month: 'long', 
                year: 'numeric' 
              })}
            </h2>
            {!isMobile && (
              <div className="flex items-center gap-1">
                <Button
                  variant="outline"
                  size="sm"
                  onClick={() => setSelectedView('month')}
                  className={selectedView === 'month' ? 'bg-blue-50' : ''}
                >
                  Month
                </Button>
                <Button
                  variant="outline"
                  size="sm"
                  onClick={() => setSelectedView('week')}
                  className={selectedView === 'week' ? 'bg-blue-50' : ''}
                >
                  Week
                </Button>
                <Button
                  variant="outline"
                  size="sm"
                  onClick={() => setSelectedView('day')}
                  className={selectedView === 'day' ? 'bg-blue-50' : ''}
                >
                  Day
                </Button>
                <Button
                  variant="outline"
                  size="sm"
                  onClick={() => setSelectedView('agenda')}
                  className={selectedView === 'agenda' ? 'bg-blue-50' : ''}
                >
                  Agenda
                </Button>
              </div>
            )}
          </div>
          
          <div className="flex items-center gap-2">
            <Button
              variant="outline"
              size="sm"
              onClick={goToPrevious}
              className="h-8 w-8 p-0"
            >
              <ChevronLeft className="h-4 w-4" />
            </Button>
            <Button
              variant="outline"
              size="sm"
              onClick={goToToday}
              className="text-sm"
            >
              Today
            </Button>
            <Button
              variant="outline"
              size="sm"
              onClick={goToNext}
              className="h-8 w-8 p-0"
            >
              <ChevronRight className="h-4 w-4" />
            </Button>
          </div>
        </div>
      </Card>

      {/* Calendar Content */}
      <Card className="p-0">
        {isLoading ? (
          <div className="p-6 text-center">
            <div className="animate-spin rounded-full h-8 w-8 border-b-2 border-blue-600 mx-auto"></div>
            <p className="mt-2 text-gray-600">Loading calendar events...</p>
          </div>
        ) : displayView === 'agenda' ? (
          <div className="p-4">
            {renderAgendaView()}
          </div>
        ) : displayView === 'week' ? (
          <div className="overflow-x-auto">
            {renderWeekView()}
          </div>
        ) : displayView === 'day' ? (
          <div className="p-4">
            {renderDayView()}
          </div>
        ) : (
          <div className="overflow-x-auto">
            {renderMonthView()}
          </div>
        )}
      </Card>

      {/* Event Detail Modal */}
      {selectedEvent && (
        <ProjectDetailModal
          recordId={selectedEvent.recordId}
          isOpen={!!selectedEvent}
          onClose={() => setSelectedEvent(null)}
        />
      )}
    </div>
  );
}
