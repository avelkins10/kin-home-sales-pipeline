'use client';

import { useMemo } from 'react';
import { Card } from '@/components/ui/card';
import { Badge } from '@/components/ui/badge';
import { Button } from '@/components/ui/button';
import { ChevronLeft, ChevronRight, Clock, Paperclip, RotateCcw } from 'lucide-react';
import { format, startOfWeek, endOfWeek, startOfMonth, endOfMonth, eachDayOfInterval, addDays, addWeeks, addMonths, subDays, subWeeks, subMonths, isToday, isSameMonth, getHours, getMinutes, setHours, startOfDay } from 'date-fns';
import { cn } from '@/lib/utils';
import { AppointmentData } from './AppointmentCard';

type ViewMode = 'day' | 'week' | 'month';

interface AppointmentCalendarViewProps {
  appointments: AppointmentData[];
  viewMode: ViewMode;
  currentDate: Date;
  onDateChange: (date: Date) => void;
  onAppointmentClick: (appointment: AppointmentData) => void;
}

// Generate time slots for day/week views (6 AM to 10 PM)
const TIME_SLOTS = Array.from({ length: 17 }, (_, i) => i + 6); // 6 AM to 10 PM

export function AppointmentCalendarView({
  appointments,
  viewMode,
  currentDate,
  onDateChange,
  onAppointmentClick
}: AppointmentCalendarViewProps) {
  // Group appointments by date and time
  const appointmentsByDate = useMemo(() => {
    const grouped = new Map<string, AppointmentData[]>();
    appointments.forEach(apt => {
      const date = apt.scheduled_at 
        ? new Date(apt.scheduled_at).toISOString().split('T')[0]
        : new Date(apt.created_at).toISOString().split('T')[0];
      if (!grouped.has(date)) {
        grouped.set(date, []);
      }
      grouped.get(date)!.push(apt);
    });
    return grouped;
  }, [appointments]);

  // Get appointments for a specific date
  const getAppointmentsForDate = (date: Date): AppointmentData[] => {
    const dateKey = format(date, 'yyyy-MM-dd');
    return appointmentsByDate.get(dateKey) || [];
  };

  // Get appointments for a specific time slot
  const getAppointmentsForTimeSlot = (date: Date, hour: number): AppointmentData[] => {
    const dayAppointments = getAppointmentsForDate(date);
    return dayAppointments.filter(apt => {
      if (!apt.scheduled_at) return false;
      const aptDate = new Date(apt.scheduled_at);
      return getHours(aptDate) === hour;
    }).sort((a, b) => {
      // Sort by time
      const timeA = a.scheduled_at ? new Date(a.scheduled_at).getTime() : 0;
      const timeB = b.scheduled_at ? new Date(b.scheduled_at).getTime() : 0;
      return timeA - timeB;
    });
  };

  // Get unscheduled appointments for a date
  const getUnscheduledAppointments = (date: Date): AppointmentData[] => {
    const dayAppointments = getAppointmentsForDate(date);
    return dayAppointments.filter(apt => !apt.scheduled_at);
  };

  // Navigation handlers
  const navigatePrevious = () => {
    if (viewMode === 'day') {
      onDateChange(subDays(currentDate, 1));
    } else if (viewMode === 'week') {
      onDateChange(subWeeks(currentDate, 1));
    } else {
      onDateChange(subMonths(currentDate, 1));
    }
  };

  const navigateNext = () => {
    if (viewMode === 'day') {
      onDateChange(addDays(currentDate, 1));
    } else if (viewMode === 'week') {
      onDateChange(addWeeks(currentDate, 1));
    } else {
      onDateChange(addMonths(currentDate, 1));
    }
  };

  const navigateToday = () => {
    onDateChange(new Date());
  };

  // Get status color
  const getStatusColor = (status: string | null) => {
    if (!status) return 'bg-gray-100 text-gray-800 border-gray-300';
    switch (status.toLowerCase()) {
      case 'scheduled':
      case 'completed':
        return 'bg-green-50 text-green-800 border-green-300';
      case 'rescheduled':
        return 'bg-yellow-50 text-yellow-800 border-yellow-300';
      case 'cancelled':
      case 'no_show':
        return 'bg-red-50 text-red-800 border-red-300';
      default:
        return 'bg-gray-50 text-gray-800 border-gray-300';
    }
  };

  // Render day view
  const renderDayView = () => {
    const dayAppointments = getAppointmentsForDate(currentDate);
    const appointmentsByHour = new Map<number, AppointmentData[]>();
    const unscheduledAppointments: AppointmentData[] = [];
    
    dayAppointments.forEach(apt => {
      if (apt.scheduled_at) {
        const hour = getHours(new Date(apt.scheduled_at));
        if (hour >= 6 && hour <= 22) { // Only show appointments in visible hours
          if (!appointmentsByHour.has(hour)) {
            appointmentsByHour.set(hour, []);
          }
          appointmentsByHour.get(hour)!.push(apt);
        }
      } else {
        unscheduledAppointments.push(apt);
      }
    });

    // Sort appointments within each hour
    appointmentsByHour.forEach((apts, hour) => {
      apts.sort((a, b) => {
        const timeA = a.scheduled_at ? new Date(a.scheduled_at).getTime() : 0;
        const timeB = b.scheduled_at ? new Date(b.scheduled_at).getTime() : 0;
        return timeA - timeB;
      });
    });

    return (
      <div className="flex flex-col h-[calc(100vh-300px)] min-h-[600px]">
        {/* Unscheduled appointments section */}
        {unscheduledAppointments.length > 0 && (
          <Card className="mb-4 p-4">
            <div className="flex items-center gap-2 mb-3">
              <Clock className="h-4 w-4 text-muted-foreground" />
              <h3 className="font-semibold text-sm">Unscheduled Appointments ({unscheduledAppointments.length})</h3>
            </div>
            <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-2">
              {unscheduledAppointments.map(apt => (
                <div
                  key={apt.id}
                  className={cn(
                    "p-2 rounded-md border cursor-pointer hover:shadow-sm transition-shadow text-xs",
                    getStatusColor(apt.status_category)
                  )}
                  onClick={() => onAppointmentClick(apt)}
                >
                  <div className="font-semibold truncate">{apt.customer_name || 'Unknown Customer'}</div>
                  <div className="text-[10px] opacity-80 mt-0.5">Created: {format(new Date(apt.created_at), 'MMM d')}</div>
                </div>
              ))}
            </div>
          </Card>
        )}

        <div className="flex-1 overflow-y-auto border rounded-lg bg-background">
          <div className="grid grid-cols-[80px_1fr] gap-0">
            {/* Time column */}
            <div className="border-r bg-muted/30 sticky left-0 z-20">
              {TIME_SLOTS.map(hour => (
                <div
                  key={hour}
                  className="h-16 border-b flex items-start justify-end pr-2 pt-1"
                >
                  <span className="text-xs text-muted-foreground font-medium">
                    {format(setHours(startOfDay(new Date()), hour), 'h a')}
                  </span>
                </div>
              ))}
            </div>

            {/* Appointments column */}
            <div className="relative">
              {TIME_SLOTS.map(hour => {
                const hourAppointments = appointmentsByHour.get(hour) || [];
                return (
                  <div
                    key={hour}
                    className="h-16 border-b relative hover:bg-muted/20 transition-colors"
                  >
                    {hourAppointments.map((apt, idx) => {
                      const aptDate = new Date(apt.scheduled_at!);
                      const minutes = getMinutes(aptDate);
                      const topOffset = (minutes / 60) * 64; // 64px per hour
                      const duration = apt.duration || 60; // Default 60 minutes
                      const height = Math.max((duration / 60) * 64, 48); // Minimum 48px
                      
                      return (
                        <div
                          key={apt.id}
                          className={cn(
                            "absolute left-1 right-1 rounded-md p-2 border cursor-pointer hover:shadow-lg transition-all min-h-[44px] touch-manipulation",
                            getStatusColor(apt.status_category),
                            "active:scale-[0.98]"
                          )}
                          style={{ 
                            top: `${topOffset}px`, 
                            height: `${height}px`,
                            zIndex: idx + 1,
                            maxWidth: hourAppointments.length > 1 ? `calc(${100 / hourAppointments.length}% - 8px)` : 'calc(100% - 8px)',
                            left: hourAppointments.length > 1 ? `${(idx * 100) / hourAppointments.length}%` : '4px',
                            marginLeft: hourAppointments.length > 1 ? '4px' : '0'
                          }}
                          onClick={() => onAppointmentClick(apt)}
                        >
                          <div className="flex items-start justify-between gap-2 h-full">
                            <div className="flex-1 min-w-0 flex flex-col justify-between">
                              <div>
                                <div className="font-semibold text-sm truncate">
                                  {apt.customer_name || 'Unknown Customer'}
                                </div>
                                <div className="text-xs opacity-80 mt-0.5">
                                  {format(aptDate, 'h:mm a')}
                                  {duration && duration !== 60 && ` • ${duration}min`}
                                </div>
                              </div>
                              {apt.customer_address && (
                                <div className="text-[10px] opacity-70 truncate mt-1">
                                  {apt.customer_address}
                                </div>
                              )}
                            </div>
                            <div className="flex flex-col items-end gap-1 shrink-0">
                              {apt.has_power_bill && (
                                <Paperclip className="h-3.5 w-3.5 opacity-70" title="Has Power Bill" />
                              )}
                              {apt.is_reschedule && (
                                <RotateCcw className="h-3.5 w-3.5 opacity-70" title="Rescheduled" />
                              )}
                            </div>
                          </div>
                        </div>
                      );
                    })}
                  </div>
                );
              })}
            </div>
          </div>
        </div>
      </div>
    );
  };

  // Render week view
  const renderWeekView = () => {
    const weekStart = startOfWeek(currentDate, { weekStartsOn: 0 }); // Sunday
    const weekEnd = endOfWeek(currentDate, { weekStartsOn: 0 });
    const weekDays = eachDayOfInterval({ start: weekStart, end: weekEnd });
    
    // Get appointments for each day and hour
    const appointmentsByDayHour = new Map<string, AppointmentData[]>();
    weekDays.forEach(day => {
      TIME_SLOTS.forEach(hour => {
        const key = `${format(day, 'yyyy-MM-dd')}-${hour}`;
        const apts = getAppointmentsForTimeSlot(day, hour);
        if (apts.length > 0) {
          appointmentsByDayHour.set(key, apts);
        }
      });
    });

    return (
      <div className="flex flex-col h-[calc(100vh-300px)] min-h-[600px]">
        <div className="flex-1 overflow-y-auto">
          <div className="grid grid-cols-[80px_repeat(7,1fr)] gap-0 border rounded-lg">
            {/* Time column header */}
            <div className="border-r bg-muted/30 sticky top-0 z-10">
              <div className="h-12 border-b flex items-center justify-center">
                <span className="text-xs font-medium text-muted-foreground">Time</span>
              </div>
            </div>

            {/* Day headers */}
            {weekDays.map(day => (
              <div
                key={format(day, 'yyyy-MM-dd')}
                className={cn(
                  "border-r last:border-r-0 bg-muted/30 sticky top-0 z-10",
                  isToday(day) && "bg-primary/10"
                )}
              >
                <div className="h-12 border-b flex flex-col items-center justify-center p-1">
                  <div className="text-xs font-medium text-muted-foreground">
                    {format(day, 'EEE')}
                  </div>
                  <div className={cn(
                    "text-sm font-semibold",
                    isToday(day) && "text-primary"
                  )}>
                    {format(day, 'd')}
                  </div>
                </div>
              </div>
            ))}

            {/* Time slots */}
            {TIME_SLOTS.map(hour => (
              <>
                {/* Time label */}
                <div
                  key={`time-${hour}`}
                  className="border-r border-t bg-muted/30"
                >
                  <div className="h-16 flex items-start justify-end pr-2 pt-1">
                    <span className="text-xs text-muted-foreground font-medium">
                      {format(setHours(startOfDay(new Date()), hour), 'h a')}
                    </span>
                  </div>
                </div>

                {/* Day columns */}
                {weekDays.map(day => {
                  const dayAppointments = getAppointmentsForTimeSlot(day, hour);
                  return (
                    <div
                      key={`${format(day, 'yyyy-MM-dd')}-${hour}`}
                      className={cn(
                        "border-r last:border-r-0 border-t relative hover:bg-muted/10 transition-colors",
                        isToday(day) && "bg-primary/5"
                      )}
                    >
                      {dayAppointments.map((apt, idx) => {
                        const aptDate = new Date(apt.scheduled_at!);
                        const minutes = getMinutes(aptDate);
                        const topOffset = (minutes / 60) * 64;
                        const duration = apt.duration || 60;
                        const height = Math.max((duration / 60) * 64, 40);
                        
                        return (
                          <div
                            key={apt.id}
                            className={cn(
                              "absolute left-0.5 right-0.5 rounded-md p-1.5 border cursor-pointer hover:shadow-lg transition-all text-xs min-h-[40px] touch-manipulation",
                              getStatusColor(apt.status_category),
                              "active:scale-[0.98]",
                              dayAppointments.length > 1 && idx > 0 && "ml-0.5"
                            )}
                            style={{ 
                              top: `${topOffset}px`, 
                              height: `${height}px`,
                              zIndex: idx + 1,
                              maxWidth: dayAppointments.length > 1 ? `calc(${100 / dayAppointments.length}% - 4px)` : 'calc(100% - 4px)',
                              left: dayAppointments.length > 1 ? `${(idx * 100) / dayAppointments.length}%` : '2px'
                            }}
                            onClick={() => onAppointmentClick(apt)}
                          >
                            <div className="font-semibold truncate leading-tight">
                              {apt.customer_name || 'Unknown'}
                            </div>
                            <div className="text-[10px] opacity-80 mt-0.5">
                              {format(aptDate, 'h:mm a')}
                            </div>
                            {(apt.has_power_bill || apt.is_reschedule) && (
                              <div className="flex items-center gap-1 mt-1">
                                {apt.has_power_bill && (
                                  <Paperclip className="h-2.5 w-2.5 opacity-70" />
                                )}
                                {apt.is_reschedule && (
                                  <RotateCcw className="h-2.5 w-2.5 opacity-70" />
                                )}
                              </div>
                            )}
                          </div>
                        );
                      })}
                    </div>
                  );
                })}
              </>
            ))}
          </div>
        </div>
      </div>
    );
  };

  // Render month view
  const renderMonthView = () => {
    const monthStart = startOfMonth(currentDate);
    const monthEnd = endOfMonth(currentDate);
    const calendarStart = startOfWeek(monthStart, { weekStartsOn: 0 });
    const calendarEnd = endOfWeek(monthEnd, { weekStartsOn: 0 });
    const calendarDays = eachDayOfInterval({ start: calendarStart, end: calendarEnd });

    // Group days into weeks
    const weeks: Date[][] = [];
    for (let i = 0; i < calendarDays.length; i += 7) {
      weeks.push(calendarDays.slice(i, i + 7));
    }

    return (
      <div className="space-y-2">
        {/* Day headers */}
        <div className="grid grid-cols-7 gap-1">
          {['Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat'].map(day => (
            <div key={day} className="text-center text-sm font-medium text-muted-foreground py-2">
              {day}
            </div>
          ))}
        </div>

        {/* Calendar grid */}
        <div className="grid grid-cols-7 gap-1">
          {calendarDays.map(day => {
            const dayAppointments = getAppointmentsForDate(day);
            const scheduledAppointments = dayAppointments.filter(apt => apt.scheduled_at);
            const unscheduledCount = dayAppointments.length - scheduledAppointments.length;
            const isCurrentMonth = isSameMonth(day, currentDate);
            const isDayToday = isToday(day);

            return (
              <Card
                key={format(day, 'yyyy-MM-dd')}
                className={cn(
                  "min-h-[140px] p-2 flex flex-col transition-colors",
                  !isCurrentMonth && "opacity-40",
                  isDayToday && "ring-2 ring-primary bg-primary/5",
                  "hover:bg-muted/20"
                )}
              >
                <div className={cn(
                  "text-sm font-semibold mb-2 flex items-center justify-between",
                  isDayToday && "text-primary"
                )}>
                  <span>{format(day, 'd')}</span>
                  {dayAppointments.length > 0 && (
                    <Badge variant="secondary" className="h-5 px-1.5 text-[10px]">
                      {dayAppointments.length}
                    </Badge>
                  )}
                </div>
                <div className="flex-1 space-y-1 overflow-y-auto min-h-0">
                  {scheduledAppointments.slice(0, 3).map(apt => (
                    <div
                      key={apt.id}
                      className={cn(
                        "text-xs p-1.5 rounded border cursor-pointer hover:shadow-md transition-all touch-manipulation min-h-[44px]",
                        getStatusColor(apt.status_category),
                        "active:scale-[0.98]"
                      )}
                      onClick={() => onAppointmentClick(apt)}
                      title={`${apt.customer_name || 'Unknown Customer'} - ${apt.scheduled_at ? format(new Date(apt.scheduled_at), 'h:mm a') : 'Unscheduled'}`}
                    >
                      <div className="font-semibold truncate text-[11px]">
                        {apt.scheduled_at 
                          ? format(new Date(apt.scheduled_at), 'h:mm a')
                          : 'Unscheduled'}
                      </div>
                      <div className="truncate text-[10px] opacity-90 mt-0.5">
                        {apt.customer_name || 'Unknown'}
                      </div>
                      {(apt.has_power_bill || apt.is_reschedule) && (
                        <div className="flex items-center gap-1 mt-1">
                          {apt.has_power_bill && (
                            <Paperclip className="h-2.5 w-2.5 opacity-70" />
                          )}
                          {apt.is_reschedule && (
                            <RotateCcw className="h-2.5 w-2.5 opacity-70" />
                          )}
                        </div>
                      )}
                    </div>
                  ))}
                  {(scheduledAppointments.length > 3 || unscheduledCount > 0) && (
                    <div className="text-xs text-muted-foreground text-center py-1 font-medium">
                      {scheduledAppointments.length > 3 && `+${scheduledAppointments.length - 3} more`}
                      {unscheduledCount > 0 && (
                        <span className="block text-[10px] mt-0.5">
                          {unscheduledCount} unscheduled
                        </span>
                      )}
                    </div>
                  )}
                </div>
              </Card>
            );
          })}
        </div>
      </div>
    );
  };

  // Get date range label
  const getDateRangeLabel = () => {
    if (viewMode === 'day') {
      return format(currentDate, 'EEEE, MMMM d, yyyy');
    } else if (viewMode === 'week') {
      const weekStart = startOfWeek(currentDate, { weekStartsOn: 0 });
      const weekEnd = endOfWeek(currentDate, { weekStartsOn: 0 });
      return `${format(weekStart, 'MMM d')} - ${format(weekEnd, 'MMM d, yyyy')}`;
    } else {
      return format(currentDate, 'MMMM yyyy');
    }
  };

  return (
    <div className="space-y-4">
      {/* Navigation */}
      <Card className="p-4">
        <div className="flex items-center justify-between flex-wrap gap-4">
          <div className="flex items-center gap-2 flex-wrap">
            <Button
              variant="outline"
              size="sm"
              onClick={navigatePrevious}
              className="touch-manipulation min-h-[44px] min-w-[44px]"
            >
              <ChevronLeft className="h-4 w-4" />
            </Button>
            <Button
              variant="outline"
              size="sm"
              onClick={navigateToday}
              className="touch-manipulation min-h-[44px]"
            >
              Today
            </Button>
            <Button
              variant="outline"
              size="sm"
              onClick={navigateNext}
              className="touch-manipulation min-h-[44px] min-w-[44px]"
            >
              <ChevronRight className="h-4 w-4" />
            </Button>
            <div className="ml-2 text-lg font-semibold">
              {getDateRangeLabel()}
            </div>
          </div>
          <Badge variant="outline" className="text-sm px-3 py-1.5">
            {appointments.length} appointment{appointments.length !== 1 ? 's' : ''}
          </Badge>
        </div>
      </Card>

      {/* Calendar View */}
      <Card className="p-4 overflow-hidden">
        {viewMode === 'day' && renderDayView()}
        {viewMode === 'week' && renderWeekView()}
        {viewMode === 'month' && renderMonthView()}
      </Card>
    </div>
  );
}
