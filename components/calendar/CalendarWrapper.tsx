"use client";

import { useState } from "react";
import { Calendar, dateFnsLocalizer, EventPropGetter, ToolbarProps, View } from "react-big-calendar";
import { format, parse, startOfWeek, getDay } from "date-fns";
import { enUS } from "date-fns/locale";
import { Button } from "@/components/ui/button";
import { cn } from "@/lib/utils/cn";
import { ChevronLeft, ChevronRight, Calendar as CalendarIcon } from "lucide-react";

// Create date-fns localizer
const localizer = dateFnsLocalizer({
  format,
  parse,
  startOfWeek,
  getDay,
  locales: { 'en-US': enUS },
});

// Types
export interface CalendarEvent {
  id: string;
  title: string;
  start: Date;
  end: Date;
  resource?: any;
}

export interface CalendarWrapperProps {
  events: CalendarEvent[];
  onSelectEvent?: (event: CalendarEvent) => void;
  eventPropGetter?: EventPropGetter<CalendarEvent>;
  className?: string;
}

// Custom Toolbar Component
const CustomToolbar: React.FC<ToolbarProps<CalendarEvent, object>> = ({ label, onNavigate, onView, views, view }) => {
  // Convert views object to array of view names
  const viewNames = Array.isArray(views) ? views : Object.keys(views);

  return (
    <div className="flex items-center justify-between mb-4">
      {/* Left side: Navigation and label */}
      <div className="flex items-center gap-2">
        <Button
          variant="outline"
          size="sm"
          onClick={() => onNavigate('PREV')}
          className="h-8 w-8 p-0"
        >
          <ChevronLeft className="h-4 w-4" />
        </Button>
        <Button
          variant="outline"
          size="sm"
          onClick={() => onNavigate('TODAY')}
          className="h-8 px-3"
        >
          Today
        </Button>
        <Button
          variant="outline"
          size="sm"
          onClick={() => onNavigate('NEXT')}
          className="h-8 w-8 p-0"
        >
          <ChevronRight className="h-4 w-4" />
        </Button>
        <div className="ml-4 text-lg font-semibold text-gray-900">
          {label}
        </div>
      </div>

      {/* Right side: View toggle */}
      <div className="flex items-center gap-1">
        {viewNames.map((viewName) => (
          <Button
            key={viewName}
            variant={view === viewName ? "default" : "outline"}
            size="sm"
            onClick={() => onView(viewName as View)}
            className="h-8 px-3 capitalize"
          >
            {viewName}
          </Button>
        ))}
      </div>
    </div>
  );
};

// Main CalendarWrapper Component
export const CalendarWrapper: React.FC<CalendarWrapperProps> = ({
  events,
  onSelectEvent,
  eventPropGetter,
  className,
}) => {
  const [currentView, setCurrentView] = useState<View>('month');

  const handleViewChange = (view: View) => {
    setCurrentView(view);
  };

  return (
    <div className={cn('bg-white rounded-lg border shadow-sm p-6', className)}>
      <div className="h-[500px] ipad:h-[600px] ipad-lg:h-[700px]">
        <Calendar
          localizer={localizer}
          events={events}
          startAccessor="start"
          endAccessor="end"
          view={currentView}
          onView={handleViewChange}
          views={['month', 'week', 'day']}
          components={{
            toolbar: CustomToolbar,
          }}
          onSelectEvent={onSelectEvent}
          eventPropGetter={eventPropGetter}
          style={{ height: '100%' }}
        />
      </div>
    </div>
  );
};
