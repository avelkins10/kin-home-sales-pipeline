'use client';

import { ScrollArea } from '@/components/ui/scroll-area';
import { Badge } from '@/components/ui/badge';
import { Truck, Play, CheckCircle, AlertTriangle, Clock } from 'lucide-react';
import type { FieldTrackingEvent } from '@/lib/types/operations';
import { formatDistanceToNow } from 'date-fns';

interface FieldTrackingActivityFeedProps {
  events: FieldTrackingEvent[];
  limit?: number;
}

export function FieldTrackingActivityFeed({ events, limit = 20 }: FieldTrackingActivityFeedProps) {
  const displayEvents = events.slice(0, limit);

  const getEventIcon = (eventType: string, eventSubType: string | null) => {
    if (eventSubType === 'ENROUTE') return Truck;
    if (eventSubType === 'STARTED') return Play;
    if (eventSubType === 'COMPLETE') return CheckCircle;
    if (eventType === 'LATE' || eventType === 'NOSHOW') return AlertTriangle;
    return Clock;
  };

  const getEventColor = (eventType: string, eventSubType: string | null) => {
    if (eventSubType === 'ENROUTE') return 'text-blue-600';
    if (eventSubType === 'STARTED') return 'text-yellow-600';
    if (eventSubType === 'COMPLETE') return 'text-green-600';
    if (eventType === 'LATE' || eventType === 'NOSHOW') return 'text-red-600';
    return 'text-gray-600';
  };

  if (displayEvents.length === 0) {
    return (
      <div className="text-center text-gray-500 py-8">
        No recent activity
      </div>
    );
  }

  return (
    <ScrollArea className="h-[500px] pr-4">
      <div className="space-y-4">
        {displayEvents.map((event) => {
          const Icon = getEventIcon(event.event_type, event.event_sub_type);
          const color = getEventColor(event.event_type, event.event_sub_type);

          return (
            <div key={event.id} className="flex gap-3 pb-4 border-b last:border-0">
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
                  {event.event_sub_type && (
                    <Badge variant="outline" className="text-xs flex-shrink-0">
                      {event.event_sub_type}
                    </Badge>
                  )}
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
  );
}

