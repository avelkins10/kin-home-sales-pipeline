import { Badge } from '@/components/ui/badge'
import { CalendarEvent } from '@/lib/utils/calendar-helpers'
import { cn } from '@/lib/utils/cn'
import { ClipboardCheck, Wrench, Zap, MapPin } from 'lucide-react'

interface CalendarEventContentProps {
  event: {
    title: string
    resource: CalendarEvent
  }
}

const getEventTypeIcon = (type: string) => {
  switch (type) {
    case 'survey':
      return <MapPin className="h-3 w-3" />
    case 'install':
      return <Wrench className="h-3 w-3" />
    case 'inspection':
      return <ClipboardCheck className="h-3 w-3" />
    case 'pto':
      return <Zap className="h-3 w-3" />
    default:
      return null
  }
}

const getStatusBadgeVariant = (status: string) => {
  switch (status) {
    case 'scheduled':
      return 'default' as const
    case 'completed':
      return 'success' as const
    case 'pending':
      return 'warning' as const
    default:
      return 'secondary' as const
  }
}

export const CalendarEventContent = ({ event }: CalendarEventContentProps) => {
  const { resource } = event
  const eventTypeIcon = getEventTypeIcon(resource.type)
  const badgeVariant = getStatusBadgeVariant(resource.status)

  return (
    <div className="flex flex-col gap-1 p-1">
      {/* Event title */}
      <div className="text-xs font-medium text-slate-900 truncate">
        {event.title}
      </div>
      
      {/* Status badge and type icon */}
      <div className="flex items-center gap-1">
        {eventTypeIcon && (
          <div className="text-slate-500">
            {eventTypeIcon}
          </div>
        )}
        <Badge 
          variant={badgeVariant} 
          className="text-xs h-4 px-1.5"
        >
          {resource.status}
        </Badge>
      </div>
    </div>
  )
}
