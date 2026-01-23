'use client';

import { Card } from '@/components/ui/card';
import { Badge } from '@/components/ui/badge';
import { Clock, MapPin, Phone, Mail, User, Calendar, Paperclip, RotateCcw } from 'lucide-react';
import { cn } from '@/lib/utils';
import { format } from 'date-fns';

export interface AppointmentData {
  id: string;
  repcard_appointment_id: number;
  scheduled_at: string | null;
  created_at: string;
  customer_name: string | null;
  customer_phone: string | null;
  customer_address: string | null;
  customer_email: string | null;
  setter_name: string | null;
  setter_email: string | null;
  setter_team_name: string | null;
  closer_name: string | null;
  closer_email: string | null;
  closer_team_name: string | null;
  calendar_name: string | null;
  status_category: string | null;
  disposition: string | null;
  has_power_bill: boolean;
  is_reschedule: boolean;
  reschedule_count: number | null;
  is_within_48_hours: boolean | null;
  notes: string | null;
  customer_attachment_count: number;
  appointment_attachment_count: number;
  customer_note_count: number;
  is_confirmed: boolean;
}

interface AppointmentCardProps {
  appointment: AppointmentData;
  onClick?: (appointment: AppointmentData) => void;
  compact?: boolean;
}

export function AppointmentCard({ appointment, onClick, compact = false }: AppointmentCardProps) {
  const scheduledDate = appointment.scheduled_at ? new Date(appointment.scheduled_at) : null;
  const createdDate = new Date(appointment.created_at);

  // Status color coding
  const getStatusColor = (status: string | null) => {
    if (!status) return 'bg-gray-100 text-gray-800';
    switch (status.toLowerCase()) {
      case 'scheduled':
      case 'completed':
        return 'bg-green-100 text-green-800';
      case 'rescheduled':
        return 'bg-yellow-100 text-yellow-800';
      case 'cancelled':
      case 'no_show':
        return 'bg-red-100 text-red-800';
      default:
        return 'bg-gray-100 text-gray-800';
    }
  };

  const statusColor = getStatusColor(appointment.status_category);

  return (
    <Card
      className={cn(
        "p-4 hover:shadow-md transition-shadow cursor-pointer",
        compact && "p-3"
      )}
      onClick={() => onClick?.(appointment)}
    >
      <div className="space-y-3">
        {/* Header: Time and Status */}
        <div className="flex items-start justify-between">
          <div className="flex items-center gap-2">
            {scheduledDate ? (
              <>
                <Clock className="h-4 w-4 text-muted-foreground" />
                <span className="font-semibold">
                  {format(scheduledDate, 'MMM d, yyyy')} at {format(scheduledDate, 'h:mm a')}
                </span>
              </>
            ) : (
              <>
                <Clock className="h-4 w-4 text-muted-foreground" />
                <span className="font-semibold text-muted-foreground">Not Scheduled</span>
              </>
            )}
          </div>
          <div className="flex items-center gap-2">
            {appointment.status_category && (
              <Badge className={statusColor}>
                {appointment.status_category}
              </Badge>
            )}
            {appointment.has_power_bill && (
              <Badge variant="outline" className="bg-blue-50 text-blue-700 border-blue-200">
                <Paperclip className="h-3 w-3 mr-1" />
                Power Bill
              </Badge>
            )}
            {appointment.is_reschedule && (
              <Badge variant="outline" className="bg-yellow-50 text-yellow-700 border-yellow-200">
                <RotateCcw className="h-3 w-3 mr-1" />
                {appointment.reschedule_count ? `Rescheduled ${appointment.reschedule_count}x` : 'Rescheduled'}
              </Badge>
            )}
          </div>
        </div>

        {/* Customer Info */}
        <div className="space-y-1">
          <div className="flex items-center gap-2">
            <User className="h-4 w-4 text-muted-foreground" />
            <span className="font-medium">{appointment.customer_name || 'Unknown Customer'}</span>
          </div>
          {appointment.customer_address && (
            <div className="flex items-start gap-2 text-sm text-muted-foreground">
              <MapPin className="h-4 w-4 mt-0.5 shrink-0" />
              <span>{appointment.customer_address}</span>
            </div>
          )}
          {appointment.customer_phone && (
            <div className="flex items-center gap-2 text-sm text-muted-foreground">
              <Phone className="h-4 w-4 shrink-0" />
              <span>{appointment.customer_phone}</span>
            </div>
          )}
          {appointment.customer_email && (
            <div className="flex items-center gap-2 text-sm text-muted-foreground">
              <Mail className="h-4 w-4 shrink-0" />
              <span>{appointment.customer_email}</span>
            </div>
          )}
        </div>

        {/* Setter and Closer Info */}
        <div className="grid grid-cols-1 md:grid-cols-2 gap-3 pt-2 border-t">
          <div>
            <div className="text-xs text-muted-foreground mb-1">Setter</div>
            <div className="text-sm font-medium">
              {appointment.setter_name || 'Unknown'}
              {appointment.setter_team_name && (
                <span className="text-muted-foreground ml-1">({appointment.setter_team_name})</span>
              )}
            </div>
          </div>
          <div>
            <div className="text-xs text-muted-foreground mb-1">Closer</div>
            <div className="text-sm font-medium">
              {appointment.closer_name || 'Unassigned'}
              {appointment.closer_team_name && (
                <span className="text-muted-foreground ml-1">({appointment.closer_team_name})</span>
              )}
            </div>
          </div>
        </div>

        {/* Calendar and Metadata */}
        <div className="flex items-center justify-between pt-2 border-t text-xs text-muted-foreground">
          <div className="flex items-center gap-4">
            {appointment.calendar_name && (
              <div className="flex items-center gap-1">
                <Calendar className="h-3 w-3" />
                <span>{appointment.calendar_name}</span>
              </div>
            )}
            <div>
              Set on {format(createdDate, 'MMM d, yyyy')}
            </div>
          </div>
          {(appointment.customer_attachment_count > 0 || appointment.appointment_attachment_count > 0) && (
            <div className="flex items-center gap-1">
              <Paperclip className="h-3 w-3" />
              <span>
                {appointment.customer_attachment_count + appointment.appointment_attachment_count} attachment(s)
              </span>
            </div>
          )}
        </div>
      </div>
    </Card>
  );
}
