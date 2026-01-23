'use client';

import { Card } from '@/components/ui/card';
import { Badge } from '@/components/ui/badge';
import {
  Clock,
  MapPin,
  Phone,
  Mail,
  User,
  Calendar,
  Paperclip,
  RotateCcw,
  AlertCircle,
  CheckCircle2,
  Users
} from 'lucide-react';
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
  office_name: string | null;
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

// Capitalize first letter of each word
function capitalizeWords(str: string | null): string {
  if (!str) return '';
  return str.split('_').map(word =>
    word.charAt(0).toUpperCase() + word.slice(1).toLowerCase()
  ).join(' ');
}

export function AppointmentCard({ appointment, onClick, compact = false }: AppointmentCardProps) {
  const scheduledDate = appointment.scheduled_at ? new Date(appointment.scheduled_at) : null;
  const createdDate = new Date(appointment.created_at);
  const isUnassigned = !appointment.closer_name || appointment.closer_name === 'Unassigned';

  // Status color coding
  const getStatusColor = (status: string | null) => {
    if (!status) return 'bg-gray-100 text-gray-700 border-gray-200';
    switch (status.toLowerCase()) {
      case 'scheduled':
        return 'bg-green-50 text-green-700 border-green-200';
      case 'completed':
        return 'bg-blue-50 text-blue-700 border-blue-200';
      case 'rescheduled':
        return 'bg-yellow-50 text-yellow-700 border-yellow-200';
      case 'cancelled':
      case 'no_show':
        return 'bg-red-50 text-red-700 border-red-200';
      default:
        return 'bg-gray-100 text-gray-700 border-gray-200';
    }
  };

  const statusColor = getStatusColor(appointment.status_category);
  const totalAttachments = appointment.customer_attachment_count + appointment.appointment_attachment_count;

  return (
    <Card
      className={cn(
        "hover:shadow-lg transition-all duration-200 cursor-pointer border-l-4",
        compact ? "p-3" : "p-4",
        isUnassigned ? "border-l-amber-500" : "border-l-transparent"
      )}
      onClick={() => onClick?.(appointment)}
    >
      <div className="space-y-3">
        {/* Header: Time, Status, and Key Indicators */}
        <div className="flex items-start justify-between gap-3 flex-wrap">
          <div className="flex-1 min-w-0">
            <div className="flex items-center gap-2 mb-1">
              {scheduledDate ? (
                <>
                  <Clock className="h-4 w-4 text-blue-600 shrink-0" />
                  <span className="font-semibold text-gray-900">
                    {format(scheduledDate, 'EEE, MMM d')} at {format(scheduledDate, 'h:mm a')}
                  </span>
                </>
              ) : (
                <>
                  <Clock className="h-4 w-4 text-amber-600 shrink-0" />
                  <span className="font-semibold text-amber-600">Not Scheduled</span>
                </>
              )}
            </div>
            {scheduledDate && (
              <div className="text-xs text-muted-foreground ml-6">
                Created {format(createdDate, 'MMM d, yyyy')}
              </div>
            )}
          </div>

          <div className="flex items-center gap-1.5 flex-wrap">
            {appointment.is_confirmed && (
              <Badge variant="outline" className="bg-green-50 text-green-700 border-green-200">
                <CheckCircle2 className="h-3 w-3 mr-1" />
                Confirmed
              </Badge>
            )}
            {isUnassigned && (
              <Badge variant="outline" className="bg-amber-50 text-amber-700 border-amber-200">
                <AlertCircle className="h-3 w-3 mr-1" />
                Needs Closer
              </Badge>
            )}
            {appointment.status_category && (
              <Badge variant="outline" className={statusColor}>
                {capitalizeWords(appointment.status_category)}
              </Badge>
            )}
          </div>
        </div>

        {/* Customer Info - Prominent */}
        <div className="bg-gradient-to-r from-gray-50 to-transparent rounded-lg p-3 -mx-1">
          <div className="flex items-start gap-2 mb-2">
            <User className="h-5 w-5 text-gray-600 mt-0.5 shrink-0" />
            <div className="flex-1 min-w-0">
              <h4 className="font-semibold text-lg text-gray-900 truncate">
                {appointment.customer_name || 'Customer Name Not Available'}
              </h4>
              {appointment.customer_address && (
                <div className="flex items-start gap-1.5 text-sm text-gray-600 mt-1">
                  <MapPin className="h-3.5 w-3.5 mt-0.5 shrink-0" />
                  <span className="line-clamp-1">{appointment.customer_address}</span>
                </div>
              )}
            </div>
          </div>

          <div className="flex items-center gap-4 text-sm text-gray-600 ml-7">
            {appointment.customer_phone && (
              <a
                href={`tel:${appointment.customer_phone}`}
                className="flex items-center gap-1.5 hover:text-blue-600 transition-colors"
                onClick={(e) => e.stopPropagation()}
              >
                <Phone className="h-3.5 w-3.5" />
                <span>{appointment.customer_phone}</span>
              </a>
            )}
            {appointment.customer_email && (
              <a
                href={`mailto:${appointment.customer_email}`}
                className="flex items-center gap-1.5 hover:text-blue-600 transition-colors truncate"
                onClick={(e) => e.stopPropagation()}
              >
                <Mail className="h-3.5 w-3.5 shrink-0" />
                <span className="truncate">{appointment.customer_email}</span>
              </a>
            )}
          </div>
        </div>

        {/* Team Assignment */}
        <div className="grid grid-cols-2 gap-3">
          <div className="space-y-1">
            <div className="text-xs font-medium text-gray-500 uppercase tracking-wide">Setter</div>
            <div className="text-sm font-medium text-gray-900">
              {appointment.setter_name || 'Not Assigned'}
            </div>
            {appointment.setter_team_name && (
              <div className="flex items-center gap-1 text-xs text-gray-500">
                <Users className="h-3 w-3" />
                {appointment.setter_team_name}
              </div>
            )}
          </div>
          <div className="space-y-1">
            <div className="text-xs font-medium text-gray-500 uppercase tracking-wide">Closer</div>
            <div className={cn(
              "text-sm font-medium",
              isUnassigned ? "text-amber-600" : "text-gray-900"
            )}>
              {appointment.closer_name || 'Unassigned'}
            </div>
            {appointment.closer_team_name && (
              <div className="flex items-center gap-1 text-xs text-gray-500">
                <Users className="h-3 w-3" />
                {appointment.closer_team_name}
              </div>
            )}
          </div>
        </div>

        {/* Footer: Metadata and Badges */}
        <div className="flex items-center justify-between pt-3 border-t gap-2 flex-wrap">
          <div className="flex items-center gap-3 text-xs text-gray-500">
            {appointment.calendar_name && (
              <div className="flex items-center gap-1">
                <Calendar className="h-3 w-3" />
                <span>{appointment.calendar_name}</span>
              </div>
            )}
          </div>

          <div className="flex items-center gap-1.5 flex-wrap">
            {appointment.has_power_bill && (
              <Badge variant="outline" className="bg-blue-50 text-blue-700 border-blue-200 text-xs">
                <Paperclip className="h-3 w-3 mr-1" />
                Power Bill
              </Badge>
            )}
            {appointment.is_reschedule && (
              <Badge variant="outline" className="bg-purple-50 text-purple-700 border-purple-200 text-xs">
                <RotateCcw className="h-3 w-3 mr-1" />
                {appointment.reschedule_count ? `${appointment.reschedule_count}x` : 'Rescheduled'}
              </Badge>
            )}
            {totalAttachments > 0 && (
              <Badge variant="outline" className="bg-gray-50 text-gray-600 border-gray-200 text-xs">
                <Paperclip className="h-3 w-3 mr-1" />
                {totalAttachments}
              </Badge>
            )}
          </div>
        </div>
      </div>
    </Card>
  );
}
