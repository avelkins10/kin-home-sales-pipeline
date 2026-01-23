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

  // Get prominent outcome badge
  const getOutcomeBadge = () => {
    const status = appointment.status_category;
    const disposition = appointment.disposition;

    if (!status) return null;

    let badgeClass = '';
    let badgeText = '';
    let icon = null;

    switch (status.toLowerCase()) {
      case 'completed':
        badgeClass = 'bg-green-600 text-white border-green-700';
        badgeText = disposition ? capitalizeWords(disposition) : 'COMPLETED';
        icon = <CheckCircle2 className="h-4 w-4" />;
        break;
      case 'cancelled':
        badgeClass = 'bg-red-600 text-white border-red-700';
        badgeText = disposition ? capitalizeWords(disposition) : 'CANCELLED';
        icon = <AlertCircle className="h-4 w-4" />;
        break;
      case 'no_show':
        badgeClass = 'bg-red-700 text-white border-red-800';
        badgeText = 'NO SHOW';
        icon = <AlertCircle className="h-4 w-4" />;
        break;
      case 'rescheduled':
        badgeClass = 'bg-amber-600 text-white border-amber-700';
        badgeText = 'RESCHEDULED';
        icon = <RotateCcw className="h-4 w-4" />;
        break;
      case 'scheduled':
        badgeClass = 'bg-blue-600 text-white border-blue-700';
        badgeText = 'SCHEDULED';
        icon = <Clock className="h-4 w-4" />;
        break;
      default:
        badgeClass = 'bg-gray-600 text-white border-gray-700';
        badgeText = capitalizeWords(status);
    }

    return { badgeClass, badgeText, icon };
  };

  const outcomeBadge = getOutcomeBadge();
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
      <div className="space-y-2.5">
        {/* Header: Time + Outcome Badge */}
        <div className="flex items-center justify-between gap-3 flex-wrap">
          <div className="flex items-center gap-2">
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

          {/* Prominent Outcome Badge */}
          {outcomeBadge && (
            <div className={cn(
              "flex items-center gap-2 px-3 py-1.5 rounded-md font-bold text-sm border-2",
              outcomeBadge.badgeClass
            )}>
              {outcomeBadge.icon}
              {outcomeBadge.badgeText}
            </div>
          )}
        </div>

        {/* Customer Info */}
        <div className="bg-gradient-to-r from-blue-50/50 to-transparent rounded-lg p-2.5 -mx-1">
          <div className="flex items-start gap-2">
            <User className="h-4 w-4 text-gray-600 mt-0.5 shrink-0" />
            <div className="flex-1 min-w-0">
              <h4 className="font-semibold text-base text-gray-900 truncate">
                {appointment.customer_name || 'Customer Name Not Available'}
              </h4>
              {appointment.customer_address && (
                <div className="flex items-start gap-1 text-xs text-gray-600 mt-0.5">
                  <MapPin className="h-3 w-3 mt-0.5 shrink-0" />
                  <span className="line-clamp-1">{appointment.customer_address}</span>
                </div>
              )}
              <div className="flex items-center gap-3 text-xs text-gray-600 mt-1">
                {appointment.customer_phone && (
                  <a
                    href={`tel:${appointment.customer_phone}`}
                    className="flex items-center gap-1 hover:text-blue-600 transition-colors"
                    onClick={(e) => e.stopPropagation()}
                  >
                    <Phone className="h-3 w-3" />
                    <span>{appointment.customer_phone}</span>
                  </a>
                )}
                {appointment.customer_email && (
                  <a
                    href={`mailto:${appointment.customer_email}`}
                    className="flex items-center gap-1 hover:text-blue-600 transition-colors truncate"
                    onClick={(e) => e.stopPropagation()}
                  >
                    <Mail className="h-3 w-3 shrink-0" />
                    <span className="truncate">{appointment.customer_email}</span>
                  </a>
                )}
              </div>
            </div>
          </div>
        </div>

        {/* Assignment - Closer Prominent */}
        <div className="grid grid-cols-[2fr_1fr] gap-3">
          {/* Closer - Large and Prominent */}
          <div className={cn(
            "rounded-lg p-2.5 border-2",
            isUnassigned
              ? "bg-amber-50 border-amber-300"
              : "bg-green-50 border-green-300"
          )}>
            <div className="flex items-center gap-2 mb-1">
              <User className={cn(
                "h-4 w-4",
                isUnassigned ? "text-amber-600" : "text-green-600"
              )} />
              <div className="text-xs font-bold uppercase tracking-wide text-gray-600">Closer</div>
              {isUnassigned && (
                <Badge variant="outline" className="bg-amber-100 text-amber-800 border-amber-400 text-[10px] px-1 py-0">
                  UNASSIGNED
                </Badge>
              )}
            </div>
            <div className={cn(
              "text-base font-bold truncate",
              isUnassigned ? "text-amber-700" : "text-gray-900"
            )}>
              {appointment.closer_name || 'Unassigned'}
            </div>
            {appointment.closer_team_name && (
              <div className="flex items-center gap-1 text-xs text-gray-600 mt-0.5">
                <Users className="h-3 w-3" />
                {appointment.closer_team_name}
              </div>
            )}
          </div>

          {/* Setter - Smaller */}
          <div className="bg-gray-50 rounded-lg p-2 border border-gray-200">
            <div className="text-[10px] font-semibold uppercase tracking-wide text-gray-500 mb-1">Setter</div>
            <div className="text-sm font-medium text-gray-900 truncate">
              {appointment.setter_name || 'N/A'}
            </div>
            {appointment.setter_team_name && (
              <div className="flex items-center gap-1 text-[10px] text-gray-500 mt-0.5">
                <Users className="h-2.5 w-2.5" />
                <span className="truncate">{appointment.setter_team_name}</span>
              </div>
            )}
          </div>
        </div>

        {/* Footer: Metadata */}
        <div className="flex items-center justify-between pt-2 border-t gap-2 text-xs text-gray-500">
          <div className="flex items-center gap-3">
            {appointment.calendar_name && (
              <div className="flex items-center gap-1">
                <Calendar className="h-3 w-3" />
                <span>{appointment.calendar_name}</span>
              </div>
            )}
            {appointment.is_confirmed && (
              <div className="flex items-center gap-1 text-green-600 font-medium">
                <CheckCircle2 className="h-3 w-3" />
                Confirmed
              </div>
            )}
            {scheduledDate && (
              <div className="text-gray-400">
                Created {format(createdDate, 'MMM d')}
              </div>
            )}
          </div>

          <div className="flex items-center gap-1.5">
            {appointment.has_power_bill && (
              <div className="flex items-center gap-1 text-blue-600">
                <Paperclip className="h-3 w-3" />
                <span className="text-[10px] font-medium">Power Bill</span>
              </div>
            )}
            {appointment.is_reschedule && appointment.reschedule_count && (
              <div className="flex items-center gap-1 text-purple-600">
                <RotateCcw className="h-3 w-3" />
                <span className="text-[10px] font-medium">{appointment.reschedule_count}x</span>
              </div>
            )}
            {totalAttachments > 0 && (
              <div className="flex items-center gap-1">
                <Paperclip className="h-3 w-3" />
                <span className="text-[10px]">{totalAttachments}</span>
              </div>
            )}
          </div>
        </div>
      </div>
    </Card>
  );
}
