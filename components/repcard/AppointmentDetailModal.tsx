'use client';

import { useState, useEffect } from 'react';
import {
  Dialog,
  DialogContent,
  DialogDescription,
  DialogHeader,
  DialogTitle,
} from '@/components/ui/dialog';
import { Badge } from '@/components/ui/badge';
import { Button } from '@/components/ui/button';
import { Separator } from '@/components/ui/separator';
import { Collapsible, CollapsibleContent, CollapsibleTrigger } from '@/components/ui/collapsible';
import {
  Clock,
  MapPin,
  Phone,
  Mail,
  User,
  Calendar,
  Paperclip,
  RotateCcw,
  FileText,
  History,
  CheckCircle2,
  ChevronDown,
  Building2,
  Users,
  AlertCircle,
  ExternalLink
} from 'lucide-react';
import { format } from 'date-fns';
import { AppointmentData } from './AppointmentCard';
import { cn } from '@/lib/utils';

interface AppointmentDetailModalProps {
  appointment: AppointmentData;
  open: boolean;
  onOpenChange: (open: boolean) => void;
}

interface PreviousAppointment {
  id: string;
  repcard_appointment_id: number;
  status_category: string | null;
  disposition: string | null;
  scheduled_at: string | null;
  completed_at: string | null;
  duration: number | null;
  notes: string | null;
  is_reschedule: boolean;
  reschedule_count: number | null;
  created_at: string;
  setter_name: string | null;
  closer_name: string | null;
  is_confirmed: boolean;
}

// Capitalize first letter of each word
function capitalizeWords(str: string | null): string {
  if (!str) return '';
  return str.split('_').map(word =>
    word.charAt(0).toUpperCase() + word.slice(1).toLowerCase()
  ).join(' ');
}

export function AppointmentDetailModal({
  appointment,
  open,
  onOpenChange
}: AppointmentDetailModalProps) {
  const scheduledDate = appointment.scheduled_at ? new Date(appointment.scheduled_at) : null;
  const createdDate = new Date(appointment.created_at);
  const [previousAppointments, setPreviousAppointments] = useState<PreviousAppointment[]>([]);
  const [loadingHistory, setLoadingHistory] = useState(false);
  const [isHistoryOpen, setIsHistoryOpen] = useState(false);
  const isUnassigned = !appointment.closer_name || appointment.closer_name === 'Unassigned';

  // Fetch previous appointments when modal opens
  useEffect(() => {
    if (open && appointment.id) {
      setLoadingHistory(true);
      fetch(`/api/repcard/appointments/${appointment.id}/history`)
        .then(res => res.json())
        .then(data => {
          if (data.previousAppointments) {
            setPreviousAppointments(data.previousAppointments);
          }
          setLoadingHistory(false);
        })
        .catch(err => {
          console.error('Failed to fetch appointment history:', err);
          setLoadingHistory(false);
        });
    } else {
      setPreviousAppointments([]);
    }
  }, [open, appointment.id]);

  const getStatusColor = (status: string | null) => {
    if (!status) return 'bg-gray-50 text-gray-700 border-gray-200';
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
        return 'bg-gray-50 text-gray-700 border-gray-200';
    }
  };

  const statusColor = getStatusColor(appointment.status_category);
  const totalAttachments = appointment.customer_attachment_count + appointment.appointment_attachment_count;

  return (
    <Dialog open={open} onOpenChange={onOpenChange}>
      <DialogContent className="max-w-3xl max-h-[90vh] overflow-y-auto">
        <DialogHeader>
          <DialogTitle className="flex items-center gap-3 text-2xl">
            <Calendar className="h-6 w-6 text-blue-600" />
            Appointment Details
          </DialogTitle>
          <DialogDescription className="flex items-center gap-2">
            <span>RepCard ID: {appointment.repcard_appointment_id}</span>
            {appointment.is_confirmed && (
              <Badge variant="outline" className="bg-green-50 text-green-700 border-green-200">
                <CheckCircle2 className="h-3 w-3 mr-1" />
                Confirmed
              </Badge>
            )}
            {isUnassigned && (
              <Badge variant="outline" className="bg-amber-50 text-amber-700 border-amber-200">
                <AlertCircle className="h-3 w-3 mr-1" />
                Needs Closer Assignment
              </Badge>
            )}
            {appointment.status_category && (
              <Badge variant="outline" className={statusColor}>
                {capitalizeWords(appointment.status_category)}
              </Badge>
            )}
          </DialogDescription>
        </DialogHeader>

        <div className="space-y-6 mt-4">
          {/* Primary Info Card - Customer */}
          <div className="bg-gradient-to-r from-blue-50 to-transparent rounded-lg p-5 border border-blue-100">
            <div className="flex items-start gap-3">
              <div className="p-2.5 bg-blue-100 rounded-lg">
                <User className="h-6 w-6 text-blue-700" />
              </div>
              <div className="flex-1 min-w-0">
                <h3 className="text-lg font-semibold text-gray-900 mb-3">
                  {appointment.customer_name || 'Customer Name Not Available'}
                </h3>
                <div className="grid grid-cols-1 md:grid-cols-2 gap-3">
                  {appointment.customer_address && (
                    <div className="flex items-start gap-2 text-sm">
                      <MapPin className="h-4 w-4 text-gray-600 mt-0.5 shrink-0" />
                      <div>
                        <div className="font-medium text-gray-700">Address</div>
                        <a
                          href={`https://maps.google.com/?q=${encodeURIComponent(appointment.customer_address)}`}
                          target="_blank"
                          rel="noopener noreferrer"
                          className="text-gray-600 hover:text-blue-600 flex items-center gap-1"
                          onClick={(e) => e.stopPropagation()}
                        >
                          {appointment.customer_address}
                          <ExternalLink className="h-3 w-3" />
                        </a>
                      </div>
                    </div>
                  )}
                  {appointment.customer_phone && (
                    <div className="flex items-start gap-2 text-sm">
                      <Phone className="h-4 w-4 text-gray-600 mt-0.5 shrink-0" />
                      <div>
                        <div className="font-medium text-gray-700">Phone</div>
                        <a
                          href={`tel:${appointment.customer_phone}`}
                          className="text-gray-600 hover:text-blue-600"
                          onClick={(e) => e.stopPropagation()}
                        >
                          {appointment.customer_phone}
                        </a>
                      </div>
                    </div>
                  )}
                  {appointment.customer_email && (
                    <div className="flex items-start gap-2 text-sm md:col-span-2">
                      <Mail className="h-4 w-4 text-gray-600 mt-0.5 shrink-0" />
                      <div>
                        <div className="font-medium text-gray-700">Email</div>
                        <a
                          href={`mailto:${appointment.customer_email}`}
                          className="text-gray-600 hover:text-blue-600"
                          onClick={(e) => e.stopPropagation()}
                        >
                          {appointment.customer_email}
                        </a>
                      </div>
                    </div>
                  )}
                </div>
              </div>
            </div>
          </div>

          {/* Schedule Information */}
          <div>
            <div className="flex items-center gap-2 mb-3">
              <Clock className="h-5 w-5 text-gray-700" />
              <h3 className="font-semibold text-lg">Schedule Information</h3>
            </div>
            <div className="pl-7 space-y-3">
              {scheduledDate ? (
                <div className="bg-gray-50 rounded-lg p-4">
                  <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                    <div>
                      <div className="text-sm font-medium text-gray-500 mb-1">Appointment Date & Time</div>
                      <div className="text-base font-semibold text-gray-900">
                        {format(scheduledDate, 'EEEE, MMMM d, yyyy')}
                      </div>
                      <div className="text-base font-semibold text-gray-900">
                        {format(scheduledDate, 'h:mm a')}
                      </div>
                    </div>
                    {appointment.duration && (
                      <div>
                        <div className="text-sm font-medium text-gray-500 mb-1">Duration</div>
                        <div className="text-base font-semibold text-gray-900">
                          {appointment.duration} minutes
                        </div>
                      </div>
                    )}
                  </div>
                </div>
              ) : (
                <div className="bg-amber-50 border border-amber-200 rounded-lg p-4">
                  <div className="flex items-center gap-2 text-amber-700">
                    <AlertCircle className="h-4 w-4" />
                    <span className="font-medium">Not Yet Scheduled</span>
                  </div>
                </div>
              )}
              <div className="flex flex-wrap gap-4 text-sm text-gray-600">
                <div>
                  <span className="font-medium text-gray-700">Created:</span>{' '}
                  {format(createdDate, 'MMM d, yyyy')} at {format(createdDate, 'h:mm a')}
                </div>
                {appointment.calendar_name && (
                  <div>
                    <span className="font-medium text-gray-700">Calendar:</span>{' '}
                    {appointment.calendar_name}
                  </div>
                )}
              </div>
            </div>
          </div>

          <Separator />

          {/* Team Assignment */}
          <div>
            <div className="flex items-center gap-2 mb-3">
              <Users className="h-5 w-5 text-gray-700" />
              <h3 className="font-semibold text-lg">Team Assignment</h3>
            </div>
            <div className="grid grid-cols-1 md:grid-cols-2 gap-4 pl-7">
              <div className="bg-gray-50 rounded-lg p-4">
                <div className="text-xs font-medium text-gray-500 uppercase tracking-wide mb-2">Setter</div>
                <div className="text-base font-semibold text-gray-900">
                  {appointment.setter_name || 'Not Assigned'}
                </div>
                {appointment.setter_email && (
                  <div className="text-sm text-gray-600 mt-1">{appointment.setter_email}</div>
                )}
                {appointment.setter_team_name && (
                  <div className="flex items-center gap-1 text-sm text-gray-600 mt-2">
                    <Users className="h-3.5 w-3.5" />
                    <span>{appointment.setter_team_name}</span>
                  </div>
                )}
              </div>
              <div className={cn(
                "rounded-lg p-4",
                isUnassigned ? "bg-amber-50 border border-amber-200" : "bg-gray-50"
              )}>
                <div className="text-xs font-medium text-gray-500 uppercase tracking-wide mb-2">Closer</div>
                <div className={cn(
                  "text-base font-semibold",
                  isUnassigned ? "text-amber-700" : "text-gray-900"
                )}>
                  {appointment.closer_name || 'Unassigned'}
                </div>
                {appointment.closer_email && (
                  <div className="text-sm text-gray-600 mt-1">{appointment.closer_email}</div>
                )}
                {appointment.closer_team_name && (
                  <div className="flex items-center gap-1 text-sm text-gray-600 mt-2">
                    <Users className="h-3.5 w-3.5" />
                    <span>{appointment.closer_team_name}</span>
                  </div>
                )}
                {isUnassigned && (
                  <div className="flex items-center gap-1.5 text-sm text-amber-700 mt-3 font-medium">
                    <AlertCircle className="h-3.5 w-3.5" />
                    <span>Requires Assignment</span>
                  </div>
                )}
              </div>
            </div>
          </div>

          {/* Office/Location */}
          {appointment.office_name && (
            <>
              <Separator />
              <div>
                <div className="flex items-center gap-2 mb-3">
                  <Building2 className="h-5 w-5 text-gray-700" />
                  <h3 className="font-semibold text-lg">Office</h3>
                </div>
                <div className="pl-7">
                  <div className="text-base font-medium text-gray-900">{appointment.office_name}</div>
                </div>
              </div>
            </>
          )}

          {/* Status & Indicators */}
          <Separator />
          <div>
            <div className="flex items-center gap-2 mb-3">
              <CheckCircle2 className="h-5 w-5 text-gray-700" />
              <h3 className="font-semibold text-lg">Status & Indicators</h3>
            </div>
            <div className="flex flex-wrap gap-2 pl-7">
              {appointment.status_category && (
                <Badge variant="outline" className={cn("text-sm", statusColor)}>
                  {capitalizeWords(appointment.status_category)}
                </Badge>
              )}
              {appointment.disposition && appointment.disposition !== appointment.status_category && (
                <Badge variant="outline" className="text-sm">
                  {capitalizeWords(appointment.disposition)}
                </Badge>
              )}
              {appointment.has_power_bill && (
                <Badge variant="outline" className="bg-blue-50 text-blue-700 border-blue-200 text-sm">
                  <Paperclip className="h-3.5 w-3.5 mr-1" />
                  Power Bill Attached
                </Badge>
              )}
              {appointment.is_within_48_hours && (
                <Badge variant="outline" className="bg-green-50 text-green-700 border-green-200 text-sm">
                  <Clock className="h-3.5 w-3.5 mr-1" />
                  Within 48 Hours
                </Badge>
              )}
              {appointment.is_reschedule && (
                <Badge variant="outline" className="bg-purple-50 text-purple-700 border-purple-200 text-sm">
                  <RotateCcw className="h-3.5 w-3.5 mr-1" />
                  Rescheduled {appointment.reschedule_count ? `${appointment.reschedule_count}x` : ''}
                </Badge>
              )}
            </div>

            {/* Previous Appointments History */}
            {(previousAppointments.length > 0 || loadingHistory || appointment.is_reschedule) && (
              <div className="mt-4 pl-7">
                <Collapsible open={isHistoryOpen} onOpenChange={setIsHistoryOpen}>
                  <CollapsibleTrigger asChild>
                    <Button
                      variant="outline"
                      className="w-full justify-between hover:bg-purple-50 hover:border-purple-200"
                    >
                      <span className="flex items-center gap-2">
                        <History className="h-4 w-4" />
                        {previousAppointments.length > 0
                          ? `View ${previousAppointments.length} Previous Appointment${previousAppointments.length > 1 ? 's' : ''}`
                          : appointment.is_reschedule
                            ? `View Reschedule History (${appointment.reschedule_count || 1} time${(appointment.reschedule_count || 1) > 1 ? 's' : ''})`
                            : 'View Appointment History'}
                      </span>
                      <ChevronDown
                        className={cn(
                          "h-4 w-4 transition-transform duration-200",
                          isHistoryOpen && "transform rotate-180"
                        )}
                      />
                    </Button>
                  </CollapsibleTrigger>
                  <CollapsibleContent className="mt-3 space-y-3">
                    {loadingHistory ? (
                      <div className="text-sm text-gray-600 text-center py-4">Loading history...</div>
                    ) : previousAppointments.length > 0 ? (
                      previousAppointments.map((prevApt, index) => {
                        const prevScheduledDate = prevApt.scheduled_at ? new Date(prevApt.scheduled_at) : null;
                        const prevCompletedDate = prevApt.completed_at ? new Date(prevApt.completed_at) : null;
                        const prevStatusColor = getStatusColor(prevApt.status_category);

                        return (
                          <div
                            key={prevApt.id}
                            className="border-2 border-purple-100 rounded-lg p-4 space-y-3 bg-purple-50/30"
                          >
                            <div className="flex items-start justify-between gap-3">
                              <div className="flex-1">
                                <div className="flex items-center gap-2 mb-1">
                                  <span className="text-xs font-semibold text-purple-700 bg-purple-100 px-2 py-0.5 rounded">
                                    #{previousAppointments.length - index}
                                  </span>
                                  {prevScheduledDate ? (
                                    <div className="font-semibold text-gray-900">
                                      {format(prevScheduledDate, 'MMM d, yyyy')} at {format(prevScheduledDate, 'h:mm a')}
                                    </div>
                                  ) : (
                                    <div className="font-medium text-gray-500">Date not available</div>
                                  )}
                                </div>
                                {prevCompletedDate && (
                                  <div className="text-sm text-gray-600 ml-10">
                                    Completed: {format(prevCompletedDate, 'MMM d')} at {format(prevCompletedDate, 'h:mm a')}
                                  </div>
                                )}
                              </div>
                              <div className="flex items-center gap-2 shrink-0">
                                {prevApt.is_confirmed && (
                                  <CheckCircle2 className="h-4 w-4 text-green-600" title="Confirmed" />
                                )}
                                {prevApt.status_category && (
                                  <Badge variant="outline" className={cn("text-xs", prevStatusColor)}>
                                    {capitalizeWords(prevApt.status_category)}
                                  </Badge>
                                )}
                              </div>
                            </div>
                            {prevApt.disposition && prevApt.disposition !== prevApt.status_category && (
                              <div className="text-sm bg-white/50 rounded px-3 py-2">
                                <span className="font-medium text-gray-700">Outcome:</span>{' '}
                                <span className="text-gray-900">{capitalizeWords(prevApt.disposition)}</span>
                              </div>
                            )}
                            <div className="flex items-center gap-4 text-sm text-gray-600">
                              {prevApt.closer_name && (
                                <div>
                                  <span className="font-medium">Closer:</span> {prevApt.closer_name}
                                </div>
                              )}
                              {prevApt.setter_name && prevApt.setter_name !== 'Unassigned' && (
                                <div>
                                  <span className="font-medium">Setter:</span> {prevApt.setter_name}
                                </div>
                              )}
                            </div>
                            {prevApt.notes && (
                              <div className="text-sm bg-white/50 rounded px-3 py-2 border-t border-purple-100 mt-2">
                                <div className="font-medium text-gray-700 mb-1">Notes:</div>
                                <div className="text-gray-700 whitespace-pre-wrap">{prevApt.notes}</div>
                              </div>
                            )}
                          </div>
                        );
                      })
                    ) : (
                      <div className="text-sm text-gray-600 text-center py-4">No previous appointments found.</div>
                    )}
                  </CollapsibleContent>
                </Collapsible>
              </div>
            )}
          </div>

          {/* Attachments */}
          {totalAttachments > 0 && (
            <>
              <Separator />
              <div>
                <div className="flex items-center gap-2 mb-3">
                  <FileText className="h-5 w-5 text-gray-700" />
                  <h3 className="font-semibold text-lg">Attachments</h3>
                </div>
                <div className="pl-7 space-y-2">
                  {appointment.customer_attachment_count > 0 && (
                    <div className="flex items-center gap-2 text-sm bg-gray-50 rounded-lg px-3 py-2">
                      <Paperclip className="h-4 w-4 text-gray-600" />
                      <span className="font-medium">{appointment.customer_attachment_count}</span>
                      <span className="text-gray-600">customer attachment{appointment.customer_attachment_count > 1 ? 's' : ''}</span>
                    </div>
                  )}
                  {appointment.appointment_attachment_count > 0 && (
                    <div className="flex items-center gap-2 text-sm bg-gray-50 rounded-lg px-3 py-2">
                      <Paperclip className="h-4 w-4 text-gray-600" />
                      <span className="font-medium">{appointment.appointment_attachment_count}</span>
                      <span className="text-gray-600">appointment attachment{appointment.appointment_attachment_count > 1 ? 's' : ''}</span>
                    </div>
                  )}
                </div>
              </div>
            </>
          )}

          {/* Notes */}
          {appointment.notes && (
            <>
              <Separator />
              <div>
                <div className="flex items-center gap-2 mb-3">
                  <FileText className="h-5 w-5 text-gray-700" />
                  <h3 className="font-semibold text-lg">Notes</h3>
                </div>
                <div className="pl-7">
                  <div className="bg-gray-50 rounded-lg p-4">
                    <p className="text-sm text-gray-700 whitespace-pre-wrap leading-relaxed">
                      {appointment.notes}
                    </p>
                  </div>
                </div>
              </div>
            </>
          )}
        </div>
      </DialogContent>
    </Dialog>
  );
}
