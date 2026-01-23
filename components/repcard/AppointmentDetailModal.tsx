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
import { Separator } from '@/components/ui/separator';
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
  CheckCircle2
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

export function AppointmentDetailModal({
  appointment,
  open,
  onOpenChange
}: AppointmentDetailModalProps) {
  const scheduledDate = appointment.scheduled_at ? new Date(appointment.scheduled_at) : null;
  const createdDate = new Date(appointment.created_at);
  const [previousAppointments, setPreviousAppointments] = useState<PreviousAppointment[]>([]);
  const [loadingHistory, setLoadingHistory] = useState(false);

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
  const totalAttachments = appointment.customer_attachment_count + appointment.appointment_attachment_count;

  return (
    <Dialog open={open} onOpenChange={onOpenChange}>
      <DialogContent className="max-w-2xl max-h-[90vh] overflow-y-auto">
        <DialogHeader>
          <DialogTitle className="flex items-center gap-2">
            Appointment Details
            {appointment.status_category && (
              <Badge className={statusColor}>
                {appointment.status_category}
              </Badge>
            )}
          </DialogTitle>
          <DialogDescription>
            {appointment.repcard_appointment_id && `RepCard ID: ${appointment.repcard_appointment_id}`}
          </DialogDescription>
        </DialogHeader>

        <div className="space-y-6">
          {/* Schedule Information */}
          <div>
            <h3 className="font-semibold mb-3 flex items-center gap-2">
              <Clock className="h-4 w-4" />
              Schedule
            </h3>
            <div className="space-y-2 pl-6">
              {scheduledDate ? (
                <>
                  <div>
                    <span className="text-sm text-muted-foreground">Scheduled:</span>{' '}
                    <span className="font-medium">
                      {format(scheduledDate, 'EEEE, MMMM d, yyyy')} at {format(scheduledDate, 'h:mm a')}
                    </span>
                  </div>
                  {appointment.duration && (
                    <div>
                      <span className="text-sm text-muted-foreground">Duration:</span>{' '}
                      <span className="font-medium">{appointment.duration} minutes</span>
                    </div>
                  )}
                </>
              ) : (
                <div className="text-muted-foreground">Not scheduled</div>
              )}
              <div>
                <span className="text-sm text-muted-foreground">Created:</span>{' '}
                <span className="font-medium">
                  {format(createdDate, 'MMM d, yyyy')} at {format(createdDate, 'h:mm a')}
                </span>
              </div>
              {appointment.calendar_name && (
                <div>
                  <span className="text-sm text-muted-foreground">Calendar:</span>{' '}
                  <span className="font-medium">{appointment.calendar_name}</span>
                </div>
              )}
            </div>
          </div>

          <Separator />

          {/* Customer Information */}
          <div>
            <h3 className="font-semibold mb-3 flex items-center gap-2">
              <User className="h-4 w-4" />
              Customer
            </h3>
            <div className="space-y-2 pl-6">
              <div>
                <span className="font-medium">{appointment.customer_name || 'Unknown Customer'}</span>
              </div>
              {appointment.customer_address && (
                <div className="flex items-start gap-2 text-sm">
                  <MapPin className="h-4 w-4 mt-0.5 shrink-0 text-muted-foreground" />
                  <span>{appointment.customer_address}</span>
                </div>
              )}
              {appointment.customer_phone && (
                <div className="flex items-center gap-2 text-sm">
                  <Phone className="h-4 w-4 shrink-0 text-muted-foreground" />
                  <span>{appointment.customer_phone}</span>
                </div>
              )}
              {appointment.customer_email && (
                <div className="flex items-center gap-2 text-sm">
                  <Mail className="h-4 w-4 shrink-0 text-muted-foreground" />
                  <span>{appointment.customer_email}</span>
                </div>
              )}
            </div>
          </div>

          <Separator />

          {/* Setter and Closer */}
          <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
            <div>
              <h3 className="font-semibold mb-3">Setter</h3>
              <div className="space-y-1 pl-6">
                <div className="font-medium">
                  {appointment.setter_name || 'Unknown'}
                </div>
                {appointment.setter_email && (
                  <div className="text-sm text-muted-foreground">{appointment.setter_email}</div>
                )}
                {appointment.setter_team_name && (
                  <div className="text-sm text-muted-foreground">Team: {appointment.setter_team_name}</div>
                )}
              </div>
            </div>
            <div>
              <h3 className="font-semibold mb-3">Closer</h3>
              <div className="space-y-1 pl-6">
                <div className="font-medium">
                  {appointment.closer_name || 'Unassigned'}
                </div>
                {appointment.closer_email && (
                  <div className="text-sm text-muted-foreground">{appointment.closer_email}</div>
                )}
                {appointment.closer_team_name && (
                  <div className="text-sm text-muted-foreground">Team: {appointment.closer_team_name}</div>
                )}
              </div>
            </div>
          </div>

          <Separator />

          {/* Status and Flags */}
          <div>
            <h3 className="font-semibold mb-3">Status & Flags</h3>
            <div className="flex flex-wrap gap-2 pl-6">
              {appointment.status_category && (
                <Badge className={statusColor}>
                  {appointment.status_category}
                </Badge>
              )}
              {appointment.disposition && appointment.disposition !== appointment.status_category && (
                <Badge variant="outline">
                  {appointment.disposition}
                </Badge>
              )}
              {appointment.has_power_bill && (
                <Badge variant="outline" className="bg-blue-50 text-blue-700 border-blue-200">
                  <Paperclip className="h-3 w-3 mr-1" />
                  Has Power Bill
                </Badge>
              )}
              {appointment.is_reschedule && (
                <Badge variant="outline" className="bg-yellow-50 text-yellow-700 border-yellow-200">
                  <RotateCcw className="h-3 w-3 mr-1" />
                  {appointment.reschedule_count 
                    ? `Rescheduled ${appointment.reschedule_count} time${appointment.reschedule_count > 1 ? 's' : ''}`
                    : 'Rescheduled'}
                </Badge>
              )}
              {appointment.is_within_48_hours && (
                <Badge variant="outline" className="bg-green-50 text-green-700 border-green-200">
                  Within 48 Hours
                </Badge>
              )}
            </div>
          </div>

          {/* Attachments */}
          {totalAttachments > 0 && (
            <>
              <Separator />
              <div>
                <h3 className="font-semibold mb-3 flex items-center gap-2">
                  <FileText className="h-4 w-4" />
                  Attachments
                </h3>
                <div className="pl-6 space-y-1">
                  {appointment.customer_attachment_count > 0 && (
                    <div className="text-sm">
                      <Paperclip className="h-3 w-3 inline mr-1" />
                      {appointment.customer_attachment_count} customer attachment(s)
                    </div>
                  )}
                  {appointment.appointment_attachment_count > 0 && (
                    <div className="text-sm">
                      <Paperclip className="h-3 w-3 inline mr-1" />
                      {appointment.appointment_attachment_count} appointment attachment(s)
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
                <h3 className="font-semibold mb-3">Notes</h3>
                <div className="pl-6">
                  <p className="text-sm whitespace-pre-wrap">{appointment.notes}</p>
                </div>
              </div>
            </>
          )}

          {/* Previous Appointments */}
          {previousAppointments.length > 0 && (
            <>
              <Separator />
              <div>
                <h3 className="font-semibold mb-3 flex items-center gap-2">
                  <History className="h-4 w-4" />
                  Previous Appointments
                </h3>
                <div className="pl-6 space-y-3">
                  {previousAppointments.map((prevApt) => {
                    const prevScheduledDate = prevApt.scheduled_at ? new Date(prevApt.scheduled_at) : null;
                    const prevCompletedDate = prevApt.completed_at ? new Date(prevApt.completed_at) : null;
                    const prevStatusColor = getStatusColor(prevApt.status_category);

                    return (
                      <div
                        key={prevApt.id}
                        className="border rounded-lg p-3 space-y-2 bg-muted/30"
                      >
                        <div className="flex items-start justify-between gap-2">
                          <div className="flex-1">
                            {prevScheduledDate ? (
                              <div className="font-medium text-sm">
                                {format(prevScheduledDate, 'EEEE, MMMM d, yyyy')} at {format(prevScheduledDate, 'h:mm a')}
                              </div>
                            ) : (
                              <div className="font-medium text-sm text-muted-foreground">Date not available</div>
                            )}
                            {prevCompletedDate && (
                              <div className="text-xs text-muted-foreground mt-0.5">
                                Completed: {format(prevCompletedDate, 'MMM d, yyyy')} at {format(prevCompletedDate, 'h:mm a')}
                              </div>
                            )}
                          </div>
                          <div className="flex items-center gap-1.5 shrink-0">
                            {prevApt.is_confirmed && (
                              <CheckCircle2 className="h-4 w-4 text-green-600" title="Confirmed" />
                            )}
                            {prevApt.status_category && (
                              <Badge className={cn("text-xs", prevStatusColor)}>
                                {prevApt.status_category}
                              </Badge>
                            )}
                          </div>
                        </div>
                        {prevApt.disposition && prevApt.disposition !== prevApt.status_category && (
                          <div className="text-xs">
                            <span className="text-muted-foreground">Outcome:</span>{' '}
                            <span className="font-medium">{prevApt.disposition}</span>
                          </div>
                        )}
                        <div className="flex items-center gap-4 text-xs text-muted-foreground">
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
                          <div className="text-xs text-muted-foreground pt-1 border-t">
                            <span className="font-medium">Note:</span> {prevApt.notes}
                          </div>
                        )}
                        {prevApt.is_reschedule && (
                          <div className="text-xs">
                            <Badge variant="outline" className="bg-yellow-50 text-yellow-700 border-yellow-200">
                              <RotateCcw className="h-2.5 w-2.5 mr-1" />
                              {prevApt.reschedule_count 
                                ? `Rescheduled ${prevApt.reschedule_count} time${prevApt.reschedule_count > 1 ? 's' : ''}`
                                : 'Rescheduled'}
                            </Badge>
                          </div>
                        )}
                      </div>
                    );
                  })}
                </div>
              </div>
            </>
          )}
          {loadingHistory && (
            <>
              <Separator />
              <div className="pl-6">
                <p className="text-sm text-muted-foreground">Loading appointment history...</p>
              </div>
            </>
          )}
        </div>
      </DialogContent>
    </Dialog>
  );
}
