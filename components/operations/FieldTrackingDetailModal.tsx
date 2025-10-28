'use client';

import { useEffect, useState } from 'react';
import {
  Dialog,
  DialogContent,
  DialogHeader,
  DialogTitle,
} from '@/components/ui/dialog';
import { Badge } from '@/components/ui/badge';
import { Button } from '@/components/ui/button';
import { Skeleton } from '@/components/ui/skeleton';
import { ExternalLink, Copy, Phone, MapPin, Clock, User } from 'lucide-react';
import type { FieldTrackingTask, FieldTrackingStatus, FieldTrackingEvent } from '@/lib/types/operations';
import { format } from 'date-fns';
import { toast } from 'sonner';

interface FieldTrackingDetailModalProps {
  taskId: string | null;
  onClose: () => void;
}

interface TaskDetails {
  task: FieldTrackingTask;
  statusHistory: FieldTrackingStatus[];
  events: FieldTrackingEvent[];
}

export function FieldTrackingDetailModal({ taskId, onClose }: FieldTrackingDetailModalProps) {
  const [loading, setLoading] = useState(false);
  const [details, setDetails] = useState<TaskDetails | null>(null);

  useEffect(() => {
    if (!taskId) {
      setDetails(null);
      return;
    }

    const fetchDetails = async () => {
      setLoading(true);
      try {
        const response = await fetch(`/api/operations/field-tracking/tasks/${taskId}`);
        if (response.ok) {
          const data = await response.json();
          setDetails(data);
        } else {
          toast.error('Failed to load task details');
        }
      } catch (error) {
        toast.error('Failed to load task details');
      } finally {
        setLoading(false);
      }
    };

    fetchDetails();
  }, [taskId]);

  const copyTrackerUrl = () => {
    if (details?.task.tracker_url) {
      navigator.clipboard.writeText(details.task.tracker_url);
      toast.success('Tracker URL copied to clipboard');
    }
  };

  return (
    <Dialog open={!!taskId} onOpenChange={(open) => !open && onClose()}>
      <DialogContent className="max-w-4xl max-h-[90vh] overflow-y-auto">
        <DialogHeader>
          <DialogTitle>Field Task Details</DialogTitle>
        </DialogHeader>

        {loading && (
          <div className="space-y-4">
            <Skeleton className="h-20 w-full" />
            <Skeleton className="h-40 w-full" />
            <Skeleton className="h-32 w-full" />
          </div>
        )}

        {!loading && details && (
          <div className="space-y-6">
            {/* Header */}
            <div className="flex items-start justify-between">
              <div>
                <h3 className="text-2xl font-bold">{details.task.customer_name}</h3>
                <p className="text-gray-600">Project ID: {details.task.quickbase_project_id}</p>
              </div>
              <div className="flex gap-2">
                <Badge variant="outline" className="text-sm">
                  {details.task.task_type?.toUpperCase()}
                </Badge>
                <Badge variant="outline" className="text-sm">
                  {details.task.current_status}
                </Badge>
              </div>
            </div>

            {/* Customer Info */}
            <div className="grid grid-cols-2 gap-4 p-4 bg-gray-50 rounded-lg">
              <div>
                <p className="text-sm text-gray-600 mb-1">Phone</p>
                <div className="flex items-center gap-2">
                  <Phone className="h-4 w-4 text-gray-400" />
                  <p className="font-medium">{details.task.customer_phone || 'N/A'}</p>
                </div>
              </div>
              <div>
                <p className="text-sm text-gray-600 mb-1">Email</p>
                <p className="font-medium">{details.task.customer_email || 'N/A'}</p>
              </div>
              {details.task.customer_address && (
                <div className="col-span-2">
                  <p className="text-sm text-gray-600 mb-1">Address</p>
                  <div className="flex items-center gap-2">
                    <MapPin className="h-4 w-4 text-gray-400" />
                    <p className="font-medium">{details.task.customer_address}</p>
                  </div>
                </div>
              )}
            </div>

            {/* Schedule */}
            {details.task.scheduled_start && (
              <div>
                <h4 className="font-semibold mb-2">Schedule</h4>
                <div className="flex items-center gap-2 text-gray-600">
                  <Clock className="h-4 w-4" />
                  <span>{format(new Date(details.task.scheduled_start), 'EEEE, MMMM d, yyyy at h:mm a')}</span>
                </div>
              </div>
            )}

            {/* Assigned Crew */}
            {details.task.entity_names && details.task.entity_names.length > 0 && (
              <div>
                <h4 className="font-semibold mb-2">Assigned Crew</h4>
                <div className="flex items-center gap-2 text-gray-600">
                  <User className="h-4 w-4" />
                  <span>{details.task.entity_names.join(', ')}</span>
                </div>
              </div>
            )}

            {/* Customer Tracker */}
            {details.task.tracker_url && (
              <div>
                <h4 className="font-semibold mb-2">Customer Tracker</h4>
                <div className="flex gap-2">
                  <Button
                    variant="outline"
                    size="sm"
                    onClick={copyTrackerUrl}
                    className="flex-1"
                  >
                    <Copy className="mr-2 h-4 w-4" />
                    Copy URL
                  </Button>
                  <Button
                    variant="default"
                    size="sm"
                    onClick={() => window.open(details.task.tracker_url!, '_blank')}
                    className="flex-1"
                  >
                    <ExternalLink className="mr-2 h-4 w-4" />
                    Open Tracker
                  </Button>
                </div>
              </div>
            )}

            {/* Status Timeline */}
            <div>
              <h4 className="font-semibold mb-3">Status Timeline</h4>
              <div className="space-y-2">
                {details.statusHistory.length === 0 ? (
                  <p className="text-gray-500 text-sm">No status updates yet</p>
                ) : (
                  details.statusHistory.map((status) => (
                    <div key={status.id} className="flex items-start gap-3 p-3 bg-gray-50 rounded">
                      <div className="flex-1">
                        <div className="flex items-center justify-between">
                          <Badge variant="outline">{status.status_type}</Badge>
                          <span className="text-xs text-gray-500">
                            {format(new Date(status.reported_at), 'MMM d, h:mm a')}
                          </span>
                        </div>
                        {status.notes && (
                          <p className="text-sm text-gray-600 mt-1">{status.notes}</p>
                        )}
                        {status.reporter_name && (
                          <p className="text-xs text-gray-500 mt-1">By {status.reporter_name}</p>
                        )}
                      </div>
                    </div>
                  ))
                )}
              </div>
            </div>

            {/* Recent Events */}
            <div>
              <h4 className="font-semibold mb-3">Recent Events</h4>
              <div className="space-y-2 max-h-48 overflow-y-auto">
                {details.events.length === 0 ? (
                  <p className="text-gray-500 text-sm">No events yet</p>
                ) : (
                  details.events.map((event) => (
                    <div key={event.id} className="p-2 bg-gray-50 rounded text-sm">
                      <div className="flex justify-between">
                        <span className="font-medium">{event.title || event.event_type}</span>
                        <span className="text-xs text-gray-500">
                          {format(new Date(event.timestamp), 'MMM d, h:mm a')}
                        </span>
                      </div>
                      {event.message && <p className="text-gray-600 mt-1">{event.message}</p>}
                    </div>
                  ))
                )}
              </div>
            </div>
          </div>
        )}
      </DialogContent>
    </Dialog>
  );
}

