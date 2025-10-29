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
import { Avatar, AvatarFallback, AvatarImage } from '@/components/ui/avatar';
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card';
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs';
import { 
  ExternalLink, 
  Copy, 
  Phone, 
  MapPin, 
  Clock, 
  User,
  Star,
  Image as ImageIcon,
  Download,
  TrendingUp,
  TrendingDown,
  CheckCircle2,
  Circle,
  MessageSquare,
  Users,
} from 'lucide-react';
import type { 
  EnhancedTaskDetails, 
  TaskAttachment, 
  TaskRating, 
  CrewContact,
  FieldTrackingStatus,
} from '@/lib/types/operations';
import { format } from 'date-fns';
import { toast } from 'sonner';

interface FieldTrackingDetailModalProps {
  taskId: string | null;
  onClose: () => void;
}

export function FieldTrackingDetailModal({ taskId, onClose }: FieldTrackingDetailModalProps) {
  const [loading, setLoading] = useState(false);
  const [details, setDetails] = useState<EnhancedTaskDetails | null>(null);

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
    // Use business tracker URL for internal operations
    const urlToCopy = details?.task.business_tracker_url || details?.task.tracker_url;
    if (urlToCopy) {
      navigator.clipboard.writeText(urlToCopy);
      toast.success('Business tracker URL copied to clipboard');
    }
  };

  const formatDuration = (minutes: number | null): string => {
    if (minutes === null) return 'N/A';
    const hours = Math.floor(minutes / 60);
    const mins = minutes % 60;
    if (hours > 0) {
      return `${hours}h ${mins}m`;
    }
    return `${mins}m`;
  };

  const renderStars = (rating: number) => {
    return (
      <div className="flex gap-0.5">
        {[1, 2, 3, 4, 5].map((star) => (
          <Star
            key={star}
            className={`h-5 w-5 ${
              star <= rating 
                ? 'fill-yellow-400 text-yellow-400' 
                : 'text-gray-300'
            }`}
          />
        ))}
      </div>
    );
  };

  const getStatusIcon = (statusType: string) => {
    switch (statusType) {
      case 'COMPLETE':
        return <CheckCircle2 className="h-5 w-5 text-green-500" />;
      case 'STARTED':
        return <Circle className="h-5 w-5 text-blue-500 fill-blue-500" />;
      case 'ENROUTE':
        return <Circle className="h-5 w-5 text-yellow-500 fill-yellow-500" />;
      default:
        return <Circle className="h-5 w-5 text-gray-400" />;
    }
  };

  const getInitials = (name: string): string => {
    return name
      .split(' ')
      .map(n => n[0])
      .join('')
      .toUpperCase()
      .slice(0, 2);
  };

  const statusHasAttachments = (statusId: number): boolean => {
    if (!details) return false;
    // Check DB flag or actual attachments array
    const status = details.statusHistory.find(s => s.id === statusId);
    if (status?.has_attachments) return true;
    // Also check if any attachment maps to this status
    return details.attachments.some(att => att.status_id === statusId);
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
            {/* Hero Section - Prominent Tracker Buttons */}
            {(details.task.tracker_url || details.task.business_tracker_url) && (
              <div className="grid grid-cols-1 md:grid-cols-3 gap-3">
                {details.task.tracker_url && (
                  <Button
                    variant="default"
                    size="lg"
                    onClick={() => window.open(details.task.tracker_url!, '_blank')}
                    className="w-full"
                  >
                    <ExternalLink className="mr-2 h-5 w-5" />
                    View Live Tracker
                  </Button>
                )}
                {details.task.business_tracker_url && (
                  <Button
                    variant="outline"
                    size="lg"
                    onClick={() => window.open(details.task.business_tracker_url!, '_blank')}
                    className="w-full"
                  >
                    <ExternalLink className="mr-2 h-5 w-5" />
                    View Internal Tracker
                  </Button>
                )}
                {(details.task.business_tracker_url || details.task.tracker_url) && (
                  <Button
                    variant="outline"
                    size="lg"
                    onClick={copyTrackerUrl}
                    className="w-full"
                  >
                    <Copy className="mr-2 h-5 w-5" />
                    Copy Business Link
                  </Button>
                )}
              </div>
            )}

            {/* Header */}
            <div className="flex items-start justify-between">
              <div>
                <h3 className="text-2xl font-bold">{details.task.customer_name}</h3>
                <p className="text-gray-600">Project ID: {details.task.quickbase_project_id}</p>
                {details.durationMetrics.is_completed && details.durationMetrics.actual_duration_minutes && (
                  <p className="text-sm text-green-600 mt-1">
                    Completed in {formatDuration(details.durationMetrics.actual_duration_minutes)}
                  </p>
                )}
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

            {/* Tabs */}
            <Tabs defaultValue="overview" className="w-full">
              <TabsList className="grid w-full grid-cols-5">
                <TabsTrigger value="overview">Overview</TabsTrigger>
                <TabsTrigger value="journal">
                  Journal {(details.statusHistory.filter(s => s.notes).length + details.customerNotes.length) > 0 &&
                    `(${details.statusHistory.filter(s => s.notes).length + details.customerNotes.length})`}
                </TabsTrigger>
                <TabsTrigger value="timeline">Timeline</TabsTrigger>
                <TabsTrigger value="photos">
                  Photos {details.attachments.length > 0 && `(${details.attachments.length})`}
                </TabsTrigger>
                <TabsTrigger value="feedback">
                  Feedback {details.ratings.length > 0 && `(${details.ratings.length})`}
                </TabsTrigger>
              </TabsList>

              {/* Overview Tab */}
              <TabsContent value="overview" className="space-y-6">
                {/* Customer Info */}
                <Card>
                  <CardHeader>
                    <CardTitle className="text-lg">Customer Information</CardTitle>
                  </CardHeader>
                  <CardContent className="grid grid-cols-2 gap-4">
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
                  </CardContent>
                </Card>

                {/* Schedule & Duration Metrics */}
                <Card>
                  <CardHeader>
                    <CardTitle className="text-lg">Schedule & Duration</CardTitle>
                  </CardHeader>
                  <CardContent className="space-y-4">
                    {details.task.scheduled_start && (
                      <div>
                        <p className="text-sm text-gray-600 mb-1">Scheduled Time</p>
                        <div className="flex items-center gap-2 text-gray-900">
                          <Clock className="h-4 w-4" />
                          <span>{format(new Date(details.task.scheduled_start), 'EEEE, MMMM d, yyyy at h:mm a')}</span>
                        </div>
                      </div>
                    )}

                    {(details.durationMetrics.scheduled_duration_minutes || 
                      details.durationMetrics.actual_duration_minutes) && (
                      <div className="grid grid-cols-2 gap-4 pt-3 border-t">
                        <div>
                          <p className="text-sm text-gray-600 mb-1">Scheduled Duration</p>
                          <p className="font-medium">{formatDuration(details.durationMetrics.scheduled_duration_minutes)}</p>
                        </div>
                        {details.durationMetrics.actual_duration_minutes && (
                          <div>
                            <p className="text-sm text-gray-600 mb-1">Actual Duration</p>
                            <p className="font-medium">{formatDuration(details.durationMetrics.actual_duration_minutes)}</p>
                          </div>
                        )}
                        {details.durationMetrics.time_to_start_minutes !== null && (
                          <div className="col-span-2">
                            <p className="text-sm text-gray-600 mb-1">Start Status</p>
                            <div className="flex items-center gap-2">
                              {details.durationMetrics.is_delayed ? (
                                <>
                                  <TrendingDown className="h-4 w-4 text-red-500" />
                                  <span className="text-red-600 font-medium">
                                    Started {formatDuration(Math.abs(details.durationMetrics.time_to_start_minutes))} late
                                  </span>
                                </>
                              ) : (
                                <>
                                  <TrendingUp className="h-4 w-4 text-green-500" />
                                  <span className="text-green-600 font-medium">On Time</span>
                                </>
                              )}
                            </div>
                          </div>
                        )}
                      </div>
                    )}
                  </CardContent>
                </Card>

                {/* Crew Contacts */}
                <Card>
                  <CardHeader>
                    <CardTitle className="text-lg flex items-center gap-2">
                      <Users className="h-5 w-5" />
                      Assigned Crew
                    </CardTitle>
                  </CardHeader>
                  <CardContent>
                    {details.crewContacts.length === 0 ? (
                      <p className="text-gray-500 text-sm">No crew assigned</p>
                    ) : (
                      <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                        {details.crewContacts.map((crew) => (
                          <div 
                            key={crew.entity_id} 
                            className="flex items-center gap-3 p-3 border rounded-lg"
                          >
                            <Avatar className="h-12 w-12">
                              <AvatarFallback className="bg-blue-100 text-blue-700">
                                {getInitials(crew.name)}
                              </AvatarFallback>
                            </Avatar>
                            <div className="flex-1 min-w-0">
                              <p className="font-medium truncate">{crew.name}</p>
                              {crew.entity_type && (
                                <p className="text-xs text-gray-500">{crew.entity_type}</p>
                              )}
                              {crew.phone && (
                                <p className="text-sm text-gray-600 truncate">{crew.phone}</p>
                              )}
                              {crew.email && (
                                <p className="text-sm text-gray-600 truncate">{crew.email}</p>
                              )}
                            </div>
                            <div className="flex gap-2">
                              {crew.phone && (
                                <Button
                                  variant="ghost"
                                  size="sm"
                                  onClick={() => window.open(`tel:${crew.phone}`, '_self')}
                                >
                                  <Phone className="h-4 w-4" />
                                </Button>
                              )}
                              {crew.phone && (
                                <Button
                                  variant="ghost"
                                  size="sm"
                                  onClick={() => window.open(`sms:${crew.phone}`, '_self')}
                                >
                                  <MessageSquare className="h-4 w-4" />
                                </Button>
                              )}
                            </div>
                          </div>
                        ))}
                      </div>
                    )}
                  </CardContent>
                </Card>
              </TabsContent>

              {/* Journal Tab */}
              <TabsContent value="journal" className="space-y-4">
                <Card>
                  <CardHeader>
                    <CardTitle className="text-lg">Task Journal</CardTitle>
                  </CardHeader>
                  <CardContent>
                    {(() => {
                      // Combine status notes and customer notes
                      const statusNotes = details.statusHistory
                        .filter(s => s.notes)
                        .map(s => ({
                          type: 'status' as const,
                          timestamp: new Date(s.reported_at),
                          user: s.reporter_name || 'Unknown',
                          statusType: s.status_type,
                          message: s.notes!,
                          id: `status-${s.id}`,
                        }));

                      const customerNoteEntries = details.customerNotes.map(n => ({
                        type: 'customer' as const,
                        timestamp: new Date(n.created_at),
                        user: n.customer_name || 'Customer',
                        message: n.note,
                        id: `note-${n.event_id}`,
                      }));

                      // Combine and sort by timestamp (newest first)
                      const allEntries = [...statusNotes, ...customerNoteEntries]
                        .sort((a, b) => b.timestamp.getTime() - a.timestamp.getTime());

                      if (allEntries.length === 0) {
                        return <p className="text-gray-500 text-sm">No journal entries yet</p>;
                      }

                      return (
                        <div className="space-y-4">
                          {allEntries.map((entry) => (
                            <div key={entry.id} className="border rounded-lg p-4 hover:bg-gray-50 transition-colors">
                              <div className="flex items-start gap-3">
                                <Avatar className="h-10 w-10 flex-shrink-0">
                                  <AvatarFallback className={
                                    entry.type === 'customer'
                                      ? 'bg-blue-100 text-blue-700'
                                      : 'bg-gray-100 text-gray-700'
                                  }>
                                    {getInitials(entry.user)}
                                  </AvatarFallback>
                                </Avatar>
                                <div className="flex-1 min-w-0">
                                  <div className="flex items-center gap-2 mb-1">
                                    <span className="font-medium text-sm">{entry.user}</span>
                                    {entry.type === 'status' && entry.statusType && (
                                      <Badge variant="outline" className="text-xs">
                                        {entry.statusType}
                                      </Badge>
                                    )}
                                    {entry.type === 'customer' && (
                                      <Badge variant="outline" className="text-xs bg-blue-50">
                                        Customer Note
                                      </Badge>
                                    )}
                                  </div>
                                  <p className="text-xs text-gray-500 mb-2">
                                    {format(entry.timestamp, 'EEEE, MMMM d, yyyy at h:mm a')}
                                  </p>
                                  <p className="text-sm text-gray-700 whitespace-pre-wrap">
                                    {entry.message}
                                  </p>
                                </div>
                              </div>
                            </div>
                          ))}
                        </div>
                      );
                    })()}
                  </CardContent>
                </Card>
              </TabsContent>

              {/* Timeline Tab */}
              <TabsContent value="timeline" className="space-y-4">
                <Card>
                  <CardHeader>
                    <CardTitle className="text-lg">Status Timeline</CardTitle>
                  </CardHeader>
                  <CardContent>
                    {details.statusHistory.length === 0 ? (
                      <p className="text-gray-500 text-sm">No status updates yet</p>
                    ) : (
                      <div className="relative space-y-6">
                        {/* Vertical connecting line */}
                        <div className="absolute left-[12px] top-2 bottom-2 w-0.5 bg-gray-200" />
                        
                        {/* Reverse to show chronological order (oldest to newest) */}
                        {[...details.statusHistory].reverse().map((status, index) => (
                          <div key={status.id} className="relative flex gap-4">
                            <div className="relative z-10 flex-shrink-0">
                              {getStatusIcon(status.status_type)}
                            </div>
                            <div className="flex-1 pb-6">
                              <div className="flex items-center justify-between mb-1">
                                <Badge variant="outline">{status.status_type}</Badge>
                                <span className="text-xs text-gray-500">
                                  {format(new Date(status.reported_at), 'MMM d, h:mm a')}
                                </span>
                              </div>
                              {status.notes && (
                                <p className="text-sm text-gray-600 mt-2">{status.notes}</p>
                              )}
                              <div className="flex items-center gap-3 mt-2">
                                {status.reporter_name && (
                                  <div className="flex items-center gap-2">
                                    <Avatar className="h-6 w-6">
                                      <AvatarFallback className="text-xs bg-gray-100">
                                        {getInitials(status.reporter_name)}
                                      </AvatarFallback>
                                    </Avatar>
                                    <span className="text-xs text-gray-500">{status.reporter_name}</span>
                                  </div>
                                )}
                                {statusHasAttachments(status.id) && (
                                  <div className="flex items-center gap-1 text-xs text-gray-500">
                                    <ImageIcon className="h-3 w-3" />
                                    <span>Has attachments</span>
                                  </div>
                                )}
                              </div>
                            </div>
                          </div>
                        ))}
                      </div>
                    )}
                  </CardContent>
                </Card>
              </TabsContent>

              {/* Photos Tab */}
              <TabsContent value="photos" className="space-y-4">
                <Card>
                  <CardHeader>
                    <CardTitle className="text-lg">Task Photos & Attachments</CardTitle>
                  </CardHeader>
                  <CardContent>
                    {details.attachments.length === 0 ? (
                      <p className="text-gray-500 text-sm">No photos uploaded yet</p>
                    ) : (
                      <>
                        <p className="text-sm text-gray-600 mb-4">
                          Photos are stored in Arrivy. Click to view in Arrivy's interface.
                        </p>
                        <div className="grid grid-cols-2 md:grid-cols-3 gap-4">
                          {details.attachments.map((attachment) => (
                            <div 
                              key={attachment.file_id}
                              className="border rounded-lg p-3 hover:bg-gray-50 transition-colors"
                            >
                              <div className="flex items-center gap-2 mb-2">
                                <ImageIcon className="h-5 w-5 text-gray-400" />
                                <span className="text-sm font-medium truncate">{attachment.filename}</span>
                              </div>
                              {attachment.uploaded_by && (
                                <p className="text-xs text-gray-500 mb-1">By {attachment.uploaded_by}</p>
                              )}
                              <p className="text-xs text-gray-500 mb-2">
                                {format(new Date(attachment.uploaded_at), 'MMM d, h:mm a')}
                              </p>
                              <Button
                                variant="outline"
                                size="sm"
                                onClick={() => {
                                  const trackerUrl = details.task.business_tracker_url || details.task.tracker_url;
                                  if (trackerUrl) window.open(trackerUrl, '_blank');
                                }}
                                disabled={!details.task.business_tracker_url && !details.task.tracker_url}
                                className="w-full"
                                title={!details.task.business_tracker_url && !details.task.tracker_url ? 'Tracker link not available' : undefined}
                              >
                                <ExternalLink className="mr-2 h-3 w-3" />
                                View in Arrivy
                              </Button>
                            </div>
                          ))}
                        </div>
                      </>
                    )}
                  </CardContent>
                </Card>
              </TabsContent>

              {/* Feedback Tab */}
              <TabsContent value="feedback" className="space-y-4">
                {/* Customer Ratings */}
                <Card>
                  <CardHeader>
                    <CardTitle className="text-lg">Customer Ratings</CardTitle>
                  </CardHeader>
                  <CardContent>
                    {details.ratings.length === 0 ? (
                      <p className="text-gray-500 text-sm">No customer ratings yet</p>
                    ) : (
                      <div className="space-y-4">
                        {details.ratings.map((rating) => (
                          <div key={rating.event_id} className="border rounded-lg p-4">
                            <div className="flex items-start justify-between mb-3">
                              <div>
                                {renderStars(rating.rating)}
                                <p className="text-sm text-gray-600 mt-1">
                                  {rating.customer_name || 'Customer'} • {format(new Date(rating.rated_at), 'MMM d, yyyy')}
                                </p>
                              </div>
                              <Badge variant="outline" className="text-xs">
                                {rating.rating_type}
                              </Badge>
                            </div>
                            {rating.feedback && (
                              <p className="text-sm text-gray-700 italic">"{rating.feedback}"</p>
                            )}
                          </div>
                        ))}
                      </div>
                    )}
                  </CardContent>
                </Card>

                {/* Customer Notes */}
                {details.customerNotes && details.customerNotes.length > 0 && (
                  <Card>
                    <CardHeader>
                      <CardTitle className="text-lg">Customer Notes</CardTitle>
                    </CardHeader>
                    <CardContent>
                      <div className="space-y-3">
                        {details.customerNotes.map((note) => (
                          <div key={note.event_id} className="border-l-4 border-blue-400 pl-4 py-2">
                            <p className="text-sm text-gray-700">{note.note}</p>
                            <p className="text-xs text-gray-500 mt-2">
                              {note.customer_name || 'Customer'} • {format(new Date(note.created_at), 'MMM d, yyyy h:mm a')}
                            </p>
                          </div>
                        ))}
                      </div>
                    </CardContent>
                  </Card>
                )}
              </TabsContent>
            </Tabs>
          </div>
        )}
      </DialogContent>
    </Dialog>
  );
}
