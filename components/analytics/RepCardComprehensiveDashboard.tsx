'use client';

import { useQuery } from '@tanstack/react-query';
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card';
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs';
import { Badge } from '@/components/ui/badge';
import { 
  Calendar, 
  DoorOpen, 
  Users, 
  TrendingUp, 
  FileText, 
  MessageSquare, 
  Paperclip,
  Award,
  Building2,
  Clock,
  CheckCircle2,
  XCircle,
  AlertCircle
} from 'lucide-react';
import { cn } from '@/lib/utils';
import { Skeleton } from '@/components/ui/skeleton';

interface ComprehensiveStats {
  success: boolean;
  dateRange: {
    startDate: string;
    endDate: string;
  };
  overview: {
    doorsKnocked: number;
    appointmentsSet: number;
    salesClosed: number;
    customersWithAttachments: number;
    totalNotes: number;
    statusChanges: number;
    conversionRate: number;
    closeRate: number;
  };
  userPerformance: Array<{
    repcardUserId: number;
    name: string;
    email: string;
    office: string | null;
    officeId: number | null;
    doorsKnocked: number;
    appointmentsSet: number;
    appointmentsClosed: number;
    salesClosed: number;
    customersWithAttachments: number;
    notesWritten: number;
    appointmentsWithin48h: number;
    conversionRate: number;
    closeRate: number;
  }>;
  officeBreakdown: Array<{
    officeId: number;
    officeName: string;
    doorsKnocked: number;
    appointmentsSet: number;
    salesClosed: number;
    activeReps: number;
  }>;
  customers?: Array<any>;
  appointments?: Array<any>;
  statusLogs?: Array<any>;
  attachments?: Array<any>;
  notes?: Array<any>;
}

export function RepCardComprehensiveDashboard() {
  const { data, isLoading, error } = useQuery<ComprehensiveStats>({
    queryKey: ['repcard-comprehensive-stats'],
    queryFn: async () => {
      const response = await fetch('/api/repcard/comprehensive-stats?includeDetails=true');
      if (!response.ok) throw new Error('Failed to fetch RepCard stats');
      return response.json();
    },
    refetchInterval: 30000, // Refresh every 30 seconds
  });

  if (isLoading) {
    return (
      <div className="space-y-6">
        <Skeleton className="h-32 w-full" />
        <Skeleton className="h-64 w-full" />
        <Skeleton className="h-96 w-full" />
      </div>
    );
  }

  if (error || !data?.success) {
    return (
      <Card>
        <CardContent className="pt-6">
          <p className="text-sm text-muted-foreground text-center">
            {error instanceof Error ? error.message : 'Failed to load RepCard data'}
          </p>
        </CardContent>
      </Card>
    );
  }

  const { overview, userPerformance, officeBreakdown, customers = [], appointments = [], statusLogs = [], attachments = [], notes = [] } = data;

  const formatNumber = (num: number) => {
    if (num >= 1000) return `${(num / 1000).toFixed(1)}K`;
    return num.toString();
  };

  const formatPercentage = (num: number) => {
    return `${num.toFixed(1)}%`;
  };

  const getDispositionColor = (disposition: string) => {
    if (!disposition) return 'bg-gray-100 text-gray-800';
    const d = disposition.toLowerCase();
    if (d.includes('closed')) return 'bg-green-100 text-green-800';
    if (d.includes('cancel')) return 'bg-red-100 text-red-800';
    if (d.includes('reschedule')) return 'bg-yellow-100 text-yellow-800';
    if (d.includes('no.show') || d.includes('no_show')) return 'bg-orange-100 text-orange-800';
    return 'bg-blue-100 text-blue-800';
  };

  return (
    <div className="space-y-6">
      {/* Overview Cards */}
      <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-4">
        <Card>
          <CardContent className="pt-6">
            <div className="flex items-center gap-2 mb-2">
              <DoorOpen className="h-5 w-5 text-blue-600" />
              <span className="text-sm font-semibold text-blue-700">Doors Knocked</span>
            </div>
            <p className="text-3xl font-bold text-blue-900">{formatNumber(overview.doorsKnocked)}</p>
            <p className="text-xs text-blue-600 mt-1">Total leads created</p>
          </CardContent>
        </Card>

        <Card>
          <CardContent className="pt-6">
            <div className="flex items-center gap-2 mb-2">
              <Calendar className="h-5 w-5 text-green-600" />
              <span className="text-sm font-semibold text-green-700">Appointments Set</span>
            </div>
            <p className="text-3xl font-bold text-green-900">{formatNumber(overview.appointmentsSet)}</p>
            <p className="text-xs text-green-600 mt-1">{formatPercentage(overview.conversionRate)} conversion rate</p>
          </CardContent>
        </Card>

        <Card>
          <CardContent className="pt-6">
            <div className="flex items-center gap-2 mb-2">
              <CheckCircle2 className="h-5 w-5 text-purple-600" />
              <span className="text-sm font-semibold text-purple-700">Sales Closed</span>
            </div>
            <p className="text-3xl font-bold text-purple-900">{formatNumber(overview.salesClosed)}</p>
            <p className="text-xs text-purple-600 mt-1">{formatPercentage(overview.closeRate)} close rate</p>
          </CardContent>
        </Card>

        <Card>
          <CardContent className="pt-6">
            <div className="flex items-center gap-2 mb-2">
              <Paperclip className="h-5 w-5 text-amber-600" />
              <span className="text-sm font-semibold text-amber-700">Attachments</span>
            </div>
            <p className="text-3xl font-bold text-amber-900">{formatNumber(overview.customersWithAttachments)}</p>
            <p className="text-xs text-amber-600 mt-1">Customers with files</p>
          </CardContent>
        </Card>
      </div>

      {/* Additional Metrics */}
      <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
        <Card>
          <CardContent className="pt-6">
            <div className="flex items-center gap-2 mb-2">
              <MessageSquare className="h-5 w-5 text-indigo-600" />
              <span className="text-sm font-semibold text-indigo-700">Total Notes</span>
            </div>
            <p className="text-3xl font-bold text-indigo-900">{formatNumber(overview.totalNotes)}</p>
            <p className="text-xs text-indigo-600 mt-1">Customer interaction notes</p>
          </CardContent>
        </Card>

        <Card>
          <CardContent className="pt-6">
            <div className="flex items-center gap-2 mb-2">
              <TrendingUp className="h-5 w-5 text-teal-600" />
              <span className="text-sm font-semibold text-teal-700">Status Changes</span>
            </div>
            <p className="text-3xl font-bold text-teal-900">{formatNumber(overview.statusChanges)}</p>
            <p className="text-xs text-teal-600 mt-1">Customer status updates</p>
          </CardContent>
        </Card>

        <Card>
          <CardContent className="pt-6">
            <div className="flex items-center gap-2 mb-2">
              <Users className="h-5 w-5 text-pink-600" />
              <span className="text-sm font-semibold text-pink-700">Active Reps</span>
            </div>
            <p className="text-3xl font-bold text-pink-900">{userPerformance.length}</p>
            <p className="text-xs text-pink-600 mt-1">Reps with activity</p>
          </CardContent>
        </Card>
      </div>

      {/* Main Content Tabs */}
      <Tabs defaultValue="performance" className="space-y-4">
        <TabsList>
          <TabsTrigger value="performance">User Performance</TabsTrigger>
          <TabsTrigger value="offices">Offices</TabsTrigger>
          <TabsTrigger value="customers">Customers ({customers.length})</TabsTrigger>
          <TabsTrigger value="appointments">Appointments ({appointments.length})</TabsTrigger>
          <TabsTrigger value="notes">Notes ({notes.length})</TabsTrigger>
          <TabsTrigger value="attachments">Attachments ({attachments.length})</TabsTrigger>
          <TabsTrigger value="status-logs">Status History ({statusLogs.length})</TabsTrigger>
        </TabsList>

        {/* User Performance */}
        <TabsContent value="performance">
          <Card>
            <CardHeader>
              <CardTitle>User Performance Metrics</CardTitle>
            </CardHeader>
            <CardContent>
              <div className="overflow-x-auto">
                <table className="w-full text-sm">
                  <thead>
                    <tr className="border-b">
                      <th className="text-left p-2">Rep</th>
                      <th className="text-right p-2">Doors</th>
                      <th className="text-right p-2">Appts Set</th>
                      <th className="text-right p-2">Closed</th>
                      <th className="text-right p-2">Sales</th>
                      <th className="text-right p-2">Conv %</th>
                      <th className="text-right p-2">Close %</th>
                      <th className="text-right p-2">Attachments</th>
                      <th className="text-right p-2">Notes</th>
                      <th className="text-right p-2">48h Speed</th>
                    </tr>
                  </thead>
                  <tbody>
                    {userPerformance.map((user) => (
                      <tr key={user.repcardUserId} className="border-b hover:bg-muted/50">
                        <td className="p-2">
                          <div>
                            <p className="font-medium">{user.name}</p>
                            {user.office && (
                              <p className="text-xs text-muted-foreground">{user.office}</p>
                            )}
                          </div>
                        </td>
                        <td className="text-right p-2 font-medium">{user.doorsKnocked}</td>
                        <td className="text-right p-2">{user.appointmentsSet}</td>
                        <td className="text-right p-2">{user.appointmentsClosed}</td>
                        <td className="text-right p-2 font-medium text-green-700">{user.salesClosed}</td>
                        <td className="text-right p-2">{formatPercentage(user.conversionRate)}</td>
                        <td className="text-right p-2">{formatPercentage(user.closeRate)}</td>
                        <td className="text-right p-2">{user.customersWithAttachments}</td>
                        <td className="text-right p-2">{user.notesWritten}</td>
                        <td className="text-right p-2">{user.appointmentsWithin48h}</td>
                      </tr>
                    ))}
                  </tbody>
                </table>
              </div>
            </CardContent>
          </Card>
        </TabsContent>

        {/* Offices */}
        <TabsContent value="offices">
          <Card>
            <CardHeader>
              <CardTitle>Office Breakdown</CardTitle>
            </CardHeader>
            <CardContent>
              <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
                {officeBreakdown.map((office) => (
                  <Card key={office.officeId}>
                    <CardHeader>
                      <CardTitle className="text-lg flex items-center gap-2">
                        <Building2 className="h-4 w-4" />
                        {office.officeName}
                      </CardTitle>
                    </CardHeader>
                    <CardContent>
                      <div className="space-y-2">
                        <div className="flex justify-between">
                          <span className="text-sm text-muted-foreground">Doors Knocked:</span>
                          <span className="font-medium">{office.doorsKnocked}</span>
                        </div>
                        <div className="flex justify-between">
                          <span className="text-sm text-muted-foreground">Appointments:</span>
                          <span className="font-medium">{office.appointmentsSet}</span>
                        </div>
                        <div className="flex justify-between">
                          <span className="text-sm text-muted-foreground">Sales Closed:</span>
                          <span className="font-medium text-green-700">{office.salesClosed}</span>
                        </div>
                        <div className="flex justify-between">
                          <span className="text-sm text-muted-foreground">Active Reps:</span>
                          <span className="font-medium">{office.activeReps}</span>
                        </div>
                      </div>
                    </CardContent>
                  </Card>
                ))}
              </div>
            </CardContent>
          </Card>
        </TabsContent>

        {/* Customers */}
        <TabsContent value="customers">
          <Card>
            <CardHeader>
              <CardTitle>Customers / Leads</CardTitle>
            </CardHeader>
            <CardContent>
              <div className="space-y-4">
                {customers.map((customer) => (
                  <Card key={customer.repcardCustomerId}>
                    <CardContent className="pt-6">
                      <div className="flex justify-between items-start">
                        <div className="flex-1">
                          <h3 className="font-semibold text-lg">{customer.name}</h3>
                          <div className="mt-2 space-y-1 text-sm text-muted-foreground">
                            {customer.email && <p>Email: {customer.email}</p>}
                            {customer.phone && <p>Phone: {customer.phone}</p>}
                            {customer.address && (
                              <p>Address: {customer.address}, {customer.city} {customer.state} {customer.zip}</p>
                            )}
                            <p>Created: {new Date(customer.createdAt).toLocaleDateString()}</p>
                          </div>
                          <div className="mt-3 flex gap-2 flex-wrap">
                            <Badge variant="outline">Setter: {customer.setter.name}</Badge>
                            {customer.office.officeName && (
                              <Badge variant="outline">Office: {customer.office.officeName}</Badge>
                            )}
                            {customer.status && (
                              <Badge>{customer.status}</Badge>
                            )}
                          </div>
                          {Object.values(customer.customFields).some(v => v) && (
                            <div className="mt-3 p-3 bg-muted rounded-lg">
                              <p className="text-xs font-semibold mb-2">Custom Fields:</p>
                              <div className="grid grid-cols-2 gap-2 text-xs">
                                {customer.customFields.systemSizeKW && (
                                  <p>System Size: {customer.customFields.systemSizeKW} kW</p>
                                )}
                                {customer.customFields.systemCost && (
                                  <p>Cost: ${Number(customer.customFields.systemCost).toLocaleString()}</p>
                                )}
                                {customer.customFields.financier && (
                                  <p>Financier: {customer.customFields.financier}</p>
                                )}
                                {customer.customFields.offset && (
                                  <p>Offset: {customer.customFields.offset}%</p>
                                )}
                              </div>
                            </div>
                          )}
                        </div>
                        <div className="text-right space-y-1">
                          <Badge variant="secondary">{customer.counts.appointments} appts</Badge>
                          <Badge variant="secondary">{customer.counts.attachments} files</Badge>
                          <Badge variant="secondary">{customer.counts.notes} notes</Badge>
                        </div>
                      </div>
                    </CardContent>
                  </Card>
                ))}
              </div>
            </CardContent>
          </Card>
        </TabsContent>

        {/* Appointments */}
        <TabsContent value="appointments">
          <Card>
            <CardHeader>
              <CardTitle>Appointments</CardTitle>
            </CardHeader>
            <CardContent>
              <div className="space-y-4">
                {appointments.map((appt) => (
                  <Card key={appt.repcardAppointmentId}>
                    <CardContent className="pt-6">
                      <div className="flex justify-between items-start">
                        <div className="flex-1">
                          <div className="flex items-center gap-2 mb-2">
                            <h3 className="font-semibold text-lg">{appt.customer.name}</h3>
                            {appt.disposition && (
                              <Badge className={getDispositionColor(appt.disposition)}>
                                {appt.disposition}
                              </Badge>
                            )}
                          </div>
                          <div className="mt-2 space-y-1 text-sm text-muted-foreground">
                            {appt.scheduledAt && (
                              <p className="flex items-center gap-1">
                                <Clock className="h-3 w-3" />
                                Scheduled: {new Date(appt.scheduledAt).toLocaleString()}
                              </p>
                            )}
                            {appt.completedAt && (
                              <p className="flex items-center gap-1">
                                <CheckCircle2 className="h-3 w-3" />
                                Completed: {new Date(appt.completedAt).toLocaleString()}
                              </p>
                            )}
                            {appt.duration && <p>Duration: {appt.duration} minutes</p>}
                          </div>
                          <div className="mt-3 flex gap-2 flex-wrap">
                            <Badge variant="outline">Setter: {appt.setter.name}</Badge>
                            {appt.closer.name && (
                              <Badge variant="outline">Closer: {appt.closer.name}</Badge>
                            )}
                            {appt.office.officeName && (
                              <Badge variant="outline">Office: {appt.office.officeName}</Badge>
                            )}
                            {appt.isWithin48Hours && (
                              <Badge className="bg-green-100 text-green-800">Within 48h</Badge>
                            )}
                            {appt.hasPowerBill && (
                              <Badge className="bg-blue-100 text-blue-800">Power Bill</Badge>
                            )}
                          </div>
                          {appt.notes && (
                            <div className="mt-3 p-3 bg-muted rounded-lg">
                              <p className="text-xs font-semibold mb-1">Notes:</p>
                              <p className="text-sm">{appt.notes}</p>
                            </div>
                          )}
                        </div>
                        <div className="text-right">
                          {appt.attachmentCount > 0 && (
                            <Badge variant="secondary">{appt.attachmentCount} attachments</Badge>
                          )}
                        </div>
                      </div>
                    </CardContent>
                  </Card>
                ))}
              </div>
            </CardContent>
          </Card>
        </TabsContent>

        {/* Notes */}
        <TabsContent value="notes">
          <Card>
            <CardHeader>
              <CardTitle>Customer Notes</CardTitle>
            </CardHeader>
            <CardContent>
              <div className="space-y-4">
                {notes.map((note) => (
                  <Card key={note.repcardNoteId}>
                    <CardContent className="pt-6">
                      <div className="flex justify-between items-start mb-2">
                        <div>
                          <p className="font-semibold">{note.customerName}</p>
                          <p className="text-xs text-muted-foreground">
                            By {note.author.name} on {new Date(note.createdAt).toLocaleString()}
                          </p>
                        </div>
                      </div>
                      <p className="text-sm mt-2 whitespace-pre-wrap">{note.note}</p>
                    </CardContent>
                  </Card>
                ))}
              </div>
            </CardContent>
          </Card>
        </TabsContent>

        {/* Attachments */}
        <TabsContent value="attachments">
          <Card>
            <CardHeader>
              <CardTitle>Attachments</CardTitle>
            </CardHeader>
            <CardContent>
              <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
                {attachments.map((att) => (
                  <Card key={att.attachmentId}>
                    <CardContent className="pt-6">
                      <div className="flex items-start gap-2">
                        <Paperclip className="h-4 w-4 text-muted-foreground mt-1" />
                        <div className="flex-1 min-w-0">
                          <p className="font-medium text-sm truncate">{att.fileName}</p>
                          <p className="text-xs text-muted-foreground mt-1">
                            {att.customerName}
                          </p>
                          <p className="text-xs text-muted-foreground">
                            By {att.uploadedBy.name} • {new Date(att.createdAt).toLocaleDateString()}
                          </p>
                          {att.fileUrl && (
                            <a 
                              href={att.fileUrl} 
                              target="_blank" 
                              rel="noopener noreferrer"
                              className="text-xs text-blue-600 hover:underline mt-1 inline-block"
                            >
                              View File →
                            </a>
                          )}
                        </div>
                        <Badge variant="outline" className="text-xs">
                          {att.source}
                        </Badge>
                      </div>
                    </CardContent>
                  </Card>
                ))}
              </div>
            </CardContent>
          </Card>
        </TabsContent>

        {/* Status Logs */}
        <TabsContent value="status-logs">
          <Card>
            <CardHeader>
              <CardTitle>Status Change History</CardTitle>
            </CardHeader>
            <CardContent>
              <div className="space-y-3">
                {statusLogs.map((log) => (
                  <div key={log.repcardLogId} className="flex items-start gap-3 p-3 border rounded-lg">
                    <div className="flex-1">
                      <p className="font-medium">{log.customerName}</p>
                      <div className="flex items-center gap-2 mt-1">
                        <Badge variant="outline">{log.oldStatus || 'N/A'}</Badge>
                        <span>→</span>
                        <Badge>{log.newStatus}</Badge>
                      </div>
                      {log.notes && (
                        <p className="text-xs text-muted-foreground mt-1">{log.notes}</p>
                      )}
                    </div>
                    <div className="text-right text-xs text-muted-foreground">
                      <p>{log.changedBy.name}</p>
                      <p>{new Date(log.changedAt).toLocaleString()}</p>
                    </div>
                  </div>
                ))}
              </div>
            </CardContent>
          </Card>
        </TabsContent>
      </Tabs>
    </div>
  );
}

