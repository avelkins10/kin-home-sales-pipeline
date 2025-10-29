'use client';

import { useState, useEffect } from 'react';
import { Card, CardContent, CardHeader, CardTitle, CardDescription } from '@/components/ui/card';
import { Loader2, AlertCircle, CheckCircle, XCircle, RefreshCw } from 'lucide-react';

interface DebugData {
  sync: {
    recentSyncs: any[];
    lastCustomerSync: any;
    lastAppointmentSync: any;
  };
  database: {
    recentCustomers: any[];
    recentAppointments: any[];
    schema: {
      customersColumns: any[];
      appointmentsColumns: any[];
    };
  };
  counts: {
    today: {
      customers: number;
      appointments: number;
    };
    october2025: {
      customers: number;
      appointments: number;
    };
    total: {
      customers: number;
      appointments: number;
    };
    orphaned: {
      customers: number;
      appointments: number;
    };
  };
  dateRanges: {
    customers: {
      min: string;
      max: string;
    };
    appointments: {
      min: string;
      max: string;
    };
  };
  api: {
    recentCustomers: any[];
    recentAppointments: any[];
    error: string | null;
  };
  userMappings: {
    total: number;
    users: any[];
  };
}

export default function RepCardDebugPage() {
  const [data, setData] = useState<DebugData | null>(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  const fetchData = async () => {
    try {
      setLoading(true);
      setError(null);
      const response = await fetch('/api/operations/settings/repcard-debug');

      if (!response.ok) {
        const errorData = await response.json();
        throw new Error(errorData.error || 'Failed to fetch debug data');
      }

      const result = await response.json();
      setData(result);
    } catch (err) {
      setError(err instanceof Error ? err.message : 'Unknown error');
    } finally {
      setLoading(false);
    }
  };

  useEffect(() => {
    fetchData();
  }, []);

  if (loading) {
    return (
      <div className="flex items-center justify-center min-h-screen">
        <Loader2 className="h-8 w-8 animate-spin text-blue-500" />
      </div>
    );
  }

  if (error) {
    return (
      <div className="container mx-auto p-6">
        <Card className="border-red-200 bg-red-50">
          <CardHeader>
            <CardTitle className="text-red-700 flex items-center gap-2">
              <AlertCircle className="h-5 w-5" />
              Error Loading Debug Data
            </CardTitle>
          </CardHeader>
          <CardContent>
            <p className="text-red-600">{error}</p>
          </CardContent>
        </Card>
      </div>
    );
  }

  if (!data) return null;

  return (
    <div className="container mx-auto p-6 space-y-6">
      <div className="flex items-center justify-between">
        <div>
          <h1 className="text-3xl font-bold">RepCard Diagnostic Tool</h1>
          <p className="text-gray-500 mt-1">Debug RepCard sync and data mapping issues</p>
        </div>
        <button
          onClick={fetchData}
          className="flex items-center gap-2 px-4 py-2 bg-blue-500 text-white rounded-md hover:bg-blue-600"
        >
          <RefreshCw className="h-4 w-4" />
          Refresh
        </button>
      </div>

      {/* Data Counts Overview */}
      <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
        <Card>
          <CardHeader>
            <CardTitle>Today's Data</CardTitle>
            <CardDescription>{new Date().toLocaleDateString()}</CardDescription>
          </CardHeader>
          <CardContent>
            <div className="space-y-2">
              <div className="flex justify-between">
                <span>Customers:</span>
                <span className="font-bold">{data.counts.today.customers}</span>
              </div>
              <div className="flex justify-between">
                <span>Appointments:</span>
                <span className="font-bold">{data.counts.today.appointments}</span>
              </div>
            </div>
          </CardContent>
        </Card>

        <Card>
          <CardHeader>
            <CardTitle>October 2025</CardTitle>
            <CardDescription>Oct 1-28, 2025 (Month to Date)</CardDescription>
          </CardHeader>
          <CardContent>
            <div className="space-y-2">
              <div className="flex justify-between">
                <span>Customers:</span>
                <span className="font-bold">{data.counts.october2025.customers}</span>
              </div>
              <div className="flex justify-between">
                <span>Appointments:</span>
                <span className="font-bold">{data.counts.october2025.appointments}</span>
              </div>
            </div>
          </CardContent>
        </Card>

        <Card>
          <CardHeader>
            <CardTitle>Total in Database</CardTitle>
            <CardDescription>All time</CardDescription>
          </CardHeader>
          <CardContent>
            <div className="space-y-2">
              <div className="flex justify-between">
                <span>Customers:</span>
                <span className="font-bold">{data.counts.total.customers}</span>
              </div>
              <div className="flex justify-between">
                <span>Appointments:</span>
                <span className="font-bold">{data.counts.total.appointments}</span>
              </div>
            </div>
          </CardContent>
        </Card>
      </div>

      {/* Orphaned Records Warning */}
      {(data.counts.orphaned.customers > 0 || data.counts.orphaned.appointments > 0) && (
        <Card className="border-yellow-200 bg-yellow-50">
          <CardHeader>
            <CardTitle className="text-yellow-700 flex items-center gap-2">
              <AlertCircle className="h-5 w-5" />
              Orphaned Records Detected
            </CardTitle>
          </CardHeader>
          <CardContent>
            <p className="text-yellow-600">
              Found {data.counts.orphaned.customers} customers and {data.counts.orphaned.appointments} appointments
              with setter_user_id that don't match any user's repcard_user_id. These records won't show in leaderboards.
            </p>
          </CardContent>
        </Card>
      )}

      {/* Sync Status */}
      <Card>
        <CardHeader>
          <CardTitle>Recent Sync Operations</CardTitle>
        </CardHeader>
        <CardContent>
          <div className="space-y-4">
            <div className="grid grid-cols-2 gap-4">
              <div className="p-4 bg-gray-50 rounded-md">
                <h3 className="font-semibold mb-2">Last Customer Sync</h3>
                {data.sync.lastCustomerSync ? (
                  <>
                    <div className="flex items-center gap-2 mb-1">
                      {data.sync.lastCustomerSync.status === 'completed' ? (
                        <CheckCircle className="h-4 w-4 text-green-500" />
                      ) : (
                        <XCircle className="h-4 w-4 text-red-500" />
                      )}
                      <span className="text-sm font-medium">{data.sync.lastCustomerSync.status}</span>
                    </div>
                    <p className="text-xs text-gray-600">
                      Started: {new Date(data.sync.lastCustomerSync.started_at).toLocaleString()}
                    </p>
                    <p className="text-xs text-gray-600">
                      Fetched: {data.sync.lastCustomerSync.records_fetched} |
                      Inserted: {data.sync.lastCustomerSync.records_inserted} |
                      Updated: {data.sync.lastCustomerSync.records_updated}
                    </p>
                    {data.sync.lastCustomerSync.error_message && (
                      <p className="text-xs text-red-600 mt-1">{data.sync.lastCustomerSync.error_message}</p>
                    )}
                  </>
                ) : (
                  <p className="text-sm text-gray-500">No sync history found</p>
                )}
              </div>

              <div className="p-4 bg-gray-50 rounded-md">
                <h3 className="font-semibold mb-2">Last Appointment Sync</h3>
                {data.sync.lastAppointmentSync ? (
                  <>
                    <div className="flex items-center gap-2 mb-1">
                      {data.sync.lastAppointmentSync.status === 'completed' ? (
                        <CheckCircle className="h-4 w-4 text-green-500" />
                      ) : (
                        <XCircle className="h-4 w-4 text-red-500" />
                      )}
                      <span className="text-sm font-medium">{data.sync.lastAppointmentSync.status}</span>
                    </div>
                    <p className="text-xs text-gray-600">
                      Started: {new Date(data.sync.lastAppointmentSync.started_at).toLocaleString()}
                    </p>
                    <p className="text-xs text-gray-600">
                      Fetched: {data.sync.lastAppointmentSync.records_fetched} |
                      Inserted: {data.sync.lastAppointmentSync.records_inserted} |
                      Updated: {data.sync.lastAppointmentSync.records_updated}
                    </p>
                    {data.sync.lastAppointmentSync.error_message && (
                      <p className="text-xs text-red-600 mt-1">{data.sync.lastAppointmentSync.error_message}</p>
                    )}
                  </>
                ) : (
                  <p className="text-sm text-gray-500">No sync history found</p>
                )}
              </div>
            </div>
          </div>
        </CardContent>
      </Card>

      {/* Date Ranges */}
      <Card>
        <CardHeader>
          <CardTitle>Data Date Ranges</CardTitle>
        </CardHeader>
        <CardContent>
          <div className="grid grid-cols-2 gap-4">
            <div>
              <h3 className="font-semibold mb-2">Customers</h3>
              <p className="text-sm text-gray-600">
                First: {data.dateRanges.customers.min ? new Date(data.dateRanges.customers.min).toLocaleString() : 'N/A'}
              </p>
              <p className="text-sm text-gray-600">
                Latest: {data.dateRanges.customers.max ? new Date(data.dateRanges.customers.max).toLocaleString() : 'N/A'}
              </p>
            </div>
            <div>
              <h3 className="font-semibold mb-2">Appointments</h3>
              <p className="text-sm text-gray-600">
                First: {data.dateRanges.appointments.min ? new Date(data.dateRanges.appointments.min).toLocaleString() : 'N/A'}
              </p>
              <p className="text-sm text-gray-600">
                Latest: {data.dateRanges.appointments.max ? new Date(data.dateRanges.appointments.max).toLocaleString() : 'N/A'}
              </p>
            </div>
          </div>
        </CardContent>
      </Card>

      {/* Recent Customers from Database */}
      <Card>
        <CardHeader>
          <CardTitle>Recent Customers (Database)</CardTitle>
          <CardDescription>Last 20 customers ordered by created_at</CardDescription>
        </CardHeader>
        <CardContent>
          <div className="overflow-x-auto">
            <table className="w-full text-sm">
              <thead className="bg-gray-50">
                <tr>
                  <th className="p-2 text-left">RepCard ID</th>
                  <th className="p-2 text-left">Name</th>
                  <th className="p-2 text-left">Setter User ID</th>
                  <th className="p-2 text-left">Matched User</th>
                  <th className="p-2 text-left">Created At</th>
                </tr>
              </thead>
              <tbody>
                {data.database.recentCustomers.slice(0, 10).map((customer) => (
                  <tr key={customer.id} className="border-t">
                    <td className="p-2">{customer.repcard_customer_id}</td>
                    <td className="p-2">{customer.name || 'N/A'}</td>
                    <td className="p-2">{customer.setter_user_id || 'N/A'}</td>
                    <td className="p-2">
                      {customer.matchedUser ? (
                        <span className="text-green-600">{customer.matchedUser.name}</span>
                      ) : (
                        <span className="text-red-600">ORPHANED</span>
                      )}
                    </td>
                    <td className="p-2">{new Date(customer.created_at).toLocaleString()}</td>
                  </tr>
                ))}
              </tbody>
            </table>
          </div>
        </CardContent>
      </Card>

      {/* Recent Appointments from Database */}
      <Card>
        <CardHeader>
          <CardTitle>Recent Appointments (Database)</CardTitle>
          <CardDescription>Last 20 appointments ordered by created_at</CardDescription>
        </CardHeader>
        <CardContent>
          <div className="overflow-x-auto">
            <table className="w-full text-sm">
              <thead className="bg-gray-50">
                <tr>
                  <th className="p-2 text-left">RepCard ID</th>
                  <th className="p-2 text-left">Setter User ID</th>
                  <th className="p-2 text-left">Matched User</th>
                  <th className="p-2 text-left">Disposition</th>
                  <th className="p-2 text-left">Created At</th>
                  <th className="p-2 text-left">Scheduled At</th>
                </tr>
              </thead>
              <tbody>
                {data.database.recentAppointments.slice(0, 10).map((appointment) => (
                  <tr key={appointment.id} className="border-t">
                    <td className="p-2">{appointment.repcard_appointment_id}</td>
                    <td className="p-2">{appointment.setter_user_id || 'N/A'}</td>
                    <td className="p-2">
                      {appointment.matchedUser ? (
                        <span className="text-green-600">{appointment.matchedUser.name}</span>
                      ) : (
                        <span className="text-red-600">ORPHANED</span>
                      )}
                    </td>
                    <td className="p-2">{appointment.disposition || 'N/A'}</td>
                    <td className="p-2">{new Date(appointment.created_at).toLocaleString()}</td>
                    <td className="p-2">{appointment.scheduled_at ? new Date(appointment.scheduled_at).toLocaleString() : 'N/A'}</td>
                  </tr>
                ))}
              </tbody>
            </table>
          </div>
        </CardContent>
      </Card>

      {/* Database Schema */}
      <Card>
        <CardHeader>
          <CardTitle>Database Schema</CardTitle>
        </CardHeader>
        <CardContent>
          <div className="grid grid-cols-2 gap-4">
            <div>
              <h3 className="font-semibold mb-2">repcard_customers columns</h3>
              <ul className="text-sm space-y-1">
                {data.database.schema.customersColumns.map((col: any) => (
                  <li key={col.column_name} className="font-mono text-xs">
                    {col.column_name}: <span className="text-gray-500">{col.data_type}</span>
                  </li>
                ))}
              </ul>
            </div>
            <div>
              <h3 className="font-semibold mb-2">repcard_appointments columns</h3>
              <ul className="text-sm space-y-1">
                {data.database.schema.appointmentsColumns.map((col: any) => (
                  <li key={col.column_name} className="font-mono text-xs">
                    {col.column_name}: <span className="text-gray-500">{col.data_type}</span>
                  </li>
                ))}
              </ul>
            </div>
          </div>
        </CardContent>
      </Card>

      {/* RepCard API Data */}
      <Card>
        <CardHeader>
          <CardTitle>RepCard API Recent Data</CardTitle>
          <CardDescription>Most recent records from RepCard API</CardDescription>
        </CardHeader>
        <CardContent>
          {data.api.error ? (
            <div className="p-4 bg-red-50 text-red-600 rounded-md">
              <p className="font-semibold">API Error:</p>
              <p className="text-sm">{data.api.error}</p>
            </div>
          ) : (
            <div className="space-y-4">
              <div>
                <h3 className="font-semibold mb-2">Recent Customers from API ({data.api.recentCustomers?.length || 0})</h3>
                {data.api.recentCustomers && data.api.recentCustomers.length > 0 ? (
                  <pre className="text-xs bg-gray-50 p-4 rounded-md overflow-x-auto">
                    {JSON.stringify(data.api.recentCustomers[0], null, 2)}
                  </pre>
                ) : (
                  <p className="text-sm text-gray-500">No customers from API</p>
                )}
              </div>
              <div>
                <h3 className="font-semibold mb-2">Recent Appointments from API ({data.api.recentAppointments?.length || 0})</h3>
                {data.api.recentAppointments && data.api.recentAppointments.length > 0 ? (
                  <pre className="text-xs bg-gray-50 p-4 rounded-md overflow-x-auto">
                    {JSON.stringify(data.api.recentAppointments[0], null, 2)}
                  </pre>
                ) : (
                  <p className="text-sm text-gray-500">No appointments from API</p>
                )}
              </div>
            </div>
          )}
        </CardContent>
      </Card>

      {/* User Mappings */}
      <Card>
        <CardHeader>
          <CardTitle>User ID Mappings</CardTitle>
          <CardDescription>{data.userMappings.total} users with repcard_user_id</CardDescription>
        </CardHeader>
        <CardContent>
          <div className="overflow-x-auto">
            <table className="w-full text-sm">
              <thead className="bg-gray-50">
                <tr>
                  <th className="p-2 text-left">Name</th>
                  <th className="p-2 text-left">Email</th>
                  <th className="p-2 text-left">RepCard User ID</th>
                </tr>
              </thead>
              <tbody>
                {data.userMappings.users.slice(0, 10).map((user: any) => (
                  <tr key={user.id} className="border-t">
                    <td className="p-2">{user.name}</td>
                    <td className="p-2">{user.email}</td>
                    <td className="p-2 font-mono">{user.repcard_user_id}</td>
                  </tr>
                ))}
              </tbody>
            </table>
          </div>
        </CardContent>
      </Card>
    </div>
  );
}
