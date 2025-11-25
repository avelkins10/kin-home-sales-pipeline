'use client';

import { useState, useEffect } from 'react';
import { Card, CardHeader, CardTitle, CardContent } from '@/components/ui/card';
import { Badge } from '@/components/ui/badge';
import { Button } from '@/components/ui/button';
import { Alert, AlertDescription } from '@/components/ui/alert';
import { AlertCircle, RefreshCw, CheckCircle, XCircle } from 'lucide-react';

interface OverviewStats {
  total_closed_appointments: number;
  with_closer_assigned: number;
  without_closer: number;
  status_log_closes: number;
  total_appointments: number;
  total_customers: number;
  linked_users: number;
}

interface CloserComparison {
  repcard_user_id: number;
  name: string;
  role: string;
  appointments_method: number;
  status_logs_method: number;
  difference: number;
}

interface SampleAppointment {
  repcard_appointment_id: number;
  closer_user_id: number | null;
  closer_name: string | null;
  setter_user_id: number | null;
  setter_name: string | null;
  disposition: string;
  status_category: string | null;
  scheduled_at: string;
  customer_name: string | null;
  status_log_status: string | null;
  status_updater_name: string | null;
  attribution_status: 'MATCH' | 'MISMATCH';
}

export default function RepCardDataDebugPage() {
  const [overview, setOverview] = useState<OverviewStats | null>(null);
  const [closers, setClosers] = useState<CloserComparison[]>([]);
  const [appointments, setAppointments] = useState<SampleAppointment[]>([]);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);

  const fetchData = async () => {
    setLoading(true);
    setError(null);
    try {
      // Fetch overview
      const overviewRes = await fetch('/api/repcard/data-debug?view=overview');
      if (!overviewRes.ok) throw new Error('Failed to fetch overview');
      const overviewData = await overviewRes.json();
      setOverview(overviewData.stats);

      // Fetch closer comparison
      const closersRes = await fetch('/api/repcard/data-debug?view=closer-comparison');
      if (!closersRes.ok) throw new Error('Failed to fetch closer comparison');
      const closersData = await closersRes.json();
      setClosers(closersData.closers);

      // Fetch sample appointments
      const appointmentsRes = await fetch('/api/repcard/data-debug?view=sample-appointments&limit=20');
      if (!appointmentsRes.ok) throw new Error('Failed to fetch sample appointments');
      const appointmentsData = await appointmentsRes.json();
      setAppointments(appointmentsData.appointments);
    } catch (err) {
      setError(err instanceof Error ? err.message : 'Failed to fetch data');
    } finally {
      setLoading(false);
    }
  };

  useEffect(() => {
    fetchData();
  }, []);

  const getAttributionBadge = (status: 'MATCH' | 'MISMATCH') => {
    if (status === 'MATCH') {
      return <Badge variant="outline" className="bg-green-50 text-green-700 border-green-200"><CheckCircle className="w-3 h-3 mr-1" />Match</Badge>;
    }
    return <Badge variant="destructive" className="bg-red-50 text-red-700 border-red-200"><XCircle className="w-3 h-3 mr-1" />Mismatch</Badge>;
  };

  return (
    <div className="container mx-auto p-6 max-w-7xl">
      <div className="flex items-center justify-between mb-6">
        <div>
          <h1 className="text-3xl font-bold">RepCard Data Debug</h1>
          <p className="text-gray-600 mt-1">Inspect closer attribution and raw data</p>
        </div>
        <Button onClick={fetchData} disabled={loading}>
          <RefreshCw className={`w-4 h-4 mr-2 ${loading ? 'animate-spin' : ''}`} />
          Refresh
        </Button>
      </div>

      {error && (
        <Alert variant="destructive" className="mb-6">
          <AlertCircle className="h-4 w-4" />
          <AlertDescription>{error}</AlertDescription>
        </Alert>
      )}

      {/* Overview Stats */}
      <Card className="mb-6">
        <CardHeader>
          <CardTitle>Overview Statistics</CardTitle>
        </CardHeader>
        <CardContent>
          {overview ? (
            <div className="grid grid-cols-2 md:grid-cols-4 gap-4">
              <div className="bg-blue-50 p-4 rounded-lg">
                <div className="text-sm text-gray-600">Total Closed Appointments</div>
                <div className="text-2xl font-bold text-blue-600">{overview.total_closed_appointments}</div>
              </div>
              <div className="bg-green-50 p-4 rounded-lg">
                <div className="text-sm text-gray-600">With Closer Assigned</div>
                <div className="text-2xl font-bold text-green-600">{overview.with_closer_assigned}</div>
              </div>
              <div className="bg-yellow-50 p-4 rounded-lg">
                <div className="text-sm text-gray-600">Without Closer</div>
                <div className="text-2xl font-bold text-yellow-600">{overview.without_closer}</div>
              </div>
              <div className="bg-purple-50 p-4 rounded-lg">
                <div className="text-sm text-gray-600">Status Log Closes</div>
                <div className="text-2xl font-bold text-purple-600">{overview.status_log_closes}</div>
              </div>
              <div className="bg-gray-50 p-4 rounded-lg">
                <div className="text-sm text-gray-600">Total Appointments</div>
                <div className="text-2xl font-bold">{overview.total_appointments}</div>
              </div>
              <div className="bg-gray-50 p-4 rounded-lg">
                <div className="text-sm text-gray-600">Total Customers</div>
                <div className="text-2xl font-bold">{overview.total_customers}</div>
              </div>
              <div className="bg-gray-50 p-4 rounded-lg">
                <div className="text-sm text-gray-600">Linked Users</div>
                <div className="text-2xl font-bold">{overview.linked_users}</div>
              </div>
              <div className={`p-4 rounded-lg ${
                overview.with_closer_assigned === overview.status_log_closes
                  ? 'bg-green-50'
                  : 'bg-red-50'
              }`}>
                <div className="text-sm text-gray-600">Attribution Accuracy</div>
                <div className={`text-2xl font-bold ${
                  overview.with_closer_assigned === overview.status_log_closes
                    ? 'text-green-600'
                    : 'text-red-600'
                }`}>
                  {overview.total_closed_appointments > 0
                    ? Math.round((overview.status_log_closes / overview.with_closer_assigned) * 100)
                    : 0}%
                </div>
              </div>
            </div>
          ) : (
            <div className="text-gray-500">Loading...</div>
          )}
        </CardContent>
      </Card>

      {/* Closer Attribution Comparison */}
      <Card className="mb-6">
        <CardHeader>
          <CardTitle>Closer Attribution Comparison</CardTitle>
          <p className="text-sm text-gray-600 mt-1">
            Compare sales counts from appointments table (closer_user_id) vs status logs (changed_by_user_id)
          </p>
        </CardHeader>
        <CardContent>
          <div className="overflow-x-auto">
            <table className="w-full text-sm">
              <thead className="bg-gray-50 border-b">
                <tr>
                  <th className="text-left p-2">Closer Name</th>
                  <th className="text-left p-2">Role</th>
                  <th className="text-right p-2">Appointments Method</th>
                  <th className="text-right p-2">Status Logs Method</th>
                  <th className="text-right p-2">Difference</th>
                  <th className="text-center p-2">Status</th>
                </tr>
              </thead>
              <tbody>
                {closers.map((closer) => (
                  <tr key={closer.repcard_user_id} className="border-b hover:bg-gray-50">
                    <td className="p-2 font-medium">{closer.name}</td>
                    <td className="p-2 text-gray-600">{closer.role || 'N/A'}</td>
                    <td className="p-2 text-right font-semibold text-blue-600">
                      {closer.appointments_method}
                    </td>
                    <td className="p-2 text-right text-gray-600">
                      {closer.status_logs_method}
                    </td>
                    <td className={`p-2 text-right font-semibold ${
                      Math.abs(closer.difference) > 5 ? 'text-red-600' : 'text-gray-600'
                    }`}>
                      {closer.difference > 0 ? `+${closer.difference}` : closer.difference}
                    </td>
                    <td className="p-2 text-center">
                      {Math.abs(closer.difference) > 5 ? (
                        <Badge variant="destructive">Mismatch</Badge>
                      ) : (
                        <Badge variant="outline" className="bg-green-50 text-green-700">OK</Badge>
                      )}
                    </td>
                  </tr>
                ))}
              </tbody>
            </table>
          </div>
          {closers.length === 0 && (
            <div className="text-center text-gray-500 py-4">No data available</div>
          )}
        </CardContent>
      </Card>

      {/* Sample Appointments */}
      <Card>
        <CardHeader>
          <CardTitle>Sample Closed Appointments (Latest 20)</CardTitle>
          <p className="text-sm text-gray-600 mt-1">
            Showing attribution details for recent closed appointments
          </p>
        </CardHeader>
        <CardContent>
          <div className="overflow-x-auto">
            <table className="w-full text-sm">
              <thead className="bg-gray-50 border-b">
                <tr>
                  <th className="text-left p-2">Appt ID</th>
                  <th className="text-left p-2">Customer</th>
                  <th className="text-left p-2">Closer</th>
                  <th className="text-left p-2">Setter</th>
                  <th className="text-left p-2">Disposition</th>
                  <th className="text-left p-2">Status Updater</th>
                  <th className="text-center p-2">Attribution</th>
                  <th className="text-left p-2">Scheduled</th>
                </tr>
              </thead>
              <tbody>
                {appointments.map((appt) => (
                  <tr key={appt.repcard_appointment_id} className="border-b hover:bg-gray-50">
                    <td className="p-2 font-mono text-xs">{appt.repcard_appointment_id}</td>
                    <td className="p-2">{appt.customer_name || <span className="text-gray-400">N/A</span>}</td>
                    <td className="p-2 font-medium">{appt.closer_name || <span className="text-gray-400">Unassigned</span>}</td>
                    <td className="p-2 text-gray-600">{appt.setter_name || 'N/A'}</td>
                    <td className="p-2">
                      <Badge variant="outline" className="bg-green-50 text-green-700">{appt.disposition}</Badge>
                    </td>
                    <td className="p-2 text-gray-600">{appt.status_updater_name || <span className="text-gray-400">None</span>}</td>
                    <td className="p-2 text-center">
                      {getAttributionBadge(appt.attribution_status)}
                    </td>
                    <td className="p-2 text-xs text-gray-600">
                      {new Date(appt.scheduled_at).toLocaleDateString()}
                    </td>
                  </tr>
                ))}
              </tbody>
            </table>
          </div>
          {appointments.length === 0 && (
            <div className="text-center text-gray-500 py-4">No appointments found</div>
          )}
        </CardContent>
      </Card>

      {/* Recommendation Alert */}
      <Alert className="mt-6 bg-blue-50 border-blue-200">
        <AlertCircle className="h-4 w-4 text-blue-600" />
        <AlertDescription className="text-blue-800">
          <strong>Recommendation:</strong> Use the appointments table method (closer_user_id) for accurate closer attribution.
          This credits the person who actually ran the appointment, not who updated the CRM status.
        </AlertDescription>
      </Alert>
    </div>
  );
}
