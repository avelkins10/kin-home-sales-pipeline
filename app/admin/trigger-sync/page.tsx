'use client';

import { useState } from 'react';
import { useSession } from 'next-auth/react';
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card';
import { Button } from '@/components/ui/button';
import { Alert, AlertDescription } from '@/components/ui/alert';
import { Loader2, CheckCircle, XCircle, AlertCircle } from 'lucide-react';

export default function TriggerSyncPage() {
  const { data: session, status } = useSession();
  const [isSyncing, setIsSyncing] = useState(false);
  const [result, setResult] = useState<any>(null);
  const [error, setError] = useState<string | null>(null);

  const [isLinking, setIsLinking] = useState(false);

  const triggerSync = async () => {
    setIsSyncing(true);
    setError(null);
    setResult(null);

    try {
      const response = await fetch(
        '/api/admin/repcard/comprehensive-sync?skipCustomerAttachments=true&skipAppointmentAttachments=true',
        {
          method: 'POST',
          headers: {
            'Content-Type': 'application/json'
          }
        }
      );

      const data = await response.json();

      if (response.ok && data.success) {
        setResult(data);
      } else {
        setError(data.error || data.message || 'Sync failed');
      }
    } catch (err: any) {
      setError(err.message || 'Failed to trigger sync');
    } finally {
      setIsSyncing(false);
    }
  };

  const linkUsers = async () => {
    setIsLinking(true);
    setError(null);
    setResult(null);

    try {
      const response = await fetch('/api/admin/link-repcard-users', {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json'
        }
      });

      const data = await response.json();

      if (response.ok && data.success) {
        setResult({ message: data.message, type: 'link' });
      } else {
        setError(data.error || 'Linking failed');
      }
    } catch (err: any) {
      setError(err.message || 'Failed to link users');
    } finally {
      setIsLinking(false);
    }
  };

  if (status === 'loading') {
    return <div className="p-8">Loading...</div>;
  }

  if (!session) {
    return (
      <div className="p-8">
        <Alert>
          <AlertCircle className="h-4 w-4" />
          <AlertDescription>Please log in to trigger sync.</AlertDescription>
        </Alert>
      </div>
    );
  }

  if (session.user.role !== 'super_admin') {
    return (
      <div className="p-8">
        <Alert variant="destructive">
          <AlertCircle className="h-4 w-4" />
          <AlertDescription>Only super_admin can trigger sync.</AlertDescription>
        </Alert>
      </div>
    );
  }

  return (
    <div className="container mx-auto p-8 max-w-4xl">
      <Card>
        <CardHeader>
          <CardTitle>Trigger RepCard Sync</CardTitle>
          <CardDescription>
            Pull all RepCard data (users, offices, customers, appointments, status logs)
          </CardDescription>
        </CardHeader>
        <CardContent className="space-y-4">
          <div className="grid grid-cols-2 gap-4">
            <Button 
              onClick={triggerSync} 
              disabled={isSyncing || isLinking}
              className="w-full"
              size="lg"
            >
              {isSyncing ? (
                <>
                  <Loader2 className="mr-2 h-4 w-4 animate-spin" />
                  Syncing...
                </>
              ) : (
                'Start Comprehensive Sync'
              )}
            </Button>
            <Button 
              onClick={linkUsers} 
              disabled={isSyncing || isLinking}
              variant="outline"
              className="w-full"
              size="lg"
            >
              {isLinking ? (
                <>
                  <Loader2 className="mr-2 h-4 w-4 animate-spin" />
                  Linking...
                </>
              ) : (
                'Link Users Now'
              )}
            </Button>
          </div>

          {error && (
            <Alert variant="destructive">
              <XCircle className="h-4 w-4" />
              <AlertDescription>{error}</AlertDescription>
            </Alert>
          )}

          {result && (
            <Alert className="bg-green-50 border-green-200">
              <CheckCircle className="h-4 w-4 text-green-600" />
              <AlertDescription className="text-green-800">
                {result.type === 'link' ? (
                  <>
                    <strong>{result.message}</strong>
                    <div className="mt-4">
                      <a href="/analytics" className="text-blue-600 underline">Go to Analytics →</a>
                    </div>
                  </>
                ) : (
                  <>
                    <strong>Sync completed successfully!</strong>
                    <div className="mt-4 space-y-2">
                      <div>Users: {result.results?.users?.recordsInserted || 0} inserted</div>
                      <div>Offices: {result.results?.offices?.recordsInserted || 0} inserted</div>
                      <div>Customers: {result.results?.customers?.recordsInserted || 0} inserted</div>
                      <div>Appointments: {result.results?.appointments?.recordsInserted || 0} inserted</div>
                      <div>Status Logs: {result.results?.statusLogs?.recordsInserted || 0} inserted</div>
                    </div>
                    <div className="mt-4">
                      <a href="/analytics" className="text-blue-600 underline">Go to Analytics →</a>
                    </div>
                  </>
                )}
              </AlertDescription>
            </Alert>
          )}

          {result && (
            <div className="mt-4">
              <details>
                <summary className="cursor-pointer font-medium">View Full Results</summary>
                <pre className="mt-2 p-4 bg-gray-100 rounded overflow-auto text-xs">
                  {JSON.stringify(result, null, 2)}
                </pre>
              </details>
            </div>
          )}
        </CardContent>
      </Card>
    </div>
  );
}

