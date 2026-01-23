'use client';

import { useState } from 'react';
import { Button } from '@/components/ui/button';
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card';
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from '@/components/ui/select';
import { Input } from '@/components/ui/input';
import { Label } from '@/components/ui/label';
import { Loader2 } from 'lucide-react';

export default function RepCardAPIDataPage() {
  const [type, setType] = useState('all');
  const [limit, setLimit] = useState('5');
  const [loading, setLoading] = useState(false);
  const [data, setData] = useState<any>(null);
  const [error, setError] = useState<string | null>(null);

  const fetchData = async () => {
    setLoading(true);
    setError(null);
    setData(null);

    try {
      const params = new URLSearchParams({
        type,
        limit,
      });

      const response = await fetch(`/api/repcard/raw-api-data?${params.toString()}`);
      
      if (!response.ok) {
        const errorData = await response.json().catch(() => ({ error: 'Unknown error' }));
        throw new Error(errorData.error || errorData.message || `HTTP ${response.status}`);
      }

      const result = await response.json();
      setData(result);
    } catch (err) {
      setError(err instanceof Error ? err.message : 'Failed to fetch data');
      console.error('Error fetching RepCard API data:', err);
    } finally {
      setLoading(false);
    }
  };

  return (
    <div className="container mx-auto p-6 space-y-6">
      <div className="flex items-center justify-between">
        <div>
          <h1 className="text-3xl font-bold">RepCard API Data Viewer</h1>
          <p className="text-muted-foreground mt-2">
            View raw API payloads from RepCard to understand data structure
          </p>
        </div>
      </div>

      <Card>
        <CardHeader>
          <CardTitle>Fetch Options</CardTitle>
          <CardDescription>
            Select the type of data to fetch and the number of records
          </CardDescription>
        </CardHeader>
        <CardContent className="space-y-4">
          <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
            <div className="space-y-2">
              <Label htmlFor="type">Data Type</Label>
              <Select value={type} onValueChange={setType}>
                <SelectTrigger id="type">
                  <SelectValue placeholder="Select data type" />
                </SelectTrigger>
                <SelectContent>
                  <SelectItem value="all">All (Users, Appointments, Customers, Teams)</SelectItem>
                  <SelectItem value="users">Users Only</SelectItem>
                  <SelectItem value="appointments">Appointments Only</SelectItem>
                  <SelectItem value="customers">Customers Only</SelectItem>
                  <SelectItem value="teams">Teams Only</SelectItem>
                </SelectContent>
              </Select>
            </div>

            <div className="space-y-2">
              <Label htmlFor="limit">Record Limit</Label>
              <Input
                id="limit"
                type="number"
                min="1"
                max="50"
                value={limit}
                onChange={(e) => setLimit(e.target.value)}
                placeholder="5"
              />
            </div>
          </div>

          <Button onClick={fetchData} disabled={loading} className="w-full md:w-auto">
            {loading ? (
              <>
                <Loader2 className="mr-2 h-4 w-4 animate-spin" />
                Fetching...
              </>
            ) : (
              'Fetch RepCard API Data'
            )}
          </Button>
        </CardContent>
      </Card>

      {error && (
        <Card className="border-destructive">
          <CardHeader>
            <CardTitle className="text-destructive">Error</CardTitle>
          </CardHeader>
          <CardContent>
            <pre className="whitespace-pre-wrap text-sm">{error}</pre>
          </CardContent>
        </Card>
      )}

      {data && (
        <Card>
          <CardHeader>
            <CardTitle>API Response Data</CardTitle>
            <CardDescription>
              Raw payload structure from RepCard API
            </CardDescription>
          </CardHeader>
          <CardContent>
            <div className="space-y-4">
              <div className="text-sm text-muted-foreground">
                <p>Timestamp: {data.timestamp}</p>
                <p>Type: {data.type}</p>
                <p>Limit: {data.limit}</p>
              </div>

              {data.data && (
                <div className="space-y-6">
                  {data.data.users && (
                    <div>
                      <h3 className="font-semibold mb-2">Users Data</h3>
                      <div className="bg-muted p-4 rounded-lg overflow-auto max-h-96">
                        <pre className="text-xs whitespace-pre-wrap">
                          {JSON.stringify(data.data.users, null, 2)}
                        </pre>
                      </div>
                    </div>
                  )}

                  {data.data.appointments && (
                    <div>
                      <h3 className="font-semibold mb-2">Appointments Data</h3>
                      <div className="bg-muted p-4 rounded-lg overflow-auto max-h-96">
                        <pre className="text-xs whitespace-pre-wrap">
                          {JSON.stringify(data.data.appointments, null, 2)}
                        </pre>
                      </div>
                    </div>
                  )}

                  {data.data.customers && (
                    <div>
                      <h3 className="font-semibold mb-2">Customers Data</h3>
                      <div className="bg-muted p-4 rounded-lg overflow-auto max-h-96">
                        <pre className="text-xs whitespace-pre-wrap">
                          {JSON.stringify(data.data.customers, null, 2)}
                        </pre>
                      </div>
                    </div>
                  )}

                  {data.data.teams && (
                    <div>
                      <h3 className="font-semibold mb-2">Teams Data</h3>
                      <div className="bg-muted p-4 rounded-lg overflow-auto max-h-96">
                        <pre className="text-xs whitespace-pre-wrap">
                          {JSON.stringify(data.data.teams, null, 2)}
                        </pre>
                      </div>
                    </div>
                  )}

                  {data.data.userDetails && (
                    <div>
                      <h3 className="font-semibold mb-2">User Details</h3>
                      <div className="bg-muted p-4 rounded-lg overflow-auto max-h-96">
                        <pre className="text-xs whitespace-pre-wrap">
                          {JSON.stringify(data.data.userDetails, null, 2)}
                        </pre>
                      </div>
                    </div>
                  )}
                </div>
              )}
            </div>
          </CardContent>
        </Card>
      )}

      <Card>
        <CardHeader>
          <CardTitle>How to Use</CardTitle>
        </CardHeader>
        <CardContent className="space-y-2 text-sm">
          <p><strong>Option 1:</strong> Use this page to fetch and view data interactively</p>
          <p><strong>Option 2:</strong> Use the API endpoint directly in your browser (while logged in):</p>
          <code className="block bg-muted p-2 rounded mt-2">
            /api/repcard/raw-api-data?type=all&limit=5
          </code>
          <p className="mt-4"><strong>Option 3:</strong> Use curl in terminal (with your session cookie):</p>
          <code className="block bg-muted p-2 rounded mt-2">
            curl -H &quot;Cookie: your-session-cookie&quot; https://kineticsales.app/api/repcard/raw-api-data?type=users&limit=5
          </code>
        </CardContent>
      </Card>
    </div>
  );
}
