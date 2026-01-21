'use client';

import { useQuery } from '@tanstack/react-query';
import { Card, CardContent } from '@/components/ui/card';
import { AlertCircle, CheckCircle, XCircle, RefreshCw, ExternalLink } from 'lucide-react';
import { Button } from '@/components/ui/button';
import { Alert, AlertDescription, AlertTitle } from '@/components/ui/alert';
import { getBaseUrl } from '@/lib/utils/baseUrl';
import { useSession } from 'next-auth/react';

export function RepCardDiagnosticBanner() {
  const { data: session } = useSession();
  // Show to all admins/managers for debugging - can restrict later
  const isAuthorized = session?.user?.role && ['super_admin', 'regional', 'office_leader'].includes(session.user.role);

  const { data: diagnostic, isLoading, error, refetch } = useQuery({
    queryKey: ['repcard-diagnostic'],
    queryFn: async () => {
      const response = await fetch(`${getBaseUrl()}/api/repcard/diagnostic`);
      if (!response.ok) {
        const errorText = await response.text();
        throw new Error(`Failed to fetch diagnostic: ${response.status} ${errorText}`);
      }
      return response.json();
    },
    enabled: isAuthorized,
    refetchInterval: 30000, // Refresh every 30 seconds (aligned with sync optimization)
    staleTime: 30000,
    retry: 1
  });

  if (!isAuthorized) {
    return null; // Only show to authorized users
  }

  // Show error state if API call failed
  if (error) {
    return (
      <Alert className="mb-6 border-red-200 bg-red-50">
        <AlertCircle className="h-5 w-5 text-red-600" />
        <AlertTitle className="text-red-800">Diagnostic Check Failed</AlertTitle>
        <AlertDescription className="mt-2">
          <p className="text-sm text-red-700">
            {error instanceof Error ? error.message : 'Failed to check RepCard status'}
          </p>
          <Button
            variant="outline"
            size="sm"
            onClick={() => refetch()}
            className="mt-2 text-red-700 border-red-300 hover:bg-red-100"
          >
            <RefreshCw className="h-4 w-4 mr-1" />
            Retry
          </Button>
        </AlertDescription>
      </Alert>
    );
  }

  if (isLoading) {
    return (
      <Card className="mb-6 border-blue-200 bg-blue-50">
        <CardContent className="p-4">
          <div className="flex items-center gap-2 text-blue-600">
            <RefreshCw className="h-4 w-4 animate-spin" />
            <span className="text-sm">Checking RepCard status...</span>
          </div>
        </CardContent>
      </Card>
    );
  }

  if (!diagnostic) {
    return null;
  }

  const { status, issues, recommendations } = diagnostic;

  // Show healthy only if truly healthy (API works AND data exists)
  if (status === 'healthy') {
    return (
      <Card className="mb-6 border-green-200 bg-green-50">
        <CardContent className="p-4">
          <div className="flex items-center justify-between">
            <div className="flex items-center gap-2 text-green-700">
              <CheckCircle className="h-5 w-5" />
              <span className="text-sm font-medium">RepCard integration is healthy</span>
            </div>
            <Button
              variant="ghost"
              size="sm"
              onClick={() => refetch()}
              className="text-green-700 hover:text-green-800"
            >
              <RefreshCw className="h-4 w-4 mr-1" />
              Refresh
            </Button>
          </div>
        </CardContent>
      </Card>
    );
  }

  // Show setup needed (API works but no data)
  if (status === 'needs_setup') {
    return (
      <Alert className="mb-6 border-blue-200 bg-blue-50">
        <AlertCircle className="h-5 w-5 text-blue-600" />
        <AlertTitle className="text-blue-800">RepCard API Connected - Setup Required</AlertTitle>
        <AlertDescription className="mt-2 space-y-3">
          <p className="text-sm text-blue-700">
            The RepCard API connection is working, but no data has been synced yet. Follow the steps below to get started.
          </p>
          
          {recommendations && recommendations.length > 0 && (
            <div className="mt-4">
              <p className="text-sm font-medium text-blue-800 mb-2">Next Steps:</p>
              <div className="space-y-3">
                {recommendations.map((rec: any, index: number) => (
                  <div key={index} className="bg-white/50 rounded p-3 border border-blue-200">
                    <div className="flex items-start justify-between">
                      <div className="flex-1">
                        <p className="text-sm font-medium text-blue-900">
                          {rec.priority === 'critical' && '🔴 '}
                          {rec.priority === 'high' && '🟡 '}
                          {rec.action}
                        </p>
                        {rec.steps && (
                          <ol className="mt-2 space-y-1 text-xs text-blue-800">
                            {rec.steps.map((step: string, stepIndex: number) => (
                              <li key={stepIndex} className="flex items-start gap-2">
                                <span className="text-blue-600 mt-0.5">{stepIndex + 1}.</span>
                                <span>{step}</span>
                              </li>
                            ))}
                          </ol>
                        )}
                      </div>
                    </div>
                  </div>
                ))}
              </div>
            </div>
          )}

          <div className="flex items-center gap-2 mt-4 pt-3 border-t border-blue-200">
            <Button
              variant="outline"
              size="sm"
              onClick={() => refetch()}
              className="text-blue-700 border-blue-300 hover:bg-blue-100"
            >
              <RefreshCw className="h-4 w-4 mr-1" />
              Refresh Status
            </Button>
            <Button
              variant="outline"
              size="sm"
              onClick={() => window.open('/admin/repcard-sync', '_blank')}
              className="text-blue-700 border-blue-300 hover:bg-blue-100"
            >
              <ExternalLink className="h-4 w-4 mr-1" />
              Open Sync Dashboard
            </Button>
          </div>
        </AlertDescription>
      </Alert>
    );
  }

  return (
    <Alert className="mb-6 border-yellow-200 bg-yellow-50">
      <AlertCircle className="h-5 w-5 text-yellow-600" />
      <AlertTitle className="text-yellow-800">RepCard Integration Issues Detected</AlertTitle>
      <AlertDescription className="mt-2 space-y-3">
        <div>
          <p className="text-sm font-medium text-yellow-800 mb-2">Issues Found:</p>
          <ul className="list-disc list-inside text-sm text-yellow-700 space-y-1">
            {issues.map((issue: string, index: number) => (
              <li key={index}>{issue}</li>
            ))}
          </ul>
        </div>

        {recommendations && recommendations.length > 0 && (
          <div className="mt-4">
            <p className="text-sm font-medium text-yellow-800 mb-2">Recommended Actions:</p>
            <div className="space-y-3">
              {recommendations.map((rec: any, index: number) => (
                <div key={index} className="bg-white/50 rounded p-3 border border-yellow-200">
                  <div className="flex items-start justify-between">
                    <div className="flex-1">
                      <p className="text-sm font-medium text-yellow-900">
                        {rec.priority === 'critical' && '🔴 '}
                        {rec.priority === 'high' && '🟡 '}
                        {rec.action}
                      </p>
                      {rec.steps && (
                        <ol className="mt-2 space-y-1 text-xs text-yellow-800">
                          {rec.steps.map((step: string, stepIndex: number) => (
                            <li key={stepIndex} className="flex items-start gap-2">
                              <span className="text-yellow-600 mt-0.5">{stepIndex + 1}.</span>
                              <span>{step}</span>
                            </li>
                          ))}
                        </ol>
                      )}
                    </div>
                  </div>
                </div>
              ))}
            </div>
          </div>
        )}

        <div className="flex items-center gap-2 mt-4 pt-3 border-t border-yellow-200">
          <Button
            variant="outline"
            size="sm"
            onClick={() => refetch()}
            className="text-yellow-700 border-yellow-300 hover:bg-yellow-100"
          >
            <RefreshCw className="h-4 w-4 mr-1" />
            Refresh Status
          </Button>
          <Button
            variant="outline"
            size="sm"
            onClick={() => window.open('/admin/repcard-sync', '_blank')}
            className="text-yellow-700 border-yellow-300 hover:bg-yellow-100"
          >
            <ExternalLink className="h-4 w-4 mr-1" />
            Open Sync Dashboard
          </Button>
        </div>
      </AlertDescription>
    </Alert>
  );
}

