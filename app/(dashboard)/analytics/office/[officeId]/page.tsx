'use client';

import { useSession } from 'next-auth/react';
import { redirect, useParams, useRouter } from 'next/navigation';
import { useQuery } from '@tanstack/react-query';
import { Card, CardHeader, CardTitle, CardContent } from '@/components/ui/card';
import { Button } from '@/components/ui/button';
import { Skeleton } from '@/components/ui/skeleton';
import { ArrowLeft, Building2, Users } from 'lucide-react';
import Link from 'next/link';
import { OfficeOverviewCard } from '@/components/analytics/OfficeOverviewCard';
import { OfficeRepTable } from '@/components/analytics/OfficeRepTable';
import { formatSystemSize, formatPPW, formatPercentage } from '@/lib/utils/formatters';
import type { OfficeMetrics, RepPerformance } from '@/lib/types/analytics';
import { getBaseUrl } from '@/lib/utils/baseUrl';

// Loading skeleton for office header
function OfficeHeaderSkeleton() {
  return (
    <div className="space-y-4">
      <div className="flex items-center space-x-4">
        <Skeleton className="h-10 w-10" />
        <div className="space-y-2">
          <Skeleton className="h-8 w-48" />
          <Skeleton className="h-4 w-24" />
        </div>
      </div>
    </div>
  );
}

// Loading skeleton for rep performance table
function RepPerformanceSkeleton() {
  return (
    <Card>
      <CardHeader>
        <div className="flex items-center space-x-2">
          <Skeleton className="h-6 w-6" />
          <Skeleton className="h-6 w-32" />
        </div>
        <Skeleton className="h-4 w-48" />
      </CardHeader>
      <CardContent>
        <div className="overflow-x-auto">
          <div className="min-w-max">
            <div className="border rounded-lg">
              <div className="border-b p-4">
                <div className="flex space-x-4">
                  {Array.from({ length: 8 }).map((_, i) => (
                    <Skeleton key={i} className="h-4 w-20" />
                  ))}
                </div>
              </div>
              {Array.from({ length: 5 }).map((_, i) => (
                <div key={i} className="border-b p-4 last:border-b-0">
                  <div className="flex space-x-4">
                    {Array.from({ length: 8 }).map((_, j) => (
                      <Skeleton key={j} className="h-4 w-16" />
                    ))}
                  </div>
                </div>
              ))}
            </div>
          </div>
        </div>
      </CardContent>
    </Card>
  );
}

export default function OfficeDetailPage() {
  const { data: session, status } = useSession();
  const params = useParams();
  const router = useRouter();

  const officeId = params.officeId as string;
  const officeIdNumber = parseInt(officeId);
  const isValidOfficeId = !isNaN(officeIdNumber);

  // Fetch office metrics - always call hooks unconditionally
  const { data: officeData, isLoading: isLoadingOffice, error: officeError } = useQuery<OfficeMetrics[]>({
    queryKey: ['office-detail', officeIdNumber, session?.user?.id, session?.user?.role],
    queryFn: async () => {
      const response = await fetch(`${getBaseUrl()}/api/analytics/office-metrics?officeIds=${officeIdNumber}`);
      if (!response.ok) throw new Error('Failed to fetch office metrics');
      const result = await response.json();
      return result.metrics || [];
    },
    enabled: !!session?.user && isValidOfficeId,
  });

  // Fetch rep performance data - always call hooks unconditionally
  const { data: repsData, isLoading: isLoadingReps, error: repsError } = useQuery<RepPerformance[]>({
    queryKey: ['office-reps', officeIdNumber, session?.user?.id, session?.user?.role],
    queryFn: async () => {
      const response = await fetch(`${getBaseUrl()}/api/analytics/rep-performance?officeIds=${officeIdNumber}`);
      if (!response.ok) throw new Error('Failed to fetch rep performance');
      const data = await response.json();
      return data.metrics || [];
    },
    enabled: !!session?.user && isValidOfficeId,
  });

  // Validate office ID after hooks are called
  if (!isValidOfficeId) {
    return (
      <div className="flex items-center justify-center min-h-[400px]">
        <div className="text-center">
          <h1 className="text-2xl font-bold text-gray-900 mb-2">Invalid Office ID</h1>
          <p className="text-gray-600 mb-4">The office ID provided is not valid.</p>
          <Link href="/analytics">
            <Button variant="outline">
              <ArrowLeft className="h-4 w-4 mr-2" />
              Back to Analytics
            </Button>
          </Link>
        </div>
      </div>
    );
  }

  if (status === 'loading') {
    return <div>Loading...</div>;
  }

  if (!session) {
    redirect('/login');
  }

  // Check if user has access to analytics
  const hasAnalyticsAccess = ['office_leader', 'regional', 'super_admin'].includes(session.user.role);
  if (!hasAnalyticsAccess) {
    return (
      <div className="flex items-center justify-center min-h-[400px]">
        <div className="text-center">
          <h1 className="text-2xl font-bold text-gray-900 mb-2">Access Denied</h1>
          <p className="text-gray-600">
            You don&apos;t have permission to access the Analytics dashboard.
          </p>
        </div>
      </div>
    );
  }

  // Check office leader access to specific office
  if (session.user.role === 'office_leader' && session.user.salesOffice) {
    // We need to check access after we have office data to resolve office names to IDs
    // This check will be done after the office data is loaded
  }

  // Handle loading states
  if (isLoadingOffice) {
    return (
      <div className="space-y-6">
        <OfficeHeaderSkeleton />
        <div>Loading office details...</div>
        <RepPerformanceSkeleton />
      </div>
    );
  }

  // Handle errors
  if (officeError) {
    return (
      <div className="flex items-center justify-center min-h-[400px]">
        <div className="text-center">
          <h1 className="text-2xl font-bold text-gray-900 mb-2">Error Loading Office</h1>
          <p className="text-gray-600 mb-4">{officeError.message}</p>
          <Link href="/analytics">
            <Button variant="outline">
              <ArrowLeft className="h-4 w-4 mr-2" />
              Back to Analytics
            </Button>
          </Link>
        </div>
      </div>
    );
  }

  // Handle office not found
  if (!officeData || officeData.length === 0) {
    return (
      <div className="flex items-center justify-center min-h-[400px]">
        <div className="text-center">
          <h1 className="text-2xl font-bold text-gray-900 mb-2">Office Not Found</h1>
          <p className="text-gray-600 mb-4">The requested office could not be found.</p>
          <Link href="/analytics">
            <Button variant="outline">
              <ArrowLeft className="h-4 w-4 mr-2" />
              Back to Analytics
            </Button>
          </Link>
        </div>
      </div>
    );
  }

  const office = officeData[0]; // Get the single office from the array

  // Check office leader access to specific office (after office data is loaded)
  if (session.user.role === 'office_leader' && session.user.salesOffice) {
    const hasAccessToOffice = session.user.salesOffice.some(officeAssignment => {
      // If office is a number (ID), use it directly
      const officeId = parseInt(officeAssignment);
      if (!isNaN(officeId)) {
        return officeId === officeIdNumber;
      }
      // If office is a name, check if it matches the current office name
      return officeAssignment === office.officeName;
    });
    
    if (!hasAccessToOffice) {
      return (
        <div className="flex items-center justify-center min-h-[400px]">
          <div className="text-center">
            <h1 className="text-2xl font-bold text-gray-900 mb-2">Access Denied</h1>
            <p className="text-gray-600 mb-4">
              You don&apos;t have permission to view this office.
            </p>
            <Link href="/analytics">
              <Button variant="outline">
                <ArrowLeft className="h-4 w-4 mr-2" />
                Back to Analytics
              </Button>
            </Link>
          </div>
        </div>
      );
    }
  }

  return (
    <div className="space-y-6">
      {/* Breadcrumb Navigation */}
      <nav className="flex items-center space-x-2 text-sm text-gray-600">
        <Link href="/analytics" className="hover:text-gray-900">
          Analytics
        </Link>
        <span>/</span>
        <Link href="/analytics" className="hover:text-gray-900">
          Office Comparison
        </Link>
        <span>/</span>
        <span className="text-gray-900 font-medium">{office.officeName}</span>
      </nav>

      {/* Header Section */}
      <div className="flex items-center justify-between">
        <div className="flex items-center space-x-4">
          <Link href="/analytics">
            <Button variant="outline" size="sm">
              <ArrowLeft className="h-4 w-4 mr-2" />
              Back
            </Button>
          </Link>
          <div>
            <h1 className="text-3xl font-bold text-gray-900 flex items-center space-x-2">
              <Building2 className="h-8 w-8 text-blue-600" />
              <span>{office.officeName}</span>
            </h1>
            <p className="text-sm text-gray-600">Office ID: {office.officeId}</p>
          </div>
        </div>
      </div>

      {/* Office Overview Section */}
      <OfficeOverviewCard
        userId={session.user.id}
        role={session.user.role}
        timeRange="ytd"
        officeIds={[officeIdNumber]}
      />

      {/* Team Performance Section */}
      <Card>
        <CardHeader>
          <CardTitle className="flex items-center space-x-2">
            <Users className="h-6 w-6 text-blue-600" />
            <span>Team Performance</span>
          </CardTitle>
          <p className="text-sm text-gray-600">
            {repsData?.length || 0} team member{repsData?.length !== 1 ? 's' : ''} in this office
          </p>
        </CardHeader>
        <CardContent>
          <OfficeRepTable
            reps={repsData || []}
            isLoading={isLoadingReps}
            officeId={officeIdNumber}
          />
        </CardContent>
      </Card>

      {/* Additional Sections - Placeholders for Future Enhancement */}
      <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
        {/* Recent Projects */}
        <Card>
          <CardHeader>
            <CardTitle>Recent Projects</CardTitle>
          </CardHeader>
          <CardContent>
            <div className="text-center text-gray-500 py-8">
              Recent projects component will be implemented in the next phase
            </div>
          </CardContent>
        </Card>

        {/* Monthly Trends */}
        <Card>
          <CardHeader>
            <CardTitle>Monthly Trends</CardTitle>
          </CardHeader>
          <CardContent>
            <div className="text-center text-gray-500 py-8">
              Monthly trends chart will be implemented in the next phase
            </div>
          </CardContent>
        </Card>
      </div>

      {/* Cancellation Analysis */}
      <Card>
        <CardHeader>
          <CardTitle>Cancellation Analysis</CardTitle>
        </CardHeader>
        <CardContent>
          <div className="text-center text-gray-500 py-8">
            Cancellation analysis specific to this office will be implemented in the next phase
          </div>
        </CardContent>
      </Card>

      {/* Hold Analysis */}
      <Card>
        <CardHeader>
          <CardTitle>Hold Analysis</CardTitle>
        </CardHeader>
        <CardContent>
          <div className="text-center text-gray-500 py-8">
            Hold analysis specific to this office will be implemented in the next phase
          </div>
        </CardContent>
      </Card>
    </div>
  );
}
