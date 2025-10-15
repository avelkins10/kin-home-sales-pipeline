'use client';

import { useState, useEffect } from 'react';
import { useSession } from 'next-auth/react';
import { redirect } from 'next/navigation';
import { useSearchParams, useRouter } from 'next/navigation';
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card';
import { AnalyticsHeader } from '@/components/analytics/AnalyticsHeader';
import { AnalyticsFilters } from '@/components/analytics/AnalyticsFilters';
import { OfficeOverviewCard } from '@/components/analytics/OfficeOverviewCard';
import { StatusBreakdownCard } from '@/components/analytics/StatusBreakdownCard';
import { MilestonePerformanceCard } from '@/components/analytics/MilestonePerformanceCard';
import { PipelineForecastCard } from '@/components/analytics/PipelineForecastCard';
import { OfficeComparisonTable } from '@/components/analytics/OfficeComparisonTable';
import { RepPerformanceTable } from '@/components/analytics/RepPerformanceTable';
import { CancellationAnalysisCard } from '@/components/analytics/CancellationAnalysisCard';
import { HoldAnalysisCard } from '@/components/analytics/HoldAnalysisCard';
import { isManagerRole } from '@/lib/utils/role-helpers';
import type { TimeRange, CustomDateRange } from '@/lib/types/dashboard';


export default function AnalyticsPage() {
  const { data: session, status } = useSession();
  const searchParams = useSearchParams();
  const router = useRouter();
  
  // Filter state from URL params
  const [timeRange, setTimeRange] = useState<TimeRange>('ytd');
  const [customDateRange, setCustomDateRange] = useState<CustomDateRange | undefined>();
  const [selectedOfficeIds, setSelectedOfficeIds] = useState<number[]>([]);
  const [selectedRepEmail, setSelectedRepEmail] = useState<string | undefined>();
  
  // Loading and data states
  const [isExporting, setIsExporting] = useState(false);
  const [availableOffices, setAvailableOffices] = useState<Array<{ id: number; name: string; projectCount?: number }>>([]);
  const [availableReps, setAvailableReps] = useState<Array<{ email: string; name: string; role: 'closer' | 'setter' }>>([]);

  // Parse URL params on mount and when they change
  useEffect(() => {
    const timeRangeParam = searchParams.get('timeRange') as TimeRange;
    const officeIdsParam = searchParams.get('officeIds');
    const repEmailParam = searchParams.get('repEmail');
    const startDateParam = searchParams.get('startDate');
    const endDateParam = searchParams.get('endDate');

    if (timeRangeParam && ['ytd', 'last_30', 'last_90', 'last_12_months', 'custom'].includes(timeRangeParam)) {
      setTimeRange(timeRangeParam);
    }

    if (officeIdsParam) {
      const officeIds = officeIdsParam.split(',').map(id => parseInt(id.trim())).filter(id => !isNaN(id));
      setSelectedOfficeIds(officeIds);
    }

    if (repEmailParam) {
      setSelectedRepEmail(repEmailParam);
    }

    if (timeRangeParam === 'custom' && startDateParam && endDateParam) {
      setCustomDateRange({
        startDate: startDateParam,
        endDate: endDateParam
      });
    }
  }, [searchParams]);

  // Persist default YTD to URL for fully shareable default state
  useEffect(() => {
    const timeRangeParam = searchParams.get('timeRange');
    if (!timeRangeParam) {
      const params = new URLSearchParams(searchParams.toString());
      params.set('timeRange', 'ytd');
      router.replace(`/analytics?${params.toString()}`);
    }
  }, [searchParams, router]);

  // Load available offices and reps
  useEffect(() => {
    const loadFilterData = async () => {
      try {
        // Load offices
        const officesResponse = await fetch('/api/analytics/office-metrics');
        if (officesResponse.ok) {
          const officesData = await officesResponse.json();
          // Map from OfficeMetrics to Office format
          const offices = officesData.metrics?.map((metric: any) => ({
            id: metric.officeId,
            name: metric.officeName,
            projectCount: metric.totalProjects
          })) || [];
          setAvailableOffices(offices);
        }

        // Load reps
        const repsResponse = await fetch('/api/analytics/rep-performance');
        if (repsResponse.ok) {
          const repsData = await repsResponse.json();
          // Map from RepPerformance to Rep format
          const reps = repsData.metrics?.map((metric: any) => ({
            email: metric.repEmail,
            name: metric.repName,
            role: metric.role
          })) || [];
          setAvailableReps(reps);
        }
      } catch (error) {
        console.error('Failed to load filter data:', error);
      }
    };

    if (session?.user) {
      loadFilterData();
    }
  }, [session]);

  // Set default office selection based on user role
  useEffect(() => {
    if (availableOffices.length > 0 && selectedOfficeIds.length === 0) {
      if (session?.user?.role === 'office_leader' && session.user.salesOffice) {
        // For office leaders, pre-select their assigned offices
        // Handle both ID and name formats
        const assignedOfficeIds = session.user.salesOffice.map(office => {
          // If office is a number (ID), use it directly
          const officeId = parseInt(office);
          if (!isNaN(officeId)) {
            return officeId;
          }
          // If office is a name, find the corresponding ID
          return availableOffices.find(o => o.name === office)?.id;
        }).filter(Boolean) as number[];
        setSelectedOfficeIds(assignedOfficeIds);
      } else if (['regional', 'super_admin'].includes(session?.user?.role || '')) {
        // For regional/super admin, select all offices
        setSelectedOfficeIds(availableOffices.map(office => office.id));
      }
    }
  }, [availableOffices, session?.user, selectedOfficeIds.length]);

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

  const handleTimeRangeChange = (range: TimeRange, customRange?: CustomDateRange) => {
    const params = new URLSearchParams(searchParams.toString());
    params.set('timeRange', range);
    
    if (range === 'custom' && customRange) {
      params.set('startDate', customRange.startDate);
      params.set('endDate', customRange.endDate);
    } else {
      params.delete('startDate');
      params.delete('endDate');
    }
    
    router.push(`/analytics?${params.toString()}`);
  };

  const handleOfficeIdsChange = (officeIds: number[]) => {
    const params = new URLSearchParams(searchParams.toString());
    
    if (officeIds.length === 0 || officeIds.length === availableOffices.length) {
      params.delete('officeIds');
    } else {
      params.set('officeIds', officeIds.join(','));
    }
    
    router.push(`/analytics?${params.toString()}`);
  };

  const handleRepEmailChange = (repEmail?: string) => {
    const params = new URLSearchParams(searchParams.toString());
    
    if (!repEmail) {
      params.delete('repEmail');
    } else {
      params.set('repEmail', repEmail);
    }
    
    router.push(`/analytics?${params.toString()}`);
  };

  const handleExportCSV = async () => {
    setIsExporting(true);
    try {
      // TODO: Implement CSV export functionality
      // This will fetch data from analytics API endpoints and convert to CSV
      console.log('Exporting CSV with filters:', {
        timeRange,
        customDateRange,
        selectedOfficeIds,
        selectedRepEmail
      });
      
      // Simulate export delay
      await new Promise(resolve => setTimeout(resolve, 2000));
      
      // TODO: Trigger actual CSV download
    } catch (error) {
      console.error('Export failed:', error);
    } finally {
      setIsExporting(false);
    }
  };

  return (
    <div className="space-y-6">
      {/* Page Header */}
      <AnalyticsHeader
        onExportCSV={handleExportCSV}
        isExporting={isExporting}
      />

      {/* Filters */}
      <AnalyticsFilters
        selectedTimeRange={timeRange}
        customDateRange={customDateRange}
        selectedOfficeIds={selectedOfficeIds}
        selectedRepEmail={selectedRepEmail}
        availableOffices={availableOffices}
        availableReps={availableReps}
        onTimeRangeChange={handleTimeRangeChange}
        onOfficeIdsChange={handleOfficeIdsChange}
        onRepEmailChange={handleRepEmailChange}
      />

      {/* Analytics Content */}
      <div className="space-y-6">
        {/* Office Overview - Full Width */}
        <OfficeOverviewCard 
          userId={session.user.id} 
          role={session.user.role} 
          timeRange={timeRange} 
          customDateRange={customDateRange} 
          officeIds={selectedOfficeIds} 
        />

        {/* Status Breakdown and Pipeline Forecast - Side by Side on Desktop */}
        <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
          <StatusBreakdownCard 
            userId={session.user.id} 
            role={session.user.role} 
            timeRange={timeRange} 
            customDateRange={customDateRange} 
            officeIds={selectedOfficeIds} 
          />
          <PipelineForecastCard 
            userId={session.user.id} 
            role={session.user.role} 
            officeIds={selectedOfficeIds} 
          />
        </div>

        {/* Milestone Performance - Full Width */}
        <MilestonePerformanceCard 
          userId={session.user.id} 
          role={session.user.role} 
          timeRange={timeRange} 
          customDateRange={customDateRange} 
          officeIds={selectedOfficeIds} 
        />

        {/* Rep Performance Table - Full Width */}
        <RepPerformanceTable
          userId={session.user.id}
          role={session.user.role}
          timeRange={timeRange}
          customDateRange={customDateRange}
          officeIds={selectedOfficeIds}
          showExport={true}
        />

        {/* Cancellation and Hold Analysis - Side by Side */}
        <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
          <CancellationAnalysisCard
            userId={session.user.id}
            role={session.user.role}
            timeRange={timeRange}
            customDateRange={customDateRange}
            officeIds={selectedOfficeIds}
            showComparison={true}
          />
          
          <HoldAnalysisCard
            userId={session.user.id}
            role={session.user.role}
            timeRange={timeRange}
            customDateRange={customDateRange}
            officeIds={selectedOfficeIds}
            showComparison={true}
          />
        </div>

        {/* Office Comparison Table - Full Width */}
        <OfficeComparisonTable
          userId={session.user.id}
          role={session.user.role}
          timeRange={timeRange}
          customDateRange={customDateRange}
          officeIds={selectedOfficeIds}
          maxOffices={5}
        />
      </div>
    </div>
  );
}
