'use client';

import { useState, useEffect } from 'react';
import { useSession } from 'next-auth/react';
import { redirect } from 'next/navigation';
import { useSearchParams, useRouter } from 'next/navigation';
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card';
import { AnalyticsHeader } from '@/components/analytics/AnalyticsHeader';
import { AnalyticsFilters } from '@/components/analytics/AnalyticsFilters';
import { AnalyticsTabs } from '@/components/analytics/AnalyticsTabs';
import { OfficeOverviewCard } from '@/components/analytics/OfficeOverviewCard';
// import { StatusBreakdownCard } from '@/components/analytics/StatusBreakdownCard';
import { MilestonePerformanceCard } from '@/components/analytics/MilestonePerformanceCard';
import { PipelineForecastCard } from '@/components/analytics/PipelineForecastCard';
import { OfficeComparisonTable } from '@/components/analytics/OfficeComparisonTable';
import { RepPerformanceTable } from '@/components/analytics/RepPerformanceTable';
import { RepBenchmarkComparisonCard } from '@/components/analytics/RepBenchmarkComparisonCard';
// import { CancellationAnalysisCard } from '@/components/analytics/CancellationAnalysisCard';
// import { HoldAnalysisCard } from '@/components/analytics/HoldAnalysisCard';
import { PeriodComparisonCard } from '@/components/analytics/PeriodComparisonCard';
import { BenchmarkComparisonCard } from '@/components/analytics/BenchmarkComparisonCard';
import { VisualComparisonsCard } from '@/components/analytics/VisualComparisonsCard';
import { isManagerRole } from '@/lib/utils/role-helpers';
import type { TimeRange, CustomDateRange } from '@/lib/types/dashboard';


export default function AnalyticsPage() {
  const { data: session, status } = useSession();
  const searchParams = useSearchParams();
  const router = useRouter();
  
  // Filter state from URL params - default to last_12_months for better data visibility
  const [timeRange, setTimeRange] = useState<TimeRange>('last_12_months');
  const [customDateRange, setCustomDateRange] = useState<CustomDateRange | undefined>();
  const [selectedOfficeIds, setSelectedOfficeIds] = useState<number[]>([]);
  const [selectedRepEmail, setSelectedRepEmail] = useState<string | undefined>();
  const [activeTab, setActiveTab] = useState<string>('overview');

  // Loading and data states
  const [isExporting, setIsExporting] = useState(false);
  const [availableOffices, setAvailableOffices] = useState<Array<{ id: number; name: string; projectCount?: number }>>([]);
  const [availableReps, setAvailableReps] = useState<Array<{ email: string; name: string; role: 'closer' | 'setter' }>>([]);
  const [isInitializingFilters, setIsInitializingFilters] = useState(true);

  // Parse URL params on mount and when they change
  useEffect(() => {
    const timeRangeParam = searchParams.get('timeRange') as TimeRange;
    const officeIdsParam = searchParams.get('officeIds');
    const repEmailParam = searchParams.get('repEmail');
    const startDateParam = searchParams.get('startDate');
    const endDateParam = searchParams.get('endDate');
    const tabParam = searchParams.get('tab');

    if (timeRangeParam && ['ytd', 'last_30', 'last_90', 'last_12_months', 'custom', 'lifetime', 'month', 'last_month'].includes(timeRangeParam)) {
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

    if (tabParam && ['overview', 'performance', 'comparisons', 'analysis'].includes(tabParam)) {
      setActiveTab(tabParam);
    }
  }, [searchParams]);

  // Persist default last_12_months to URL for fully shareable default state
  useEffect(() => {
    const timeRangeParam = searchParams.get('timeRange');
    if (!timeRangeParam) {
      const params = new URLSearchParams(searchParams.toString());
      params.set('timeRange', 'last_12_months');
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
          console.log('[Analytics] Office metrics response:', officesData);
          // Map from OfficeMetrics to Office format - API returns array directly
          const offices = (Array.isArray(officesData) ? officesData : officesData.metrics || []).map((metric: any) => ({
            id: typeof metric.officeId === 'string' ? parseInt(metric.officeId.replace(/\D/g, '')) || 1 : metric.officeId,
            name: metric.officeName,
            projectCount: metric.totalProjects || metric.projectCount || 0
          }));
          console.log('[Analytics] Mapped offices:', offices);
          setAvailableOffices(offices);
        } else {
          console.error('[Analytics] Failed to load offices:', officesResponse.status, officesResponse.statusText);
          setAvailableOffices([]);
        }

        // Load reps
        const repsResponse = await fetch('/api/analytics/rep-performance');
        if (repsResponse.ok) {
          const repsData = await repsResponse.json();
          // Map from RepPerformance to Rep format - API returns array directly
          const reps = (Array.isArray(repsData) ? repsData : repsData.metrics || []).map((metric: any) => ({
            email: metric.repEmail || metric.repId || `rep-${metric.repName}@example.com`,
            name: metric.repName,
            role: metric.role
          }));
          setAvailableReps(reps);
        } else {
          console.error('[Analytics] Failed to load reps:', repsResponse.status, repsResponse.statusText);
          setAvailableReps([]);
        }
      } catch (error) {
        console.error('Failed to load filter data:', error);
        // Set empty arrays on error to prevent infinite loading
        setAvailableOffices([]);
        setAvailableReps([]);
      }
    };

    if (session?.user) {
      loadFilterData();
    }
  }, [session]);

  // Set default office selection based on user role
  useEffect(() => {
    // Wait for session to be loaded
    if (!session?.user) return;

    console.log('[Analytics] Office selection effect:', {
      availableOfficesCount: availableOffices.length,
      selectedOfficeIdsCount: selectedOfficeIds.length,
      userRole: session.user.role
    });

    if (availableOffices.length > 0 && selectedOfficeIds.length === 0) {
      if (session.user.role === 'office_leader' && session.user.salesOffice) {
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
        console.log('[Analytics] Setting office IDs for office_leader:', assignedOfficeIds);
        setSelectedOfficeIds(assignedOfficeIds);
        setIsInitializingFilters(false);
      } else if (['regional', 'super_admin'].includes(session.user.role)) {
        // For regional/super admin, select all offices
        const allOfficeIds = availableOffices.map(office => office.id);
        console.log('[Analytics] Setting all office IDs for regional/super_admin:', allOfficeIds);
        setSelectedOfficeIds(allOfficeIds);
        setIsInitializingFilters(false);
      } else {
        // No offices to auto-select
        console.log('[Analytics] No auto-selection logic for role:', session.user.role);
        setIsInitializingFilters(false);
      }
    } else if (availableOffices.length > 0 && selectedOfficeIds.length > 0) {
      // Offices already selected from URL params
      console.log('[Analytics] Offices already selected from URL');
      setIsInitializingFilters(false);
    } else if (availableOffices.length === 0) {
      // No offices available - still need to stop loading after reasonable time
      console.warn('[Analytics] No offices available, stopping initialization');
      setIsInitializingFilters(false);
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

  const handleTabChange = (value: string) => {
    const params = new URLSearchParams(searchParams.toString());
    params.set('tab', value);
    router.push(`/analytics?${params.toString()}`);
    // Smooth scroll to top when switching tabs
    window.scrollTo({ top: 0, behavior: 'smooth' });
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
      {isInitializingFilters ? (
        <div className="flex items-center justify-center py-12">
          <div className="text-center">
            <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-gray-900 mx-auto mb-4"></div>
            <p className="text-gray-600">Loading analytics data...</p>
          </div>
        </div>
      ) : (
        <AnalyticsTabs
          defaultTab={activeTab}
          onTabChange={handleTabChange}
          overviewContent={
            <>
              {/* Office Overview - Full Width */}
              <OfficeOverviewCard
                userId={session.user.id}
                role={session.user.role}
                timeRange={timeRange}
                customDateRange={customDateRange}
                officeIds={selectedOfficeIds}
              />

              {/* Status Breakdown and Pipeline Forecast - Side by Side */}
              <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
                {/* <StatusBreakdownCard
                  userId={session.user.id}
                  role={session.user.role}
                  timeRange={timeRange}
                  customDateRange={customDateRange}
                  officeIds={selectedOfficeIds}
                /> */}
                <PipelineForecastCard
                  userId={session.user.id}
                  role={session.user.role}
                  officeIds={selectedOfficeIds}
                />
              </div>
            </>
          }
          performanceContent={
            <>
              {/* Rep Performance Table - Full Width */}
              <RepPerformanceTable
                userId={session.user.id}
                role={session.user.role}
                timeRange={timeRange}
                customDateRange={customDateRange}
                officeIds={selectedOfficeIds}
                showExport={true}
              />

              {/* Rep Benchmark Comparison - Full Width */}
              <RepBenchmarkComparisonCard
                userId={session.user.id}
                userEmail={session.user.email}
                role={session.user.role}
                timeRange={timeRange}
                customDateRange={customDateRange}
                officeIds={selectedOfficeIds}
              />

              {/* Milestone Performance - Full Width */}
              <MilestonePerformanceCard
                userId={session.user.id}
                role={session.user.role}
                timeRange={timeRange}
                customDateRange={customDateRange}
                officeIds={selectedOfficeIds}
              />
            </>
          }
          comparisonsContent={
            <>
              {/* Period Comparison - Full Width */}
              <PeriodComparisonCard
                userId={session.user.id}
                role={session.user.role}
                timeRange={timeRange}
                officeIds={selectedOfficeIds}
              />

              {/* Benchmark Comparison - Full Width */}
              <BenchmarkComparisonCard
                userId={session.user.id}
                role={session.user.role}
                timeRange={timeRange}
                customDateRange={customDateRange}
                officeIds={selectedOfficeIds}
              />

              {/* Visual Comparison Chart - Full Width */}
              <VisualComparisonsCard
                userId={session.user.id}
                role={session.user.role}
                timeRange={timeRange}
                customDateRange={customDateRange}
                officeIds={selectedOfficeIds}
              />

              {/* Office Comparison Table - Full Width */}
              <OfficeComparisonTable
                userId={session.user.id}
                role={session.user.role}
                timeRange={timeRange}
                customDateRange={customDateRange}
                officeIds={selectedOfficeIds}
                maxOffices={10}
              />
            </>
          }
          analysisContent={
            <>
              {/* Cancellation and Hold Analysis - Side by Side */}
              <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
                {/* <CancellationAnalysisCard
                  userId={session.user.id}
                  role={session.user.role}
                  timeRange={timeRange}
                  customDateRange={customDateRange}
                  officeIds={selectedOfficeIds}
                  showComparison={true}
                /> */}

                {/* <HoldAnalysisCard
                  userId={session.user.id}
                  role={session.user.role}
                  timeRange={timeRange}
                  customDateRange={customDateRange}
                  officeIds={selectedOfficeIds}
                  showComparison={true}
                /> */}
              </div>
            </>
          }
        />
      )}
    </div>
  );
}
