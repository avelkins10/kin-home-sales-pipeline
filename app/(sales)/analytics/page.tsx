'use client';

import { useState, useEffect } from 'react';
import { useSession } from 'next-auth/react';
import { redirect } from 'next/navigation';
import { useSearchParams, useRouter } from 'next/navigation';
import { format, subDays, startOfYear, startOfMonth, subMonths } from 'date-fns';
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
import { SetterPerformanceTable } from '@/components/analytics/SetterPerformanceTable';
import { CloserPerformanceTable } from '@/components/analytics/CloserPerformanceTable';
import { CloserDashboard } from '@/components/analytics/CloserDashboard';
import { RepBenchmarkComparisonCard } from '@/components/analytics/RepBenchmarkComparisonCard';
import { ConfigurableLeaderboard } from '@/components/analytics/ConfigurableLeaderboard';
import { CanvassingOverviewCard } from '@/components/analytics/CanvassingOverviewCard';
import { DoorsKnockedTrendsCard } from '@/components/analytics/DoorsKnockedTrendsCard';
import { AppointmentRatesCard } from '@/components/analytics/AppointmentRatesCard';
import { LeadQualityAnalysisCard } from '@/components/analytics/LeadQualityAnalysisCard';
import { RepCardOptimizedDashboard } from '@/components/analytics/RepCardOptimizedDashboard';
// import { CancellationAnalysisCard } from '@/components/analytics/CancellationAnalysisCard';
// import { HoldAnalysisCard } from '@/components/analytics/HoldAnalysisCard';
import { PeriodComparisonCard } from '@/components/analytics/PeriodComparisonCard';
import { BenchmarkComparisonCard } from '@/components/analytics/BenchmarkComparisonCard';
import { VisualComparisonsCard } from '@/components/analytics/VisualComparisonsCard';
import { isManagerRole } from '@/lib/utils/role-helpers';
import type { TimeRange, CustomDateRange } from '@/lib/types/dashboard';

/**
 * Convert TimeRange to actual date strings for API calls
 */
function getDateRangeFromTimeRange(timeRange: TimeRange, customDateRange?: CustomDateRange): { startDate: string; endDate: string } | undefined {
  if (timeRange === 'custom' && customDateRange) {
    return customDateRange;
  }

  const now = new Date();
  const endDate = format(now, 'yyyy-MM-dd');
  let startDate: string;

  switch (timeRange) {
    case 'today':
      startDate = format(now, 'yyyy-MM-dd');
      break;
    case 'last_30':
      startDate = format(subDays(now, 30), 'yyyy-MM-dd');
      break;
    case 'last_90':
      startDate = format(subDays(now, 90), 'yyyy-MM-dd');
      break;
    case 'last_12_months':
      startDate = format(subDays(now, 365), 'yyyy-MM-dd');
      break;
    case 'ytd':
      startDate = format(startOfYear(now), 'yyyy-MM-dd');
      break;
    case 'month':
      startDate = format(startOfMonth(now), 'yyyy-MM-dd');
      break;
    case 'last_month':
      const lastMonth = subMonths(now, 1);
      startDate = format(startOfMonth(lastMonth), 'yyyy-MM-dd');
      return {
        startDate,
        endDate: format(subDays(startOfMonth(now), 1), 'yyyy-MM-dd') // Last day of previous month
      };
    case 'lifetime':
      // Return undefined for lifetime to fetch all data
      return undefined;
    default:
      return undefined;
  }

  return { startDate, endDate };
}


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
  const [hasAttemptedLoad, setHasAttemptedLoad] = useState(false);

  // Parse URL params on mount and when they change
  useEffect(() => {
    const timeRangeParam = searchParams.get('timeRange') as TimeRange;
    const officeIdsParam = searchParams.get('officeIds');
    const repEmailParam = searchParams.get('repEmail');
    const startDateParam = searchParams.get('startDate');
    const endDateParam = searchParams.get('endDate');
    const tabParam = searchParams.get('tab');

    if (timeRangeParam && ['ytd', 'last_30', 'last_90', 'last_12_months', 'custom', 'lifetime', 'month', 'last_month', 'today'].includes(timeRangeParam)) {
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
    } else if (timeRangeParam !== 'custom') {
      // Clear customDateRange when switching away from 'custom'
      setCustomDateRange(undefined);
    }

    if (tabParam && ['overview', 'performance', 'comparisons', 'analysis', 'leaderboards', 'canvassing', 'repcard'].includes(tabParam)) {
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
          // Map from OfficeMetrics to Office format
          const offices = officesData.metrics?.map((metric: any) => ({
            id: metric.officeId,
            name: metric.officeName,
            projectCount: metric.totalProjects
          })) || [];
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
          // Map from RepPerformance to Rep format
          const reps = repsData.metrics?.map((metric: any) => ({
            email: metric.repEmail,
            name: metric.repName,
            role: metric.role
          })) || [];
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
      } finally {
        // Mark that we've attempted to load data
        setHasAttemptedLoad(true);
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
    } else if (availableOffices.length === 0 && hasAttemptedLoad) {
      // No offices available after attempting to load - this is a real issue
      console.warn('[Analytics] No offices available after load attempt, stopping initialization');
      setIsInitializingFilters(false);
    }
  }, [availableOffices, session?.user, selectedOfficeIds.length, hasAttemptedLoad]);

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
      // Clear customDateRange state when switching away from 'custom'
      setCustomDateRange(undefined);
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
    setActiveTab(value);
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
          value={activeTab}
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

              {/* NEW: Setter Performance Table - Full Width */}
              <SetterPerformanceTable
                userId={session.user.id}
                role={session.user.role}
                timeRange={timeRange}
                customDateRange={customDateRange}
                officeIds={selectedOfficeIds}
                showExport={true}
              />

              {/* NEW: Closer Dashboard - Full Width */}
              <CloserDashboard
                userId={session.user.id}
                role={session.user.role}
                timeRange={timeRange}
                customDateRange={customDateRange}
                officeIds={selectedOfficeIds}
              />

              {/* NEW: Closer Performance Table - Full Width */}
              <CloserPerformanceTable
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
          leaderboardsContent={
            <>
              {/* Top Setters - Doors Knocked */}
              <ConfigurableLeaderboard
                defaultRole="setter"
                defaultMetric="doors_knocked"
                defaultTimeRange={timeRange}
                defaultOfficeIds={selectedOfficeIds}
                title="Top Setters - Doors Knocked"
                description="Leading setters by door knocking activity"
                limit={25}
                collapsible={true}
                defaultOpen={true}
                showFilters={true}
                showExport={true}
                showRefresh={true}
              />

              {/* Top Closers - Revenue */}
              <ConfigurableLeaderboard
                defaultRole="closer"
                defaultMetric="revenue"
                defaultTimeRange={timeRange}
                defaultOfficeIds={selectedOfficeIds}
                title="Top Closers - Revenue"
                description="Highest revenue generators"
                limit={25}
                collapsible={true}
                defaultOpen={false}
                showFilters={true}
                showExport={true}
                showRefresh={true}
              />

              {/* Quality Leaders - All Roles */}
              <ConfigurableLeaderboard
                defaultRole="all"
                defaultMetric="quality_score"
                defaultTimeRange={timeRange}
                defaultOfficeIds={selectedOfficeIds}
                title="Quality Leaders"
                description="Reps with highest quality scores"
                limit={50}
                collapsible={true}
                defaultOpen={false}
                showFilters={true}
                showExport={true}
                showRefresh={true}
              />

              {/* Volume Leaders - Appointments Set */}
              <ConfigurableLeaderboard
                defaultRole="all"
                defaultMetric="appointments_set"
                defaultTimeRange={timeRange}
                defaultOfficeIds={selectedOfficeIds}
                title="Volume Leaders - Appointments"
                description="Reps with most appointments set"
                limit={50}
                collapsible={true}
                defaultOpen={false}
                showFilters={true}
                showExport={true}
                showRefresh={true}
              />
            </>
          }
          canvassingContent={
            <>
              {/* Canvassing Overview - Full Width */}
              <CanvassingOverviewCard
                userId={session.user.id}
                role={session.user.role}
                timeRange={timeRange}
                customDateRange={customDateRange}
                officeIds={selectedOfficeIds}
              />

              {/* Doors Knocked Trends and Appointment Rates - Side by Side */}
              <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
                <DoorsKnockedTrendsCard
                  userId={session.user.id}
                  role={session.user.role}
                  timeRange={timeRange}
                  customDateRange={customDateRange}
                  officeIds={selectedOfficeIds}
                />
                <AppointmentRatesCard
                  userId={session.user.id}
                  role={session.user.role}
                  timeRange={timeRange}
                  customDateRange={customDateRange}
                  officeIds={selectedOfficeIds}
                />
              </div>

              {/* Lead Quality Analysis - Full Width */}
              <LeadQualityAnalysisCard
                userId={session.user.id}
                role={session.user.role}
                timeRange={timeRange}
                customDateRange={customDateRange}
                officeIds={selectedOfficeIds}
              />
            </>
          }
          repcardContent={
            <RepCardOptimizedDashboard
              startDate={getDateRangeFromTimeRange(timeRange, customDateRange)?.startDate}
              endDate={getDateRangeFromTimeRange(timeRange, customDateRange)?.endDate}
            />
          }
        />
      )}
    </div>
  );
}
