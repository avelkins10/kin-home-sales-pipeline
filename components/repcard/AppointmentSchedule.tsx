'use client';

import { useState, useMemo } from 'react';
import { useQuery } from '@tanstack/react-query';
import { Card } from '@/components/ui/card';
import { Button } from '@/components/ui/button';
import { Badge } from '@/components/ui/badge';
import { Skeleton } from '@/components/ui/skeleton';
import { 
  Calendar as CalendarIcon, 
  List, 
  RefreshCw,
  AlertCircle
} from 'lucide-react';
import { useMediaQuery } from '@/lib/hooks/useMediaQuery';
import { cn } from '@/lib/utils';
import { AppointmentFilters } from './AppointmentFilters';
import { AppointmentCard, AppointmentData } from './AppointmentCard';
import { AppointmentDetailModal } from './AppointmentDetailModal';
import { AppointmentCalendarView } from './AppointmentCalendarView';
import { AppointmentMetrics } from './AppointmentMetrics';
import { getBaseUrl } from '@/lib/utils/baseUrl';

type ViewMode = 'list' | 'day' | 'week' | 'month';

interface AppointmentScheduleProps {
  userId: string;
  userRole: string;
  initialFilters?: Partial<AppointmentFilters>;
}

export function AppointmentSchedule({ 
  userId, 
  userRole,
  initialFilters 
}: AppointmentScheduleProps) {
  const isMobile = useMediaQuery('(max-width: 768px)');
  const [viewMode, setViewMode] = useState<ViewMode>(isMobile ? 'list' : 'week');
  const [showAdvanced, setShowAdvanced] = useState(false);
  const [selectedAppointment, setSelectedAppointment] = useState<AppointmentData | null>(null);
  const [calendarDate, setCalendarDate] = useState<Date>(new Date());

  // Default filters
  const today = new Date();
  today.setHours(0, 0, 0, 0);
  const defaultEndDate = new Date(today);
  defaultEndDate.setDate(defaultEndDate.getDate() + 7);

  const [filters, setFilters] = useState<AppointmentFilters>({
    startDate: initialFilters?.startDate || today.toISOString().split('T')[0],
    endDate: initialFilters?.endDate || defaultEndDate.toISOString().split('T')[0],
    ...initialFilters
  });

  // Build query params
  const queryParams = useMemo(() => {
    const params = new URLSearchParams({
      startDate: filters.startDate,
      endDate: filters.endDate
    });

    if (filters.officeIds && filters.officeIds.length > 0) {
      params.append('officeIds', filters.officeIds.join(','));
    }
    if (filters.teamIds && filters.teamIds.length > 0) {
      params.append('teamIds', filters.teamIds.join(','));
    }
    if (filters.calendarId) {
      params.append('calendarId', filters.calendarId.toString());
    }
    if (filters.status) {
      params.append('status', filters.status);
    }
    if (filters.hasPowerBill !== undefined) {
      params.append('hasPowerBill', filters.hasPowerBill.toString());
    }
    if (filters.isReschedule !== undefined) {
      params.append('isReschedule', filters.isReschedule.toString());
    }

    return params.toString();
  }, [filters]);

  // Fetch appointments
  const { data, isLoading, error, refetch } = useQuery<{
    appointments: AppointmentData[];
    count: number;
    filters: any;
    metadata: any;
  }>({
    queryKey: ['repcard-appointments-schedule', userId, queryParams],
    queryFn: async () => {
      const response = await fetch(`${getBaseUrl()}/api/repcard/appointments/schedule?${queryParams}`);
      if (!response.ok) {
        throw new Error('Failed to fetch appointments');
      }
      return response.json();
    },
    refetchInterval: 30000, // Poll every 30 seconds
    staleTime: 60000
  });

  // Fetch available offices, teams, and calendars for filters
  const { data: filterData, isLoading: isLoadingFilterData, error: filterError } = useQuery({
    queryKey: ['repcard-filter-data', userId, userRole],
    queryFn: async () => {
      console.log('[AppointmentSchedule] Fetching filter data for role:', userRole);

      // Fetch offices, teams, and calendars
      const [officesRes, teamsRes, calendarsRes] = await Promise.all([
        fetch(`${getBaseUrl()}/api/repcard/data?type=offices`).catch(err => {
          console.error('[AppointmentSchedule] Failed to fetch offices:', err);
          return null;
        }),
        fetch(`${getBaseUrl()}/api/repcard/data?type=teams&limit=1000`).catch(err => {
          console.error('[AppointmentSchedule] Failed to fetch teams:', err);
          return null;
        }),
        fetch(`${getBaseUrl()}/api/repcard/data?type=calendars`).catch(err => {
          console.error('[AppointmentSchedule] Failed to fetch calendars:', err);
          return null;
        })
      ]);

      // Get offices directly from offices table
      const officesData = officesRes?.ok ? (await officesRes.json()).data : [];
      const offices = officesData.map((office: any) => ({
        id: office.repcard_office_id,
        name: office.name
      }));
      console.log('[AppointmentSchedule] Loaded offices:', offices.length);

      // Get teams directly from teams table
      const teamsData = teamsRes?.ok ? (await teamsRes.json()).data : [];
      const teams = teamsData.map((team: any) => ({
        id: team.repcard_team_id,
        name: team.team_name,
        officeId: team.office_id
      }));
      console.log('[AppointmentSchedule] Loaded teams:', teams.length);

      // Get calendars directly from calendars table
      const calendarsData = calendarsRes?.ok ? (await calendarsRes.json()).data : [];
      const calendars = calendarsData.map((calendar: any) => ({
        id: calendar.repcard_calendar_id,
        name: calendar.name
      }));
      console.log('[AppointmentSchedule] Loaded calendars:', calendars.length);

      return { offices, teams, calendars };
    },
    staleTime: 300000, // 5 minutes
    retry: 2
  });

  const appointments = data?.appointments || [];
  const isLoadingFilters = !filterData && ['office_leader', 'area_director', 'divisional', 'regional', 'super_admin'].includes(userRole);

  // Group appointments by date for calendar views
  const appointmentsByDate = useMemo(() => {
    const grouped = new Map<string, AppointmentData[]>();
    appointments.forEach(apt => {
      const date = apt.scheduled_at 
        ? new Date(apt.scheduled_at).toISOString().split('T')[0]
        : new Date(apt.created_at).toISOString().split('T')[0];
      if (!grouped.has(date)) {
        grouped.set(date, []);
      }
      grouped.get(date)!.push(apt);
    });
    return grouped;
  }, [appointments]);

  if (error) {
    return (
      <Card className="p-6">
        <div className="flex items-center gap-2 text-red-600">
          <AlertCircle className="h-5 w-5" />
          <span>Error loading appointments: {error instanceof Error ? error.message : 'Unknown error'}</span>
        </div>
        <Button onClick={() => refetch()} className="mt-4">
          <RefreshCw className="h-4 w-4 mr-2" />
          Retry
        </Button>
      </Card>
    );
  }

  return (
    <div className="space-y-6">
      {/* Metrics Cards */}
      <AppointmentMetrics
        userId={userId}
        userRole={userRole}
        queryParams={queryParams}
      />

      {/* Filters */}
      <AppointmentFilters
        filters={filters}
        onFiltersChange={setFilters}
        availableOffices={filterData?.offices || []}
        availableTeams={filterData?.teams || []}
        availableCalendars={filterData?.calendars || []}
        userRole={userRole}
        showAdvanced={showAdvanced}
        onToggleAdvanced={() => setShowAdvanced(!showAdvanced)}
      />

      {/* View Controls */}
      <Card className="p-4">
        <div className="flex items-center justify-between">
          <div className="flex items-center gap-2">
            <span className="text-sm font-medium">View:</span>
            <div className="flex gap-1">
              <Button
                variant={viewMode === 'list' ? 'default' : 'outline'}
                size="sm"
                onClick={() => setViewMode('list')}
              >
                <List className="h-4 w-4 mr-1" />
                List
              </Button>
              <Button
                variant={viewMode === 'day' ? 'default' : 'outline'}
                size="sm"
                onClick={() => setViewMode('day')}
              >
                Day
              </Button>
              <Button
                variant={viewMode === 'week' ? 'default' : 'outline'}
                size="sm"
                onClick={() => setViewMode('week')}
              >
                Week
              </Button>
              <Button
                variant={viewMode === 'month' ? 'default' : 'outline'}
                size="sm"
                onClick={() => setViewMode('month')}
              >
                <CalendarIcon className="h-4 w-4 mr-1" />
                Month
              </Button>
            </div>
          </div>
          <div className="flex items-center gap-2">
            <Badge variant="outline">
              {appointments.length} appointment{appointments.length !== 1 ? 's' : ''}
            </Badge>
            <Button
              variant="ghost"
              size="sm"
              onClick={() => refetch()}
              disabled={isLoading}
            >
              <RefreshCw className={cn("h-4 w-4", isLoading && "animate-spin")} />
            </Button>
          </div>
        </div>
      </Card>

      {/* Appointments List/View */}
      {isLoading ? (
        <div className="space-y-4">
          {[1, 2, 3].map(i => (
            <Card key={i} className="p-4">
              <Skeleton className="h-24 w-full" />
            </Card>
          ))}
        </div>
      ) : appointments.length === 0 ? (
        <Card className="p-8 text-center">
          <CalendarIcon className="h-12 w-12 mx-auto text-muted-foreground mb-4" />
          <p className="text-muted-foreground">No appointments found for the selected date range and filters.</p>
        </Card>
      ) : viewMode === 'list' ? (
        <div className="space-y-4">
          {appointments.map(appointment => (
            <AppointmentCard
              key={appointment.id}
              appointment={appointment}
              onClick={setSelectedAppointment}
            />
          ))}
        </div>
      ) : (
        <AppointmentCalendarView
          appointments={appointments}
          viewMode={viewMode}
          currentDate={calendarDate}
          onDateChange={setCalendarDate}
          onAppointmentClick={setSelectedAppointment}
        />
      )}

      {/* Appointment Detail Modal */}
      {selectedAppointment && (
        <AppointmentDetailModal
          appointment={selectedAppointment}
          open={!!selectedAppointment}
          onOpenChange={(open) => !open && setSelectedAppointment(null)}
        />
      )}
    </div>
  );
}
