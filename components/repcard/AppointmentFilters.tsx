'use client';

import { useState } from 'react';
import { Card } from '@/components/ui/card';
import { Button } from '@/components/ui/button';
import { Label } from '@/components/ui/label';
import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from '@/components/ui/select';
import { Calendar, Filter, X, Building2, Users, CalendarDays } from 'lucide-react';
import { Input } from '@/components/ui/input';
import { Badge } from '@/components/ui/badge';

export type DateRangeOption = 'today' | 'tomorrow' | 'this_week' | 'next_7_days' | 'next_14_days' | 'this_month' | 'custom';

export interface AppointmentFilters {
  dateRange: DateRangeOption;
  customStartDate?: string;
  customEndDate?: string;
  officeIds?: number[];
  teamIds?: number[];
  calendarId?: number;
  status?: string;
  hasPowerBill?: boolean;
  isReschedule?: boolean;
}

// Helper to convert date range to actual dates
export function getDateRangeFromOption(option: DateRangeOption, customStart?: string, customEnd?: string): { startDate: string; endDate: string; viewMode: 'day' | 'week' | 'month'; currentDate: Date } {
  const today = new Date();
  today.setHours(0, 0, 0, 0);

  let startDate: Date;
  let endDate: Date;
  let viewMode: 'day' | 'week' | 'month' = 'week';
  let currentDate = new Date(today);

  switch (option) {
    case 'today':
      startDate = new Date(today);
      endDate = new Date(today);
      viewMode = 'day';
      currentDate = new Date(today);
      break;
    case 'tomorrow':
      startDate = new Date(today);
      startDate.setDate(startDate.getDate() + 1);
      endDate = new Date(startDate);
      viewMode = 'day';
      currentDate = new Date(startDate);
      break;
    case 'this_week':
      // Sunday to Saturday of current week
      const dayOfWeek = today.getDay();
      startDate = new Date(today);
      startDate.setDate(startDate.getDate() - dayOfWeek);
      endDate = new Date(startDate);
      endDate.setDate(endDate.getDate() + 6);
      viewMode = 'week';
      currentDate = new Date(today);
      break;
    case 'next_7_days':
      startDate = new Date(today);
      endDate = new Date(today);
      endDate.setDate(endDate.getDate() + 6);
      viewMode = 'week';
      currentDate = new Date(today);
      break;
    case 'next_14_days':
      startDate = new Date(today);
      endDate = new Date(today);
      endDate.setDate(endDate.getDate() + 13);
      viewMode = 'week';
      currentDate = new Date(today);
      break;
    case 'this_month':
      startDate = new Date(today.getFullYear(), today.getMonth(), 1);
      endDate = new Date(today.getFullYear(), today.getMonth() + 1, 0);
      viewMode = 'month';
      currentDate = new Date(today);
      break;
    case 'custom':
      if (customStart && customEnd) {
        startDate = new Date(customStart);
        endDate = new Date(customEnd);
        const daysDiff = Math.floor((endDate.getTime() - startDate.getTime()) / (1000 * 60 * 60 * 24));
        if (daysDiff === 0) {
          viewMode = 'day';
        } else if (daysDiff <= 7) {
          viewMode = 'week';
        } else {
          viewMode = 'month';
        }
        currentDate = new Date(startDate);
      } else {
        startDate = new Date(today);
        endDate = new Date(today);
        endDate.setDate(endDate.getDate() + 6);
        viewMode = 'week';
        currentDate = new Date(today);
      }
      break;
  }

  return {
    startDate: startDate.toISOString().split('T')[0],
    endDate: endDate.toISOString().split('T')[0],
    viewMode,
    currentDate
  };
}

interface AppointmentFiltersProps {
  filters: AppointmentFilters;
  onFiltersChange: (filters: AppointmentFilters) => void;
  availableOffices?: Array<{ id: number; name: string }>;
  availableTeams?: Array<{ id: number; name: string; officeId?: number }>;
  availableCalendars?: Array<{ id: number; name: string }>;
  userRole: string;
  showAdvanced?: boolean;
  onToggleAdvanced?: () => void;
}

export function AppointmentFilters({
  filters,
  onFiltersChange,
  availableOffices = [],
  availableTeams = [],
  availableCalendars = [],
  userRole,
  showAdvanced = false,
  onToggleAdvanced
}: AppointmentFiltersProps) {
  const [showCustomDates, setShowCustomDates] = useState(filters.dateRange === 'custom');

  const isLeader = ['office_leader', 'area_director', 'divisional', 'regional', 'super_admin'].includes(userRole);

  // Count active filters (excluding date range as it's always set)
  const activeFiltersCount = [
    filters.officeIds?.length,
    filters.teamIds?.length,
    filters.calendarId,
    filters.status,
    filters.hasPowerBill !== undefined,
    filters.isReschedule !== undefined,
  ].filter(Boolean).length;

  const handleDateRangeChange = (value: DateRangeOption) => {
    setShowCustomDates(value === 'custom');
    onFiltersChange({
      ...filters,
      dateRange: value,
      customStartDate: value === 'custom' ? filters.customStartDate : undefined,
      customEndDate: value === 'custom' ? filters.customEndDate : undefined
    });
  };

  const handleCustomDateChange = (field: 'start' | 'end', value: string) => {
    onFiltersChange({
      ...filters,
      dateRange: 'custom',
      customStartDate: field === 'start' ? value : filters.customStartDate,
      customEndDate: field === 'end' ? value : filters.customEndDate
    });
  };

  const clearFilters = () => {
    onFiltersChange({
      dateRange: 'next_7_days',
      customStartDate: undefined,
      customEndDate: undefined,
      officeIds: undefined,
      teamIds: undefined,
      calendarId: undefined,
      status: undefined,
      hasPowerBill: undefined,
      isReschedule: undefined
    });
    setShowCustomDates(false);
  };

  return (
    <Card className="p-4 space-y-4">
      <div className="flex items-center justify-between">
        <div className="flex items-center gap-2">
          <Filter className="h-5 w-5 text-blue-600" />
          <h3 className="font-semibold text-lg">Filters</h3>
          {activeFiltersCount > 0 && (
            <Badge variant="secondary" className="ml-1">
              {activeFiltersCount} active
            </Badge>
          )}
        </div>
        <div className="flex items-center gap-2">
          {onToggleAdvanced && (
            <Button
              variant="ghost"
              size="sm"
              onClick={onToggleAdvanced}
            >
              {showAdvanced ? 'Less' : 'More'} Options
            </Button>
          )}
          {activeFiltersCount > 0 && (
            <Button
              variant="outline"
              size="sm"
              onClick={clearFilters}
            >
              <X className="h-4 w-4 mr-1" />
              Clear All
            </Button>
          )}
        </div>
      </div>

      {/* Single Date Range Selector */}
      <div className="space-y-3">
        <div>
          <Label className="text-sm font-medium flex items-center gap-2">
            <Calendar className="h-4 w-4" />
            Date Range
          </Label>
          <Select value={filters.dateRange} onValueChange={(value) => handleDateRangeChange(value as DateRangeOption)}>
            <SelectTrigger className="mt-1.5">
              <SelectValue />
            </SelectTrigger>
            <SelectContent>
              <SelectItem value="today">Today</SelectItem>
              <SelectItem value="tomorrow">Tomorrow</SelectItem>
              <SelectItem value="this_week">This Week</SelectItem>
              <SelectItem value="next_7_days">Next 7 Days</SelectItem>
              <SelectItem value="next_14_days">Next 14 Days</SelectItem>
              <SelectItem value="this_month">This Month</SelectItem>
              <SelectItem value="custom">Custom Range...</SelectItem>
            </SelectContent>
          </Select>
        </div>

        {/* Custom Date Inputs (show when Custom selected) */}
        {showCustomDates && (
          <div className="grid grid-cols-2 gap-3 pl-6 border-l-2 border-blue-200">
            <div>
              <Label className="text-xs font-medium text-gray-600">From</Label>
              <Input
                type="date"
                value={filters.customStartDate || ''}
                onChange={(e) => handleCustomDateChange('start', e.target.value)}
                className="mt-1"
              />
            </div>
            <div>
              <Label className="text-xs font-medium text-gray-600">To</Label>
              <Input
                type="date"
                value={filters.customEndDate || ''}
                onChange={(e) => handleCustomDateChange('end', e.target.value)}
                className="mt-1"
              />
            </div>
          </div>
        )}
      </div>

      {/* Primary Filters - Office, Team, Calendar */}
      {(isLeader || availableCalendars.length > 0) && (
        <div className="grid grid-cols-1 md:grid-cols-3 gap-4 pt-2">
          {/* Office Filter (for leaders) */}
          {isLeader && (
            <div>
              <Label className="text-sm font-medium flex items-center gap-1.5">
                <Building2 className="h-3.5 w-3.5" />
                Office
              </Label>
              <Select
                value={filters.officeIds?.join(',') || 'all'}
                onValueChange={(value) => {
                  if (value === 'all') {
                    onFiltersChange({ ...filters, officeIds: undefined });
                  } else {
                    onFiltersChange({ ...filters, officeIds: value.split(',').map(id => parseInt(id)) });
                  }
                }}
                disabled={availableOffices.length === 0}
              >
                <SelectTrigger className="mt-1.5">
                  <SelectValue placeholder={availableOffices.length === 0 ? "Loading offices..." : "All Offices"} />
                </SelectTrigger>
                <SelectContent>
                  <SelectItem value="all">All Offices</SelectItem>
                  {availableOffices.map(office => (
                    <SelectItem key={office.id} value={office.id.toString()}>
                      {office.name}
                    </SelectItem>
                  ))}
                </SelectContent>
              </Select>
            </div>
          )}

          {/* Team Filter */}
          {isLeader && (
            <div>
              <Label className="text-sm font-medium flex items-center gap-1.5">
                <Users className="h-3.5 w-3.5" />
                Team
              </Label>
              <Select
                value={filters.teamIds?.join(',') || 'all'}
                onValueChange={(value) => {
                  if (value === 'all') {
                    onFiltersChange({ ...filters, teamIds: undefined });
                  } else {
                    onFiltersChange({ ...filters, teamIds: value.split(',').map(id => parseInt(id)) });
                  }
                }}
                disabled={availableTeams.length === 0}
              >
                <SelectTrigger className="mt-1.5">
                  <SelectValue placeholder={availableTeams.length === 0 ? "Loading teams..." : "All Teams"} />
                </SelectTrigger>
                <SelectContent>
                  <SelectItem value="all">All Teams</SelectItem>
                  {availableTeams.map(team => (
                    <SelectItem key={team.id} value={team.id.toString()}>
                      {team.name}
                    </SelectItem>
                  ))}
                </SelectContent>
              </Select>
            </div>
          )}

          {/* Calendar Filter - Show for everyone */}
          <div>
            <Label className="text-sm font-medium flex items-center gap-1.5">
              <CalendarDays className="h-3.5 w-3.5" />
              Calendar
            </Label>
            <Select
              value={filters.calendarId?.toString() || 'all'}
              onValueChange={(value) => {
                if (value === 'all') {
                  onFiltersChange({ ...filters, calendarId: undefined });
                } else {
                  onFiltersChange({ ...filters, calendarId: parseInt(value) });
                }
              }}
              disabled={availableCalendars.length === 0}
            >
              <SelectTrigger className="mt-1.5">
                <SelectValue placeholder={availableCalendars.length === 0 ? "Loading calendars..." : "All Calendars"} />
              </SelectTrigger>
              <SelectContent>
                <SelectItem value="all">All Calendars</SelectItem>
                {availableCalendars.map(calendar => (
                  <SelectItem key={calendar.id} value={calendar.id.toString()}>
                    {calendar.name}
                  </SelectItem>
                ))}
              </SelectContent>
            </Select>
          </div>
        </div>
      )}

      {/* Advanced Filters */}
      {showAdvanced && (
        <div className="grid grid-cols-1 md:grid-cols-3 gap-4 pt-4 border-t">
          {/* Status Filter */}
          <div>
            <Label className="text-sm font-medium">Status</Label>
            <Select
              value={filters.status || 'all'}
              onValueChange={(value) => {
                onFiltersChange({ ...filters, status: value === 'all' ? undefined : value });
              }}
            >
              <SelectTrigger className="mt-1.5">
                <SelectValue placeholder="All Statuses" />
              </SelectTrigger>
              <SelectContent>
                <SelectItem value="all">All Statuses</SelectItem>
                <SelectItem value="scheduled">Scheduled</SelectItem>
                <SelectItem value="completed">Completed</SelectItem>
                <SelectItem value="cancelled">Cancelled</SelectItem>
                <SelectItem value="rescheduled">Rescheduled</SelectItem>
                <SelectItem value="no_show">No Show</SelectItem>
              </SelectContent>
            </Select>
          </div>

          {/* Power Bill Filter */}
          <div>
            <Label className="text-sm font-medium">Power Bill</Label>
            <Select
              value={filters.hasPowerBill === undefined ? 'all' : filters.hasPowerBill ? 'yes' : 'no'}
              onValueChange={(value) => {
                onFiltersChange({
                  ...filters,
                  hasPowerBill: value === 'all' ? undefined : value === 'yes'
                });
              }}
            >
              <SelectTrigger className="mt-1.5">
                <SelectValue placeholder="All" />
              </SelectTrigger>
              <SelectContent>
                <SelectItem value="all">All</SelectItem>
                <SelectItem value="yes">With Power Bill</SelectItem>
                <SelectItem value="no">Without Power Bill</SelectItem>
              </SelectContent>
            </Select>
          </div>

          {/* Reschedule Filter */}
          <div>
            <Label className="text-sm font-medium">Reschedule Status</Label>
            <Select
              value={filters.isReschedule === undefined ? 'all' : filters.isReschedule ? 'yes' : 'no'}
              onValueChange={(value) => {
                onFiltersChange({
                  ...filters,
                  isReschedule: value === 'all' ? undefined : value === 'yes'
                });
              }}
            >
              <SelectTrigger className="mt-1.5">
                <SelectValue placeholder="All" />
              </SelectTrigger>
              <SelectContent>
                <SelectItem value="all">All</SelectItem>
                <SelectItem value="yes">Reschedules Only</SelectItem>
                <SelectItem value="no">Original Only</SelectItem>
              </SelectContent>
            </Select>
          </div>
        </div>
      )}
    </Card>
  );
}
