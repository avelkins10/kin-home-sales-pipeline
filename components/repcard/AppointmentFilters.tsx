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

export interface AppointmentFilters {
  startDate: string;
  endDate: string;
  officeIds?: number[];
  teamIds?: number[];
  calendarId?: number;
  status?: string;
  hasPowerBill?: boolean;
  isReschedule?: boolean;
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
  const [localStartDate, setLocalStartDate] = useState(filters.startDate);
  const [localEndDate, setLocalEndDate] = useState(filters.endDate);

  const isLeader = ['office_leader', 'area_director', 'divisional', 'regional', 'super_admin'].includes(userRole);

  // Count active filters
  const activeFiltersCount = [
    filters.officeIds?.length,
    filters.teamIds?.length,
    filters.calendarId,
    filters.status,
    filters.hasPowerBill !== undefined,
    filters.isReschedule !== undefined,
  ].filter(Boolean).length;

  const handleDateChange = (field: 'startDate' | 'endDate', value: string) => {
    if (field === 'startDate') {
      setLocalStartDate(value);
      onFiltersChange({ ...filters, startDate: value });
    } else {
      setLocalEndDate(value);
      onFiltersChange({ ...filters, endDate: value });
    }
  };

  const handleQuickDate = (days: number) => {
    const today = new Date();
    today.setHours(0, 0, 0, 0);
    const endDate = new Date(today);
    endDate.setDate(endDate.getDate() + days);

    const startDateStr = today.toISOString().split('T')[0];
    const endDateStr = endDate.toISOString().split('T')[0];

    setLocalStartDate(startDateStr);
    setLocalEndDate(endDateStr);
    onFiltersChange({ ...filters, startDate: startDateStr, endDate: endDateStr });
  };

  const clearFilters = () => {
    const today = new Date();
    today.setHours(0, 0, 0, 0);
    const defaultEnd = new Date(today);
    defaultEnd.setDate(defaultEnd.getDate() + 7);

    const defaultStart = today.toISOString().split('T')[0];
    const defaultEndStr = defaultEnd.toISOString().split('T')[0];

    setLocalStartDate(defaultStart);
    setLocalEndDate(defaultEndStr);
    onFiltersChange({
      startDate: defaultStart,
      endDate: defaultEndStr,
      officeIds: undefined,
      teamIds: undefined,
      calendarId: undefined,
      status: undefined,
      hasPowerBill: undefined,
      isReschedule: undefined
    });
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

      {/* Date Range */}
      <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
        <div>
          <Label className="text-sm font-medium">Start Date</Label>
          <Input
            type="date"
            value={localStartDate}
            onChange={(e) => handleDateChange('startDate', e.target.value)}
            className="mt-1.5"
          />
        </div>
        <div>
          <Label className="text-sm font-medium">End Date</Label>
          <Input
            type="date"
            value={localEndDate}
            onChange={(e) => handleDateChange('endDate', e.target.value)}
            className="mt-1.5"
          />
        </div>
        <div>
          <Label className="text-sm font-medium">Quick Select</Label>
          <Select onValueChange={(value) => handleQuickDate(parseInt(value))}>
            <SelectTrigger className="mt-1.5">
              <Calendar className="mr-2 h-4 w-4" />
              <SelectValue placeholder="Choose range" />
            </SelectTrigger>
            <SelectContent>
              <SelectItem value="0">Today</SelectItem>
              <SelectItem value="1">Tomorrow</SelectItem>
              <SelectItem value="7">Next 7 Days</SelectItem>
              <SelectItem value="14">Next 14 Days</SelectItem>
              <SelectItem value="30">Next 30 Days</SelectItem>
            </SelectContent>
          </Select>
        </div>
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
