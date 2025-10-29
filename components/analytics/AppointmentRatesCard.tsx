'use client';

import { useState } from 'react';
import { useQuery } from '@tanstack/react-query';
import { Card, CardHeader, CardTitle, CardContent } from '@/components/ui/card';
import { Skeleton } from '@/components/ui/skeleton';
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from '@/components/ui/select';
import { BarChart3, TrendingUp } from 'lucide-react';
import { formatPercentage } from '@/lib/utils/formatters';
import { getBaseUrl } from '@/lib/utils/baseUrl';
import { TimeRange, CustomDateRange } from '@/lib/types/dashboard';
import { BarChart, Bar, XAxis, YAxis, CartesianGrid, Tooltip, Legend, ResponsiveContainer, ReferenceLine } from 'recharts';

interface AppointmentRatesCardProps {
  userId: string;
  role: string;
  timeRange: TimeRange;
  customDateRange?: CustomDateRange;
  officeIds: number[];
}

export function AppointmentRatesCard({
  userId,
  role,
  timeRange,
  customDateRange,
  officeIds
}: AppointmentRatesCardProps) {
  const [groupBy, setGroupBy] = useState<'office' | 'rep'>('office');

  // Fetch doors knocked data
  const { data: doorsData, isLoading: doorsLoading, error: doorsError } = useQuery({
    queryKey: ['appointment-rates', 'doors', timeRange, customDateRange, officeIds],
    queryFn: async () => {
      const params = new URLSearchParams({
        metric: 'doors_knocked',
        timeRange
      });
      
      if (officeIds.length > 0) {
        params.append('officeIds', officeIds.join(','));
      }
      
      if (customDateRange) {
        params.set('startDate', customDateRange.startDate);
        params.set('endDate', customDateRange.endDate);
      }

      const response = await fetch(`${getBaseUrl()}/api/repcard/leaderboard?${params}`);
      if (!response.ok) throw new Error('Failed to fetch doors data');
      return response.json();
    },
    refetchInterval: 30000, // Auto-refresh every 30 seconds
    staleTime: 60000
  });

  // Fetch appointments set data
  const { data: appointmentsData, isLoading: appointmentsLoading, error: appointmentsError } = useQuery({
    queryKey: ['appointment-rates', 'appointments', timeRange, customDateRange, officeIds],
    queryFn: async () => {
      const params = new URLSearchParams({
        metric: 'appointments_set',
        timeRange
      });
      
      if (officeIds.length > 0) {
        params.append('officeIds', officeIds.join(','));
      }
      
      if (customDateRange) {
        params.set('startDate', customDateRange.startDate);
        params.set('endDate', customDateRange.endDate);
      }

      const response = await fetch(`${getBaseUrl()}/api/repcard/leaderboard?${params}`);
      if (!response.ok) throw new Error('Failed to fetch appointments data');
      return response.json();
    },
    refetchInterval: 30000, // Auto-refresh every 30 seconds
    staleTime: 60000
  });

  const isLoading = doorsLoading || appointmentsLoading;
  const hasError = doorsError || appointmentsError;

  // Calculate appointment rates
  const calculateRates = () => {
    if (!doorsData?.leaderboard || !appointmentsData?.leaderboard) return [];

    const ratesMap = new Map();

    // Group by office or rep
    doorsData.leaderboard.forEach((doorsEntry: any) => {
      const key = groupBy === 'office' ? doorsEntry.office : doorsEntry.userId;
      if (!ratesMap.has(key)) {
        ratesMap.set(key, { 
          id: key, 
          name: groupBy === 'office' ? doorsEntry.office : doorsEntry.userName, 
          doors: 0, 
          appointments: 0 
        });
      }
      ratesMap.get(key).doors += doorsEntry.metricValue || 0;
    });

    appointmentsData.leaderboard.forEach((appointmentsEntry: any) => {
      const key = groupBy === 'office' ? appointmentsEntry.office : appointmentsEntry.userId;
      if (!ratesMap.has(key)) {
        ratesMap.set(key, { 
          id: key, 
          name: groupBy === 'office' ? appointmentsEntry.office : appointmentsEntry.userName, 
          doors: 0, 
          appointments: 0 
        });
      }
      ratesMap.get(key).appointments += appointmentsEntry.metricValue || 0;
    });

    // Calculate rates and sort
    const rates = Array.from(ratesMap.values())
      .map(item => ({
        ...item,
        rate: item.doors > 0 ? (item.appointments / item.doors) * 100 : 0
      }))
      .sort((a, b) => b.rate - a.rate)
      .slice(0, 10); // Top 10

    return rates;
  };

  const ratesData = calculateRates();

  if (hasError) {
    return (
      <Card>
        <CardHeader>
          <CardTitle className="flex items-center gap-2">
            <BarChart3 className="h-5 w-5 text-blue-600" />
            Appointment Set Rates
          </CardTitle>
        </CardHeader>
        <CardContent>
          <div className="text-center py-8">
            <p className="text-red-600 mb-4">Failed to load appointment rate data</p>
            <p className="text-sm text-gray-500">
              {doorsError?.message || appointmentsError?.message}
            </p>
          </div>
        </CardContent>
      </Card>
    );
  }

  if (isLoading) {
    return (
      <Card>
        <CardHeader>
          <CardTitle className="flex items-center gap-2">
            <BarChart3 className="h-5 w-5 text-blue-600" />
            Appointment Set Rates
          </CardTitle>
        </CardHeader>
        <CardContent>
          <Skeleton className="h-[300px] w-full" />
        </CardContent>
      </Card>
    );
  }

  return (
    <Card>
      <CardHeader>
        <div className="flex items-center justify-between">
          <CardTitle className="flex items-center gap-2">
            <BarChart3 className="h-5 w-5 text-blue-600" />
            Appointment Set Rates
          </CardTitle>
          <Select value={groupBy} onValueChange={(value: 'office' | 'rep') => setGroupBy(value)}>
            <SelectTrigger className="w-32">
              <SelectValue />
            </SelectTrigger>
            <SelectContent>
              <SelectItem value="office">By Office</SelectItem>
              <SelectItem value="rep">By Rep</SelectItem>
            </SelectContent>
          </Select>
        </div>
        <p className="text-sm text-gray-600">
          Percentage of doors knocked that resulted in appointments
        </p>
      </CardHeader>
      <CardContent>
        <div className="h-[300px] w-full">
          <ResponsiveContainer width="100%" height="100%">
            <BarChart data={ratesData}>
              <CartesianGrid strokeDasharray="3 3" stroke="#f0f0f0" />
              <XAxis 
                dataKey="name" 
                tick={{ fontSize: 12 }}
                stroke="#666"
                angle={-45}
                textAnchor="end"
                height={60}
              />
              <YAxis 
                tick={{ fontSize: 12 }}
                stroke="#666"
                domain={[0, 100]}
              />
              <Tooltip 
                formatter={(value: number, name: string, props: any) => {
                  if (name === 'rate') {
                    return [
                      formatPercentage(value),
                      'Rate'
                    ];
                  }
                  return [value, name];
                }}
                labelFormatter={(label: string, payload: any[]) => {
                  if (payload && payload.length > 0) {
                    const data = payload[0].payload;
                    return `${label}\nDoors: ${data.doors}\nAppointments: ${data.appointments}\nRate: ${formatPercentage(data.rate)}`;
                  }
                  return label;
                }}
                labelStyle={{ color: '#333' }}
                contentStyle={{ 
                  backgroundColor: '#fff', 
                  border: '1px solid #e5e7eb',
                  borderRadius: '6px'
                }}
              />
              <Legend />
              <ReferenceLine y={20} stroke="#10b981" strokeDasharray="5 5" label="Target: 20%" />
              <Bar 
                dataKey="rate" 
                fill="#3b82f6" 
                radius={[2, 2, 0, 0]}
              />
            </BarChart>
          </ResponsiveContainer>
        </div>
        <div className="mt-4 p-3 bg-green-50 border border-green-200 rounded-lg">
          <p className="text-sm text-green-800">
            <strong>Target rate:</strong> 20% (industry standard)
          </p>
        </div>
      </CardContent>
    </Card>
  );
}
