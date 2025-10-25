'use client';

import { useQuery } from '@tanstack/react-query';
import { Card, CardHeader, CardTitle, CardContent } from '@/components/ui/card';
import { Skeleton } from '@/components/ui/skeleton';
import { Badge } from '@/components/ui/badge';
import { TrendingUp, Calendar } from 'lucide-react';
import { formatLargeNumber } from '@/lib/utils/formatters';
import { getBaseUrl } from '@/lib/utils/baseUrl';
import { TimeRange, CustomDateRange } from '@/lib/types/dashboard';
import { LineChart, Line, XAxis, YAxis, CartesianGrid, Tooltip, Legend, ResponsiveContainer } from 'recharts';

interface DoorsKnockedTrendsCardProps {
  userId: string;
  role: string;
  timeRange: TimeRange;
  customDateRange?: CustomDateRange;
  officeIds: number[];
}

// Generate mock trend data based on time range
const generateMockTrendData = (timeRange: TimeRange) => {
  const data = [];
  const now = new Date();
  
  if (timeRange === 'week') {
    // 7 days of data
    for (let i = 6; i >= 0; i--) {
      const date = new Date(now);
      date.setDate(date.getDate() - i);
      const dayOfWeek = date.getDay();
      
      // Higher activity on weekdays (Mon-Fri), lower on weekends
      const baseActivity = dayOfWeek >= 1 && dayOfWeek <= 5 ? 80 : 30;
      const randomVariation = Math.random() * 40 - 20; // -20 to +20
      const doors = Math.max(0, Math.round(baseActivity + randomVariation));
      
      data.push({
        date: date.toLocaleDateString('en-US', { month: 'short', day: 'numeric' }),
        doors
      });
    }
  } else if (timeRange === 'month') {
    // 30 days of data
    for (let i = 29; i >= 0; i--) {
      const date = new Date(now);
      date.setDate(date.getDate() - i);
      const dayOfWeek = date.getDay();
      
      const baseActivity = dayOfWeek >= 1 && dayOfWeek <= 5 ? 75 : 25;
      const randomVariation = Math.random() * 50 - 25;
      const doors = Math.max(0, Math.round(baseActivity + randomVariation));
      
      data.push({
        date: date.toLocaleDateString('en-US', { month: 'short', day: 'numeric' }),
        doors
      });
    }
  } else if (timeRange === 'quarter') {
    // 13 weeks of data
    for (let i = 12; i >= 0; i--) {
      const weekStart = new Date(now);
      weekStart.setDate(weekStart.getDate() - (i * 7));
      
      const baseActivity = 400; // Weekly average
      const randomVariation = Math.random() * 200 - 100;
      const doors = Math.max(0, Math.round(baseActivity + randomVariation));
      
      data.push({
        date: `Week ${13 - i}`,
        doors
      });
    }
  } else {
    // Default to week
    for (let i = 6; i >= 0; i--) {
      const date = new Date(now);
      date.setDate(date.getDate() - i);
      const doors = Math.round(50 + Math.random() * 100);
      data.push({
        date: date.toLocaleDateString('en-US', { month: 'short', day: 'numeric' }),
        doors
      });
    }
  }
  
  return data;
};

export function DoorsKnockedTrendsCard({
  userId,
  role,
  timeRange,
  customDateRange,
  officeIds
}: DoorsKnockedTrendsCardProps) {
  // TODO: Replace mock data with real time-series data from RepCard API
  const { data: trendData, isLoading, error } = useQuery({
    queryKey: ['doors-knocked-trends', timeRange, customDateRange, officeIds],
    queryFn: async () => {
      // For now, return mock data
      // Future: Fetch from /api/repcard/canvassing/trends
      return { data: generateMockTrendData(timeRange) };
    },
    enabled: officeIds.length > 0
  });

  if (error) {
    return (
      <Card>
        <CardHeader>
          <CardTitle className="flex items-center gap-2">
            <TrendingUp className="h-5 w-5 text-purple-600" />
            Doors Knocked Trends
          </CardTitle>
        </CardHeader>
        <CardContent>
          <div className="text-center py-8">
            <p className="text-red-600 mb-4">Failed to load trend data</p>
            <p className="text-sm text-gray-500">{error.message}</p>
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
            <TrendingUp className="h-5 w-5 text-purple-600" />
            Doors Knocked Trends
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
            <TrendingUp className="h-5 w-5 text-purple-600" />
            Doors Knocked Trends
          </CardTitle>
          <Badge variant="secondary" className="bg-yellow-100 text-yellow-800">
            Coming Soon
          </Badge>
        </div>
        <p className="text-sm text-gray-600">
          Daily door knocking activity over selected time period
        </p>
      </CardHeader>
      <CardContent>
        <div className="h-[300px] w-full">
          <ResponsiveContainer width="100%" height="100%">
            <LineChart data={trendData?.data || []}>
              <CartesianGrid strokeDasharray="3 3" stroke="#f0f0f0" />
              <XAxis 
                dataKey="date" 
                tick={{ fontSize: 12 }}
                stroke="#666"
              />
              <YAxis 
                tick={{ fontSize: 12 }}
                stroke="#666"
              />
              <Tooltip 
                formatter={(value: number) => [formatLargeNumber(value), 'Doors']}
                labelStyle={{ color: '#333' }}
                contentStyle={{ 
                  backgroundColor: '#fff', 
                  border: '1px solid #e5e7eb',
                  borderRadius: '6px'
                }}
              />
              <Legend />
              <Line 
                type="monotone" 
                dataKey="doors" 
                stroke="#8b5cf6" 
                strokeWidth={2}
                dot={{ fill: '#8b5cf6', strokeWidth: 2, r: 4 }}
                activeDot={{ r: 6, stroke: '#8b5cf6', strokeWidth: 2 }}
              />
            </LineChart>
          </ResponsiveContainer>
        </div>
        <div className="mt-4 p-3 bg-yellow-50 border border-yellow-200 rounded-lg">
          <p className="text-sm text-yellow-800">
            <strong>Note:</strong> This is placeholder data. Full implementation requires time-series data from RepCard API.
          </p>
        </div>
      </CardContent>
    </Card>
  );
}
