'use client';

import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card';
import { PCResponseBreakdown } from '@/lib/types/operations';
import { PieChart as PieChartIcon } from 'lucide-react';
import {
  PieChart,
  Pie,
  Cell,
  ResponsiveContainer,
  Tooltip
} from 'recharts';

interface PCResponseRatesChartProps {
  data: PCResponseBreakdown[];
  title?: string;
  height?: number;
}

const CustomTooltip = ({ active, payload }: any) => {
  if (active && payload && payload.length) {
    const data = payload[0].payload;
    return (
      <div className="bg-white p-3 border border-gray-200 rounded-lg shadow-lg">
        <p className="font-medium text-gray-900">{data.status}</p>
        <div className="space-y-1 mt-2">
          <p className="text-sm">
            <span className="text-gray-600 font-medium">Count:</span> {data.count}
          </p>
          <p className="text-sm">
            <span className="text-gray-600 font-medium">Percentage:</span> {data.percentage.toFixed(1)}%
          </p>
        </div>
      </div>
    );
  }
  return null;
};

const getStatusColor = (status: string) => {
  switch (status) {
    case 'Complete':
      return '#10b981'; // Green
    case 'No Answer Left Message':
      return '#f59e0b'; // Orange
    case 'Complete - No Answer':
      return '#eab308'; // Yellow
    case 'Pending':
      return '#3b82f6'; // Blue
    case 'Failed':
      return '#ef4444'; // Red
    case 'Scheduled':
      return '#8b5cf6'; // Purple
    case 'Cancelled':
      return '#6b7280'; // Gray
    case 'Unknown':
      return '#9ca3af'; // Light Gray
    default:
      return '#6b7280'; // Gray - sensible default for unknown statuses
  }
};

export function PCResponseRatesChart({ 
  data, 
  title = 'Outreach Response Rates',
  height = 300 
}: PCResponseRatesChartProps) {
  if (!data || data.length === 0) {
    return (
      <Card className="h-64 mobile:h-72 ipad:h-80">
        <CardHeader>
          <CardTitle className="flex items-center gap-2">
            <PieChartIcon className="h-5 w-5 text-gray-400" />
            {title}
          </CardTitle>
        </CardHeader>
        <CardContent className="flex items-center justify-center h-full">
          <div className="text-center text-gray-500">
            <PieChartIcon className="h-8 w-8 mx-auto mb-2 text-gray-300" />
            <p>No response data available</p>
          </div>
        </CardContent>
      </Card>
    );
  }

  const totalOutreach = data.reduce((sum, item) => sum + item.count, 0);

  return (
    <Card className="shadow-sm hover:shadow-md transition-shadow">
      <CardHeader>
        <CardTitle className="flex items-center gap-2">
          <PieChartIcon className="h-5 w-5 text-blue-600" />
          {title}
        </CardTitle>
        <p className="text-sm text-gray-600">
          {totalOutreach} total outreach attempts
        </p>
      </CardHeader>
      <CardContent>
        <div className="flex flex-col md:flex-row gap-4">
          {/* Left section - Pie Chart */}
          <div className="flex-1">
            <ResponsiveContainer width="100%" height={height}>
              <PieChart>
                <Pie
                  data={data}
                  cx="50%"
                  cy="50%"
                  outerRadius={80}
                  dataKey="count"
                  nameKey="status"
                  label={(props) => `${(props.percent * 100).toFixed(1)}%`}
                >
                  {data.map((entry, index) => (
                    <Cell key={`cell-${index}`} fill={getStatusColor(entry.status)} />
                  ))}
                </Pie>
                <Tooltip content={<CustomTooltip />} />
              </PieChart>
            </ResponsiveContainer>
          </div>
          
          {/* Right section - Legend */}
          <div className="flex-1 space-y-2">
            <h4 className="font-medium text-gray-900 text-sm">Response Breakdown</h4>
            <div className="space-y-2">
              {data.map((item, index) => (
                <div key={index} className="flex items-center gap-2">
                  <div 
                    className="w-3 h-3 rounded-full" 
                    style={{ backgroundColor: getStatusColor(item.status) }}
                  />
                  <div className="flex-1">
                    <p className="text-sm font-medium text-gray-900">{item.status}</p>
                    <p className="text-xs text-gray-600">
                      {item.count} ({item.percentage.toFixed(1)}%)
                    </p>
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
