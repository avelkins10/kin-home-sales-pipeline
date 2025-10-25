'use client';

import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card';
import { PCOutreachTrendData } from '@/lib/types/operations';
import { TrendingUp } from 'lucide-react';
import {
  LineChart,
  Line,
  XAxis,
  YAxis,
  CartesianGrid,
  Tooltip,
  Legend,
  ResponsiveContainer
} from 'recharts';

interface PCOutreachTrendChartProps {
  data: PCOutreachTrendData[];
  title?: string;
  height?: number;
}

const CustomTooltip = ({ active, payload, label }: any) => {
  if (active && payload && payload.length) {
    const data = payload[0].payload;
    return (
      <div className="bg-white p-3 border border-gray-200 rounded-lg shadow-lg">
        <p className="font-medium text-gray-900">
          {new Date(label).toLocaleDateString('en-US', { 
            month: 'short', 
            day: 'numeric', 
            year: 'numeric' 
          })}
        </p>
        <div className="space-y-1 mt-2">
          <p className="text-sm">
            <span className="text-blue-600 font-medium">Total Outreach:</span> {data.outreachCount}
          </p>
          <p className="text-sm">
            <span className="text-green-600 font-medium">Successful:</span> {data.successfulCount}
          </p>
          <p className="text-sm">
            <span className="text-gray-600 font-medium">Response Rate:</span> {data.responseRate.toFixed(1)}%
          </p>
        </div>
      </div>
    );
  }
  return null;
};

export function PCOutreachTrendChart({ 
  data, 
  title = 'Outreach Trend',
  height = 300 
}: PCOutreachTrendChartProps) {
  if (!data || data.length === 0) {
    return (
      <Card className="h-64 mobile:h-72 ipad:h-80">
        <CardHeader>
          <CardTitle className="flex items-center gap-2">
            <TrendingUp className="h-5 w-5 text-gray-400" />
            {title}
          </CardTitle>
        </CardHeader>
        <CardContent className="flex items-center justify-center h-full">
          <div className="text-center text-gray-500">
            <TrendingUp className="h-8 w-8 mx-auto mb-2 text-gray-300" />
            <p>No outreach data available for this period</p>
          </div>
        </CardContent>
      </Card>
    );
  }

  return (
    <Card className="shadow-sm hover:shadow-md transition-shadow">
      <CardHeader>
        <CardTitle className="flex items-center gap-2">
          <TrendingUp className="h-5 w-5 text-blue-600" />
          {title}
        </CardTitle>
        <p className="text-sm text-gray-600">
          Last 30 days of outreach activity
        </p>
      </CardHeader>
      <CardContent>
        <ResponsiveContainer width="100%" height={height}>
          <LineChart data={data} margin={{ top: 5, right: 30, left: 20, bottom: 5 }}>
            <CartesianGrid strokeDasharray="3 3" stroke="#e5e7eb" />
            <XAxis 
              dataKey="date" 
              tickFormatter={(value) => {
                const date = new Date(value);
                return date.toLocaleDateString('en-US', { month: 'short', day: 'numeric' });
              }}
              tick={{ fontSize: 12 }}
            />
            <YAxis 
              tickFormatter={(value) => value.toString()}
              tick={{ fontSize: 12 }}
            />
            <Tooltip content={<CustomTooltip />} />
            <Legend />
            <Line 
              type="monotone" 
              dataKey="outreachCount" 
              stroke="#3b82f6" 
              strokeWidth={2}
              name="Total Outreach"
              dot={{ r: 4 }}
              activeDot={{ r: 6 }}
            />
            <Line 
              type="monotone" 
              dataKey="successfulCount" 
              stroke="#10b981" 
              strokeWidth={2}
              name="Successful Contacts"
              dot={{ r: 4 }}
              activeDot={{ r: 6 }}
            />
          </LineChart>
        </ResponsiveContainer>
      </CardContent>
    </Card>
  );
}
