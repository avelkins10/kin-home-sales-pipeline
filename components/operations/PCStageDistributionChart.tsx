'use client';

import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card';
import { PCStageDistribution } from '@/lib/types/operations';
import { BarChart3 } from 'lucide-react';
import {
  BarChart,
  Bar,
  Cell,
  XAxis,
  YAxis,
  CartesianGrid,
  Tooltip,
  ResponsiveContainer
} from 'recharts';

interface PCStageDistributionChartProps {
  data: PCStageDistribution[];
  title?: string;
  height?: number;
}

const CustomTooltip = ({ active, payload, label }: any) => {
  if (active && payload && payload.length) {
    const data = payload[0].payload;
    return (
      <div className="bg-white p-3 border border-gray-200 rounded-lg shadow-lg">
        <p className="font-medium text-gray-900">{label}</p>
        <div className="space-y-1 mt-2">
          <p className="text-sm">
            <span className="text-gray-600 font-medium">Projects:</span> {data.projectCount}
          </p>
          <p className="text-sm">
            <span className="text-gray-600 font-medium">Avg Days:</span> {data.avgDaysInStage.toFixed(1)}
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

const getBarColor = (avgDays: number) => {
  if (avgDays < 30) return '#10b981'; // Green - healthy
  if (avgDays < 60) return '#f59e0b'; // Yellow - attention needed
  return '#ef4444'; // Red - bottleneck
};

export function PCStageDistributionChart({ 
  data, 
  title = 'Project Stage Distribution',
  height = 300 
}: PCStageDistributionChartProps) {
  if (!data || data.length === 0) {
    return (
      <Card className="h-64 mobile:h-72 ipad:h-80">
        <CardHeader>
          <CardTitle className="flex items-center gap-2">
            <BarChart3 className="h-5 w-5 text-gray-400" />
            {title}
          </CardTitle>
        </CardHeader>
        <CardContent className="flex items-center justify-center h-full">
          <div className="text-center text-gray-500">
            <BarChart3 className="h-8 w-8 mx-auto mb-2 text-gray-300" />
            <p>No project stage data available</p>
          </div>
        </CardContent>
      </Card>
    );
  }

  const totalProjects = data.reduce((sum, stage) => sum + stage.projectCount, 0);

  return (
    <Card className="shadow-sm hover:shadow-md transition-shadow">
      <CardHeader>
        <CardTitle className="flex items-center gap-2">
          <BarChart3 className="h-5 w-5 text-blue-600" />
          {title}
        </CardTitle>
        <p className="text-sm text-gray-600">
          {totalProjects} total projects across all stages
        </p>
      </CardHeader>
      <CardContent>
        <ResponsiveContainer width="100%" height={height}>
          <BarChart 
            data={data} 
            margin={{ top: 20, right: 30, left: 20, bottom: 80 }}
          >
            <CartesianGrid strokeDasharray="3 3" stroke="#e5e7eb" />
            <XAxis 
              dataKey="stageName" 
              angle={-45}
              textAnchor="end"
              height={80}
              tick={{ fontSize: 12 }}
            />
            <YAxis 
              tickFormatter={(value) => value.toString()}
              tick={{ fontSize: 12 }}
            />
            <Tooltip content={<CustomTooltip />} />
            <Bar 
              dataKey="projectCount" 
              radius={[8, 8, 0, 0]}
              label={{ position: 'top', formatter: (value: number) => `${value}` }}
            >
              {data.map((entry, index) => (
                <Cell 
                  key={`cell-${index}`} 
                  fill={getBarColor(entry.avgDaysInStage)} 
                />
              ))}
            </Bar>
          </BarChart>
        </ResponsiveContainer>
      </CardContent>
    </Card>
  );
}
