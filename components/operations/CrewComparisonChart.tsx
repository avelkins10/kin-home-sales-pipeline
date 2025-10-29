'use client';

import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card';
import { BarChart, Bar, XAxis, YAxis, CartesianGrid, Tooltip, ResponsiveContainer, ReferenceLine, Cell } from 'recharts';
import { BarChart3 } from 'lucide-react';
import { CrewPerformanceMetrics } from '@/lib/types/operations';

interface CrewComparisonChartProps {
  crewMetrics: CrewPerformanceMetrics[];
  teamAverage: number;
  metric: 'tasks_completed' | 'completion_time' | 'on_time' | 'ratings';
  title?: string;
  height?: number;
}

export function CrewComparisonChart({
  crewMetrics,
  teamAverage,
  metric,
  title,
  height = 400
}: CrewComparisonChartProps) {
  // Metric configuration
  const metricConfig = {
    tasks_completed: {
      label: 'Tasks Completed',
      dataKey: 'tasks_completed_month',
      formatter: (value: number) => value.toString(),
      color: '#3b82f6'
    },
    completion_time: {
      label: 'Avg Completion Time (min)',
      dataKey: 'avg_completion_time_minutes',
      formatter: (value: number) => {
        const hours = Math.floor(value / 60);
        const mins = Math.round(value % 60);
        return hours > 0 ? `${hours}h ${mins}m` : `${mins}m`;
      },
      color: '#8b5cf6'
    },
    on_time: {
      label: 'On-Time Percentage',
      dataKey: 'on_time_percentage',
      formatter: (value: number) => `${value.toFixed(1)}%`,
      color: '#10b981'
    },
    ratings: {
      label: 'Customer Rating',
      dataKey: 'customer_rating_avg',
      formatter: (value: number) => value.toFixed(1),
      color: '#f59e0b'
    }
  };

  const config = metricConfig[metric];

  // Transform data for chart
  const chartData = crewMetrics
    .map(crew => {
      let value = 0;
      if (metric === 'tasks_completed') {
        value = crew.tasks_completed_month;
      } else if (metric === 'completion_time') {
        value = crew.avg_completion_time_minutes || 0;
      } else if (metric === 'on_time') {
        value = crew.on_time_percentage;
      } else if (metric === 'ratings') {
        value = crew.customer_rating_avg || 0;
      }

      return {
        crew_name: crew.crew_name,
        value,
        delta: value - teamAverage
      };
    })
    .filter(item => item.value > 0) // Filter out zero values
    .sort((a, b) => b.value - a.value) // Sort by value descending
    .slice(0, 10); // Limit to top 10

  // Custom tooltip
  const CustomTooltip = ({ active, payload }: any) => {
    if (active && payload && payload.length) {
      const data = payload[0].payload;
      return (
        <div className="bg-white p-3 border border-gray-200 rounded shadow-lg">
          <p className="font-semibold text-gray-900">{data.crew_name}</p>
          <p className="text-sm text-gray-600">
            {config.label}: <span className="font-medium">{config.formatter(data.value)}</span>
          </p>
          <p className={`text-sm ${data.delta >= 0 ? 'text-green-600' : 'text-red-600'}`}>
            {data.delta >= 0 ? '+' : ''}{config.formatter(Math.abs(data.delta))} {data.delta >= 0 ? 'above' : 'below'} average
          </p>
        </div>
      );
    }
    return null;
  };

  if (!crewMetrics || crewMetrics.length === 0) {
    return (
      <Card>
        <CardHeader>
          <CardTitle className="flex items-center gap-2">
            <BarChart3 className="h-5 w-5 text-gray-400" />
            {title || 'Crew Comparison'}
          </CardTitle>
        </CardHeader>
        <CardContent className="flex items-center justify-center" style={{ height }}>
          <div className="text-center text-gray-500">
            <BarChart3 className="h-8 w-8 mx-auto mb-2 text-gray-300" />
            <p>No crew comparison data available</p>
          </div>
        </CardContent>
      </Card>
    );
  }

  return (
    <Card>
      <CardHeader>
        <CardTitle className="flex items-center gap-2">
          <BarChart3 className="h-5 w-5 text-blue-600" />
          {title || `Crew Comparison - ${config.label}`}
        </CardTitle>
        <p className="text-sm text-gray-600">
          Comparing {chartData.length} crew members (Team avg: {config.formatter(teamAverage)})
        </p>
      </CardHeader>
      <CardContent>
        <ResponsiveContainer width="100%" height={height}>
          <BarChart 
            data={chartData} 
            layout="vertical"
            margin={{ top: 5, right: 30, left: 100, bottom: 5 }}
          >
            <CartesianGrid strokeDasharray="3 3" horizontal={false} />
            <XAxis type="number" />
            <YAxis 
              dataKey="crew_name" 
              type="category" 
              width={90}
              tick={{ fontSize: 12 }}
            />
            <Tooltip content={<CustomTooltip />} />
            <ReferenceLine 
              x={teamAverage} 
              stroke="#9ca3af" 
              strokeDasharray="5 5" 
              label={{ 
                value: 'Team Avg', 
                position: 'top',
                fill: '#6b7280',
                fontSize: 12
              }} 
            />
            <Bar dataKey="value" radius={[0, 4, 4, 0]}>
              {chartData.map((entry, index) => {
                // For completion_time, lower is better (green), higher is worse (red)
                // For other metrics, higher is better (green), lower is worse (red)
                let fillColor: string;
                if (metric === 'completion_time') {
                  fillColor = entry.value <= teamAverage ? '#10b981' : '#ef4444';
                } else {
                  fillColor = entry.value >= teamAverage ? '#10b981' : '#ef4444';
                }
                
                return (
                  <Cell 
                    key={`cell-${index}`}
                    fill={fillColor}
                  />
                );
              })}
            </Bar>
          </BarChart>
        </ResponsiveContainer>
      </CardContent>
    </Card>
  );
}

