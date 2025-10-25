'use client';

import { Card, CardContent } from '@/components/ui/card';
import { Skeleton } from '@/components/ui/skeleton';
import { 
  Users, 
  Bell, 
  AlertTriangle, 
  AlertCircle, 
  CheckCircle, 
  TrendingUp 
} from 'lucide-react';
import type { PCDashboardMetrics } from '@/lib/types/operations';

interface PCMetricsCardsProps {
  metrics: PCDashboardMetrics;
}

function PCMetricsSkeleton() {
  return (
    <div className="grid grid-cols-2 md:grid-cols-3 lg:grid-cols-6 gap-3 mobile:gap-4 ipad:gap-6">
      {Array.from({ length: 6 }).map((_, i) => (
        <Card key={i} className="hover:shadow-md transition-shadow">
          <CardContent className="p-3 mobile:p-4 ipad:p-6">
            <div className="flex items-center justify-between">
              <div className="space-y-2">
                <Skeleton className="h-4 w-24" />
                <Skeleton className="h-8 w-16" />
                <Skeleton className="h-3 w-20" />
              </div>
              <Skeleton className="h-14 w-14 rounded-full" />
            </div>
          </CardContent>
        </Card>
      ))}
    </div>
  );
}

export function PCMetricsCards({ metrics }: PCMetricsCardsProps) {
  const stats = [
    {
      label: 'Total Projects',
      value: metrics.totalProjects,
      icon: Users,
      color: 'text-blue-600',
      bgColor: 'bg-blue-50',
    },
    {
      label: 'Pending Outreach',
      value: metrics.pendingOutreach,
      icon: Bell,
      color: 'text-orange-600',
      bgColor: 'bg-orange-50',
    },
    {
      label: 'Unresponsive Customers',
      value: metrics.unresponsiveCustomers,
      icon: AlertTriangle,
      color: 'text-red-600',
      bgColor: 'bg-red-50',
    },
    {
      label: 'Active Escalations',
      value: metrics.activeEscalations,
      icon: AlertCircle,
      color: 'text-yellow-600',
      bgColor: 'bg-yellow-50',
    },
    {
      label: "Today's Installs",
      value: metrics.todaysInstalls,
      icon: CheckCircle,
      color: 'text-green-600',
      bgColor: 'bg-green-50',
    },
    {
      label: 'SLA Compliance',
      value: `${metrics.slaCompliance}%`,
      icon: TrendingUp,
      color: 'text-purple-600',
      bgColor: 'bg-purple-50',
    },
  ];

  return (
    <div className="grid grid-cols-2 md:grid-cols-3 lg:grid-cols-6 gap-3 mobile:gap-4 ipad:gap-6">
      {stats.map((stat) => {
        const Icon = stat.icon;
        return (
          <Card key={stat.label} className="hover:shadow-md transition-shadow">
            <CardContent className="p-3 mobile:p-4 ipad:p-6">
              <div className="flex flex-col mobile:flex-row items-start mobile:items-center mobile:justify-between gap-2 mobile:gap-0">
                <div className="flex-1 min-w-0">
                  <p className="text-xs mobile:text-sm font-medium text-gray-600 truncate">{stat.label}</p>
                  <p className="text-xl mobile:text-2xl ipad:text-3xl font-bold text-gray-900">{stat.value}</p>
                </div>
                <div className={`p-2 mobile:p-2.5 ipad:p-3 rounded-full ${stat.bgColor} flex-shrink-0`}>
                  <Icon className={`h-5 w-5 mobile:h-6 mobile:w-6 ipad:h-8 ipad:w-8 ${stat.color}`} />
                </div>
              </div>
            </CardContent>
          </Card>
        );
      })}
    </div>
  );
}

export { PCMetricsSkeleton };
