'use client'

// components/dashboard/ProjectBuckets.tsx
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card';
import { Skeleton } from '@/components/ui/skeleton';
import Link from 'next/link';
import { Package, Calendar, Pause, AlertTriangle, XCircle, Ban } from 'lucide-react';
import type { ProjectBuckets as ProjectBucketsType } from '@/lib/types/dashboard';

interface ProjectBucketsProps {
  buckets: ProjectBucketsType;
  isLoading?: boolean;
}

export function ProjectBuckets({ buckets, isLoading = false }: ProjectBucketsProps) {
  if (isLoading) {
    return <ProjectBucketsSkeleton />;
  }

  const bucketConfigs = [
    {
      key: 'installs' as keyof ProjectBucketsType,
      label: 'Installs',
      description: 'Completed but not yet PTO\'d',
      icon: Package,
      color: 'text-blue-600',
      bgColor: 'bg-blue-50',
      iconBg: 'bg-blue-100',
      hoverColor: 'hover:bg-blue-100',
      view: 'install-completed',
    },
    {
      key: 'readyForInstall' as keyof ProjectBucketsType,
      label: 'Ready for Install',
      description: 'Ready to schedule',
      icon: Calendar,
      color: 'text-green-600',
      bgColor: 'bg-green-50',
      iconBg: 'bg-green-100',
      hoverColor: 'hover:bg-green-100',
      view: 'install-ready',
    },
    {
      key: 'onHold' as keyof ProjectBucketsType,
      label: 'On Hold',
      description: 'Currently held',
      icon: Pause,
      color: 'text-orange-600',
      bgColor: 'bg-orange-50',
      iconBg: 'bg-orange-100',
      hoverColor: 'hover:bg-orange-100',
      view: 'on-hold',
      highlight: buckets.onHold > 5, // Highlight if more than 5
    },
    {
      key: 'repAttention' as keyof ProjectBucketsType,
      label: 'Rep Attention',
      description: 'Needs action',
      icon: AlertTriangle,
      color: 'text-red-600',
      bgColor: 'bg-red-50',
      iconBg: 'bg-red-100',
      hoverColor: 'hover:bg-red-100',
      view: 'needs-attention',
      highlight: buckets.repAttention > 0, // Highlight if any
    },
    {
      key: 'pendingCancel' as keyof ProjectBucketsType,
      label: 'Pending Cancel',
      description: 'In cancellation process',
      icon: XCircle,
      color: 'text-gray-600',
      bgColor: 'bg-gray-50',
      iconBg: 'bg-gray-100',
      hoverColor: 'hover:bg-gray-100',
      view: 'pending-cancel',
    },
    {
      key: 'rejected' as keyof ProjectBucketsType,
      label: 'Rejected',
      description: 'Rejected projects',
      icon: Ban,
      color: 'text-gray-700',
      bgColor: 'bg-gray-50',
      iconBg: 'bg-gray-100',
      hoverColor: 'hover:bg-gray-100',
      view: 'rejected',
    },
  ];

  return (
    <Card>
      <CardHeader>
        <CardTitle className="text-lg font-semibold">Project Status</CardTitle>
      </CardHeader>
      <CardContent>
        <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-3 gap-4">
          {bucketConfigs.map((config) => {
            const Icon = config.icon;
            const count = buckets[config.key];
            const isHighlighted = config.highlight;

            return (
              <Link
                key={config.key}
                href={`/projects?view=${config.view}`}
                className={`block p-4 ${config.bgColor} rounded-lg transition-all duration-200 ${config.hoverColor} ${
                  isHighlighted ? 'ring-2 ring-red-300 animate-pulse' : ''
                }`}
                aria-label={`View ${count} ${config.label.toLowerCase()} projects`}
              >
                <div className="text-center">
                  <p className="text-3xl font-bold text-gray-900 mb-2">{count}</p>
                  <div className={`inline-flex items-center justify-center w-12 h-12 ${config.iconBg} rounded-lg mb-2`}>
                    <Icon className={`h-6 w-6 ${config.color}`} />
                  </div>
                  <p className="text-sm font-medium text-gray-900">{config.label}</p>
                  <p className="text-xs text-gray-600 mt-1">{config.description}</p>
                </div>
              </Link>
            );
          })}
        </div>
      </CardContent>
    </Card>
  );
}

function ProjectBucketsSkeleton() {
  return (
    <Card>
      <CardHeader>
        <Skeleton className="h-6 w-32" />
      </CardHeader>
      <CardContent>
        <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-3 gap-4">
          {Array.from({ length: 6 }).map((_, i) => (
            <div key={i} className="p-4 bg-gray-50 rounded-lg">
              <div className="text-center">
                <Skeleton className="h-8 w-12 mx-auto mb-2" />
                <Skeleton className="h-12 w-12 mx-auto mb-2 rounded-lg" />
                <Skeleton className="h-4 w-20 mx-auto mb-1" />
                <Skeleton className="h-3 w-24 mx-auto" />
              </div>
            </div>
          ))}
        </div>
      </CardContent>
    </Card>
  );
}
