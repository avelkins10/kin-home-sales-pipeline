'use client'

// components/dashboard/ProjectBuckets.tsx
import { useState } from 'react';
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card';
import { Skeleton } from '@/components/ui/skeleton';
import Link from 'next/link';
import { Package, Calendar, Pause, AlertTriangle, XCircle, Ban, ChevronDown, ChevronUp, Users } from 'lucide-react';
import type { ProjectBuckets as ProjectBucketsType, TeamMemberBuckets } from '@/lib/types/dashboard';

interface ProjectBucketsProps {
  buckets: ProjectBucketsType;
  isLoading?: boolean;
  // New props for team member breakdown
  bucketsByMember?: TeamMemberBuckets[]; // Team member breakdown (managers only)
  isManager?: boolean;                    // Whether user is a manager
  scope?: 'personal' | 'team';            // Current scope
}

export function ProjectBuckets({ 
  buckets, 
  isLoading = false,
  bucketsByMember,
  isManager = false,
  scope = 'personal',
}: ProjectBucketsProps) {
  const [expandedBucket, setExpandedBucket] = useState<string | null>(null);

  // Helper function to filter members by bucket
  const getMembersForBucket = (bucketKey: keyof ProjectBucketsType): TeamMemberBuckets[] => {
    if (!bucketsByMember) return [];
    return bucketsByMember.filter(member => member[bucketKey] > 0);
  };

  // Helper function to toggle expansion
  const toggleExpanded = (bucketKey: string) => {
    setExpandedBucket(expandedBucket === bucketKey ? null : bucketKey);
  };

  if (isLoading) {
    return <ProjectBucketsSkeleton />;
  }

  // TeamMemberBucketCard component
  const TeamMemberBucketCard = ({ 
    member, 
    bucketKey, 
    bucketView 
  }: { 
    member: TeamMemberBuckets; 
    bucketKey: keyof ProjectBucketsType; 
    bucketView: string; 
  }) => {
    const url = `/projects?view=${bucketView}&memberEmail=${encodeURIComponent(member.memberEmail || '')}`;
    const count = member[bucketKey];
    
    return (
      <Link href={url}>
        <div className="flex items-center justify-between p-3 bg-gray-50 rounded-lg hover:bg-gray-100 transition-colors">
          <div>
            <p className="font-medium text-sm">{member.memberName}</p>
            <p className="text-xs text-gray-500">
              <span className={`inline-flex items-center px-2 py-0.5 rounded text-xs font-medium ${
                member.role === 'closer' 
                  ? 'bg-blue-100 text-blue-800' 
                  : 'bg-purple-100 text-purple-800'
              }`}>
                {member.role}
              </span>
              <span className="ml-2">• {member.totalProjects} total projects</span>
            </p>
          </div>
          <div className="text-right">
            <p className="text-lg font-semibold">{count}</p>
            <p className="text-xs text-gray-500">in this bucket</p>
          </div>
        </div>
      </Link>
    );
  };

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
            const hasTeamMemberData = isManager && scope === 'team' && bucketsByMember && bucketsByMember.length > 0;
            const membersForBucket = hasTeamMemberData
              ? [...getMembersForBucket(config.key)].sort((a, b) => b[config.key] - a[config.key])
              : [];
            const isExpanded = expandedBucket === config.key;

            return (
              <div key={config.key} className="relative">
                {/* Main bucket card */}
                <Link
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

                {/* Drill-down button (only for managers with team data) */}
                {hasTeamMemberData && membersForBucket.length > 0 && (
                  <button
                    onClick={() => toggleExpanded(config.key)}
                    className="w-full mt-2 flex items-center justify-center gap-2 px-3 py-2 text-sm text-gray-600 hover:text-gray-800 hover:bg-gray-100 rounded-lg transition-colors"
                    aria-expanded={isExpanded}
                    aria-label={`View ${membersForBucket.length} team members with ${config.label.toLowerCase()} projects`}
                  >
                    <Users className="h-4 w-4" />
                    <span>View by Team Member ({membersForBucket.length})</span>
                    {isExpanded ? <ChevronUp className="h-4 w-4" /> : <ChevronDown className="h-4 w-4" />}
                  </button>
                )}

                {/* Expanded team member breakdown */}
                {isExpanded && membersForBucket.length > 0 && (
                  <div className="mt-3 space-y-2 max-h-64 overflow-y-auto">
                    {membersForBucket.map((member) => (
                      <TeamMemberBucketCard
                        key={member.memberEmail || member.memberName}
                        member={member}
                        bucketKey={config.key}
                        bucketView={config.view}
                      />
                    ))}
                  </div>
                )}
              </div>
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
