'use client'

// components/dashboard/CommissionSummary.tsx
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card';
import { Skeleton } from '@/components/ui/skeleton';
import { formatCurrency } from '@/lib/utils/formatters';
import { CheckCircle, Clock, Pause, XCircle, Users, Info, ChevronDown, ChevronUp } from 'lucide-react';
import type { TimeRange, TeamMemberCommission } from '@/lib/types/dashboard';
import { useState } from 'react';

interface CommissionSummaryProps {
  earnedCommission: number;
  lostCommission: number;
  onHoldCommission: number;
  pendingCommission: number;
  salesAidCommission: number;
  timeRange: TimeRange;
  isLoading?: boolean;
  // New props for team member breakdown
  commissionByMember?: TeamMemberCommission[]; // Team member breakdown (managers only)
  isManager?: boolean;                          // Whether user is a manager
  scope?: 'personal' | 'team';                  // Current scope
}

export function CommissionSummary({
  earnedCommission,
  lostCommission,
  onHoldCommission,
  pendingCommission,
  salesAidCommission,
  timeRange,
  isLoading = false,
  commissionByMember,
  isManager = false,
  scope = 'personal',
}: CommissionSummaryProps) {
  const [isExpanded, setIsExpanded] = useState(false);
  const getTimeRangeLabel = (range: TimeRange) => {
    switch (range) {
      case 'month': return 'This Month';
      case 'week': return 'This Week';
      case 'lifetime': return 'Lifetime';
      default: return 'Lifetime';
    }
  };

  const totalPotential = earnedCommission + pendingCommission;

  if (isLoading) {
    return <CommissionSummarySkeleton />;
  }

  // Show empty state if all values are 0
  if (earnedCommission === 0 && lostCommission === 0 && onHoldCommission === 0 && pendingCommission === 0 && salesAidCommission === 0) {
    return (
      <Card>
        <CardHeader>
          <CardTitle className="text-lg font-semibold">Commission Breakdown</CardTitle>
          <span className="text-xs bg-blue-100 text-blue-800 px-2 py-1 rounded-full w-fit">
            {getTimeRangeLabel(timeRange)}
          </span>
        </CardHeader>
        <CardContent>
          <div className="text-center py-8 text-gray-500">
            <Users className="h-12 w-12 mx-auto mb-4 text-gray-300" />
            <p>No commission data available for this time period</p>
          </div>
        </CardContent>
      </Card>
    );
  }

  const commissionItems = [
    {
      label: 'Earned',
      description: 'Commission received from completed installs',
      amount: earnedCommission,
      icon: CheckCircle,
      color: 'text-green-600',
      bgColor: 'bg-green-50',
      iconBg: 'bg-green-100',
    },
    {
      label: 'Pending',
      description: 'Commission from active projects not yet installed',
      amount: pendingCommission,
      icon: Clock,
      color: 'text-blue-600',
      bgColor: 'bg-blue-50',
      iconBg: 'bg-blue-100',
    },
    {
      label: 'On Hold',
      description: 'Commission at risk from held projects',
      amount: onHoldCommission,
      icon: Pause,
      color: 'text-orange-600',
      bgColor: 'bg-orange-50',
      iconBg: 'bg-orange-100',
    },
    {
      label: 'Lost',
      description: 'Commission from cancelled projects',
      amount: lostCommission,
      icon: XCircle,
      color: 'text-red-600',
      bgColor: 'bg-red-50',
      iconBg: 'bg-red-100',
    },
  ];

  // Only show Sales Aid if > 0
  if (salesAidCommission > 0) {
    commissionItems.push({
      label: 'Sales Aid',
      description: 'Commission shared with setter',
      amount: salesAidCommission,
      icon: Users,
      color: 'text-purple-600',
      bgColor: 'bg-purple-50',
      iconBg: 'bg-purple-100',
    });
  }

  return (
    <Card>
      <CardHeader>
        <CardTitle className="text-lg font-semibold">Commission Breakdown</CardTitle>
        <span className="text-xs bg-blue-100 text-blue-800 px-2 py-1 rounded-full w-fit">
          {getTimeRangeLabel(timeRange)}
        </span>
      </CardHeader>
      <CardContent className="space-y-3">
        {commissionItems.map((item) => {
          const Icon = item.icon;
          return (
            <div key={item.label} className={`flex items-center space-x-3 p-3 ${item.bgColor} rounded-lg`}>
              <div className={`p-2 ${item.iconBg} rounded-lg`}>
                <Icon className={`h-4 w-4 ${item.color}`} />
              </div>
              <div className="flex-1">
                <div className="flex items-center space-x-2">
                  <p className="text-sm font-medium text-gray-900">{item.label}</p>
                  <Info className="h-3 w-3 text-gray-400" />
                </div>
                <p className="text-xs text-gray-600">{item.description}</p>
              </div>
              <div className="text-right">
                <p className={`text-lg font-semibold ${item.color}`}>
                  {formatCurrency(item.amount)}
                </p>
              </div>
            </div>
          );
        })}

        {/* Total Potential */}
        {totalPotential > 0 && (
          <>
            <div className="border-t border-gray-200 pt-3">
              <div className="flex items-center justify-between p-3 bg-gray-50 rounded-lg">
                <div>
                  <p className="text-sm font-semibold text-gray-900">Total Potential</p>
                  <p className="text-xs text-gray-600">Earned + Pending</p>
                </div>
                <p className="text-xl font-bold text-gray-900">
                  {formatCurrency(totalPotential)}
                </p>
              </div>
            </div>
          </>
        )}

        {/* Team Member Breakdown - Only for managers with team scope */}
        {isManager && scope === 'team' && commissionByMember && commissionByMember.length > 0 && (
          <div className="border-t border-gray-200 pt-3">
            <button
              onClick={() => setIsExpanded(!isExpanded)}
              className="w-full flex items-center justify-between p-3 bg-gray-50 rounded-lg hover:bg-gray-100 transition-colors"
              aria-expanded={isExpanded}
              aria-label="Toggle team member commission breakdown"
            >
              <div className="flex items-center space-x-2">
                <Users className="h-4 w-4 text-gray-600" />
                <span className="text-sm font-medium text-gray-900">
                  View by Team Member ({commissionByMember.length})
                </span>
              </div>
              {isExpanded ? (
                <ChevronUp className="h-4 w-4 text-gray-600" />
              ) : (
                <ChevronDown className="h-4 w-4 text-gray-600" />
              )}
            </button>

            {isExpanded && (
              <div data-testid="team-member-list" className="mt-3 space-y-2">
                {commissionByMember.map((member, index) => {
                  const totalPotential = member.earnedCommission + member.pendingCommission;
                  
                  return (
                    <div
                      key={`${member.memberEmail || member.memberName}-${index}`}
                      data-testid="team-member-card"
                      className="p-3 bg-white border border-gray-200 rounded-lg"
                    >
                      {/* Member Header */}
                      <div className="flex items-center justify-between mb-2">
                        <div>
                          <p data-testid="member-name" className="text-sm font-semibold text-gray-900">
                            {member.memberName}
                          </p>
                          <p data-testid="member-role" className="text-xs text-gray-500">
                            {member.role === 'closer' ? 'Closer' : 'Setter'} • {member.projectCount} projects
                          </p>
                        </div>
                        <div className="text-right">
                          <p data-testid="total-potential" className="text-sm font-semibold text-gray-900">
                            {formatCurrency(totalPotential)}
                          </p>
                          <p className="text-xs text-gray-500">Total Potential</p>
                        </div>
                      </div>

                      {/* Member Commission Breakdown */}
                      <div className="grid grid-cols-2 gap-2 mt-2">
                        {member.earnedCommission > 0 && (
                          <div className="text-xs">
                            <span className="text-gray-600">Earned:</span>
                            <span data-testid="earned-commission" className="ml-1 font-medium text-green-600">
                              {formatCurrency(member.earnedCommission)}
                            </span>
                          </div>
                        )}
                        {member.pendingCommission > 0 && (
                          <div className="text-xs">
                            <span className="text-gray-600">Pending:</span>
                            <span data-testid="pending-commission" className="ml-1 font-medium text-blue-600">
                              {formatCurrency(member.pendingCommission)}
                            </span>
                          </div>
                        )}
                        {member.onHoldCommission > 0 && (
                          <div className="text-xs">
                            <span className="text-gray-600">On Hold:</span>
                            <span data-testid="on-hold-commission" className="ml-1 font-medium text-orange-600">
                              {formatCurrency(member.onHoldCommission)}
                            </span>
                          </div>
                        )}
                        {member.lostCommission > 0 && (
                          <div className="text-xs">
                            <span className="text-gray-600">Lost:</span>
                            <span data-testid="lost-commission" className="ml-1 font-medium text-red-600">
                              {formatCurrency(member.lostCommission)}
                            </span>
                          </div>
                        )}
                      </div>
                    </div>
                  );
                })}
              </div>
            )}
          </div>
        )}

        {/* Business Note */}
        {salesAidCommission === 0 && (
          <div className="text-xs text-gray-500 italic">
            * Sales Aid Commission calculation needs business clarification
          </div>
        )}
      </CardContent>
    </Card>
  );
}

function CommissionSummarySkeleton() {
  return (
    <Card>
      <CardHeader>
        <Skeleton className="h-6 w-40" />
        <Skeleton className="h-6 w-20" />
      </CardHeader>
      <CardContent className="space-y-3">
        {Array.from({ length: 4 }).map((_, i) => (
          <div key={i} className="flex items-center space-x-3 p-3 bg-gray-50 rounded-lg">
            <Skeleton className="h-8 w-8 rounded-lg" />
            <div className="flex-1">
              <Skeleton className="h-4 w-16 mb-1" />
              <Skeleton className="h-3 w-32" />
            </div>
            <Skeleton className="h-6 w-20" />
          </div>
        ))}
        <div className="border-t border-gray-200 pt-3">
          <div className="flex items-center justify-between p-3 bg-gray-50 rounded-lg">
            <div>
              <Skeleton className="h-4 w-24 mb-1" />
              <Skeleton className="h-3 w-20" />
            </div>
            <Skeleton className="h-6 w-24" />
          </div>
        </div>
      </CardContent>
    </Card>
  );
}
