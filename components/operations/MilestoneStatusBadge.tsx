'use client';

import { Badge } from '@/components/ui/badge';
import { type OperationsMilestone, type MilestoneStatus } from '@/lib/types/operations';
import {
  formatMilestoneName,
  formatMilestoneStatus,
  getMilestoneColor
} from '@/lib/utils/operations-milestones';
import { AlertCircle, Clock } from 'lucide-react';

interface MilestoneStatusBadgeProps {
  milestone: OperationsMilestone;
  status?: MilestoneStatus;
  daysInStage?: number;
  showDays?: boolean;
  isBlocked?: boolean;
  isOnHold?: boolean;
  size?: 'sm' | 'md' | 'lg';
}

export function MilestoneStatusBadge({
  milestone,
  status,
  daysInStage,
  showDays = false,
  isBlocked = false,
  isOnHold = false,
  size = 'md',
}: MilestoneStatusBadgeProps) {
  const colors = getMilestoneColor(milestone);
  const milestoneName = formatMilestoneName(milestone);

  // Override colors if blocked or on hold
  let bgColor = colors.bg;
  let textColor = colors.text;
  let borderColor = colors.border;

  if (isBlocked) {
    bgColor = 'bg-red-50';
    textColor = 'text-red-700';
    borderColor = 'border-red-200';
  } else if (isOnHold) {
    bgColor = 'bg-yellow-50';
    textColor = 'text-yellow-700';
    borderColor = 'border-yellow-200';
  }

  // Size classes
  const sizeClasses = {
    sm: 'text-xs px-2 py-0.5',
    md: 'text-sm px-2.5 py-1',
    lg: 'text-base px-3 py-1.5',
  };

  return (
    <div className="flex items-center gap-2">
      <Badge
        variant="outline"
        className={`
          ${bgColor} ${textColor} ${borderColor} border
          ${sizeClasses[size]}
          font-medium
        `}
      >
        {isBlocked && <AlertCircle className="w-3 h-3 mr-1 inline-block" />}
        {isOnHold && <Clock className="w-3 h-3 mr-1 inline-block" />}
        {milestoneName}
      </Badge>

      {status && (
        <span className={`text-xs text-slate-600`}>
          {formatMilestoneStatus(status)}
        </span>
      )}

      {showDays && daysInStage !== undefined && (
        <span className={`text-xs font-medium ${
          daysInStage > 7 ? 'text-amber-600' : 'text-slate-500'
        }`}>
          {daysInStage}d
        </span>
      )}
    </div>
  );
}
