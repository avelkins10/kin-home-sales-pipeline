'use client';

import { AlertTriangle, Clock } from 'lucide-react';
import { PROJECT_FIELDS } from '@/lib/constants/fieldIds';
import { QuickbaseProject } from '@/lib/types/project';

interface HoldBannerProps {
  project: QuickbaseProject;
  holdType: 'finance' | 'roof' | 'customer' | 'permit' | 'hoa' | 'generic';
}

function calculateDaysOnHold(dateOnHold?: string): number {
  if (!dateOnHold) return 0;
  
  try {
    const holdDate = new Date(dateOnHold);
    const now = new Date();
    const diffTime = now.getTime() - holdDate.getTime();
    return Math.floor(diffTime / (1000 * 60 * 60 * 24));
  } catch {
    return 0;
  }
}

function getHoldTypeClasses(holdType: string): string {
  switch (holdType) {
    case 'finance':
      return 'bg-rose-50 border-l-rose-500 text-rose-900';
    case 'roof':
      return 'bg-orange-50 border-l-orange-500 text-orange-900';
    case 'customer':
      return 'bg-amber-50 border-l-amber-500 text-amber-900';
    case 'permit':
      return 'bg-yellow-50 border-l-yellow-500 text-yellow-900';
    case 'hoa':
      return 'bg-lime-50 border-l-lime-500 text-lime-900';
    case 'generic':
    default:
      return 'bg-slate-50 border-l-slate-400 text-slate-900';
  }
}

function getHoldTypeLabel(holdType: string): string {
  switch (holdType) {
    case 'finance':
      return 'Finance Hold';
    case 'roof':
      return 'Roof Hold';
    case 'customer':
      return 'Customer Hold';
    case 'permit':
      return 'Permit Hold';
    case 'hoa':
      return 'HOA Hold';
    case 'generic':
    default:
      return 'On Hold';
  }
}

export function HoldBanner({ project, holdType }: HoldBannerProps) {
  // Extract hold data
  const holdReason = project[PROJECT_FIELDS.HOLD_REASON]?.value || 
                    project[PROJECT_FIELDS.BLOCK_REASON]?.value || 
                    'No reason specified';
  const dateOnHold = project[PROJECT_FIELDS.DATE_ON_HOLD]?.value;
  const daysOnHold = calculateDaysOnHold(dateOnHold);

  const colorClasses = getHoldTypeClasses(holdType);
  const holdLabel = getHoldTypeLabel(holdType);

  return (
    <div className={`rounded-t-lg border-l-4 border-slate-200 border-t border-r p-3.5 ${colorClasses}`}>
      <div className="flex items-center justify-between gap-4">
        {/* Left side: Alert icon + hold label + reason */}
        <div className="flex items-center gap-2.5 min-w-0 flex-1">
          <AlertTriangle className="h-4 w-4 flex-shrink-0" />
          <span className="font-semibold text-sm flex-shrink-0">{holdLabel}</span>
          <span className="text-sm text-current/40">•</span>
          <span className="text-sm truncate">{holdReason}</span>
        </div>

        {/* Right side: Days on hold (if > 0) */}
        {daysOnHold > 0 && (
          <div className="flex items-center gap-1.5 text-sm font-medium flex-shrink-0">
            <Clock className="h-3.5 w-3.5" />
            <span>{daysOnHold} {daysOnHold === 1 ? 'day' : 'days'}</span>
          </div>
        )}
      </div>
    </div>
  );
}
