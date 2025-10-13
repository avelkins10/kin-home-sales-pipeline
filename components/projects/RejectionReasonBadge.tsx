'use client';

import { AlertTriangle } from 'lucide-react';

interface RejectionReasonBadgeProps {
  reasons: string | number | null | undefined | any;
  className?: string;
}

export function RejectionReasonBadge({ reasons, className = '' }: RejectionReasonBadgeProps) {
  if (!reasons) return null;

  // Convert to string if needed (handle various QuickBase data types)
  const reasonsStr = typeof reasons === 'string' ? reasons : String(reasons);
  if (!reasonsStr || reasonsStr === 'null' || reasonsStr === 'undefined') return null;

  // Parse multitext field (could be newline or semicolon separated)
  const reasonsList = reasonsStr
    .split(/[\n;]/)
    .map(r => r.trim())
    .filter(r => r.length > 0);

  if (reasonsList.length === 0) return null;

  // Display first reason with truncation, show count if multiple
  const displayReason = reasonsList[0];
  const additionalCount = reasonsList.length - 1;

  return (
    <div className={`inline-flex items-start gap-2 mt-2 px-3 py-2 bg-red-50 border border-red-200 rounded-md ${className}`}>
      <AlertTriangle className="h-4 w-4 text-red-600 mt-0.5 flex-shrink-0" />
      <div className="flex flex-col">
        <div className="text-xs font-semibold text-red-900 mb-0.5">
          Rejection Reason{additionalCount > 0 ? 's' : ''}:
        </div>
        <div className="text-xs text-red-800">
          {displayReason}
          {additionalCount > 0 && (
            <span className="ml-1 text-red-600 font-medium">
              (+{additionalCount} more)
            </span>
          )}
        </div>
      </div>
    </div>
  );
}
