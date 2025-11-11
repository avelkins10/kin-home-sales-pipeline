'use client';

import { useState } from 'react';
import { cn } from '@/lib/utils/cn';
import { getMilestoneStatus, type MilestoneState } from '@/lib/utils/milestone-engine';
import { useMilestoneConfig } from '@/lib/hooks/useMilestoneConfig';
import { QuickbaseProject } from '@/lib/types/project';
import {
  ClipboardList,
  Camera,
  Ruler,
  FileText,
  Wrench,
  CheckCircle,
  Power,
  ChevronRight,
  ChevronDown,
  X,
  Clock,
  AlertCircle,
  type LucideIcon
} from 'lucide-react';

interface TrafficLightPipelineMobileProps {
  project: QuickbaseProject;
}

// 7-milestone system: Intake → Survey → Design → Permitting → Installation → Inspection → PTO
const milestones: Array<{ id: string; label: string; shortLabel: string; Icon: LucideIcon }> = [
  { id: 'intake', label: 'Intake', shortLabel: 'Int', Icon: ClipboardList },
  { id: 'survey', label: 'Survey', shortLabel: 'Sur', Icon: Camera },
  { id: 'design', label: 'Design', shortLabel: 'Des', Icon: Ruler },
  { id: 'permitting', label: 'Permitting', shortLabel: 'Per', Icon: FileText },
  { id: 'install', label: 'Install', shortLabel: 'Ins', Icon: Wrench },
  { id: 'inspection', label: 'Inspect', shortLabel: 'Chk', Icon: CheckCircle },
  { id: 'pto', label: 'PTO', shortLabel: 'PTO', Icon: Power },
];

function getTrafficLightClass(state: MilestoneState): string {
  switch (state) {
    case 'complete':
      return 'bg-emerald-500 text-white shadow-sm';
    case 'in-progress':
      return 'bg-amber-500 text-white shadow-md ring-2 ring-amber-200';
    case 'ready-for':
      return 'bg-blue-500 text-white shadow-sm';
    case 'pending':
      return 'bg-slate-200 text-slate-400 border border-slate-300';
    case 'blocked':
      return 'bg-rose-500 text-white shadow-sm';
    case 'overdue':
      return 'bg-rose-600 text-white ring-2 ring-rose-300 shadow-md animate-pulse';
    case 'not-applicable':
      return 'bg-slate-100 text-slate-300 border border-dashed border-slate-200 opacity-50';
    default:
      return 'bg-slate-200 text-slate-400 border border-slate-300';
  }
}

function getStateIcon(state: MilestoneState, defaultIcon: LucideIcon) {
  switch (state) {
    case 'complete':
      return CheckCircle;
    case 'overdue':
      return Clock;
    case 'blocked':
      return AlertCircle;
    case 'not-applicable':
      return X;
    default:
      return defaultIcon;
  }
}

export function TrafficLightPipelineMobile({ project }: TrafficLightPipelineMobileProps) {
  const { config } = useMilestoneConfig();
  const [expanded, setExpanded] = useState(false);

  // Get all milestone statuses
  const milestoneStatuses = milestones.map(milestone => ({
    ...milestone,
    status: getMilestoneStatus(project, milestone.id, config)
  })).filter(m => m.status.state !== 'not-applicable'); // Remove N/A milestones

  // Find the current active milestone (in-progress, overdue, or first non-complete)
  const currentMilestoneIndex = milestoneStatuses.findIndex(
    m => m.status.state === 'in-progress' || m.status.state === 'overdue'
  );

  const activeMilestoneIndex = currentMilestoneIndex >= 0
    ? currentMilestoneIndex
    : milestoneStatuses.findIndex(m => m.status.state !== 'complete');

  // Show 3 milestones in compact view: previous, current, next
  const visibleMilestones = expanded
    ? milestoneStatuses
    : milestoneStatuses.slice(
        Math.max(0, activeMilestoneIndex - 1),
        Math.min(milestoneStatuses.length, activeMilestoneIndex + 2)
      );

  const hasMore = milestoneStatuses.length > visibleMilestones.length;

  return (
    <div className="space-y-2">
      {/* Mobile-optimized compact horizontal layout */}
      <div className="flex items-center gap-1 justify-start">
        {visibleMilestones.map((milestone, index) => {
          const DisplayIcon = getStateIcon(milestone.status.state, milestone.Icon);
          const isActive = milestone.status.state === 'in-progress' || milestone.status.state === 'overdue';

          return (
            <div key={milestone.id} className="flex items-center">
              {/* Milestone circle - slightly smaller for mobile */}
              <div className="flex flex-col items-center gap-0.5 min-w-[50px]">
                <div
                  className={cn(
                    'w-8 h-8 rounded-full flex items-center justify-center transition-all duration-300',
                    getTrafficLightClass(milestone.status.state),
                    isActive && 'ring-2 ring-offset-2 ring-amber-400'
                  )}
                  role="status"
                  aria-label={`${milestone.label} status: ${milestone.status.state}`}
                >
                  <DisplayIcon className="w-4 h-4" strokeWidth={2.5} />
                </div>

                {/* Short label below - only for active milestone or all if expanded */}
                {(isActive || expanded) && (
                  <span className={cn(
                    "text-[9px] font-medium text-center leading-tight",
                    milestone.status.state === 'complete' ? 'text-emerald-600' :
                    milestone.status.state === 'in-progress' ? 'text-amber-600' :
                    milestone.status.state === 'ready-for' ? 'text-blue-600' :
                    milestone.status.state === 'overdue' ? 'text-rose-600' :
                    milestone.status.state === 'blocked' ? 'text-rose-500' :
                    'text-slate-400'
                  )}>
                    {milestone.shortLabel}
                  </span>
                )}

                {/* Completion date display for completed milestones */}
                {milestone.status.state === 'complete' && milestone.status.completedDate && (isActive || expanded) && (
                  <div className="text-[8px] text-emerald-600 mt-0.5">
                    {milestone.status.completedDate.toLocaleDateString('en-US', { month: 'short', day: 'numeric' })}
                  </div>
                )}

                {/* Days badge for in-progress */}
                {milestone.status.state === 'in-progress' && milestone.status.daysInProgress !== undefined && milestone.status.daysInProgress > 0 && (
                  <span className={cn(
                    "text-[8px] font-semibold px-1 py-0.5 rounded mt-0.5",
                    milestone.status.daysInProgress < 7 ? 'bg-emerald-100 text-emerald-700' :
                    milestone.status.daysInProgress <= 14 ? 'bg-amber-100 text-amber-700' :
                    'bg-rose-100 text-rose-700'
                  )}>
                    {milestone.status.daysInProgress}d
                  </span>
                )}
              </div>

              {/* Connector - smaller for mobile */}
              {index < visibleMilestones.length - 1 && (
                <div
                  className={cn(
                    'h-0.5 w-8 transition-colors duration-300 mx-0.5',
                    milestone.status.state === 'complete' ? 'bg-emerald-400' : 'bg-slate-200'
                  )}
                  aria-hidden="true"
                />
              )}
            </div>
          );
        })}

        {/* Expand/Collapse button */}
        {hasMore && (
          <button
            onClick={() => setExpanded(!expanded)}
            className="ml-2 flex items-center gap-1 text-xs text-slate-600 hover:text-slate-900 transition-colors px-2 py-1 rounded-md hover:bg-slate-100 active:bg-slate-200"
            aria-label={expanded ? 'Show less milestones' : 'Show all milestones'}
          >
            {expanded ? (
              <>
                <span>Less</span>
                <ChevronRight className="w-3 h-3" />
              </>
            ) : (
              <>
                <span>All</span>
                <ChevronDown className="w-3 h-3" />
              </>
            )}
          </button>
        )}
      </div>

      {/* Active milestone label - shows full name */}
      {!expanded && activeMilestoneIndex >= 0 && activeMilestoneIndex < milestoneStatuses.length && (
        <div className="text-xs text-slate-600 font-medium">
          Current: {milestoneStatuses[activeMilestoneIndex].label}
          {milestoneStatuses[activeMilestoneIndex].status.daysInProgress !== undefined &&
           milestoneStatuses[activeMilestoneIndex].status.daysInProgress > 0 && (
            <span className="text-slate-500 ml-1">
              ({milestoneStatuses[activeMilestoneIndex].status.daysInProgress} days)
            </span>
          )}
        </div>
      )}
    </div>
  );
}
