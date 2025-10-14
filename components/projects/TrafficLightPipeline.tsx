'use client';

import { cn } from '@/lib/utils/cn';
import { getMilestoneStatus, getCurrentMilestone, type MilestoneState } from '@/lib/utils/milestone-engine';
import { useMilestoneConfig } from '@/lib/hooks/useMilestoneConfig';
import { QuickbaseProject } from '@/lib/types/project';
import {
  ClipboardList,
  Camera,
  Ruler,
  Home,
  Zap,
  FileText,
  Wrench,
  Search,
  CheckCircle,
  Power,
  X,
  Clock,
  AlertCircle,
  type LucideIcon
} from 'lucide-react';

interface TrafficLightPipelineProps {
  project: QuickbaseProject;
}

// 7-milestone system: Intake → Survey → Design → Permitting → Installation → Inspection → PTO
const milestones: Array<{ id: string; label: string; Icon: LucideIcon }> = [
  { id: 'intake', label: 'Intake', Icon: ClipboardList },
  { id: 'survey', label: 'Survey', Icon: Camera },
  { id: 'design', label: 'Design', Icon: Ruler },
  { id: 'permitting', label: 'Permitting', Icon: FileText }, // Combines HOA, NEM, and Permit
  { id: 'install', label: 'Install', Icon: Wrench },
  { id: 'inspection', label: 'Inspect', Icon: CheckCircle },
  { id: 'pto', label: 'PTO', Icon: Power },
];

function getTrafficLightClass(state: MilestoneState): string {
  switch (state) {
    case 'complete':
      return 'bg-emerald-500 text-white shadow-sm hover:bg-emerald-600';
    case 'in-progress':
      return 'bg-amber-500 text-white shadow-md ring-2 ring-amber-200 hover:bg-amber-600';
    case 'ready-for':
      return 'bg-blue-500 text-white shadow-sm hover:bg-blue-600';
    case 'pending':
      return 'bg-slate-200 text-slate-400 border border-slate-300 hover:bg-slate-300';
    case 'blocked':
      return 'bg-rose-500 text-white shadow-sm hover:bg-rose-600';
    case 'overdue':
      return 'bg-rose-600 text-white ring-2 ring-rose-300 shadow-md animate-pulse hover:bg-rose-700';
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

function getTooltipText(
  milestone: string,
  state: MilestoneState,
  completedDate: Date | null,
  scheduledDate: Date | null,
  daysInProgress?: number,
  blockedReason?: string
): string {
  const stateLabels: Record<MilestoneState, string> = {
    'complete': '✅',
    'in-progress': '●',
    'ready-for': '▶',
    'pending': '○',
    'blocked': '■',
    'overdue': '⚠',
    'not-applicable': '−'
  };

  const lines: string[] = [];
  lines.push(`${stateLabels[state] || ''} ${milestone}`);

  // Add state-specific details
  if (state === 'complete' && completedDate) {
    const formattedDate = completedDate.toLocaleDateString('en-US', { month: 'short', day: 'numeric', year: 'numeric' });
    lines.push(`Completed: ${formattedDate}`);

    // Calculate duration if we have scheduled date
    if (scheduledDate) {
      const durationDays = Math.floor((completedDate.getTime() - scheduledDate.getTime()) / (1000 * 60 * 60 * 24));
      if (durationDays > 0) {
        lines.push(`Duration: ${durationDays} ${durationDays === 1 ? 'day' : 'days'}`);
      }
    }
  } else if (state === 'in-progress') {
    if (daysInProgress !== undefined && daysInProgress > 0) {
      lines.push(`In progress: ${daysInProgress} ${daysInProgress === 1 ? 'day' : 'days'}`);
    } else {
      lines.push('In progress');
    }
    if (scheduledDate) {
      const formattedDate = scheduledDate.toLocaleDateString('en-US', { month: 'short', day: 'numeric', year: 'numeric' });
      lines.push(`Started: ${formattedDate}`);
    }
  } else if (state === 'ready-for') {
    lines.push('Ready to start');
  } else if (state === 'pending') {
    lines.push('Not started yet');
  } else if (state === 'blocked') {
    lines.push('Blocked');
    if (blockedReason) {
      lines.push(blockedReason);
    }
  } else if (state === 'overdue') {
    lines.push('Overdue!');
    if (scheduledDate) {
      const formattedDate = scheduledDate.toLocaleDateString('en-US', { month: 'short', day: 'numeric', year: 'numeric' });
      lines.push(`Due: ${formattedDate}`);
    }
  }

  return lines.join('\n');
}

export function TrafficLightPipeline({ project }: TrafficLightPipelineProps) {
  // Fetch dynamic milestone configuration
  const { config } = useMilestoneConfig();

  return (
    <div className="flex items-start gap-1 overflow-x-auto pb-2">
      {milestones.map((milestone, index) => {
        const status = getMilestoneStatus(project, milestone.id, config);

        // Skip not-applicable milestones (e.g., HOA when not needed)
        if (status.state === 'not-applicable') {
          return null;
        }

        const isLast = index === milestones.length - 1;
        const DisplayIcon = getStateIcon(status.state, milestone.Icon);

        // Get next milestone status for "Ready for" label
        const nextMilestone = !isLast ? milestones[index + 1] : null;
        const nextStatus = nextMilestone ? getMilestoneStatus(project, nextMilestone.id, config) : null;

        return (
          <div key={milestone.id} className="flex items-center">
            {/* Milestone column */}
            <div className="flex flex-col items-center gap-1 group min-w-[70px]">
              {/* Traffic light circle */}
              <div
                className={cn(
                  'w-10 h-10 rounded-full flex items-center justify-center transition-all duration-300 cursor-help',
                  getTrafficLightClass(status.state)
                )}
                title={getTooltipText(
                  milestone.label,
                  status.state,
                  status.completedDate,
                  status.scheduledDate,
                  status.daysInProgress,
                  status.blockedReason
                )}
                role="status"
                aria-label={`${milestone.label} status: ${status.state}`}
              >
                <DisplayIcon className="w-5 h-5" strokeWidth={2.5} />
              </div>

              {/* Label below circle */}
              <span className={cn(
                "text-[10px] font-medium transition-colors duration-200 text-center",
                status.state === 'complete' ? 'text-emerald-600' :
                status.state === 'in-progress' ? 'text-amber-600' :
                status.state === 'ready-for' ? 'text-blue-600' :
                status.state === 'overdue' ? 'text-rose-600' :
                status.state === 'blocked' ? 'text-rose-500' :
                'text-slate-400'
              )}>
                {milestone.label}
              </span>

              {/* "In: [Milestone]" label below for in-progress */}
              {status.state === 'in-progress' && (
                <div className="text-[9px] font-semibold text-amber-600 mt-0.5">
                  In: {milestone.label}
                </div>
              )}

              {/* Scheduled date display for in-progress milestones */}
              {status.state === 'in-progress' && status.scheduledDate && !status.completedDate && (
                <div className="text-[9px] text-blue-600 mt-0.5 flex items-center gap-0.5">
                  <Clock className="w-2.5 h-2.5" />
                  {status.scheduledDate.toLocaleDateString('en-US', { month: 'short', day: 'numeric' })}
                </div>
              )}

              {/* Days badge for in-progress milestones */}
              {status.state === 'in-progress' && status.daysInProgress !== undefined && status.daysInProgress > 0 && (
                <span className={cn(
                  "text-[9px] font-semibold px-1.5 py-0.5 rounded mt-0.5",
                  status.daysInProgress < 7 ? 'bg-emerald-100 text-emerald-700' :
                  status.daysInProgress <= 14 ? 'bg-amber-100 text-amber-700' :
                  'bg-rose-100 text-rose-700'
                )}>
                  {status.daysInProgress}d
                </span>
              )}
            </div>

            {/* Connector with optional "Ready for" label */}
            {!isLast && (
              <div className="flex flex-col items-center justify-center min-w-[80px] px-2">
                {/* Connector line */}
                <div
                  className={cn(
                    'h-0.5 w-full transition-colors duration-300',
                    status.state === 'complete' ? 'bg-emerald-400' : 'bg-slate-200'
                  )}
                  aria-hidden="true"
                />

                {/* "Ready for: [Next Milestone]" label if next milestone is ready */}
                {nextStatus?.state === 'ready-for' && (
                  <div className="text-[9px] font-semibold text-blue-600 mt-1 whitespace-nowrap">
                    Ready for: {nextMilestone?.label}
                  </div>
                )}
              </div>
            )}
          </div>
        );
      })}
    </div>
  );
}
