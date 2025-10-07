'use client';

import { cn } from '@/lib/utils/cn';
import { calculateMilestoneState, getMilestoneStatusText, MilestoneState } from '@/lib/utils/traffic-lights';
import { QuickbaseProject } from '@/lib/types/project';
import {
  ClipboardList,
  Camera,
  Ruler,
  Zap,
  FileText,
  Wrench,
  CheckCircle,
  X,
  Clock,
  type LucideIcon
} from 'lucide-react';

interface TrafficLightPipelineProps {
  project: QuickbaseProject;
}

const milestones: Array<{ id: 'intake' | 'survey' | 'design' | 'nem' | 'permit' | 'install' | 'inspection'; label: string; Icon: LucideIcon }> = [
  { id: 'intake' as const, label: 'Intake', Icon: ClipboardList },
  { id: 'survey' as const, label: 'Survey', Icon: Camera },
  { id: 'design' as const, label: 'Design', Icon: Ruler },
  { id: 'nem' as const, label: 'NEM', Icon: Zap },
  { id: 'permit' as const, label: 'Permit', Icon: FileText },
  { id: 'install' as const, label: 'Install', Icon: Wrench },
  { id: 'inspection' as const, label: 'PTO', Icon: CheckCircle },
];

function getTrafficLightClass(state: MilestoneState): string {
  switch (state) {
    case 'complete':
      return 'bg-emerald-500 text-white shadow-sm hover:bg-emerald-600';
    case 'in-progress':
      return 'bg-amber-500 text-white shadow-md ring-2 ring-amber-200 hover:bg-amber-600';
    case 'pending':
      return 'bg-slate-200 text-slate-400 border border-slate-300 hover:bg-slate-300';
    case 'on-hold':
      return 'bg-rose-500 text-white shadow-sm hover:bg-rose-600';
    case 'overdue':
      return 'bg-rose-600 text-white ring-2 ring-rose-300 shadow-md animate-pulse hover:bg-rose-700';
    case 'rejected':
      return 'bg-red-500 text-white shadow-sm hover:bg-red-600';
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
    case 'rejected':
      return X;
    default:
      return defaultIcon;
  }
}

function getTooltipText(milestone: string, state: MilestoneState): string {
  const stateLabels = {
    'complete': '✓ Complete',
    'in-progress': '● In Progress',
    'pending': '○ Pending',
    'on-hold': '■ On Hold',
    'overdue': '⚠ Overdue',
    'rejected': '✗ Rejected'
  };
  return `${milestone}: ${stateLabels[state] || state}`;
}

export function TrafficLightPipeline({ project }: TrafficLightPipelineProps) {
  return (
    <div className="space-y-3">
      {/* Traffic lights row with labels */}
      <div className="flex items-start gap-3">
        {milestones.map((milestone, index) => {
          const state = calculateMilestoneState(project, milestone.id);
          const isLast = index === milestones.length - 1;
          const DisplayIcon = getStateIcon(state, milestone.Icon);

          return (
            <div key={milestone.id} className="flex items-center gap-2">
              {/* Milestone column */}
              <div className="flex flex-col items-center gap-1 group">
                {/* Traffic light circle */}
                <div
                  className={cn(
                    'w-10 h-10 rounded-full flex items-center justify-center transition-all duration-300 cursor-help',
                    getTrafficLightClass(state)
                  )}
                  title={getTooltipText(milestone.label, state)}
                  role="status"
                  aria-label={`${milestone.label} status: ${state}`}
                >
                  <DisplayIcon className="w-5 h-5" strokeWidth={2.5} />
                </div>

                {/* Label below circle */}
                <span className={cn(
                  "text-[10px] font-medium transition-colors duration-200",
                  state === 'complete' ? 'text-emerald-600' :
                  state === 'in-progress' ? 'text-amber-600' :
                  state === 'overdue' ? 'text-rose-600' :
                  'text-slate-400'
                )}>
                  {milestone.label}
                </span>
              </div>

              {/* Connector line */}
              {!isLast && (
                <div
                  className={cn(
                    'h-0.5 w-4 mt-[-10px] transition-colors duration-300',
                    state === 'complete' ? 'bg-emerald-400' : 'bg-slate-200'
                  )}
                  aria-hidden="true"
                />
              )}
            </div>
          );
        })}
      </div>

      {/* Enhanced status text with badge */}
      <div className="flex items-center gap-2 px-3 py-2 bg-gradient-to-r from-slate-50 to-transparent rounded-lg border-l-2 border-indigo-400">
        <div className="text-xs font-semibold text-slate-700">
          {getMilestoneStatusText(project)}
        </div>
      </div>
    </div>
  );
}
