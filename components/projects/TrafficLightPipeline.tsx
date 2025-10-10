'use client';

import { cn } from '@/lib/utils/cn';
import { getMilestoneStatus, getCurrentMilestone, type MilestoneState } from '@/lib/utils/milestone-engine';
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

function getTooltipText(milestone: string, state: MilestoneState, blockedReason?: string): string {
  const stateLabels: Record<MilestoneState, string> = {
    'complete': '✓ Complete',
    'in-progress': '● In Progress',
    'ready-for': '▶ Ready to Start',
    'pending': '○ Pending',
    'blocked': '■ Blocked',
    'overdue': '⚠ Overdue',
    'not-applicable': '− Not Applicable'
  };

  const baseText = `${milestone}: ${stateLabels[state] || state}`;
  if (blockedReason && state === 'blocked') {
    return `${baseText} (${blockedReason})`;
  }
  return baseText;
}

export function TrafficLightPipeline({ project }: TrafficLightPipelineProps) {
  // Get current milestone for status text
  const currentMilestoneId = getCurrentMilestone(project);
  const currentMilestone = milestones.find(m => m.id === currentMilestoneId);

  return (
    <div className="space-y-3">
      {/* Traffic lights row with labels */}
      <div className="flex items-start gap-3 overflow-x-auto pb-2">
        {milestones.map((milestone, index) => {
          const status = getMilestoneStatus(project, milestone.id);

          // Skip not-applicable milestones (e.g., HOA when not needed)
          if (status.state === 'not-applicable') {
            return null;
          }

          const isLast = index === milestones.length - 1;
          const DisplayIcon = getStateIcon(status.state, milestone.Icon);

          return (
            <div key={milestone.id} className="flex items-center gap-2">
              {/* Milestone column */}
              <div className="flex flex-col items-center gap-1 group">
                {/* Traffic light circle */}
                <div
                  className={cn(
                    'w-10 h-10 rounded-full flex items-center justify-center transition-all duration-300 cursor-help',
                    getTrafficLightClass(status.state)
                  )}
                  title={getTooltipText(milestone.label, status.state, status.blockedReason)}
                  role="status"
                  aria-label={`${milestone.label} status: ${status.state}`}
                >
                  <DisplayIcon className="w-5 h-5" strokeWidth={2.5} />
                </div>

                {/* Label below circle */}
                <span className={cn(
                  "text-[10px] font-medium transition-colors duration-200",
                  status.state === 'complete' ? 'text-emerald-600' :
                  status.state === 'in-progress' ? 'text-amber-600' :
                  status.state === 'ready-for' ? 'text-blue-600' :
                  status.state === 'overdue' ? 'text-rose-600' :
                  status.state === 'blocked' ? 'text-rose-500' :
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
                    status.state === 'complete' ? 'bg-emerald-400' : 'bg-slate-200'
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
          Current: {currentMilestone?.label || 'Unknown'}
        </div>
      </div>
    </div>
  );
}
