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
      return 'bg-emerald-500 text-white shadow-sm';
    case 'in-progress':
      return 'bg-orange-500 text-white shadow-md ring-2 ring-orange-200';
    case 'pending':
      return 'bg-slate-100 text-slate-400 border border-slate-200';
    case 'on-hold':
      return 'bg-rose-500 text-white shadow-sm';
    case 'overdue':
      return 'bg-rose-600 text-white ring-2 ring-rose-300 shadow-md';
    default:
      return 'bg-slate-100 text-slate-400 border border-slate-200';
  }
}

export function TrafficLightPipeline({ project }: TrafficLightPipelineProps) {
  return (
    <div className="space-y-2.5">
      {/* Traffic lights row */}
      <div className="flex items-center gap-2">
        {milestones.map((milestone, index) => {
          const state = calculateMilestoneState(project, milestone.id);
          const isLast = index === milestones.length - 1;
          const Icon = milestone.Icon;

          return (
            <div key={milestone.id} className="flex items-center gap-2">
              {/* Traffic light circle */}
              <div
                className={cn(
                  'w-9 h-9 rounded-full flex items-center justify-center transition-all duration-200',
                  getTrafficLightClass(state),
                  state === 'in-progress' && 'animate-pulse'
                )}
                title={`${milestone.label}: ${state}`}
                role="status"
                aria-label={`${milestone.label} status: ${state}`}
              >
                <Icon className="w-4 h-4" strokeWidth={2.5} />
              </div>

              {/* Connector line */}
              {!isLast && (
                <div
                  className={cn(
                    'h-0.5 w-3 transition-colors duration-200',
                    state === 'complete' ? 'bg-emerald-400' : 'bg-slate-200'
                  )}
                  aria-hidden="true"
                />
              )}
            </div>
          );
        })}
      </div>

      {/* Status text below lights */}
      <div className="text-xs font-medium text-slate-600">
        {getMilestoneStatusText(project)}
      </div>
    </div>
  );
}
