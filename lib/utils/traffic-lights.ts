// lib/utils/traffic-lights.ts
/**
 * @deprecated This file is legacy code for backward compatibility only.
 * New code should use /lib/utils/milestone-engine.ts which implements the new 7-milestone system:
 * Intake → Survey → Design → Permitting (combines NEM/Permit/HOA) → Installation → Inspection → PTO
 *
 * This file is retained for debug pages and legacy components only.
 */

import { PROJECT_FIELDS } from '@/lib/constants/fieldIds';
import { QuickbaseProject } from '@/lib/types/project';
import { getProjectAge } from './project-helpers';

// Types for traffic light states
export type MilestoneId = 'intake' | 'survey' | 'design' | 'nem' | 'permit' | 'install' | 'inspection';
export type MilestoneState = 'complete' | 'in-progress' | 'pending' | 'on-hold' | 'overdue' | 'rejected';

// Helper function to detect if project is on hold
function detectHoldStatus(status: string): boolean {
  const holdKeywords = ['On Hold', 'Finance Hold', 'Roof Hold', 'Customer Hold', 'Permit Hold', 'HOA Hold'];
  return holdKeywords.some(keyword => status.includes(keyword));
}

// Helper function to calculate days between dates
function calculateDaysWaiting(submitDate?: string): number {
  if (!submitDate) return 0;
  const submit = new Date(submitDate);
  const now = new Date();
  const diffTime = now.getTime() - submit.getTime();
  return Math.floor(diffTime / (1000 * 60 * 60 * 24));
}

// Helper function to format date as M/D
function formatDate(dateStr?: string): string {
  if (!dateStr || typeof dateStr !== 'string') return '';
  // Parse date string as local date to avoid timezone issues
  const datePart = dateStr.split('T')[0];
  if (!datePart) return '';
  const [year, month, day] = datePart.split('-');
  return `${parseInt(month)}/${parseInt(day)}`;
}

// Main function to calculate milestone state
export function calculateMilestoneState(project: QuickbaseProject, milestoneId: MilestoneId): MilestoneState {
  // Check if project is on hold
  const projectStatus = project[PROJECT_FIELDS.PROJECT_STATUS]?.value || '';
  if (detectHoldStatus(projectStatus)) {
    return 'on-hold';
  }

  // Switch on milestone and call appropriate calculator
  switch (milestoneId) {
    case 'intake':
      return calculateIntakeState(project);
    case 'survey':
      return calculateSurveyState(project);
    case 'design':
      return calculateDesignState(project);
    case 'nem':
      return calculateNEMState(project);
    case 'permit':
      return calculatePermitState(project);
    case 'install':
      return calculateInstallState(project);
    case 'inspection':
      return calculateInspectionState(project);
    default:
      return 'pending';
  }
}

// Intake milestone calculator
function calculateIntakeState(project: QuickbaseProject): MilestoneState {
  const intakeCompletedDate = project[PROJECT_FIELDS.INTAKE_COMPLETED_DATE]?.value;
  const firstPassResult = project[PROJECT_FIELDS.INTAKE_FIRST_PASS_FINANCE_APPROVED]?.value;
  const projectAge = getProjectAge(project); // Use calculated age from SALES_DATE

  // Check if intake completed (final approval)
  if (intakeCompletedDate) {
    return 'complete';
  }

  // Check if rejected on first pass and awaiting resubmit
  if (firstPassResult === 'Reject') {
    return 'rejected';
  }

  // Check if overdue (more than 7 days waiting for intake)
  if (projectAge > 7) {
    return 'overdue';
  }

  // Otherwise, intake is pending/in-progress
  return 'in-progress';
}

// Survey milestone calculator
function calculateSurveyState(project: QuickbaseProject): MilestoneState {
  const surveyApproved = project[PROJECT_FIELDS.SURVEY_APPROVED]?.value;
  const surveySubmitted = project[PROJECT_FIELDS.SURVEY_SUBMITTED]?.value;

  if (surveyApproved) {
    return 'complete';
  }

  if (surveySubmitted) {
    return 'in-progress';
  }

  return 'pending';
}

// Design milestone calculator
function calculateDesignState(project: QuickbaseProject): MilestoneState {
  const surveyApproved = project[PROJECT_FIELDS.SURVEY_APPROVED]?.value;
  const cadDesignApproved = project[PROJECT_FIELDS.CAD_DESIGN_APPROVED]?.value;
  const designCompleted = project[PROJECT_FIELDS.DESIGN_COMPLETED]?.value;
  const projectAge = getProjectAge(project); // Use calculated age from SALES_DATE

  if (!surveyApproved) {
    return 'pending';
  }

  if (cadDesignApproved) {
    return 'complete';
  }

  if (designCompleted) {
    return 'in-progress';
  }

  if (projectAge > 21) {
    return 'overdue';
  }

  return 'in-progress';
}

// NEM milestone calculator
function calculateNEMState(project: QuickbaseProject): MilestoneState {
  const cadDesignApproved = project[PROJECT_FIELDS.CAD_DESIGN_APPROVED]?.value;
  const nemApproved = project[PROJECT_FIELDS.NEM_APPROVED]?.value;
  const nemSubmitted = project[PROJECT_FIELDS.NEM_SUBMITTED]?.value;

  if (!cadDesignApproved) {
    return 'pending';
  }

  if (nemApproved) {
    return 'complete';
  }

  if (nemSubmitted) {
    return 'in-progress';
  }

  return 'in-progress';
}

// Permit milestone calculator
function calculatePermitState(project: QuickbaseProject): MilestoneState {
  const cadDesignApproved = project[PROJECT_FIELDS.CAD_DESIGN_APPROVED]?.value;
  const nemApproved = project[PROJECT_FIELDS.NEM_APPROVED]?.value;
  const permitApproved = project[PROJECT_FIELDS.PERMIT_APPROVED]?.value;
  const permitSubmitted = project[PROJECT_FIELDS.PERMIT_SUBMITTED]?.value;

  if (!cadDesignApproved || !nemApproved) {
    return 'pending';
  }

  if (permitApproved) {
    return 'complete';
  }

  if (permitSubmitted) {
    return 'in-progress';
  }

  return 'in-progress';
}

// Install milestone calculator
function calculateInstallState(project: QuickbaseProject): MilestoneState {
  const nemApproved = project[PROJECT_FIELDS.NEM_APPROVED]?.value;
  const permitApproved = project[PROJECT_FIELDS.PERMIT_APPROVED]?.value;
  const installCompletedDate = project[PROJECT_FIELDS.INSTALL_COMPLETED_DATE]?.value;
  const installationCompleted = project[PROJECT_FIELDS.INSTALLATION_COMPLETED_AT]?.value;
  const installScheduledDate = project[PROJECT_FIELDS.INSTALL_SCHEDULED_DATE_CAPTURE]?.value;
  const estimatedInstallDate = project[PROJECT_FIELDS.ESTIMATED_INSTALL_DATE]?.value;
  const installScheduledStartDate = project[PROJECT_FIELDS.INSTALL_SCHEDULED_START_DATE]?.value;

  if (!nemApproved || !permitApproved) {
    return 'pending';
  }

  if (installCompletedDate || installationCompleted) {
    return 'complete';
  }

  if (installScheduledDate || estimatedInstallDate || installScheduledStartDate) {
    return 'in-progress';
  }

  return 'in-progress';
}

// Inspection milestone calculator
function calculateInspectionState(project: QuickbaseProject): MilestoneState {
  const installCompletedDate = project[PROJECT_FIELDS.INSTALL_COMPLETED_DATE]?.value;
  const installationCompleted = project[PROJECT_FIELDS.INSTALLATION_COMPLETED_AT]?.value;
  const ptoApproved = project[PROJECT_FIELDS.PTO_APPROVED]?.value;
  const passingInspectionCompleted = project[PROJECT_FIELDS.PASSING_INSPECTION_COMPLETED]?.value;

  if (!installCompletedDate && !installationCompleted) {
    return 'pending';
  }

  if (ptoApproved) {
    return 'complete';
  }

  if (passingInspectionCompleted) {
    return 'in-progress';
  }

  return 'in-progress';
}

// Get current milestone ID (first in-progress milestone)
export function getCurrentMilestoneId(project: QuickbaseProject): MilestoneId {
  const milestones: MilestoneId[] = ['inspection', 'install', 'permit', 'nem', 'design', 'survey', 'intake'];
  
  for (const milestoneId of milestones) {
    const state = calculateMilestoneState(project, milestoneId);
    if (state === 'in-progress') {
      return milestoneId;
    }
  }
  
  return 'intake';
}

// Get descriptive status text for current milestone
export function getMilestoneStatusText(project: QuickbaseProject): string {
  const projectStatus = project[PROJECT_FIELDS.PROJECT_STATUS]?.value || '';
  const currentMilestone = getCurrentMilestoneId(project);

  switch (currentMilestone) {
    case 'intake':
      const intakeCompletedDate = project[PROJECT_FIELDS.INTAKE_COMPLETED_DATE]?.value;
      const firstPassResult = project[PROJECT_FIELDS.INTAKE_FIRST_PASS_FINANCE_APPROVED]?.value;

      // Check if intake completed (approved)
      if (intakeCompletedDate) {
        return '✅ Intake Approved • Ready for survey';
      }

      // Check if rejected on first pass
      if (firstPassResult === 'Reject') {
        return '⛔ Intake Rejected - Awaiting Resubmit';
      }

      const projectAge = getProjectAge(project);
      if (projectAge > 7) {
        return `⚠️ Pending Intake • ${projectAge}d overdue`;
      }

      return `⏳ Pending Intake • ${projectAge}d waiting`;

    case 'survey':
      const surveyApproved = project[PROJECT_FIELDS.SURVEY_APPROVED]?.value;
      const surveySubmitted = project[PROJECT_FIELDS.SURVEY_SUBMITTED]?.value;

      if (surveyApproved) {
        return `✅ Survey Approved ${formatDate(surveyApproved)}`;
      }

      if (surveySubmitted) {
        const daysWaiting = calculateDaysWaiting(surveySubmitted);
        return `📅 Survey Scheduled ${formatDate(surveySubmitted)} • ${daysWaiting}d ago`;
      }
      return '📋 Survey • Awaiting schedule';

    case 'design':
      const cadApproved = project[PROJECT_FIELDS.CAD_DESIGN_APPROVED]?.value;
      const designCompleted = project[PROJECT_FIELDS.DESIGN_COMPLETED]?.value;

      if (cadApproved) {
        return `✅ Design Approved ${formatDate(cadApproved)}`;
      }

      if (designCompleted) {
        return `🎨 Design • Awaiting CAD approval`;
      }

      return '🎨 Design • CAD in progress';

    case 'nem':
      const nemApproved = project[PROJECT_FIELDS.NEM_APPROVED]?.value;
      const nemSubmitted = project[PROJECT_FIELDS.NEM_SUBMITTED]?.value;

      if (nemApproved) {
        return `✅ NEM Approved ${formatDate(nemApproved)}`;
      }

      if (nemSubmitted) {
        const daysWaiting = calculateDaysWaiting(nemSubmitted);
        return `⚡ NEM • Submitted ${formatDate(nemSubmitted)} (${daysWaiting}d waiting)`;
      }
      return '⚡ NEM • Preparing submission';

    case 'permit':
      const permitApproved = project[PROJECT_FIELDS.PERMIT_APPROVED]?.value;
      const permitSubmitted = project[PROJECT_FIELDS.PERMIT_SUBMITTED]?.value;

      if (permitApproved) {
        return `✅ Permit Approved ${formatDate(permitApproved)}`;
      }

      if (permitSubmitted) {
        const daysWaiting = calculateDaysWaiting(permitSubmitted);
        return `📄 Permit • Submitted ${formatDate(permitSubmitted)} (${daysWaiting}d waiting)`;
      }
      return '📄 Permit • Preparing submission';

    case 'install':
      const installCompleted = project[PROJECT_FIELDS.INSTALL_COMPLETED_DATE]?.value;
      const installStarted = project[PROJECT_FIELDS.INSTALL_STARTED_DATE]?.value;
      const installScheduled = project[PROJECT_FIELDS.INSTALL_SCHEDULED_DATE_CAPTURE]?.value;
      const estimatedInstall = project[PROJECT_FIELDS.ESTIMATED_INSTALL_DATE]?.value;

      if (installCompleted) {
        return `✅ Install Complete ${formatDate(installCompleted)}`;
      }

      if (installStarted) {
        return `🔧 Install • In progress since ${formatDate(installStarted)}`;
      }
      if (installScheduled) {
        return `📅 Install • Scheduled ${formatDate(installScheduled)}`;
      }
      if (estimatedInstall) {
        return `📅 Install • Estimated ${formatDate(estimatedInstall)}`;
      }
      return '🔧 Install • Ready to schedule';

    case 'inspection':
      const ptoApproved = project[PROJECT_FIELDS.PTO_APPROVED]?.value;
      const inspectionPassed = project[PROJECT_FIELDS.PASSING_INSPECTION_COMPLETED]?.value;

      if (ptoApproved) {
        return `✅ PTO Approved ${formatDate(ptoApproved)} • Complete!`;
      }

      if (inspectionPassed) {
        const daysWaiting = calculateDaysWaiting(inspectionPassed);
        return `✅ Inspection Passed ${formatDate(inspectionPassed)} • Awaiting PTO (${daysWaiting}d)`;
      }
      return '🔍 Inspection • Pending';

    default:
      return 'Project in progress';
  }
}
