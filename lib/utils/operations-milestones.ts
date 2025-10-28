// lib/utils/operations-milestones.ts
// Operations milestone determination and status calculation

import { parseQuickbaseDate } from './date-helpers';

// 7-stage operations milestone flow
export type OperationsMilestone =
  | 'intake'
  | 'survey'
  | 'design'
  | 'permitting'
  | 'install'
  | 'inspections'
  | 'pto';

// Milestone-specific statuses
export type IntakeStatus =
  | 'pending'
  | 'in_progress'
  | 'completed'
  | 'on_hold'
  | 'blocked';

export type SurveyStatus =
  | 'not_scheduled'
  | 'scheduled'
  | 'in_progress'
  | 'approved'
  | 'failed'
  | 'on_hold'
  | 'blocked';

export type DesignStatus =
  | 'pending'
  | 'in_progress'
  | 'completed'
  | 'changes_required'
  | 'on_hold'
  | 'blocked';

export type PermittingStatus =
  | 'preparing'
  | 'submitted'
  | 'under_review'
  | 'approved'
  | 'rejected'
  | 'on_hold'
  | 'blocked';

export type InstallStatus =
  | 'not_scheduled'
  | 'scheduled'
  | 'in_progress'
  | 'completed'
  | 'on_hold'
  | 'blocked';

export type InspectionStatus =
  | 'pending'
  | 'scheduled'
  | 'passed'
  | 'failed'
  | 'reinspection_scheduled'
  | 'on_hold'
  | 'blocked';

export type PTOStatus =
  | 'preparing'
  | 'submitted'
  | 'approved'
  | 'on_hold'
  | 'blocked';

export type MilestoneStatus =
  | IntakeStatus
  | SurveyStatus
  | DesignStatus
  | PermittingStatus
  | InstallStatus
  | InspectionStatus
  | PTOStatus;

export interface MilestoneData {
  currentMilestone: OperationsMilestone;
  milestoneStatus: MilestoneStatus;
  milestoneStartDate: Date | null;
  daysInStage: number;
}

export interface ProjectMilestoneFields {
  // Completion dates
  intakeCompletedDate?: any;
  surveyApproved?: any;
  designCompleted?: any;
  permitApproved?: any;
  installCompletedDate?: any;
  passingInspectionCompleted?: any;
  ptoApproved?: any;

  // Status fields
  intakeStatus?: any;
  surveyStatus?: any;
  designStatus?: any;
  permitStatus?: any;
  nemInterconnectionStatus?: any;
  ptoStatus?: any;

  // Schedule/in-progress fields
  surveyScheduledDate?: any;
  surveySubmitted?: any;
  designInProgress?: any;
  cadDesignSubmitted?: any;
  permitSubmitted?: any;
  installScheduledDate?: any;
  installStartedDate?: any;
  inspectionScheduledDate?: any;
  inspectionFailedDate?: any;
  ptoSubmitted?: any;

  // Hold/block fields
  onHold?: any;
  holdReason?: any;
  blockReason?: any;

  // Sales date (fallback)
  salesDate?: any;
}

/**
 * Determine which milestone a project is currently in based on completion dates
 * Logic: Find the earliest incomplete milestone
 */
export function determineCurrentMilestone(fields: ProjectMilestoneFields): OperationsMilestone {
  const intakeCompleted = parseQuickbaseDate(fields.intakeCompletedDate);
  const surveyApproved = parseQuickbaseDate(fields.surveyApproved);
  const designCompleted = parseQuickbaseDate(fields.designCompleted);
  const permitApproved = parseQuickbaseDate(fields.permitApproved);
  const installCompleted = parseQuickbaseDate(fields.installCompletedDate);
  const inspectionPassed = parseQuickbaseDate(fields.passingInspectionCompleted);
  const ptoApproved = parseQuickbaseDate(fields.ptoApproved);

  // Work backwards from PTO to find current milestone
  if (ptoApproved) {
    return 'pto'; // Completed!
  }
  if (inspectionPassed) {
    return 'pto'; // Waiting for PTO
  }
  if (installCompleted) {
    return 'inspections'; // Install done, waiting for inspection
  }
  if (permitApproved) {
    return 'install'; // Permit approved, ready for install
  }
  if (designCompleted) {
    return 'permitting'; // Design done, waiting for permit
  }
  if (surveyApproved) {
    return 'design'; // Survey done, in design phase
  }
  if (intakeCompleted) {
    return 'survey'; // Intake done, waiting for survey
  }

  // Default to intake if no milestones completed
  return 'intake';
}

/**
 * Calculate days in current stage
 * Uses the most recent milestone completion date as the stage start
 */
export function calculateDaysInStage(
  currentMilestone: OperationsMilestone,
  fields: ProjectMilestoneFields
): { days: number; startDate: Date | null } {
  let startDate: Date | null = null;

  // Determine stage start date based on previous milestone completion
  switch (currentMilestone) {
    case 'intake':
      // For intake, use sales date as start
      startDate = parseQuickbaseDate(fields.salesDate);
      break;
    case 'survey':
      // Survey starts when intake completes
      startDate = parseQuickbaseDate(fields.intakeCompletedDate);
      break;
    case 'design':
      // Design starts when survey is approved
      startDate = parseQuickbaseDate(fields.surveyApproved);
      break;
    case 'permitting':
      // Permitting starts when design completes
      startDate = parseQuickbaseDate(fields.designCompleted);
      break;
    case 'install':
      // Install stage starts when permit is approved
      startDate = parseQuickbaseDate(fields.permitApproved);
      break;
    case 'inspections':
      // Inspections start when install completes
      startDate = parseQuickbaseDate(fields.installCompletedDate);
      break;
    case 'pto':
      // PTO stage starts when inspection passes
      startDate = parseQuickbaseDate(fields.passingInspectionCompleted);
      break;
  }

  if (!startDate) {
    return { days: 0, startDate: null };
  }

  const now = new Date();
  const diffMs = now.getTime() - startDate.getTime();
  const days = Math.floor(diffMs / (1000 * 60 * 60 * 24));

  return { days: Math.max(0, days), startDate };
}

/**
 * Determine milestone-specific status based on sub-fields and activity
 */
export function determineMilestoneStatus(
  milestone: OperationsMilestone,
  fields: ProjectMilestoneFields
): MilestoneStatus {
  // Check for hold/block status first (applies to all milestones)
  const isOnHold = fields.onHold === true || fields.onHold === 1 || fields.onHold === '1';
  const blockReason = String(fields.blockReason || '').trim();
  const isBlocked = blockReason.length > 0;

  if (isBlocked) return 'blocked';
  if (isOnHold) return 'on_hold';

  // Milestone-specific status logic
  switch (milestone) {
    case 'intake':
      return determineIntakeStatus(fields);
    case 'survey':
      return determineSurveyStatus(fields);
    case 'design':
      return determineDesignStatus(fields);
    case 'permitting':
      return determinePermittingStatus(fields);
    case 'install':
      return determineInstallStatus(fields);
    case 'inspections':
      return determineInspectionStatus(fields);
    case 'pto':
      return determinePTOStatus(fields);
    default:
      return 'pending';
  }
}

function determineIntakeStatus(fields: ProjectMilestoneFields): IntakeStatus {
  const statusText = String(fields.intakeStatus || '').toLowerCase();

  if (statusText.includes('complete')) return 'completed';
  if (statusText.includes('in progress') || statusText.includes('started')) return 'in_progress';
  return 'pending';
}

function determineSurveyStatus(fields: ProjectMilestoneFields): SurveyStatus {
  const statusText = String(fields.surveyStatus || '').toLowerCase();
  const surveyScheduled = parseQuickbaseDate(fields.surveyScheduledDate);
  const surveySubmitted = parseQuickbaseDate(fields.surveySubmitted);

  if (statusText.includes('approved')) return 'approved';
  if (statusText.includes('failed') || statusText.includes('reject')) return 'failed';
  if (surveySubmitted) return 'in_progress';
  if (surveyScheduled) return 'scheduled';
  return 'not_scheduled';
}

function determineDesignStatus(fields: ProjectMilestoneFields): DesignStatus {
  const statusText = String(fields.designStatus || '').toLowerCase();
  const cadSubmitted = parseQuickbaseDate(fields.cadDesignSubmitted);

  if (statusText.includes('complete') || statusText.includes('approved')) return 'completed';
  if (statusText.includes('revision') || statusText.includes('changes')) return 'changes_required';
  if (cadSubmitted || statusText.includes('in progress')) return 'in_progress';
  return 'pending';
}

function determinePermittingStatus(fields: ProjectMilestoneFields): PermittingStatus {
  const statusText = String(fields.permitStatus || '').toLowerCase();
  const permitSubmitted = parseQuickbaseDate(fields.permitSubmitted);

  if (statusText.includes('approved') || statusText.includes('issued')) return 'approved';
  if (statusText.includes('reject') || statusText.includes('denied')) return 'rejected';
  if (statusText.includes('under review') || statusText.includes('processing')) return 'under_review';
  if (permitSubmitted || statusText.includes('submitted')) return 'submitted';
  return 'preparing';
}

function determineInstallStatus(fields: ProjectMilestoneFields): InstallStatus {
  const installScheduled = parseQuickbaseDate(fields.installScheduledDate);
  const installStarted = parseQuickbaseDate(fields.installStartedDate);
  const installCompleted = parseQuickbaseDate(fields.installCompletedDate);

  if (installCompleted) return 'completed';
  if (installStarted) return 'in_progress';
  if (installScheduled) return 'scheduled';
  return 'not_scheduled';
}

function determineInspectionStatus(fields: ProjectMilestoneFields): InspectionStatus {
  const inspectionScheduled = parseQuickbaseDate(fields.inspectionScheduledDate);
  const inspectionFailed = parseQuickbaseDate(fields.inspectionFailedDate);
  const inspectionPassed = parseQuickbaseDate(fields.passingInspectionCompleted);

  if (inspectionPassed) return 'passed';

  // If failed, check if reinspection is scheduled
  if (inspectionFailed) {
    if (inspectionScheduled && inspectionScheduled > inspectionFailed) {
      return 'reinspection_scheduled';
    }
    return 'failed';
  }

  if (inspectionScheduled) return 'scheduled';
  return 'pending';
}

function determinePTOStatus(fields: ProjectMilestoneFields): PTOStatus {
  const statusText = String(fields.ptoStatus || '').toLowerCase();
  const ptoSubmitted = parseQuickbaseDate(fields.ptoSubmitted);
  const ptoApproved = parseQuickbaseDate(fields.ptoApproved);

  if (ptoApproved || statusText.includes('approved')) return 'approved';
  if (ptoSubmitted || statusText.includes('submitted')) return 'submitted';
  return 'preparing';
}

/**
 * Get all milestone data for a project
 */
export function getProjectMilestoneData(fields: ProjectMilestoneFields): MilestoneData {
  const currentMilestone = determineCurrentMilestone(fields);
  const milestoneStatus = determineMilestoneStatus(currentMilestone, fields);
  const { days, startDate } = calculateDaysInStage(currentMilestone, fields);

  return {
    currentMilestone,
    milestoneStatus,
    milestoneStartDate: startDate,
    daysInStage: days,
  };
}

/**
 * Format milestone for display
 */
export function formatMilestoneName(milestone: OperationsMilestone): string {
  const names: Record<OperationsMilestone, string> = {
    intake: 'Intake',
    survey: 'Survey',
    design: 'Design',
    permitting: 'Permitting',
    install: 'Install',
    inspections: 'Inspections',
    pto: 'PTO',
  };
  return names[milestone];
}

/**
 * Format milestone status for display
 */
export function formatMilestoneStatus(status: MilestoneStatus): string {
  return status
    .split('_')
    .map(word => word.charAt(0).toUpperCase() + word.slice(1))
    .join(' ');
}

/**
 * Get color scheme for milestone badge
 */
export function getMilestoneColor(milestone: OperationsMilestone): {
  bg: string;
  text: string;
  border: string;
} {
  const colors: Record<OperationsMilestone, { bg: string; text: string; border: string }> = {
    intake: { bg: 'bg-orange-50', text: 'text-orange-700', border: 'border-orange-200' },
    survey: { bg: 'bg-purple-50', text: 'text-purple-700', border: 'border-purple-200' },
    design: { bg: 'bg-blue-50', text: 'text-blue-700', border: 'border-blue-200' },
    permitting: { bg: 'bg-teal-50', text: 'text-teal-700', border: 'border-teal-200' },
    install: { bg: 'bg-indigo-50', text: 'text-indigo-700', border: 'border-indigo-200' },
    inspections: { bg: 'bg-amber-50', text: 'text-amber-700', border: 'border-amber-200' },
    pto: { bg: 'bg-emerald-50', text: 'text-emerald-700', border: 'border-emerald-200' },
  };
  return colors[milestone];
}

/**
 * Get available statuses for a milestone (for status tabs)
 */
export function getAvailableStatusesForMilestone(milestone: OperationsMilestone): MilestoneStatus[] {
  const statusMap: Record<OperationsMilestone, MilestoneStatus[]> = {
    intake: ['pending', 'in_progress', 'completed', 'on_hold', 'blocked'],
    survey: ['not_scheduled', 'scheduled', 'in_progress', 'approved', 'failed', 'on_hold', 'blocked'],
    design: ['pending', 'in_progress', 'completed', 'changes_required', 'on_hold', 'blocked'],
    permitting: ['preparing', 'submitted', 'under_review', 'approved', 'rejected', 'on_hold', 'blocked'],
    install: ['not_scheduled', 'scheduled', 'in_progress', 'completed', 'on_hold', 'blocked'],
    inspections: ['pending', 'scheduled', 'passed', 'failed', 'reinspection_scheduled', 'on_hold', 'blocked'],
    pto: ['preparing', 'submitted', 'approved', 'on_hold', 'blocked'],
  };
  return statusMap[milestone] || [];
}
