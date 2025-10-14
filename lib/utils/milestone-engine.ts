/**
 * Milestone Engine v2.0
 *
 * Single source of truth for milestone state calculations.
 * Reads from /lib/config/milestones.json and provides consistent
 * milestone states across all components (traffic lights, timeline, status badges).
 *
 * Priority Order:
 * 1. Check STATUS field first (100% usage where available)
 * 2. Check PRIMARY completion/date fields
 * 3. Fall back to BACKUP timestamp fields
 * 4. Fall back to CALCULATED or ESTIMATED fields
 * 5. Show 'Not Available' if all fields empty
 */

import staticMilestonesConfig from '@/lib/config/milestones.json';
import { PROJECT_FIELDS } from '@/lib/constants/fieldIds';
import type { QuickbaseProject } from '@/lib/types/project';

// ============================================================================
// TYPES
// ============================================================================

export interface MilestoneConfiguration {
  version: string;
  lastUpdated: string;
  milestones: any[];
}

export type MilestoneState =
  | 'complete'
  | 'in-progress'
  | 'ready-for'
  | 'pending'
  | 'blocked'
  | 'overdue'
  | 'not-applicable';

export type MilestoneUrgency = 'normal' | 'warning' | 'urgent' | 'critical';

export interface MilestoneStatus {
  id: string;
  name: string;
  state: MilestoneState;
  urgency: MilestoneUrgency;
  date: Date | null;
  completedDate: Date | null;
  scheduledDate: Date | null;
  estimatedDate: Date | null;
  substeps: SubstepStatus[];
  blockedReason?: string;
  helperData?: Record<string, any>;
  daysInProgress?: number;
  daysOverdue?: number;
}

export interface SubstepStatus {
  id: string;
  name: string;
  state: 'complete' | 'in-progress' | 'pending';
  date: Date | null;
  order: number;
}

export interface TrafficLight {
  milestoneId: string;
  label: string;
  state: MilestoneState;
  urgency: MilestoneUrgency;
  date: Date | null;
}

// ============================================================================
// CONFIGURATION HELPERS
// ============================================================================

/**
 * Gets milestone configuration from provided config or falls back to static JSON
 */
function getMilestoneConfig(milestoneId: string, config?: MilestoneConfiguration) {
  const configToUse = config || staticMilestonesConfig;
  return configToUse.milestones.find(m => m.id === milestoneId);
}

/**
 * Gets all milestone configurations from provided config or falls back to static JSON
 */
function getAllMilestoneConfigs(config?: MilestoneConfiguration) {
  const configToUse = config || staticMilestonesConfig;
  return configToUse.milestones;
}

// ============================================================================
// FIELD VALUE GETTERS
// ============================================================================

/**
 * Gets field value from project, handling different field types
 */
function getFieldValue(project: QuickbaseProject, fieldId: number): any {
  const field = (project as any)[fieldId];
  if (field === undefined || field === null) {
    return null;
  }
  // QuickBase fields are structured as { fieldId: { value: ... } }
  const value = field.value;
  if (value === undefined || value === null || value === '') {
    return null;
  }
  return value;
}

/**
 * Gets boolean field value (checkbox)
 */
function getBooleanField(project: QuickbaseProject, fieldId: number): boolean {
  const value = getFieldValue(project, fieldId);
  if (value === null) return false;
  if (typeof value === 'boolean') return value;
  if (typeof value === 'string') {
    const lowerValue = value.toLowerCase();
    return lowerValue === 'true' || lowerValue === 'yes' || lowerValue === '1';
  }
  if (typeof value === 'number') return value === 1;
  return false;
}

/**
 * Gets date field value
 */
function getDateField(project: QuickbaseProject, fieldId: number): Date | null {
  const value = getFieldValue(project, fieldId);
  if (!value) return null;

  const date = new Date(value);
  if (isNaN(date.getTime())) return null;

  return date;
}

/**
 * Gets string field value
 */
function getStringField(project: QuickbaseProject, fieldId: number): string | null {
  const value = getFieldValue(project, fieldId);
  if (!value) return null;
  return String(value);
}

/**
 * Gets number field value
 */
function getNumberField(project: QuickbaseProject, fieldId: number): number | null {
  const value = getFieldValue(project, fieldId);
  if (value === null || value === undefined) return null;
  const num = Number(value);
  if (isNaN(num)) return null;
  return num;
}

// ============================================================================
// PROJECT STATE HELPERS
// ============================================================================

/**
 * Checks if project is in an active state
 */
function isProjectActive(project: QuickbaseProject): boolean {
  const status = getStringField(project, PROJECT_FIELDS.PROJECT_STATUS);
  if (!status) return true; // Assume active if no status

  const inactiveStatuses = ['rejected', 'cancelled', 'on hold', 'dead'];
  return !inactiveStatuses.some(s => status.toLowerCase().includes(s));
}

/**
 * Checks if project is on hold
 */
function isProjectOnHold(project: QuickbaseProject): boolean {
  return getBooleanField(project, PROJECT_FIELDS.ON_HOLD);
}

/**
 * Gets hold reason if project is on hold
 */
function getHoldReason(project: QuickbaseProject): string | null {
  if (!isProjectOnHold(project)) return null;
  return getStringField(project, PROJECT_FIELDS.HOLD_REASON);
}

/**
 * Gets project age in days
 */
function getProjectAge(project: QuickbaseProject): number {
  // First try the calculated PROJECT_AGE field
  const ageField = getNumberField(project, PROJECT_FIELDS.PROJECT_AGE);
  if (ageField !== null) return ageField;

  // Fallback: Calculate from SALES_DATE
  const salesDate = getDateField(project, PROJECT_FIELDS.SALES_DATE);
  if (!salesDate) return 0;

  const now = new Date();
  const diffMs = now.getTime() - salesDate.getTime();
  return Math.floor(diffMs / (1000 * 60 * 60 * 24));
}

// ============================================================================
// MILESTONE COMPLETION DETECTION
// ============================================================================

/**
 * Checks if milestone is complete based on configuration
 */
function isMilestoneComplete(project: QuickbaseProject, milestoneId: string, dynamicConfig?: MilestoneConfiguration): boolean {
  const config = getMilestoneConfig(milestoneId, dynamicConfig);
  if (!config) return false;

  // Special handling for Permitting milestone (requires ALL permits approved)
  if (milestoneId === 'permitting') {
    return isPermittingComplete(project);
  }

  // 1. Check STATUS field first (highest priority)
  if (config.statusField) {
    const statusValue = getStringField(project, config.statusField.fieldId);
    if (statusValue) {
      // Status field values that indicate completion
      const completeValues = ['complete', 'completed', 'approved', 'received', 'passed'];
      if (completeValues.some(cv => statusValue.toLowerCase().includes(cv))) {
        return true;
      }
    }
  }

  // 2. Check PRIMARY completion fields
  if (config.completionFields?.primary) {
    for (const field of config.completionFields.primary) {
      // Check if this is a boolean field or if checkValue is specified
      const checkValue = (field as any).checkValue;
      if (checkValue) {
        // Status field with specific value to check
        const value = getStringField(project, field.fieldId);
        if (value && value.toLowerCase().includes(checkValue.toLowerCase())) {
          return true;
        }
      } else {
        // Date field - check if it exists
        const dateValue = getDateField(project, field.fieldId);
        if (dateValue) {
          return true;
        }
      }
    }
  }

  return false;
}

/**
 * Checks if milestone is in progress
 */
function isMilestoneInProgress(project: QuickbaseProject, milestoneId: string, dynamicConfig?: MilestoneConfiguration): boolean {
  const config = getMilestoneConfig(milestoneId, dynamicConfig);
  if (!config) return false;

  // If already complete, not in progress
  if (isMilestoneComplete(project, milestoneId, dynamicConfig)) {
    return false;
  }

  // Check STATUS field for in-progress values
  if (config.statusField) {
    const statusValue = getStringField(project, config.statusField.fieldId);
    if (statusValue) {
      const inProgressValues = ['in progress', 'processing', 'submitted', 'pending', 'scheduled', 'started'];
      if (inProgressValues.some(ipv => statusValue.toLowerCase().includes(ipv))) {
        return true;
      }
    }
  }

  // Check substeps - if any substep is complete, milestone is in progress
  if (config.inProgressFields?.substeps) {
    for (const substep of config.inProgressFields.substeps) {
      if (getBooleanField(project, substep.fieldId) || getDateField(project, substep.fieldId)) {
        return true;
      }
    }
  }

  return false;
}

/**
 * Checks if milestone dependencies are met
 */
function areDependenciesMet(project: QuickbaseProject, milestoneId: string, dynamicConfig?: MilestoneConfiguration): boolean {
  const config = getMilestoneConfig(milestoneId, dynamicConfig);
  if (!config || !config.dependencies || config.dependencies.length === 0) {
    return true;
  }

  // All dependencies must be complete
  for (const depId of config.dependencies) {
    if (!isMilestoneComplete(project, depId, dynamicConfig)) {
      return false;
    }
  }

  return true;
}

/**
 * Checks if milestone is blocked
 */
function isMilestoneBlocked(project: QuickbaseProject, milestoneId: string): { blocked: boolean; reason?: string } {
  // Project on hold blocks all milestones
  if (isProjectOnHold(project)) {
    return { blocked: true, reason: getHoldReason(project) || 'Project on hold' };
  }

  // Project cancelled/rejected
  if (!isProjectActive(project)) {
    const status = getStringField(project, PROJECT_FIELDS.PROJECT_STATUS);
    return { blocked: true, reason: `Project ${status}` };
  }

  // Special handling for Permitting milestone (NEM signatures > 7 days)
  if (milestoneId === 'permitting') {
    const permittingBlocked = isPermittingBlocked(project);
    if (permittingBlocked.blocked) {
      return permittingBlocked;
    }
  }

  // NOTE: Dependencies not met is NOT a blocked state - it's a pending state
  // This is handled in getMilestoneStatus logic

  return { blocked: false };
}

/**
 * Checks if milestone is not applicable (e.g., HOA for non-HOA projects)
 */
function isMilestoneNotApplicable(project: QuickbaseProject, milestoneId: string): boolean {
  // Note: HOA milestone no longer exists separately - it's part of Permitting
  // This function is kept for future conditional milestones

  return false;
}

/**
 * Special completion check for Permitting milestone (combines NEM, Permit, and optional HOA)
 */
function isPermittingComplete(project: QuickbaseProject): boolean {
  // Check if NEM is approved
  const nemApproved = getDateField(project, PROJECT_FIELDS.NEM_APPROVED);

  // Check if Permit is approved
  const permitApproved = getDateField(project, PROJECT_FIELDS.PERMIT_APPROVED);

  // Check if HOA applies and is approved if needed
  const hoaSubmitted = getDateField(project, PROJECT_FIELDS.HOA_APPLICATION_SUBMITTED);
  const hoaApproved = getDateField(project, PROJECT_FIELDS.HOA_APPLICATION_APPROVED);

  // If HOA was submitted, it must be approved too
  const hoaRequired = !!hoaSubmitted;
  const hoaCompleteIfRequired = hoaRequired ? !!hoaApproved : true;

  // All required permits must be approved
  return !!nemApproved && !!permitApproved && hoaCompleteIfRequired;
}

/**
 * Special blocked check for Permitting milestone (NEM signatures > 7 days)
 */
function isPermittingBlocked(project: QuickbaseProject): { blocked: boolean; reason?: string } {
  // Check if NEM signatures were sent but not submitted after 7 days
  const signaturesSent = getDateField(project, PROJECT_FIELDS.NEM_SIGNATURES_SENT);
  const nemSubmitted = getDateField(project, PROJECT_FIELDS.NEM_SUBMITTED);

  if (signaturesSent && !nemSubmitted) {
    const now = new Date();
    const daysSinceSent = Math.floor((now.getTime() - signaturesSent.getTime()) / (1000 * 60 * 60 * 24));

    if (daysSinceSent > 7) {
      return { blocked: true, reason: 'Customer signatures needed - follow up!' };
    }
  }

  return { blocked: false };
}

// ============================================================================
// MILESTONE DATE GETTERS
// ============================================================================

/**
 * Gets milestone completion date with fallback logic
 */
function getMilestoneCompletionDate(project: QuickbaseProject, milestoneId: string, dynamicConfig?: MilestoneConfiguration): Date | null {
  const config = getMilestoneConfig(milestoneId, dynamicConfig);
  if (!config) return null;

  // 1. Primary completion fields
  if (config.completionFields?.primary && Array.isArray(config.completionFields.primary)) {
    for (const field of config.completionFields.primary) {
      const date = getDateField(project, field.fieldId);
      if (date) return date;
    }
  }

  // 2. Backup completion fields
  if (config.completionFields?.backup && Array.isArray(config.completionFields.backup)) {
    for (const field of config.completionFields.backup) {
      const date = getDateField(project, field.fieldId);
      if (date) return date;
    }
  }

  return null;
}

/**
 * Gets milestone scheduled date with fallback logic
 */
function getMilestoneScheduledDate(project: QuickbaseProject, milestoneId: string, dynamicConfig?: MilestoneConfiguration): Date | null {
  const config = getMilestoneConfig(milestoneId, dynamicConfig);
  if (!config) return null;

  // Check scheduledFields (new structure)
  const scheduledFields = (config as any).scheduledFields;
  if (scheduledFields?.primary) {
    const date = getDateField(project, scheduledFields.primary.fieldId);
    if (date) return date;
  }

  // Also check inProgressFields for substeps that might have scheduled dates
  if (config.inProgressFields?.substeps) {
    for (const substep of config.inProgressFields.substeps) {
      // Look for fields with "scheduled" in the label
      if (substep.label && substep.label.toLowerCase().includes('scheduled')) {
        const date = getDateField(project, substep.fieldId);
        if (date) return date;
      }
    }
  }

  return null;
}

/**
 * Gets milestone estimated date with fallback logic
 */
function getMilestoneEstimatedDate(project: QuickbaseProject, milestoneId: string, dynamicConfig?: MilestoneConfiguration): Date | null {
  const config = getMilestoneConfig(milestoneId, dynamicConfig);
  if (!config) return null;

  // Check helper fields for estimated dates
  if (config.helperFields && Array.isArray(config.helperFields)) {
    for (const field of config.helperFields) {
      const fieldType = (field as any).type;
      if (fieldType === 'estimated_date' || fieldType === 'tentative_date') {
        const date = getDateField(project, field.fieldId);
        if (date) return date;
      }
    }
  }

  return null;
}

/**
 * Gets the best available date for a milestone
 */
function getMilestoneBestDate(project: QuickbaseProject, milestoneId: string, dynamicConfig?: MilestoneConfiguration): Date | null {
  // Priority: Completed > Scheduled > Estimated
  const completed = getMilestoneCompletionDate(project, milestoneId, dynamicConfig);
  if (completed) return completed;

  const scheduled = getMilestoneScheduledDate(project, milestoneId, dynamicConfig);
  if (scheduled) return scheduled;

  const estimated = getMilestoneEstimatedDate(project, milestoneId, dynamicConfig);
  if (estimated) return estimated;

  return null;
}

// ============================================================================
// MILESTONE URGENCY DETECTION
// ============================================================================

/**
 * Calculates urgency level for a milestone
 */
function getMilestoneUrgency(project: QuickbaseProject, milestoneId: string, state: MilestoneState): MilestoneUrgency {
  // Completed milestones are never urgent
  if (state === 'complete') return 'normal';

  // Pending and ready-for milestones are never urgent (waiting to start)
  if (state === 'pending' || state === 'ready-for') return 'normal';

  // Blocked/NA milestones are never urgent
  if (state === 'blocked' || state === 'not-applicable') return 'normal';

  // For in-progress or overdue milestones, check SLA and dates
  const config = getMilestoneConfig(milestoneId);
  if (!config) return 'normal';

  // Check project age
  const projectAge = getProjectAge(project);

  // Check if overdue
  const scheduledDate = getMilestoneScheduledDate(project, milestoneId);
  if (scheduledDate) {
    const now = new Date();
    const daysOverdue = Math.floor((now.getTime() - scheduledDate.getTime()) / (1000 * 60 * 60 * 24));

    if (daysOverdue > 14) return 'critical';
    if (daysOverdue > 7) return 'urgent';
    if (daysOverdue > 3) return 'warning';
  }

  // Check SLA deadline (for milestones that have it)
  if (milestoneId === 'design') {
    const slaDeadline = getDateField(project, PROJECT_FIELDS.DESIGN_SLA_DEADLINE);
    if (slaDeadline) {
      const now = new Date();
      const daysUntilDeadline = Math.floor((slaDeadline.getTime() - now.getTime()) / (1000 * 60 * 60 * 24));

      if (daysUntilDeadline < 0) return 'critical'; // Past deadline
      if (daysUntilDeadline <= 2) return 'urgent';
      if (daysUntilDeadline <= 5) return 'warning';
    }
  }

  // Age-based urgency for intake
  if (milestoneId === 'intake' && state === 'in-progress') {
    if (projectAge > 30) return 'critical';
    if (projectAge > 21) return 'urgent';
    if (projectAge > 14) return 'warning';
  }

  return 'normal';
}

// ============================================================================
// SUBSTEP DETECTION
// ============================================================================

/**
 * Gets substep statuses for a milestone
 */
function getMilestoneSubsteps(project: QuickbaseProject, milestoneId: string, dynamicConfig?: MilestoneConfiguration): SubstepStatus[] {
  const config = getMilestoneConfig(milestoneId, dynamicConfig);
  if (!config || !config.inProgressFields?.substeps) {
    return [];
  }

  const substeps: SubstepStatus[] = [];

  for (let i = 0; i < config.inProgressFields.substeps.length; i++) {
    const substepConfig = config.inProgressFields.substeps[i];
    const isComplete = getBooleanField(project, substepConfig.fieldId);
    const date = getDateField(project, substepConfig.fieldId);

    // Determine state
    let state: 'complete' | 'in-progress' | 'pending' = 'pending';
    if (isComplete || date) {
      state = 'complete';
    } else {
      // Check if this substep is the current active one
      const previousComplete = substeps.every(s => s.state === 'complete');
      if (previousComplete && isMilestoneInProgress(project, milestoneId, dynamicConfig)) {
        state = 'in-progress';
      }
    }

    substeps.push({
      id: (substepConfig as any).id || `substep-${substepConfig.fieldId}`,
      name: (substepConfig as any).label || substepConfig.fieldName,
      state,
      date,
      order: i + 1,
    });
  }

  return substeps;
}

// ============================================================================
// HELPER DATA EXTRACTION
// ============================================================================

/**
 * Gets helper data for a milestone (duration, audit info, etc.)
 */
function getMilestoneHelperData(project: QuickbaseProject, milestoneId: string, dynamicConfig?: MilestoneConfiguration): Record<string, any> {
  const config = getMilestoneConfig(milestoneId, dynamicConfig);
  if (!config || !config.helperFields) {
    return {};
  }

  const data: Record<string, any> = {};

  for (const helper of config.helperFields) {
    const value = getFieldValue(project, helper.fieldId);
    if (value !== null) {
      const key = (helper as any).id || helper.fieldName;
      data[key] = value;
    }
  }

  return data;
}

// ============================================================================
// MAIN MILESTONE STATUS FUNCTION
// ============================================================================

/**
 * Gets complete milestone status
 */
export function getMilestoneStatus(project: QuickbaseProject, milestoneId: string, dynamicConfig?: MilestoneConfiguration): MilestoneStatus {
  const config = getMilestoneConfig(milestoneId, dynamicConfig);
  if (!config) {
    throw new Error(`Milestone configuration not found: ${milestoneId}`);
  }

  // Determine state
  let state: MilestoneState = 'pending';
  let blockedReason: string | undefined;

  // Check not applicable first
  if (isMilestoneNotApplicable(project, milestoneId)) {
    state = 'not-applicable';
  }
  // Check blocked
  else {
    const blockedStatus = isMilestoneBlocked(project, milestoneId);
    if (blockedStatus.blocked) {
      state = 'blocked';
      blockedReason = blockedStatus.reason;
    }
    // Check complete
    else if (isMilestoneComplete(project, milestoneId, dynamicConfig)) {
      state = 'complete';
    }
    // Check in progress
    else if (isMilestoneInProgress(project, milestoneId, dynamicConfig)) {
      state = 'in-progress';

      // DISABLED: Overdue state logic (will be enabled in future phase)
      // const scheduledDate = getMilestoneScheduledDate(project, milestoneId);
      // if (scheduledDate && scheduledDate < new Date()) {
      //   state = 'overdue';
      // }
    }
    // Check if ready to start (dependencies met but not started)
    else if (areDependenciesMet(project, milestoneId, dynamicConfig)) {
      state = 'ready-for';
    }
    // Otherwise pending (dependencies not met)
  }

  // Get dates
  const completedDate = getMilestoneCompletionDate(project, milestoneId, dynamicConfig);
  const scheduledDate = getMilestoneScheduledDate(project, milestoneId, dynamicConfig);
  const estimatedDate = getMilestoneEstimatedDate(project, milestoneId, dynamicConfig);
  const bestDate = getMilestoneBestDate(project, milestoneId, dynamicConfig);

  // Calculate urgency
  const urgency = getMilestoneUrgency(project, milestoneId, state);

  // Get substeps
  const substeps = getMilestoneSubsteps(project, milestoneId, dynamicConfig);

  // Get helper data
  const helperData = getMilestoneHelperData(project, milestoneId, dynamicConfig);

  // Calculate days in progress/overdue
  let daysInProgress: number | undefined;
  let daysOverdue: number | undefined;

  if (state === 'in-progress') { // || state === 'overdue') {
    // Find when milestone started - priority order:
    // 1. Scheduled date (if available)
    // 2. First substep date (if available)
    // 3. Previous milestone completion date (fallback)
    let startDate = scheduledDate || substeps.find(s => s.date)?.date;

    // If no direct start date, use previous milestone's completion date
    if (!startDate && config.dependencies && config.dependencies.length > 0) {
      const prevMilestoneId = config.dependencies[config.dependencies.length - 1];
      const prevCompletionDate = getMilestoneCompletionDate(project, prevMilestoneId, dynamicConfig);
      if (prevCompletionDate) {
        startDate = prevCompletionDate;
      }
    }

    if (startDate) {
      const now = new Date();
      daysInProgress = Math.floor((now.getTime() - startDate.getTime()) / (1000 * 60 * 60 * 24));
    }
  }

  // DISABLED: Overdue days calculation (will be enabled in future phase)
  // if (state === 'overdue' && scheduledDate) {
  //   const now = new Date();
  //   daysOverdue = Math.floor((now.getTime() - scheduledDate.getTime()) / (1000 * 60 * 60 * 24));
  // }

  return {
    id: milestoneId,
    name: config.name,
    state,
    urgency,
    date: bestDate,
    completedDate,
    scheduledDate,
    estimatedDate,
    substeps,
    blockedReason,
    helperData,
    daysInProgress,
    daysOverdue,
  };
}

// ============================================================================
// CURRENT MILESTONE DETECTION
// ============================================================================

/**
 * Gets the current active milestone for a project
 */
export function getCurrentMilestone(project: QuickbaseProject, dynamicConfig?: MilestoneConfiguration): string {
  const allConfigs = getAllMilestoneConfigs(dynamicConfig);

  // Track the first incomplete milestone (fallback for blocked projects)
  let firstIncomplete: string | null = null;

  // Find the first milestone that is either in-progress or pending (and not blocked)
  for (const config of allConfigs) {
    const status = getMilestoneStatus(project, config.id, dynamicConfig);

    // Skip not-applicable milestones
    if (status.state === 'not-applicable') {
      continue;
    }

    // Track first incomplete milestone (for fallback)
    if (!firstIncomplete && status.state !== 'complete') {
      firstIncomplete = config.id;
    }

    // If in-progress, this is the current milestone (overdue state disabled)
    if (status.state === 'in-progress') { // || status.state === 'overdue') {
      return config.id;
    }

    // If ready to start, this is the current milestone
    if (status.state === 'ready-for') {
      return config.id;
    }

    // If pending and not blocked, this is the current milestone
    if (status.state === 'pending') {
      return config.id;
    }

    // If blocked, this is the current milestone (stuck here)
    if (status.state === 'blocked') {
      return config.id;
    }
  }

  // If we had an incomplete milestone, return it
  if (firstIncomplete) {
    return firstIncomplete;
  }

  // If all milestones are complete, return the last one
  const lastMilestone = allConfigs[allConfigs.length - 1];
  return lastMilestone.id;
}

// ============================================================================
// TRAFFIC LIGHT GENERATION
// ============================================================================

/**
 * Gets all milestones as traffic lights
 */
export function getTrafficLights(project: QuickbaseProject, dynamicConfig?: MilestoneConfiguration): TrafficLight[] {
  const allConfigs = getAllMilestoneConfigs(dynamicConfig);
  const lights: TrafficLight[] = [];

  for (const config of allConfigs) {
    const status = getMilestoneStatus(project, config.id, dynamicConfig);

    // Skip not-applicable milestones in traffic lights
    if (status.state === 'not-applicable') {
      continue;
    }

    lights.push({
      milestoneId: config.id,
      label: config.name,
      state: status.state,
      urgency: status.urgency,
      date: status.date,
    });
  }

  return lights;
}

/**
 * Gets all milestone statuses for a project
 */
export function getAllMilestoneStatuses(project: QuickbaseProject, dynamicConfig?: MilestoneConfiguration): MilestoneStatus[] {
  const allConfigs = getAllMilestoneConfigs(dynamicConfig);
  return allConfigs.map(config => getMilestoneStatus(project, config.id, dynamicConfig));
}

// ============================================================================
// PROGRESS CALCULATION
// ============================================================================

/**
 * Gets overall project completion percentage
 */
export function getProjectCompletionPercentage(project: QuickbaseProject, dynamicConfig?: MilestoneConfiguration): number {
  const statuses = getAllMilestoneStatuses(project, dynamicConfig);

  // Filter out not-applicable milestones
  const applicableStatuses = statuses.filter(s => s.state !== 'not-applicable');
  if (applicableStatuses.length === 0) return 0;

  // Count completed milestones
  const completedCount = applicableStatuses.filter(s => s.state === 'complete').length;

  return Math.round((completedCount / applicableStatuses.length) * 100);
}

/**
 * Gets milestone completion percentage (based on substeps)
 */
export function getMilestoneCompletionPercentage(project: QuickbaseProject, milestoneId: string, dynamicConfig?: MilestoneConfiguration): number {
  const status = getMilestoneStatus(project, milestoneId, dynamicConfig);

  if (status.state === 'complete') return 100;
  if (status.state === 'pending' || status.state === 'blocked' || status.state === 'not-applicable') return 0;

  // Calculate based on substeps
  if (status.substeps.length > 0) {
    const completedSubsteps = status.substeps.filter(s => s.state === 'complete').length;
    return Math.round((completedSubsteps / status.substeps.length) * 100);
  }

  // If in-progress but no substeps, assume 50%
  return 50;
}

// ============================================================================
// FUNDING MILESTONE HELPERS
// ============================================================================

/**
 * Gets M1/M2/M3 funding status
 */
export function getFundingStatus(project: QuickbaseProject): {
  m1: string | null;
  m2: string | null;
  m3: string | null;
  readyForM2: boolean;
} {
  return {
    m1: getStringField(project, PROJECT_FIELDS.FUNDING_DASHBOARD_M1_STATUS),
    m2: getStringField(project, PROJECT_FIELDS.FUNDING_DASHBOARD_M2_STATUS),
    m3: getStringField(project, PROJECT_FIELDS.FUNDING_DASHBOARD_M3_STATUS),
    readyForM2: getBooleanField(project, PROJECT_FIELDS.FUNDING_DASHBOARD_READY_FOR_M2),
  };
}

// ============================================================================
// EXPORTS
// ============================================================================

export {
  // Main functions
  getMilestoneConfig,
  getAllMilestoneConfigs,

  // Field getters
  getFieldValue,
  getBooleanField,
  getDateField,
  getStringField,
  getNumberField,

  // Project state
  isProjectActive,
  isProjectOnHold,
  getHoldReason,
  getProjectAge,

  // Milestone detection
  isMilestoneComplete,
  isMilestoneInProgress,
  areDependenciesMet,
  isMilestoneBlocked,
  isMilestoneNotApplicable,

  // Date getters
  getMilestoneCompletionDate,
  getMilestoneScheduledDate,
  getMilestoneEstimatedDate,
  getMilestoneBestDate,
};
