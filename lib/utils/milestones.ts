import { PROJECT_FIELDS } from '@/lib/constants/fieldIds'
import { QuickbaseProject } from '@/lib/types/project'

/**
 * Determine which of the 7 milestones the project is currently in
 */
export function getCurrentMilestone(project: QuickbaseProject): string {
  // Check milestones in reverse order (most advanced first)

  // 7. PTO (final milestone)
  if (project[PROJECT_FIELDS.PTO_APPROVED]?.value) {
    return 'PTO'
  }

  // 6. Inspection
  if (project[PROJECT_FIELDS.PASSING_INSPECTION_COMPLETED]?.value) {
    return 'Inspection'
  }

  // 5. Installation
  if (project[PROJECT_FIELDS.INSTALL_COMPLETED_DATE]?.value) {
    return 'Installation'
  }

  if (project[PROJECT_FIELDS.INSTALL_SCHEDULED_DATE_CAPTURE]?.value) {
    return 'Installation'
  }

  // 4. Permitting (combines NEM, Permit, and HOA)
  // Check if ALL required permits are approved
  const nemApproved = project[PROJECT_FIELDS.NEM_APPROVED]?.value
  const permitApproved = project[PROJECT_FIELDS.PERMIT_APPROVED]?.value
  const hoaSubmitted = project[PROJECT_FIELDS.HOA_APPLICATION_SUBMITTED]?.value
  const hoaApproved = project[PROJECT_FIELDS.HOA_APPLICATION_APPROVED]?.value

  const allPermitsApproved = nemApproved && permitApproved && (!hoaSubmitted || hoaApproved)

  if (allPermitsApproved) {
    return 'Permitting' // Complete, ready for install
  }

  // If any permitting activity has started, we're in Permitting phase
  if (project[PROJECT_FIELDS.NEM_SIGNATURES_SENT]?.value ||
      project[PROJECT_FIELDS.NEM_SUBMITTED]?.value ||
      project[PROJECT_FIELDS.PERMIT_SUBMITTED]?.value ||
      project[PROJECT_FIELDS.HOA_APPLICATION_SUBMITTED]?.value) {
    return 'Permitting'
  }

  // 3. Design
  if (project[PROJECT_FIELDS.ENGINEERING_COMPLETED]?.value ||
      project[PROJECT_FIELDS.DESIGN_COMPLETED]?.value ||
      project[PROJECT_FIELDS.CAD_DESIGN_APPROVED]?.value) {
    return 'Design'
  }

  // 2. Survey
  if (project[PROJECT_FIELDS.SURVEY_APPROVED]?.value) {
    return 'Survey'
  }

  // 1. Intake (default/initial milestone)
  return 'Intake'
}

/**
 * Calculate percentage progress through the 7 milestones (0-100)
 */
export function getMilestoneProgress(project: QuickbaseProject): number {
  let completedMilestones = 0
  const totalMilestones = 7

  // 1. Intake - check for Intake Completed Date
  if (project[PROJECT_FIELDS.INTAKE_COMPLETED_DATE]?.value) {
    completedMilestones++
  }

  // 2. Survey - check for Survey Approved
  if (project[PROJECT_FIELDS.SURVEY_APPROVED]?.value) {
    completedMilestones++
  }

  // 3. Design - check for Engineering Completed (primary) or Design Completed
  if (project[PROJECT_FIELDS.ENGINEERING_COMPLETED]?.value ||
      project[PROJECT_FIELDS.DESIGN_COMPLETED]?.value) {
    completedMilestones++
  }

  // 4. Permitting - check if ALL required permits are approved
  const nemApproved = project[PROJECT_FIELDS.NEM_APPROVED]?.value
  const permitApproved = project[PROJECT_FIELDS.PERMIT_APPROVED]?.value
  const hoaSubmitted = project[PROJECT_FIELDS.HOA_APPLICATION_SUBMITTED]?.value
  const hoaApproved = project[PROJECT_FIELDS.HOA_APPLICATION_APPROVED]?.value
  const allPermitsApproved = nemApproved && permitApproved && (!hoaSubmitted || hoaApproved)

  if (allPermitsApproved) {
    completedMilestones++
  }

  // 5. Installation - check for Install Completed Date
  if (project[PROJECT_FIELDS.INSTALL_COMPLETED_DATE]?.value) {
    completedMilestones++
  }

  // 6. Inspection - check for Passing Inspection Completed
  if (project[PROJECT_FIELDS.PASSING_INSPECTION_COMPLETED]?.value) {
    completedMilestones++
  }

  // 7. PTO - check for PTO Approved
  if (project[PROJECT_FIELDS.PTO_APPROVED]?.value) {
    completedMilestones++
  }

  // Return percentage with 2 decimal places
  return Math.round((completedMilestones / totalMilestones) * 10000) / 100
}

/**
 * Determine urgency level based on multiple factors
 */
export function getProjectUrgency(project: QuickbaseProject): 'critical' | 'high' | 'medium' | 'low' {
  const onHold = project[PROJECT_FIELDS.ON_HOLD]?.value === 'Yes'
  const dateOnHold = project[PROJECT_FIELDS.DATE_ON_HOLD]?.value
  const priority = project[PROJECT_FIELDS.PROJECT_PRIORITY]?.value || ''
  const projectAge = parseInt(project[PROJECT_FIELDS.PROJECT_AGE]?.value || '0')
  
  // Calculate days on hold
  let daysOnHold = 0
  if (onHold && dateOnHold) {
    const holdDate = new Date(dateOnHold)
    const now = new Date()
    const diffTime = now.getTime() - holdDate.getTime()
    daysOnHold = Math.floor(diffTime / (1000 * 60 * 60 * 24))
  }
  
  // Critical urgency
  if (
    (onHold && daysOnHold > 14) ||
    priority === 'Insane' ||
    projectAge > 180
  ) {
    return 'critical'
  }
  
  // High urgency
  if (
    (onHold && daysOnHold > 7) ||
    priority === 'Urgent' ||
    projectAge > 120
  ) {
    return 'high'
  }
  
  // Medium urgency
  if (onHold || projectAge > 90) {
    return 'medium'
  }
  
  // Low urgency
  return 'low'
}

/**
 * Return Tailwind color classes for milestone badges
 */
export function getMilestoneColor(milestone: string): string {
  const colors: Record<string, string> = {
    'Intake': 'bg-gray-100 text-gray-800',
    'Survey': 'bg-blue-100 text-blue-800',
    'Design': 'bg-purple-100 text-purple-800',
    'Permitting': 'bg-indigo-100 text-indigo-800',  // New combined milestone
    'Installation': 'bg-green-100 text-green-800',
    'Inspection': 'bg-teal-100 text-teal-800',
    'PTO': 'bg-emerald-100 text-emerald-800',
  }

  return colors[milestone] || 'bg-gray-100 text-gray-800'
}

/**
 * Get the next milestone in the sequence
 */
export function getNextMilestone(currentMilestone: string): string {
  const milestoneSequence = [
    'Intake',
    'Survey',
    'Design',
    'Permitting',
    'Installation',
    'Inspection',
    'PTO'
  ]

  const currentIndex = milestoneSequence.indexOf(currentMilestone)
  if (currentIndex === -1 || currentIndex === milestoneSequence.length - 1) {
    return 'Complete'
  }

  return milestoneSequence[currentIndex + 1]
}

/**
 * Check if a milestone is overdue based on project age and typical timelines
 */
export function isMilestoneOverdue(project: QuickbaseProject, milestone: string): boolean {
  const projectAge = parseInt(project[PROJECT_FIELDS.PROJECT_AGE]?.value || '0')

  // Typical milestone timelines (in days) - based on 7-milestone structure
  const milestoneTimelines: Record<string, number> = {
    'Intake': 7,
    'Survey': 14,
    'Design': 21,
    'Permitting': 45,  // Combines HOA/NEM/Permit - longest of the three
    'Installation': 60,
    'Inspection': 75,
    'PTO': 90,
  }

  const expectedDays = milestoneTimelines[milestone] || 0
  return projectAge > expectedDays
}
