import { PROJECT_FIELDS } from '@/lib/constants/fieldIds'
import { QuickbaseProject } from '@/lib/types/project'

/**
 * Determine which of the 9 milestones the project is currently in
 */
export function getCurrentMilestone(project: QuickbaseProject): string {
  // Check milestones in reverse order (most advanced first)
  
  // 9. PTO (final milestone)
  if (project[PROJECT_FIELDS.PTO_APPROVED]?.value) {
    return 'PTO'
  }
  
  // 8. Inspection
  if (project[PROJECT_FIELDS.PASSING_INSPECTION_COMPLETED]?.value) {
    return 'Inspection'
  }
  
  // 7. Verification (1-3 days after install)
  if (project[PROJECT_FIELDS.INSTALL_COMPLETED_DATE]?.value) {
    return 'Verification'
  }
  
  // 6. Installation
  if (project[PROJECT_FIELDS.INSTALL_SCHEDULED_DATE_CAPTURE]?.value) {
    return 'Install'
  }
  
  // 5. NEM
  if (project[PROJECT_FIELDS.NEM_APPROVED]?.value) {
    return 'NEM'
  }
  
  // 4. Permitting
  if (project[PROJECT_FIELDS.PERMIT_APPROVED]?.value) {
    return 'Permit'
  }
  
  // 3. Design
  if (project[PROJECT_FIELDS.DESIGN_COMPLETED]?.value || 
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
 * Calculate percentage progress through the 9 milestones (0-100)
 */
export function getMilestoneProgress(project: QuickbaseProject): number {
  let completedMilestones = 0
  const totalMilestones = 9
  
  // Check each milestone completion field
  const milestoneFields = [
    PROJECT_FIELDS.INTAKE_INSTALL_DATE_TENTATIVE, // Always present
    PROJECT_FIELDS.SURVEY_APPROVED,
    PROJECT_FIELDS.DESIGN_COMPLETED,
    PROJECT_FIELDS.CAD_DESIGN_APPROVED,
    PROJECT_FIELDS.PERMIT_APPROVED,
    PROJECT_FIELDS.NEM_APPROVED,
    PROJECT_FIELDS.INSTALL_COMPLETED_DATE,
    // Verification is calculated, not a field
    PROJECT_FIELDS.PASSING_INSPECTION_COMPLETED,
    PROJECT_FIELDS.PTO_APPROVED,
  ]
  
  milestoneFields.forEach(field => {
    if (project[field]?.value) {
      completedMilestones++
    }
  })
  
  // Add verification milestone if install is completed
  if (project[PROJECT_FIELDS.INSTALL_COMPLETED_DATE]?.value) {
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
    'Permit': 'bg-yellow-100 text-yellow-800',
    'NEM': 'bg-orange-100 text-orange-800',
    'Install': 'bg-indigo-100 text-indigo-800',
    'Verification': 'bg-cyan-100 text-cyan-800',
    'Inspection': 'bg-teal-100 text-teal-800',
    'PTO': 'bg-green-100 text-green-800',
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
    'Permit',
    'NEM',
    'Install',
    'Verification',
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
  
  // Typical milestone timelines (in days)
  const milestoneTimelines: Record<string, number> = {
    'Intake': 7,
    'Survey': 14,
    'Design': 21,
    'Permit': 35,
    'NEM': 45,
    'Install': 60,
    'Verification': 65,
    'Inspection': 75,
    'PTO': 90,
  }
  
  const expectedDays = milestoneTimelines[milestone] || 0
  return projectAge > expectedDays
}
