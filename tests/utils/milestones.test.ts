import { describe, it, expect } from 'vitest'
import {
  getCurrentMilestone,
  getMilestoneProgress,
  getProjectUrgency,
  getMilestoneColor,
  getNextMilestone,
  isMilestoneOverdue
} from '@/lib/utils/milestones'
import { PROJECT_FIELDS } from '@/lib/constants/fieldIds'
import { QuickbaseProject } from '@/lib/types/project'

// Helper function for creating mock projects
function createMockProject(fields: Record<number, any>): QuickbaseProject {
  const project: QuickbaseProject = {}
  Object.entries(fields).forEach(([fieldId, value]) => {
    project[parseInt(fieldId)] = { value }
  })
  return project
}

describe('getCurrentMilestone', () => {
  it('returns Intake for new project with no milestone data', () => {
    const project = createMockProject({})
    const result = getCurrentMilestone(project)
    expect(result).toBe('Intake')
  })

  it('returns Survey when survey approved', () => {
    const project = createMockProject({
      [PROJECT_FIELDS.SURVEY_APPROVED]: '2024-01-01'
    })
    const result = getCurrentMilestone(project)
    expect(result).toBe('Survey')
  })

  it('returns Design when design completed', () => {
    const project = createMockProject({
      [PROJECT_FIELDS.DESIGN_COMPLETED]: '2024-01-01'
    })
    const result = getCurrentMilestone(project)
    expect(result).toBe('Design')
  })

  it('returns Permit when permit approved', () => {
    const project = createMockProject({
      [PROJECT_FIELDS.PERMIT_APPROVED]: '2024-01-01'
    })
    const result = getCurrentMilestone(project)
    expect(result).toBe('Permit')
  })

  it('returns NEM when NEM approved', () => {
    const project = createMockProject({
      [PROJECT_FIELDS.NEM_APPROVED]: '2024-01-01'
    })
    const result = getCurrentMilestone(project)
    expect(result).toBe('NEM')
  })

  it('returns Install when install scheduled', () => {
    const project = createMockProject({
      [PROJECT_FIELDS.INSTALL_SCHEDULED_DATE_CAPTURE]: '2024-01-01'
    })
    const result = getCurrentMilestone(project)
    expect(result).toBe('Install')
  })

  it('returns Verification when install completed', () => {
    const project = createMockProject({
      [PROJECT_FIELDS.INSTALL_COMPLETED_DATE]: '2024-01-01'
    })
    const result = getCurrentMilestone(project)
    expect(result).toBe('Verification')
  })

  it('returns Inspection when inspection passed', () => {
    const project = createMockProject({
      [PROJECT_FIELDS.PASSING_INSPECTION_COMPLETED]: '2024-01-01'
    })
    const result = getCurrentMilestone(project)
    expect(result).toBe('Inspection')
  })

  it('returns PTO when PTO approved (final milestone)', () => {
    const project = createMockProject({
      [PROJECT_FIELDS.PTO_APPROVED]: '2024-01-01'
    })
    const result = getCurrentMilestone(project)
    expect(result).toBe('PTO')
  })

  it('prioritizes later milestones (if both design and install complete, returns Verification)', () => {
    const project = createMockProject({
      [PROJECT_FIELDS.DESIGN_COMPLETED]: '2024-01-01',
      [PROJECT_FIELDS.INSTALL_COMPLETED_DATE]: '2024-01-02'
    })
    const result = getCurrentMilestone(project)
    expect(result).toBe('Verification')
  })
})

describe('getMilestoneProgress', () => {
  it('returns 0 for project with only intake data', () => {
    const project = createMockProject({})
    const result = getMilestoneProgress(project)
    expect(result).toBe(0)
  })

  it('returns ~11% (1/9) for project with intake only', () => {
    const project = createMockProject({
      [PROJECT_FIELDS.INTAKE_INSTALL_DATE_TENTATIVE]: '2024-01-01'
    })
    const result = getMilestoneProgress(project)
    expect(result).toBeCloseTo(11.11, 1)
  })

  it('returns ~22% (2/9) for project with intake and survey', () => {
    const project = createMockProject({
      [PROJECT_FIELDS.INTAKE_INSTALL_DATE_TENTATIVE]: '2024-01-01',
      [PROJECT_FIELDS.SURVEY_APPROVED]: '2024-01-02'
    })
    const result = getMilestoneProgress(project)
    expect(result).toBeCloseTo(22.22, 1)
  })

  it('returns ~44% (4/9) for project through design', () => {
    const project = createMockProject({
      [PROJECT_FIELDS.INTAKE_INSTALL_DATE_TENTATIVE]: '2024-01-01',
      [PROJECT_FIELDS.SURVEY_APPROVED]: '2024-01-02',
      [PROJECT_FIELDS.DESIGN_COMPLETED]: '2024-01-03',
      [PROJECT_FIELDS.PERMIT_APPROVED]: '2024-01-04'
    })
    const result = getMilestoneProgress(project)
    expect(result).toBeCloseTo(44.44, 1)
  })

  it('returns 100% for project with PTO approved', () => {
    const project = createMockProject({
      [PROJECT_FIELDS.INTAKE_INSTALL_DATE_TENTATIVE]: '2024-01-01',
      [PROJECT_FIELDS.SURVEY_APPROVED]: '2024-01-02',
      [PROJECT_FIELDS.DESIGN_COMPLETED]: '2024-01-03',
      [PROJECT_FIELDS.PERMIT_APPROVED]: '2024-01-04',
      [PROJECT_FIELDS.NEM_APPROVED]: '2024-01-05',
      [PROJECT_FIELDS.INSTALL_COMPLETED_DATE]: '2024-01-06',
      [PROJECT_FIELDS.PASSING_INSPECTION_COMPLETED]: '2024-01-07',
      [PROJECT_FIELDS.PTO_APPROVED]: '2024-01-08'
    })
    const result = getMilestoneProgress(project)
    expect(result).toBe(100)
  })

  it('handles missing fields gracefully (doesn\'t crash)', () => {
    const project = createMockProject({
      [PROJECT_FIELDS.INTAKE_INSTALL_DATE_TENTATIVE]: '2024-01-01'
    })
    const result = getMilestoneProgress(project)
    expect(result).toBeGreaterThanOrEqual(0)
    expect(result).toBeLessThanOrEqual(100)
  })

  it('counts verification milestone when install complete', () => {
    const project = createMockProject({
      [PROJECT_FIELDS.INTAKE_INSTALL_DATE_TENTATIVE]: '2024-01-01',
      [PROJECT_FIELDS.INSTALL_COMPLETED_DATE]: '2024-01-02'
    })
    const result = getMilestoneProgress(project)
    expect(result).toBeGreaterThan(11.11) // Should include verification
  })
})

describe('getProjectUrgency', () => {
  it('returns low for new project (< 90 days, not on hold)', () => {
    const project = createMockProject({
      [PROJECT_FIELDS.PROJECT_AGE]: '30'
    })
    const result = getProjectUrgency(project)
    expect(result).toBe('low')
  })

  it('returns medium for project on hold (< 7 days)', () => {
    const project = createMockProject({
      [PROJECT_FIELDS.ON_HOLD]: 'Yes',
      [PROJECT_FIELDS.DATE_ON_HOLD]: new Date(Date.now() - 3 * 24 * 60 * 60 * 1000).toISOString()
    })
    const result = getProjectUrgency(project)
    expect(result).toBe('medium')
  })

  it('returns medium for project > 90 days old', () => {
    const project = createMockProject({
      [PROJECT_FIELDS.PROJECT_AGE]: '100'
    })
    const result = getProjectUrgency(project)
    expect(result).toBe('medium')
  })

  it('returns high for project on hold > 7 days', () => {
    const project = createMockProject({
      [PROJECT_FIELDS.ON_HOLD]: 'Yes',
      [PROJECT_FIELDS.DATE_ON_HOLD]: new Date(Date.now() - 10 * 24 * 60 * 60 * 1000).toISOString()
    })
    const result = getProjectUrgency(project)
    expect(result).toBe('high')
  })

  it('returns high for project with Urgent priority', () => {
    const project = createMockProject({
      [PROJECT_FIELDS.PROJECT_PRIORITY]: 'Urgent'
    })
    const result = getProjectUrgency(project)
    expect(result).toBe('high')
  })

  it('returns high for project > 120 days old', () => {
    const project = createMockProject({
      [PROJECT_FIELDS.PROJECT_AGE]: '130'
    })
    const result = getProjectUrgency(project)
    expect(result).toBe('high')
  })

  it('returns critical for project on hold > 14 days', () => {
    const project = createMockProject({
      [PROJECT_FIELDS.ON_HOLD]: 'Yes',
      [PROJECT_FIELDS.DATE_ON_HOLD]: new Date(Date.now() - 20 * 24 * 60 * 60 * 1000).toISOString()
    })
    const result = getProjectUrgency(project)
    expect(result).toBe('critical')
  })

  it('returns critical for project with Insane priority', () => {
    const project = createMockProject({
      [PROJECT_FIELDS.PROJECT_PRIORITY]: 'Insane'
    })
    const result = getProjectUrgency(project)
    expect(result).toBe('critical')
  })

  it('returns critical for project > 180 days old', () => {
    const project = createMockProject({
      [PROJECT_FIELDS.PROJECT_AGE]: '200'
    })
    const result = getProjectUrgency(project)
    expect(result).toBe('critical')
  })

  it('handles missing ON_HOLD field (treats as false)', () => {
    const project = createMockProject({
      [PROJECT_FIELDS.PROJECT_AGE]: '30'
    })
    const result = getProjectUrgency(project)
    expect(result).toBe('low')
  })

  it('handles missing PROJECT_AGE field (treats as 0)', () => {
    const project = createMockProject({})
    const result = getProjectUrgency(project)
    expect(result).toBe('low')
  })
})

describe('getMilestoneColor', () => {
  it('returns correct color classes for each milestone name', () => {
    const milestones = [
      { name: 'Intake', expectedClasses: 'bg-gray-100 text-gray-800' },
      { name: 'Survey', expectedClasses: 'bg-blue-100 text-blue-800' },
      { name: 'Design', expectedClasses: 'bg-purple-100 text-purple-800' },
      { name: 'Permit', expectedClasses: 'bg-yellow-100 text-yellow-800' },
      { name: 'NEM', expectedClasses: 'bg-orange-100 text-orange-800' },
      { name: 'Install', expectedClasses: 'bg-indigo-100 text-indigo-800' },
      { name: 'Verification', expectedClasses: 'bg-cyan-100 text-cyan-800' },
      { name: 'Inspection', expectedClasses: 'bg-teal-100 text-teal-800' },
      { name: 'PTO', expectedClasses: 'bg-green-100 text-green-800' }
    ]

    milestones.forEach(({ name, expectedClasses }) => {
      const result = getMilestoneColor(name)
      expect(result).toBe(expectedClasses)
    })
  })

  it('returns default gray classes for unknown milestone name', () => {
    const result = getMilestoneColor('Unknown')
    expect(result).toBe('bg-gray-100 text-gray-800')
  })
})

describe('getNextMilestone', () => {
  it('returns Survey when current is Intake', () => {
    const result = getNextMilestone('Intake')
    expect(result).toBe('Survey')
  })

  it('returns Design when current is Survey', () => {
    const result = getNextMilestone('Survey')
    expect(result).toBe('Design')
  })

  it('returns Permit when current is Design', () => {
    const result = getNextMilestone('Design')
    expect(result).toBe('Permit')
  })

  it('returns NEM when current is Permit', () => {
    const result = getNextMilestone('Permit')
    expect(result).toBe('NEM')
  })

  it('returns Install when current is NEM', () => {
    const result = getNextMilestone('NEM')
    expect(result).toBe('Install')
  })

  it('returns Verification when current is Install', () => {
    const result = getNextMilestone('Install')
    expect(result).toBe('Verification')
  })

  it('returns Inspection when current is Verification', () => {
    const result = getNextMilestone('Verification')
    expect(result).toBe('Inspection')
  })

  it('returns PTO when current is Inspection', () => {
    const result = getNextMilestone('Inspection')
    expect(result).toBe('PTO')
  })

  it('returns Complete when current is PTO', () => {
    const result = getNextMilestone('PTO')
    expect(result).toBe('Complete')
  })

  it('returns Complete for unknown milestone', () => {
    const result = getNextMilestone('Unknown')
    expect(result).toBe('Complete')
  })
})

describe('isMilestoneOverdue', () => {
  it('returns false for Intake milestone when project age < 7 days', () => {
    const project = createMockProject({
      [PROJECT_FIELDS.PROJECT_AGE]: '5'
    })
    const result = isMilestoneOverdue(project, 'Intake')
    expect(result).toBe(false)
  })

  it('returns true for Intake milestone when project age > 7 days', () => {
    const project = createMockProject({
      [PROJECT_FIELDS.PROJECT_AGE]: '10'
    })
    const result = isMilestoneOverdue(project, 'Intake')
    expect(result).toBe(true)
  })

  it('returns false for Survey milestone when project age < 14 days', () => {
    const project = createMockProject({
      [PROJECT_FIELDS.PROJECT_AGE]: '10'
    })
    const result = isMilestoneOverdue(project, 'Survey')
    expect(result).toBe(false)
  })

  it('returns true for Survey milestone when project age > 14 days', () => {
    const project = createMockProject({
      [PROJECT_FIELDS.PROJECT_AGE]: '20'
    })
    const result = isMilestoneOverdue(project, 'Survey')
    expect(result).toBe(true)
  })

  it('returns false for Design milestone when project age < 21 days', () => {
    const project = createMockProject({
      [PROJECT_FIELDS.PROJECT_AGE]: '15'
    })
    const result = isMilestoneOverdue(project, 'Design')
    expect(result).toBe(false)
  })

  it('returns true for Design milestone when project age > 21 days', () => {
    const project = createMockProject({
      [PROJECT_FIELDS.PROJECT_AGE]: '25'
    })
    const result = isMilestoneOverdue(project, 'Design')
    expect(result).toBe(true)
  })

  it('returns false for Permit milestone when project age < 35 days', () => {
    const project = createMockProject({
      [PROJECT_FIELDS.PROJECT_AGE]: '30'
    })
    const result = isMilestoneOverdue(project, 'Permit')
    expect(result).toBe(false)
  })

  it('returns true for Permit milestone when project age > 35 days', () => {
    const project = createMockProject({
      [PROJECT_FIELDS.PROJECT_AGE]: '40'
    })
    const result = isMilestoneOverdue(project, 'Permit')
    expect(result).toBe(true)
  })

  it('returns false for NEM milestone when project age < 45 days', () => {
    const project = createMockProject({
      [PROJECT_FIELDS.PROJECT_AGE]: '40'
    })
    const result = isMilestoneOverdue(project, 'NEM')
    expect(result).toBe(false)
  })

  it('returns true for NEM milestone when project age > 45 days', () => {
    const project = createMockProject({
      [PROJECT_FIELDS.PROJECT_AGE]: '50'
    })
    const result = isMilestoneOverdue(project, 'NEM')
    expect(result).toBe(true)
  })

  it('returns false for Install milestone when project age < 60 days', () => {
    const project = createMockProject({
      [PROJECT_FIELDS.PROJECT_AGE]: '55'
    })
    const result = isMilestoneOverdue(project, 'Install')
    expect(result).toBe(false)
  })

  it('returns true for Install milestone when project age > 60 days', () => {
    const project = createMockProject({
      [PROJECT_FIELDS.PROJECT_AGE]: '65'
    })
    const result = isMilestoneOverdue(project, 'Install')
    expect(result).toBe(true)
  })

  it('returns false for Verification milestone when project age < 65 days', () => {
    const project = createMockProject({
      [PROJECT_FIELDS.PROJECT_AGE]: '60'
    })
    const result = isMilestoneOverdue(project, 'Verification')
    expect(result).toBe(false)
  })

  it('returns true for Verification milestone when project age > 65 days', () => {
    const project = createMockProject({
      [PROJECT_FIELDS.PROJECT_AGE]: '70'
    })
    const result = isMilestoneOverdue(project, 'Verification')
    expect(result).toBe(true)
  })

  it('returns false for Inspection milestone when project age < 75 days', () => {
    const project = createMockProject({
      [PROJECT_FIELDS.PROJECT_AGE]: '70'
    })
    const result = isMilestoneOverdue(project, 'Inspection')
    expect(result).toBe(false)
  })

  it('returns true for Inspection milestone when project age > 75 days', () => {
    const project = createMockProject({
      [PROJECT_FIELDS.PROJECT_AGE]: '80'
    })
    const result = isMilestoneOverdue(project, 'Inspection')
    expect(result).toBe(true)
  })

  it('returns false for PTO milestone when project age < 90 days', () => {
    const project = createMockProject({
      [PROJECT_FIELDS.PROJECT_AGE]: '85'
    })
    const result = isMilestoneOverdue(project, 'PTO')
    expect(result).toBe(false)
  })

  it('returns true for PTO milestone when project age > 90 days', () => {
    const project = createMockProject({
      [PROJECT_FIELDS.PROJECT_AGE]: '95'
    })
    const result = isMilestoneOverdue(project, 'PTO')
    expect(result).toBe(true)
  })

  it('handles missing PROJECT_AGE field (treats as 0, returns false)', () => {
    const project = createMockProject({})
    const result = isMilestoneOverdue(project, 'Intake')
    expect(result).toBe(false)
  })
})
