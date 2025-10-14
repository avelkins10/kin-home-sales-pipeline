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
  it('returns intake for new project with no milestone data', () => {
    const project = createMockProject({})
    const result = getCurrentMilestone(project)
    expect(result).toBe('intake')
  })

  it('returns survey when survey approved', () => {
    const project = createMockProject({
      [PROJECT_FIELDS.SURVEY_APPROVED]: '2024-01-01'
    })
    const result = getCurrentMilestone(project)
    expect(result).toBe('survey')
  })

  it('returns design when design completed', () => {
    const project = createMockProject({
      [PROJECT_FIELDS.DESIGN_COMPLETED]: '2024-01-01'
    })
    const result = getCurrentMilestone(project)
    expect(result).toBe('design')
  })

  it('returns permitting when permit submitted', () => {
    const project = createMockProject({
      [PROJECT_FIELDS.PERMIT_SUBMITTED]: '2024-01-01'
    })
    const result = getCurrentMilestone(project)
    expect(result).toBe('permitting')
  })

  it('returns permitting when NEM and permit approved', () => {
    const project = createMockProject({
      [PROJECT_FIELDS.NEM_APPROVED]: '2024-01-01',
      [PROJECT_FIELDS.PERMIT_APPROVED]: '2024-01-01'
    })
    const result = getCurrentMilestone(project)
    expect(result).toBe('permitting')
  })

  it('returns install when install scheduled', () => {
    const project = createMockProject({
      [PROJECT_FIELDS.INSTALL_SCHEDULED_DATE_CAPTURE]: '2024-01-01'
    })
    const result = getCurrentMilestone(project)
    expect(result).toBe('install')
  })

  it('returns install when install completed', () => {
    const project = createMockProject({
      [PROJECT_FIELDS.INSTALL_COMPLETED_DATE]: '2024-01-01'
    })
    const result = getCurrentMilestone(project)
    expect(result).toBe('install')
  })

  it('returns inspection when inspection passed', () => {
    const project = createMockProject({
      [PROJECT_FIELDS.PASSING_INSPECTION_COMPLETED]: '2024-01-01'
    })
    const result = getCurrentMilestone(project)
    expect(result).toBe('inspection')
  })

  it('returns pto when PTO approved (final milestone)', () => {
    const project = createMockProject({
      [PROJECT_FIELDS.PTO_APPROVED]: '2024-01-01'
    })
    const result = getCurrentMilestone(project)
    expect(result).toBe('pto')
  })

  it('prioritizes later milestones (if both design and install complete, returns install)', () => {
    const project = createMockProject({
      [PROJECT_FIELDS.DESIGN_COMPLETED]: '2024-01-01',
      [PROJECT_FIELDS.INSTALL_COMPLETED_DATE]: '2024-01-02'
    })
    const result = getCurrentMilestone(project)
    expect(result).toBe('install')
  })
})

describe('getMilestoneProgress', () => {
  it('returns 0 for project with no completed milestones', () => {
    const project = createMockProject({})
    const result = getMilestoneProgress(project)
    expect(result).toBe(0)
  })

  it('returns ~14.29% (1/7) for project with intake completed', () => {
    const project = createMockProject({
      [PROJECT_FIELDS.INTAKE_COMPLETED_DATE]: '2024-01-01'
    })
    const result = getMilestoneProgress(project)
    expect(result).toBeCloseTo(14.29, 1)
  })

  it('returns ~28.57% (2/7) for project with intake and survey', () => {
    const project = createMockProject({
      [PROJECT_FIELDS.INTAKE_COMPLETED_DATE]: '2024-01-01',
      [PROJECT_FIELDS.SURVEY_APPROVED]: '2024-01-02'
    })
    const result = getMilestoneProgress(project)
    expect(result).toBeCloseTo(28.57, 1)
  })

  it('returns ~57.14% (4/7) for project through permitting', () => {
    const project = createMockProject({
      [PROJECT_FIELDS.INTAKE_COMPLETED_DATE]: '2024-01-01',
      [PROJECT_FIELDS.SURVEY_APPROVED]: '2024-01-02',
      [PROJECT_FIELDS.DESIGN_COMPLETED]: '2024-01-03',
      [PROJECT_FIELDS.PERMIT_APPROVED]: '2024-01-04',
      [PROJECT_FIELDS.NEM_APPROVED]: '2024-01-05'
    })
    const result = getMilestoneProgress(project)
    expect(result).toBeCloseTo(57.14, 1)
  })

  it('returns 100% for project with PTO approved', () => {
    const project = createMockProject({
      [PROJECT_FIELDS.INTAKE_COMPLETED_DATE]: '2024-01-01',
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
      [PROJECT_FIELDS.INTAKE_COMPLETED_DATE]: '2024-01-01'
    })
    const result = getMilestoneProgress(project)
    expect(result).toBeGreaterThanOrEqual(0)
    expect(result).toBeLessThanOrEqual(100)
  })

  it('counts install milestone when install complete', () => {
    const project = createMockProject({
      [PROJECT_FIELDS.INTAKE_COMPLETED_DATE]: '2024-01-01',
      [PROJECT_FIELDS.INSTALL_COMPLETED_DATE]: '2024-01-02'
    })
    const result = getMilestoneProgress(project)
    expect(result).toBeGreaterThan(14.29) // Should include install
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
    expect(getMilestoneColor('intake')).toBe('bg-gray-100 text-gray-800')
    expect(getMilestoneColor('survey')).toBe('bg-blue-100 text-blue-800')
    expect(getMilestoneColor('design')).toBe('bg-purple-100 text-purple-800')
    expect(getMilestoneColor('permitting')).toBe('bg-indigo-100 text-indigo-800')
    expect(getMilestoneColor('install')).toBe('bg-green-100 text-green-800')
    expect(getMilestoneColor('inspection')).toBe('bg-teal-100 text-teal-800')
    expect(getMilestoneColor('pto')).toBe('bg-emerald-100 text-emerald-800')
  })

  it('returns default gray classes for unknown milestone name', () => {
    const result = getMilestoneColor('Unknown')
    expect(result).toBe('bg-gray-100 text-gray-800')
  })
})

describe('getNextMilestone', () => {
  it('returns survey when current is intake', () => {
    const result = getNextMilestone('intake')
    expect(result).toBe('survey')
  })

  it('returns design when current is survey', () => {
    const result = getNextMilestone('survey')
    expect(result).toBe('design')
  })

  it('returns permitting when current is design', () => {
    const result = getNextMilestone('design')
    expect(result).toBe('permitting')
  })

  it('returns install when current is permitting', () => {
    const result = getNextMilestone('permitting')
    expect(result).toBe('install')
  })

  it('returns inspection when current is install', () => {
    const result = getNextMilestone('install')
    expect(result).toBe('inspection')
  })

  it('returns pto when current is inspection', () => {
    const result = getNextMilestone('inspection')
    expect(result).toBe('pto')
  })

  it('returns complete when current is pto', () => {
    const result = getNextMilestone('pto')
    expect(result).toBe('complete')
  })

  it('returns complete for unknown milestone', () => {
    const result = getNextMilestone('Unknown')
    expect(result).toBe('complete')
  })
})

describe('isMilestoneOverdue', () => {
  it('returns false for intake milestone when project age < 7 days', () => {
    const project = createMockProject({
      [PROJECT_FIELDS.PROJECT_AGE]: '5'
    })
    const result = isMilestoneOverdue(project, 'intake')
    expect(result).toBe(false)
  })

  it('returns true for intake milestone when project age > 7 days', () => {
    const project = createMockProject({
      [PROJECT_FIELDS.PROJECT_AGE]: '10'
    })
    const result = isMilestoneOverdue(project, 'intake')
    expect(result).toBe(true)
  })

  it('returns false for survey milestone when project age < 14 days', () => {
    const project = createMockProject({
      [PROJECT_FIELDS.PROJECT_AGE]: '10'
    })
    const result = isMilestoneOverdue(project, 'survey')
    expect(result).toBe(false)
  })

  it('returns true for survey milestone when project age > 14 days', () => {
    const project = createMockProject({
      [PROJECT_FIELDS.PROJECT_AGE]: '20'
    })
    const result = isMilestoneOverdue(project, 'survey')
    expect(result).toBe(true)
  })

  it('returns false for design milestone when project age < 21 days', () => {
    const project = createMockProject({
      [PROJECT_FIELDS.PROJECT_AGE]: '15'
    })
    const result = isMilestoneOverdue(project, 'design')
    expect(result).toBe(false)
  })

  it('returns true for design milestone when project age > 21 days', () => {
    const project = createMockProject({
      [PROJECT_FIELDS.PROJECT_AGE]: '25'
    })
    const result = isMilestoneOverdue(project, 'design')
    expect(result).toBe(true)
  })

  it('returns false for permitting milestone when project age < 45 days', () => {
    const project = createMockProject({
      [PROJECT_FIELDS.PROJECT_AGE]: '30'
    })
    const result = isMilestoneOverdue(project, 'permitting')
    expect(result).toBe(false)
  })

  it('returns true for permitting milestone when project age > 45 days', () => {
    const project = createMockProject({
      [PROJECT_FIELDS.PROJECT_AGE]: '50'
    })
    const result = isMilestoneOverdue(project, 'permitting')
    expect(result).toBe(true)
  })

  it('returns false for install milestone when project age < 60 days', () => {
    const project = createMockProject({
      [PROJECT_FIELDS.PROJECT_AGE]: '55'
    })
    const result = isMilestoneOverdue(project, 'install')
    expect(result).toBe(false)
  })

  it('returns true for install milestone when project age > 60 days', () => {
    const project = createMockProject({
      [PROJECT_FIELDS.PROJECT_AGE]: '65'
    })
    const result = isMilestoneOverdue(project, 'install')
    expect(result).toBe(true)
  })

  it('returns false for inspection milestone when project age < 75 days', () => {
    const project = createMockProject({
      [PROJECT_FIELDS.PROJECT_AGE]: '70'
    })
    const result = isMilestoneOverdue(project, 'inspection')
    expect(result).toBe(false)
  })

  it('returns true for inspection milestone when project age > 75 days', () => {
    const project = createMockProject({
      [PROJECT_FIELDS.PROJECT_AGE]: '80'
    })
    const result = isMilestoneOverdue(project, 'inspection')
    expect(result).toBe(true)
  })

  it('returns false for pto milestone when project age < 90 days', () => {
    const project = createMockProject({
      [PROJECT_FIELDS.PROJECT_AGE]: '85'
    })
    const result = isMilestoneOverdue(project, 'pto')
    expect(result).toBe(false)
  })

  it('returns true for pto milestone when project age > 90 days', () => {
    const project = createMockProject({
      [PROJECT_FIELDS.PROJECT_AGE]: '95'
    })
    const result = isMilestoneOverdue(project, 'pto')
    expect(result).toBe(true)
  })

  it('handles missing PROJECT_AGE field (treats as 0, returns false)', () => {
    const project = createMockProject({})
    const result = isMilestoneOverdue(project, 'intake')
    expect(result).toBe(false)
  })
})
