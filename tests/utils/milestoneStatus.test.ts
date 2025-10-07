import { describe, it, expect } from 'vitest'
import {
  getSurveyStatus,
  getDesignStatus,
  getHOAStatus,
  getPermitStatus,
  getNEMStatus,
  getInstallStatus,
  getVerificationStatus,
  getInspectionStatus,
  getPTOStatus
} from '@/lib/utils/milestoneStatus'
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

// Helper function for creating dates
function daysAgo(days: number): string {
  const date = new Date()
  date.setDate(date.getDate() - days)
  return date.toISOString()
}

function daysFromNow(days: number): string {
  const date = new Date()
  date.setDate(date.getDate() + days)
  return date.toISOString()
}

describe('getSurveyStatus', () => {
  it('returns pending when no survey data', () => {
    const project = createMockProject({})
    const result = getSurveyStatus(project)
    expect(result).toEqual({ status: 'pending' })
  })

  it('returns in-progress when submitted but not approved', () => {
    const submittedDate = daysAgo(5)
    const project = createMockProject({
      [PROJECT_FIELDS.SURVEY_SUBMITTED]: submittedDate
    })
    const result = getSurveyStatus(project)
    expect(result.status).toBe('in-progress')
    expect(result.date).toEqual(new Date(submittedDate))
    expect(result.substeps).toHaveLength(2)
    expect(result.substeps![0]).toEqual({
      label: 'Submitted',
      date: new Date(submittedDate),
      status: 'complete'
    })
    expect(result.substeps![1]).toEqual({
      label: 'Approved',
      date: null,
      status: 'pending'
    })
  })

  it('returns complete when approved', () => {
    const submittedDate = daysAgo(10)
    const approvedDate = daysAgo(5)
    const project = createMockProject({
      [PROJECT_FIELDS.SURVEY_SUBMITTED]: submittedDate,
      [PROJECT_FIELDS.SURVEY_APPROVED]: approvedDate
    })
    const result = getSurveyStatus(project)
    expect(result.status).toBe('complete')
    expect(result.date).toEqual(new Date(approvedDate))
    expect(result.substeps).toHaveLength(2)
    expect(result.substeps![0].status).toBe('complete')
    expect(result.substeps![1].status).toBe('complete')
  })

  it('handles backup timestamp fields (MAX_SURVEY_SUBMITTED)', () => {
    const submittedDate = daysAgo(5)
    const project = createMockProject({
      [PROJECT_FIELDS.MAX_SURVEY_SUBMITTED]: submittedDate
    })
    const result = getSurveyStatus(project)
    expect(result.status).toBe('in-progress')
    expect(result.date).toEqual(new Date(submittedDate))
  })
})

describe('getDesignStatus', () => {
  it('returns pending when no design data', () => {
    const project = createMockProject({})
    const result = getDesignStatus(project)
    expect(result).toEqual({ status: 'pending' })
  })

  it('returns in-progress when predesign approved but not complete', () => {
    const predesignDate = daysAgo(5)
    const project = createMockProject({
      [PROJECT_FIELDS.PREDESIGN_APPROVED]: predesignDate
    })
    const result = getDesignStatus(project)
    expect(result.status).toBe('in-progress')
    expect(result.substeps).toHaveLength(3)
    expect(result.substeps![0].status).toBe('complete')
    expect(result.substeps![1].status).toBe('pending')
    expect(result.substeps![2].status).toBe('pending')
  })

  it('returns complete when design completed', () => {
    const predesignDate = daysAgo(10)
    const cadDate = daysAgo(7)
    const completedDate = daysAgo(3)
    const project = createMockProject({
      [PROJECT_FIELDS.PREDESIGN_APPROVED]: predesignDate,
      [PROJECT_FIELDS.CAD_DESIGN_APPROVED]: cadDate,
      [PROJECT_FIELDS.DESIGN_COMPLETED]: completedDate
    })
    const result = getDesignStatus(project)
    expect(result.status).toBe('complete')
    expect(result.substeps).toHaveLength(3)
    expect(result.substeps!.every(step => step.status === 'complete')).toBe(true)
  })

  it('sets urgent flag when SLA deadline within 3 days', () => {
    const deadline = daysFromNow(2)
    const project = createMockProject({
      [PROJECT_FIELDS.DESIGN_SLA_DEADLINE]: deadline
    })
    const result = getDesignStatus(project)
    expect(result.urgent).toBe(true)
    expect(result.warning).toBe('Design SLA deadline approaching')
  })

  it('no urgent flag when SLA deadline > 3 days away', () => {
    const deadline = daysFromNow(5)
    const project = createMockProject({
      [PROJECT_FIELDS.DESIGN_SLA_DEADLINE]: deadline
    })
    const result = getDesignStatus(project)
    expect(result.urgent).toBe(false)
    expect(result.warning).toBeUndefined()
  })
})

describe('getHOAStatus', () => {
  it('returns hasHOA false when no HOA fields', () => {
    const project = createMockProject({})
    const result = getHOAStatus(project)
    expect(result).toEqual({ hasHOA: false, status: 'pending' })
  })

  it('returns hasHOA true when HOA submitted', () => {
    const submittedDate = daysAgo(5)
    const project = createMockProject({
      [PROJECT_FIELDS.HOA_APPLICATION_SUBMITTED]: submittedDate
    })
    const result = getHOAStatus(project)
    expect(result.hasHOA).toBe(true)
    expect(result.status).toBe('in-progress')
    expect(result.warning).toBe('HOA approval can take 30-90 days')
  })

  it('returns complete when HOA approved', () => {
    const submittedDate = daysAgo(10)
    const approvedDate = daysAgo(5)
    const project = createMockProject({
      [PROJECT_FIELDS.HOA_APPLICATION_SUBMITTED]: submittedDate,
      [PROJECT_FIELDS.HOA_APPLICATION_APPROVED]: approvedDate
    })
    const result = getHOAStatus(project)
    expect(result.hasHOA).toBe(true)
    expect(result.status).toBe('complete')
    expect(result.substeps).toHaveLength(2)
    expect(result.substeps!.every(step => step.status === 'complete')).toBe(true)
  })
})

describe('getPermitStatus', () => {
  it('returns pending when no permit data', () => {
    const project = createMockProject({})
    const result = getPermitStatus(project)
    expect(result).toEqual({ status: 'pending' })
  })

  it('returns in-progress when submitted but not approved', () => {
    const submittedDate = daysAgo(5)
    const project = createMockProject({
      [PROJECT_FIELDS.PERMIT_SUBMITTED]: submittedDate
    })
    const result = getPermitStatus(project)
    expect(result.status).toBe('in-progress')
    expect(result.substeps).toHaveLength(2)
    expect(result.substeps![0].status).toBe('complete')
    expect(result.substeps![1].status).toBe('pending')
  })

  it('returns complete when approved', () => {
    const submittedDate = daysAgo(10)
    const approvedDate = daysAgo(5)
    const project = createMockProject({
      [PROJECT_FIELDS.PERMIT_SUBMITTED]: submittedDate,
      [PROJECT_FIELDS.PERMIT_APPROVED]: approvedDate
    })
    const result = getPermitStatus(project)
    expect(result.status).toBe('complete')
    expect(result.substeps).toHaveLength(2)
    expect(result.substeps!.every(step => step.status === 'complete')).toBe(true)
  })

  it('adds warning when waiting > 30 days', () => {
    const submittedDate = daysAgo(35)
    const project = createMockProject({
      [PROJECT_FIELDS.PERMIT_SUBMITTED]: submittedDate
    })
    const result = getPermitStatus(project)
    expect(result.warning).toBe('Permit taking longer than expected')
  })

  it('handles as-built scenario (install complete but no as-built)', () => {
    const installDate = daysAgo(5)
    const project = createMockProject({
      [PROJECT_FIELDS.INSTALL_COMPLETED_DATE]: installDate
    })
    const result = getPermitStatus(project)
    expect(result.status).toBe('in-progress')
    expect(result.warning).toBe('As-built plans needed for final inspection')
    expect(result.substeps).toHaveLength(3)
  })

  it('returns complete when as-built submitted', () => {
    const asBuiltDate = daysAgo(2)
    const project = createMockProject({
      [PROJECT_FIELDS.AS_BUILT_SUBMITTED_TO_AHJ]: asBuiltDate
    })
    const result = getPermitStatus(project)
    expect(result.status).toBe('complete')
    expect(result.substeps).toHaveLength(3)
    expect(result.substeps!.every(step => step.status === 'complete')).toBe(true)
  })
})

describe('getNEMStatus', () => {
  it('returns pending when no NEM data', () => {
    const project = createMockProject({})
    const result = getNEMStatus(project)
    expect(result).toEqual({ status: 'pending' })
  })

  it('returns in-progress when signatures sent', () => {
    const signaturesDate = daysAgo(5)
    const project = createMockProject({
      [PROJECT_FIELDS.NEM_SIGNATURES_SENT]: signaturesDate
    })
    const result = getNEMStatus(project)
    expect(result.status).toBe('in-progress')
    expect(result.substeps).toHaveLength(3)
    expect(result.substeps![0].status).toBe('complete')
    expect(result.substeps![1].status).toBe('pending')
    expect(result.substeps![2].status).toBe('pending')
  })

  it('sets blocked status when signatures sent > 7 days ago', () => {
    const signaturesDate = daysAgo(10)
    const project = createMockProject({
      [PROJECT_FIELDS.NEM_SIGNATURES_SENT]: signaturesDate
    })
    const result = getNEMStatus(project)
    expect(result.status).toBe('blocked')
    expect(result.urgent).toBe(true)
    expect(result.warning).toBe('Customer signatures needed - follow up!')
  })

  it('returns in-progress when submitted', () => {
    const submittedDate = daysAgo(5)
    const project = createMockProject({
      [PROJECT_FIELDS.NEM_SUBMITTED]: submittedDate
    })
    const result = getNEMStatus(project)
    expect(result.status).toBe('in-progress')
    expect(result.substeps).toHaveLength(3)
    expect(result.substeps![0].status).toBe('complete')
    expect(result.substeps![1].status).toBe('complete')
    expect(result.substeps![2].status).toBe('pending')
  })

  it('returns complete when approved', () => {
    const approvedDate = daysAgo(3)
    const project = createMockProject({
      [PROJECT_FIELDS.NEM_APPROVED]: approvedDate
    })
    const result = getNEMStatus(project)
    expect(result.status).toBe('complete')
    expect(result.substeps).toHaveLength(3)
    expect(result.substeps!.every(step => step.status === 'complete')).toBe(true)
  })
})

describe('getInstallStatus', () => {
  it('returns pending when no install data', () => {
    const project = createMockProject({})
    const result = getInstallStatus(project)
    expect(result).toEqual({ status: 'pending' })
  })

  it('returns in-progress when scheduled (future date)', () => {
    const scheduledDate = daysFromNow(10)
    const project = createMockProject({
      [PROJECT_FIELDS.INSTALL_SCHEDULED_DATE_CAPTURE]: scheduledDate
    })
    const result = getInstallStatus(project)
    expect(result.status).toBe('in-progress')
    expect(result.substeps).toHaveLength(2)
    expect(result.substeps![0].status).toBe('complete')
    expect(result.substeps![1].status).toBe('pending')
  })

  it('returns upcoming when scheduled within 7 days', () => {
    const scheduledDate = daysFromNow(5)
    const project = createMockProject({
      [PROJECT_FIELDS.INSTALL_SCHEDULED_DATE_CAPTURE]: scheduledDate
    })
    const result = getInstallStatus(project)
    expect(result.status).toBe('upcoming')
  })

  it('returns complete when install completed', () => {
    const completedDate = daysAgo(3)
    const project = createMockProject({
      [PROJECT_FIELDS.INSTALL_COMPLETED_DATE]: completedDate
    })
    const result = getInstallStatus(project)
    expect(result.status).toBe('complete')
    expect(result.substeps).toHaveLength(2)
    expect(result.substeps!.every(step => step.status === 'complete')).toBe(true)
  })

  it('returns complete when ready for commission', () => {
    const readyDate = daysAgo(1)
    const project = createMockProject({
      [PROJECT_FIELDS.READY_FOR_COMMISSION]: readyDate
    })
    const result = getInstallStatus(project)
    expect(result.status).toBe('complete')
    expect(result.substeps).toHaveLength(3)
    expect(result.substeps!.every(step => step.status === 'complete')).toBe(true)
  })

  it('handles backup date fields (ESTIMATED_INSTALL_DATE)', () => {
    const estimatedDate = daysFromNow(5)
    const project = createMockProject({
      [PROJECT_FIELDS.ESTIMATED_INSTALL_DATE]: estimatedDate
    })
    const result = getInstallStatus(project)
    expect(result.status).toBe('upcoming')
  })
})

describe('getVerificationStatus', () => {
  it('returns pending when install not complete', () => {
    const project = createMockProject({})
    const result = getVerificationStatus(project)
    expect(result).toEqual({ status: 'pending' })
  })

  it('returns in-progress when install just completed (< 1 day)', () => {
    const installDate = new Date().toISOString()
    const project = createMockProject({
      [PROJECT_FIELDS.INSTALL_COMPLETED_DATE]: installDate
    })
    const result = getVerificationStatus(project)
    expect(result.status).toBe('in-progress')
  })

  it('returns complete with estimated date when install complete >= 1 day', () => {
    const installDate = daysAgo(2)
    const project = createMockProject({
      [PROJECT_FIELDS.INSTALL_COMPLETED_DATE]: installDate
    })
    const result = getVerificationStatus(project)
    expect(result.status).toBe('complete')
    expect(result.calculated).toBe(true)
    expect(result.estimatedDate).toBeDefined()
  })

  it('returns complete when inspection passed', () => {
    const inspectionDate = daysAgo(1)
    const project = createMockProject({
      [PROJECT_FIELDS.PASSING_INSPECTION_COMPLETED]: inspectionDate
    })
    const result = getVerificationStatus(project)
    expect(result.status).toBe('complete')
    expect(result.date).toEqual(new Date(inspectionDate))
  })
})

describe('getInspectionStatus', () => {
  it('returns pending when no inspection data', () => {
    const project = createMockProject({})
    const result = getInspectionStatus(project)
    expect(result).toEqual({ status: 'pending' })
  })

  it('returns in-progress when as-built submitted', () => {
    const asBuiltDate = daysAgo(5)
    const project = createMockProject({
      [PROJECT_FIELDS.AS_BUILT_SUBMITTED_TO_AHJ]: asBuiltDate
    })
    const result = getInspectionStatus(project)
    expect(result.status).toBe('in-progress')
    expect(result.substeps).toHaveLength(2)
    expect(result.substeps![0].status).toBe('complete')
    expect(result.substeps![1].status).toBe('pending')
  })

  it('returns complete when inspection passed', () => {
    const passedDate = daysAgo(2)
    const project = createMockProject({
      [PROJECT_FIELDS.PASSING_INSPECTION_COMPLETED]: passedDate
    })
    const result = getInspectionStatus(project)
    expect(result.status).toBe('complete')
    expect(result.substeps).toHaveLength(2)
    expect(result.substeps!.every(step => step.status === 'complete')).toBe(true)
  })
})

describe('getPTOStatus', () => {
  it('returns pending when no PTO data', () => {
    const project = createMockProject({})
    const result = getPTOStatus(project)
    expect(result).toEqual({ status: 'pending' })
  })

  it('returns in-progress when submitted', () => {
    const submittedDate = daysAgo(5)
    const project = createMockProject({
      [PROJECT_FIELDS.PTO_SUBMITTED]: submittedDate
    })
    const result = getPTOStatus(project)
    expect(result.status).toBe('in-progress')
    expect(result.substeps).toHaveLength(3)
    expect(result.substeps![0].status).toBe('complete')
    expect(result.substeps![1].status).toBe('pending')
    expect(result.substeps![2].status).toBe('pending')
  })

  it('adds warning when waiting > 42 days', () => {
    const submittedDate = daysAgo(50)
    const project = createMockProject({
      [PROJECT_FIELDS.PTO_SUBMITTED]: submittedDate
    })
    const result = getPTOStatus(project)
    expect(result.warning).toBe('Taking longer than expected')
  })

  it('returns complete when approved', () => {
    const approvedDate = daysAgo(3)
    const project = createMockProject({
      [PROJECT_FIELDS.PTO_APPROVED]: approvedDate
    })
    const result = getPTOStatus(project)
    expect(result.status).toBe('complete')
    expect(result.substeps).toHaveLength(3)
    expect(result.substeps![0].status).toBe('complete')
    expect(result.substeps![1].status).toBe('complete')
    expect(result.substeps![2].status).toBe('pending')
  })

  it('returns complete with celebration when uploaded to lender', () => {
    const uploadedDate = daysAgo(1)
    const project = createMockProject({
      [PROJECT_FIELDS.PTO_UPLOADED_TO_LENDER]: uploadedDate
    })
    const result = getPTOStatus(project)
    expect(result.status).toBe('complete')
    expect(result.celebration).toBe(true)
    expect(result.substeps).toHaveLength(3)
    expect(result.substeps![2].note).toBe('M3 Funded!')
  })
})
