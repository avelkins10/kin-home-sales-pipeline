import { PROJECT_FIELDS } from '@/lib/constants/fieldIds'
import { QuickbaseProject } from '@/lib/types/project'

export interface MilestoneStatus {
  status: 'complete' | 'in-progress' | 'pending' | 'blocked' | 'upcoming'
  date?: Date
  estimatedDate?: Date
  substeps?: Array<{ label: string; date: Date | null; status: string; note?: string }>
  warning?: string
  urgent?: boolean
  calculated?: boolean
  hasHOA?: boolean
  celebration?: boolean
}

function calculateDaysWaiting(date: string | null): number {
  if (!date) return 0
  const dateObj = new Date(date)
  const now = new Date()
  const diffTime = now.getTime() - dateObj.getTime()
  return Math.floor(diffTime / (1000 * 60 * 60 * 24))
}

export function getSurveyStatus(project: QuickbaseProject): MilestoneStatus {
  const submitted = project[PROJECT_FIELDS.SURVEY_SUBMITTED]?.value || 
                   project[PROJECT_FIELDS.MAX_SURVEY_SUBMITTED]?.value
  const approved = project[PROJECT_FIELDS.SURVEY_APPROVED]?.value

  if (approved) {
    return {
      status: 'complete',
      date: new Date(approved),
      substeps: [
        { label: 'Submitted', date: submitted ? new Date(submitted) : null, status: 'complete' },
        { label: 'Approved', date: new Date(approved), status: 'complete' }
      ]
    }
  }

  if (submitted) {
    return {
      status: 'in-progress',
      date: new Date(submitted),
      substeps: [
        { label: 'Submitted', date: new Date(submitted), status: 'complete' },
        { label: 'Approved', date: null, status: 'pending' }
      ]
    }
  }

  return { status: 'pending' }
}

export function getDesignStatus(project: QuickbaseProject): MilestoneStatus {
  const predesignApproved = project[PROJECT_FIELDS.PREDESIGN_APPROVED]?.value
  const cadApproved = project[PROJECT_FIELDS.CAD_DESIGN_APPROVED]?.value
  const designCompleted = project[PROJECT_FIELDS.DESIGN_COMPLETED]?.value
  const slaDeadline = project[PROJECT_FIELDS.DESIGN_SLA_DEADLINE]?.value

  let isUrgent = false
  if (slaDeadline) {
    const deadline = new Date(slaDeadline)
    const now = new Date()
    const daysUntilDeadline = Math.ceil((deadline.getTime() - now.getTime()) / (1000 * 60 * 60 * 24))
    isUrgent = daysUntilDeadline <= 3
  }

  if (designCompleted) {
    return {
      status: 'complete',
      date: new Date(designCompleted),
      substeps: [
        { label: 'Predesign', date: predesignApproved ? new Date(predesignApproved) : null, status: 'complete' },
        { label: 'CAD Approved', date: cadApproved ? new Date(cadApproved) : null, status: 'complete' },
        { label: 'Complete', date: new Date(designCompleted), status: 'complete' }
      ]
    }
  }

  if (cadApproved) {
    return {
      status: 'in-progress',
      date: new Date(cadApproved),
      urgent: isUrgent,
      warning: isUrgent ? 'Design SLA deadline approaching' : undefined,
      substeps: [
        { label: 'Predesign', date: predesignApproved ? new Date(predesignApproved) : null, status: 'complete' },
        { label: 'CAD Approved', date: new Date(cadApproved), status: 'complete' },
        { label: 'Complete', date: null, status: 'pending' }
      ]
    }
  }

  if (predesignApproved) {
    return {
      status: 'in-progress',
      date: new Date(predesignApproved),
      urgent: isUrgent,
      warning: isUrgent ? 'Design SLA deadline approaching' : undefined,
      substeps: [
        { label: 'Predesign', date: new Date(predesignApproved), status: 'complete' },
        { label: 'CAD Approved', date: null, status: 'pending' },
        { label: 'Complete', date: null, status: 'pending' }
      ]
    }
  }

  const result: MilestoneStatus = { status: 'pending' }
  if (slaDeadline) {
    result.urgent = isUrgent
    if (isUrgent) {
      result.warning = 'Design SLA deadline approaching'
    }
  }
  return result
}

export function getPermittingStatus(project: QuickbaseProject): MilestoneStatus {
  // NEM fields
  const nemSignaturesSent = project[PROJECT_FIELDS.NEM_SIGNATURES_SENT]?.value
  const nemSubmitted = project[PROJECT_FIELDS.NEM_SUBMITTED]?.value
  const nemApproved = project[PROJECT_FIELDS.NEM_APPROVED]?.value

  // Permit fields
  const permitSubmitted = project[PROJECT_FIELDS.PERMIT_SUBMITTED]?.value
  const permitApproved = project[PROJECT_FIELDS.PERMIT_APPROVED]?.value

  // HOA fields (conditional)
  const hoaSubmitted = project[PROJECT_FIELDS.HOA_APPLICATION_SUBMITTED]?.value
  const hoaApproved = project[PROJECT_FIELDS.HOA_APPLICATION_APPROVED]?.value

  // Check if HOA is required
  const hoaRequired = !!hoaSubmitted || !!hoaApproved

  // Build substeps array
  const substeps: Array<{ label: string; date: Date | null; status: string; note?: string }> = []

  // NEM substeps
  substeps.push({
    label: 'NEM Signatures Sent',
    date: nemSignaturesSent ? new Date(nemSignaturesSent) : null,
    status: nemSignaturesSent ? 'complete' : 'pending',
    note: nemSignaturesSent && !nemSubmitted ? 'Awaiting customer signatures' : undefined
  })

  substeps.push({
    label: 'NEM Submitted',
    date: nemSubmitted ? new Date(nemSubmitted) : null,
    status: nemSubmitted ? 'complete' : 'pending'
  })

  substeps.push({
    label: 'NEM Approved',
    date: nemApproved ? new Date(nemApproved) : null,
    status: nemApproved ? 'complete' : 'pending'
  })

  // Permit substeps
  substeps.push({
    label: 'Permit Submitted',
    date: permitSubmitted ? new Date(permitSubmitted) : null,
    status: permitSubmitted ? 'complete' : 'pending'
  })

  substeps.push({
    label: 'Permit Approved',
    date: permitApproved ? new Date(permitApproved) : null,
    status: permitApproved ? 'complete' : 'pending'
  })

  // HOA substeps (if required)
  if (hoaRequired) {
    substeps.push({
      label: 'HOA Submitted',
      date: hoaSubmitted ? new Date(hoaSubmitted) : null,
      status: hoaSubmitted ? 'complete' : 'pending'
    })

    substeps.push({
      label: 'HOA Approved',
      date: hoaApproved ? new Date(hoaApproved) : null,
      status: hoaApproved ? 'complete' : 'pending'
    })
  }

  // Check if all required permits are approved
  const allPermitsApproved = nemApproved && permitApproved && (!hoaRequired || hoaApproved)

  if (allPermitsApproved) {
    return {
      status: 'complete',
      date: new Date(Math.max(
        new Date(nemApproved!).getTime(),
        new Date(permitApproved!).getTime(),
        hoaApproved ? new Date(hoaApproved).getTime() : 0
      )),
      substeps
    }
  }

  // Check if NEM signatures are blocked (>7 days without submission)
  if (nemSignaturesSent && !nemSubmitted) {
    const daysWaiting = calculateDaysWaiting(nemSignaturesSent)
    if (daysWaiting > 7) {
      return {
        status: 'blocked',
        date: new Date(nemSignaturesSent),
        urgent: true,
        warning: 'Customer signatures needed - follow up!',
        substeps
      }
    }
  }

  // Check if any permits are in progress
  if (nemSignaturesSent || nemSubmitted || permitSubmitted || hoaSubmitted) {
    // Find the most recent date
    const dates = [nemSignaturesSent, nemSubmitted, permitSubmitted, hoaSubmitted]
      .filter(Boolean)
      .map(d => new Date(d!))

    const mostRecentDate = dates.length > 0 ? new Date(Math.max(...dates.map(d => d.getTime()))) : undefined

    return {
      status: 'in-progress',
      date: mostRecentDate,
      substeps
    }
  }

  return { status: 'pending', substeps }
}

/**
 * @deprecated Use getPermittingStatus() instead - HOA is now part of the unified Permitting milestone
 */
export function getHOAStatus(project: QuickbaseProject): MilestoneStatus {
  const submitted = project[PROJECT_FIELDS.HOA_APPLICATION_SUBMITTED]?.value
  const approved = project[PROJECT_FIELDS.HOA_APPLICATION_APPROVED]?.value

  if (!submitted && !approved) {
    return { hasHOA: false, status: 'pending' }
  }

  if (approved) {
    return {
      hasHOA: true,
      status: 'complete',
      date: new Date(approved),
      substeps: [
        { label: 'Submitted', date: submitted ? new Date(submitted) : null, status: 'complete' },
        { label: 'Approved', date: new Date(approved), status: 'complete' }
      ]
    }
  }

  if (submitted) {
    const daysWaiting = calculateDaysWaiting(submitted)
    return {
      hasHOA: true,
      status: 'in-progress',
      date: new Date(submitted),
      warning: 'HOA approval can take 30-90 days',
      substeps: [
        { label: 'Submitted', date: new Date(submitted), status: 'complete' },
        { label: 'Approved', date: null, status: 'pending' }
      ]
    }
  }

  return { hasHOA: true, status: 'pending' }
}

/**
 * @deprecated Use getPermittingStatus() instead - Permit is now part of the unified Permitting milestone
 */
export function getPermitStatus(project: QuickbaseProject): MilestoneStatus {
  const submitted = project[PROJECT_FIELDS.PERMIT_SUBMITTED]?.value
  const approved = project[PROJECT_FIELDS.PERMIT_APPROVED]?.value
  const asBuiltSubmitted = project[PROJECT_FIELDS.AS_BUILT_SUBMITTED_TO_AHJ]?.value
  const installCompleted = project[PROJECT_FIELDS.INSTALL_COMPLETED_DATE]?.value

  // Special case: install complete but no as-built
  if (installCompleted && !asBuiltSubmitted) {
    return {
      status: 'in-progress',
      date: approved ? new Date(approved) : undefined,
      warning: 'As-built plans needed for final inspection',
      substeps: [
        { label: 'Submitted', date: submitted ? new Date(submitted) : null, status: 'complete' },
        { label: 'Approved', date: approved ? new Date(approved) : null, status: 'complete' },
        { label: 'As-Built', date: null, status: 'pending' }
      ]
    }
  }

  if (asBuiltSubmitted) {
    return {
      status: 'complete',
      date: new Date(asBuiltSubmitted),
      substeps: [
        { label: 'Submitted', date: submitted ? new Date(submitted) : null, status: 'complete' },
        { label: 'Approved', date: approved ? new Date(approved) : null, status: 'complete' },
        { label: 'As-Built', date: new Date(asBuiltSubmitted), status: 'complete' }
      ]
    }
  }

  if (approved) {
    return {
      status: 'complete',
      date: new Date(approved),
      substeps: [
        { label: 'Submitted', date: submitted ? new Date(submitted) : null, status: 'complete' },
        { label: 'Approved', date: new Date(approved), status: 'complete' }
      ]
    }
  }

  if (submitted) {
    const daysWaiting = calculateDaysWaiting(submitted)
    return {
      status: 'in-progress',
      date: new Date(submitted),
      warning: daysWaiting > 30 ? 'Permit taking longer than expected' : undefined,
      substeps: [
        { label: 'Submitted', date: new Date(submitted), status: 'complete' },
        { label: 'Approved', date: null, status: 'pending' }
      ]
    }
  }

  return { status: 'pending' }
}

/**
 * @deprecated Use getPermittingStatus() instead - NEM is now part of the unified Permitting milestone
 */
export function getNEMStatus(project: QuickbaseProject): MilestoneStatus {
  const signaturesSent = project[PROJECT_FIELDS.NEM_SIGNATURES_SENT]?.value
  const submitted = project[PROJECT_FIELDS.NEM_SUBMITTED]?.value
  const approved = project[PROJECT_FIELDS.NEM_APPROVED]?.value

  if (approved) {
    return {
      status: 'complete',
      date: new Date(approved),
      substeps: [
        { label: 'Signatures Sent', date: signaturesSent ? new Date(signaturesSent) : null, status: 'complete' },
        { label: 'Submitted', date: submitted ? new Date(submitted) : null, status: 'complete' },
        { label: 'Approved', date: new Date(approved), status: 'complete' }
      ]
    }
  }

  if (submitted) {
    return {
      status: 'in-progress',
      date: new Date(submitted),
      substeps: [
        { label: 'Signatures Sent', date: signaturesSent ? new Date(signaturesSent) : null, status: 'complete' },
        { label: 'Submitted', date: new Date(submitted), status: 'complete' },
        { label: 'Approved', date: null, status: 'pending' }
      ]
    }
  }

  if (signaturesSent) {
    const daysWaiting = calculateDaysWaiting(signaturesSent)
    return {
      status: daysWaiting > 7 ? 'blocked' : 'in-progress',
      date: new Date(signaturesSent),
      urgent: daysWaiting > 7,
      warning: daysWaiting > 7 ? 'Customer signatures needed - follow up!' : undefined,
      substeps: [
        { label: 'Signatures Sent', date: new Date(signaturesSent), status: 'complete' },
        { label: 'Submitted', date: null, status: 'pending' },
        { label: 'Approved', date: null, status: 'pending' }
      ]
    }
  }

  return { status: 'pending' }
}

export function getInstallStatus(project: QuickbaseProject): MilestoneStatus {
  const scheduled = project[PROJECT_FIELDS.INSTALL_SCHEDULED_DATE_CAPTURE]?.value || 
                   project[PROJECT_FIELDS.ESTIMATED_INSTALL_DATE]?.value
  const completed = project[PROJECT_FIELDS.INSTALL_COMPLETED_DATE]?.value
  const readyForCommission = project[PROJECT_FIELDS.READY_FOR_COMMISSION]?.value

  if (readyForCommission) {
    return {
      status: 'complete',
      date: new Date(readyForCommission),
      substeps: [
        { label: 'Scheduled', date: scheduled ? new Date(scheduled) : null, status: 'complete' },
        { label: 'Completed', date: completed ? new Date(completed) : null, status: 'complete' },
        { label: 'Ready', date: new Date(readyForCommission), status: 'complete' }
      ]
    }
  }

  if (completed) {
    return {
      status: 'complete',
      date: new Date(completed),
      substeps: [
        { label: 'Scheduled', date: scheduled ? new Date(scheduled) : null, status: 'complete' },
        { label: 'Completed', date: new Date(completed), status: 'complete' }
      ]
    }
  }

  if (scheduled) {
    const installDate = new Date(scheduled)
    const now = new Date()
    const daysUntilInstall = Math.ceil((installDate.getTime() - now.getTime()) / (1000 * 60 * 60 * 24))
    
    return {
      status: daysUntilInstall <= 7 ? 'upcoming' : 'in-progress',
      date: new Date(scheduled),
      substeps: [
        { label: 'Scheduled', date: new Date(scheduled), status: 'complete' },
        { label: 'Completed', date: null, status: 'pending' }
      ]
    }
  }

  return { status: 'pending' }
}

export function getVerificationStatus(project: QuickbaseProject): MilestoneStatus {
  const installCompleted = project[PROJECT_FIELDS.INSTALL_COMPLETED_DATE]?.value
  const inspectionPassed = project[PROJECT_FIELDS.PASSING_INSPECTION_COMPLETED]?.value

  if (inspectionPassed) {
    return {
      status: 'complete',
      date: new Date(inspectionPassed)
    }
  }

  if (installCompleted) {
    const installDate = new Date(installCompleted)
    const now = new Date()
    const daysSinceInstall = Math.floor((now.getTime() - installDate.getTime()) / (1000 * 60 * 60 * 24))
    
    if (daysSinceInstall >= 1) {
      const estimatedDate = new Date(installDate)
      estimatedDate.setDate(estimatedDate.getDate() + 2)
      
      return {
        status: 'complete',
        date: estimatedDate,
        estimatedDate,
        calculated: true
      }
    } else {
      return {
        status: 'in-progress',
        date: new Date(installCompleted)
      }
    }
  }

  return { status: 'pending' }
}

export function getInspectionStatus(project: QuickbaseProject): MilestoneStatus {
  const inspectionPassed = project[PROJECT_FIELDS.PASSING_INSPECTION_COMPLETED]?.value
  const asBuiltSubmitted = project[PROJECT_FIELDS.AS_BUILT_SUBMITTED_TO_AHJ]?.value

  if (inspectionPassed) {
    return {
      status: 'complete',
      date: new Date(inspectionPassed),
      substeps: [
        { label: 'As-Built', date: asBuiltSubmitted ? new Date(asBuiltSubmitted) : null, status: 'complete' },
        { label: 'Passed', date: new Date(inspectionPassed), status: 'complete' }
      ]
    }
  }

  if (asBuiltSubmitted) {
    return {
      status: 'in-progress',
      date: new Date(asBuiltSubmitted),
      substeps: [
        { label: 'As-Built', date: new Date(asBuiltSubmitted), status: 'complete' },
        { label: 'Passed', date: null, status: 'pending' }
      ]
    }
  }

  return { status: 'pending' }
}

export function getPTOStatus(project: QuickbaseProject): MilestoneStatus {
  const submitted = project[PROJECT_FIELDS.PTO_SUBMITTED]?.value
  const approved = project[PROJECT_FIELDS.PTO_APPROVED]?.value
  const uploadedToLender = project[PROJECT_FIELDS.PTO_UPLOADED_TO_LENDER]?.value

  if (uploadedToLender) {
    return {
      status: 'complete',
      date: new Date(uploadedToLender),
      celebration: true,
      substeps: [
        { label: 'Submitted', date: submitted ? new Date(submitted) : null, status: 'complete' },
        { label: 'Approved', date: approved ? new Date(approved) : null, status: 'complete' },
        { label: 'Uploaded', date: new Date(uploadedToLender), status: 'complete', note: 'M3 Funded!' }
      ]
    }
  }

  if (approved) {
    return {
      status: 'complete',
      date: new Date(approved),
      substeps: [
        { label: 'Submitted', date: submitted ? new Date(submitted) : null, status: 'complete' },
        { label: 'Approved', date: new Date(approved), status: 'complete' },
        { label: 'Uploaded', date: null, status: 'pending' }
      ]
    }
  }

  if (submitted) {
    const daysWaiting = calculateDaysWaiting(submitted)
    return {
      status: 'in-progress',
      date: new Date(submitted),
      warning: daysWaiting > 42 ? 'Taking longer than expected' : undefined,
      substeps: [
        { label: 'Submitted', date: new Date(submitted), status: 'complete' },
        { label: 'Approved', date: null, status: 'pending' },
        { label: 'Uploaded', date: null, status: 'pending' }
      ]
    }
  }

  return { status: 'pending' }
}
