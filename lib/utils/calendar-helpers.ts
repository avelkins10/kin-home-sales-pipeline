// lib/utils/calendar-helpers.ts
import { PROJECT_FIELDS } from '@/lib/constants/fieldIds';
import { QuickbaseProject } from '@/lib/types/project';

export interface CalendarEvent {
  id: string;
  title: string;
  date: string;
  type: 'survey' | 'install' | 'inspection' | 'pto';
  status: 'scheduled' | 'completed' | 'pending';
  project: {
    recordId: number;
    projectId: string;
    customerName?: string | null;
    customerAddress?: string | null;
    customerPhone?: string | null;
    salesOffice?: string | null;
    closerName?: string | null;
    setterName?: string | null;
  };
}

/**
 * Normalize date string by trimming whitespace and checking if empty
 */
function normalizeDateString(dateString: string | undefined | null): string | null {
  if (!dateString) return null;
  const trimmed = dateString.trim();
  return trimmed.length > 0 ? trimmed : null;
}

/**
 * Extract calendar dates from Quickbase project data using field IDs
 * This function reads values from the raw Quickbase row using PROJECT_FIELDS constants
 */
export function getCalendarDates(project?: QuickbaseProject | null): CalendarEvent[] {
  const events: CalendarEvent[] = [];

  // Early return for null/undefined projects
  if (!project) {
    return events;
  }

  // Extract project identity fields for payload
  const recordId = project[PROJECT_FIELDS.RECORD_ID]?.value;
  const projectId = project[PROJECT_FIELDS.PROJECT_ID]?.value;
  const customerName = project[PROJECT_FIELDS.CUSTOMER_NAME]?.value;
  const customerAddress = project[PROJECT_FIELDS.CUSTOMER_ADDRESS]?.value;
  const customerPhone = project[PROJECT_FIELDS.CUSTOMER_PHONE]?.value;
  const salesOffice = project[PROJECT_FIELDS.SALES_OFFICE]?.value;
  const closerName = project[PROJECT_FIELDS.CLOSER_NAME]?.value;
  const setterName = project[PROJECT_FIELDS.SETTER_NAME]?.value;

  // Ensure we have required fields
  if (!recordId || !projectId) {
    return events;
  }

  const projectPayload = {
    recordId: Number(recordId),
    projectId: String(projectId),
    customerName: customerName || null,
    customerAddress: customerAddress || null,
    customerPhone: customerPhone || null,
    salesOffice: salesOffice || null,
    closerName: closerName || null,
    setterName: setterName || null,
  };

  // Survey scheduled date: SITE_SURVEY_ARRIVY_SCHEDULED (field 2526)
  const surveyScheduled = normalizeDateString(project[PROJECT_FIELDS.SITE_SURVEY_ARRIVY_SCHEDULED]?.value);
  if (surveyScheduled) {
    events.push({
      id: `${recordId}-survey-scheduled`,
      title: `Survey Scheduled - ${customerName || 'Unknown Customer'}`,
      date: surveyScheduled,
      type: 'survey',
      status: 'scheduled',
      project: projectPayload,
    });
  }

  // Survey approved: SURVEY_APPROVED (field 165)
  const surveyApproved = normalizeDateString(project[PROJECT_FIELDS.SURVEY_APPROVED]?.value);
  if (surveyApproved) {
    events.push({
      id: `${recordId}-survey-approved`,
      title: `Survey Approved - ${customerName || 'Unknown Customer'}`,
      date: surveyApproved,
      type: 'survey',
      status: 'completed',
      project: projectPayload,
    });
  }

  // Install scheduled candidates: INSTALL_SCHEDULED_DATE_CAPTURE (field 710) - PRIMARY
  const installScheduled = normalizeDateString(project[PROJECT_FIELDS.INSTALL_SCHEDULED_DATE_CAPTURE]?.value);
  if (installScheduled) {
    events.push({
      id: `${recordId}-install-scheduled`,
      title: `Install Scheduled - ${customerName || 'Unknown Customer'}`,
      date: installScheduled,
      type: 'install',
      status: 'scheduled',
      project: projectPayload,
    });
  }

  // Install scheduled start date: INSTALL_SCHEDULED_START_DATE (field 178) - BACKUP
  const installStartDate = normalizeDateString(project[PROJECT_FIELDS.INSTALL_SCHEDULED_START_DATE]?.value);
  if (installStartDate && !installScheduled) {
    events.push({
      id: `${recordId}-install-start`,
      title: `Install Start - ${customerName || 'Unknown Customer'}`,
      date: installStartDate,
      type: 'install',
      status: 'scheduled',
      project: projectPayload,
    });
  }

  // Estimated install date: ESTIMATED_INSTALL_DATE (field 1124) - BACKUP
  const estimatedInstall = normalizeDateString(project[PROJECT_FIELDS.ESTIMATED_INSTALL_DATE]?.value);
  if (estimatedInstall && !installScheduled && !installStartDate) {
    events.push({
      id: `${recordId}-install-estimated`,
      title: `Install Estimated - ${customerName || 'Unknown Customer'}`,
      date: estimatedInstall,
      type: 'install',
      status: 'pending',
      project: projectPayload,
    });
  }

  // Install completed: INSTALL_COMPLETED_DATE (field 534)
  const installCompleted = normalizeDateString(project[PROJECT_FIELDS.INSTALL_COMPLETED_DATE]?.value);
  if (installCompleted) {
    events.push({
      id: `${recordId}-install-completed`,
      title: `Install Completed - ${customerName || 'Unknown Customer'}`,
      date: installCompleted,
      type: 'install',
      status: 'completed',
      project: projectPayload,
    });
  }

  // Inspection scheduled: INSPECTION_SCHEDULED_DATE (field 226)
  const inspectionScheduled = normalizeDateString(project[PROJECT_FIELDS.INSPECTION_SCHEDULED_DATE]?.value);
  if (inspectionScheduled) {
    events.push({
      id: `${recordId}-inspection-scheduled`,
      title: `Inspection Scheduled - ${customerName || 'Unknown Customer'}`,
      date: inspectionScheduled,
      type: 'inspection',
      status: 'scheduled',
      project: projectPayload,
    });
  }

  // Inspection completed: PASSING_INSPECTION_COMPLETED (field 491)
  const inspectionCompleted = normalizeDateString(project[PROJECT_FIELDS.PASSING_INSPECTION_COMPLETED]?.value);
  if (inspectionCompleted) {
    events.push({
      id: `${recordId}-inspection-completed`,
      title: `Inspection Completed - ${customerName || 'Unknown Customer'}`,
      date: inspectionCompleted,
      type: 'inspection',
      status: 'completed',
      project: projectPayload,
    });
  }

  // PTO submitted: PTO_SUBMITTED (field 537)
  const ptoSubmitted = normalizeDateString(project[PROJECT_FIELDS.PTO_SUBMITTED]?.value);
  if (ptoSubmitted) {
    events.push({
      id: `${recordId}-pto-submitted`,
      title: `PTO Submitted - ${customerName || 'Unknown Customer'}`,
      date: ptoSubmitted,
      type: 'pto',
      status: 'completed',
      project: projectPayload,
    });
  }

  // PTO approved: PTO_APPROVED (field 538)
  const ptoApproved = normalizeDateString(project[PROJECT_FIELDS.PTO_APPROVED]?.value);
  if (ptoApproved) {
    events.push({
      id: `${recordId}-pto-approved`,
      title: `PTO Approved - ${customerName || 'Unknown Customer'}`,
      date: ptoApproved,
      type: 'pto',
      status: 'completed',
      project: projectPayload,
    });
  }

  return events;
}
