import { PROJECT_FIELDS } from '@/lib/constants/fieldIds';
import { QuickbaseProject } from '@/lib/types/project';

/**
 * Calendar Event Interface
 * 
 * Represents a single calendar event extracted from a project.
 * Each project can generate 0-2 events (survey and/or install).
 */
export interface CalendarEvent {
  /** Unique identifier: format `${projectId}-${eventType}` */
  id: string;
  /** Display title: format `${customerName} - ${eventType}` */
  title: string;
  /** Event start date/time */
  start: Date;
  /** Event end date/time */
  end: Date;
  /** Type of event */
  type: 'survey' | 'install';
  /** Current status of the event */
  status: 'scheduled' | 'completed';
  /** Essential project information for event details */
  project: {
    recordId: string;
    projectId: string;
    customerName: string;
    customerAddress: string;
    customerPhone: string;
    salesOffice: string;
    closerName: string;
    setterName: string;
  };
}

/**
 * Field Selection Strategy Documentation
 * 
 * This function implements a carefully researched field selection strategy
 * based on usage percentages and reliability from Quickbase data analysis.
 * 
 * SURVEY EVENTS:
 * - Primary: SITE_SURVEY_ARRIVY_SCHEDULED (2526) - 100% usage ⭐ MOST RELIABLE
 * - Fallback: None - if Arrivy field is empty, skip survey event
 * - Completion: SURVEY_APPROVED (165) - 26% usage but reliable for completion status
 * 
 * INSTALL EVENTS:
 * - Primary: INSTALL_SCHEDULED_DATE_CAPTURE (710) - 54.4% usage ⭐ HIGHEST
 * - Secondary: INSTALL_SCHEDULED_START_DATE (178) - 20.1% usage
 * - Tertiary: ESTIMATED_INSTALL_DATE (1124) - 27.5% usage
 * - Completion: INSTALL_COMPLETED_DATE (534) - 50%+ usage, most reliable
 * 
 * FUTURE IMPROVEMENTS:
 * - If Quickbase adds more reliable survey scheduled fields, update survey logic here
 * - If install date reliability improves, consider reordering fallback priority
 * - Monitor field usage percentages quarterly and adjust as needed
 */

/**
 * Extracts calendar events from a project
 * 
 * @param project - The Quickbase project to extract events from
 * @returns Array of calendar events (0-2 events per project)
 */
export function getCalendarDates(project: QuickbaseProject): CalendarEvent[] {
  if (!project) {
    return [];
  }

  const events: CalendarEvent[] = [];

  // Extract survey event
  const surveyEvent = extractSurveyEvent(project);
  if (surveyEvent) {
    events.push(surveyEvent);
  }

  // Extract install event
  const installEvent = extractInstallEvent(project);
  if (installEvent) {
    events.push(installEvent);
  }

  return events;
}

/**
 * Extracts survey event from project
 * 
 * Uses SITE_SURVEY_ARRIVY_SCHEDULED as primary source (100% usage).
 * If Arrivy field is empty, skips survey event entirely.
 */
function extractSurveyEvent(project: QuickbaseProject): CalendarEvent | null {
  // Primary source: SITE_SURVEY_ARRIVY_SCHEDULED (field 2526) - 100% usage
  const scheduledDate = project.siteSurveyArrivyScheduled;
  
  if (!scheduledDate) {
    // No fallback - if Arrivy field is empty, skip survey event
    return null;
  }

  const startDate = new Date(scheduledDate);
  if (isNaN(startDate.getTime())) {
    // Invalid date - skip event
    return null;
  }

  // Determine completion status
  const isCompleted = !!project.surveyApproved;
  const status: 'scheduled' | 'completed' = isCompleted ? 'completed' : 'scheduled';

  // Event duration: Use exact timestamp from Arrivy (includes time)
  // Default to 2-hour window if time not available
  const endDate = new Date(startDate);
  if (startDate.getHours() === 0 && startDate.getMinutes() === 0) {
    // No specific time provided, default to 2-hour window
    endDate.setHours(2, 0, 0, 0);
  } else {
    // Specific time provided, add 2 hours
    endDate.setHours(endDate.getHours() + 2);
  }

  return {
    id: `${project.projectId}-survey`,
    title: `${project.customerName} - Site Survey`,
    start: startDate,
    end: endDate,
    type: 'survey',
    status,
    project: {
      recordId: project.recordId,
      projectId: project.projectId,
      customerName: project.customerName,
      customerAddress: project.customerAddress,
      customerPhone: project.customerPhone,
      salesOffice: project.salesOffice,
      closerName: project.closerName,
      setterName: project.setterName,
    },
  };
}

/**
 * Extracts install event from project
 * 
 * Uses fallback priority order for scheduled dates:
 * 1. INSTALL_SCHEDULED_DATE_CAPTURE (710) - 54.4% usage ⭐ HIGHEST
 * 2. INSTALL_SCHEDULED_START_DATE (178) - 20.1% usage  
 * 3. ESTIMATED_INSTALL_DATE (1124) - 27.5% usage
 */
function extractInstallEvent(project: QuickbaseProject): CalendarEvent | null {
  // Priority order for scheduled dates (use first non-null)
  const scheduledDate = 
    project.installScheduledDateCapture ||           // 54.4% usage ⭐ HIGHEST
    project.installScheduledStartDate ||             // 20.1% usage
    project.estimatedInstallDate;                    // 27.5% usage

  if (!scheduledDate) {
    return null;
  }

  const startDate = new Date(scheduledDate);
  if (isNaN(startDate.getTime())) {
    // Invalid date - skip event
    return null;
  }

  // Determine completion status
  const isCompleted = !!project.installCompletedDate;
  const status: 'scheduled' | 'completed' = isCompleted ? 'completed' : 'scheduled';

  // Event duration: Default to all-day event (start of day to end of day)
  const endDate = new Date(startDate);
  endDate.setHours(23, 59, 59, 999); // End of day

  return {
    id: `${project.projectId}-install`,
    title: `${project.customerName} - Installation`,
    start: startDate,
    end: endDate,
    type: 'install',
    status,
    project: {
      recordId: project.recordId,
      projectId: project.projectId,
      customerName: project.customerName,
      customerAddress: project.customerAddress,
      customerPhone: project.customerPhone,
      salesOffice: project.salesOffice,
      closerName: project.closerName,
      setterName: project.setterName,
    },
  };
}
