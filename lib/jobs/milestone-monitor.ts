/**
 * Milestone Monitoring Job
 * Checks for late/missed PC milestones and creates notifications
 */

import { qbClient } from '@/lib/quickbase/client';
import { createNotification, getNotificationsForUser } from '@/lib/db/notifications';
import { sendCustomEmail } from '@/lib/utils/email-helpers';
import { getPCMilestoneEmailTemplate } from '@/lib/utils/email-templates';
import { logInfo, logError } from '@/lib/logging/logger';
import { 
  PCMilestoneCheck, 
  PCNotificationSummary 
} from '@/lib/types/operations';
import { 
  PCMilestoneNotificationMetadata 
} from '@/lib/types/notification';
import { PROJECT_FIELDS } from '@/lib/constants/fieldIds';
import { getProjectsForMilestoneCheck, logNotificationToQuickBase } from '@/lib/quickbase/queries';
import { getPCNotificationPreferences, shouldSendEmailNotification } from '@/lib/db/pcNotificationPreferences';
import { PCNotificationPreferences } from '@/lib/types/operations';

/**
 * Main function to check milestones for all PCs or a specific PC
 */
export async function checkMilestones(pcEmail?: string): Promise<PCNotificationSummary> {
  const startTime = Date.now();
  logInfo('Starting milestone check', { pcEmail });

  const summary: PCNotificationSummary = {
    total_notifications: 0,
    by_type: {},
    by_priority: { critical: 0, normal: 0, info: 0 },
    created_count: 0,
    skipped_count: 0,
    error_count: 0
  };

  try {
    // Get projects for milestone checking
    const projects = await getProjectsForMilestoneCheck(pcEmail, `milestone-check-${Date.now()}`);
    logInfo(`Found ${projects.length} projects to check`, { pcEmail });

    // Check each project for milestone issues
    for (const project of projects) {
      try {
        const checks = await checkProjectMilestones(project);
        
        for (const check of checks) {
          if (check.should_notify) {
            try {
              await createNotificationForMilestone(check);
              summary.created_count++;
              summary.by_type[check.milestone_type] = (summary.by_type[check.milestone_type] || 0) + 1;
              summary.by_priority[check.notification_priority]++;
              summary.total_notifications++;
              
              // Send email notification if enabled
              await sendEmailNotification(check);
              
              logInfo('Created milestone notification', {
                projectId: check.project_id,
                milestoneType: check.milestone_type,
                priority: check.notification_priority
              });
            } catch (error) {
              logError('Failed to create notification', error, { 
                projectId: check.project_id,
                milestoneType: check.milestone_type 
              });
              summary.error_count++;
            }
          } else {
            summary.skipped_count++;
          }
        }
      } catch (error) {
        logError('Failed to check project milestones', error, { 
          projectId: project.PROJECT_ID 
        });
        summary.error_count++;
      }
    }

    const executionTime = Date.now() - startTime;
    logInfo('Milestone check completed', { 
      ...summary, 
      executionTimeMs: executionTime,
      pcEmail 
    });

    return summary;
  } catch (error) {
    logError('Milestone check failed', error, { pcEmail });
    summary.error_count++;
    return summary;
  }
}


/**
 * Check all milestones for a single project
 */
async function checkProjectMilestones(project: any): Promise<PCMilestoneCheck[]> {
  const checks: PCMilestoneCheck[] = [];

  // Check survey milestone
  const surveyCheck = checkSurveyMilestone(project);
  if (surveyCheck) checks.push(surveyCheck);

  // Check install milestone
  const installCheck = checkInstallMilestone(project);
  if (installCheck) checks.push(installCheck);

  // Check NEM milestone
  const nemCheck = checkNEMMilestone(project);
  if (nemCheck) checks.push(nemCheck);

  // Check PTO milestone
  const ptoCheck = checkPTOMilestone(project);
  if (ptoCheck) checks.push(ptoCheck);

  // Check unresponsive escalation
  const unresponsiveCheck = checkUnresponsiveEscalation(project);
  if (unresponsiveCheck) checks.push(unresponsiveCheck);

  return checks;
}

/**
 * Check if survey milestone is late
 */
function checkSurveyMilestone(project: any): PCMilestoneCheck | null {
  // Access normalized fields using FID-based keys
  const scheduledDate = project[PROJECT_FIELDS.SURVEY_SCHEDULED_DATE];
  const submittedDate = project[PROJECT_FIELDS.SURVEY_SUBMITTED];

  if (!scheduledDate || submittedDate) return null;

  const scheduled = new Date(scheduledDate);
  const today = new Date();
  const daysOverdue = Math.floor((today.getTime() - scheduled.getTime()) / (1000 * 60 * 60 * 24));

  if (daysOverdue <= 0) return null;

  return {
    project_id: project[PROJECT_FIELDS.PROJECT_ID],
    record_id: project[PROJECT_FIELDS.RECORD_ID],
    customer_name: project[PROJECT_FIELDS.CUSTOMER_NAME],
    coordinator_email: project[PROJECT_FIELDS.PROJECT_COORDINATOR_EMAIL],
    milestone_type: 'survey',
    scheduled_date: scheduled,
    completion_date: null,
    days_overdue: daysOverdue,
    should_notify: true,
    notification_priority: daysOverdue > 3 ? 'critical' : 'normal'
  };
}

/**
 * Check if install milestone is late
 */
function checkInstallMilestone(project: any): PCMilestoneCheck | null {
  const scheduledDate = project[PROJECT_FIELDS.INSTALL_SCHEDULED_DATE_CAPTURE];
  const completedDate = project[PROJECT_FIELDS.INSTALL_COMPLETED_DATE];

  if (!scheduledDate || completedDate) return null;

  const scheduled = new Date(scheduledDate);
  const today = new Date();
  const daysOverdue = Math.floor((today.getTime() - scheduled.getTime()) / (1000 * 60 * 60 * 24));

  if (daysOverdue <= 0) return null;

  return {
    project_id: project[PROJECT_FIELDS.PROJECT_ID],
    record_id: project[PROJECT_FIELDS.RECORD_ID],
    customer_name: project[PROJECT_FIELDS.CUSTOMER_NAME],
    coordinator_email: project[PROJECT_FIELDS.PROJECT_COORDINATOR_EMAIL],
    milestone_type: 'install',
    scheduled_date: scheduled,
    completion_date: null,
    days_overdue: daysOverdue,
    should_notify: true,
    notification_priority: daysOverdue > 2 ? 'critical' : 'normal'
  };
}

/**
 * Check if NEM milestone is overdue
 */
function checkNEMMilestone(project: any): PCMilestoneCheck | null {
  const nemStatus = project[PROJECT_FIELDS.NEM_INTERCONNECTION_STATUS];
  const nemSubmitted = project[PROJECT_FIELDS.NEM_SUBMITTED];
  
  // Check if NEM is in active phase and not yet submitted
  if (!nemStatus || nemStatus === 'Submitted' || nemStatus === 'Approved' || nemSubmitted) return null;

  // Determine if project is actively in NEM phase using status
  const activeNEMStatuses = ['Pending', 'In Progress', 'Under Review', 'Awaiting Response'];
  if (!activeNEMStatuses.includes(nemStatus)) return null;

  // Calculate days in NEM phase using proper phase start date
  // Use design completion date as NEM phase start, or permit approval as fallback
  const designCompleted = project[PROJECT_FIELDS.DESIGN_COMPLETED];
  const permitApproved = project[PROJECT_FIELDS.PERMIT_APPROVED];
  
  let phaseStartDate: Date | null = null;
  if (designCompleted) {
    phaseStartDate = new Date(designCompleted);
  } else if (permitApproved) {
    phaseStartDate = new Date(permitApproved);
  }
  
  if (!phaseStartDate) return null; // Can't determine phase start

  const today = new Date();
  const daysInPhase = Math.floor((today.getTime() - phaseStartDate.getTime()) / (1000 * 60 * 60 * 24));

  if (daysInPhase <= 30) return null;

  return {
    project_id: project[PROJECT_FIELDS.PROJECT_ID],
    record_id: project[PROJECT_FIELDS.RECORD_ID],
    customer_name: project[PROJECT_FIELDS.CUSTOMER_NAME],
    coordinator_email: project[PROJECT_FIELDS.PROJECT_COORDINATOR_EMAIL],
    milestone_type: 'nem',
    scheduled_date: null,
    completion_date: null,
    days_overdue: daysInPhase - 30,
    should_notify: true,
    notification_priority: daysInPhase > 45 ? 'critical' : 'normal'
  };
}

/**
 * Check if PTO milestone is overdue
 */
function checkPTOMilestone(project: any): PCMilestoneCheck | null {
  const ptoStatus = project[PROJECT_FIELDS.PTO_STATUS];
  const ptoApproved = project[PROJECT_FIELDS.PTO_APPROVED];
  
  // Check if PTO is in active phase and not yet approved
  if (!ptoStatus || ptoStatus === 'Approved' || ptoApproved) return null;

  // Determine if project is actively in PTO phase using status
  const activePTOStatuses = ['Pending', 'Submitted', 'Under Review', 'Awaiting Response'];
  if (!activePTOStatuses.includes(ptoStatus)) return null;

  // Calculate days in PTO phase using proper phase start date
  // Use inspection pass date as PTO phase start, or install completion as fallback
  const inspectionPassed = project[PROJECT_FIELDS.PASSING_INSPECTION_COMPLETED];
  const installCompleted = project[PROJECT_FIELDS.INSTALL_COMPLETED_DATE];
  
  let phaseStartDate: Date | null = null;
  if (inspectionPassed) {
    phaseStartDate = new Date(inspectionPassed);
  } else if (installCompleted) {
    phaseStartDate = new Date(installCompleted);
  }
  
  if (!phaseStartDate) return null; // Can't determine phase start

  const today = new Date();
  const daysInPhase = Math.floor((today.getTime() - phaseStartDate.getTime()) / (1000 * 60 * 60 * 24));

  if (daysInPhase <= 45) return null;

  return {
    project_id: project[PROJECT_FIELDS.PROJECT_ID],
    record_id: project[PROJECT_FIELDS.RECORD_ID],
    customer_name: project[PROJECT_FIELDS.CUSTOMER_NAME],
    coordinator_email: project[PROJECT_FIELDS.PROJECT_COORDINATOR_EMAIL],
    milestone_type: 'pto',
    scheduled_date: null,
    completion_date: null,
    days_overdue: daysInPhase - 45,
    should_notify: true,
    notification_priority: daysInPhase > 60 ? 'critical' : 'normal'
  };
}

/**
 * Check if customer is unresponsive and needs escalation
 */
function checkUnresponsiveEscalation(project: any): PCMilestoneCheck | null {
  const isUnresponsive = project[PROJECT_FIELDS.PC_IS_UNRESPONSIVE] === 'Yes';
  const daysSinceContact = Math.max(
    project[PROJECT_FIELDS.PC_DAYS_SINCE_CONTACT_INTAKE] || 0,
    project[PROJECT_FIELDS.PC_DAYS_SINCE_CONTACT_INSTALL] || 0,
    project[PROJECT_FIELDS.PC_DAYS_SINCE_CONTACT_NEM] || 0,
    project[PROJECT_FIELDS.PC_DAYS_SINCE_CONTACT_PTO] || 0
  );
  const contactAttempts = Math.max(
    project[PROJECT_FIELDS.PC_CONTACT_ATTEMPTS_INTAKE] || 0,
    project[PROJECT_FIELDS.PC_CONTACT_ATTEMPTS_INSTALL] || 0,
    project[PROJECT_FIELDS.PC_CONTACT_ATTEMPTS_NEM] || 0,
    project[PROJECT_FIELDS.PC_CONTACT_ATTEMPTS_PTO] || 0
  );

  if (!isUnresponsive && (daysSinceContact < 7 || contactAttempts < 3)) return null;

  return {
    project_id: project[PROJECT_FIELDS.PROJECT_ID],
    record_id: project[PROJECT_FIELDS.RECORD_ID],
    customer_name: project[PROJECT_FIELDS.CUSTOMER_NAME],
    coordinator_email: project[PROJECT_FIELDS.PROJECT_COORDINATOR_EMAIL],
    milestone_type: 'unresponsive',
    scheduled_date: null,
    completion_date: null,
    days_overdue: daysSinceContact,
    should_notify: true,
    notification_priority: daysSinceContact > 14 ? 'critical' : 'normal'
  };
}

/**
 * Create notification for a milestone check
 */
async function createNotificationForMilestone(check: PCMilestoneCheck) {
  // Check for existing unread notification for the same user, project, and type
  const existingNotifications = await getNotificationsForUser(check.coordinator_email, {
    limit: 10,
    offset: 0,
    unreadOnly: true
  });

  const notificationType = getNotificationType(check.milestone_type);
  const existingNotification = existingNotifications.find(n => 
    n.project_id === check.record_id && 
    n.type === notificationType && 
    !n.is_read
  );

  if (existingNotification) {
    logInfo('Skipping duplicate notification', {
      projectId: check.project_id,
      milestoneType: check.milestone_type,
      coordinatorEmail: check.coordinator_email,
      existingNotificationId: existingNotification.id
    });
    return existingNotification;
  }

  const metadata: PCMilestoneNotificationMetadata = {
    milestone_type: check.milestone_type,
    scheduled_date: check.scheduled_date?.toISOString() || '',
    days_overdue: check.days_overdue,
    days_in_phase: check.days_overdue,
    days_since_contact: check.days_overdue,
    contact_attempts: 0, // Would need to calculate from project data
    current_stage: 'Unknown', // Would need to determine from project data
    recommended_action: getRecommendedAction(check.milestone_type, check.days_overdue)
  };

  const title = getNotificationTitle(check.milestone_type, check.customer_name);
  const message = getNotificationMessage(check.milestone_type, check.customer_name, check.days_overdue);

  const notification = await createNotification({
    user_id: check.coordinator_email,
    project_id: check.record_id,
    type: getNotificationType(check.milestone_type),
    priority: check.notification_priority,
    source: 'system',
    title,
    message,
    metadata,
    icon: check.notification_priority === 'critical' ? 'alert-circle' : 'clock',
    color: check.notification_priority === 'critical' ? 'red' : 'orange',
    action_url: `/operations/projects/${check.project_id}`
  });

  // Log to QuickBase audit trail
  await logNotificationToQuickBase(
    notification.id,
    getNotificationType(check.milestone_type),
    check.project_id,
    check.coordinator_email,
    metadata
  );

  return notification;
}

/**
 * Send email notification for milestone
 */
async function sendEmailNotification(check: PCMilestoneCheck) {
  try {
    const notificationType = getNotificationType(check.milestone_type);
    
    // Check user preferences for email notifications
    const shouldSend = await shouldSendEmailNotification(check.coordinator_email, notificationType);
    
    if (!shouldSend) {
      logInfo('Email notification skipped due to user preferences', {
        coordinatorEmail: check.coordinator_email,
        milestoneType: check.milestone_type,
        notificationType
      });
      return;
    }

    const pcName = check.coordinator_email.split('@')[0]; // Simplified name extraction
    const dashboardUrl = `${process.env.NEXT_PUBLIC_APP_URL}/operations`;
    
    const emailTemplate = getPCMilestoneEmailTemplate(
      pcName,
      check.milestone_type,
      check.customer_name,
      check.project_id,
      check.days_overdue,
      getRecommendedAction(check.milestone_type, check.days_overdue),
      dashboardUrl
    );

    await sendCustomEmail({
      to: check.coordinator_email,
      subject: `${getNotificationTitle(check.milestone_type, check.customer_name)} - Action Required`,
      html: emailTemplate
    });

    logInfo('Sent milestone email notification', {
      coordinatorEmail: check.coordinator_email,
      milestoneType: check.milestone_type
    });
  } catch (error) {
    logError('Failed to send milestone email', error, {
      coordinatorEmail: check.coordinator_email,
      milestoneType: check.milestone_type
    });
  }
}

// Helper functions for notification content
function getNotificationType(milestoneType: string): string {
  const typeMap: Record<string, string> = {
    survey: 'milestone_survey_late',
    install: 'milestone_install_late',
    nem: 'milestone_nem_overdue',
    pto: 'milestone_pto_overdue',
    unresponsive: 'milestone_unresponsive_escalation'
  };
  return typeMap[milestoneType] || 'system_alert';
}

function getNotificationTitle(milestoneType: string, customerName: string): string {
  const titleMap: Record<string, string> = {
    survey: `Survey Overdue: ${customerName}`,
    install: `Install Overdue: ${customerName}`,
    nem: `NEM Overdue: ${customerName}`,
    pto: `PTO Overdue: ${customerName}`,
    unresponsive: `Customer Unresponsive: ${customerName}`
  };
  return titleMap[milestoneType] || `Milestone Alert: ${customerName}`;
}

function getNotificationMessage(milestoneType: string, customerName: string, daysOverdue: number): string {
  const messageMap: Record<string, string> = {
    survey: `The survey for ${customerName} was scheduled ${daysOverdue} days ago but hasn't been submitted yet.`,
    install: `The installation for ${customerName} was scheduled ${daysOverdue} days ago but hasn't been completed.`,
    nem: `${customerName}'s project has been in the NEM phase for ${daysOverdue} days without submission.`,
    pto: `${customerName}'s project has been in the PTO phase for ${daysOverdue} days without approval.`,
    unresponsive: `${customerName} has been unresponsive for ${daysOverdue} days despite multiple contact attempts.`
  };
  return messageMap[milestoneType] || `Milestone alert for ${customerName}`;
}

function getRecommendedAction(milestoneType: string, daysOverdue: number): string {
  const actionMap: Record<string, string> = {
    survey: 'Contact customer to schedule survey completion',
    install: 'Coordinate with installation team and customer',
    nem: 'Follow up on NEM submission status with utility',
    pto: 'Check PTO approval status and follow up if needed',
    unresponsive: 'Escalate to sales rep or try alternative contact methods'
  };
  return actionMap[milestoneType] || 'Review project status and take appropriate action';
}

