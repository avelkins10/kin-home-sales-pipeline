/**
 * Task Email Sender
 *
 * Handles sending task notification emails with proper data fetching
 */

import {
  sendTaskSubmittedEmail,
  sendTaskApprovedEmail,
  sendTaskRevisionNeededEmail,
  sendAllTasksCompleteEmail
} from './email-helpers';
import { logInfo, logError } from '@/lib/logging/logger';

interface TaskEmailParams {
  type: 'task_submitted' | 'task_approved' | 'task_revision_needed' | 'all_tasks_complete';
  userId: string; // Email address
  projectId: number;
  taskName: string;
  taskCategory?: string;
  submissionId?: number;
  opsDisposition?: 'Approved' | 'Needs Revision';
  opsFeedback?: string;
  totalTasks?: number;
  approvedTasks?: number;
  submitterEmail?: string; // For task_submitted type
  notes?: string; // For task_submitted type
}

/**
 * Extract user's first name from email
 * Examples:
 *   john.doe@company.com -> John
 *   jane_smith@company.com -> Jane
 *   bob@company.com -> Bob
 */
function getUserNameFromEmail(email: string): string {
  try {
    const localPart = email.split('@')[0];
    const namePart = localPart.split(/[._]/)[0]; // Split by dot or underscore
    return namePart.charAt(0).toUpperCase() + namePart.slice(1).toLowerCase();
  } catch {
    return 'there'; // Fallback
  }
}

/**
 * Fetch project details from QuickBase
 */
async function getProjectDetails(projectId: number): Promise<{
  projectName: string;
  customerName: string;
} | null> {
  try {
    // Dynamic import to avoid circular dependencies
    const { getProjectById } = await import('@/lib/quickbase/queries');

    const project = await getProjectById(projectId);
    if (!project) {
      logError('Project not found for email', new Error('Project not found'), { projectId });
      return null;
    }

    return {
      projectName: project.project_name || `Project ${projectId}`,
      customerName: project.customer_name || 'Customer'
    };
  } catch (error) {
    logError('Failed to fetch project details for email', error as Error, { projectId });
    return null;
  }
}

/**
 * Send task notification email based on type
 */
export async function sendTaskNotificationEmail(params: TaskEmailParams): Promise<void> {
  try {
    // Get project details
    const projectDetails = await getProjectDetails(params.projectId);
    if (!projectDetails) {
      console.warn('[sendTaskNotificationEmail] Skipping email - project details not found', {
        projectId: params.projectId
      });
      return;
    }

    const recipientName = getUserNameFromEmail(params.userId);
    const { projectName, customerName } = projectDetails;

    // Send appropriate email based on type
    switch (params.type) {
      case 'task_submitted':
        {
          // For task_submitted, we need to send to OPS coordinators
          // For now, we'll skip this as it requires OPS coordinator lookup
          // TODO: Implement OPS coordinator email fetching
          const submitterName = params.submitterEmail
            ? getUserNameFromEmail(params.submitterEmail)
            : recipientName;

          logInfo('[sendTaskNotificationEmail] Skipping task_submitted email (OPS coordinator lookup not implemented)', {
            taskName: params.taskName,
            projectId: params.projectId
          });

          // await sendTaskSubmittedEmail(
          //   opsCoordinatorEmail,
          //   opsCoordinatorName,
          //   submitterName,
          //   params.taskName,
          //   params.taskCategory,
          //   params.projectId,
          //   projectName,
          //   customerName,
          //   params.notes
          // );
        }
        break;

      case 'task_approved':
        await sendTaskApprovedEmail(
          params.userId,
          recipientName,
          params.taskName,
          params.taskCategory,
          params.projectId,
          projectName,
          customerName
        );
        break;

      case 'task_revision_needed':
        await sendTaskRevisionNeededEmail(
          params.userId,
          recipientName,
          params.taskName,
          params.taskCategory,
          params.projectId,
          projectName,
          customerName,
          params.opsFeedback
        );
        break;

      case 'all_tasks_complete':
        await sendAllTasksCompleteEmail(
          params.userId,
          recipientName,
          params.projectId,
          projectName,
          customerName,
          params.totalTasks || params.approvedTasks || 0
        );
        break;
    }

    logInfo('[sendTaskNotificationEmail] Email sent successfully', {
      type: params.type,
      userId: params.userId,
      projectId: params.projectId,
      taskName: params.taskName
    });

  } catch (error) {
    // Log error but don't throw - email failures shouldn't break the notification system
    logError('[sendTaskNotificationEmail] Failed to send task email', error as Error, {
      type: params.type,
      userId: params.userId,
      projectId: params.projectId,
      taskName: params.taskName
    });
  }
}
