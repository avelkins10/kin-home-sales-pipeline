/**
 * Email Helper Functions for Kin Home Sales Dashboard
 * 
 * Provides email sending functions with retry logic, error handling, and logging.
 * Wraps the mailer utility with business logic for different email types.
 */

import { sendMail } from './mailer';
import { logInfo, logError, logEmailEvent } from '@/lib/logging/logger';
import {
  getInviteEmailTemplate,
  getWelcomeEmailTemplate,
  getPasswordResetEmailTemplate,
  getTaskSubmittedEmailTemplate,
  getTaskApprovedEmailTemplate,
  getTaskRevisionNeededEmailTemplate,
  getAllTasksCompleteEmailTemplate
} from './email-templates';

interface EmailResult {
  success: boolean;
  error?: string;
  skipped?: boolean;
}

interface RetryOptions {
  maxRetries?: number;
  baseDelay?: number;
}

/**
 * Retry function with exponential backoff
 */
async function retryWithBackoff<T>(
  fn: () => Promise<T>,
  options: RetryOptions = {}
): Promise<T> {
  const { maxRetries = 3, baseDelay = 1000 } = options;
  let lastError: Error;

  for (let attempt = 1; attempt <= maxRetries; attempt++) {
    try {
      return await fn();
    } catch (error) {
      lastError = error as Error;
      
      if (attempt === maxRetries) {
        throw lastError;
      }

      const delay = baseDelay * Math.pow(2, attempt - 1);
      logInfo(`Email send attempt ${attempt} failed, retrying in ${delay}ms`, {
        error: lastError.message,
        attempt,
        maxRetries
      });
      
      await new Promise(resolve => setTimeout(resolve, delay));
    }
  }

  throw lastError!;
}

/**
 * Validate email configuration
 */
export function validateEmailConfig(): { valid: boolean; missingVars: string[] } {
  const requiredVars = ['EMAIL_ENABLED', 'MAIL_PROVIDER', 'MAIL_FROM'];
  const missingVars: string[] = [];

  // Check if email is enabled
  if (process.env.EMAIL_ENABLED !== 'true') {
    return { valid: false, missingVars: ['EMAIL_ENABLED'] };
  }

  // Check required variables
  for (const varName of requiredVars) {
    if (!process.env[varName]) {
      missingVars.push(varName);
    }
  }

  // Check provider-specific API key
  const provider = process.env.MAIL_PROVIDER;
  if (provider === 'resend' && !process.env.RESEND_API_KEY) {
    missingVars.push('RESEND_API_KEY');
  } else if (provider === 'sendgrid' && !process.env.SENDGRID_API_KEY) {
    missingVars.push('SENDGRID_API_KEY');
  }

  return {
    valid: missingVars.length === 0,
    missingVars
  };
}

/**
 * Send invitation email to new user
 */
export async function sendInviteEmail(
  to: string,
  name: string,
  role: string,
  inviteLink: string,
  invitedBy: string,
  office?: string,
  offices?: string[]
): Promise<EmailResult> {
  try {
    // Check if email is enabled
    if (process.env.EMAIL_ENABLED !== 'true') {
      logEmailEvent('send_skipped', { recipient: to, emailType: 'invite' });
      return { success: false, error: 'email_disabled', skipped: true };
    }

    // Validate email configuration
    const config = validateEmailConfig();
    if (!config.valid) {
      logEmailEvent('send_skipped', { recipient: to, emailType: 'invite' });
      return { success: false, error: 'email_disabled', skipped: true };
    }

    logEmailEvent('send_attempt', { recipient: to, emailType: 'invite' });

    // Generate HTML template
    const html = getInviteEmailTemplate(name, role, inviteLink, invitedBy, office, offices);
    
    // Send email with retry logic
    await retryWithBackoff(async () => {
      await sendMail({
        to,
        subject: "You've been invited to Kin Home Sales Dashboard",
        html
      });
    });

    logEmailEvent('send_success', { recipient: to, emailType: 'invite' });
    return { success: true };

  } catch (error) {
    const errorMessage = error instanceof Error ? error.message : 'Unknown error';
    logEmailEvent('send_failure', { recipient: to, emailType: 'invite', error: errorMessage });
    
    return { 
      success: false, 
      error: errorMessage 
    };
  }
}

/**
 * Send welcome email after invite acceptance
 */
export async function sendWelcomeEmail(
  to: string,
  name: string,
  role: string
): Promise<EmailResult> {
  try {
    // Check if email is enabled
    if (process.env.EMAIL_ENABLED !== 'true') {
      logEmailEvent('send_skipped', { recipient: to, emailType: 'welcome' });
      return { success: false, error: 'email_disabled', skipped: true };
    }

    // Validate email configuration
    const config = validateEmailConfig();
    if (!config.valid) {
      logEmailEvent('send_skipped', { recipient: to, emailType: 'welcome' });
      return { success: false, error: 'email_disabled', skipped: true };
    }

    logEmailEvent('send_attempt', { recipient: to, emailType: 'welcome' });

    // Get dashboard URL
    const dashboardUrl = process.env.NEXT_PUBLIC_APP_URL || 'https://dashboard.kinhome.com';
    
    // Generate HTML template
    const html = getWelcomeEmailTemplate(name, role, dashboardUrl);
    
    // Send email with retry logic
    await retryWithBackoff(async () => {
      await sendMail({
        to,
        subject: "Welcome to Kin Home Sales Dashboard",
        html
      });
    });

    logEmailEvent('send_success', { recipient: to, emailType: 'welcome' });
    return { success: true };

  } catch (error) {
    const errorMessage = error instanceof Error ? error.message : 'Unknown error';
    logEmailEvent('send_failure', { recipient: to, emailType: 'welcome', error: errorMessage });
    
    return { 
      success: false, 
      error: errorMessage 
    };
  }
}

/**
 * Send password reset email
 */
export async function sendPasswordResetEmail(
  to: string,
  name: string,
  resetLink: string
): Promise<EmailResult> {
  try {
    // Check if email is enabled
    if (process.env.EMAIL_ENABLED !== 'true') {
      logEmailEvent('send_skipped', { recipient: to, emailType: 'password_reset' });
      return { success: false, error: 'email_disabled', skipped: true };
    }

    // Validate email configuration
    const config = validateEmailConfig();
    if (!config.valid) {
      logEmailEvent('send_skipped', { recipient: to, emailType: 'password_reset' });
      return { success: false, error: 'email_disabled', skipped: true };
    }

    logEmailEvent('send_attempt', { recipient: to, emailType: 'password_reset' });

    // Generate HTML template
    const html = getPasswordResetEmailTemplate(name, resetLink);
    
    // Send email with retry logic
    await retryWithBackoff(async () => {
      await sendMail({
        to,
        subject: "Reset your Kin Home Dashboard password",
        html
      });
    });

    logEmailEvent('send_success', { recipient: to, emailType: 'password_reset' });
    return { success: true };

  } catch (error) {
    const errorMessage = error instanceof Error ? error.message : 'Unknown error';
    logEmailEvent('send_failure', { recipient: to, emailType: 'password_reset', error: errorMessage });
    
    return { 
      success: false, 
      error: errorMessage 
    };
  }
}

/**
 * Send email with custom template (for future use)
 */
export async function sendCustomEmail(
  to: string,
  subject: string,
  html: string,
  options: { retryOptions?: RetryOptions } = {}
): Promise<EmailResult> {
  try {
    // Check if email is enabled
    if (process.env.EMAIL_ENABLED !== 'true') {
      logEmailEvent('send_skipped', { recipient: to, emailType: 'custom' });
      return { success: false, error: 'email_disabled', skipped: true };
    }

    // Validate email configuration
    const config = validateEmailConfig();
    if (!config.valid) {
      logEmailEvent('send_skipped', { recipient: to, emailType: 'custom' });
      return { success: false, error: 'email_disabled', skipped: true };
    }

    logEmailEvent('send_attempt', { recipient: to, emailType: 'custom' });

    // Send email with retry logic
    await retryWithBackoff(async () => {
      await sendMail({
        to,
        subject,
        html
      });
    }, options.retryOptions);

    logEmailEvent('send_success', { recipient: to, emailType: 'custom' });
    return { success: true };

  } catch (error) {
    const errorMessage = error instanceof Error ? error.message : 'Unknown error';
    logEmailEvent('send_failure', { recipient: to, emailType: 'custom', error: errorMessage });

    return {
      success: false,
      error: errorMessage
    };
  }
}

/**
 * Send Arrivy field alert email
 */
export async function sendArrivyFieldAlertEmail(
  coordinatorEmail: string,
  coordinatorName: string,
  eventType: 'LATE' | 'NOSHOW' | 'EXCEPTION' | 'CANCELLED',
  customerName: string,
  taskType: string,
  scheduledTime: string,
  crewNames: string[],
  eventMessage: string,
  trackerUrl: string,
  businessTrackerUrl: string
): Promise<EmailResult> {
  // Check if email is enabled globally
  if (process.env.EMAIL_ENABLED !== 'true') {
    logEmailEvent('send_skipped', { 
      recipient: coordinatorEmail, 
      emailType: 'arrivy_field_alert',
      reason: 'EMAIL_ENABLED not true' 
    });
    return { success: false, skipped: true };
  }

  // Validate email configuration
  const config = validateEmailConfig();
  if (!config.valid) {
    logEmailEvent('send_skipped', { 
      recipient: coordinatorEmail, 
      emailType: 'arrivy_field_alert',
      reason: 'Invalid email configuration' 
    });
    return { success: false, skipped: true };
  }

  try {
    // Generate email template
    const { getArrivyFieldAlertEmailTemplate } = await import('./email-templates');
    const dashboardUrl = process.env.NEXT_PUBLIC_APP_URL || 'https://dashboard.kinhome.com';
    
    const html = getArrivyFieldAlertEmailTemplate(
      coordinatorName,
      eventType,
      customerName,
      taskType,
      scheduledTime,
      crewNames,
      eventMessage,
      trackerUrl,
      businessTrackerUrl,
      dashboardUrl
    );

    // Generate subject line based on event type
    const subjectLines: Record<string, string> = {
      'LATE': `🚨 Task Running Late: ${customerName} - Action Required`,
      'NOSHOW': `🚫 Customer No-Show: ${customerName} - Immediate Action Needed`,
      'EXCEPTION': `⚠️ Field Exception: ${customerName} - Review Required`,
      'CANCELLED': `❌ Task Cancelled: ${customerName} - Update Required`,
    };
    const subject = subjectLines[eventType] || `Field Alert: ${customerName}`;

    logEmailEvent('send_attempt', { 
      recipient: coordinatorEmail, 
      emailType: 'arrivy_field_alert',
      eventType 
    });

    // Send email with retry logic
    await retryWithBackoff(async () => {
      await sendMail({
        to: coordinatorEmail,
        subject,
        html,
      });
    });

    logEmailEvent('send_success', { 
      recipient: coordinatorEmail, 
      emailType: 'arrivy_field_alert',
      eventType 
    });

    return { success: true };

  } catch (error) {
    const errorMessage = error instanceof Error ? error.message : 'Unknown error';
    logEmailEvent('send_failure', { 
      recipient: coordinatorEmail, 
      emailType: 'arrivy_field_alert',
      eventType,
      error: errorMessage 
    });

    return {
      success: false,
      error: errorMessage
    };
  }
}

/**
 * Send task submitted notification email
 * ALWAYS sent - no user configuration needed
 */
export async function sendTaskSubmittedEmail(
  recipientEmail: string,
  recipientName: string,
  submitterName: string,
  taskName: string,
  taskCategory: string | undefined,
  projectId: number,
  projectName: string,
  customerName: string,
  notes: string | undefined
): Promise<EmailResult> {
  try {
    // Check if email is enabled
    if (process.env.EMAIL_ENABLED !== 'true') {
      logEmailEvent('send_skipped', { recipient: recipientEmail, emailType: 'task_submitted' });
      return { success: false, error: 'email_disabled', skipped: true };
    }

    // Validate email configuration
    const config = validateEmailConfig();
    if (!config.valid) {
      logEmailEvent('send_skipped', { recipient: recipientEmail, emailType: 'task_submitted' });
      return { success: false, error: 'email_disabled', skipped: true };
    }

    logEmailEvent('send_attempt', { recipient: recipientEmail, emailType: 'task_submitted' });

    const dashboardUrl = process.env.NEXT_PUBLIC_APP_URL || 'https://dashboard.kinhome.com';
    const html = getTaskSubmittedEmailTemplate(
      recipientName,
      submitterName,
      taskName,
      taskCategory,
      projectId,
      projectName,
      customerName,
      notes,
      dashboardUrl
    );

    await retryWithBackoff(async () => {
      await sendMail({
        to: recipientEmail,
        subject: `Task Submitted: ${taskName}`,
        html
      });
    });

    logEmailEvent('send_success', { recipient: recipientEmail, emailType: 'task_submitted' });
    return { success: true };

  } catch (error) {
    const errorMessage = error instanceof Error ? error.message : 'Unknown error';
    logEmailEvent('send_failure', { recipient: recipientEmail, emailType: 'task_submitted', error: errorMessage });

    return {
      success: false,
      error: errorMessage
    };
  }
}

/**
 * Send task approved notification email
 * ALWAYS sent - no user configuration needed
 */
export async function sendTaskApprovedEmail(
  recipientEmail: string,
  recipientName: string,
  taskName: string,
  taskCategory: string | undefined,
  projectId: number,
  projectName: string,
  customerName: string
): Promise<EmailResult> {
  try {
    // Check if email is enabled
    if (process.env.EMAIL_ENABLED !== 'true') {
      logEmailEvent('send_skipped', { recipient: recipientEmail, emailType: 'task_approved' });
      return { success: false, error: 'email_disabled', skipped: true };
    }

    // Validate email configuration
    const config = validateEmailConfig();
    if (!config.valid) {
      logEmailEvent('send_skipped', { recipient: recipientEmail, emailType: 'task_approved' });
      return { success: false, error: 'email_disabled', skipped: true };
    }

    logEmailEvent('send_attempt', { recipient: recipientEmail, emailType: 'task_approved' });

    const dashboardUrl = process.env.NEXT_PUBLIC_APP_URL || 'https://dashboard.kinhome.com';
    const html = getTaskApprovedEmailTemplate(
      recipientName,
      taskName,
      taskCategory,
      projectId,
      projectName,
      customerName,
      dashboardUrl
    );

    await retryWithBackoff(async () => {
      await sendMail({
        to: recipientEmail,
        subject: `Task Approved: ${taskName}`,
        html
      });
    });

    logEmailEvent('send_success', { recipient: recipientEmail, emailType: 'task_approved' });
    return { success: true };

  } catch (error) {
    const errorMessage = error instanceof Error ? error.message : 'Unknown error';
    logEmailEvent('send_failure', { recipient: recipientEmail, emailType: 'task_approved', error: errorMessage });

    return {
      success: false,
      error: errorMessage
    };
  }
}

/**
 * Send task revision needed notification email
 * ALWAYS sent - no user configuration needed
 */
export async function sendTaskRevisionNeededEmail(
  recipientEmail: string,
  recipientName: string,
  taskName: string,
  taskCategory: string | undefined,
  projectId: number,
  projectName: string,
  customerName: string,
  opsFeedback: string | undefined
): Promise<EmailResult> {
  try {
    // Check if email is enabled
    if (process.env.EMAIL_ENABLED !== 'true') {
      logEmailEvent('send_skipped', { recipient: recipientEmail, emailType: 'task_revision_needed' });
      return { success: false, error: 'email_disabled', skipped: true };
    }

    // Validate email configuration
    const config = validateEmailConfig();
    if (!config.valid) {
      logEmailEvent('send_skipped', { recipient: recipientEmail, emailType: 'task_revision_needed' });
      return { success: false, error: 'email_disabled', skipped: true };
    }

    logEmailEvent('send_attempt', { recipient: recipientEmail, emailType: 'task_revision_needed' });

    const dashboardUrl = process.env.NEXT_PUBLIC_APP_URL || 'https://dashboard.kinhome.com';
    const html = getTaskRevisionNeededEmailTemplate(
      recipientName,
      taskName,
      taskCategory,
      projectId,
      projectName,
      customerName,
      opsFeedback,
      dashboardUrl
    );

    await retryWithBackoff(async () => {
      await sendMail({
        to: recipientEmail,
        subject: `Revision Needed: ${taskName}`,
        html
      });
    });

    logEmailEvent('send_success', { recipient: recipientEmail, emailType: 'task_revision_needed' });
    return { success: true };

  } catch (error) {
    const errorMessage = error instanceof Error ? error.message : 'Unknown error';
    logEmailEvent('send_failure', { recipient: recipientEmail, emailType: 'task_revision_needed', error: errorMessage });

    return {
      success: false,
      error: errorMessage
    };
  }
}

/**
 * Send all tasks complete notification email
 * ALWAYS sent - no user configuration needed
 */
export async function sendAllTasksCompleteEmail(
  recipientEmail: string,
  recipientName: string,
  projectId: number,
  projectName: string,
  customerName: string,
  totalTasks: number
): Promise<EmailResult> {
  try {
    // Check if email is enabled
    if (process.env.EMAIL_ENABLED !== 'true') {
      logEmailEvent('send_skipped', { recipient: recipientEmail, emailType: 'all_tasks_complete' });
      return { success: false, error: 'email_disabled', skipped: true };
    }

    // Validate email configuration
    const config = validateEmailConfig();
    if (!config.valid) {
      logEmailEvent('send_skipped', { recipient: recipientEmail, emailType: 'all_tasks_complete' });
      return { success: false, error: 'email_disabled', skipped: true };
    }

    logEmailEvent('send_attempt', { recipient: recipientEmail, emailType: 'all_tasks_complete' });

    const dashboardUrl = process.env.NEXT_PUBLIC_APP_URL || 'https://dashboard.kinhome.com';
    const html = getAllTasksCompleteEmailTemplate(
      recipientName,
      projectId,
      projectName,
      customerName,
      totalTasks,
      dashboardUrl
    );

    await retryWithBackoff(async () => {
      await sendMail({
        to: recipientEmail,
        subject: `All Tasks Complete: ${projectName}`,
        html
      });
    });

    logEmailEvent('send_success', { recipient: recipientEmail, emailType: 'all_tasks_complete' });
    return { success: true };

  } catch (error) {
    const errorMessage = error instanceof Error ? error.message : 'Unknown error';
    logEmailEvent('send_failure', { recipient: recipientEmail, emailType: 'all_tasks_complete', error: errorMessage });

    return {
      success: false,
      error: errorMessage
    };
  }
}
