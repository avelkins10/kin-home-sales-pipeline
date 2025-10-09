/**
 * Email Helper Functions for Kin Home Sales Dashboard
 * 
 * Provides email sending functions with retry logic, error handling, and logging.
 * Wraps the mailer utility with business logic for different email types.
 */

import { sendMail } from './mailer';
import { logInfo, logError } from '@/lib/logging/logger';
import { 
  getInviteEmailTemplate, 
  getWelcomeEmailTemplate, 
  getPasswordResetEmailTemplate 
} from './email-templates';

interface EmailResult {
  success: boolean;
  error?: string;
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
      logInfo('Email sending disabled, skipping invite email', { to, name, role });
      return { success: true };
    }

    // Validate email configuration
    const config = validateEmailConfig();
    if (!config.valid) {
      logInfo('Email not configured, skipping invite email', { 
        to, 
        name, 
        role, 
        missingVars: config.missingVars 
      });
      return { success: true };
    }

    logInfo('Sending invite email', { to, name, role, invitedBy });

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

    logInfo('Invite email sent successfully', { to, name, role });
    return { success: true };

  } catch (error) {
    const errorMessage = error instanceof Error ? error.message : 'Unknown error';
    logError('Failed to send invite email', error instanceof Error ? error : new Error(errorMessage), {
      to,
      name,
      role
    });
    
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
      logInfo('Email sending disabled, skipping welcome email', { to, name, role });
      return { success: true };
    }

    // Validate email configuration
    const config = validateEmailConfig();
    if (!config.valid) {
      logInfo('Email not configured, skipping welcome email', { 
        to, 
        name, 
        role, 
        missingVars: config.missingVars 
      });
      return { success: true };
    }

    logInfo('Sending welcome email', { to, name, role });

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

    logInfo('Welcome email sent successfully', { to, name, role });
    return { success: true };

  } catch (error) {
    const errorMessage = error instanceof Error ? error.message : 'Unknown error';
    logError('Failed to send welcome email', error instanceof Error ? error : new Error(errorMessage), {
      to,
      name,
      role
    });
    
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
      logInfo('Email sending disabled, skipping password reset email', { to, name });
      return { success: true };
    }

    // Validate email configuration
    const config = validateEmailConfig();
    if (!config.valid) {
      logInfo('Email not configured, skipping password reset email', { 
        to, 
        name, 
        missingVars: config.missingVars 
      });
      return { success: true };
    }

    logInfo('Sending password reset email', { to, name });

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

    logInfo('Password reset email sent successfully', { to, name });
    return { success: true };

  } catch (error) {
    const errorMessage = error instanceof Error ? error.message : 'Unknown error';
    logError('Failed to send password reset email', error instanceof Error ? error : new Error(errorMessage), {
      to,
      name
    });
    
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
      logInfo('Email sending disabled, skipping custom email', { to, subject });
      return { success: true };
    }

    // Validate email configuration
    const config = validateEmailConfig();
    if (!config.valid) {
      logInfo('Email not configured, skipping custom email', { 
        to, 
        subject, 
        missingVars: config.missingVars 
      });
      return { success: true };
    }

    logInfo('Sending custom email', { to, subject });

    // Send email with retry logic
    await retryWithBackoff(async () => {
      await sendMail({
        to,
        subject,
        html
      });
    }, options.retryOptions);

    logInfo('Custom email sent successfully', { to, subject });
    return { success: true };

  } catch (error) {
    const errorMessage = error instanceof Error ? error.message : 'Unknown error';
    logError('Failed to send custom email', error instanceof Error ? error : new Error(errorMessage), {
      to,
      subject
    });
    
    return { 
      success: false, 
      error: errorMessage 
    };
  }
}
