export const runtime = 'nodejs';

import { NextRequest, NextResponse } from 'next/server';
import { requireRole } from '@/lib/auth/guards';
import { sendMail } from '@/lib/utils/mailer';
import { validateEmailConfig } from '@/lib/utils/email-helpers';

/**
 * GET /api/test-email?to=email@example.com
 * Test email configuration and send a test email
 * Super admin only
 */
export async function GET(request: NextRequest) {
  try {
    // Require super admin
    const auth = await requireRole(['super_admin']);
    if (!auth.authorized) {
      return auth.response;
    }

    const { searchParams } = new URL(request.url);
    const recipientEmail = searchParams.get('to') || auth.session.user.email;

    // Step 1: Check environment variables
    const envCheck = {
      EMAIL_ENABLED: process.env.EMAIL_ENABLED,
      MAIL_PROVIDER: process.env.MAIL_PROVIDER,
      RESEND_API_KEY: process.env.RESEND_API_KEY ? '✓ Set' : '✗ Missing',
      MAIL_FROM: process.env.MAIL_FROM,
      NEXT_PUBLIC_APP_URL: process.env.NEXT_PUBLIC_APP_URL,
    };

    // Step 2: Validate email configuration
    const emailConfig = validateEmailConfig();

    // Step 3: Try to send test email
    let emailResult = null;
    let emailError = null;

    if (emailConfig.valid) {
      try {
        await sendMail({
          to: recipientEmail,
          subject: 'Test Email from Kin Home Sales Dashboard',
          html: `
            <!DOCTYPE html>
            <html>
              <head>
                <meta charset="utf-8">
                <meta name="viewport" content="width=device-width, initial-scale=1.0">
              </head>
              <body style="font-family: Arial, sans-serif; line-height: 1.6; color: #333; max-width: 600px; margin: 0 auto; padding: 20px;">
                <div style="background-color: #2563eb; color: white; padding: 20px; border-radius: 8px; text-align: center; margin-bottom: 30px;">
                  <h1 style="margin: 0; font-size: 24px;">Test Email</h1>
                </div>

                <div style="margin-bottom: 20px;">
                  <h2 style="color: #2563eb; margin: 0 0 10px 0;">Email Configuration Test</h2>
                  <p style="margin: 0 0 10px 0;">This is a test email from the Kin Home Sales Dashboard.</p>
                  <p style="margin: 0;">If you're seeing this, your email configuration is working correctly!</p>
                </div>

                <div style="background-color: #f3f4f6; padding: 15px; border-radius: 6px; margin-bottom: 20px;">
                  <h3 style="margin: 0 0 10px 0; font-size: 16px;">Email Details</h3>
                  <p style="margin: 5px 0; font-size: 14px;"><strong>Sent to:</strong> ${recipientEmail}</p>
                  <p style="margin: 5px 0; font-size: 14px;"><strong>From:</strong> ${process.env.MAIL_FROM || 'Not configured'}</p>
                  <p style="margin: 5px 0; font-size: 14px;"><strong>Provider:</strong> ${process.env.MAIL_PROVIDER || 'Not configured'}</p>
                  <p style="margin: 5px 0; font-size: 14px;"><strong>Time:</strong> ${new Date().toISOString()}</p>
                </div>

                <div style="border-top: 1px solid #e5e7eb; padding-top: 15px; font-size: 12px; color: #6b7280; text-align: center;">
                  <p style="margin: 0;">Kin Home Sales Dashboard</p>
                </div>
              </body>
            </html>
          `,
        });

        emailResult = {
          success: true,
          message: 'Test email sent successfully',
          sentTo: recipientEmail,
        };
      } catch (error) {
        emailError = {
          message: error instanceof Error ? error.message : 'Unknown error',
          stack: error instanceof Error ? error.stack : undefined,
        };
      }
    }

    // Return comprehensive diagnostic information
    return NextResponse.json({
      success: emailResult?.success || false,
      environment: envCheck,
      emailConfig: {
        valid: emailConfig.valid,
        errors: emailConfig.missingVars || [],
      },
      emailResult,
      emailError,
      instructions: !emailConfig.valid
        ? 'Email configuration is invalid. Please set the required environment variables in Vercel and redeploy.'
        : emailResult
        ? 'Test email sent! Check your inbox (and spam folder).'
        : 'Email configuration is valid but sending failed. See emailError for details.',
    });
  } catch (error) {
    return NextResponse.json(
      {
        success: false,
        error: error instanceof Error ? error.message : 'Unknown error',
        stack: error instanceof Error ? error.stack : undefined,
      },
      { status: 500 }
    );
  }
}
