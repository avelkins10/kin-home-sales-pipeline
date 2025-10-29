/**
 * HTML Email Templates for Kin Home Sales Dashboard
 * 
 * Provides professional email templates for:
 * - User invitations
 * - Welcome emails
 * - Password reset emails
 * 
 * All templates are mobile-responsive and tested across major email clients.
 */

interface EmailStyles {
  primary: string;
  secondary: string;
  text: string;
  background: string;
  border: string;
  shadow: string;
  fontFamily: string;
  fontSize: {
    small: string;
    medium: string;
    large: string;
    xlarge: string;
  };
}

/**
 * Get consistent email styles for all templates
 */
function getEmailStyles(): EmailStyles {
  return {
    primary: '#0066CC',
    secondary: '#00A86B',
    text: '#333333',
    background: '#F5F5F5',
    border: '#E5E5E5',
    shadow: '0 2px 4px rgba(0,0,0,0.1)',
    fontFamily: '-apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, "Helvetica Neue", Arial, sans-serif',
    fontSize: {
      small: '14px',
      medium: '16px',
      large: '18px',
      xlarge: '24px'
    }
  };
}

/**
 * Get full HTML email layout with proper structure and compatibility
 */
function getEmailLayout(content: string, preheader?: string): string {
  const styles = getEmailStyles();
  
  return `<!DOCTYPE html>
<html lang="en" xmlns="http://www.w3.org/1999/xhtml" xmlns:v="urn:schemas-microsoft-com:vml" xmlns:o="urn:schemas-microsoft-com:office:office">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <meta http-equiv="X-UA-Compatible" content="IE=edge">
  <meta name="x-apple-disable-message-reformatting">
  <meta name="format-detection" content="telephone=no,address=no,email=no,date=no,url=no">
  <meta name="color-scheme" content="light">
  <meta name="supported-color-schemes" content="light">
  <title>Kin Home Sales Dashboard</title>
  ${preheader ? `<div style="display: none; font-size: 1px; color: #fefefe; line-height: 1px; font-family: ${styles.fontFamily}; max-height: 0px; max-width: 0px; opacity: 0; overflow: hidden;">${preheader}</div>` : ''}
  
  <!--[if mso]>
  <noscript>
    <xml>
      <o:OfficeDocumentSettings>
        <o:PixelsPerInch>96</o:PixelsPerInch>
      </o:OfficeDocumentSettings>
    </xml>
  </noscript>
  <![endif]-->
  
  <style>
    /* Reset styles */
    body, table, td, p, a, li, blockquote {
      -webkit-text-size-adjust: 100%;
      -ms-text-size-adjust: 100%;
    }
    table, td {
      mso-table-lspace: 0pt;
      mso-table-rspace: 0pt;
    }
    img {
      -ms-interpolation-mode: bicubic;
      border: 0;
      height: auto;
      line-height: 100%;
      outline: none;
      text-decoration: none;
    }
    
    /* Mobile styles */
    @media only screen and (max-width: 600px) {
      .mobile-padding {
        padding: 20px !important;
      }
      .mobile-text-center {
        text-align: center !important;
      }
      .mobile-full-width {
        width: 100% !important;
      }
      .mobile-stack {
        display: block !important;
        width: 100% !important;
      }
    }
  </style>
</head>
<body style="margin: 0; padding: 0; width: 100%; background-color: ${styles.background};">
  <table role="presentation" cellspacing="0" cellpadding="0" border="0" width="100%" style="background-color: ${styles.background};">
    <tr>
      <td align="center" style="padding: 40px 20px;">
        <table role="presentation" cellspacing="0" cellpadding="0" border="0" width="600" style="max-width: 600px; background-color: #ffffff; border-radius: 8px; box-shadow: ${styles.shadow};">
          <tr>
            <td style="padding: 40px 40px 20px 40px;" class="mobile-padding">
              ${content}
            </td>
          </tr>
        </table>
      </td>
    </tr>
  </table>
</body>
</html>`;
}

/**
 * Generate HTML template for user invitation emails
 */
export function getInviteEmailTemplate(
  name: string,
  role: string,
  inviteLink: string,
  invitedBy: string,
  office?: string,
  offices?: string[]
): string {
  const styles = getEmailStyles();
  
  // Role-specific messaging
  const getRoleMessage = (role: string): string => {
    switch (role) {
      case 'office_leader':
        return `As an Office Leader, you'll have access to all projects in your assigned office${office ? ` (${office})` : ''}. You can track project progress, manage holds, and oversee your team's performance.`;
      case 'area_director':
        return `As an Area Director, you'll have access to all projects across your assigned offices${offices && offices.length > 0 ? ` (${offices.join(', ')})` : ''}. You can monitor regional performance and support multiple offices.`;
      case 'divisional':
        return `As a Divisional Manager, you'll have access to all projects across your assigned offices${offices && offices.length > 0 ? ` (${offices.join(', ')})` : ''}. You can oversee large-scale operations and strategic planning.`;
      case 'team_lead':
        return `As a Team Lead, you'll have access to projects for your managed sales representatives. You can track team performance and provide coaching support.`;
      case 'closer':
        return `As a Closer, you'll have access to your assigned projects where you're listed as the closer. You can track project progress and manage customer communications.`;
      case 'setter':
        return `As a Setter, you'll have access to your assigned projects where you're listed as the setter. You can track project progress and manage customer communications.`;
      default:
        return `You'll have access to the Kin Home Sales Dashboard based on your role and assigned permissions.`;
    }
  };

  const roleMessage = getRoleMessage(role);
  const officeInfo = office || (offices && offices.length > 0 ? offices.join(', ') : '');
  
  const content = `
    <!-- Header with Logo -->
    <table role="presentation" cellspacing="0" cellpadding="0" border="0" width="100%">
      <tr>
        <td align="center" style="padding-bottom: 30px;">
          <div style="background-color: ${styles.primary}; color: white; padding: 20px; border-radius: 8px; text-align: center;">
            <h1 style="margin: 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.xlarge}; font-weight: 600; color: white;">
              Kin Home Sales Dashboard
            </h1>
          </div>
        </td>
      </tr>
    </table>

    <!-- Personalized Greeting -->
    <div style="margin-bottom: 30px;">
      <h2 style="margin: 0 0 10px 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.large}; font-weight: 600; color: ${styles.text};">
        Hi ${name}!
      </h2>
      <p style="margin: 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; color: ${styles.text}; line-height: 1.5;">
        ${invitedBy} has invited you to join the Kin Home Sales Dashboard.
      </p>
    </div>

    <!-- Invite Message -->
    <div style="margin-bottom: 30px; padding: 20px; background-color: #f8f9fa; border-left: 4px solid ${styles.secondary}; border-radius: 4px;">
      <p style="margin: 0 0 15px 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; color: ${styles.text}; line-height: 1.5;">
        <strong>Your Role:</strong> ${role.replace('_', ' ').replace(/\b\w/g, l => l.toUpperCase())}
      </p>
      ${officeInfo ? `
        <p style="margin: 0 0 15px 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; color: ${styles.text}; line-height: 1.5;">
          <strong>Assigned Office(s):</strong> ${officeInfo}
        </p>
      ` : ''}
      <p style="margin: 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; color: ${styles.text}; line-height: 1.5;">
        ${roleMessage}
      </p>
    </div>

    <!-- Call to Action -->
    <div style="margin-bottom: 30px; text-align: center;">
      <a href="${inviteLink}" 
         style="display: inline-block; background-color: ${styles.primary}; color: white; text-decoration: none; padding: 15px 30px; border-radius: 6px; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; font-weight: 600; text-align: center; min-width: 200px;">
        Accept Invite
      </a>
    </div>

    <!-- Alternative Link -->
    <div style="margin-bottom: 30px; padding: 15px; background-color: #f8f9fa; border-radius: 4px; text-align: center;">
      <p style="margin: 0 0 10px 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.small}; color: #666; line-height: 1.4;">
        If the button doesn't work, copy and paste this link:
      </p>
      <p style="margin: 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.small}; color: ${styles.primary}; word-break: break-all; line-height: 1.4;">
        ${inviteLink}
      </p>
    </div>

    <!-- Expiration Notice -->
    <div style="margin-bottom: 30px; padding: 15px; background-color: #fff3cd; border: 1px solid #ffeaa7; border-radius: 4px;">
      <p style="margin: 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.small}; color: #856404; line-height: 1.4;">
        <strong>⏰ Important:</strong> This invite expires in 7 days for security reasons. Please accept it soon to activate your account.
      </p>
    </div>

    <!-- Security Notice -->
    <div style="margin-bottom: 30px; padding: 15px; background-color: #f8d7da; border: 1px solid #f5c6cb; border-radius: 4px;">
      <p style="margin: 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.small}; color: #721c24; line-height: 1.4;">
        <strong>🔒 Security:</strong> If you didn't expect this invite, please ignore this email. Your account will not be created unless you accept the invite.
      </p>
    </div>

    <!-- Footer -->
    <div style="border-top: 1px solid ${styles.border}; padding-top: 20px; text-align: center;">
      <p style="margin: 0 0 10px 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.small}; color: #666; line-height: 1.4;">
        Questions? Contact your manager or IT support.
      </p>
      <p style="margin: 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.small}; color: #999; line-height: 1.4;">
        © ${new Date().getFullYear()} Kin Home. All rights reserved.
      </p>
    </div>
  `;

  return getEmailLayout(content, `${name}, you've been invited to join the Kin Home Sales Dashboard`);
}

/**
 * Generate HTML template for welcome emails after invite acceptance
 */
export function getWelcomeEmailTemplate(
  name: string,
  role: string,
  dashboardUrl: string
): string {
  const styles = getEmailStyles();
  
  const getRoleTips = (role: string): string => {
    switch (role) {
      case 'office_leader':
        return `
          <li>Review all projects in your office using the dashboard filters</li>
          <li>Monitor project holds and take action to resolve blockers</li>
          <li>Track team performance and identify training opportunities</li>
        `;
      case 'area_director':
        return `
          <li>Compare performance across multiple offices</li>
          <li>Identify regional trends and opportunities</li>
          <li>Support office leaders with data-driven insights</li>
        `;
      case 'divisional':
        return `
          <li>Oversee large-scale operations across multiple regions</li>
          <li>Analyze strategic performance metrics</li>
          <li>Make data-driven decisions for business growth</li>
        `;
      case 'team_lead':
        return `
          <li>Monitor your team's project progress</li>
          <li>Provide coaching based on performance data</li>
          <li>Identify team members who need additional support</li>
        `;
      case 'closer':
        return `
          <li>Track your assigned projects from start to finish</li>
          <li>Update project status and communicate with customers</li>
          <li>Monitor your performance metrics and goals</li>
        `;
      case 'setter':
        return `
          <li>Track your assigned projects from start to finish</li>
          <li>Update project status and communicate with customers</li>
          <li>Monitor your performance metrics and goals</li>
        `;
      default:
        return `
          <li>Explore the dashboard to understand your role-specific features</li>
          <li>Contact your manager if you need help getting started</li>
        `;
    }
  };

  const roleTips = getRoleTips(role);
  
  const content = `
    <!-- Header with Logo -->
    <table role="presentation" cellspacing="0" cellpadding="0" border="0" width="100%">
      <tr>
        <td align="center" style="padding-bottom: 30px;">
          <div style="background-color: ${styles.secondary}; color: white; padding: 20px; border-radius: 8px; text-align: center;">
            <h1 style="margin: 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.xlarge}; font-weight: 600; color: white;">
              Welcome to Kin Home!
            </h1>
          </div>
        </td>
      </tr>
    </table>

    <!-- Welcome Message -->
    <div style="margin-bottom: 30px;">
      <h2 style="margin: 0 0 10px 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.large}; font-weight: 600; color: ${styles.text};">
        Welcome, ${name}!
      </h2>
      <p style="margin: 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; color: ${styles.text}; line-height: 1.5;">
        Your account has been successfully activated. You're now ready to access the Kin Home Sales Dashboard.
      </p>
    </div>

    <!-- Quick Start Guide -->
    <div style="margin-bottom: 30px; padding: 20px; background-color: #f8f9fa; border-left: 4px solid ${styles.primary}; border-radius: 4px;">
      <h3 style="margin: 0 0 15px 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; font-weight: 600; color: ${styles.text};">
        Getting Started
      </h3>
      <p style="margin: 0 0 15px 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; color: ${styles.text}; line-height: 1.5;">
        As a <strong>${role.replace('_', ' ').replace(/\b\w/g, l => l.toUpperCase())}</strong>, here's what you can do:
      </p>
      <ul style="margin: 0; padding-left: 20px; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; color: ${styles.text}; line-height: 1.5;">
        ${roleTips}
      </ul>
    </div>

    <!-- Dashboard Access -->
    <div style="margin-bottom: 30px; text-align: center;">
      <a href="${dashboardUrl}" 
         style="display: inline-block; background-color: ${styles.primary}; color: white; text-decoration: none; padding: 15px 30px; border-radius: 6px; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; font-weight: 600; text-align: center; min-width: 200px;">
        Access Dashboard
      </a>
    </div>

    <!-- Support Information -->
    <div style="margin-bottom: 30px; padding: 20px; background-color: #e7f3ff; border: 1px solid #b3d9ff; border-radius: 4px;">
      <h3 style="margin: 0 0 15px 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; font-weight: 600; color: ${styles.text};">
        Need Help?
      </h3>
      <p style="margin: 0 0 10px 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; color: ${styles.text}; line-height: 1.5;">
        If you have questions or need assistance:
      </p>
      <ul style="margin: 0; padding-left: 20px; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; color: ${styles.text}; line-height: 1.5;">
        <li>Contact your manager or team lead</li>
        <li>Reach out to IT support for technical issues</li>
        <li>Check the help documentation in the dashboard</li>
      </ul>
    </div>

    <!-- Footer -->
    <div style="border-top: 1px solid ${styles.border}; padding-top: 20px; text-align: center;">
      <p style="margin: 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.small}; color: #666; line-height: 1.4;">
        Thank you for joining the Kin Home team!
      </p>
      <p style="margin: 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.small}; color: #999; line-height: 1.4;">
        © ${new Date().getFullYear()} Kin Home. All rights reserved.
      </p>
    </div>
  `;

  return getEmailLayout(content, `Welcome to Kin Home Sales Dashboard, ${name}!`);
}

/**
 * Generate HTML template for password reset emails
 */
export function getPasswordResetEmailTemplate(
  name: string,
  resetLink: string
): string {
  const styles = getEmailStyles();
  
  const content = `
    <!-- Header with Logo -->
    <table role="presentation" cellspacing="0" cellpadding="0" border="0" width="100%">
      <tr>
        <td align="center" style="padding-bottom: 30px;">
          <div style="background-color: ${styles.primary}; color: white; padding: 20px; border-radius: 8px; text-align: center;">
            <h1 style="margin: 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.xlarge}; font-weight: 600; color: white;">
              Password Reset Request
            </h1>
          </div>
        </td>
      </tr>
    </table>

    <!-- Reset Request Message -->
    <div style="margin-bottom: 30px;">
      <h2 style="margin: 0 0 10px 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.large}; font-weight: 600; color: ${styles.text};">
        Hi ${name},
      </h2>
      <p style="margin: 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; color: ${styles.text}; line-height: 1.5;">
        We received a request to reset your password for the Kin Home Sales Dashboard.
      </p>
    </div>

    <!-- Reset Button -->
    <div style="margin-bottom: 30px; text-align: center;">
      <a href="${resetLink}" 
         style="display: inline-block; background-color: ${styles.primary}; color: white; text-decoration: none; padding: 15px 30px; border-radius: 6px; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; font-weight: 600; text-align: center; min-width: 200px;">
        Reset Password
      </a>
    </div>

    <!-- Alternative Link -->
    <div style="margin-bottom: 30px; padding: 15px; background-color: #f8f9fa; border-radius: 4px; text-align: center;">
      <p style="margin: 0 0 10px 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.small}; color: #666; line-height: 1.4;">
        If the button doesn't work, copy and paste this link:
      </p>
      <p style="margin: 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.small}; color: ${styles.primary}; word-break: break-all; line-height: 1.4;">
        ${resetLink}
      </p>
    </div>

    <!-- Expiration Notice -->
    <div style="margin-bottom: 30px; padding: 15px; background-color: #fff3cd; border: 1px solid #ffeaa7; border-radius: 4px;">
      <p style="margin: 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.small}; color: #856404; line-height: 1.4;">
        <strong>⏰ Important:</strong> This password reset link expires in 1 hour for security reasons.
      </p>
    </div>

    <!-- Security Notice -->
    <div style="margin-bottom: 30px; padding: 15px; background-color: #f8d7da; border: 1px solid #f5c6cb; border-radius: 4px;">
      <p style="margin: 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.small}; color: #721c24; line-height: 1.4;">
        <strong>🔒 Security:</strong> If you didn't request this password reset, please ignore this email. Your password will remain unchanged.
      </p>
    </div>

    <!-- Security Tips -->
    <div style="margin-bottom: 30px; padding: 20px; background-color: #e7f3ff; border: 1px solid #b3d9ff; border-radius: 4px;">
      <h3 style="margin: 0 0 15px 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; font-weight: 600; color: ${styles.text};">
        Password Security Tips
      </h3>
      <ul style="margin: 0; padding-left: 20px; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; color: ${styles.text}; line-height: 1.5;">
        <li>Use a strong, unique password</li>
        <li>Include uppercase and lowercase letters, numbers, and symbols</li>
        <li>Don't reuse passwords from other accounts</li>
        <li>Consider using a password manager</li>
      </ul>
    </div>

    <!-- Footer -->
    <div style="border-top: 1px solid ${styles.border}; padding-top: 20px; text-align: center;">
      <p style="margin: 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.small}; color: #666; line-height: 1.4;">
        Questions? Contact IT support for assistance.
      </p>
      <p style="margin: 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.small}; color: #999; line-height: 1.4;">
        © ${new Date().getFullYear()} Kin Home. All rights reserved.
      </p>
    </div>
  `;

  return getEmailLayout(content, `Reset your Kin Home Dashboard password`);
}

/**
 * Generate HTML template for PC milestone notification emails
 */
export function getPCMilestoneEmailTemplate(
  pcName: string,
  milestoneType: 'survey' | 'install' | 'nem' | 'pto' | 'unresponsive',
  customerName: string,
  projectId: string,
  daysOverdue: number,
  recommendedAction: string,
  dashboardUrl: string
): string {
  const styles = getEmailStyles();
  
  // Milestone-specific messaging and colors
  const getMilestoneInfo = (type: string) => {
    switch (type) {
      case 'survey':
        return {
          title: 'Survey Overdue Alert',
          icon: '📋',
          color: '#dc3545', // Red for critical
          message: `The survey for ${customerName} was scheduled ${daysOverdue} days ago but hasn't been submitted yet.`,
          details: 'Survey completion is required to move the project forward.'
        };
      case 'install':
        return {
          title: 'Installation Overdue Alert',
          icon: '🔧',
          color: '#dc3545', // Red for critical
          message: `The installation for ${customerName} was scheduled ${daysOverdue} days ago but hasn't been completed.`,
          details: 'Installation completion is required for project progression.'
        };
      case 'nem':
        return {
          title: 'NEM Phase Overdue Alert',
          icon: '⚡',
          color: '#fd7e14', // Orange for warning
          message: `${customerName}'s project has been in the NEM phase for ${daysOverdue} days without submission.`,
          details: 'NEM submission is required for utility interconnection approval.'
        };
      case 'pto':
        return {
          title: 'PTO Phase Overdue Alert',
          icon: '✅',
          color: '#fd7e14', // Orange for warning
          message: `${customerName}'s project has been in the PTO phase for ${daysOverdue} days without approval.`,
          details: 'PTO approval is required for project completion.'
        };
      case 'unresponsive':
        return {
          title: 'Customer Unresponsive Alert',
          icon: '📞',
          color: '#dc3545', // Red for critical
          message: `${customerName} has been unresponsive for ${daysOverdue} days despite multiple contact attempts.`,
          details: 'Customer engagement is required to maintain project momentum.'
        };
      default:
        return {
          title: 'Milestone Alert',
          icon: '⚠️',
          color: '#6c757d',
          message: `Milestone alert for ${customerName}`,
          details: 'Please review project status and take appropriate action.'
        };
    }
  };

  const milestoneInfo = getMilestoneInfo(milestoneType);
  
  const content = `
    <!-- Header with Alert Icon -->
    <table role="presentation" cellspacing="0" cellpadding="0" border="0" width="100%">
      <tr>
        <td align="center" style="padding-bottom: 30px;">
          <div style="background-color: ${milestoneInfo.color}; color: white; padding: 20px; border-radius: 8px; text-align: center;">
            <div style="font-size: 32px; margin-bottom: 10px;">${milestoneInfo.icon}</div>
            <h1 style="margin: 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.xlarge}; font-weight: 600; color: white;">
              ${milestoneInfo.title}
            </h1>
          </div>
        </td>
      </tr>
    </table>

    <!-- Greeting -->
    <div style="margin-bottom: 30px;">
      <h2 style="margin: 0 0 10px 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.large}; font-weight: 600; color: ${styles.text};">
        Hi ${pcName},
      </h2>
      <p style="margin: 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; color: ${styles.text}; line-height: 1.5;">
        ${milestoneInfo.message}
      </p>
    </div>

    <!-- Project Details Card -->
    <div style="margin-bottom: 30px; padding: 20px; background-color: #f8f9fa; border: 1px solid ${styles.border}; border-radius: 8px;">
      <h3 style="margin: 0 0 15px 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; font-weight: 600; color: ${styles.text};">
        Project Details
      </h3>
      <table role="presentation" cellspacing="0" cellpadding="0" border="0" width="100%">
        <tr>
          <td style="padding: 8px 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; color: ${styles.text}; font-weight: 600; width: 120px;">
            Customer:
          </td>
          <td style="padding: 8px 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; color: ${styles.text};">
            ${customerName}
          </td>
        </tr>
        <tr>
          <td style="padding: 8px 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; color: ${styles.text}; font-weight: 600;">
            Project ID:
          </td>
          <td style="padding: 8px 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; color: ${styles.text};">
            ${projectId}
          </td>
        </tr>
        <tr>
          <td style="padding: 8px 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; color: ${styles.text}; font-weight: 600;">
            Milestone:
          </td>
          <td style="padding: 8px 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; color: ${styles.text};">
            ${milestoneType.charAt(0).toUpperCase() + milestoneType.slice(1)}
          </td>
        </tr>
        <tr>
          <td style="padding: 8px 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; color: ${styles.text}; font-weight: 600;">
            Days Overdue:
          </td>
          <td style="padding: 8px 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; color: ${milestoneInfo.color}; font-weight: 600;">
            ${daysOverdue} days
          </td>
        </tr>
      </table>
      <p style="margin: 15px 0 0 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; color: ${styles.text}; line-height: 1.5;">
        ${milestoneInfo.details}
      </p>
    </div>

    <!-- Recommended Action -->
    <div style="margin-bottom: 30px; padding: 20px; background-color: #e7f3ff; border: 1px solid #b3d9ff; border-radius: 8px;">
      <h3 style="margin: 0 0 15px 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; font-weight: 600; color: ${styles.text};">
        Recommended Action
      </h3>
      <p style="margin: 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; color: ${styles.text}; line-height: 1.5;">
        ${recommendedAction}
      </p>
    </div>

    <!-- Call to Action -->
    <div style="margin-bottom: 30px; text-align: center;">
      <a href="${dashboardUrl}" 
         style="display: inline-block; background-color: ${styles.primary}; color: white; text-decoration: none; padding: 15px 30px; border-radius: 6px; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; font-weight: 600; text-align: center; min-width: 200px;">
        View Project
      </a>
    </div>

    <!-- Alternative Link -->
    <div style="margin-bottom: 30px; padding: 15px; background-color: #f8f9fa; border-radius: 4px; text-align: center;">
      <p style="margin: 0 0 10px 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.small}; color: #666; line-height: 1.4;">
        If the button doesn't work, copy and paste this link:
      </p>
      <p style="margin: 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.small}; color: ${styles.primary}; word-break: break-all; line-height: 1.4;">
        ${dashboardUrl}
      </p>
    </div>

    <!-- Urgency Notice -->
    <div style="margin-bottom: 30px; padding: 15px; background-color: #fff3cd; border: 1px solid #ffeaa7; border-radius: 4px;">
      <p style="margin: 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.small}; color: #856404; line-height: 1.4;">
        <strong>⏰ Urgent:</strong> This milestone requires immediate attention to prevent project delays and maintain customer satisfaction.
      </p>
    </div>

    <!-- Footer -->
    <div style="border-top: 1px solid ${styles.border}; padding-top: 20px; text-align: center;">
      <p style="margin: 0 0 10px 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.small}; color: #666; line-height: 1.4;">
        This is an automated notification from the Kin Home Operations Dashboard.
      </p>
      <p style="margin: 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.small}; color: #999; line-height: 1.4;">
        © ${new Date().getFullYear()} Kin Home. All rights reserved.
      </p>
    </div>
  `;

  return getEmailLayout(content, `${milestoneInfo.title}: ${customerName} - Action Required`);
}

/**
 * Generate HTML template for Arrivy field alert notification
 */
export function getArrivyFieldAlertEmailTemplate(
  coordinatorName: string,
  eventType: 'LATE' | 'NOSHOW' | 'EXCEPTION' | 'CANCELLED',
  customerName: string,
  taskType: string,
  scheduledTime: string,
  crewNames: string[],
  eventMessage: string,
  trackerUrl: string,
  businessTrackerUrl: string,
  dashboardUrl: string
): string {
  const styles = getEmailStyles();

  // Event-specific styling and messaging
  const getEventInfo = () => {
    switch (eventType) {
      case 'LATE':
        return {
          color: '#dc3545',
          icon: '⏰',
          title: 'Task Running Late',
          preheader: 'Task running late - immediate action required',
          recommendedAction: 'Contact customer immediately to confirm appointment and provide updated ETA. Update project notes with current status.',
          urgencyNotice: '⚠️ This alert requires immediate attention. Please respond within 15 minutes.',
        };
      case 'NOSHOW':
        return {
          color: '#dc3545',
          icon: '🚫',
          title: 'Customer No-Show',
          preheader: 'Customer no-show reported',
          recommendedAction: 'Attempt to reach customer immediately to understand the situation. Document the no-show and reschedule if appropriate.',
          urgencyNotice: '⚠️ This alert requires immediate attention. Please respond within 15 minutes.',
        };
      case 'EXCEPTION':
        return {
          color: '#fd7e14',
          icon: '⚠️',
          title: 'Field Exception Reported',
          preheader: 'Field crew reported an exception',
          recommendedAction: 'Review crew report and determine if additional resources or rescheduling is needed. Contact crew and customer as appropriate.',
          urgencyNotice: null,
        };
      case 'CANCELLED':
        return {
          color: '#6c757d',
          icon: '❌',
          title: 'Task Cancelled',
          preheader: 'Task has been cancelled',
          recommendedAction: 'Verify cancellation reason and update project status in QuickBase. Follow up with customer if needed.',
          urgencyNotice: null,
        };
    }
  };

  const eventInfo = getEventInfo();
  const taskTypeLabel = taskType.charAt(0).toUpperCase() + taskType.slice(1);
  const crewList = crewNames.length > 0 ? crewNames.join(', ') : 'Not assigned';

  const content = `
    <!-- Alert Header -->
    <div style="background: ${eventInfo.color}; color: white; padding: 20px; text-align: center; border-radius: 8px 8px 0 0;">
      <div style="font-size: 48px; margin-bottom: 10px;">${eventInfo.icon}</div>
      <h1 style="margin: 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.xlarge}; font-weight: 600;">
        ${eventInfo.title}
      </h1>
    </div>

    <!-- Main Content -->
    <div style="padding: 30px; background: white; border-radius: 0 0 8px 8px;">
      <p style="margin: 0 0 20px 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; color: ${styles.text}; line-height: 1.6;">
        Hi <strong>${coordinatorName}</strong>,
      </p>

      <p style="margin: 0 0 25px 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; color: ${styles.text}; line-height: 1.6;">
        A field alert has been triggered for <strong>${customerName}</strong>'s ${taskTypeLabel.toLowerCase()} appointment.
      </p>

      <!-- Task Details Card -->
      <div style="background: #f8f9fa; border-left: 4px solid ${eventInfo.color}; padding: 20px; margin-bottom: 25px; border-radius: 4px;">
        <h3 style="margin: 0 0 15px 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.large}; color: ${styles.text};">
          Task Details
        </h3>
        <table style="width: 100%; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium};">
          <tr>
            <td style="padding: 8px 0; color: #666; width: 40%;">Customer:</td>
            <td style="padding: 8px 0; color: ${styles.text}; font-weight: 600;">${customerName}</td>
          </tr>
          <tr>
            <td style="padding: 8px 0; color: #666;">Task Type:</td>
            <td style="padding: 8px 0; color: ${styles.text}; font-weight: 600;">${taskTypeLabel}</td>
          </tr>
          <tr>
            <td style="padding: 8px 0; color: #666;">Scheduled Time:</td>
            <td style="padding: 8px 0; color: ${styles.text}; font-weight: 600;">${scheduledTime}</td>
          </tr>
          <tr>
            <td style="padding: 8px 0; color: #666;">Assigned Crew:</td>
            <td style="padding: 8px 0; color: ${styles.text}; font-weight: 600;">${crewList}</td>
          </tr>
        </table>
      </div>

      <!-- Event Details -->
      <div style="margin-bottom: 25px;">
        <h3 style="margin: 0 0 10px 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.large}; color: ${styles.text};">
          Event Details
        </h3>
        <p style="margin: 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; color: ${styles.text}; line-height: 1.6; padding: 15px; background: #fff3cd; border-radius: 4px;">
          ${eventMessage}
        </p>
      </div>

      <!-- Recommended Action -->
      <div style="background: #e7f3ff; border-left: 4px solid ${styles.primary}; padding: 20px; margin-bottom: 25px; border-radius: 4px;">
        <h3 style="margin: 0 0 10px 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.large}; color: ${styles.text};">
          Recommended Action
        </h3>
        <p style="margin: 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; color: ${styles.text}; line-height: 1.6;">
          ${eventInfo.recommendedAction}
        </p>
      </div>

      ${eventInfo.urgencyNotice ? `
      <!-- Urgency Notice -->
      <div style="background: #fff3cd; border: 2px solid #ffc107; padding: 15px; margin-bottom: 25px; border-radius: 4px; text-align: center;">
        <p style="margin: 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; color: #856404; font-weight: 600;">
          ${eventInfo.urgencyNotice}
        </p>
      </div>
      ` : ''}

      <!-- Action Buttons -->
      <div style="text-align: center; margin: 30px 0;">
        <!-- Primary CTA -->
        <a href="${dashboardUrl}" style="display: inline-block; background: ${styles.primary}; color: white; padding: 14px 32px; text-decoration: none; border-radius: 6px; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; font-weight: 600; margin: 0 8px 12px 8px; box-shadow: ${styles.shadow};">
          View in Operations Dashboard
        </a>
        
        <!-- Secondary CTA -->
        <a href="${trackerUrl}" style="display: inline-block; background: ${styles.secondary}; color: white; padding: 14px 32px; text-decoration: none; border-radius: 6px; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; font-weight: 600; margin: 0 8px 12px 8px; box-shadow: ${styles.shadow};">
          View Live Tracker
        </a>
      </div>

      <!-- Business Tracker Link -->
      <div style="text-align: center; margin-bottom: 25px;">
        <a href="${businessTrackerUrl}" style="font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.small}; color: ${styles.primary}; text-decoration: underline;">
          View in Arrivy Dashboard →
        </a>
      </div>
    </div>

    <!-- Footer -->
    <div style="border-top: 1px solid ${styles.border}; padding-top: 20px; text-align: center;">
      <p style="margin: 0 0 10px 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.small}; color: #666; line-height: 1.4;">
        This is an automated field alert from the Kin Home Operations Dashboard.
      </p>
      <p style="margin: 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.small}; color: #999; line-height: 1.4;">
        © ${new Date().getFullYear()} Kin Home. All rights reserved.
      </p>
    </div>
  `;

  return getEmailLayout(content, eventInfo.preheader);
}

/**
 * Generate HTML template for task submitted notification
 */
export function getTaskSubmittedEmailTemplate(
  recipientName: string,
  submitterName: string,
  taskName: string,
  taskCategory: string | undefined,
  projectId: number,
  projectName: string,
  customerName: string,
  notes: string | undefined,
  dashboardUrl: string
): string {
  const styles = getEmailStyles();

  const content = `
    <!-- Header -->
    <table role="presentation" cellspacing="0" cellpadding="0" border="0" width="100%">
      <tr>
        <td align="center" style="padding-bottom: 30px;">
          <div style="background-color: ${styles.primary}; color: white; padding: 20px; border-radius: 8px; text-align: center;">
            <div style="font-size: 32px; margin-bottom: 10px;">📋</div>
            <h1 style="margin: 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.xlarge}; font-weight: 600; color: white;">
              Task Submitted for Review
            </h1>
          </div>
        </td>
      </tr>
    </table>

    <!-- Greeting -->
    <div style="margin-bottom: 30px;">
      <h2 style="margin: 0 0 10px 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.large}; font-weight: 600; color: ${styles.text};">
        Hi ${recipientName},
      </h2>
      <p style="margin: 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; color: ${styles.text}; line-height: 1.5;">
        ${submitterName} has submitted a task for review.
      </p>
    </div>

    <!-- Task Details Card -->
    <div style="margin-bottom: 30px; padding: 20px; background-color: #f8f9fa; border: 1px solid ${styles.border}; border-radius: 8px;">
      <h3 style="margin: 0 0 15px 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; font-weight: 600; color: ${styles.text};">
        Task Details
      </h3>
      <table role="presentation" cellspacing="0" cellpadding="0" border="0" width="100%">
        <tr>
          <td style="padding: 8px 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; color: ${styles.text}; font-weight: 600; width: 120px;">
            Task:
          </td>
          <td style="padding: 8px 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; color: ${styles.text};">
            ${taskName}
          </td>
        </tr>
        ${taskCategory ? `
        <tr>
          <td style="padding: 8px 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; color: ${styles.text}; font-weight: 600;">
            Category:
          </td>
          <td style="padding: 8px 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; color: ${styles.text};">
            ${taskCategory}
          </td>
        </tr>
        ` : ''}
        <tr>
          <td style="padding: 8px 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; color: ${styles.text}; font-weight: 600;">
            Project:
          </td>
          <td style="padding: 8px 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; color: ${styles.text};">
            ${projectName}
          </td>
        </tr>
        <tr>
          <td style="padding: 8px 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; color: ${styles.text}; font-weight: 600;">
            Customer:
          </td>
          <td style="padding: 8px 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; color: ${styles.text};">
            ${customerName}
          </td>
        </tr>
        <tr>
          <td style="padding: 8px 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; color: ${styles.text}; font-weight: 600;">
            Submitted By:
          </td>
          <td style="padding: 8px 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; color: ${styles.text};">
            ${submitterName}
          </td>
        </tr>
      </table>
      ${notes ? `
        <div style="margin-top: 15px; padding-top: 15px; border-top: 1px solid ${styles.border};">
          <p style="margin: 0 0 8px 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; color: ${styles.text}; font-weight: 600;">
            Submission Notes:
          </p>
          <p style="margin: 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; color: ${styles.text}; line-height: 1.5; white-space: pre-wrap;">
            ${notes}
          </p>
        </div>
      ` : ''}
    </div>

    <!-- Call to Action -->
    <div style="margin-bottom: 30px; text-align: center;">
      <a href="${dashboardUrl}/projects/${projectId}#tasks"
         style="display: inline-block; background-color: ${styles.primary}; color: white; text-decoration: none; padding: 15px 30px; border-radius: 6px; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; font-weight: 600; text-align: center; min-width: 200px;">
        Review Submission
      </a>
    </div>

    <!-- Footer -->
    <div style="border-top: 1px solid ${styles.border}; padding-top: 20px; text-align: center;">
      <p style="margin: 0 0 10px 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.small}; color: #666; line-height: 1.4;">
        This is an automated notification from the Kin Home Sales Dashboard.
      </p>
      <p style="margin: 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.small}; color: #999; line-height: 1.4;">
        © ${new Date().getFullYear()} Kin Home. All rights reserved.
      </p>
    </div>
  `;

  return getEmailLayout(content, `Task Submitted: ${taskName}`);
}

/**
 * Generate HTML template for task approved notification
 */
export function getTaskApprovedEmailTemplate(
  recipientName: string,
  taskName: string,
  taskCategory: string | undefined,
  projectId: number,
  projectName: string,
  customerName: string,
  dashboardUrl: string
): string {
  const styles = getEmailStyles();

  const content = `
    <!-- Header -->
    <table role="presentation" cellspacing="0" cellpadding="0" border="0" width="100%">
      <tr>
        <td align="center" style="padding-bottom: 30px;">
          <div style="background-color: ${styles.secondary}; color: white; padding: 20px; border-radius: 8px; text-align: center;">
            <div style="font-size: 32px; margin-bottom: 10px;">✅</div>
            <h1 style="margin: 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.xlarge}; font-weight: 600; color: white;">
              Task Approved!
            </h1>
          </div>
        </td>
      </tr>
    </table>

    <!-- Greeting -->
    <div style="margin-bottom: 30px;">
      <h2 style="margin: 0 0 10px 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.large}; font-weight: 600; color: ${styles.text};">
        Great news, ${recipientName}!
      </h2>
      <p style="margin: 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; color: ${styles.text}; line-height: 1.5;">
        Your task submission has been approved by operations.
      </p>
    </div>

    <!-- Task Details Card -->
    <div style="margin-bottom: 30px; padding: 20px; background-color: #f0fdf4; border: 1px solid #bbf7d0; border-radius: 8px;">
      <h3 style="margin: 0 0 15px 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; font-weight: 600; color: ${styles.text};">
        Approved Task
      </h3>
      <table role="presentation" cellspacing="0" cellpadding="0" border="0" width="100%">
        <tr>
          <td style="padding: 8px 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; color: ${styles.text}; font-weight: 600; width: 120px;">
            Task:
          </td>
          <td style="padding: 8px 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; color: ${styles.text};">
            ${taskName}
          </td>
        </tr>
        ${taskCategory ? `
        <tr>
          <td style="padding: 8px 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; color: ${styles.text}; font-weight: 600;">
            Category:
          </td>
          <td style="padding: 8px 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; color: ${styles.text};">
            ${taskCategory}
          </td>
        </tr>
        ` : ''}
        <tr>
          <td style="padding: 8px 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; color: ${styles.text}; font-weight: 600;">
            Project:
          </td>
          <td style="padding: 8px 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; color: ${styles.text};">
            ${projectName}
          </td>
        </tr>
        <tr>
          <td style="padding: 8px 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; color: ${styles.text}; font-weight: 600;">
            Customer:
          </td>
          <td style="padding: 8px 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; color: ${styles.text};">
            ${customerName}
          </td>
        </tr>
      </table>
    </div>

    <!-- Call to Action -->
    <div style="margin-bottom: 30px; text-align: center;">
      <a href="${dashboardUrl}/projects/${projectId}#tasks"
         style="display: inline-block; background-color: ${styles.secondary}; color: white; text-decoration: none; padding: 15px 30px; border-radius: 6px; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; font-weight: 600; text-align: center; min-width: 200px;">
        View Project
      </a>
    </div>

    <!-- Footer -->
    <div style="border-top: 1px solid ${styles.border}; padding-top: 20px; text-align: center;">
      <p style="margin: 0 0 10px 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.small}; color: #666; line-height: 1.4;">
        This is an automated notification from the Kin Home Sales Dashboard.
      </p>
      <p style="margin: 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.small}; color: #999; line-height: 1.4;">
        © ${new Date().getFullYear()} Kin Home. All rights reserved.
      </p>
    </div>
  `;

  return getEmailLayout(content, `Task Approved: ${taskName}`);
}

/**
 * Generate HTML template for task revision needed notification
 */
export function getTaskRevisionNeededEmailTemplate(
  recipientName: string,
  taskName: string,
  taskCategory: string | undefined,
  projectId: number,
  projectName: string,
  customerName: string,
  opsFeedback: string | undefined,
  dashboardUrl: string
): string {
  const styles = getEmailStyles();

  const content = `
    <!-- Header -->
    <table role="presentation" cellspacing="0" cellpadding="0" border="0" width="100%">
      <tr>
        <td align="center" style="padding-bottom: 30px;">
          <div style="background-color: #f97316; color: white; padding: 20px; border-radius: 8px; text-align: center;">
            <div style="font-size: 32px; margin-bottom: 10px;">⚠️</div>
            <h1 style="margin: 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.xlarge}; font-weight: 600; color: white;">
              Revision Needed
            </h1>
          </div>
        </td>
      </tr>
    </table>

    <!-- Greeting -->
    <div style="margin-bottom: 30px;">
      <h2 style="margin: 0 0 10px 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.large}; font-weight: 600; color: ${styles.text};">
        Hi ${recipientName},
      </h2>
      <p style="margin: 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; color: ${styles.text}; line-height: 1.5;">
        Operations has requested revisions on your task submission.
      </p>
    </div>

    <!-- Task Details Card -->
    <div style="margin-bottom: 30px; padding: 20px; background-color: #fff7ed; border: 1px solid #fed7aa; border-radius: 8px;">
      <h3 style="margin: 0 0 15px 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; font-weight: 600; color: ${styles.text};">
        Task Details
      </h3>
      <table role="presentation" cellspacing="0" cellpadding="0" border="0" width="100%">
        <tr>
          <td style="padding: 8px 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; color: ${styles.text}; font-weight: 600; width: 120px;">
            Task:
          </td>
          <td style="padding: 8px 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; color: ${styles.text};">
            ${taskName}
          </td>
        </tr>
        ${taskCategory ? `
        <tr>
          <td style="padding: 8px 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; color: ${styles.text}; font-weight: 600;">
            Category:
          </td>
          <td style="padding: 8px 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; color: ${styles.text};">
            ${taskCategory}
          </td>
        </tr>
        ` : ''}
        <tr>
          <td style="padding: 8px 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; color: ${styles.text}; font-weight: 600;">
            Project:
          </td>
          <td style="padding: 8px 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; color: ${styles.text};">
            ${projectName}
          </td>
        </tr>
        <tr>
          <td style="padding: 8px 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; color: ${styles.text}; font-weight: 600;">
            Customer:
          </td>
          <td style="padding: 8px 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; color: ${styles.text};">
            ${customerName}
          </td>
        </tr>
      </table>
    </div>

    ${opsFeedback ? `
    <!-- Feedback Card -->
    <div style="margin-bottom: 30px; padding: 20px; background-color: #fef2f2; border: 1px solid #fecaca; border-radius: 8px;">
      <h3 style="margin: 0 0 10px 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; font-weight: 600; color: ${styles.text};">
        Operations Feedback:
      </h3>
      <p style="margin: 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; color: ${styles.text}; line-height: 1.5; white-space: pre-wrap;">
        ${opsFeedback}
      </p>
    </div>
    ` : ''}

    <!-- Call to Action -->
    <div style="margin-bottom: 30px; text-align: center;">
      <a href="${dashboardUrl}/projects/${projectId}#tasks"
         style="display: inline-block; background-color: #f97316; color: white; text-decoration: none; padding: 15px 30px; border-radius: 6px; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; font-weight: 600; text-align: center; min-width: 200px;">
        Revise Task
      </a>
    </div>

    <!-- Footer -->
    <div style="border-top: 1px solid ${styles.border}; padding-top: 20px; text-align: center;">
      <p style="margin: 0 0 10px 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.small}; color: #666; line-height: 1.4;">
        This is an automated notification from the Kin Home Sales Dashboard.
      </p>
      <p style="margin: 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.small}; color: #999; line-height: 1.4;">
        © ${new Date().getFullYear()} Kin Home. All rights reserved.
      </p>
    </div>
  `;

  return getEmailLayout(content, `Revision Needed: ${taskName}`);
}

/**
 * Generate HTML template for all tasks complete notification
 */
export function getAllTasksCompleteEmailTemplate(
  recipientName: string,
  projectId: number,
  projectName: string,
  customerName: string,
  totalTasks: number,
  dashboardUrl: string
): string {
  const styles = getEmailStyles();

  const content = `
    <!-- Header -->
    <table role="presentation" cellspacing="0" cellpadding="0" border="0" width="100%">
      <tr>
        <td align="center" style="padding-bottom: 30px;">
          <div style="background-color: ${styles.secondary}; color: white; padding: 20px; border-radius: 8px; text-align: center;">
            <div style="font-size: 32px; margin-bottom: 10px;">🎉</div>
            <h1 style="margin: 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.xlarge}; font-weight: 600; color: white;">
              All Tasks Complete!
            </h1>
          </div>
        </td>
      </tr>
    </table>

    <!-- Greeting -->
    <div style="margin-bottom: 30px;">
      <h2 style="margin: 0 0 10px 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.large}; font-weight: 600; color: ${styles.text};">
        Congratulations, ${recipientName}!
      </h2>
      <p style="margin: 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; color: ${styles.text}; line-height: 1.5;">
        All ${totalTasks} tasks have been approved for your project. Your project is ready for reactivation.
      </p>
    </div>

    <!-- Project Details Card -->
    <div style="margin-bottom: 30px; padding: 20px; background-color: #f0fdf4; border: 1px solid #bbf7d0; border-radius: 8px;">
      <h3 style="margin: 0 0 15px 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; font-weight: 600; color: ${styles.text};">
        Project Information
      </h3>
      <table role="presentation" cellspacing="0" cellpadding="0" border="0" width="100%">
        <tr>
          <td style="padding: 8px 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; color: ${styles.text}; font-weight: 600; width: 140px;">
            Project:
          </td>
          <td style="padding: 8px 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; color: ${styles.text};">
            ${projectName}
          </td>
        </tr>
        <tr>
          <td style="padding: 8px 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; color: ${styles.text}; font-weight: 600;">
            Customer:
          </td>
          <td style="padding: 8px 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; color: ${styles.text};">
            ${customerName}
          </td>
        </tr>
        <tr>
          <td style="padding: 8px 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; color: ${styles.text}; font-weight: 600;">
            Tasks Completed:
          </td>
          <td style="padding: 8px 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; color: ${styles.secondary}; font-weight: 600;">
            ${totalTasks} / ${totalTasks}
          </td>
        </tr>
      </table>
    </div>

    <!-- Next Steps -->
    <div style="margin-bottom: 30px; padding: 20px; background-color: #e7f3ff; border: 1px solid #b3d9ff; border-radius: 8px;">
      <h3 style="margin: 0 0 15px 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; font-weight: 600; color: ${styles.text};">
        Next Steps
      </h3>
      <ul style="margin: 0; padding-left: 20px; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; color: ${styles.text}; line-height: 1.8;">
        <li>Your project will be reactivated by operations</li>
        <li>You'll be notified when the project moves to the next stage</li>
        <li>Continue monitoring project progress in the dashboard</li>
      </ul>
    </div>

    <!-- Call to Action -->
    <div style="margin-bottom: 30px; text-align: center;">
      <a href="${dashboardUrl}/projects/${projectId}"
         style="display: inline-block; background-color: ${styles.secondary}; color: white; text-decoration: none; padding: 15px 30px; border-radius: 6px; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; font-weight: 600; text-align: center; min-width: 200px;">
        View Project
      </a>
    </div>

    <!-- Footer -->
    <div style="border-top: 1px solid ${styles.border}; padding-top: 20px; text-align: center;">
      <p style="margin: 0 0 10px 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.small}; color: #666; line-height: 1.4;">
        This is an automated notification from the Kin Home Sales Dashboard.
      </p>
      <p style="margin: 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.small}; color: #999; line-height: 1.4;">
        © ${new Date().getFullYear()} Kin Home. All rights reserved.
      </p>
    </div>
  `;

  return getEmailLayout(content, `All Tasks Complete: ${projectName}`);
}

/**
 * Generate HTML template for daily digest email
 */
export function getDailyDigestEmailTemplate(
  recipientName: string,
  digestData: {
    unreadNotifications: number;
    criticalNotifications: number;
    tasksCompleted: number;
    tasksNeedingAttention: number;
    projectsUpdated: number;
    newProjects: number;
    topNotifications: Array<{
      title: string;
      message: string;
      projectName: string;
      priority: 'critical' | 'normal' | 'info';
    }>;
  },
  dashboardUrl: string
): string {
  const styles = getEmailStyles();
  const today = new Date().toLocaleDateString('en-US', {
    weekday: 'long',
    year: 'numeric',
    month: 'long',
    day: 'numeric'
  });

  const content = `
    <!-- Header -->
    <table role="presentation" cellspacing="0" cellpadding="0" border="0" width="100%">
      <tr>
        <td align="center" style="padding-bottom: 30px;">
          <div style="background-color: ${styles.primary}; color: white; padding: 20px; border-radius: 8px; text-align: center;">
            <div style="font-size: 32px; margin-bottom: 10px;">📊</div>
            <h1 style="margin: 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.xlarge}; font-weight: 600; color: white;">
              Daily Digest
            </h1>
            <p style="margin: 8px 0 0 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; color: rgba(255,255,255,0.9);">
              ${today}
            </p>
          </div>
        </td>
      </tr>
    </table>

    <!-- Greeting -->
    <div style="margin-bottom: 30px;">
      <h2 style="margin: 0 0 10px 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.large}; font-weight: 600; color: ${styles.text};">
        Good morning, ${recipientName}!
      </h2>
      <p style="margin: 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; color: ${styles.text}; line-height: 1.5;">
        Here's your daily summary from the Kin Home Sales Dashboard.
      </p>
    </div>

    <!-- Quick Stats Grid -->
    <table role="presentation" cellspacing="0" cellpadding="0" border="0" width="100%" style="margin-bottom: 30px;">
      <tr>
        <td style="padding: 0 10px 10px 0; width: 50%;">
          <div style="background-color: #fef2f2; border: 1px solid #fecaca; border-radius: 8px; padding: 15px; text-align: center;">
            <div style="font-family: ${styles.fontFamily}; font-size: 32px; font-weight: 700; color: #dc2626; margin-bottom: 5px;">
              ${digestData.criticalNotifications}
            </div>
            <div style="font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.small}; color: #991b1b; font-weight: 600;">
              Critical Alerts
            </div>
          </div>
        </td>
        <td style="padding: 0 0 10px 10px; width: 50%;">
          <div style="background-color: #eff6ff; border: 1px solid #bfdbfe; border-radius: 8px; padding: 15px; text-align: center;">
            <div style="font-family: ${styles.fontFamily}; font-size: 32px; font-weight: 700; color: #2563eb; margin-bottom: 5px;">
              ${digestData.unreadNotifications}
            </div>
            <div style="font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.small}; color: #1e40af; font-weight: 600;">
              Unread Messages
            </div>
          </div>
        </td>
      </tr>
      <tr>
        <td style="padding: 10px 10px 0 0; width: 50%;">
          <div style="background-color: #f0fdf4; border: 1px solid #bbf7d0; border-radius: 8px; padding: 15px; text-align: center;">
            <div style="font-family: ${styles.fontFamily}; font-size: 32px; font-weight: 700; color: #16a34a; margin-bottom: 5px;">
              ${digestData.tasksCompleted}
            </div>
            <div style="font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.small}; color: #15803d; font-weight: 600;">
              Tasks Completed
            </div>
          </div>
        </td>
        <td style="padding: 10px 0 0 10px; width: 50%;">
          <div style="background-color: #fff7ed; border: 1px solid #fed7aa; border-radius: 8px; padding: 15px; text-align: center;">
            <div style="font-family: ${styles.fontFamily}; font-size: 32px; font-weight: 700; color: #ea580c; margin-bottom: 5px;">
              ${digestData.tasksNeedingAttention}
            </div>
            <div style="font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.small}; color: #c2410c; font-weight: 600;">
              Need Attention
            </div>
          </div>
        </td>
      </tr>
    </table>

    <!-- Activity Summary -->
    ${digestData.newProjects > 0 || digestData.projectsUpdated > 0 ? `
    <div style="margin-bottom: 30px; padding: 20px; background-color: #f8f9fa; border: 1px solid ${styles.border}; border-radius: 8px;">
      <h3 style="margin: 0 0 15px 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; font-weight: 600; color: ${styles.text};">
        Activity Summary
      </h3>
      <ul style="margin: 0; padding-left: 20px; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; color: ${styles.text}; line-height: 1.8;">
        ${digestData.newProjects > 0 ? `<li><strong>${digestData.newProjects}</strong> new project${digestData.newProjects === 1 ? '' : 's'} added</li>` : ''}
        ${digestData.projectsUpdated > 0 ? `<li><strong>${digestData.projectsUpdated}</strong> project${digestData.projectsUpdated === 1 ? '' : 's'} updated</li>` : ''}
        ${digestData.tasksCompleted > 0 ? `<li><strong>${digestData.tasksCompleted}</strong> task${digestData.tasksCompleted === 1 ? '' : 's'} completed</li>` : ''}
      </ul>
    </div>
    ` : ''}

    <!-- Top Notifications -->
    ${digestData.topNotifications.length > 0 ? `
    <div style="margin-bottom: 30px;">
      <h3 style="margin: 0 0 15px 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; font-weight: 600; color: ${styles.text};">
        Recent Notifications
      </h3>
      ${digestData.topNotifications.map(notif => {
        const bgColor = notif.priority === 'critical' ? '#fef2f2' : notif.priority === 'normal' ? '#eff6ff' : '#f8f9fa';
        const borderColor = notif.priority === 'critical' ? '#fecaca' : notif.priority === 'normal' ? '#bfdbfe' : '#e5e7eb';

        return `
          <div style="margin-bottom: 12px; padding: 15px; background-color: ${bgColor}; border: 1px solid ${borderColor}; border-radius: 6px;">
            <div style="font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; font-weight: 600; color: ${styles.text}; margin-bottom: 5px;">
              ${notif.title}
            </div>
            <div style="font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.small}; color: #666; margin-bottom: 5px;">
              ${notif.message}
            </div>
            <div style="font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.small}; color: #999;">
              Project: ${notif.projectName}
            </div>
          </div>
        `;
      }).join('')}
    </div>
    ` : ''}

    <!-- Call to Action -->
    <div style="margin-bottom: 30px; text-align: center;">
      <a href="${dashboardUrl}"
         style="display: inline-block; background-color: ${styles.primary}; color: white; text-decoration: none; padding: 15px 30px; border-radius: 6px; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.medium}; font-weight: 600; text-align: center; min-width: 200px;">
        View Dashboard
      </a>
    </div>

    <!-- Footer -->
    <div style="border-top: 1px solid ${styles.border}; padding-top: 20px; text-align: center;">
      <p style="margin: 0 0 10px 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.small}; color: #666; line-height: 1.4;">
        This daily digest is sent every morning at 8:00 AM. You can manage your email preferences in Settings.
      </p>
      <p style="margin: 0; font-family: ${styles.fontFamily}; font-size: ${styles.fontSize.small}; color: #999; line-height: 1.4;">
        © ${new Date().getFullYear()} Kin Home. All rights reserved.
      </p>
    </div>
  `;

  return getEmailLayout(content, `Daily Digest - ${today}`);
}
