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
