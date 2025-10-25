export enum SmsTemplateType {
  INITIAL_OUTREACH = 'initial_outreach',
  FOLLOW_UP = 'follow_up',
  INSTALL_SCHEDULING = 'install_scheduling',
  NEM_SUBMITTED = 'nem_submitted',
  PTO_APPROVED = 'pto_approved',
  SURVEY_REMINDER = 'survey_reminder',
  WELCOME_CALL = 'welcome_call',
}

export interface SmsTemplateVariables {
  customerName: string;
  projectId: string;
  coordinatorName: string;
  coordinatorPhone: string;
  appointmentDate?: string;
  appointmentTime?: string;
  customMessage?: string;
}

export interface SmsTemplate {
  type: SmsTemplateType;
  name: string;
  description: string;
  template: string;
  maxLength: number;
  requiredVariables: string[];
}

/**
 * SMS Templates for PC outreach scenarios
 */
export const SMS_TEMPLATES: Record<SmsTemplateType, SmsTemplate> = {
  [SmsTemplateType.INITIAL_OUTREACH]: {
    type: SmsTemplateType.INITIAL_OUTREACH,
    name: 'Initial Outreach',
    description: 'First contact with customer after project creation',
    template: 'Hi {{customerName}}, this is {{coordinatorName}} from Kin Home. I\'m your Project Coordinator for solar project {{projectId}}. I\'ll be your main contact throughout the installation process. Please call or text me at {{coordinatorPhone}} if you have any questions. Looking forward to working with you!',
    maxLength: 320,
    requiredVariables: ['customerName', 'coordinatorName', 'coordinatorPhone', 'projectId'],
  },
  [SmsTemplateType.FOLLOW_UP]: {
    type: SmsTemplateType.FOLLOW_UP,
    name: 'Follow Up',
    description: 'Follow-up message with custom content',
    template: 'Hi {{customerName}}, this is {{coordinatorName}} from Kin Home following up on your solar project {{projectId}}. {{customMessage}} Please call or text me at {{coordinatorPhone}}. Thanks!',
    maxLength: 320,
    requiredVariables: ['customerName', 'coordinatorName', 'coordinatorPhone', 'projectId', 'customMessage'],
  },
  [SmsTemplateType.INSTALL_SCHEDULING]: {
    type: SmsTemplateType.INSTALL_SCHEDULING,
    name: 'Install Scheduling',
    description: 'Notification about scheduled installation',
    template: 'Hi {{customerName}}, your solar installation is scheduled for {{appointmentDate}} at {{appointmentTime}}. Our crew will arrive on time and complete the installation. Call {{coordinatorPhone}} if you have questions. Thanks! - {{coordinatorName}}, Kin Home',
    maxLength: 320,
    requiredVariables: ['customerName', 'appointmentDate', 'appointmentTime', 'coordinatorPhone', 'coordinatorName'],
  },
  [SmsTemplateType.NEM_SUBMITTED]: {
    type: SmsTemplateType.NEM_SUBMITTED,
    name: 'NEM Submitted',
    description: 'Notification that NEM application has been submitted',
    template: 'Good news {{customerName}}! Your NEM application for project {{projectId}} has been submitted to the utility. We\'ll notify you when it\'s approved. This typically takes 2-4 weeks. Questions? Call {{coordinatorPhone}}. - {{coordinatorName}}, Kin Home',
    maxLength: 320,
    requiredVariables: ['customerName', 'projectId', 'coordinatorPhone', 'coordinatorName'],
  },
  [SmsTemplateType.PTO_APPROVED]: {
    type: SmsTemplateType.PTO_APPROVED,
    name: 'PTO Approved',
    description: 'Congratulations message for PTO approval',
    template: 'Congratulations {{customerName}}! Your solar system for project {{projectId}} has received PTO approval. Your system is now live and generating clean energy! Questions? Call {{coordinatorPhone}}. - {{coordinatorName}}, Kin Home',
    maxLength: 320,
    requiredVariables: ['customerName', 'projectId', 'coordinatorPhone', 'coordinatorName'],
  },
  [SmsTemplateType.SURVEY_REMINDER]: {
    type: SmsTemplateType.SURVEY_REMINDER,
    name: 'Survey Reminder',
    description: 'Reminder to schedule site survey',
    template: 'Hi {{customerName}}, friendly reminder that we need to schedule your site survey for solar project {{projectId}}. Please call or text {{coordinatorPhone}} to find a convenient time. Thanks! - {{coordinatorName}}, Kin Home',
    maxLength: 320,
    requiredVariables: ['customerName', 'projectId', 'coordinatorPhone', 'coordinatorName'],
  },
  [SmsTemplateType.WELCOME_CALL]: {
    type: SmsTemplateType.WELCOME_CALL,
    name: 'Welcome Call',
    description: 'Request to schedule welcome call',
    template: 'Hi {{customerName}}, this is {{coordinatorName}} from Kin Home. I\'d like to schedule a welcome call to discuss your solar project {{projectId}} and answer any questions. When\'s a good time to chat? Text or call {{coordinatorPhone}}. Thanks!',
    maxLength: 320,
    requiredVariables: ['customerName', 'coordinatorName', 'projectId', 'coordinatorPhone'],
  },
};

/**
 * Get formatted SMS template
 */
export function getSmsTemplate(
  templateType: SmsTemplateType,
  variables: SmsTemplateVariables
): string {
  const template = SMS_TEMPLATES[templateType];
  
  if (!template) {
    throw new Error(`Unknown template type: ${templateType}`);
  }

  // Validate required variables
  for (const requiredVar of template.requiredVariables) {
    if (!variables[requiredVar as keyof SmsTemplateVariables]) {
      throw new Error(`Missing required variable: ${requiredVar}`);
    }
  }

  // Replace variables in template
  let formattedMessage = template.template;
  for (const [key, value] of Object.entries(variables)) {
    const placeholder = `{{${key}}}`;
    formattedMessage = formattedMessage.replace(new RegExp(placeholder, 'g'), value || '');
  }

  // Validate final message length
  if (formattedMessage.length > template.maxLength) {
    throw new Error(`Message too long: ${formattedMessage.length} characters (max: ${template.maxLength})`);
  }

  return formattedMessage;
}

/**
 * Validate SMS message length and return segment count
 */
export function validateSmsLength(message: string): { isValid: boolean; segments: number; warning?: string } {
  const length = message.length;
  
  if (length <= 160) {
    return { isValid: true, segments: 1 };
  } else if (length <= 320) {
    return { isValid: true, segments: 2, warning: 'Message will be split into 2 SMS segments' };
  } else if (length <= 480) {
    return { isValid: true, segments: 3, warning: 'Message will be split into 3 SMS segments' };
  } else {
    return { 
      isValid: false, 
      segments: Math.ceil(length / 160), 
      warning: 'Message too long for SMS (max 480 characters)' 
    };
  }
}

/**
 * List all available templates
 */
export function listAvailableTemplates(): Array<{ type: SmsTemplateType; name: string; description: string }> {
  return Object.values(SMS_TEMPLATES).map(template => ({
    type: template.type,
    name: template.name,
    description: template.description,
  }));
}
