// Client and configuration
export { twilioClient, twilioConfig, isTwilioConfigured } from './client';
export type { TwilioConfig } from './client';

// SMS functionality
export { sendSms } from './sms';

// Voice/Call functionality
export { initiateCall } from './calls';

// SMS templates
export { SmsTemplateType } from './templates';
export type { SmsTemplateVariables, SmsTemplate } from './templates';
export { getSmsTemplate, listAvailableTemplates, validateSmsLength, SMS_TEMPLATES } from './templates';
