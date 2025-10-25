import twilio from 'twilio';
import { logError, logInfo, logWarn } from '@/lib/logging/logger';

export interface TwilioConfig {
  accountSid: string;
  authToken: string;
  phoneNumber: string;
  webhookUrl: string;
  messagingServiceSid?: string;
  recordCalls: boolean;
}

/**
 * Get Twilio configuration from environment variables
 */
function getTwilioConfig(): TwilioConfig | null {
  try {
    const accountSid = process.env.TWILIO_ACCOUNT_SID;
    const authToken = process.env.TWILIO_AUTH_TOKEN;
    const phoneNumber = process.env.TWILIO_PHONE_NUMBER;
    const webhookUrl = process.env.TWILIO_WEBHOOK_URL;
    const messagingServiceSid = process.env.TWILIO_MESSAGING_SERVICE_SID;
    const recordCalls = process.env.TWILIO_RECORD_CALLS === 'true';

    if (!accountSid || !authToken || !phoneNumber || !webhookUrl) {
      logWarn('Twilio configuration incomplete. Missing required environment variables.', {
        service: 'twilio',
        hasAccountSid: !!accountSid,
        hasAuthToken: !!authToken,
        hasPhoneNumber: !!phoneNumber,
        hasWebhookUrl: !!webhookUrl,
      });
      return null;
    }

    // Validate webhook URL format
    if (process.env.NODE_ENV === 'production' && !webhookUrl.startsWith('https://')) {
      logWarn('Twilio webhook URL should use HTTPS in production', {
        service: 'twilio',
        webhookUrl,
      });
    }

    return {
      accountSid,
      authToken,
      phoneNumber,
      webhookUrl,
      messagingServiceSid,
      recordCalls,
    };
  } catch (error) {
    logError('Failed to load Twilio configuration', error, {
      service: 'twilio',
    });
    return null;
  }
}

/**
 * Initialize Twilio client
 */
function initializeTwilioClient(): { client: any; config: TwilioConfig | null } {
  try {
    const config = getTwilioConfig();
    
    if (!config) {
      return { client: null, config: null };
    }

    const client = twilio(config.accountSid, config.authToken);
    
    logInfo('Twilio client initialized successfully', {
      service: 'twilio',
      accountSid: config.accountSid,
      phoneNumber: config.phoneNumber,
      webhookUrl: config.webhookUrl,
    });

    return { client, config };
  } catch (error) {
    logError('Failed to initialize Twilio client', error, {
      service: 'twilio',
    });
    return { client: null, config: null };
  }
}

// Initialize Twilio client and config as module-level singletons
const { client: twilioClient, config: twilioConfig } = initializeTwilioClient();

/**
 * Check if Twilio is properly configured
 */
export function isTwilioConfigured(): boolean {
  return twilioClient !== null && twilioConfig !== null;
}

// Export the configured client and config
export { twilioClient, twilioConfig };
export type { TwilioConfig };
