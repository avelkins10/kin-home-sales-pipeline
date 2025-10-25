import { twilioClient, twilioConfig, isTwilioConfigured } from './client';
import { logTwilioRequest, logTwilioResponse, logTwilioError, logInfo, logError } from '@/lib/logging/logger';
import { qbClient } from '@/lib/quickbase/client';
import { OUTREACH_RECORD_FIELDS, QB_TABLE_OUTREACH_RECORDS, INSTALL_COMMUNICATION_FIELDS, QB_TABLE_INSTALL_COMMUNICATIONS } from '@/lib/constants/fieldIds';
import { TwilioSmsParams, TwilioSmsResult } from '@/lib/types/operations';

/**
 * Send SMS message via Twilio with QuickBase logging
 */
export async function sendSms(
  params: TwilioSmsParams,
  options: {
    projectId?: string;
    recordId?: number;
    coordinatorEmail?: string;
    logToQuickbase?: boolean;
  } = {}
): Promise<TwilioSmsResult> {
  const startTime = Date.now();

  try {
    // Validate Twilio configuration
    if (!isTwilioConfigured()) {
      return {
        success: false,
        messageSid: null,
        status: 'failed',
        to: params.to,
        body: params.body,
        error: 'Twilio not configured',
        errorCode: 500,
      };
    }

    // Validate phone number format
    const formattedPhone = formatPhoneNumber(params.to);
    if (!formattedPhone) {
      return {
        success: false,
        messageSid: null,
        status: 'failed',
        to: params.to,
        body: params.body,
        error: 'Invalid phone number format',
        errorCode: 400,
      };
    }

    // Validate message body length
    if (params.body.length > 1600) {
      return {
        success: false,
        messageSid: null,
        status: 'failed',
        to: params.to,
        body: params.body,
        error: 'Message too long (max 1600 characters)',
        errorCode: 400,
      };
    }

    // Prepare SMS parameters
    const from = params.from || twilioConfig!.phoneNumber;
    const statusCallback = params.statusCallback || `${twilioConfig!.webhookUrl}/api/operations/twilio/sms`;
    
    // Add metadata to status callback URL
    const callbackUrl = new URL(statusCallback);
    if (options.projectId) callbackUrl.searchParams.set('projectId', options.projectId);
    if (options.recordId) callbackUrl.searchParams.set('recordId', options.recordId.toString());
    if (options.coordinatorEmail) callbackUrl.searchParams.set('coordinatorEmail', options.coordinatorEmail);

    // Log request
    logTwilioRequest('send_sms', {
      to: formattedPhone,
      from,
      body: params.body,
    });

    // Send SMS via Twilio
    const message = await twilioClient!.messages.create({
      body: params.body,
      from,
      to: formattedPhone,
      statusCallback: callbackUrl.toString(),
    });

    const duration = Date.now() - startTime;

    // Log response
    logTwilioResponse('send_sms', duration, message.sid, message.status);

    const result: TwilioSmsResult = {
      success: true,
      messageSid: message.sid,
      status: message.status,
      to: formattedPhone,
      body: params.body,
    };

    // Log to QuickBase if enabled
    if (options.logToQuickbase && options.projectId && options.coordinatorEmail) {
      try {
        await logSmsToQuickbase(
          options.projectId,
          options.recordId,
          result,
          options.coordinatorEmail
        );
      } catch (error) {
        logError('Failed to log SMS to QuickBase', error, {
          service: 'twilio_sms',
          messageSid: message.sid,
          projectId: options.projectId,
        });
        // Don't fail the SMS send if logging fails
      }
    }

    return result;

  } catch (error: any) {
    const duration = Date.now() - startTime;
    
    // Log error
    logTwilioError('send_sms', error, {
      to: params.to,
      from: params.from,
      twilioErrorCode: error.code,
    });

    return {
      success: false,
      messageSid: null,
      status: 'failed',
      to: params.to,
      body: params.body,
      error: error.message || 'Unknown error',
      errorCode: error.code || 500,
    };
  }
}

/**
 * Log SMS to QuickBase
 */
async function logSmsToQuickbase(
  projectId: string,
  recordId: number | undefined,
  smsResult: TwilioSmsResult,
  coordinatorEmail: string
): Promise<void> {
  const timestamp = new Date().toISOString();

  if (recordId) {
    // First, query the current record to get existing attempt count
    const currentRecord = await qbClient.queryRecords({
      from: QB_TABLE_OUTREACH_RECORDS,
      select: [OUTREACH_RECORD_FIELDS.NUM_ATTEMPTS, OUTREACH_RECORD_FIELDS.ATTEMPT_NOTE],
      where: `{3.EX.'${recordId}'}`
    });

    const currentAttempts = currentRecord.data?.[0]?.data?.[OUTREACH_RECORD_FIELDS.NUM_ATTEMPTS]?.value || 0;
    const currentNote = currentRecord.data?.[0]?.data?.[OUTREACH_RECORD_FIELDS.ATTEMPT_NOTE]?.value || '';

    // Update existing Outreach Record
    const updateData: Record<string, any> = {
      [OUTREACH_RECORD_FIELDS.OUTREACH_COMPLETED_DATE]: timestamp,
    };

    if (smsResult.success) {
      updateData[OUTREACH_RECORD_FIELDS.OUTREACH_STATUS] = 'Complete';
    }

    // Append to attempt note with timestamp
    const newAttemptNote = `SMS sent via Twilio (SID: ${smsResult.messageSid}) - ${new Date().toISOString()}`;
    const combinedNote = currentNote ? `${currentNote}\n${newAttemptNote}` : newAttemptNote;
    updateData[OUTREACH_RECORD_FIELDS.ATTEMPT_NOTE] = combinedNote;

    // Increment attempt count
    updateData[OUTREACH_RECORD_FIELDS.NUM_ATTEMPTS] = currentAttempts + 1;

    await qbClient.updateRecord({
      to: QB_TABLE_OUTREACH_RECORDS,
      data: [{
        3: { value: recordId },
        ...Object.fromEntries(
          Object.entries(updateData).map(([key, value]) => [key, { value }])
        )
      }]
    });

  } else {
    // Create new Install Communication record
    const communicationData = {
      [INSTALL_COMMUNICATION_FIELDS.DATE]: timestamp,
      [INSTALL_COMMUNICATION_FIELDS.NOTE_BY]: coordinatorEmail,
      [INSTALL_COMMUNICATION_FIELDS.RELATED_PROJECT]: projectId,
      [INSTALL_COMMUNICATION_FIELDS.COMMUNICATION_NOTE]: `SMS sent: ${smsResult.body} (Twilio SID: ${smsResult.messageSid})`,
    };

    await qbClient.updateRecord({
      to: QB_TABLE_INSTALL_COMMUNICATIONS,
      data: [{
        ...Object.fromEntries(
          Object.entries(communicationData).map(([key, value]) => [key, { value }])
        )
      }]
    });
  }
}

/**
 * Format phone number to E.164 format
 */
function formatPhoneNumber(phone: string): string | null {
  // Remove all non-numeric characters
  const cleaned = phone.replace(/\D/g, '');
  
  // If it's 10 digits, assume US number and add +1
  if (cleaned.length === 10) {
    return `+1${cleaned}`;
  }
  
  // If it's 11 digits and starts with 1, add +
  if (cleaned.length === 11 && cleaned.startsWith('1')) {
    return `+${cleaned}`;
  }
  
  // If it already has country code, just add +
  if (cleaned.length > 11) {
    return `+${cleaned}`;
  }
  
  // Invalid format
  return null;
}
