import { twilioClient, twilioConfig, isTwilioConfigured } from './client';
import { logTwilioRequest, logTwilioResponse, logTwilioError, logInfo, logError } from '@/lib/logging/logger';
import { qbClient } from '@/lib/quickbase/client';
import { OUTREACH_RECORD_FIELDS, QB_TABLE_OUTREACH_RECORDS, INSTALL_COMMUNICATION_FIELDS, QB_TABLE_INSTALL_COMMUNICATIONS } from '@/lib/constants/fieldIds';
import { TwilioCallParams, TwilioCallResult } from '@/lib/types/operations';

/**
 * Initiate voice call via Twilio with recording and status callbacks
 */
export async function initiateCall(
  params: TwilioCallParams,
  options: {
    projectId?: string;
    recordId?: number;
    coordinatorEmail?: string;
    coordinatorPhone?: string;
    logToQuickbase?: boolean;
  } = {}
): Promise<TwilioCallResult> {
  const startTime = Date.now();

  try {
    // Validate Twilio configuration
    if (!isTwilioConfigured()) {
      return {
        success: false,
        callSid: null,
        status: 'failed',
        to: params.to,
        error: 'Twilio not configured',
        errorCode: 500,
      };
    }

    // Validate phone number format
    const formattedPhone = formatPhoneNumber(params.to);
    if (!formattedPhone) {
      return {
        success: false,
        callSid: null,
        status: 'failed',
        to: params.to,
        error: 'Invalid phone number format',
        errorCode: 400,
      };
    }

    // Prepare call parameters
    const from = params.from || twilioConfig!.phoneNumber;
    const url = params.url || generateDefaultTwiML(options.projectId, options.coordinatorPhone);
    const statusCallback = params.statusCallback || `${twilioConfig!.webhookUrl}/api/operations/twilio/voice/status`;
    const statusCallbackEvent = params.statusCallbackEvent || ['initiated', 'ringing', 'answered', 'completed'];
    const record = params.record ?? twilioConfig!.recordCalls;
    
    // Add metadata to status callback URL
    const callbackUrl = new URL(statusCallback);
    if (options.projectId) callbackUrl.searchParams.set('projectId', options.projectId);
    if (options.recordId) callbackUrl.searchParams.set('recordId', options.recordId.toString());
    if (options.coordinatorEmail) callbackUrl.searchParams.set('coordinatorEmail', options.coordinatorEmail);

    const callParams: any = {
      to: formattedPhone,
      from,
      url,
      statusCallback: callbackUrl.toString(),
      statusCallbackEvent,
      record,
    };

    // Add recording status callback if recording is enabled
    if (record) {
      callParams.recordingStatusCallback = `${twilioConfig!.webhookUrl}/api/operations/twilio/voice/recording`;
    }

    // Log request
    logTwilioRequest('initiate_call', {
      to: formattedPhone,
      from,
      url,
    });

    // Initiate call via Twilio
    const call = await twilioClient!.calls.create(callParams);

    const duration = Date.now() - startTime;

    // Log response
    logTwilioResponse('initiate_call', duration, call.sid, call.status);

    const result: TwilioCallResult = {
      success: true,
      callSid: call.sid,
      status: call.status,
      to: formattedPhone,
    };

    // Log to QuickBase if enabled
    if (options.logToQuickbase && options.projectId && options.coordinatorEmail) {
      try {
        await logCallToQuickbase(
          options.projectId,
          options.recordId,
          result,
          options.coordinatorEmail
        );
      } catch (error) {
        logError('Failed to log call to QuickBase', error, {
          service: 'twilio_calls',
          callSid: call.sid,
          projectId: options.projectId,
        });
        // Don't fail the call initiation if logging fails
      }
    }

    return result;

  } catch (error: any) {
    const duration = Date.now() - startTime;
    
    // Log error
    logTwilioError('initiate_call', error, {
      to: params.to,
      from: params.from,
      twilioErrorCode: error.code,
    });

    return {
      success: false,
      callSid: null,
      status: 'failed',
      to: params.to,
      error: error.message || 'Unknown error',
      errorCode: error.code || 500,
    };
  }
}

/**
 * Log call to QuickBase
 */
async function logCallToQuickbase(
  projectId: string,
  recordId: number | undefined,
  callResult: TwilioCallResult,
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

    // Set status based on call status
    if (callResult.status === 'initiated' || callResult.status === 'ringing') {
      updateData[OUTREACH_RECORD_FIELDS.OUTREACH_STATUS] = 'No Answer Left Message';
    } else if (callResult.status === 'completed') {
      updateData[OUTREACH_RECORD_FIELDS.OUTREACH_STATUS] = 'Complete';
    }

    // Append to attempt note with timestamp
    const newAttemptNote = `Call initiated via Twilio (SID: ${callResult.callSid}) - ${new Date().toISOString()}`;
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
      [INSTALL_COMMUNICATION_FIELDS.COMMUNICATION_NOTE]: `Call initiated to ${callResult.to} (Twilio SID: ${callResult.callSid})`,
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
 * Generate default TwiML URL
 */
function generateDefaultTwiML(projectId?: string, coordinatorPhone?: string): string {
  const baseUrl = twilioConfig!.webhookUrl;
  const twimlUrl = `${baseUrl}/api/operations/twilio/voice`;
  
  const url = new URL(twimlUrl);
  if (projectId) {
    url.searchParams.set('projectId', projectId);
  }
  if (coordinatorPhone) {
    url.searchParams.set('coordinatorPhone', coordinatorPhone);
  }
  
  return url.toString();
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
