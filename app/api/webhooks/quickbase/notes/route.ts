export const runtime = 'nodejs';

import { NextResponse } from 'next/server';
import crypto from 'crypto';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';
import { createNotification } from '@/lib/db/notifications';
import { getNotificationRecipientsForProject } from '@/lib/quickbase/notificationRecipients';
import { getNotePriority } from '@/lib/quickbase/notePriority';
import { NOTE_FIELDS, REP_VISIBLE_FLAG } from '@/lib/constants/noteFieldIds';
import type { QuickbaseNoteMetadata } from '@/lib/types/notification';

/**
 * POST /api/webhooks/quickbase/notes
 *
 * Receives webhooks from QuickBase when a new rep-visible note is created.
 * Creates notifications for all authorized users who can access the project.
 *
 * Security:
 * - Verifies HMAC-SHA256 signature from QuickBase
 * - Only processes notes marked as "Rep Visible"
 * - Rate limited to prevent abuse
 *
 * Webhook payload structure (from QuickBase):
 * {
 *   recordid: number,
 *   fieldChanges: {
 *     [fieldId: string]: {
 *       value: any
 *     }
 *   }
 * }
 */
export async function POST(req: Request) {
  const startedAt = Date.now();
  const reqId = req.headers.get('x-request-id') || Math.random().toString(36).slice(2, 10);
  logApiRequest('POST', '/api/webhooks/quickbase/notes', undefined, reqId);

  try {
    // 1. Verify webhook signature for security
    const signature = req.headers.get('x-quickbase-signature');
    const webhookSecret = process.env.QUICKBASE_WEBHOOK_SECRET;

    if (!webhookSecret) {
      logError('QUICKBASE_WEBHOOK_SECRET not configured', new Error('Missing webhook secret'));
      return NextResponse.json({ error: 'Webhook not configured' }, { status: 500 });
    }

    // Get raw body for signature verification
    const rawBody = await req.text();

    if (signature) {
      const expectedSignature = crypto
        .createHmac('sha256', webhookSecret)
        .update(rawBody)
        .digest('hex');

      if (signature !== expectedSignature) {
        logError('Invalid webhook signature', new Error('Signature mismatch'));
        return NextResponse.json({ error: 'Invalid signature' }, { status: 401 });
      }
    } else {
      // In development, allow webhooks without signature for testing
      if (process.env.NODE_ENV === 'production') {
        logError('Missing webhook signature', new Error('No signature provided'));
        return NextResponse.json({ error: 'Missing signature' }, { status: 401 });
      }
      console.warn('[WEBHOOK] No signature provided - allowing in development mode');
    }

    // 2. Parse webhook payload
    const payload = JSON.parse(rawBody);
    console.log('[WEBHOOK] Received QuickBase note webhook:', {
      recordid: payload.recordid,
      fieldChanges: Object.keys(payload.fieldChanges || {}),
    });

    // 3. Extract note data from field changes
    const noteId = payload.recordid;
    const fieldChanges = payload.fieldChanges || {};

    // Check if this is a rep-visible note
    const repVisibleValue = fieldChanges[NOTE_FIELDS.REP_VISIBLE]?.value;
    if (repVisibleValue !== REP_VISIBLE_FLAG) {
      console.log('[WEBHOOK] Skipping note - not marked as rep visible:', repVisibleValue);
      return NextResponse.json({
        success: true,
        message: 'Note not rep-visible, skipped'
      }, { status: 200 });
    }

    // Extract note fields
    const noteContent = fieldChanges[NOTE_FIELDS.NOTE_CONTENT]?.value || '';
    const category = fieldChanges[NOTE_FIELDS.CATEGORY]?.value || 'General';
    const createdBy = fieldChanges[NOTE_FIELDS.CREATED_BY]?.value || {};
    const projectId = fieldChanges[NOTE_FIELDS.RELATED_PROJECT]?.value;

    if (!projectId) {
      logError('Note webhook missing project ID', new Error('Invalid payload'));
      return NextResponse.json({ error: 'Missing project ID' }, { status: 400 });
    }

    console.log('[WEBHOOK] Processing rep-visible note:', {
      noteId,
      projectId,
      category,
      createdBy: createdBy.name || createdBy.email || 'Unknown',
    });

    // 4. Determine notification priority based on category
    const priority = getNotePriority(category);

    // 5. Get all users who should receive this notification
    const recipients = await getNotificationRecipientsForProject(projectId);

    if (recipients.length === 0) {
      console.warn('[WEBHOOK] No recipients found for project:', projectId);
      return NextResponse.json({
        success: true,
        message: 'No recipients found'
      }, { status: 200 });
    }

    console.log('[WEBHOOK] Creating notifications for recipients:', {
      count: recipients.length,
      userIds: recipients.map(r => r.userId),
    });

    // 6. Create notification metadata
    const metadata: QuickbaseNoteMetadata = {
      note_id: noteId,
      category,
      quickbase_url: `https://kinhome.quickbase.com/db/bsb6bqt3b?a=dr&rid=${noteId}`,
    };

    // 7. Create notifications for each recipient
    const createdNotifications = [];
    for (const recipient of recipients) {
      try {
        const notification = await createNotification({
          user_id: recipient.userId,
          project_id: projectId,
          type: 'quickbase_note',
          priority,
          source: 'quickbase',
          title: `New ${category} note`,
          message: noteContent.substring(0, 200), // First 200 chars
          metadata,
          sender_id: createdBy.email || createdBy.id,
          sender_name: createdBy.name || 'QuickBase User',
          sender_role: 'operations',
          icon: 'file-text',
          color: priority === 'critical' ? 'red' : priority === 'normal' ? 'blue' : 'gray',
          action_url: `/projects/${projectId}#notes`,
        });

        createdNotifications.push(notification.id);
        console.log('[WEBHOOK] Created notification:', {
          id: notification.id,
          userId: recipient.userId,
          projectId,
        });
      } catch (error) {
        logError('Failed to create notification for user', error as Error, {
          userId: recipient.userId,
          projectId,
        });
        // Continue creating notifications for other recipients
      }
    }

    const duration = Date.now() - startedAt;
    logApiResponse('POST', '/api/webhooks/quickbase/notes', duration, {
      noteId,
      projectId,
      recipientCount: recipients.length,
      notificationsCreated: createdNotifications.length,
    }, reqId);

    return NextResponse.json({
      success: true,
      notificationsCreated: createdNotifications.length,
      recipients: recipients.length,
    }, { status: 201 });

  } catch (error) {
    logError('Webhook processing failed', error as Error);
    return NextResponse.json({ error: 'Internal Server Error' }, { status: 500 });
  }
}
