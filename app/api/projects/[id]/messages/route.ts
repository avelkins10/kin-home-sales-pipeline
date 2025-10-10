export const runtime = 'nodejs';

import { NextResponse } from 'next/server';
import { requireAuth, requireProjectAccessById } from '@/lib/auth/guards';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';
import { getMessagesForProject, createMessage, getMessageCount } from '@/lib/db/messages';
import { createNotification } from '@/lib/db/notifications';
import { getNotificationRecipientsForProject } from '@/lib/quickbase/notificationRecipients';
import { parseMentions } from '@/lib/utils/mentions';
import { sql } from '@/lib/db/client';
import type { CreateMessageInput } from '@/lib/types/message';
import type { InternalMessageMetadata } from '@/lib/types/notification';

/**
 * GET /api/projects/[id]/messages
 * Fetch messages for a specific project
 *
 * Query params:
 *   - limit: number (default: 50)
 *   - offset: number (default: 0)
 */
export async function GET(
  req: Request,
  { params }: { params: { id: string } }
) {
  const startedAt = Date.now();
  const projectId = parseInt(params.id, 10);
  const reqId = req.headers.get('x-request-id') || Math.random().toString(36).slice(2, 10);
  logApiRequest('GET', `/api/projects/${params.id}/messages`, undefined, reqId);

  // Auth check
  const auth = await requireAuth();
  if (!auth.authorized) {
    return auth.response;
  }

  // Project access check
  const access = await requireProjectAccessById(projectId);
  if (!access.authorized) {
    return access.response;
  }

  try {
    const { searchParams } = new URL(req.url);
    const limit = parseInt(searchParams.get('limit') || '50');
    const offset = parseInt(searchParams.get('offset') || '0');

    // Validate parameters
    if (limit < 1 || limit > 100) {
      return NextResponse.json({ error: 'Limit must be between 1 and 100' }, { status: 400 });
    }

    if (offset < 0) {
      return NextResponse.json({ error: 'Offset must be non-negative' }, { status: 400 });
    }

    // Fetch messages
    const messages = await getMessagesForProject(projectId, { limit, offset });
    const totalCount = await getMessageCount(projectId);

    const duration = Date.now() - startedAt;
    logApiResponse('GET', `/api/projects/${params.id}/messages`, duration, {
      count: messages.length,
      totalCount,
    }, reqId);

    return NextResponse.json({
      messages,
      count: messages.length,
      total: totalCount,
      has_more: offset + messages.length < totalCount,
    }, { status: 200 });

  } catch (error) {
    logError('Failed to fetch project messages', error as Error, { projectId });
    return NextResponse.json({ error: 'Internal Server Error' }, { status: 500 });
  }
}

/**
 * POST /api/projects/[id]/messages
 * Create a new message for a project
 *
 * Body:
 *   - message: string (required)
 *
 * Automatically:
 *   - Creates notifications for all authorized project team members
 *   - Excludes the sender from receiving a notification
 */
export async function POST(
  req: Request,
  { params }: { params: { id: string } }
) {
  const startedAt = Date.now();
  const projectId = parseInt(params.id, 10);
  const reqId = req.headers.get('x-request-id') || Math.random().toString(36).slice(2, 10);
  logApiRequest('POST', `/api/projects/${params.id}/messages`, undefined, reqId);

  // Auth check
  const auth = await requireAuth();
  if (!auth.authorized) {
    return auth.response;
  }

  // Project access check
  const access = await requireProjectAccessById(projectId);
  if (!access.authorized) {
    return access.response;
  }

  try {
    const { user } = auth.session as any;
    const body = await req.json();

    // Validate required fields
    if (!body.message || typeof body.message !== 'string') {
      return NextResponse.json(
        { error: 'Message is required' },
        { status: 400 }
      );
    }

    // Validate message length
    if (body.message.trim().length === 0) {
      return NextResponse.json(
        { error: 'Message cannot be empty' },
        { status: 400 }
      );
    }

    if (body.message.length > 10000) {
      return NextResponse.json(
        { error: 'Message too long (max 10000 characters)' },
        { status: 400 }
      );
    }

    // Parse @mentions from message
    const mentions = await parseMentions(body.message.trim());
    const mentionedUserIds = mentions.map(m => m.user_id);

    // Create message with mention metadata
    const messageInput: CreateMessageInput = {
      project_id: projectId,
      sender_id: user.email || user.id,
      sender_name: user.name,
      sender_role: user.role,
      message: body.message.trim(),
      is_system_message: false,
      metadata: {
        ...(body.metadata || {}),
        mentions: mentions.length > 0 ? mentions : undefined,
      },
    };

    const createdMessage = await createMessage(messageInput);

    // Get project name for notification title
    const projectResult = await sql`
      SELECT p.project_id, p.customer_name
      FROM projects p
      WHERE p.record_id = ${projectId}
      LIMIT 1
    `;
    const projectName = projectResult.rows[0]?.project_id || projectResult.rows[0]?.customer_name || `Project #${projectId}`;

    let notificationsSent = 0;

    // 1. Create HIGH PRIORITY notifications for mentioned users
    if (mentions.length > 0) {
      for (const mention of mentions) {
        // Don't notify yourself
        if (mention.user_email === messageInput.sender_id || mention.user_id === user.id) {
          continue;
        }

        try {
          const metadata: InternalMessageMetadata = {
            message_id: createdMessage.id.toString(),
          };

          await createNotification({
            user_id: mention.user_id,
            project_id: projectId,
            type: 'internal_message',
            priority: 'critical', // High priority for mentions
            source: 'internal',
            title: `${messageInput.sender_name} mentioned you in ${projectName}`,
            message: createdMessage.message.substring(0, 200), // First 200 chars
            metadata,
            sender_id: messageInput.sender_id,
            sender_name: messageInput.sender_name,
            sender_role: messageInput.sender_role,
            icon: 'at-sign',
            color: 'orange',
            action_url: `/projects/${projectId}#messages`,
          });

          notificationsSent++;
        } catch (error) {
          logError('Failed to create mention notification', error as Error, {
            userId: mention.user_id,
            messageId: createdMessage.id,
          });
        }
      }
    }

    // 2. Create NORMAL notifications for other project team members (not mentioned)
    const recipients = await getNotificationRecipientsForProject(projectId);
    const otherRecipients = recipients.filter(r =>
      r.userId !== messageInput.sender_id && // Not sender
      !mentionedUserIds.includes(r.userId) // Not already mentioned
    );

    for (const recipient of otherRecipients) {
      try {
        const metadata: InternalMessageMetadata = {
          message_id: createdMessage.id.toString(),
        };

        await createNotification({
          user_id: recipient.userId,
          project_id: projectId,
          type: 'internal_message',
          priority: 'normal',
          source: 'internal',
          title: `New message from ${messageInput.sender_name}`,
          message: createdMessage.message.substring(0, 200), // First 200 chars
          metadata,
          sender_id: messageInput.sender_id,
          sender_name: messageInput.sender_name,
          sender_role: messageInput.sender_role,
          icon: 'message-square',
          color: 'blue',
          action_url: `/projects/${projectId}#messages`,
        });

        notificationsSent++;
      } catch (error) {
        logError('Failed to create notification for message', error as Error, {
          userId: recipient.userId,
          messageId: createdMessage.id,
        });
        // Continue creating notifications for other recipients
      }
    }

    const duration = Date.now() - startedAt;
    logApiResponse('POST', `/api/projects/${params.id}/messages`, duration, {
      messageId: createdMessage.id,
      notificationsSent,
    }, reqId);

    return NextResponse.json({
      success: true,
      message: createdMessage,
      notifications_sent: notificationsSent,
    }, { status: 201 });

  } catch (error) {
    console.error('[POST /api/projects/[id]/messages] Full error:', error);
    logError('Failed to create project message', error as Error, { projectId });

    // Return detailed error in development/staging for debugging
    const isDev = process.env.NODE_ENV === 'development' || process.env.VERCEL_ENV === 'preview';
    return NextResponse.json({
      error: 'Internal Server Error',
      ...(isDev && {
        message: error instanceof Error ? error.message : String(error),
        stack: error instanceof Error ? error.stack : undefined
      })
    }, { status: 500 });
  }
}
