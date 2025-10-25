export const runtime = 'nodejs';

import { NextResponse } from 'next/server';
import { requireAuth } from '@/lib/auth/guards';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';
import { getPCMessagesForProject, createPCMessage, markPCMessagesRead } from '@/lib/quickbase/queries';
import { createNotification } from '@/lib/db/notifications';

/**
 * Resolve mention usernames to user IDs or emails
 * This is a placeholder implementation - in production, you'd query your user directory
 */
async function resolveMentionToUser(mention: string): Promise<{ userId?: string; email?: string } | null> {
  // TODO: Implement actual user lookup from your user directory
  // This could query QuickBase users table, your database, or external directory
  
  // For now, we'll assume mentions are emails or usernames that can be looked up
  // In a real implementation, you'd:
  // 1. Query your user directory/QuickBase users table
  // 2. Match by username, email, or handle
  // 3. Return the actual user ID and email
  
  // Placeholder logic - replace with actual implementation
  if (mention.includes('@')) {
    // If it looks like an email, use it directly
    return { email: mention };
  }
  
  // For usernames, you'd need to look them up
  // This is where you'd implement the actual lookup logic
  console.warn(`Mention resolution not implemented for: ${mention}`);
  return null;
}

/**
 * GET /api/operations/messages
 * Get PC messages for a project (optionally filtered by task)
 */
export async function GET(req: Request) {
  const startedAt = Date.now();
  const reqId = req.headers.get('x-request-id') || Math.random().toString(36).substring(7);

  logApiRequest('GET', '/api/operations/messages', undefined, reqId);

  try {
    // Authentication
    const auth = await requireAuth();
    if (!auth.authorized) {
      return auth.response;
    }

    const userRole = auth.session.user.role;
    
    // Check user role is operations_coordinator, operations_manager, closer, setter, office_leader, regional, or super_admin
    const allowedRoles = [
      'operations_coordinator',
      'operations_manager',
      'closer',
      'setter',
      'office_leader',
      'regional',
      'super_admin'
    ];

    if (!allowedRoles.includes(userRole)) {
      return NextResponse.json(
        { error: 'Forbidden', message: 'Insufficient permissions' },
        { status: 403 }
      );
    }

    // Parse query parameters
    const { searchParams } = new URL(req.url);
    const projectId = searchParams.get('projectId');
    const recordId = searchParams.get('recordId');
    const taskId = searchParams.get('taskId');

    if (!projectId && !recordId) {
      return NextResponse.json(
        { error: 'Bad Request', message: 'projectId or recordId is required' },
        { status: 400 }
      );
    }

    // Prefer recordId when present and validate it is a number
    const numericRecordId = recordId ? parseInt(recordId) : parseInt(projectId!);
    if (isNaN(numericRecordId)) {
      return NextResponse.json(
        { error: 'Bad Request', message: 'Invalid recordId or projectId' },
        { status: 400 }
      );
    }

    // Extract user email from session
    const userEmail = auth.session.user.email;
    if (!userEmail) {
      return NextResponse.json(
        { error: 'Bad Request', message: 'User email not found' },
        { status: 400 }
      );
    }

    // Get PC messages for the project
    const messages = await getPCMessagesForProject(
      projectId || recordId!,
      numericRecordId,
      taskId ? parseInt(taskId) : null,
      userEmail,
      reqId
    );

    logApiResponse('GET', '/api/operations/messages', Date.now() - startedAt, {
      messages: messages.length,
      projectId,
      taskId
    }, reqId);

    return NextResponse.json({
      messages,
      count: messages.length
    }, { status: 200 });

  } catch (error) {
    logError('Failed to fetch PC messages', error as Error, { reqId });
    return NextResponse.json(
      { error: 'Internal Server Error', message: 'Failed to fetch messages' },
      { status: 500 }
    );
  }
}

/**
 * POST /api/operations/messages
 * Create a new PC message
 */
export async function POST(req: Request) {
  const startedAt = Date.now();
  const reqId = req.headers.get('x-request-id') || Math.random().toString(36).substring(7);

  logApiRequest('POST', '/api/operations/messages', undefined, reqId);

  try {
    // Authentication
    const auth = await requireAuth();
    if (!auth.authorized) {
      return auth.response;
    }

    const userRole = auth.session.user.role;
    
    // Check user role is operations_coordinator, operations_manager, closer, setter, office_leader, regional, or super_admin
    const allowedRoles = [
      'operations_coordinator',
      'operations_manager',
      'closer',
      'setter',
      'office_leader',
      'regional',
      'super_admin'
    ];

    if (!allowedRoles.includes(userRole)) {
      return NextResponse.json(
        { error: 'Forbidden', message: 'Insufficient permissions' },
        { status: 403 }
      );
    }

    // Parse request body
    const body = await req.json();
    const { projectId, recordId, content, taskId, mentions = [] } = body;

    // Validate input
    if ((!projectId && !recordId) || !content) {
      return NextResponse.json(
        { error: 'Bad Request', message: 'projectId or recordId and content are required' },
        { status: 400 }
      );
    }

    // Prefer recordId when present and validate it is a number
    const numericRecordId = recordId ? parseInt(recordId.toString()) : parseInt(projectId!);
    if (isNaN(numericRecordId)) {
      return NextResponse.json(
        { error: 'Bad Request', message: 'Invalid recordId or projectId' },
        { status: 400 }
      );
    }

    // Validate content length (max 5000 chars)
    if (content.length > 5000) {
      return NextResponse.json(
        { error: 'Bad Request', message: 'Content too long (max 5000 characters)' },
        { status: 400 }
      );
    }

    // Validate mentions are valid usernames (simple validation)
    if (mentions && Array.isArray(mentions)) {
      const mentionRegex = /^[a-zA-Z0-9_]+$/;
      for (const mention of mentions) {
        if (typeof mention !== 'string' || !mentionRegex.test(mention)) {
          return NextResponse.json(
            { error: 'Bad Request', message: 'Invalid mention format' },
            { status: 400 }
          );
        }
      }
    }

    // Extract user email from session
    const userEmail = auth.session.user.email;
    if (!userEmail) {
      return NextResponse.json(
        { error: 'Bad Request', message: 'User email not found' },
        { status: 400 }
      );
    }

    // Create the message
    const messageId = await createPCMessage(
      projectId || recordId!.toString(),
      numericRecordId,
      content,
      userEmail,
      taskId || null,
      mentions || [],
      reqId
    );

    // Create notifications for mentioned users
    if (mentions && mentions.length > 0) {
      try {
        for (const mention of mentions) {
          const resolvedUser = await resolveMentionToUser(mention);
          
          if (resolvedUser) {
            await createNotification({
              userId: resolvedUser.userId || resolvedUser.email || mention,
              type: 'mention',
              title: 'You were mentioned in a message',
              message: `You were mentioned in a project message: ${content.substring(0, 100)}...`,
              data: {
                projectId: projectId || recordId!.toString(),
                recordId: numericRecordId,
                taskId,
                messageId,
                mentionedBy: userEmail,
                email: resolvedUser.email
              }
            });
          } else {
            // Log when we can't resolve a mention
            logError('Failed to resolve mention', new Error(`Could not resolve mention: ${mention}`), { reqId, mention });
          }
        }
      } catch (notificationError) {
        // Log notification error but don't fail the message creation
        logError('Failed to create mention notifications', notificationError as Error, { reqId, mentions });
      }
    }

    logApiResponse('POST', '/api/operations/messages', Date.now() - startedAt, {
      messageId,
      projectId: projectId || recordId!.toString(),
      recordId: numericRecordId,
      taskId,
      mentionsCount: mentions?.length || 0
    }, reqId);

    return NextResponse.json({
      success: true,
      messageId,
      message: 'Message created successfully'
    }, { status: 201 });

  } catch (error) {
    logError('Failed to create PC message', error as Error, { reqId });
    return NextResponse.json(
      { error: 'Internal Server Error', message: 'Failed to create message' },
      { status: 500 }
    );
  }
}

/**
 * PATCH /api/operations/messages
 * Mark messages as read for the current user
 */
export async function PATCH(req: Request) {
  const startedAt = Date.now();
  const reqId = req.headers.get('x-request-id') || Math.random().toString(36).substring(7);

  logApiRequest('PATCH', '/api/operations/messages', undefined, reqId);

  try {
    // Authentication
    const auth = await requireAuth();
    if (!auth.authorized) {
      return auth.response;
    }

    const userRole = auth.session.user.role;
    
    // Check user role is operations_coordinator, operations_manager, closer, setter, office_leader, regional, or super_admin
    const allowedRoles = [
      'operations_coordinator',
      'operations_manager',
      'closer',
      'setter',
      'office_leader',
      'regional',
      'super_admin'
    ];

    if (!allowedRoles.includes(userRole)) {
      return NextResponse.json(
        { error: 'Forbidden', message: 'Insufficient permissions' },
        { status: 403 }
      );
    }

    // Parse request body
    const body = await req.json();
    const { messageIds } = body;

    // Validate input
    if (!messageIds || !Array.isArray(messageIds) || messageIds.length === 0) {
      return NextResponse.json(
        { error: 'Bad Request', message: 'messageIds array is required' },
        { status: 400 }
      );
    }

    // Validate messageIds are numbers
    for (const messageId of messageIds) {
      if (!Number.isInteger(messageId) || messageId <= 0) {
        return NextResponse.json(
          { error: 'Bad Request', message: 'Invalid messageId - must be positive integers' },
          { status: 400 }
        );
      }
    }

    // Extract user email from session
    const userEmail = auth.session.user.email;
    if (!userEmail) {
      return NextResponse.json(
        { error: 'Bad Request', message: 'User email not found' },
        { status: 400 }
      );
    }

    // Mark messages as read
    await markPCMessagesRead(messageIds, userEmail, reqId);

    logApiResponse('PATCH', '/api/operations/messages', Date.now() - startedAt, {
      messageIds: messageIds.length,
      userEmail
    }, reqId);

    return NextResponse.json({
      success: true,
      message: 'Messages marked as read successfully'
    }, { status: 200 });

  } catch (error) {
    logError('Failed to mark messages as read', error as Error, { reqId });
    return NextResponse.json(
      { error: 'Internal Server Error', message: 'Failed to mark messages as read' },
      { status: 500 }
    );
  }
}
