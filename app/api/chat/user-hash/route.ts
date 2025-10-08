export const runtime = 'nodejs';

import { NextResponse } from 'next/server';
import { createHmac } from 'crypto';
import { requireAuth } from '@/lib/auth/guards';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';

/**
 * GET /api/chat/user-hash
 * Generate a secure HMAC-SHA256 hash for Front Chat user verification
 */
export async function GET(req: Request) {
  const startedAt = Date.now();
  const reqId = req.headers.get('x-request-id') || Math.random().toString(36).slice(2, 10);
  logApiRequest('GET', '/api/chat/user-hash', undefined, reqId);

  // Auth check
  const auth = await requireAuth();
  if (!auth.authorized) {
    return auth.response;
  }

  try {
    const { user } = auth.session as any;
    const userEmail = user.email || user.name;

    if (!userEmail) {
      return NextResponse.json({ error: 'User email not found' }, { status: 400 });
    }

    // Get Front Chat secret from environment
    const secret = process.env.FRONT_CHAT_SECRET;
    if (!secret) {
      logError('Front Chat secret not configured', new Error('FRONT_CHAT_SECRET not set'));
      return NextResponse.json({ error: 'Chat service not configured' }, { status: 500 });
    }

    // Generate HMAC-SHA256 hash
    const userHash = createHmac('sha256', secret)
      .update(userEmail)
      .digest('hex');

    const duration = Date.now() - startedAt;
    logApiResponse('GET', '/api/chat/user-hash', duration, { success: true }, reqId);

    return NextResponse.json({ userHash, email: userEmail }, { status: 200 });
  } catch (error) {
    logError('Failed to generate user hash', error as Error);
    return NextResponse.json({ error: 'Internal Server Error' }, { status: 500 });
  }
}
