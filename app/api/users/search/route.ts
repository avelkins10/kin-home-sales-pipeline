export const runtime = 'nodejs';

import { NextRequest, NextResponse } from 'next/server';
import { requireAuth } from '@/lib/auth/guards';
import { sql } from '@/lib/db/client';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';

/**
 * GET /api/users/search?q=search_term
 * Search for users by name or email (for @mentions)
 * Returns active users only
 */
export async function GET(request: NextRequest) {
  const startedAt = Date.now();
  const reqId = request.headers.get('x-request-id') || Math.random().toString(36).slice(2, 10);
  logApiRequest('GET', '/api/users/search', undefined, reqId);

  try {
    const auth = await requireAuth();
    if (!auth.authorized) {
      logApiResponse('GET', '/api/users/search', Date.now() - startedAt, { status: 403 }, reqId);
      return auth.response;
    }

    const { searchParams } = new URL(request.url);
    const query = searchParams.get('q')?.trim();

    if (!query || query.length < 2) {
      return NextResponse.json(
        { error: 'Search query must be at least 2 characters' },
        { status: 400 }
      );
    }

    // Search users by name or email
    // Limit to 20 results for autocomplete performance
    const result = await sql`
      SELECT
        id,
        name,
        email,
        role,
        sales_office
      FROM users
      WHERE is_active = true
        AND (
          LOWER(name) LIKE LOWER(${'%' + query + '%'})
          OR LOWER(email) LIKE LOWER(${'%' + query + '%'})
        )
      ORDER BY
        CASE
          WHEN LOWER(name) LIKE LOWER(${query + '%'}) THEN 1
          WHEN LOWER(email) LIKE LOWER(${query + '%'}) THEN 2
          ELSE 3
        END,
        name
      LIMIT 20
    `;

    const users = result.rows.map(user => ({
      id: user.id,
      name: user.name,
      email: user.email,
      role: user.role,
      office: user.sales_office?.[0] || null,
    }));

    const duration = Date.now() - startedAt;
    logApiResponse('GET', '/api/users/search', duration, {
      query,
      count: users.length,
    }, reqId);

    return NextResponse.json({ users }, { status: 200 });

  } catch (error) {
    logError('Failed to search users', error as Error);
    return NextResponse.json(
      { error: 'Internal Server Error' },
      { status: 500 }
    );
  }
}
