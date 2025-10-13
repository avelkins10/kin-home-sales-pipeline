// app/api/user/timezone/route.ts
import { NextResponse } from 'next/server';
import { requireAuth } from '@/lib/auth/guards';
import { sql } from '@/lib/db/client';
import { logInfo, logError } from '@/lib/logging/logger';

// Valid IANA timezone identifiers
const VALID_TIMEZONES = [
  'America/New_York',
  'America/Chicago',
  'America/Denver',
  'America/Los_Angeles',
  'America/Phoenix',
  'America/Anchorage',
  'Pacific/Honolulu',
  'America/Toronto',
  'America/Vancouver',
  // Add more as needed
];

export async function POST(req: Request) {
  const auth = await requireAuth();
  if (!auth.authorized) return auth.response;

  try {
    const body = await req.json();
    const { timezone } = body;

    if (!timezone || typeof timezone !== 'string') {
      return NextResponse.json(
        { error: 'Timezone is required and must be a string' },
        { status: 400 }
      );
    }

    // Validate timezone (allow any valid IANA timezone)
    // Basic validation - check if it's a reasonable format
    if (!timezone.match(/^[A-Za-z_]+\/[A-Za-z_]+$/)) {
      return NextResponse.json(
        { error: 'Invalid timezone format' },
        { status: 400 }
      );
    }

    const userId = auth.session.user.id;

    // Update user timezone in database
    await sql`
      UPDATE users
      SET timezone = ${timezone},
          updated_at = NOW()
      WHERE id = ${userId}
    `;

    logInfo('[TIMEZONE] Updated user timezone', {
      userId,
      timezone,
      important: true,
    });

    return NextResponse.json({ success: true, timezone });
  } catch (error) {
    console.error('[API] /user/timezone ERROR:', error);
    logError('Failed to update user timezone', error as Error, {});
    return NextResponse.json(
      {
        error: 'Internal Server Error',
        message: error instanceof Error ? error.message : String(error),
      },
      { status: 500 }
    );
  }
}
