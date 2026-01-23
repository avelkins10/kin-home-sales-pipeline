import { NextRequest, NextResponse } from 'next/server';
import { getServerSession } from 'next-auth';
import { authOptions } from '@/lib/auth/config';
import { sql } from '@/lib/db/client';

export const runtime = 'nodejs';

/**
 * Debug endpoint to check what appointments exist in the database for a date range
 * Helps diagnose why appointments aren't showing up
 */
export async function GET(request: NextRequest) {
  try {
    const session = await getServerSession(authOptions);
    if (!session) {
      return NextResponse.json({ error: 'Unauthorized' }, { status: 401 });
    }

    const { searchParams } = new URL(request.url);
    const startDate = searchParams.get('startDate') || '2026-01-23';
    const endDate = searchParams.get('endDate') || '2026-01-30';

    // Check total appointments in database
    const totalCount = await sql`
      SELECT COUNT(*)::int as total FROM repcard_appointments
    `;
    const total = Array.from(totalCount)[0]?.total || 0;

    // Check appointments with scheduled_at in date range (no timezone conversion)
    const withScheduledRaw = await sql`
      SELECT COUNT(*)::int as count
      FROM repcard_appointments
      WHERE scheduled_at IS NOT NULL
        AND scheduled_at::date >= ${startDate}::date
        AND scheduled_at::date <= ${endDate}::date
    `;
    const withScheduledRawCount = Array.from(withScheduledRaw)[0]?.count || 0;

    // Check appointments with scheduled_at in date range (with timezone conversion)
    const withScheduledTZ = await sql`
      SELECT COUNT(*)::int as count
      FROM repcard_appointments
      WHERE scheduled_at IS NOT NULL
        AND (scheduled_at AT TIME ZONE 'UTC' AT TIME ZONE 'America/New_York')::date >= ${startDate}::date
        AND (scheduled_at AT TIME ZONE 'UTC' AT TIME ZONE 'America/New_York')::date <= ${endDate}::date
    `;
    const withScheduledTZCount = Array.from(withScheduledTZ)[0]?.count || 0;

    // Check appointments with NULL scheduled_at but created_at in range
    const withCreatedTZ = await sql`
      SELECT COUNT(*)::int as count
      FROM repcard_appointments
      WHERE scheduled_at IS NULL
        AND (created_at AT TIME ZONE 'UTC' AT TIME ZONE 'America/New_York')::date >= ${startDate}::date
        AND (created_at AT TIME ZONE 'UTC' AT TIME ZONE 'America/New_York')::date <= ${endDate}::date
    `;
    const withCreatedTZCount = Array.from(withCreatedTZ)[0]?.count || 0;

    // Get sample appointments to see date formats
    const samples = await sql`
      SELECT 
        repcard_appointment_id,
        scheduled_at,
        created_at,
        (scheduled_at AT TIME ZONE 'UTC' AT TIME ZONE 'America/New_York')::date as scheduled_at_eastern_date,
        (created_at AT TIME ZONE 'UTC' AT TIME ZONE 'America/New_York')::date as created_at_eastern_date,
        status_category,
        setter_user_id,
        closer_user_id
      FROM repcard_appointments
      WHERE scheduled_at IS NOT NULL
      ORDER BY scheduled_at DESC
      LIMIT 10
    `;
    const sampleAppointments = Array.from(samples);

    // Check date range of all appointments
    const dateRange = await sql`
      SELECT 
        MIN(scheduled_at) as earliest_scheduled,
        MAX(scheduled_at) as latest_scheduled,
        MIN(created_at) as earliest_created,
        MAX(created_at) as latest_created,
        COUNT(*) FILTER (WHERE scheduled_at IS NOT NULL) as has_scheduled_at,
        COUNT(*) FILTER (WHERE scheduled_at IS NULL) as null_scheduled_at
      FROM repcard_appointments
    `;
    const range = Array.from(dateRange)[0];

    return NextResponse.json({
      dateRange: { startDate, endDate },
      counts: {
        totalAppointments: total,
        withScheduledRaw: withScheduledRawCount,
        withScheduledTZ: withScheduledTZCount,
        withCreatedTZ: withCreatedTZCount,
        totalInRange: withScheduledTZCount + withCreatedTZCount
      },
      dateRangeInfo: {
        earliestScheduled: range?.earliest_scheduled,
        latestScheduled: range?.latest_scheduled,
        earliestCreated: range?.earliest_created,
        latestCreated: range?.latest_created,
        hasScheduledAt: range?.has_scheduled_at || 0,
        nullScheduledAt: range?.null_scheduled_at || 0
      },
      sampleAppointments: sampleAppointments.map((a: any) => ({
        repcardAppointmentId: a.repcard_appointment_id,
        scheduledAt: a.scheduled_at,
        scheduledAtEasternDate: a.scheduled_at_eastern_date,
        createdAt: a.created_at,
        createdAtEasternDate: a.created_at_eastern_date,
        statusCategory: a.status_category,
        setterUserId: a.setter_user_id,
        closerUserId: a.closer_user_id
      }))
    });
  } catch (error) {
    return NextResponse.json(
      { error: 'Failed to debug', message: error instanceof Error ? error.message : 'Unknown error' },
      { status: 500 }
    );
  }
}
