import { NextRequest, NextResponse } from 'next/server';
import { requireRole } from '@/lib/auth/guards';
import { sql } from '@/lib/db/client';
import { logApiRequest, logApiResponse } from '@/lib/logging/logger';

export const runtime = 'nodejs';

/**
 * GET /api/repcard/debug-data
 * 
 * Debug endpoint to show actual data structure for troubleshooting
 */
export async function GET(request: NextRequest) {
  const start = Date.now();
  const path = new URL(request.url).pathname;

  try {
    logApiRequest('GET', path, {});

    // Authentication
    const auth = await requireRole(['closer', 'setter', 'office_leader', 'regional', 'super_admin']);
    if (!auth.authorized) return auth.response;

    // Sample repcard_users data (team fields)
    const usersSample = await sql`
      SELECT 
        repcard_user_id,
        first_name,
        last_name,
        email,
        team,
        team_id,
        team_name,
        office_name,
        role
      FROM repcard_users
      WHERE team IS NOT NULL OR team_name IS NOT NULL OR team_id IS NOT NULL
      LIMIT 10
    `;

    // Sample appointments with 48h calculation
    const appointmentsSample = await sql`
      SELECT 
        a.id,
        a.repcard_appointment_id,
        a.setter_user_id,
        a.closer_user_id,
        a.scheduled_at,
        a.created_at,
        a.is_within_48_hours,
        a.has_power_bill,
        a.is_reschedule,
        c.repcard_customer_id,
        c.created_at as customer_created_at,
        -- Calculate hours difference manually
        CASE
          WHEN a.scheduled_at IS NOT NULL AND c.created_at IS NOT NULL THEN
            EXTRACT(EPOCH FROM (
              (a.scheduled_at AT TIME ZONE 'America/New_York') - 
              (c.created_at AT TIME ZONE 'America/New_York')
            )) / 3600
          ELSE NULL
        END as hours_diff_manual
      FROM repcard_appointments a
      LEFT JOIN repcard_customers c ON c.repcard_customer_id::text = a.repcard_customer_id::text
      WHERE a.scheduled_at IS NOT NULL
        AND c.created_at IS NOT NULL
      ORDER BY a.scheduled_at DESC
      LIMIT 20
    `;

    // Sample customers
    const customersSample = await sql`
      SELECT 
        repcard_customer_id,
        setter_user_id,
        created_at,
        name
      FROM repcard_customers
      ORDER BY created_at DESC
      LIMIT 10
    `;

    // Check team data in repcard_teams
    const teamsSample = await sql`
      SELECT 
        repcard_team_id,
        team_name,
        office_id
      FROM repcard_teams
      LIMIT 10
    `;

    // Check users linked to repcard_users
    const usersLinkedSample = await sql`
      SELECT 
        u.id,
        u.repcard_user_id,
        u.name,
        u.email,
        ru.repcard_user_id as ru_repcard_user_id,
        ru.team,
        ru.team_id,
        ru.team_name,
        ru.first_name,
        ru.last_name
      FROM users u
      LEFT JOIN repcard_users ru ON ru.repcard_user_id::text = u.repcard_user_id::text
      WHERE u.repcard_user_id IS NOT NULL
      LIMIT 10
    `;

    // Today's appointments count
    const today = new Date().toISOString().split('T')[0];
    const todayAppointments = await sql`
      SELECT 
        COUNT(*) FILTER (WHERE scheduled_at IS NOT NULL AND (scheduled_at AT TIME ZONE 'UTC' AT TIME ZONE 'America/New_York')::date = ${today}::date) as with_scheduled,
        COUNT(*) FILTER (WHERE scheduled_at IS NULL AND (created_at AT TIME ZONE 'UTC' AT TIME ZONE 'America/New_York')::date = ${today}::date) as with_created,
        COUNT(*) FILTER (WHERE is_within_48_hours = TRUE AND (scheduled_at AT TIME ZONE 'UTC' AT TIME ZONE 'America/New_York')::date = ${today}::date) as within_48h_today
      FROM repcard_appointments
    `;

    const duration = Date.now() - start;
    logApiResponse('GET', path, duration, { status: 200 });

    return NextResponse.json({
      success: true,
      data: {
        repcardUsers: {
          description: 'Sample repcard_users showing team fields',
          count: Array.from(usersSample).length,
          sample: Array.from(usersSample)
        },
        appointments: {
          description: 'Sample appointments with 48h calculation',
          count: Array.from(appointmentsSample).length,
          sample: Array.from(appointmentsSample)
        },
        customers: {
          description: 'Sample customers',
          count: Array.from(customersSample).length,
          sample: Array.from(customersSample)
        },
        teams: {
          description: 'Sample repcard_teams',
          count: Array.from(teamsSample).length,
          sample: Array.from(teamsSample)
        },
        usersLinked: {
          description: 'Sample users linked to repcard_users',
          count: Array.from(usersLinkedSample).length,
          sample: Array.from(usersLinkedSample)
        },
        todayStats: {
          description: "Today's appointment statistics",
          date: today,
          stats: Array.from(todayAppointments)[0]
        }
      }
    });
  } catch (error) {
    console.error('[RepCard Debug Data] Error:', error);
    return NextResponse.json(
      {
        success: false,
        error: error instanceof Error ? error.message : 'Unknown error',
        details: error instanceof Error ? error.stack : String(error)
      },
      { status: 500 }
    );
  }
}
