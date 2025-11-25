import { NextRequest, NextResponse } from 'next/server';
import { getServerSession } from 'next-auth';
import { authOptions } from '@/lib/auth/config';
import { sql } from '@/lib/db/client';

export const dynamic = 'force-dynamic';
export const runtime = 'nodejs';

/**
 * GET /api/repcard/data-debug
 *
 * Debug endpoint to inspect RepCard attribution data
 * Requires super_admin role
 */
export async function GET(request: NextRequest) {
  try {
    // Check authentication
    const session = await getServerSession(authOptions);
    if (!session || session.user.role !== 'super_admin') {
      return NextResponse.json(
        { error: 'Unauthorized - super_admin access required' },
        { status: 403 }
      );
    }

    const { searchParams } = new URL(request.url);
    const view = searchParams.get('view') || 'overview';

    // Overview statistics
    if (view === 'overview') {
      const stats = await sql`
        SELECT
          (SELECT COUNT(DISTINCT repcard_appointment_id)::int FROM repcard_appointments WHERE disposition ILIKE '%closed%') as total_closed_appointments,
          (SELECT COUNT(DISTINCT repcard_appointment_id)::int FROM repcard_appointments WHERE disposition ILIKE '%closed%' AND closer_user_id IS NOT NULL) as with_closer_assigned,
          (SELECT COUNT(DISTINCT repcard_appointment_id)::int FROM repcard_appointments WHERE disposition ILIKE '%closed%' AND closer_user_id IS NULL) as without_closer,
          (SELECT COUNT(DISTINCT repcard_customer_id)::int FROM repcard_status_logs WHERE LOWER(new_status) LIKE '%closed%' OR LOWER(new_status) LIKE '%sold%') as status_log_closes,
          (SELECT COUNT(DISTINCT repcard_appointment_id)::int FROM repcard_appointments) as total_appointments,
          (SELECT COUNT(*)::int FROM repcard_customers) as total_customers,
          (SELECT COUNT(*)::int FROM users WHERE repcard_user_id IS NOT NULL) as linked_users
      `;

      return NextResponse.json({ stats: stats.rows[0] });
    }

    // Closer comparison - side by side
    if (view === 'closer-comparison') {
      const comparison = await sql`
        WITH appointment_sales AS (
          SELECT
            u.repcard_user_id,
            u.name,
            u.role,
            COUNT(DISTINCT a.repcard_appointment_id)::int as sales_from_appointments
          FROM repcard_appointments a
          JOIN users u ON u.repcard_user_id::TEXT = a.closer_user_id::TEXT
          WHERE a.disposition ILIKE '%closed%'
            AND a.closer_user_id IS NOT NULL
          GROUP BY u.repcard_user_id, u.name, u.role
        ),
        status_log_sales AS (
          SELECT
            u.repcard_user_id,
            u.name,
            COUNT(DISTINCT sl.repcard_customer_id)::int as sales_from_logs
          FROM repcard_status_logs sl
          JOIN users u ON u.repcard_user_id::TEXT = sl.changed_by_user_id::TEXT
          WHERE (
            LOWER(sl.new_status) LIKE '%sold%'
            OR LOWER(sl.new_status) LIKE '%closed%'
            OR LOWER(sl.new_status) LIKE '%won%'
            OR LOWER(sl.new_status) LIKE '%install%'
          )
          GROUP BY u.repcard_user_id, u.name
        )
        SELECT
          COALESCE(a.repcard_user_id, s.repcard_user_id) as repcard_user_id,
          COALESCE(a.name, s.name) as name,
          a.role,
          COALESCE(a.sales_from_appointments, 0) as appointments_method,
          COALESCE(s.sales_from_logs, 0) as status_logs_method,
          COALESCE(a.sales_from_appointments, 0) - COALESCE(s.sales_from_logs, 0) as difference
        FROM appointment_sales a
        FULL OUTER JOIN status_log_sales s ON a.repcard_user_id = s.repcard_user_id
        ORDER BY COALESCE(a.sales_from_appointments, 0) DESC
      `;

      return NextResponse.json({ closers: comparison.rows });
    }

    // Sample appointments with full details
    if (view === 'sample-appointments') {
      const limit = parseInt(searchParams.get('limit') || '50');
      const samples = await sql`
        SELECT
          a.repcard_appointment_id,
          a.closer_user_id,
          u1.name as closer_name,
          a.setter_user_id,
          u2.name as setter_name,
          a.disposition,
          a.status_category,
          a.scheduled_at,
          a.completed_at,
          c.name as customer_name,
          c.repcard_customer_id,
          sl.new_status as status_log_status,
          sl.changed_by_user_id as status_updater_id,
          u3.name as status_updater_name,
          sl.changed_at as status_changed_at,
          CASE
            WHEN a.closer_user_id::TEXT = COALESCE(sl.changed_by_user_id::TEXT, '') THEN 'MATCH'
            ELSE 'MISMATCH'
          END as attribution_status
        FROM repcard_appointments a
        LEFT JOIN repcard_customers c ON c.repcard_customer_id = a.repcard_customer_id
        LEFT JOIN users u1 ON u1.repcard_user_id::TEXT = a.closer_user_id::TEXT
        LEFT JOIN users u2 ON u2.repcard_user_id::TEXT = a.setter_user_id::TEXT
        LEFT JOIN repcard_status_logs sl ON sl.repcard_customer_id = a.repcard_customer_id
          AND (
            LOWER(sl.new_status) LIKE '%sold%'
            OR LOWER(sl.new_status) LIKE '%closed%'
            OR LOWER(sl.new_status) LIKE '%won%'
            OR LOWER(sl.new_status) LIKE '%install%'
          )
        LEFT JOIN users u3 ON u3.repcard_user_id::TEXT = sl.changed_by_user_id::TEXT
        WHERE a.disposition ILIKE '%closed%'
        ORDER BY a.scheduled_at DESC
        LIMIT ${limit}
      `;

      return NextResponse.json({ appointments: samples.rows });
    }

    // Raw appointment data by ID
    if (view === 'appointment-detail') {
      const appointmentId = searchParams.get('id');
      if (!appointmentId) {
        return NextResponse.json({ error: 'Appointment ID required' }, { status: 400 });
      }

      const appointment = await sql`
        SELECT *
        FROM repcard_appointments
        WHERE repcard_appointment_id = ${parseInt(appointmentId)}
      `;

      if (appointment.rows.length === 0) {
        return NextResponse.json({ error: 'Appointment not found' }, { status: 404 });
      }

      return NextResponse.json({ appointment: appointment.rows[0] });
    }

    return NextResponse.json({ error: 'Invalid view parameter' }, { status: 400 });

  } catch (error) {
    console.error('[RepCard Data Debug] Error:', error);
    return NextResponse.json(
      { error: 'Failed to fetch debug data', details: error instanceof Error ? error.message : 'Unknown error' },
      { status: 500 }
    );
  }
}
