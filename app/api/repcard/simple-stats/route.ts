import { NextRequest, NextResponse } from 'next/server';
import { sql } from '@/lib/db/client';
import { getServerSession } from 'next-auth';
import { authOptions } from '@/lib/auth/config';

/**
 * Simple RepCard Stats API
 * Direct queries to RepCard tables - no complex filtering, no user linking required
 * Just show ALL RepCard data from the database
 */
export async function GET(request: NextRequest) {
  try {
    // Basic auth check
    const session = await getServerSession(authOptions);
    if (!session) {
      return NextResponse.json({ error: 'Unauthorized' }, { status: 401 });
    }

    const { searchParams } = new URL(request.url);
    const startDate = searchParams.get('startDate') || undefined;
    const endDate = searchParams.get('endDate') || undefined;

    // Calculate date range (default: last 90 days)
    let calculatedStartDate: string;
    let calculatedEndDate: string = new Date().toISOString().split('T')[0];

    if (startDate && endDate) {
      calculatedStartDate = startDate;
      calculatedEndDate = endDate;
    } else {
      const end = new Date();
      const start = new Date();
      start.setDate(start.getDate() - 90);
      calculatedStartDate = start.toISOString().split('T')[0];
      calculatedEndDate = end.toISOString().split('T')[0];
    }

    console.log(`[RepCard Simple Stats] Querying data from ${calculatedStartDate} to ${calculatedEndDate}`);

    // Helper to extract count
    const getCount = (result: any): number => {
      if (Array.isArray(result)) {
        return Number(result[0]?.count || 0);
      }
      if (result?.rows && Array.isArray(result.rows)) {
        return Number(result.rows[0]?.count || 0);
      }
      const arr = Array.from(result);
      return Number(arr[0]?.count || 0);
    };

    // 1. Total Doors Knocked (customers created in date range)
    const doorsKnockedResult = await sql`
      SELECT COUNT(*)::bigint as count
      FROM repcard_customers
      WHERE created_at::date >= ${calculatedStartDate}::date
        AND created_at::date <= ${calculatedEndDate}::date
    `;
    const doorsKnocked = getCount(doorsKnockedResult);

    // 2. Total Appointments Set (appointments created/scheduled in date range)
    const appointmentsResult = await sql`
      SELECT COUNT(*)::bigint as count
      FROM repcard_appointments
      WHERE (
        (scheduled_at IS NOT NULL AND scheduled_at::date >= ${calculatedStartDate}::date AND scheduled_at::date <= ${calculatedEndDate}::date)
        OR
        (scheduled_at IS NULL AND created_at::date >= ${calculatedStartDate}::date AND created_at::date <= ${calculatedEndDate}::date)
      )
    `;
    const appointmentsSet = getCount(appointmentsResult);

    // 3. Conversion Rate (appointments / doors knocked)
    const conversionRate = doorsKnocked > 0 ? (appointmentsSet / doorsKnocked) * 100 : 0;

    // 4. Active Reps (RepCard users with data in date range)
    const activeRepsResult = await sql`
      SELECT COUNT(DISTINCT ru.repcard_user_id)::bigint as count
      FROM repcard_users ru
      WHERE ru.status = 1
        AND (
          EXISTS (
            SELECT 1 FROM repcard_customers c
            WHERE c.setter_user_id = ru.repcard_user_id
              AND c.created_at::date >= ${calculatedStartDate}::date
              AND c.created_at::date <= ${calculatedEndDate}::date
          )
          OR EXISTS (
            SELECT 1 FROM repcard_appointments a
            WHERE a.setter_user_id = ru.repcard_user_id
              AND (
                (a.scheduled_at IS NOT NULL AND a.scheduled_at::date >= ${calculatedStartDate}::date AND a.scheduled_at::date <= ${calculatedEndDate}::date)
                OR
                (a.scheduled_at IS NULL AND a.created_at::date >= ${calculatedStartDate}::date AND a.created_at::date <= ${calculatedEndDate}::date)
              )
          )
        )
    `;
    const activeReps = getCount(activeRepsResult);

    // 5. Appointment Speed (% scheduled within 24 hours)
    const speedResult = await sql`
      SELECT 
        COUNT(*)::bigint as total,
        COUNT(*) FILTER (
          WHERE c.created_at IS NOT NULL 
            AND a.scheduled_at IS NOT NULL
            AND EXTRACT(EPOCH FROM (a.scheduled_at - c.created_at)) / 3600 < 24
        )::bigint as within_24h
      FROM repcard_appointments a
      LEFT JOIN repcard_customers c ON a.repcard_customer_id = c.repcard_customer_id
      WHERE (
        (a.scheduled_at IS NOT NULL AND a.scheduled_at::date >= ${calculatedStartDate}::date AND a.scheduled_at::date <= ${calculatedEndDate}::date)
        OR
        (a.scheduled_at IS NULL AND a.created_at::date >= ${calculatedStartDate}::date AND a.created_at::date <= ${calculatedEndDate}::date)
      )
    `;
    const speedData = Array.isArray(speedResult) ? speedResult[0] : speedResult.rows?.[0] || Array.from(speedResult)[0];
    const appointmentSpeed = speedData?.total > 0 ? (Number(speedData.within_24h || 0) / Number(speedData.total)) * 100 : 0;

    // 6. Power Bill Attachment Rate (% customers with attachments)
    const attachmentResult = await sql`
      SELECT 
        COUNT(DISTINCT c.repcard_customer_id)::bigint as total_customers,
        COUNT(DISTINCT CASE WHEN att.id IS NOT NULL THEN c.repcard_customer_id END)::bigint as with_attachments
      FROM repcard_customers c
      LEFT JOIN repcard_customer_attachments att ON c.repcard_customer_id = att.repcard_customer_id::text
      WHERE c.created_at::date >= ${calculatedStartDate}::date
        AND c.created_at::date <= ${calculatedEndDate}::date
    `;
    const attachmentData = Array.isArray(attachmentResult) ? attachmentResult[0] : attachmentResult.rows?.[0] || Array.from(attachmentResult)[0];
    const attachmentRate = attachmentData?.total_customers > 0 
      ? (Number(attachmentData.with_attachments || 0) / Number(attachmentData.total_customers)) * 100 
      : 0;

    // 7. Quality Score (composite: 60% appointment speed, 40% attachment rate)
    const qualityScore = (appointmentSpeed * 0.6) + (attachmentRate * 0.4);

    // 8. Top Reps by Doors Knocked
    const topDoorsResult = await sql`
      SELECT 
        ru.repcard_user_id,
        COALESCE(u.name, TRIM(ru.first_name || ' ' || ru.last_name), ru.email, 'RepCard User ' || ru.repcard_user_id::text) as name,
        ru.email,
        COALESCE(u.sales_office[1], ru.office_name) as office,
        COUNT(c.repcard_customer_id)::bigint as doors_knocked
      FROM repcard_users ru
      LEFT JOIN repcard_customers c ON ru.repcard_user_id = c.setter_user_id
        AND c.created_at::date >= ${calculatedStartDate}::date
        AND c.created_at::date <= ${calculatedEndDate}::date
      LEFT JOIN users u ON u.repcard_user_id = ru.repcard_user_id
      WHERE ru.status = 1
      GROUP BY ru.repcard_user_id, ru.first_name, ru.last_name, ru.email, ru.office_name, u.name, u.sales_office
      HAVING COUNT(c.repcard_customer_id) > 0
      ORDER BY doors_knocked DESC
      LIMIT 20
    `;
    // Extract results - handle different result formats
    const getRows = (result: any): any[] => {
      if (Array.isArray(result)) {
        return result;
      }
      if (result?.rows && Array.isArray(result.rows)) {
        return result.rows;
      }
      return Array.from(result);
    };
    
    const topDoors = getRows(topDoorsResult);
    console.log(`[RepCard Simple Stats] Top doors query returned ${topDoors.length} reps`);

    // 9. Top Reps by Appointments
    const topAppointmentsResult = await sql`
      SELECT 
        ru.repcard_user_id,
        COALESCE(u.name, TRIM(ru.first_name || ' ' || ru.last_name), ru.email, 'RepCard User ' || ru.repcard_user_id::text) as name,
        ru.email,
        COALESCE(u.sales_office[1], ru.office_name) as office,
        COUNT(a.repcard_appointment_id)::bigint as appointments
      FROM repcard_users ru
      LEFT JOIN repcard_appointments a ON ru.repcard_user_id = a.setter_user_id
        AND (
          (a.scheduled_at IS NOT NULL AND a.scheduled_at::date >= ${calculatedStartDate}::date AND a.scheduled_at::date <= ${calculatedEndDate}::date)
          OR
          (a.scheduled_at IS NULL AND a.created_at::date >= ${calculatedStartDate}::date AND a.created_at::date <= ${calculatedEndDate}::date)
        )
      LEFT JOIN users u ON u.repcard_user_id = ru.repcard_user_id
      WHERE ru.status = 1
      GROUP BY ru.repcard_user_id, ru.first_name, ru.last_name, ru.email, ru.office_name, u.name, u.sales_office
      HAVING COUNT(a.repcard_appointment_id) > 0
      ORDER BY appointments DESC
      LIMIT 20
    `;
    const topAppointments = getRows(topAppointmentsResult);
    console.log(`[RepCard Simple Stats] Top appointments query returned ${topAppointments.length} reps`);

    return NextResponse.json({
      success: true,
      dateRange: {
        startDate: calculatedStartDate,
        endDate: calculatedEndDate
      },
      overview: {
        doorsKnocked,
        appointmentsSet,
        conversionRate: Math.round(conversionRate * 10) / 10,
        qualityScore: Math.round(qualityScore * 10) / 10,
        activeReps
      },
      qualityMetrics: {
        appointmentSpeed: Math.round(appointmentSpeed * 10) / 10,
        attachmentRate: Math.round(attachmentRate * 10) / 10,
        qualityScore: Math.round(qualityScore * 10) / 10
      },
      leaderboards: {
        doorsKnocked: topDoors.map((r: any, i: number) => {
          const doors = Number(r.doors_knocked || 0);
          return {
            rank: i + 1,
            repcardUserId: r.repcard_user_id,
            name: r.name || `RepCard User ${r.repcard_user_id}`,
            email: r.email || '',
            office: r.office || null,
            value: doors
          };
        }),
        appointments: topAppointments.map((r: any, i: number) => {
          const appointments = Number(r.appointments || 0);
          return {
            rank: i + 1,
            repcardUserId: r.repcard_user_id,
            name: r.name || `RepCard User ${r.repcard_user_id}`,
            email: r.email || '',
            office: r.office || null,
            value: appointments
          };
        })
      }
    });

  } catch (error) {
    console.error('[RepCard Simple Stats] Error:', error);
    return NextResponse.json(
      {
        success: false,
        error: error instanceof Error ? error.message : 'Unknown error'
      },
      { status: 500 }
    );
  }
}

