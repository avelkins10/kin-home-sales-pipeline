import { NextRequest, NextResponse } from 'next/server';
import { sql } from '@/lib/db/client';
import { getServerSession } from 'next-auth';
import { authOptions } from '@/lib/auth/config';

/**
 * RepCard Quality Aggregate API
 * Returns aggregated quality metrics: show rate, sit rate, close rate, follow-up rate
 */
export async function GET(request: NextRequest) {
  try {
    const session = await getServerSession(authOptions);
    if (!session) {
      return NextResponse.json({ error: 'Unauthorized' }, { status: 401 });
    }

    const { searchParams } = new URL(request.url);
    const timeRange = searchParams.get('timeRange') || 'month';
    const startDate = searchParams.get('startDate');
    const endDate = searchParams.get('endDate');
    const officeIds = searchParams.get('officeIds')?.split(',').map(id => parseInt(id.trim())).filter(id => !isNaN(id)) || [];
    const repcardUserId = searchParams.get('repcardUserId');

    // Calculate date range
    let calculatedStartDate: string;
    let calculatedEndDate: string = new Date().toISOString().split('T')[0];

    if (startDate && endDate) {
      calculatedStartDate = startDate;
      calculatedEndDate = endDate;
    } else {
      const end = new Date();
      const start = new Date();
      
      switch (timeRange) {
        case 'week':
          start.setDate(start.getDate() - 7);
          break;
        case 'month':
          start.setDate(start.getDate() - 30);
          break;
        case 'quarter':
          start.setDate(start.getDate() - 90);
          break;
        case 'ytd':
          start.setMonth(0, 1);
          start.setHours(0, 0, 0, 0);
          break;
        case 'last_30':
          start.setDate(start.getDate() - 30);
          break;
        case 'last_90':
          start.setDate(start.getDate() - 90);
          break;
        case 'last_12_months':
          start.setDate(start.getDate() - 365);
          break;
        default:
          start.setDate(start.getDate() - 30);
      }
      
      calculatedStartDate = start.toISOString().split('T')[0];
      calculatedEndDate = end.toISOString().split('T')[0];
    }

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

    // Total appointments scheduled
    const totalAppointmentsResult = await sql`
      SELECT COUNT(*)::bigint as count
      FROM repcard_appointments a
      WHERE (
        (a.scheduled_at IS NOT NULL AND a.scheduled_at::date >= ${calculatedStartDate}::date AND a.scheduled_at::date <= ${calculatedEndDate}::date)
        OR (a.scheduled_at IS NULL AND a.created_at::date >= ${calculatedStartDate}::date AND a.created_at::date <= ${calculatedEndDate}::date)
      )
        ${repcardUserId ? sql`AND a.setter_user_id = ${parseInt(repcardUserId)}` : sql``}
        ${officeIds.length > 0 ? sql`AND a.office_id = ANY(${officeIds}::int[])` : sql``}
    `;
    const totalAppointments = getCount(totalAppointmentsResult);

    // Appointments that were completed (showed up)
    const completedAppointmentsResult = await sql`
      SELECT COUNT(*)::bigint as count
      FROM repcard_appointments a
      WHERE a.completed_at IS NOT NULL
        AND a.completed_at::date >= ${calculatedStartDate}::date
        AND a.completed_at::date <= ${calculatedEndDate}::date
        ${repcardUserId ? sql`AND a.setter_user_id = ${parseInt(repcardUserId)}` : sql``}
        ${officeIds.length > 0 ? sql`AND a.office_id = ANY(${officeIds}::int[])` : sql``}
    `;
    const completedAppointments = getCount(completedAppointmentsResult);

    // Appointments that sat (completed and not cancelled/no-show)
    const satAppointmentsResult = await sql`
      SELECT COUNT(*)::bigint as count
      FROM repcard_appointments a
      WHERE a.completed_at IS NOT NULL
        AND a.completed_at::date >= ${calculatedStartDate}::date
        AND a.completed_at::date <= ${calculatedEndDate}::date
        AND a.status_category NOT IN ('cancelled', 'no_show')
        ${repcardUserId ? sql`AND a.setter_user_id = ${parseInt(repcardUserId)}` : sql``}
        ${officeIds.length > 0 ? sql`AND a.office_id = ANY(${officeIds}::int[])` : sql``}
    `;
    const satAppointments = getCount(satAppointmentsResult);

    // Appointments that closed (sat_closed)
    const closedAppointmentsResult = await sql`
      SELECT COUNT(*)::bigint as count
      FROM repcard_appointments a
      WHERE a.status_category = 'sat_closed'
        AND a.completed_at::date >= ${calculatedStartDate}::date
        AND a.completed_at::date <= ${calculatedEndDate}::date
        ${repcardUserId ? sql`AND a.setter_user_id = ${parseInt(repcardUserId)}` : sql``}
        ${officeIds.length > 0 ? sql`AND a.office_id = ANY(${officeIds}::int[])` : sql``}
    `;
    const closedAppointments = getCount(closedAppointmentsResult);

    // Appointments that were rescheduled
    const rescheduledAppointmentsResult = await sql`
      SELECT COUNT(*)::bigint as count
      FROM repcard_appointments a
      WHERE a.status_category = 'rescheduled'
        AND (
          (a.scheduled_at IS NOT NULL AND a.scheduled_at::date >= ${calculatedStartDate}::date AND a.scheduled_at::date <= ${calculatedEndDate}::date)
          OR (a.scheduled_at IS NULL AND a.created_at::date >= ${calculatedStartDate}::date AND a.created_at::date <= ${calculatedEndDate}::date)
        )
        ${repcardUserId ? sql`AND a.setter_user_id = ${parseInt(repcardUserId)}` : sql``}
        ${officeIds.length > 0 ? sql`AND a.office_id = ANY(${officeIds}::int[])` : sql``}
    `;
    const rescheduledAppointments = getCount(rescheduledAppointmentsResult);

    // Customers with follow-up appointments
    const followUpCustomersResult = await sql`
      SELECT COUNT(DISTINCT c.repcard_customer_id)::bigint as count
      FROM repcard_customers c
      WHERE EXISTS (
        SELECT 1 FROM repcard_appointments a1
        WHERE a1.repcard_customer_id = c.repcard_customer_id
          AND a1.created_at::date >= ${calculatedStartDate}::date
          AND a1.created_at::date <= ${calculatedEndDate}::date
      )
      AND EXISTS (
        SELECT 1 FROM repcard_appointments a2
        WHERE a2.repcard_customer_id = c.repcard_customer_id
          AND a2.created_at > (
            SELECT MIN(a3.created_at) FROM repcard_appointments a3
            WHERE a3.repcard_customer_id = c.repcard_customer_id
          )
      )
      AND c.created_at::date >= ${calculatedStartDate}::date
      AND c.created_at::date <= ${calculatedEndDate}::date
      ${repcardUserId ? sql`AND c.setter_user_id = ${parseInt(repcardUserId)}` : sql``}
      ${officeIds.length > 0 ? sql`AND c.office_id = ANY(${officeIds}::int[])` : sql``}
    `;
    const followUpCustomers = getCount(followUpCustomersResult);

    // Total customers
    const totalCustomersResult = await sql`
      SELECT COUNT(*)::bigint as count
      FROM repcard_customers c
      WHERE c.created_at::date >= ${calculatedStartDate}::date
        AND c.created_at::date <= ${calculatedEndDate}::date
        ${repcardUserId ? sql`AND c.setter_user_id = ${parseInt(repcardUserId)}` : sql``}
        ${officeIds.length > 0 ? sql`AND c.office_id = ANY(${officeIds}::int[])` : sql``}
    `;
    const totalCustomers = getCount(totalCustomersResult);

    // Calculate rates
    const showRate = totalAppointments > 0 ? (completedAppointments / totalAppointments) * 100 : 0;
    const sitRate = totalAppointments > 0 ? (satAppointments / totalAppointments) * 100 : 0;
    const closeRate = satAppointments > 0 ? (closedAppointments / satAppointments) * 100 : 0;
    const rescheduleRate = totalAppointments > 0 ? (rescheduledAppointments / totalAppointments) * 100 : 0;
    const followUpRate = totalCustomers > 0 ? (followUpCustomers / totalCustomers) * 100 : 0;

    return NextResponse.json({
      success: true,
      timeRange: {
        startDate: calculatedStartDate,
        endDate: calculatedEndDate
      },
      metrics: {
        showRate: Math.round(showRate * 10) / 10,
        sitRate: Math.round(sitRate * 10) / 10,
        closeRate: Math.round(closeRate * 10) / 10,
        rescheduleRate: Math.round(rescheduleRate * 10) / 10,
        followUpRate: Math.round(followUpRate * 10) / 10
      },
      counts: {
        totalAppointments,
        completedAppointments,
        satAppointments,
        closedAppointments,
        rescheduledAppointments,
        followUpCustomers,
        totalCustomers
      }
    });

  } catch (error) {
    console.error('[RepCard Quality Aggregate] Error:', error);
    return NextResponse.json(
      {
        success: false,
        error: error instanceof Error ? error.message : 'Unknown error'
      },
      { status: 500 }
    );
  }
}
