import { NextRequest, NextResponse } from 'next/server';
import { sql } from '@/lib/db/client';
import { getServerSession } from 'next-auth';
import { authOptions } from '@/lib/auth/config';

/**
 * RepCard Trends API
 * Returns time-series data for trend charts
 * Supports: doors knocked, appointments set, sales closed, etc.
 */
export async function GET(request: NextRequest) {
  try {
    const session = await getServerSession(authOptions);
    if (!session) {
      return NextResponse.json({ error: 'Unauthorized' }, { status: 401 });
    }

    const { searchParams } = new URL(request.url);
    const metric = searchParams.get('metric') || 'doors_knocked'; // doors_knocked, appointments_set, sales_closed
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

    // Determine grouping interval based on date range
    const daysDiff = Math.ceil((new Date(calculatedEndDate).getTime() - new Date(calculatedStartDate).getTime()) / (1000 * 60 * 60 * 24));
    let groupBy: 'day' | 'week' | 'month';
    let dateFormat: string;
    
    if (daysDiff <= 30) {
      groupBy = 'day';
      dateFormat = 'YYYY-MM-DD';
    } else if (daysDiff <= 90) {
      groupBy = 'week';
      dateFormat = 'YYYY-"W"WW';
    } else {
      groupBy = 'month';
      dateFormat = 'YYYY-MM';
    }

    // Helper to extract rows
    const getRows = (result: any): any[] => {
      if (Array.isArray(result)) return result;
      if (result?.rows && Array.isArray(result.rows)) return result.rows;
      return Array.from(result);
    };

    let trends: any[] = [];

    if (metric === 'doors_knocked') {
      // Doors knocked by day/week/month
      const result = await sql`
        SELECT 
          DATE_TRUNC(${groupBy}, c.created_at::date) as period,
          COUNT(*)::bigint as value
        FROM repcard_customers c
        WHERE c.created_at::date >= ${calculatedStartDate}::date
          AND c.created_at::date <= ${calculatedEndDate}::date
          ${repcardUserId ? sql`AND c.setter_user_id = ${parseInt(repcardUserId)}` : sql``}
          ${officeIds.length > 0 ? sql`AND c.office_id = ANY(${officeIds}::int[])` : sql``}
        GROUP BY period
        ORDER BY period ASC
      `;
      trends = getRows(result).map((r: any) => ({
        date: r.period.toISOString().split('T')[0],
        value: Number(r.value || 0)
      }));
    } else if (metric === 'appointments_set') {
      // Appointments set by day/week/month
      const result = await sql`
        SELECT 
          DATE_TRUNC(${groupBy}, COALESCE(a.scheduled_at::date, a.created_at::date)) as period,
          COUNT(*)::bigint as value
        FROM repcard_appointments a
        WHERE (
          (a.scheduled_at IS NOT NULL AND a.scheduled_at::date >= ${calculatedStartDate}::date AND a.scheduled_at::date <= ${calculatedEndDate}::date)
          OR (a.scheduled_at IS NULL AND a.created_at::date >= ${calculatedStartDate}::date AND a.created_at::date <= ${calculatedEndDate}::date)
        )
          ${repcardUserId ? sql`AND a.setter_user_id = ${parseInt(repcardUserId)}` : sql``}
          ${officeIds.length > 0 ? sql`AND a.office_id = ANY(${officeIds}::int[])` : sql``}
        GROUP BY period
        ORDER BY period ASC
      `;
      trends = getRows(result).map((r: any) => ({
        date: r.period.toISOString().split('T')[0],
        value: Number(r.value || 0)
      }));
    } else if (metric === 'sales_closed') {
      // Sales closed by day/week/month (from status logs)
      const result = await sql`
        SELECT 
          DATE_TRUNC(${groupBy}, sl.changed_at::date) as period,
          COUNT(*)::bigint as value
        FROM repcard_status_logs sl
        WHERE sl.changed_at::date >= ${calculatedStartDate}::date
          AND sl.changed_at::date <= ${calculatedEndDate}::date
          AND (sl.new_status ILIKE '%closed%' OR sl.new_status ILIKE '%sold%')
          ${repcardUserId ? sql`AND sl.changed_by_user_id = ${parseInt(repcardUserId)}` : sql``}
        GROUP BY period
        ORDER BY period ASC
      `;
      trends = getRows(result).map((r: any) => ({
        date: r.period.toISOString().split('T')[0],
        value: Number(r.value || 0)
      }));
    } else if (metric === 'conversion_rate') {
      // Conversion rate over time (appointments / doors)
      const doorsResult = await sql`
        SELECT 
          DATE_TRUNC(${groupBy}, c.created_at::date) as period,
          COUNT(*)::bigint as doors
        FROM repcard_customers c
        WHERE c.created_at::date >= ${calculatedStartDate}::date
          AND c.created_at::date <= ${calculatedEndDate}::date
          ${repcardUserId ? sql`AND c.setter_user_id = ${parseInt(repcardUserId)}` : sql``}
          ${officeIds.length > 0 ? sql`AND c.office_id = ANY(${officeIds}::int[])` : sql``}
        GROUP BY period
        ORDER BY period ASC
      `;
      const doorsMap = new Map(getRows(doorsResult).map((r: any) => [r.period.toISOString().split('T')[0], Number(r.doors || 0)]));

      const appointmentsResult = await sql`
        SELECT 
          DATE_TRUNC(${groupBy}, COALESCE(a.scheduled_at::date, a.created_at::date)) as period,
          COUNT(*)::bigint as appointments
        FROM repcard_appointments a
        WHERE (
          (a.scheduled_at IS NOT NULL AND a.scheduled_at::date >= ${calculatedStartDate}::date AND a.scheduled_at::date <= ${calculatedEndDate}::date)
          OR (a.scheduled_at IS NULL AND a.created_at::date >= ${calculatedStartDate}::date AND a.created_at::date <= ${calculatedEndDate}::date)
        )
          ${repcardUserId ? sql`AND a.setter_user_id = ${parseInt(repcardUserId)}` : sql``}
          ${officeIds.length > 0 ? sql`AND a.office_id = ANY(${officeIds}::int[])` : sql``}
        GROUP BY period
        ORDER BY period ASC
      `;
      const appointmentsMap = new Map(getRows(appointmentsResult).map((r: any) => [r.period.toISOString().split('T')[0], Number(r.appointments || 0)]));

      // Combine and calculate conversion rate
      const allPeriods = new Set([...doorsMap.keys(), ...appointmentsMap.keys()]);
      trends = Array.from(allPeriods).sort().map(period => {
        const doors = doorsMap.get(period) || 0;
        const appointments = appointmentsMap.get(period) || 0;
        return {
          date: period,
          value: doors > 0 ? Math.round((appointments / doors) * 1000) / 10 : 0
        };
      });
    }

    // Fill in missing dates with 0 values for continuous chart
    const filledTrends: any[] = [];
    const start = new Date(calculatedStartDate);
    const end = new Date(calculatedEndDate);
    const trendsMap = new Map(trends.map(t => [t.date, t.value]));

    let current = new Date(start);
    while (current <= end) {
      const dateStr = current.toISOString().split('T')[0];
      filledTrends.push({
        date: dateStr,
        value: trendsMap.get(dateStr) || 0
      });
      
      if (groupBy === 'day') {
        current.setDate(current.getDate() + 1);
      } else if (groupBy === 'week') {
        current.setDate(current.getDate() + 7);
      } else {
        current.setMonth(current.getMonth() + 1);
      }
    }

    return NextResponse.json({
      success: true,
      metric,
      timeRange: {
        startDate: calculatedStartDate,
        endDate: calculatedEndDate
      },
      groupBy,
      trends: filledTrends
    });

  } catch (error) {
    console.error('[RepCard Trends] Error:', error);
    return NextResponse.json(
      {
        success: false,
        error: error instanceof Error ? error.message : 'Unknown error'
      },
      { status: 500 }
    );
  }
}

