// app/api/operations/settings/repcard-debug/route.ts
import { NextRequest, NextResponse } from 'next/server';
import { requireAuth } from '@/lib/auth/guards';
import { sql } from '@/lib/db/client';
import { RepCardClient } from '@/lib/repcard/client';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';

export const runtime = 'nodejs';

/**
 * GET /api/operations/settings/repcard-debug
 * Get comprehensive RepCard diagnostic data
 */
export async function GET(request: NextRequest) {
  const requestId = crypto.randomUUID();

  try {
    logApiRequest('GET', '/api/operations/settings/repcard-debug', requestId);

    // Require authentication and operations role
    const { session } = await requireAuth();
    const userRole = session?.user?.role;
    const operationsRoles = ['operations_coordinator', 'operations_manager', 'office_leader', 'regional', 'super_admin'];

    if (!userRole || !operationsRoles.includes(userRole)) {
      return NextResponse.json(
        { error: 'Forbidden - operations role required' },
        { status: 403 }
      );
    }

    // Initialize RepCard client
    const repcardClient = new RepCardClient();

    // Get today's date for filtering
    const today = new Date().toISOString().split('T')[0];
    const octStart = '2025-10-01';
    const octEnd = '2025-10-28';

    // 1. Fetch recent sync logs
    const recentSyncsRaw = await sql`
      SELECT
        entity_type,
        sync_type,
        started_at,
        completed_at,
        status,
        records_fetched,
        records_inserted,
        records_updated,
        records_failed,
        error_message
      FROM repcard_sync_log
      ORDER BY started_at DESC
      LIMIT 10
    `;
    const recentSyncs = Array.from(recentSyncsRaw);

    // 2. Fetch recent customers from database
    const recentCustomersRaw = await sql`
      SELECT
        id,
        repcard_customer_id,
        setter_user_id,
        name,
        created_at,
        updated_at,
        synced_at
      FROM repcard_customers
      ORDER BY created_at DESC
      LIMIT 20
    `;
    const recentCustomers = Array.from(recentCustomersRaw);

    // 3. Fetch recent appointments from database
    const recentAppointmentsRaw = await sql`
      SELECT
        id,
        repcard_appointment_id,
        setter_user_id,
        closer_user_id,
        disposition,
        scheduled_at,
        completed_at,
        created_at,
        updated_at,
        synced_at
      FROM repcard_appointments
      ORDER BY created_at DESC
      LIMIT 20
    `;
    const recentAppointments = Array.from(recentAppointmentsRaw);

    // 4. Get today's data counts
    const todayCustomersCountRaw = await sql`
      SELECT COUNT(*) as count
      FROM repcard_customers
      WHERE DATE(created_at) = ${today}::date
    `;
    const todayCustomersCount = parseInt(Array.from(todayCustomersCountRaw)[0].count);

    const todayAppointmentsCountRaw = await sql`
      SELECT COUNT(*) as count
      FROM repcard_appointments
      WHERE DATE(created_at) = ${today}::date
    `;
    const todayAppointmentsCount = parseInt(Array.from(todayAppointmentsCountRaw)[0].count);

    // 5. Get October 2025 data counts
    const octCustomersCountRaw = await sql`
      SELECT COUNT(*) as count
      FROM repcard_customers
      WHERE created_at >= ${octStart}::timestamp
        AND created_at <= (${octEnd}::timestamp + INTERVAL '1 day')
    `;
    const octCustomersCount = parseInt(Array.from(octCustomersCountRaw)[0].count);

    const octAppointmentsCountRaw = await sql`
      SELECT COUNT(*) as count
      FROM repcard_appointments
      WHERE created_at >= ${octStart}::timestamp
        AND created_at <= (${octEnd}::timestamp + INTERVAL '1 day')
    `;
    const octAppointmentsCount = parseInt(Array.from(octAppointmentsCountRaw)[0].count);

    // 6. Get date range of all data
    const dataRangesRaw = await sql`
      SELECT
        (SELECT MIN(created_at) FROM repcard_customers) as customers_min,
        (SELECT MAX(created_at) FROM repcard_customers) as customers_max,
        (SELECT MIN(created_at) FROM repcard_appointments) as appointments_min,
        (SELECT MAX(created_at) FROM repcard_appointments) as appointments_max,
        (SELECT COUNT(*) FROM repcard_customers) as total_customers,
        (SELECT COUNT(*) FROM repcard_appointments) as total_appointments
    `;
    const dataRanges = Array.from(dataRangesRaw)[0];

    // 7. Check user ID mappings for recent data
    const uniqueSetterIds = [
      ...new Set([
        ...recentCustomers.map((c: any) => c.setter_user_id).filter(Boolean),
        ...recentAppointments.map((a: any) => a.setter_user_id).filter(Boolean)
      ])
    ];

    const userMappingsRaw = await sql`
      SELECT
        id,
        name,
        email,
        repcard_user_id
      FROM users
      WHERE repcard_user_id = ANY(${uniqueSetterIds.map(String)}::text[])
    `;
    const userMappings = Array.from(userMappingsRaw);

    // Create lookup map
    const userMappingLookup = new Map(
      userMappings.map((u: any) => [u.repcard_user_id, u])
    );

    // 8. Get orphaned records count (setter_user_id not matching any user)
    const orphanedCustomersRaw = await sql`
      SELECT COUNT(*) as count
      FROM repcard_customers rc
      WHERE rc.setter_user_id IS NOT NULL
        AND NOT EXISTS (
          SELECT 1 FROM users u
          WHERE u.repcard_user_id::text = rc.setter_user_id::text
        )
    `;
    const orphanedCustomersCount = parseInt(Array.from(orphanedCustomersRaw)[0].count);

    const orphanedAppointmentsRaw = await sql`
      SELECT COUNT(*) as count
      FROM repcard_appointments ra
      WHERE ra.setter_user_id IS NOT NULL
        AND NOT EXISTS (
          SELECT 1 FROM users u
          WHERE u.repcard_user_id::text = ra.setter_user_id::text
        )
    `;
    const orphanedAppointmentsCount = parseInt(Array.from(orphanedAppointmentsRaw)[0].count);

    // 9. Try to fetch recent data from RepCard API (with error handling)
    let apiRecentCustomers = null;
    let apiRecentAppointments = null;
    let apiError = null;

    try {
      // Fetch most recent customers from API
      const customersResponse = await repcardClient.getCustomers({
        page: 1,
        perPage: 10,
        // Sort by most recent (RepCard API doesn't have explicit sort param, but pagination returns newest first)
      });
      apiRecentCustomers = customersResponse.result.data;

      // Fetch most recent appointments from API
      const appointmentsResponse = await repcardClient.getAppointments({
        page: 1,
        perPage: 10,
      });
      apiRecentAppointments = appointmentsResponse.result.data;
    } catch (error) {
      apiError = error instanceof Error ? error.message : 'Unknown error fetching from RepCard API';
      console.error('[RepCard Debug] API fetch error:', error);
    }

    // 10. Get database schema info
    const customersColumnsRaw = await sql`
      SELECT column_name, data_type
      FROM information_schema.columns
      WHERE table_name = 'repcard_customers'
      ORDER BY ordinal_position
    `;
    const customersColumns = Array.from(customersColumnsRaw);

    const appointmentsColumnsRaw = await sql`
      SELECT column_name, data_type
      FROM information_schema.columns
      WHERE table_name = 'repcard_appointments'
      ORDER BY ordinal_position
    `;
    const appointmentsColumns = Array.from(appointmentsColumnsRaw);

    const response = {
      sync: {
        recentSyncs,
        lastCustomerSync: recentSyncs.find((s: any) => s.entity_type === 'customers'),
        lastAppointmentSync: recentSyncs.find((s: any) => s.entity_type === 'appointments'),
      },
      database: {
        recentCustomers: recentCustomers.map((c: any) => ({
          ...c,
          matchedUser: userMappingLookup.get(c.setter_user_id) || null
        })),
        recentAppointments: recentAppointments.map((a: any) => ({
          ...a,
          matchedUser: userMappingLookup.get(a.setter_user_id) || null
        })),
        schema: {
          customersColumns,
          appointmentsColumns
        }
      },
      counts: {
        today: {
          customers: todayCustomersCount,
          appointments: todayAppointmentsCount
        },
        october2025: {
          customers: octCustomersCount,
          appointments: octAppointmentsCount
        },
        total: {
          customers: parseInt(dataRanges.total_customers),
          appointments: parseInt(dataRanges.total_appointments)
        },
        orphaned: {
          customers: orphanedCustomersCount,
          appointments: orphanedAppointmentsCount
        }
      },
      dateRanges: {
        customers: {
          min: dataRanges.customers_min,
          max: dataRanges.customers_max
        },
        appointments: {
          min: dataRanges.appointments_min,
          max: dataRanges.appointments_max
        }
      },
      api: {
        recentCustomers: apiRecentCustomers,
        recentAppointments: apiRecentAppointments,
        error: apiError
      },
      userMappings: {
        total: userMappings.length,
        users: userMappings
      }
    };

    logApiResponse(requestId, 200, { success: true });

    return NextResponse.json(response);

  } catch (error) {
    logError('Failed to get RepCard debug data', error as Error, { requestId });
    return NextResponse.json(
      { error: 'Internal server error', details: error instanceof Error ? error.message : 'Unknown error' },
      { status: 500 }
    );
  }
}
