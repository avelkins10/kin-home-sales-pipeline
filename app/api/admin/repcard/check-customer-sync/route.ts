import { NextRequest, NextResponse } from 'next/server';
import { requireRole } from '@/lib/auth/guards';
import { sql } from '@/lib/db/client';

export const runtime = 'nodejs';

/**
 * GET /api/admin/repcard/check-customer-sync
 * 
 * Checks why appointments don't have matching customers or customer.created_at
 */
export async function GET(request: NextRequest) {
  try {
    const auth = await requireRole(['super_admin']);
    if (!auth.authorized) return auth.response;

    // Check appointments from last 30 days
    const thirtyDaysAgo = new Date();
    thirtyDaysAgo.setDate(thirtyDaysAgo.getDate() - 30);

    // Find appointments with missing customers
    const missingCustomers = await sql`
      SELECT 
        a.id,
        a.repcard_appointment_id,
        a.repcard_customer_id,
        a.scheduled_at,
        CASE 
          WHEN c.id IS NULL THEN 'customer_not_found'
          WHEN c.created_at IS NULL THEN 'customer_missing_created_at'
          ELSE 'customer_ok'
        END as issue_type,
        c.id as customer_table_id,
        c.created_at as customer_created_at
      FROM repcard_appointments a
      LEFT JOIN repcard_customers c ON c.repcard_customer_id::text = a.repcard_customer_id::text
      WHERE a.scheduled_at >= ${thirtyDaysAgo.toISOString()}
        AND (c.id IS NULL OR c.created_at IS NULL)
      ORDER BY a.scheduled_at DESC
      LIMIT 50
    `;
    const missing = Array.isArray(missingCustomers) ? missingCustomers : (missingCustomers?.rows || []);

    // Get summary stats
    const summary = await sql`
      SELECT 
        COUNT(*)::int as total_recent_appointments,
        COUNT(*) FILTER (WHERE c.id IS NULL)::int as appointments_without_customer,
        COUNT(*) FILTER (WHERE c.id IS NOT NULL AND c.created_at IS NULL)::int as customers_without_created_at,
        COUNT(*) FILTER (WHERE c.id IS NOT NULL AND c.created_at IS NOT NULL)::int as appointments_with_valid_customer
      FROM repcard_appointments a
      LEFT JOIN repcard_customers c ON c.repcard_customer_id::text = a.repcard_customer_id::text
      WHERE a.scheduled_at >= ${thirtyDaysAgo.toISOString()}
    `;
    const summaryRow = Array.isArray(summary) ? summary[0] : (summary?.rows?.[0] || {});

    // Check if customers exist but just missing created_at
    const customersWithoutCreatedAt = await sql`
      SELECT 
        repcard_customer_id,
        id,
        name,
        created_at,
        updated_at,
        synced_at
      FROM repcard_customers
      WHERE created_at IS NULL
      ORDER BY synced_at DESC
      LIMIT 20
    `;
    const customersMissing = Array.isArray(customersWithoutCreatedAt) ? customersWithoutCreatedAt : (customersWithoutCreatedAt?.rows || []);

    // Check sample of repcard_customer_ids from appointments that don't have customers
    const sampleMissingIds = missing.slice(0, 10).map((m: any) => m.repcard_customer_id).filter(Boolean);
    let customerLookup: any[] = [];
    if (sampleMissingIds.length > 0) {
      const lookup = await sql`
        SELECT 
          repcard_customer_id,
          id,
          name,
          created_at,
          synced_at
        FROM repcard_customers
        WHERE repcard_customer_id::text = ANY(${sql(sampleMissingIds.map(String))})
      `;
      customerLookup = Array.isArray(lookup) ? lookup : (lookup?.rows || []);
    }

    return NextResponse.json({
      success: true,
      summary: {
        totalRecentAppointments: summaryRow.total_recent_appointments || 0,
        appointmentsWithoutCustomer: summaryRow.appointments_without_customer || 0,
        customersWithoutCreatedAt: summaryRow.customers_without_created_at || 0,
        appointmentsWithValidCustomer: summaryRow.appointments_with_valid_customer || 0,
      },
      sampleMissingCustomers: missing.slice(0, 20).map((m: any) => ({
        appointmentId: m.id,
        repcardAppointmentId: m.repcard_appointment_id,
        repcardCustomerId: m.repcard_customer_id,
        scheduledAt: m.scheduled_at,
        issueType: m.issue_type,
        customerTableId: m.customer_table_id,
        customerCreatedAt: m.customer_created_at,
      })),
      customersWithoutCreatedAt: customersMissing.map((c: any) => ({
        repcardCustomerId: c.repcard_customer_id,
        customerTableId: c.id,
        name: c.name,
        createdAt: c.created_at,
        syncedAt: c.synced_at,
      })),
      customerLookupResults: customerLookup,
      diagnosis: {
        issue: (summaryRow.appointments_without_customer || 0) > 0 || (summaryRow.customers_without_created_at || 0) > 0
          ? `${summaryRow.appointments_without_customer || 0} appointments don't have matching customers, ${summaryRow.customers_without_created_at || 0} customers missing created_at`
          : 'All appointments have valid customers with created_at',
        recommendation: (summaryRow.appointments_without_customer || 0) > 0
          ? 'Run customer sync to ensure all customers are synced from RepCard'
          : (summaryRow.customers_without_created_at || 0) > 0
          ? 'Customers exist but missing created_at - check RepCard API response and sync service'
          : 'Customer sync appears correct'
      }
    });

  } catch (error) {
    console.error('[Check Customer Sync] Error:', error);
    return NextResponse.json(
      {
        success: false,
        error: 'Failed to check customer sync',
        message: error instanceof Error ? error.message : 'Unknown error',
      },
      { status: 500 }
    );
  }
}
