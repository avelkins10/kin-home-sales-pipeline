import { NextRequest, NextResponse } from 'next/server';
import { requireRole } from '@/lib/auth/guards';
import { sql } from '@/lib/db/client';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';

export const runtime = 'nodejs';

/**
 * GET /api/admin/repcard/comprehensive-metrics
 * 
 * Returns ALL available RepCard metrics - comprehensive source of truth
 * Shows every metric we're tracking from RepCard data
 */
export async function GET(request: NextRequest) {
  const start = Date.now();
  const path = new URL(request.url).pathname;
  const requestId = request.headers.get('x-request-id') || `req-${Date.now()}`;

  try {
    logApiRequest('GET', path, { endpoint: 'repcard-comprehensive-metrics', requestId });

    // Authentication - admin only
    const auth = await requireRole(['super_admin']);
    if (!auth.authorized) return auth.response;

    // Helper to extract rows
    const getRows = (result: any): any[] => {
      if (Array.isArray(result)) return result;
      if (result?.rows && Array.isArray(result.rows)) return result.rows;
      return Array.from(result);
    };

    // ========================================
    // 1. CUSTOMERS METRICS
    // ========================================
    let customersMetrics;
    try {
      customersMetrics = await sql`
      SELECT 
        COUNT(*)::int as total_customers,
        COUNT(*) FILTER (WHERE created_at >= NOW() - INTERVAL '30 days')::int as customers_last_30_days,
        COUNT(*) FILTER (WHERE created_at >= NOW() - INTERVAL '7 days')::int as customers_last_7_days,
        COUNT(DISTINCT setter_user_id)::int as unique_setters,
        COUNT(DISTINCT office_id)::int as unique_offices,
        COUNT(*) FILTER (WHERE status IS NOT NULL)::int as customers_with_status,
        COUNT(DISTINCT status)::int as unique_statuses,
        COUNT(*) FILTER (WHERE email IS NOT NULL)::int as customers_with_email,
        COUNT(*) FILTER (WHERE phone IS NOT NULL)::int as customers_with_phone,
        COUNT(*) FILTER (WHERE address IS NOT NULL)::int as customers_with_address,
        MIN(created_at) as earliest_customer,
        MAX(created_at) as latest_customer
      FROM repcard_customers
    `;
    } catch (error) {
      console.error('[RepCard Comprehensive Metrics] Error in customersMetrics:', error);
      throw error;
    }
    const customers = getRows(customersMetrics)[0] || {};

    // Customers by status
    let customersByStatus;
    try {
      customersByStatus = await sql`
      SELECT 
        status,
        COUNT(*)::int as count
      FROM repcard_customers
      WHERE status IS NOT NULL
      GROUP BY status
      ORDER BY count DESC
    `;
    } catch (error) {
      console.error('[RepCard Comprehensive Metrics] Error in customersByStatus:', error);
      customersByStatus = [];
    }

    // Customers by office
    let customersByOffice;
    try {
      customersByOffice = await sql`
      SELECT 
        o.name as office_name,
        o.repcard_office_id,
        COUNT(c.repcard_customer_id)::int as count
      FROM repcard_offices o
      LEFT JOIN repcard_customers c ON c.office_id = o.repcard_office_id
      GROUP BY o.repcard_office_id, o.name
      ORDER BY count DESC
    `;
    } catch (error) {
      console.error('[RepCard Comprehensive Metrics] Error in customersByOffice:', error);
      customersByOffice = [];
    }

    // ========================================
    // 2. APPOINTMENTS METRICS
    // ========================================
    let appointmentsMetrics;
    try {
      appointmentsMetrics = await sql`
      SELECT 
        COUNT(*)::int as total_appointments,
        COUNT(*) FILTER (WHERE scheduled_at >= NOW() - INTERVAL '30 days')::int as appointments_last_30_days,
        COUNT(*) FILTER (WHERE scheduled_at >= NOW() - INTERVAL '7 days')::int as appointments_last_7_days,
        COUNT(DISTINCT setter_user_id)::int as unique_setters,
        COUNT(DISTINCT closer_user_id)::int as unique_closers,
        COUNT(DISTINCT office_id)::int as unique_offices,
        COUNT(*) FILTER (WHERE scheduled_at IS NOT NULL)::int as appointments_with_scheduled_time,
        COUNT(*) FILTER (WHERE completed_at IS NOT NULL)::int as appointments_completed,
        COUNT(*) FILTER (WHERE disposition IS NOT NULL)::int as appointments_with_disposition,
        COUNT(DISTINCT disposition)::int as unique_dispositions,
        COUNT(*) FILTER (WHERE status_category IS NOT NULL)::int as appointments_with_status_category,
        COUNT(DISTINCT status_category)::int as unique_status_categories,
        COUNT(*) FILTER (WHERE is_within_48_hours = TRUE)::int as within_48_hours,
        COUNT(*) FILTER (WHERE has_power_bill = TRUE)::int as with_power_bill,
        COUNT(*) FILTER (WHERE is_reschedule = TRUE)::int as reschedules,
        COUNT(*) FILTER (WHERE is_within_48_hours = TRUE AND has_power_bill = TRUE)::int as both_quality_metrics,
        COUNT(*) FILTER (WHERE is_within_48_hours IS NULL)::int as null_48h,
        COUNT(*) FILTER (WHERE has_power_bill IS NULL)::int as null_power_bill,
        MIN(scheduled_at) as earliest_appointment,
        MAX(scheduled_at) as latest_appointment
      FROM repcard_appointments
    `;
    } catch (error) {
      console.error('[RepCard Comprehensive Metrics] Error in appointmentsMetrics:', error);
      throw error;
    }
    const appointments = getRows(appointmentsMetrics)[0] || {};

    // Appointments by disposition
    let appointmentsByDisposition;
    try {
      appointmentsByDisposition = await sql`
      SELECT 
        disposition,
        COUNT(*)::int as count
      FROM repcard_appointments
      WHERE disposition IS NOT NULL
      GROUP BY disposition
      ORDER BY count DESC
    `;
    } catch (error) {
      console.error('[RepCard Comprehensive Metrics] Error in appointmentsByDisposition:', error);
      appointmentsByDisposition = [];
    }

    // Appointments by status category
    let appointmentsByStatusCategory;
    try {
      appointmentsByStatusCategory = await sql`
      SELECT 
        status_category,
        COUNT(*)::int as count
      FROM repcard_appointments
      WHERE status_category IS NOT NULL
      GROUP BY status_category
      ORDER BY count DESC
    `;
    } catch (error) {
      console.error('[RepCard Comprehensive Metrics] Error in appointmentsByStatusCategory:', error);
      appointmentsByStatusCategory = [];
    }

    // ========================================
    // 3. QUALITY METRICS
    // ========================================
    let qualityMetrics;
    try {
      qualityMetrics = await sql`
      SELECT 
        COUNT(*)::int as total_appointments,
        COUNT(*) FILTER (WHERE is_within_48_hours = TRUE)::int as within_48h_count,
        COUNT(*) FILTER (WHERE has_power_bill = TRUE)::int as with_power_bill_count,
        COUNT(*) FILTER (WHERE is_within_48_hours = TRUE AND has_power_bill = TRUE)::int as both_count,
        COUNT(*) FILTER (WHERE is_within_48_hours = FALSE AND has_power_bill = FALSE)::int as neither_count,
        COUNT(*) FILTER (WHERE is_reschedule = TRUE)::int as reschedule_count,
        ROUND(
          ((COUNT(*) FILTER (WHERE is_within_48_hours = TRUE)::numeric / 
           NULLIF(COUNT(*) FILTER (WHERE scheduled_at IS NOT NULL), 0)::numeric) * 100)::numeric, 
          2
        ) as within_48h_percentage,
        ROUND(
          ((COUNT(*) FILTER (WHERE has_power_bill = TRUE)::numeric / 
           NULLIF(COUNT(*), 0)::numeric) * 100)::numeric, 
          2
        ) as power_bill_percentage,
        ROUND(
          ((COUNT(*) FILTER (WHERE is_reschedule = TRUE)::numeric / 
           NULLIF(COUNT(*), 0)::numeric) * 100)::numeric, 
          2
        ) as reschedule_percentage
      FROM repcard_appointments
      WHERE scheduled_at >= NOW() - INTERVAL '30 days'
    `;
    } catch (error) {
      console.error('[RepCard Comprehensive Metrics] Error in qualityMetrics:', error);
      throw error;
    }
    const quality = getRows(qualityMetrics)[0] || {};

    // ========================================
    // 4. ATTACHMENTS METRICS
    // ========================================
    let attachmentsMetrics;
    try {
      attachmentsMetrics = await sql`
      SELECT 
        (SELECT COUNT(*)::int FROM repcard_customer_attachments) as customer_attachments_total,
        (SELECT COUNT(*)::int FROM repcard_appointment_attachments) as appointment_attachments_total,
        (SELECT COUNT(DISTINCT repcard_customer_id)::int FROM repcard_customer_attachments WHERE repcard_customer_id IS NOT NULL) as customers_with_attachments,
        (SELECT COUNT(DISTINCT repcard_appointment_id)::int FROM repcard_appointment_attachments WHERE repcard_appointment_id IS NOT NULL) as appointments_with_attachments,
        (SELECT COUNT(*)::int 
         FROM repcard_customer_attachments 
         WHERE (attachment_type IS NOT NULL AND (attachment_type ILIKE '%power%' OR attachment_type ILIKE '%bill%'))
            OR (file_name IS NOT NULL AND (file_name ILIKE '%power%' OR file_name ILIKE '%bill%'))) as power_bill_attachments_customer,
        (SELECT COUNT(*)::int 
         FROM repcard_appointment_attachments 
         WHERE (attachment_type IS NOT NULL AND (attachment_type ILIKE '%power%' OR attachment_type ILIKE '%bill%'))
            OR (file_name IS NOT NULL AND (file_name ILIKE '%power%' OR file_name ILIKE '%bill%'))) as power_bill_attachments_appointment
    `;
    } catch (error) {
      console.error('[RepCard Comprehensive Metrics] Error in attachmentsMetrics:', error);
      // Don't throw - return empty metrics instead
      attachmentsMetrics = [{
        customer_attachments_total: 0,
        appointment_attachments_total: 0,
        customers_with_attachments: 0,
        appointments_with_attachments: 0,
        power_bill_attachments_customer: 0,
        power_bill_attachments_appointment: 0
      }];
    }
    const attachments = getRows(attachmentsMetrics)[0] || {};

    // Attachments by type
    let attachmentsByType;
    try {
      attachmentsByType = await sql`
      SELECT 
        COALESCE(attachment_type, 'Unknown') as attachment_type,
        COUNT(*)::int as count
      FROM (
        SELECT attachment_type FROM repcard_customer_attachments
        UNION ALL
        SELECT attachment_type FROM repcard_appointment_attachments
      ) combined
      GROUP BY attachment_type
      ORDER BY count DESC
    `;
    } catch (error) {
      console.error('[RepCard Comprehensive Metrics] Error in attachmentsByType:', error);
      attachmentsByType = [];
    }

    // ========================================
    // 5. NOTES METRICS
    // ========================================
    let notesMetrics;
    try {
      notesMetrics = await sql`
      SELECT 
        COUNT(*)::int as total_notes,
        COUNT(DISTINCT repcard_customer_id)::int as customers_with_notes,
        COUNT(DISTINCT repcard_user_id)::int as users_who_wrote_notes,
        COUNT(*) FILTER (WHERE created_at >= NOW() - INTERVAL '30 days')::int as notes_last_30_days,
        MIN(created_at) as earliest_note,
        MAX(created_at) as latest_note
      FROM repcard_customer_notes
    `;
    } catch (error) {
      console.error('[RepCard Comprehensive Metrics] Error in notesMetrics:', error);
      notesMetrics = [];
    }
    const notes = getRows(notesMetrics)[0] || {};

    // ========================================
    // 6. STATUS LOGS METRICS
    // ========================================
    let statusLogsMetrics;
    try {
      statusLogsMetrics = await sql`
      SELECT 
        COUNT(*)::int as total_status_changes,
        COUNT(DISTINCT repcard_customer_id)::int as customers_with_status_changes,
        COUNT(DISTINCT changed_by_user_id)::int as users_who_changed_status,
        COUNT(DISTINCT new_status)::int as unique_statuses,
        COUNT(*) FILTER (WHERE changed_at >= NOW() - INTERVAL '30 days')::int as changes_last_30_days,
        MIN(changed_at) as earliest_change,
        MAX(changed_at) as latest_change
      FROM repcard_status_logs
    `;
    } catch (error) {
      console.error('[RepCard Comprehensive Metrics] Error in statusLogsMetrics:', error);
      statusLogsMetrics = [];
    }
    const statusLogs = getRows(statusLogsMetrics)[0] || {};

    // Status changes by status
    let statusChangesByStatus;
    try {
      statusChangesByStatus = await sql`
      SELECT 
        new_status,
        COUNT(*)::int as count
      FROM repcard_status_logs
      WHERE new_status IS NOT NULL
      GROUP BY new_status
      ORDER BY count DESC
      LIMIT 20
    `;
    } catch (error) {
      console.error('[RepCard Comprehensive Metrics] Error in statusChangesByStatus:', error);
      statusChangesByStatus = [];
    }

    // ========================================
    // 7. USERS METRICS
    // ========================================
    let usersMetrics;
    try {
      usersMetrics = await sql`
      SELECT 
        COUNT(*)::int as total_users,
        COUNT(*) FILTER (WHERE status = 1)::int as active_users,
        COUNT(*) FILTER (WHERE status = 0)::int as inactive_users,
        COUNT(DISTINCT role)::int as unique_roles,
        COUNT(DISTINCT office_id)::int as users_in_offices,
        COUNT(*) FILTER (WHERE first_verified_door_knock IS NOT NULL)::int as users_with_door_knocks,
        COUNT(*) FILTER (WHERE first_appointment IS NOT NULL)::int as users_with_appointments
      FROM repcard_users
    `;
    } catch (error) {
      console.error('[RepCard Comprehensive Metrics] Error in usersMetrics:', error);
      throw error;
    }
    const users = getRows(usersMetrics)[0] || {};

    // Users by role
    let usersByRole;
    try {
      usersByRole = await sql`
      SELECT 
        role,
        COUNT(*)::int as count
      FROM repcard_users
      WHERE role IS NOT NULL
      GROUP BY role
      ORDER BY count DESC
    `;
    } catch (error) {
      console.error('[RepCard Comprehensive Metrics] Error in usersByRole:', error);
      usersByRole = [];
    }

    // ========================================
    // 8. OFFICES METRICS
    // ========================================
    let officesMetrics;
    try {
      officesMetrics = await sql`
      SELECT 
        COUNT(*)::int as total_offices,
        COUNT(DISTINCT company_id)::int as unique_companies
      FROM repcard_offices
    `;
    } catch (error) {
      console.error('[RepCard Comprehensive Metrics] Error in officesMetrics:', error);
      throw error;
    }
    const offices = getRows(officesMetrics)[0] || {};

    // ========================================
    // 9. CONVERSION METRICS
    // ========================================
    let conversionMetrics;
    try {
      conversionMetrics = await sql`
      SELECT 
        (SELECT COUNT(*)::int FROM repcard_customers) as total_customers,
        (SELECT COUNT(*)::int FROM repcard_appointments) as total_appointments,
        (SELECT COUNT(*) FILTER (WHERE disposition ILIKE '%closed%')::int FROM repcard_appointments) as closed_appointments,
        ROUND(
          (((SELECT COUNT(*)::int FROM repcard_appointments)::numeric / 
           NULLIF((SELECT COUNT(*)::int FROM repcard_customers), 0)::numeric) * 100)::numeric, 
          2
        ) as customer_to_appointment_rate,
        ROUND(
          (((SELECT COUNT(*) FILTER (WHERE disposition ILIKE '%closed%')::int FROM repcard_appointments)::numeric / 
           NULLIF((SELECT COUNT(*)::int FROM repcard_appointments), 0)::numeric) * 100)::numeric, 
          2
        ) as appointment_to_close_rate
    `;
    } catch (error) {
      console.error('[RepCard Comprehensive Metrics] Error in conversionMetrics:', error);
      throw error;
    }
    const conversion = getRows(conversionMetrics)[0] || {};

    // ========================================
    // 10. SYNC METRICS
    // ========================================
    let syncMetrics;
    try {
      syncMetrics = await sql`
      SELECT 
        entity_type,
        MAX(completed_at) as last_sync,
        COUNT(*)::int as total_syncs,
        SUM(records_fetched)::bigint as total_fetched,
        SUM(records_inserted)::bigint as total_inserted,
        SUM(records_updated)::bigint as total_updated,
        COUNT(*) FILTER (WHERE status = 'completed')::int as successful_syncs,
        COUNT(*) FILTER (WHERE status = 'failed')::int as failed_syncs
      FROM repcard_sync_log
      GROUP BY entity_type
      ORDER BY last_sync DESC
    `;
    } catch (error) {
      console.error('[RepCard Comprehensive Metrics] Error in syncMetrics:', error);
      syncMetrics = [];
    }

    const duration = Date.now() - start;
    logApiResponse('GET', path, duration, { status: 200, requestId });

    return NextResponse.json({
      success: true,
      metrics: {
        customers: {
          summary: customers,
          byStatus: getRows(customersByStatus),
          byOffice: getRows(customersByOffice),
        },
        appointments: {
          summary: appointments,
          byDisposition: getRows(appointmentsByDisposition),
          byStatusCategory: getRows(appointmentsByStatusCategory),
        },
        quality: quality,
        attachments: {
          summary: attachments,
          byType: getRows(attachmentsByType),
        },
        notes: notes,
        statusLogs: {
          summary: statusLogs,
          byStatus: getRows(statusChangesByStatus),
        },
        users: {
          summary: users,
          byRole: getRows(usersByRole),
        },
        offices: offices,
        conversion: conversion,
        sync: getRows(syncMetrics),
      },
      generatedAt: new Date().toISOString(),
      duration: duration
    });

  } catch (error) {
    const duration = Date.now() - start;
    const errorMessage = error instanceof Error ? error.message : 'Unknown error';
    const errorStack = error instanceof Error ? error.stack : undefined;
    
    console.error('[RepCard Comprehensive Metrics] Full error:', {
      message: errorMessage,
      stack: errorStack,
      requestId
    });
    
    logError('repcard-comprehensive-metrics', error as Error, { requestId });
    logApiResponse('GET', path, duration, { status: 500, requestId });
    
    return NextResponse.json(
      {
        success: false,
        error: 'Failed to fetch comprehensive metrics',
        message: errorMessage,
        ...(process.env.NODE_ENV === 'development' && { stack: errorStack }),
      },
      { status: 500 }
    );
  }
}
