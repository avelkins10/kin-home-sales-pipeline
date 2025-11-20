import { NextRequest, NextResponse } from 'next/server';
import { sql } from '@/lib/db/client';
import { getServerSession } from 'next-auth';
import { authOptions } from '@/lib/auth/config';

/**
 * Comprehensive RepCard Stats API
 * Returns ALL RepCard data with proper attribution:
 * - Customers with notes, attachments, status history
 * - Appointments with setter/closer attribution, disposition
 * - Status logs timeline
 * - Attachments (customer & appointment)
 * - Custom fields
 * - User performance metrics
 * - Office/team breakdowns
 */
export async function GET(request: NextRequest) {
  try {
    const session = await getServerSession(authOptions);
    if (!session) {
      return NextResponse.json({ error: 'Unauthorized' }, { status: 401 });
    }

    const { searchParams } = new URL(request.url);
    const startDate = searchParams.get('startDate') || undefined;
    const endDate = searchParams.get('endDate') || undefined;
    const repcardUserId = searchParams.get('repcardUserId') || undefined;
    const officeIdParam = searchParams.get('officeId') || searchParams.get('officeIds') || undefined;
    const officeIds = officeIdParam ? officeIdParam.split(',').map(id => parseInt(id.trim())).filter(id => !isNaN(id)) : [];
    const includeDetails = searchParams.get('includeDetails') === 'true';

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

    console.log(`[RepCard Comprehensive Stats] Querying data from ${calculatedStartDate} to ${calculatedEndDate}`);

    // Helper to extract rows
    const getRows = (result: any): any[] => {
      if (Array.isArray(result)) return result;
      if (result?.rows && Array.isArray(result.rows)) return result.rows;
      return Array.from(result);
    };

    // Helper to extract count
    const getCount = (result: any): number => {
      const rows = getRows(result);
      return Number(rows[0]?.count || 0);
    };

    // Build WHERE clause parts
    const hasRepcardUserId = !!repcardUserId;
    const hasOfficeIds = officeIds.length > 0;
    
    // Helper to conditionally add WHERE conditions
    const addCondition = (condition: boolean, sqlFragment: any) => {
      return condition ? sqlFragment : sql``;
    };


    // 1. OVERVIEW METRICS
    let overviewResult;
    if (hasRepcardUserId && hasOfficeIds) {
      overviewResult = await sql`
        SELECT 
          COUNT(DISTINCT c.repcard_customer_id)::bigint as doors_knocked,
          COUNT(DISTINCT a.repcard_appointment_id)::bigint as appointments_set,
          COUNT(DISTINCT CASE WHEN a.disposition ILIKE '%closed%' THEN a.repcard_appointment_id END)::bigint as sales_closed,
          COUNT(DISTINCT CASE WHEN att.id IS NOT NULL THEN c.repcard_customer_id END)::bigint as customers_with_attachments,
          COUNT(DISTINCT n.repcard_note_id)::bigint as total_notes,
          COUNT(DISTINCT sl.repcard_log_id)::bigint as status_changes
        FROM repcard_customers c
        LEFT JOIN repcard_appointments a ON c.repcard_customer_id = a.repcard_customer_id
          AND (
            (a.scheduled_at IS NOT NULL AND a.scheduled_at::date >= ${calculatedStartDate}::date AND a.scheduled_at::date <= ${calculatedEndDate}::date)
            OR (a.scheduled_at IS NULL AND a.created_at::date >= ${calculatedStartDate}::date AND a.created_at::date <= ${calculatedEndDate}::date)
          )
        LEFT JOIN repcard_customer_attachments att ON c.repcard_customer_id = att.repcard_customer_id::text
        LEFT JOIN repcard_customer_notes n ON c.repcard_customer_id = n.repcard_customer_id
        LEFT JOIN repcard_status_logs sl ON c.repcard_customer_id = sl.repcard_customer_id
        WHERE c.created_at::date >= ${calculatedStartDate}::date
          AND c.created_at::date <= ${calculatedEndDate}::date
          AND c.setter_user_id = ${parseInt(repcardUserId!)}
          AND c.office_id = ANY(${officeIds}::int[])
      `;
    } else if (hasRepcardUserId) {
      overviewResult = await sql`
        SELECT 
          COUNT(DISTINCT c.repcard_customer_id)::bigint as doors_knocked,
          COUNT(DISTINCT a.repcard_appointment_id)::bigint as appointments_set,
          COUNT(DISTINCT CASE WHEN a.disposition ILIKE '%closed%' THEN a.repcard_appointment_id END)::bigint as sales_closed,
          COUNT(DISTINCT CASE WHEN att.id IS NOT NULL THEN c.repcard_customer_id END)::bigint as customers_with_attachments,
          COUNT(DISTINCT n.repcard_note_id)::bigint as total_notes,
          COUNT(DISTINCT sl.repcard_log_id)::bigint as status_changes
        FROM repcard_customers c
        LEFT JOIN repcard_appointments a ON c.repcard_customer_id = a.repcard_customer_id
          AND (
            (a.scheduled_at IS NOT NULL AND a.scheduled_at::date >= ${calculatedStartDate}::date AND a.scheduled_at::date <= ${calculatedEndDate}::date)
            OR (a.scheduled_at IS NULL AND a.created_at::date >= ${calculatedStartDate}::date AND a.created_at::date <= ${calculatedEndDate}::date)
          )
        LEFT JOIN repcard_customer_attachments att ON c.repcard_customer_id = att.repcard_customer_id::text
        LEFT JOIN repcard_customer_notes n ON c.repcard_customer_id = n.repcard_customer_id
        LEFT JOIN repcard_status_logs sl ON c.repcard_customer_id = sl.repcard_customer_id
        WHERE c.created_at::date >= ${calculatedStartDate}::date
          AND c.created_at::date <= ${calculatedEndDate}::date
          AND c.setter_user_id = ${parseInt(repcardUserId!)}
      `;
    } else if (hasOfficeIds) {
      overviewResult = await sql`
        SELECT 
          COUNT(DISTINCT c.repcard_customer_id)::bigint as doors_knocked,
          COUNT(DISTINCT a.repcard_appointment_id)::bigint as appointments_set,
          COUNT(DISTINCT CASE WHEN a.disposition ILIKE '%closed%' THEN a.repcard_appointment_id END)::bigint as sales_closed,
          COUNT(DISTINCT CASE WHEN att.id IS NOT NULL THEN c.repcard_customer_id END)::bigint as customers_with_attachments,
          COUNT(DISTINCT n.repcard_note_id)::bigint as total_notes,
          COUNT(DISTINCT sl.repcard_log_id)::bigint as status_changes
        FROM repcard_customers c
        LEFT JOIN repcard_appointments a ON c.repcard_customer_id = a.repcard_customer_id
          AND (
            (a.scheduled_at IS NOT NULL AND a.scheduled_at::date >= ${calculatedStartDate}::date AND a.scheduled_at::date <= ${calculatedEndDate}::date)
            OR (a.scheduled_at IS NULL AND a.created_at::date >= ${calculatedStartDate}::date AND a.created_at::date <= ${calculatedEndDate}::date)
          )
        LEFT JOIN repcard_customer_attachments att ON c.repcard_customer_id = att.repcard_customer_id::text
        LEFT JOIN repcard_customer_notes n ON c.repcard_customer_id = n.repcard_customer_id
        LEFT JOIN repcard_status_logs sl ON c.repcard_customer_id = sl.repcard_customer_id
        WHERE c.created_at::date >= ${calculatedStartDate}::date
          AND c.created_at::date <= ${calculatedEndDate}::date
          AND c.office_id = ANY(${officeIds}::int[])
      `;
    } else {
      overviewResult = await sql`
        SELECT 
          COUNT(DISTINCT c.repcard_customer_id)::bigint as doors_knocked,
          COUNT(DISTINCT a.repcard_appointment_id)::bigint as appointments_set,
          COUNT(DISTINCT CASE WHEN a.disposition ILIKE '%closed%' THEN a.repcard_appointment_id END)::bigint as sales_closed,
          COUNT(DISTINCT CASE WHEN att.id IS NOT NULL THEN c.repcard_customer_id END)::bigint as customers_with_attachments,
          COUNT(DISTINCT n.repcard_note_id)::bigint as total_notes,
          COUNT(DISTINCT sl.repcard_log_id)::bigint as status_changes
        FROM repcard_customers c
        LEFT JOIN repcard_appointments a ON c.repcard_customer_id = a.repcard_customer_id
          AND (
            (a.scheduled_at IS NOT NULL AND a.scheduled_at::date >= ${calculatedStartDate}::date AND a.scheduled_at::date <= ${calculatedEndDate}::date)
            OR (a.scheduled_at IS NULL AND a.created_at::date >= ${calculatedStartDate}::date AND a.created_at::date <= ${calculatedEndDate}::date)
          )
        LEFT JOIN repcard_customer_attachments att ON c.repcard_customer_id = att.repcard_customer_id::text
        LEFT JOIN repcard_customer_notes n ON c.repcard_customer_id = n.repcard_customer_id
        LEFT JOIN repcard_status_logs sl ON c.repcard_customer_id = sl.repcard_customer_id
        WHERE c.created_at::date >= ${calculatedStartDate}::date
          AND c.created_at::date <= ${calculatedEndDate}::date
      `;
    }
    const overview = getRows(overviewResult)[0] || {};

    // 2. CUSTOMERS WITH FULL DETAILS (if includeDetails)
    let customers: any[] = [];
    if (includeDetails) {
      const customersResult = await sql`
        SELECT 
          c.repcard_customer_id,
          c.id as customer_id,
          c.setter_user_id,
          c.office_id,
          c.name,
          c.email,
          c.phone,
          c.address,
          c.city,
          c.state,
          c.zip,
          c.status,
          c.created_at,
          c.updated_at,
          -- Setter info
          ru_setter.repcard_user_id as setter_repcard_user_id,
          COALESCE(u_setter.name, TRIM(ru_setter.first_name || ' ' || ru_setter.last_name), ru_setter.email) as setter_name,
          ru_setter.email as setter_email,
          -- Office info
          ro.name as office_name,
          -- Custom fields from raw_data
          c.raw_data->>'systemSizeKW' as system_size_kw,
          c.raw_data->>'systemCost' as system_cost,
          c.raw_data->>'financier' as financier,
          c.raw_data->>'offset' as offset,
          -- Counts
          (SELECT COUNT(*) FROM repcard_appointments WHERE repcard_customer_id = c.repcard_customer_id)::bigint as appointment_count,
          (SELECT COUNT(*) FROM repcard_customer_attachments WHERE repcard_customer_id::text = c.repcard_customer_id::text)::bigint as attachment_count,
          (SELECT COUNT(*) FROM repcard_customer_notes WHERE repcard_customer_id = c.repcard_customer_id)::bigint as note_count
        FROM repcard_customers c
        LEFT JOIN repcard_users ru_setter ON c.setter_user_id = ru_setter.repcard_user_id
        LEFT JOIN users u_setter ON u_setter.repcard_user_id = ru_setter.repcard_user_id
        LEFT JOIN repcard_offices ro ON c.office_id = ro.repcard_office_id
        WHERE c.created_at::date >= ${calculatedStartDate}::date
          AND c.created_at::date <= ${calculatedEndDate}::date
          ${addCondition(hasRepcardUserId, sql`AND c.setter_user_id = ${parseInt(repcardUserId!)}`)}
          ${addCondition(hasOfficeIds, sql`AND c.office_id = ANY(${officeIds}::int[])`)}
        ORDER BY c.created_at DESC
        LIMIT 100
      `;
      customers = getRows(customersResult);
    }

    // 3. APPOINTMENTS WITH FULL DETAILS (if includeDetails)
    let appointments: any[] = [];
    if (includeDetails) {
      const appointmentsResult = await sql`
        SELECT 
          a.repcard_appointment_id,
          a.id as appointment_id,
          a.repcard_customer_id,
          a.setter_user_id,
          a.closer_user_id,
          a.office_id,
          a.disposition,
          a.status_category,
          a.scheduled_at,
          a.completed_at,
          a.duration,
          a.notes as appointment_notes,
          a.is_within_48_hours,
          a.has_power_bill,
          a.created_at,
          a.updated_at,
          -- Customer info
          c.name as customer_name,
          c.email as customer_email,
          c.phone as customer_phone,
          -- Setter info
          ru_setter.repcard_user_id as setter_repcard_user_id,
          COALESCE(u_setter.name, TRIM(ru_setter.first_name || ' ' || ru_setter.last_name), ru_setter.email) as setter_name,
          -- Closer info
          ru_closer.repcard_user_id as closer_repcard_user_id,
          COALESCE(u_closer.name, TRIM(ru_closer.first_name || ' ' || ru_closer.last_name), ru_closer.email) as closer_name,
          -- Office info
          ro.name as office_name,
          -- Attachment count
          (SELECT COUNT(*) FROM repcard_appointment_attachments WHERE repcard_appointment_id = a.repcard_appointment_id)::bigint as attachment_count
        FROM repcard_appointments a
        LEFT JOIN repcard_customers c ON a.repcard_customer_id = c.repcard_customer_id
        LEFT JOIN repcard_users ru_setter ON a.setter_user_id = ru_setter.repcard_user_id
        LEFT JOIN users u_setter ON u_setter.repcard_user_id = ru_setter.repcard_user_id
        LEFT JOIN repcard_users ru_closer ON a.closer_user_id = ru_closer.repcard_user_id
        LEFT JOIN users u_closer ON u_closer.repcard_user_id = ru_closer.repcard_user_id
        LEFT JOIN repcard_offices ro ON a.office_id = ro.repcard_office_id
        WHERE (
          (a.scheduled_at IS NOT NULL AND a.scheduled_at::date >= ${calculatedStartDate}::date AND a.scheduled_at::date <= ${calculatedEndDate}::date)
          OR (a.scheduled_at IS NULL AND a.created_at::date >= ${calculatedStartDate}::date AND a.created_at::date <= ${calculatedEndDate}::date)
        )
        ${addCondition(hasRepcardUserId, sql`AND (a.setter_user_id = ${parseInt(repcardUserId!)} OR a.closer_user_id = ${parseInt(repcardUserId!)})`)}
        ${addCondition(hasOfficeIds, sql`AND a.office_id = ANY(${officeIds}::int[])`)}
        ORDER BY COALESCE(a.scheduled_at, a.created_at) DESC
        LIMIT 100
      `;
      appointments = getRows(appointmentsResult);
    }

    // 4. STATUS LOGS (if includeDetails)
    let statusLogs: any[] = [];
    if (includeDetails) {
      const statusLogsResult = await sql`
        SELECT 
          sl.repcard_log_id,
          sl.repcard_customer_id,
          sl.old_status,
          sl.new_status,
          sl.changed_at,
          sl.changed_by_user_id,
          sl.notes,
          c.name as customer_name,
          ru_changed_by.repcard_user_id as changed_by_repcard_user_id,
          COALESCE(u_changed_by.name, TRIM(ru_changed_by.first_name || ' ' || ru_changed_by.last_name), ru_changed_by.email) as changed_by_name
        FROM repcard_status_logs sl
        LEFT JOIN repcard_customers c ON sl.repcard_customer_id = c.repcard_customer_id
        LEFT JOIN repcard_users ru_changed_by ON sl.changed_by_user_id = ru_changed_by.repcard_user_id
        LEFT JOIN users u_changed_by ON u_changed_by.repcard_user_id = ru_changed_by.repcard_user_id
        WHERE sl.changed_at::date >= ${calculatedStartDate}::date
          AND sl.changed_at::date <= ${calculatedEndDate}::date
          ${addCondition(hasRepcardUserId, sql`AND sl.changed_by_user_id = ${parseInt(repcardUserId!)}`)}
        ORDER BY sl.changed_at DESC
        LIMIT 100
      `;
      statusLogs = getRows(statusLogsResult);
    }

    // 5. ATTACHMENTS (if includeDetails)
    let attachments: any[] = [];
    if (includeDetails) {
      const attachmentsResult = await sql`
        SELECT 
          att.id as attachment_id,
          att.repcard_attachment_id,
          att.repcard_customer_id,
          att.attachment_type,
          att.file_name,
          att.file_url,
          att.file_size,
          att.uploaded_by_user_id,
          att.created_at,
          c.name as customer_name,
          ru_uploader.repcard_user_id as uploader_repcard_user_id,
          COALESCE(u_uploader.name, TRIM(ru_uploader.first_name || ' ' || ru_uploader.last_name), ru_uploader.email) as uploader_name,
          'customer' as attachment_source
        FROM repcard_customer_attachments att
        LEFT JOIN repcard_customers c ON att.repcard_customer_id::text = c.repcard_customer_id::text
        LEFT JOIN repcard_users ru_uploader ON att.uploaded_by_user_id = ru_uploader.repcard_user_id
        LEFT JOIN users u_uploader ON u_uploader.repcard_user_id = ru_uploader.repcard_user_id
        WHERE att.created_at::date >= ${calculatedStartDate}::date
          AND att.created_at::date <= ${calculatedEndDate}::date
          ${addCondition(hasRepcardUserId, sql`AND att.uploaded_by_user_id = ${parseInt(repcardUserId!)}`)}
        UNION ALL
        SELECT 
          att.id as attachment_id,
          att.repcard_attachment_id,
          att.repcard_customer_id,
          att.attachment_type,
          att.file_name,
          att.file_url,
          att.file_size,
          att.uploaded_by_user_id,
          att.created_at,
          c.name as customer_name,
          ru_uploader.repcard_user_id as uploader_repcard_user_id,
          COALESCE(u_uploader.name, TRIM(ru_uploader.first_name || ' ' || ru_uploader.last_name), ru_uploader.email) as uploader_name,
          'appointment' as attachment_source
        FROM repcard_appointment_attachments att
        LEFT JOIN repcard_customers c ON att.repcard_customer_id::text = c.repcard_customer_id::text
        LEFT JOIN repcard_users ru_uploader ON att.uploaded_by_user_id = ru_uploader.repcard_user_id
        LEFT JOIN users u_uploader ON u_uploader.repcard_user_id = ru_uploader.repcard_user_id
        WHERE att.created_at::date >= ${calculatedStartDate}::date
          AND att.created_at::date <= ${calculatedEndDate}::date
          ${addCondition(hasRepcardUserId, sql`AND att.uploaded_by_user_id = ${parseInt(repcardUserId!)}`)}
        ORDER BY created_at DESC
        LIMIT 100
      `;
      attachments = getRows(attachmentsResult);
    }

    // 6. CUSTOMER NOTES (if includeDetails)
    let notes: any[] = [];
    if (includeDetails) {
      const notesResult = await sql`
        SELECT 
          n.repcard_note_id,
          n.repcard_customer_id,
          n.repcard_user_id,
          n.note,
          n.created_at,
          n.updated_at,
          c.name as customer_name,
          ru_author.repcard_user_id as author_repcard_user_id,
          COALESCE(u_author.name, TRIM(ru_author.first_name || ' ' || ru_author.last_name), ru_author.email) as author_name
        FROM repcard_customer_notes n
        LEFT JOIN repcard_customers c ON n.repcard_customer_id = c.repcard_customer_id
        LEFT JOIN repcard_users ru_author ON n.repcard_user_id = ru_author.repcard_user_id
        LEFT JOIN users u_author ON u_author.repcard_user_id = ru_author.repcard_user_id
        WHERE n.created_at::date >= ${calculatedStartDate}::date
          AND n.created_at::date <= ${calculatedEndDate}::date
          ${addCondition(hasRepcardUserId, sql`AND n.repcard_user_id = ${parseInt(repcardUserId!)}`)}
        ORDER BY n.created_at DESC
        LIMIT 100
      `;
      notes = getRows(notesResult);
    }

    // 7. USER PERFORMANCE METRICS
    const userPerformanceResult = await sql`
      SELECT 
        ru.repcard_user_id,
          COALESCE(u.name, TRIM(ru.first_name || ' ' || ru.last_name), ru.email, 'RepCard User ' || ru.repcard_user_id::text) as name,
          ru.email,
          COALESCE(u.sales_office[1], ru.office_name) as office,
          ru.office_id,
          ru.team_name,
        -- Metrics
        COUNT(DISTINCT c.repcard_customer_id)::bigint as doors_knocked,
        COUNT(DISTINCT a.repcard_appointment_id)::bigint as appointments_set,
        COUNT(DISTINCT CASE WHEN a.closer_user_id = ru.repcard_user_id THEN a.repcard_appointment_id END)::bigint as appointments_closed,
        COUNT(DISTINCT CASE WHEN a.disposition ILIKE '%closed%' AND a.closer_user_id = ru.repcard_user_id THEN a.repcard_appointment_id END)::bigint as sales_closed,
        COUNT(DISTINCT CASE WHEN att.id IS NOT NULL THEN c.repcard_customer_id END)::bigint as customers_with_attachments,
        COUNT(DISTINCT n.repcard_note_id)::bigint as notes_written,
        COUNT(DISTINCT CASE WHEN a.is_within_48_hours = TRUE THEN a.repcard_appointment_id END)::bigint as appointments_within_48h
      FROM repcard_users ru
      LEFT JOIN repcard_customers c ON ru.repcard_user_id = c.setter_user_id
        AND c.created_at::date >= ${calculatedStartDate}::date
        AND c.created_at::date <= ${calculatedEndDate}::date
      LEFT JOIN repcard_appointments a ON (
        a.setter_user_id = ru.repcard_user_id
        OR a.closer_user_id = ru.repcard_user_id
      )
        AND (
          (a.scheduled_at IS NOT NULL AND a.scheduled_at::date >= ${calculatedStartDate}::date AND a.scheduled_at::date <= ${calculatedEndDate}::date)
          OR (a.scheduled_at IS NULL AND a.created_at::date >= ${calculatedStartDate}::date AND a.created_at::date <= ${calculatedEndDate}::date)
        )
      LEFT JOIN repcard_customer_attachments att ON c.repcard_customer_id = att.repcard_customer_id::text
      LEFT JOIN repcard_customer_notes n ON n.repcard_user_id = ru.repcard_user_id
        AND n.created_at::date >= ${calculatedStartDate}::date
        AND n.created_at::date <= ${calculatedEndDate}::date
      LEFT JOIN users u ON u.repcard_user_id = ru.repcard_user_id
        WHERE ru.status = 1
        ${addCondition(hasRepcardUserId, sql`AND ru.repcard_user_id = ${parseInt(repcardUserId!)}`)}
        ${addCondition(hasOfficeIds, sql`AND ru.office_id = ANY(${officeIds}::int[])`)}
      GROUP BY ru.repcard_user_id, ru.first_name, ru.last_name, ru.email, ru.office_name, ru.office_id, u.name, u.sales_office
      HAVING COUNT(DISTINCT c.repcard_customer_id) > 0
        OR COUNT(DISTINCT a.repcard_appointment_id) > 0
      ORDER BY doors_knocked DESC, appointments_set DESC
      LIMIT 50
    `;
    const userPerformance = getRows(userPerformanceResult);

    // 8. OFFICE BREAKDOWN
    const officeBreakdownResult = await sql`
      SELECT 
        ro.repcard_office_id,
        ro.name as office_name,
        COUNT(DISTINCT c.repcard_customer_id)::bigint as doors_knocked,
        COUNT(DISTINCT a.repcard_appointment_id)::bigint as appointments_set,
        COUNT(DISTINCT CASE WHEN a.disposition ILIKE '%closed%' THEN a.repcard_appointment_id END)::bigint as sales_closed,
        COUNT(DISTINCT ru.repcard_user_id)::bigint as active_reps
      FROM repcard_offices ro
      LEFT JOIN repcard_customers c ON ro.repcard_office_id = c.office_id
        AND c.created_at::date >= ${calculatedStartDate}::date
        AND c.created_at::date <= ${calculatedEndDate}::date
      LEFT JOIN repcard_appointments a ON c.repcard_customer_id = a.repcard_customer_id
        AND (
          (a.scheduled_at IS NOT NULL AND a.scheduled_at::date >= ${calculatedStartDate}::date AND a.scheduled_at::date <= ${calculatedEndDate}::date)
          OR (a.scheduled_at IS NULL AND a.created_at::date >= ${calculatedStartDate}::date AND a.created_at::date <= ${calculatedEndDate}::date)
        )
      LEFT JOIN repcard_users ru ON ru.office_id = ro.repcard_office_id AND ru.status = 1
      GROUP BY ro.repcard_office_id, ro.name
      HAVING COUNT(DISTINCT c.repcard_customer_id) > 0
      ORDER BY doors_knocked DESC
    `;
    const officeBreakdown = getRows(officeBreakdownResult);

    return NextResponse.json({
      success: true,
      dateRange: {
        startDate: calculatedStartDate,
        endDate: calculatedEndDate
      },
      overview: {
        doorsKnocked: Number(overview.doors_knocked || 0),
        appointmentsSet: Number(overview.appointments_set || 0),
        salesClosed: Number(overview.sales_closed || 0),
        customersWithAttachments: Number(overview.customers_with_attachments || 0),
        totalNotes: Number(overview.total_notes || 0),
        statusChanges: Number(overview.status_changes || 0),
        conversionRate: overview.doors_knocked > 0 
          ? Math.round((Number(overview.appointments_set || 0) / Number(overview.doors_knocked)) * 1000) / 10 
          : 0,
        closeRate: overview.appointments_set > 0
          ? Math.round((Number(overview.sales_closed || 0) / Number(overview.appointments_set)) * 1000) / 10
          : 0
      },
      userPerformance: userPerformance.map((u: any) => ({
        repcardUserId: u.repcard_user_id,
        name: u.name || `RepCard User ${u.repcard_user_id}`,
        email: u.email || '',
        office: u.office || null,
        officeId: u.office_id || null,
        team: u.team_name || null,
        doorsKnocked: Number(u.doors_knocked || 0),
        appointmentsSet: Number(u.appointments_set || 0),
        appointmentsClosed: Number(u.appointments_closed || 0),
        salesClosed: Number(u.sales_closed || 0),
        customersWithAttachments: Number(u.customers_with_attachments || 0),
        notesWritten: Number(u.notes_written || 0),
        appointmentsWithin48h: Number(u.appointments_within_48h || 0),
        conversionRate: u.doors_knocked > 0 
          ? Math.round((Number(u.appointments_set || 0) / Number(u.doors_knocked)) * 1000) / 10 
          : 0,
        closeRate: u.appointments_set > 0
          ? Math.round((Number(u.sales_closed || 0) / Number(u.appointments_set)) * 1000) / 10
          : 0
      })),
      officeBreakdown: officeBreakdown.map((o: any) => ({
        officeId: o.repcard_office_id,
        officeName: o.office_name,
        doorsKnocked: Number(o.doors_knocked || 0),
        appointmentsSet: Number(o.appointments_set || 0),
        salesClosed: Number(o.sales_closed || 0),
        activeReps: Number(o.active_reps || 0)
      })),
      ...(includeDetails && {
        customers: customers.map((c: any) => ({
          repcardCustomerId: c.repcard_customer_id,
          customerId: c.customer_id,
          name: c.name,
          email: c.email,
          phone: c.phone,
          address: c.address,
          city: c.city,
          state: c.state,
          zip: c.zip,
          status: c.status,
          createdAt: c.created_at,
          updatedAt: c.updated_at,
          setter: {
            repcardUserId: c.setter_repcard_user_id,
            name: c.setter_name,
            email: c.setter_email
          },
          office: {
            officeId: c.office_id,
            officeName: c.office_name
          },
          customFields: {
            systemSizeKW: c.system_size_kw,
            systemCost: c.system_cost,
            financier: c.financier,
            offset: c.offset
          },
          counts: {
            appointments: Number(c.appointment_count || 0),
            attachments: Number(c.attachment_count || 0),
            notes: Number(c.note_count || 0)
          }
        })),
        appointments: appointments.map((a: any) => ({
          repcardAppointmentId: a.repcard_appointment_id,
          appointmentId: a.appointment_id,
          repcardCustomerId: a.repcard_customer_id,
          customer: {
            name: a.customer_name,
            email: a.customer_email,
            phone: a.customer_phone
          },
          setter: {
            repcardUserId: a.setter_repcard_user_id,
            name: a.setter_name
          },
          closer: {
            repcardUserId: a.closer_repcard_user_id,
            name: a.closer_name
          },
          office: {
            officeId: a.office_id,
            officeName: a.office_name
          },
          disposition: a.disposition,
          statusCategory: a.status_category,
          scheduledAt: a.scheduled_at,
          completedAt: a.completed_at,
          duration: a.duration,
          notes: a.appointment_notes,
          isWithin48Hours: a.is_within_48_hours,
          hasPowerBill: a.has_power_bill,
          createdAt: a.created_at,
          updatedAt: a.updated_at,
          attachmentCount: Number(a.attachment_count || 0)
        })),
        statusLogs: statusLogs.map((sl: any) => ({
          repcardLogId: sl.repcard_log_id,
          repcardCustomerId: sl.repcard_customer_id,
          customerName: sl.customer_name,
          oldStatus: sl.old_status,
          newStatus: sl.new_status,
          changedAt: sl.changed_at,
          changedBy: {
            repcardUserId: sl.changed_by_repcard_user_id,
            name: sl.changed_by_name
          },
          notes: sl.notes
        })),
        attachments: attachments.map((att: any) => ({
          attachmentId: att.attachment_id,
          repcardAttachmentId: att.repcard_attachment_id,
          repcardCustomerId: att.repcard_customer_id,
          customerName: att.customer_name,
          attachmentType: att.attachment_type,
          fileName: att.file_name,
          fileUrl: att.file_url,
          fileSize: att.file_size,
          uploadedBy: {
            repcardUserId: att.uploader_repcard_user_id,
            name: att.uploader_name
          },
          createdAt: att.created_at,
          source: att.attachment_source
        })),
        notes: notes.map((n: any) => ({
          repcardNoteId: n.repcard_note_id,
          repcardCustomerId: n.repcard_customer_id,
          customerName: n.customer_name,
          repcardUserId: n.repcard_user_id,
          author: {
            repcardUserId: n.author_repcard_user_id,
            name: n.author_name
          },
          note: n.note,
          createdAt: n.created_at,
          updatedAt: n.updated_at
        }))
      })
    });

  } catch (error) {
    console.error('[RepCard Comprehensive Stats] Error:', error);
    return NextResponse.json(
      {
        success: false,
        error: error instanceof Error ? error.message : 'Unknown error'
      },
      { status: 500 }
    );
  }
}

