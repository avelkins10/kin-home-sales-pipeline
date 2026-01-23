import { NextRequest, NextResponse } from 'next/server';
import { getServerSession } from 'next-auth';
import { authOptions } from '@/lib/auth/config';
import { sql } from '@/lib/db/client';
import { logApiRequest, logApiResponse, logError, logInfo } from '@/lib/logging/logger';

export const runtime = 'nodejs';

/**
 * GET /api/repcard/data
 * Query synced RepCard data from database
 * 
 * Query params:
 * - type: 'customers' | 'appointments' | 'attachments' | 'users' | 'offices' | 'status_logs'
 * - userId: Filter by user ID (dashboard user ID)
 * - repcardUserId: Filter by RepCard user ID
 * - officeId: Filter by office ID
 * - customerId: Filter by customer ID
 * - startDate: YYYY-MM-DD
 * - endDate: YYYY-MM-DD
 * - limit: Number of records (default: 100, max: 1000)
 * - page: Page number (default: 1)
 */
export async function GET(request: NextRequest) {
  const start = Date.now();
  const path = new URL(request.url).pathname;
  const requestId = request.headers.get('x-request-id') || `req-${Date.now()}`;

  // Declare variables at function scope for error handling
  let type: string | undefined;
  let userId: string | null = null;
  let repcardUserId: string | null = null;
  let officeId: string | null = null;
  let customerId: string | null = null;
  let startDate: string | null = null;
  let endDate: string | null = null;
  let limit = 100;
  let page = 1;
  let offset = 0;
  let actualRepcardUserId: string | null = null;

  try {
    logApiRequest('GET', path, { endpoint: 'repcard-data', requestId });

    // Authentication
    const session = await getServerSession(authOptions);
    if (!session) {
      const duration = Date.now() - start;
      logApiResponse('GET', path, duration, { status: 401, cached: false, requestId });
      return NextResponse.json({ error: 'Unauthorized' }, { status: 401 });
    }

    const { searchParams } = new URL(request.url);
    type = searchParams.get('type') || 'customers';
    userId = searchParams.get('userId');
    repcardUserId = searchParams.get('repcardUserId');
    officeId = searchParams.get('officeId');
    customerId = searchParams.get('customerId');
    startDate = searchParams.get('startDate');
    endDate = searchParams.get('endDate');
    limit = Math.min(parseInt(searchParams.get('limit') || '100'), 1000);
    page = Math.max(parseInt(searchParams.get('page') || '1'), 1);
    offset = (page - 1) * limit;

    // Get RepCard user ID if userId provided
    actualRepcardUserId = repcardUserId;
    if (userId && !repcardUserId) {
      const userResult = await sql`
        SELECT repcard_user_id FROM users WHERE id = ${userId}
      `;
      const userRows = Array.from(userResult);
      if (userRows.length > 0 && userRows[0].repcard_user_id) {
        actualRepcardUserId = userRows[0].repcard_user_id;
      }
    }

    let data: any[] = [];
    let total = 0;

    // Log query parameters for debugging
    logInfo('repcard-data', {
      requestId,
      type,
      filters: {
        userId,
        repcardUserId: actualRepcardUserId,
        officeId,
        customerId,
        startDate,
        endDate
      },
      pagination: { limit, page, offset }
    });

    switch (type) {
      case 'customers': {
        let query = sql`
          SELECT 
            id,
            repcard_customer_id,
            setter_user_id,
            office_id,
            name,
            email,
            phone,
            address,
            city,
            state,
            zip,
            status,
            created_at,
            updated_at,
            synced_at
          FROM repcard_customers
          WHERE 1=1
        `;

        if (actualRepcardUserId) {
          query = sql`${query} AND setter_user_id = ${actualRepcardUserId}`;
        }
        if (officeId) {
          query = sql`${query} AND office_id = ${parseInt(officeId)}`;
        }
        if (startDate) {
          query = sql`${query} AND created_at >= ${startDate}::timestamp`;
        }
        if (endDate) {
          query = sql`${query} AND created_at <= (${endDate}::timestamp + INTERVAL '1 day')`;
        }

        const countResult = await sql`
          SELECT COUNT(*) as count FROM (${query}) as subquery
        `;
        total = parseInt(Array.from(countResult)[0]?.count || '0');

        query = sql`${query} ORDER BY created_at DESC LIMIT ${limit} OFFSET ${offset}`;
        const result = await query;
        data = Array.from(result);
        break;
      }

      case 'appointments': {
        let query = sql`
          SELECT 
            a.id,
            a.repcard_appointment_id,
            a.customer_id,
            a.repcard_customer_id,
            a.setter_user_id,
            a.closer_user_id,
            a.disposition,
            a.status_category,
            a.scheduled_at,
            a.completed_at,
            a.duration,
            a.notes,
            a.is_within_48_hours,
            a.has_power_bill,
            a.is_reschedule,
            a.created_at,
            a.updated_at,
            a.synced_at,
            c.name as customer_name,
            c.phone as customer_phone,
            c.email as customer_email,
            c.address as customer_address
          FROM repcard_appointments a
          LEFT JOIN repcard_customers c ON c.repcard_customer_id::text = a.repcard_customer_id::text
          WHERE 1=1
        `;

        if (actualRepcardUserId) {
          query = sql`${query} AND (a.setter_user_id::text = ${actualRepcardUserId}::text OR a.closer_user_id::text = ${actualRepcardUserId}::text)`;
        }
        if (customerId) {
          query = sql`${query} AND a.repcard_customer_id::text = ${customerId}::text`;
        }
        if (startDate) {
          query = sql`${query} AND a.scheduled_at >= ${startDate}::timestamp`;
        }
        if (endDate) {
          query = sql`${query} AND a.scheduled_at <= (${endDate}::timestamp + INTERVAL '1 day')`;
        }

        const countResult = await sql`
          SELECT COUNT(*) as count FROM (${query}) as subquery
        `;
        total = parseInt(Array.from(countResult)[0]?.count || '0');

        query = sql`${query} ORDER BY a.scheduled_at DESC LIMIT ${limit} OFFSET ${offset}`;
        const result = await query;
        data = Array.from(result);
        break;
      }

      case 'attachments': {
        // Build customer attachments query using parameterized sql template tags
        const userIdNum = actualRepcardUserId ? (typeof actualRepcardUserId === 'string' ? parseInt(actualRepcardUserId) : actualRepcardUserId) : null;
        const parsedCustomerId = customerId ? parseInt(customerId) : null;
        
        // Include conditions directly in queries to avoid nested fragment parameter binding issues
        const customerAttachmentsResult = await sql`
          SELECT
            id,
            repcard_attachment_id,
            customer_id,
            repcard_customer_id,
            attachment_type,
            file_name,
            file_url,
            file_size,
            uploaded_by_user_id,
            created_at,
            updated_at
          FROM repcard_customer_attachments
          WHERE 1=1
          ${userIdNum ? sql`AND uploaded_by_user_id = ${userIdNum}` : ''}
          ${parsedCustomerId ? sql`AND repcard_customer_id = ${parsedCustomerId}` : ''}
          ${startDate ? sql`AND created_at >= ${startDate}::timestamp` : ''}
          ${endDate ? sql`AND created_at <= (${endDate}::timestamp + INTERVAL '1 day')` : ''}
          ORDER BY created_at DESC
          LIMIT ${limit}
          OFFSET ${offset}
        `;

        // Build appointment attachments query using parameterized sql template tags
        const appointmentAttachmentsResult = await sql`
          SELECT
            id,
            repcard_attachment_id,
            appointment_id,
            repcard_appointment_id,
            customer_id,
            repcard_customer_id,
            attachment_type,
            file_name,
            file_url,
            file_size,
            uploaded_by_user_id,
            created_at,
            updated_at
          FROM repcard_appointment_attachments
          WHERE 1=1
          ${userIdNum ? sql`AND uploaded_by_user_id = ${userIdNum}` : ''}
          ${parsedCustomerId ? sql`AND repcard_customer_id = ${parsedCustomerId}` : ''}
          ${startDate ? sql`AND created_at >= ${startDate}::timestamp` : ''}
          ${endDate ? sql`AND created_at <= (${endDate}::timestamp + INTERVAL '1 day')` : ''}
          ORDER BY created_at DESC
          LIMIT ${limit}
          OFFSET ${offset}
        `;

        data = [
          ...Array.from(customerAttachmentsResult).map((a: any) => ({ ...a, source: 'customer' })),
          ...Array.from(appointmentAttachmentsResult).map((a: any) => ({ ...a, source: 'appointment' }))
        ];
        total = data.length; // Simplified - could be more accurate
        break;
      }

      case 'users': {
        // Build query using parameterized sql template tags
        const parsedRepcardUserId = actualRepcardUserId ? parseInt(actualRepcardUserId) : null;
        const parsedOfficeId = officeId ? parseInt(officeId) : null;
        
        // Include conditions directly in query to avoid nested fragment parameter binding issues
        const result = await sql`
          SELECT
            id,
            repcard_user_id,
            company_id,
            office_id,
            first_name,
            last_name,
            email,
            phone,
            username,
            role,
            status,
            office_name,
            team,
            job_title,
            profile_image,
            rating,
            bio,
            badge_id,
            created_at,
            updated_at,
            synced_at
          FROM repcard_users
          WHERE 1=1
          ${parsedRepcardUserId ? sql`AND repcard_user_id = ${parsedRepcardUserId}` : ''}
          ${parsedOfficeId ? sql`AND office_id = ${parsedOfficeId}` : ''}
          ${startDate ? sql`AND created_at >= ${startDate}::timestamp` : ''}
          ${endDate ? sql`AND created_at <= (${endDate}::timestamp + INTERVAL '1 day')` : ''}
          ORDER BY last_name, first_name
          LIMIT ${limit}
          OFFSET ${offset}
        `;
        data = Array.from(result);

        // Build count query using parameterized sql template tags
        const countResult = await sql`
          SELECT COUNT(*) as count FROM repcard_users
          WHERE 1=1
          ${parsedRepcardUserId ? sql`AND repcard_user_id = ${parsedRepcardUserId}` : ''}
          ${parsedOfficeId ? sql`AND office_id = ${parsedOfficeId}` : ''}
          ${startDate ? sql`AND created_at >= ${startDate}::timestamp` : ''}
          ${endDate ? sql`AND created_at <= (${endDate}::timestamp + INTERVAL '1 day')` : ''}
        `;
        total = parseInt(Array.from(countResult)[0]?.count || '0');
        break;
      }

      case 'offices': {
        // Build query using parameterized sql template tags
        const parsedOfficeId = officeId ? parseInt(officeId) : null;
        
        // Include conditions directly in queries to avoid nested fragment parameter binding issues
        const result = await sql`
          SELECT
            id,
            repcard_office_id,
            company_id,
            name,
            address,
            city,
            state,
            zip_code,
            created_at,
            updated_at,
            synced_at
          FROM repcard_offices
          WHERE 1=1
          ${parsedOfficeId ? sql`AND repcard_office_id = ${parsedOfficeId}` : ''}
          ORDER BY name
          LIMIT ${limit}
          OFFSET ${offset}
        `;
        data = Array.from(result);

        // Build count query using parameterized sql template tags
        const countResult = await sql`
          SELECT COUNT(*) as count FROM repcard_offices
          WHERE 1=1
          ${parsedOfficeId ? sql`AND repcard_office_id = ${parsedOfficeId}` : ''}
        `;
        total = parseInt(Array.from(countResult)[0]?.count || '0');
        break;
      }

      case 'status_logs': {
        // Build query using parameterized sql template tags
        const userIdNum = actualRepcardUserId ? (typeof actualRepcardUserId === 'string' ? parseInt(actualRepcardUserId) : actualRepcardUserId) : null;
        const parsedCustomerId = customerId ? parseInt(customerId) : null;
        
        // Include conditions directly in queries to avoid nested fragment parameter binding issues
        const result = await sql`
          SELECT
            id,
            repcard_log_id,
            customer_id,
            repcard_customer_id,
            old_status,
            new_status,
            changed_at,
            changed_by_user_id,
            synced_at
          FROM repcard_status_logs
          WHERE 1=1
          ${userIdNum ? sql`AND changed_by_user_id = ${userIdNum}` : ''}
          ${parsedCustomerId ? sql`AND repcard_customer_id = ${parsedCustomerId}` : ''}
          ${startDate ? sql`AND changed_at >= ${startDate}::timestamp` : ''}
          ${endDate ? sql`AND changed_at <= (${endDate}::timestamp + INTERVAL '1 day')` : ''}
          ORDER BY changed_at DESC
          LIMIT ${limit}
          OFFSET ${offset}
        `;
        data = Array.from(result);

        // Build count query using parameterized sql template tags
        const countResult = await sql`
          SELECT COUNT(*) as count FROM repcard_status_logs
          WHERE 1=1
          ${userIdNum ? sql`AND changed_by_user_id = ${userIdNum}` : ''}
          ${parsedCustomerId ? sql`AND repcard_customer_id = ${parsedCustomerId}` : ''}
          ${startDate ? sql`AND changed_at >= ${startDate}::timestamp` : ''}
          ${endDate ? sql`AND changed_at <= (${endDate}::timestamp + INTERVAL '1 day')` : ''}
        `;
        total = parseInt(Array.from(countResult)[0]?.count || '0');
        break;
      }

      default:
        return NextResponse.json(
          { error: `Invalid type. Must be one of: customers, appointments, attachments, users, offices, status_logs` },
          { status: 400 }
        );
    }

    const duration = Date.now() - start;
    logApiResponse('GET', path, duration, { status: 200, cached: false, requestId });

    return NextResponse.json({
      type,
      data,
      pagination: {
        page,
        limit,
        total,
        totalPages: Math.ceil(total / limit)
      },
      filters: {
        userId,
        repcardUserId: actualRepcardUserId,
        officeId,
        customerId,
        startDate,
        endDate
      }
    });

  } catch (error) {
    const duration = Date.now() - start;
    const errorDetails = error instanceof Error ? {
      message: error.message,
      stack: error.stack,
      name: error.name
    } : { error: String(error) };
    
    // Safely access variables that might not be assigned yet
    const queryParams: any = {};
    try {
      queryParams.type = type;
      queryParams.userId = userId;
      queryParams.repcardUserId = actualRepcardUserId;
      queryParams.officeId = officeId;
      queryParams.customerId = customerId;
      queryParams.startDate = startDate;
      queryParams.endDate = endDate;
      queryParams.limit = limit;
      queryParams.page = page;
    } catch (e) {
      // If accessing variables fails, just log that
      queryParams.error = 'Could not access query parameters';
    }
    
    logError('repcard-data', error as Error, { 
      requestId,
      errorDetails,
      queryParams
    });
    logApiResponse('GET', path, duration, { status: 500, cached: false, requestId });
    return NextResponse.json(
      {
        error: 'Internal server error',
        message: error instanceof Error ? error.message : 'Unknown error',
        requestId
      },
      { status: 500 }
    );
  }
}

