import { NextRequest, NextResponse } from 'next/server';
import { getServerSession } from 'next-auth';
import { authOptions } from '@/lib/auth/config';
import { sql } from '@/lib/db/client';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';

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
    const type = searchParams.get('type') || 'customers';
    const userId = searchParams.get('userId');
    const repcardUserId = searchParams.get('repcardUserId');
    const officeId = searchParams.get('officeId');
    const customerId = searchParams.get('customerId');
    const startDate = searchParams.get('startDate');
    const endDate = searchParams.get('endDate');
    const limit = Math.min(parseInt(searchParams.get('limit') || '100'), 1000);
    const page = Math.max(parseInt(searchParams.get('page') || '1'), 1);
    const offset = (page - 1) * limit;

    // Get RepCard user ID if userId provided
    let actualRepcardUserId = repcardUserId;
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
            id,
            repcard_appointment_id,
            customer_id,
            repcard_customer_id,
            setter_user_id,
            closer_user_id,
            disposition,
            scheduled_at,
            completed_at,
            duration,
            notes,
            created_at,
            updated_at,
            synced_at
          FROM repcard_appointments
          WHERE 1=1
        `;

        if (actualRepcardUserId) {
          query = sql`${query} AND (setter_user_id = ${actualRepcardUserId} OR closer_user_id = ${actualRepcardUserId})`;
        }
        if (customerId) {
          query = sql`${query} AND repcard_customer_id = ${parseInt(customerId)}`;
        }
        if (startDate) {
          query = sql`${query} AND scheduled_at >= ${startDate}::timestamp`;
        }
        if (endDate) {
          query = sql`${query} AND scheduled_at <= (${endDate}::timestamp + INTERVAL '1 day')`;
        }

        const countResult = await sql`
          SELECT COUNT(*) as count FROM (${query}) as subquery
        `;
        total = parseInt(Array.from(countResult)[0]?.count || '0');

        query = sql`${query} ORDER BY scheduled_at DESC LIMIT ${limit} OFFSET ${offset}`;
        const result = await query;
        data = Array.from(result);
        break;
      }

      case 'attachments': {
        const customerAttachments = await sql`
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
            ${actualRepcardUserId ? sql`AND uploaded_by_user_id = ${actualRepcardUserId}` : sql``}
            ${customerId ? sql`AND repcard_customer_id = ${parseInt(customerId)}` : sql``}
            ${startDate ? sql`AND created_at >= ${startDate}::timestamp` : sql``}
            ${endDate ? sql`AND created_at <= (${endDate}::timestamp + INTERVAL '1 day')` : sql``}
          ORDER BY created_at DESC
          LIMIT ${limit}
          OFFSET ${offset}
        `;

        const appointmentAttachments = await sql`
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
            ${actualRepcardUserId ? sql`AND uploaded_by_user_id = ${actualRepcardUserId}` : sql``}
            ${customerId ? sql`AND repcard_customer_id = ${parseInt(customerId)}` : sql``}
            ${startDate ? sql`AND created_at >= ${startDate}::timestamp` : sql``}
            ${endDate ? sql`AND created_at <= (${endDate}::timestamp + INTERVAL '1 day')` : sql``}
          ORDER BY created_at DESC
          LIMIT ${limit}
          OFFSET ${offset}
        `;

        data = [
          ...Array.from(customerAttachments).map((a: any) => ({ ...a, source: 'customer' })),
          ...Array.from(appointmentAttachments).map((a: any) => ({ ...a, source: 'appointment' }))
        ];
        total = data.length; // Simplified - could be more accurate
        break;
      }

      case 'users': {
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
            ${actualRepcardUserId ? sql`AND repcard_user_id = ${parseInt(actualRepcardUserId)}` : sql``}
            ${officeId ? sql`AND office_id = ${parseInt(officeId)}` : sql``}
            ${startDate ? sql`AND created_at >= ${startDate}::timestamp` : sql``}
            ${endDate ? sql`AND created_at <= (${endDate}::timestamp + INTERVAL '1 day')` : sql``}
          ORDER BY last_name, first_name
          LIMIT ${limit}
          OFFSET ${offset}
        `;
        data = Array.from(result);
        
        const countResult = await sql`
          SELECT COUNT(*) as count FROM repcard_users
          WHERE 1=1
            ${actualRepcardUserId ? sql`AND repcard_user_id = ${parseInt(actualRepcardUserId)}` : sql``}
            ${officeId ? sql`AND office_id = ${parseInt(officeId)}` : sql``}
            ${startDate ? sql`AND created_at >= ${startDate}::timestamp` : sql``}
            ${endDate ? sql`AND created_at <= (${endDate}::timestamp + INTERVAL '1 day')` : sql``}
        `;
        total = parseInt(Array.from(countResult)[0]?.count || '0');
        break;
      }

      case 'offices': {
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
            ${officeId ? sql`AND repcard_office_id = ${parseInt(officeId)}` : sql``}
          ORDER BY name
          LIMIT ${limit}
          OFFSET ${offset}
        `;
        data = Array.from(result);
        
        const countResult = await sql`
          SELECT COUNT(*) as count FROM repcard_offices
          WHERE 1=1
            ${officeId ? sql`AND repcard_office_id = ${parseInt(officeId)}` : sql``}
        `;
        total = parseInt(Array.from(countResult)[0]?.count || '0');
        break;
      }

      case 'status_logs': {
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
            ${actualRepcardUserId ? sql`AND changed_by_user_id = ${actualRepcardUserId}` : sql``}
            ${customerId ? sql`AND repcard_customer_id = ${parseInt(customerId)}` : sql``}
            ${startDate ? sql`AND changed_at >= ${startDate}::timestamp` : sql``}
            ${endDate ? sql`AND changed_at <= (${endDate}::timestamp + INTERVAL '1 day')` : sql``}
          ORDER BY changed_at DESC
          LIMIT ${limit}
          OFFSET ${offset}
        `;
        data = Array.from(result);
        
        const countResult = await sql`
          SELECT COUNT(*) as count FROM repcard_status_logs
          WHERE 1=1
            ${actualRepcardUserId ? sql`AND changed_by_user_id = ${actualRepcardUserId}` : sql``}
            ${customerId ? sql`AND repcard_customer_id = ${parseInt(customerId)}` : sql``}
            ${startDate ? sql`AND changed_at >= ${startDate}::timestamp` : sql``}
            ${endDate ? sql`AND changed_at <= (${endDate}::timestamp + INTERVAL '1 day')` : sql``}
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
    logError('repcard-data', error as Error, { requestId });
    logApiResponse('GET', path, duration, { status: 500, cached: false, requestId });
    return NextResponse.json(
      {
        error: 'Internal server error',
        message: error instanceof Error ? error.message : 'Unknown error'
      },
      { status: 500 }
    );
  }
}

