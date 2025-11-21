import { NextRequest, NextResponse } from 'next/server';
import { getServerSession } from 'next-auth';
import { authOptions } from '@/lib/auth/config';
import { sql } from '@/lib/db/client';

export const runtime = 'nodejs';

/**
 * Diagnostic endpoint to check attachment attribution
 */
export async function GET(request: NextRequest) {
  try {
    const session = await getServerSession(authOptions);
    if (!session) {
      return NextResponse.json({ error: 'Unauthorized' }, { status: 401 });
    }

    // Check if user is admin
    if (session.user.role !== 'super_admin') {
      return NextResponse.json({ error: 'Forbidden' }, { status: 403 });
    }

    // Get attachment counts
    const customerAttachmentsResult = await sql`
      SELECT 
        COUNT(*)::bigint as total,
        COUNT(DISTINCT repcard_customer_id)::bigint as unique_customers,
        COUNT(DISTINCT CASE WHEN customer_id IS NULL THEN repcard_customer_id END)::bigint as missing_customer_id,
        MIN(created_at) as earliest,
        MAX(created_at) as latest
      FROM repcard_customer_attachments
    `;
    const customerAttachments = customerAttachmentsResult.rows?.[0] || customerAttachmentsResult[0];

    const appointmentAttachmentsResult = await sql`
      SELECT 
        COUNT(*)::bigint as total,
        COUNT(DISTINCT repcard_appointment_id)::bigint as unique_appointments,
        COUNT(DISTINCT repcard_customer_id)::bigint as unique_customers,
        COUNT(DISTINCT CASE WHEN appointment_id IS NULL THEN repcard_appointment_id END)::bigint as missing_appointment_id,
        COUNT(DISTINCT CASE WHEN customer_id IS NULL THEN repcard_customer_id END)::bigint as missing_customer_id,
        MIN(created_at) as earliest,
        MAX(created_at) as latest
      FROM repcard_appointment_attachments
    `;
    const appointmentAttachments = appointmentAttachmentsResult.rows?.[0] || appointmentAttachmentsResult[0];

    // Check sample attachments with attribution
    const sampleCustomerAttachments = await sql`
      SELECT 
        att.repcard_attachment_id,
        att.repcard_customer_id,
        att.customer_id,
        c.name as customer_name,
        att.file_name,
        att.file_url,
        att.created_at,
        CASE WHEN c.id IS NULL THEN 'MISSING CUSTOMER' ELSE 'OK' END as attribution_status
      FROM repcard_customer_attachments att
      LEFT JOIN repcard_customers c ON att.repcard_customer_id::text = c.repcard_customer_id::text
      ORDER BY att.created_at DESC
      LIMIT 10
    `;

    const sampleAppointmentAttachments = await sql`
      SELECT 
        att.repcard_attachment_id,
        att.repcard_appointment_id,
        att.appointment_id,
        att.repcard_customer_id,
        att.customer_id,
        a.repcard_customer_id as appointment_customer_id,
        c.name as customer_name,
        att.file_name,
        att.file_url,
        att.created_at,
        CASE 
          WHEN a.id IS NULL THEN 'MISSING APPOINTMENT'
          WHEN c.id IS NULL THEN 'MISSING CUSTOMER'
          ELSE 'OK'
        END as attribution_status
      FROM repcard_appointment_attachments att
      LEFT JOIN repcard_appointments a ON att.repcard_appointment_id::text = a.repcard_appointment_id::text
      LEFT JOIN repcard_customers c ON COALESCE(att.repcard_customer_id::text, a.repcard_customer_id::text) = c.repcard_customer_id::text
      ORDER BY att.created_at DESC
      LIMIT 10
    `;

    // Check customers with attachments
    const customersWithAttachments = await sql`
      SELECT 
        COUNT(DISTINCT c.repcard_customer_id)::bigint as customers_with_customer_attachments,
        COUNT(DISTINCT CASE WHEN att_appt.repcard_customer_id IS NOT NULL THEN c.repcard_customer_id END)::bigint as customers_with_appointment_attachments,
        COUNT(DISTINCT CASE WHEN (att_customer.id IS NOT NULL OR att_appt.id IS NOT NULL) THEN c.repcard_customer_id END)::bigint as customers_with_any_attachments
      FROM repcard_customers c
      LEFT JOIN repcard_customer_attachments att_customer ON c.repcard_customer_id::text = att_customer.repcard_customer_id::text
      LEFT JOIN repcard_appointment_attachments att_appt ON c.repcard_customer_id::text = att_appt.repcard_customer_id::text
      WHERE c.created_at >= NOW() - INTERVAL '90 days'
    `;

    return NextResponse.json({
      customerAttachments: {
        total: Number(customerAttachments?.total || 0),
        uniqueCustomers: Number(customerAttachments?.unique_customers || 0),
        missingCustomerId: Number(customerAttachments?.missing_customer_id || 0),
        earliest: customerAttachments?.earliest,
        latest: customerAttachments?.latest,
        samples: sampleCustomerAttachments.rows || sampleCustomerAttachments
      },
      appointmentAttachments: {
        total: Number(appointmentAttachments?.total || 0),
        uniqueAppointments: Number(appointmentAttachments?.unique_appointments || 0),
        uniqueCustomers: Number(appointmentAttachments?.unique_customers || 0),
        missingAppointmentId: Number(appointmentAttachments?.missing_appointment_id || 0),
        missingCustomerId: Number(appointmentAttachments?.missing_customer_id || 0),
        earliest: appointmentAttachments?.earliest,
        latest: appointmentAttachments?.latest,
        samples: sampleAppointmentAttachments.rows || sampleAppointmentAttachments
      },
      customersWithAttachments: customersWithAttachments.rows?.[0] || customersWithAttachments[0]
    });

  } catch (error) {
    console.error('[Check Attachments] Error:', error);
    return NextResponse.json(
      { 
        error: 'Internal server error',
        message: error instanceof Error ? error.message : 'Unknown error'
      },
      { status: 500 }
    );
  }
}

