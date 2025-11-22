import { sql as vercelSql } from '@vercel/postgres';
import { config } from 'dotenv';

config({ path: '.env.local' });

// RepCard API client setup
const REPCARD_API_URL = process.env.REPCARD_API_URL || 'https://api.repcard.com/api';
const REPCARD_API_KEY = process.env.REPCARD_API_KEY;

if (!REPCARD_API_KEY) {
  throw new Error('REPCARD_API_KEY is required');
}

async function repcardGet(endpoint: string) {
  const url = `${REPCARD_API_URL}${endpoint}`;
  const response = await fetch(url, {
    headers: {
      'x-api-key': REPCARD_API_KEY,
      'Content-Type': 'application/json',
    },
  });

  if (!response.ok) {
    const errorText = await response.text();
    throw new Error(`RepCard API error: ${response.status} ${response.statusText} - ${errorText}`);
  }

  return response.json();
}

async function syncAttachments() {
  try {
    console.log('🚀 Starting RepCard Attachment Sync...\n');

    // Sync Customer Attachments
    console.log('📎 Fetching customer attachments from RepCard API...');
    let customerAttachments = 0;
    let page = 1;
    let hasMore = true;

    while (hasMore && page <= 10) {
      const response = await repcardGet(`/customers/attachments?page=${page}&per_page=100`);
      const attachments = Array.isArray(response.result?.data) ? response.result.data : [];

      if (attachments.length === 0) {
        hasMore = false;
        break;
      }

      console.log(`   Page ${page}: Processing ${attachments.length} attachments...`);

      for (const att of attachments) {
        try {
          // Get customer_id from our database
          const customerResult = await vercelSql`
            SELECT id FROM repcard_customers
            WHERE repcard_customer_id::text = ${att.customerId}::text
            LIMIT 1
          `;
          const customerId = customerResult.rows?.[0]?.id || null;

          await vercelSql`
            INSERT INTO repcard_customer_attachments (
              repcard_attachment_id,
              customer_id,
              repcard_customer_id,
              attachment_type,
              file_name,
              file_url,
              file_size,
              uploaded_by_user_id,
              created_at,
              updated_at,
              raw_data
            ) VALUES (
              ${att.id},
              ${customerId},
              ${att.customerId},
              ${att.type || null},
              ${att.fileName || null},
              ${att.attachmentUrl || null},
              ${att.fileSize || null},
              ${att.userId || null},
              ${att.createdAt ? new Date(att.createdAt) : new Date()},
              ${att.updatedAt ? new Date(att.updatedAt) : new Date()},
              ${JSON.stringify(att)}
            )
            ON CONFLICT (repcard_attachment_id) DO UPDATE SET
              customer_id = EXCLUDED.customer_id,
              attachment_type = EXCLUDED.attachment_type,
              file_name = EXCLUDED.file_name,
              file_url = EXCLUDED.file_url,
              updated_at = EXCLUDED.updated_at,
              raw_data = EXCLUDED.raw_data,
              synced_at = NOW()
          `;
          customerAttachments++;
        } catch (err: any) {
          console.error(`   ⚠️  Failed to insert attachment ${att.id}:`, err.message);
        }
      }

      page++;
      hasMore = response.result?.currentPage < response.result?.totalPages;
    }

    console.log(`✅ Customer attachments synced: ${customerAttachments}\n`);

    // Sync Appointment Attachments
    console.log('📎 Fetching appointment attachments from RepCard API...');
    let appointmentAttachments = 0;
    page = 1;
    hasMore = true;

    while (hasMore && page <= 10) {
      const response = await repcardGet(`/appointments/attachments?page=${page}&per_page=100`);
      const attachments = Array.isArray(response.result?.data) ? response.result.data : [];

      if (attachments.length === 0) {
        hasMore = false;
        break;
      }

      console.log(`   Page ${page}: Processing ${attachments.length} attachments...`);

      for (const att of attachments) {
        try {
          // Get appointment_id and customer_id from our database
          const appointmentResult = await vercelSql`
            SELECT id, customer_id, repcard_customer_id
            FROM repcard_appointments
            WHERE repcard_appointment_id::text = ${att.appointmentId}::text
            LIMIT 1
          `;
          const appointmentId = appointmentResult.rows?.[0]?.id || null;
          const customerId = appointmentResult.rows?.[0]?.customer_id || null;
          const repcardCustomerId = appointmentResult.rows?.[0]?.repcard_customer_id || null;

          await vercelSql`
            INSERT INTO repcard_appointment_attachments (
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
              updated_at,
              raw_data
            ) VALUES (
              ${att.id},
              ${appointmentId},
              ${att.appointmentId},
              ${customerId},
              ${repcardCustomerId},
              ${att.type || null},
              ${att.fileName || null},
              ${att.attachmentUrl || null},
              ${att.fileSize || null},
              ${att.userId || null},
              ${att.createdAt ? new Date(att.createdAt) : new Date()},
              ${att.updatedAt ? new Date(att.updatedAt) : new Date()},
              ${JSON.stringify(att)}
            )
            ON CONFLICT (repcard_attachment_id) DO UPDATE SET
              appointment_id = EXCLUDED.appointment_id,
              customer_id = EXCLUDED.customer_id,
              attachment_type = EXCLUDED.attachment_type,
              file_name = EXCLUDED.file_name,
              file_url = EXCLUDED.file_url,
              updated_at = EXCLUDED.updated_at,
              raw_data = EXCLUDED.raw_data,
              synced_at = NOW()
          `;
          appointmentAttachments++;
        } catch (err: any) {
          console.error(`   ⚠️  Failed to insert attachment ${att.id}:`, err.message);
        }
      }

      page++;
      hasMore = response.result?.currentPage < response.result?.totalPages;
    }

    console.log(`✅ Appointment attachments synced: ${appointmentAttachments}\n`);

    console.log('\n✅ Attachment sync complete!');
    console.log(`📊 Total attachments: ${customerAttachments + appointmentAttachments}`);

    process.exit(0);
  } catch (error) {
    console.error('❌ Sync failed:', error);
    process.exit(1);
  }
}

syncAttachments();
