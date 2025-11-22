import { sql as vercelSql } from '@vercel/postgres';
import { config } from 'dotenv';
import { repcardAPI } from './lib/repcard/api-client';

config({ path: '.env.local' });

async function runFullSync() {
  try {
    console.log('🚀 Starting Full RepCard Sync (Attachments Only)...\n');

    // Sync Customer Attachments
    console.log('📎 Fetching customer attachments from RepCard API...');
    let customerAttachments = 0;
    let page = 1;
    let hasMore = true;

    while (hasMore && page <= 5) { // Limit to 5 pages for safety
      const response = await repcardAPI.get(`/customer-attachments?page=${page}&limit=100`);
      const attachments = Array.isArray(response.result?.data) ? response.result.data : [];

      if (attachments.length === 0) {
        hasMore = false;
        break;
      }

      console.log(`   Page ${page}: Processing ${attachments.length} attachments...`);

      for (const att of attachments) {
        try {
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
              NULL,
              ${att.customerId},
              ${att.type || null},
              ${att.fileName || null},
              ${att.attachmentUrl || null},
              ${att.fileSize || null},
              ${att.uploadedBy || null},
              ${att.createdAt ? new Date(att.createdAt) : new Date()},
              ${att.updatedAt ? new Date(att.updatedAt) : new Date()},
              ${JSON.stringify(att)}
            )
            ON CONFLICT (repcard_attachment_id) DO UPDATE SET
              attachment_type = EXCLUDED.attachment_type,
              file_name = EXCLUDED.file_name,
              file_url = EXCLUDED.file_url,
              updated_at = EXCLUDED.updated_at,
              raw_data = EXCLUDED.raw_data
          `;
          customerAttachments++;
        } catch (err) {
          console.error(`   ⚠️  Failed to insert attachment ${att.id}:`, err.message);
        }
      }

      page++;
      hasMore = response.result?.pagination?.hasMore || false;
    }

    console.log(`✅ Customer attachments synced: ${customerAttachments}\n`);

    // Sync Appointment Attachments
    console.log('📎 Fetching appointment attachments from RepCard API...');
    let appointmentAttachments = 0;
    page = 1;
    hasMore = true;

    while (hasMore && page <= 5) {
      const response = await repcardAPI.get(`/appointment-attachments?page=${page}&limit=100`);
      const attachments = Array.isArray(response.result?.data) ? response.result.data : [];

      if (attachments.length === 0) {
        hasMore = false;
        break;
      }

      console.log(`   Page ${page}: Processing ${attachments.length} attachments...`);

      for (const att of attachments) {
        try {
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
              NULL,
              ${att.appointmentId},
              NULL,
              ${att.customerId || null},
              ${att.type || null},
              ${att.fileName || null},
              ${att.attachmentUrl || null},
              ${att.fileSize || null},
              ${att.uploadedBy || null},
              ${att.createdAt ? new Date(att.createdAt) : new Date()},
              ${att.updatedAt ? new Date(att.updatedAt) : new Date()},
              ${JSON.stringify(att)}
            )
            ON CONFLICT (repcard_attachment_id) DO UPDATE SET
              attachment_type = EXCLUDED.attachment_type,
              file_name = EXCLUDED.file_name,
              file_url = EXCLUDED.file_url,
              updated_at = EXCLUDED.updated_at,
              raw_data = EXCLUDED.raw_data
          `;
          appointmentAttachments++;
        } catch (err) {
          console.error(`   ⚠️  Failed to insert attachment ${att.id}:`, err.message);
        }
      }

      page++;
      hasMore = response.result?.pagination?.hasMore || false;
    }

    console.log(`✅ Appointment attachments synced: ${appointmentAttachments}\n`);

    console.log('\n✅ Full sync complete!');
    console.log(`📊 Total attachments: ${customerAttachments + appointmentAttachments}`);

    process.exit(0);
  } catch (error) {
    console.error('❌ Sync failed:', error);
    process.exit(1);
  }
}

runFullSync();
