import { config } from 'dotenv';
import { syncCustomerAttachments, syncAppointmentAttachments } from './lib/repcard/comprehensive-sync';

config({ path: '.env.local' });

async function syncAttachmentsOnly() {
  try {
    console.log('🚀 Starting RepCard Attachment Sync...\n');

    console.log('📎 Syncing customer attachments...');
    const customerResult = await syncCustomerAttachments({ incremental: false });
    console.log(`✅ Customer attachments: ${customerResult.recordsInserted} inserted, ${customerResult.recordsUpdated} updated\n`);

    console.log('📎 Syncing appointment attachments...');
    const appointmentResult = await syncAppointmentAttachments({ incremental: false });
    console.log(`✅ Appointment attachments: ${appointmentResult.recordsInserted} inserted, ${appointmentResult.recordsUpdated} updated\n`);

    console.log('✅ Attachment sync complete!');
    console.log(`📊 Total: ${customerResult.recordsInserted + appointmentResult.recordsInserted} new, ${customerResult.recordsUpdated + appointmentResult.recordsUpdated} updated`);

    process.exit(0);
  } catch (error) {
    console.error('❌ Sync failed:', error);
    process.exit(1);
  }
}

syncAttachmentsOnly();
