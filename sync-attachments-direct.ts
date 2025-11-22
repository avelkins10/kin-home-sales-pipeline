import { config } from 'dotenv';
config({ path: '.env.local' });

async function syncAttachments() {
  try {
    console.log('🚀 Starting RepCard Attachment Sync...\n');

    // Import directly
    const { syncRepCardAttachments } = await import('./lib/repcard/sync-attachments');

    console.log('📎 Syncing customer attachments...');
    const customerResult = await syncRepCardAttachments('customer');
    console.log(`✅ Customer attachments: ${customerResult.recordsInserted} inserted, ${customerResult.recordsUpdated} updated\n`);

    console.log('📎 Syncing appointment attachments...');
    const appointmentResult = await syncRepCardAttachments('appointment');
    console.log(`✅ Appointment attachments: ${appointmentResult.recordsInserted} inserted, ${appointmentResult.recordsUpdated} updated\n`);

    console.log('✅ Attachment sync complete!');
    console.log(`📊 Total: ${customerResult.recordsInserted + appointmentResult.recordsInserted} new attachments`);

    process.exit(0);
  } catch (error) {
    console.error('❌ Sync failed:', error);
    console.error(error);
    process.exit(1);
  }
}

syncAttachments();
