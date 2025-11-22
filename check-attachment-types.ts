import { sql } from '@vercel/postgres';
import { config } from 'dotenv';

config({ path: '.env.local' });

async function checkAttachmentTypes() {
  try {
    console.log('🔍 Checking attachment types...\n');

    // Check customer attachment types
    const customerTypes = await sql`
      SELECT attachment_type, COUNT(*)::int as count
      FROM repcard_customer_attachments
      WHERE attachment_type IS NOT NULL
      GROUP BY attachment_type
      ORDER BY count DESC
      LIMIT 10
    `;

    console.log('📎 Customer Attachment Types:');
    if (customerTypes.rows.length === 0) {
      console.log('   (No attachment types found - all NULL)');
    } else {
      customerTypes.rows.forEach(row => {
        console.log(`   ${row.attachment_type}: ${row.count}`);
      });
    }

    // Check appointment attachment types
    const appointmentTypes = await sql`
      SELECT attachment_type, COUNT(*)::int as count
      FROM repcard_appointment_attachments
      WHERE attachment_type IS NOT NULL
      GROUP BY attachment_type
      ORDER BY count DESC
      LIMIT 10
    `;

    console.log('\n📎 Appointment Attachment Types:');
    if (appointmentTypes.rows.length === 0) {
      console.log('   (No attachment types found - all NULL)');
    } else {
      appointmentTypes.rows.forEach(row => {
        console.log(`   ${row.attachment_type}: ${row.count}`);
      });
    }

    // Sample a few customer attachments to see raw data
    const sampleCustomer = await sql`
      SELECT repcard_attachment_id, attachment_type, file_name, raw_data
      FROM repcard_customer_attachments
      LIMIT 3
    `;

    console.log('\n🔬 Sample Customer Attachments:');
    sampleCustomer.rows.forEach(row => {
      console.log(`   ID: ${row.repcard_attachment_id}`);
      console.log(`   Type: ${row.attachment_type || '(NULL)'}`);
      console.log(`   File: ${row.file_name || '(NULL)'}`);
      if (row.raw_data) {
        const data = typeof row.raw_data === 'string' ? JSON.parse(row.raw_data) : row.raw_data;
        console.log(`   Raw type: ${data.type || '(not set)'}`);
        console.log(`   Raw fileName: ${data.fileName || '(not set)'}`);
      }
      console.log('');
    });

    // Sample a few appointment attachments
    const sampleAppointment = await sql`
      SELECT repcard_attachment_id, attachment_type, file_name, raw_data
      FROM repcard_appointment_attachments
      LIMIT 3
    `;

    console.log('🔬 Sample Appointment Attachments:');
    sampleAppointment.rows.forEach(row => {
      console.log(`   ID: ${row.repcard_attachment_id}`);
      console.log(`   Type: ${row.attachment_type || '(NULL)'}`);
      console.log(`   File: ${row.file_name || '(NULL)'}`);
      if (row.raw_data) {
        const data = typeof row.raw_data === 'string' ? JSON.parse(row.raw_data) : row.raw_data;
        console.log(`   Raw type: ${data.type || '(not set)'}`);
        console.log(`   Raw fileName: ${data.fileName || '(not set)'}`);
      }
      console.log('');
    });

    process.exit(0);
  } catch (error) {
    console.error('❌ Error:', error);
    process.exit(1);
  }
}

checkAttachmentTypes();
