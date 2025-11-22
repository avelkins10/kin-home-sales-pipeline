import { sql } from '@vercel/postgres';
import { config } from 'dotenv';

config({ path: '.env.local' });

async function analyzeAttachments() {
  try {
    console.log('🔍 Analyzing attachments for power bill identification...\n');

    // Get sample of raw_data to see all available fields
    const samples = await sql`
      SELECT raw_data
      FROM repcard_appointment_attachments
      WHERE raw_data IS NOT NULL
      LIMIT 5
    `;

    console.log('📋 Sample raw_data structures:');
    samples.rows.forEach((row, idx) => {
      const data = typeof row.raw_data === 'string' ? JSON.parse(row.raw_data) : row.raw_data;
      console.log(`\nSample ${idx + 1}:`);
      console.log(JSON.stringify(data, null, 2));
    });

    // Count attachments by checking file_name for power/bill keywords
    const powerBillCheck = await sql`
      SELECT
        COUNT(*) FILTER (WHERE file_name ILIKE '%power%' OR file_name ILIKE '%bill%' OR file_name ILIKE '%electric%' OR file_name ILIKE '%utility%')::int as possible_power_bills,
        COUNT(*) FILTER (WHERE file_name IS NOT NULL)::int as with_file_name,
        COUNT(*)::int as total
      FROM repcard_appointment_attachments
    `;

    console.log('\n\n📊 Attachment Analysis:');
    console.log(`   Total appointment attachments: ${powerBillCheck.rows[0].total}`);
    console.log(`   With file_name: ${powerBillCheck.rows[0].with_file_name}`);
    console.log(`   Possible power bills (name contains power/bill/electric/utility): ${powerBillCheck.rows[0].possible_power_bills}`);

    // Check customer attachments too
    const customerPowerBillCheck = await sql`
      SELECT
        COUNT(*) FILTER (WHERE file_name ILIKE '%power%' OR file_name ILIKE '%bill%' OR file_name ILIKE '%electric%' OR file_name ILIKE '%utility%')::int as possible_power_bills,
        COUNT(*) FILTER (WHERE file_name IS NOT NULL)::int as with_file_name,
        COUNT(*)::int as total
      FROM repcard_customer_attachments
    `;

    console.log(`\n   Total customer attachments: ${customerPowerBillCheck.rows[0].total}`);
    console.log(`   With file_name: ${customerPowerBillCheck.rows[0].with_file_name}`);
    console.log(`   Possible power bills (name contains power/bill/electric/utility): ${customerPowerBillCheck.rows[0].possible_power_bills}`);

    process.exit(0);
  } catch (error) {
    console.error('❌ Error:', error);
    process.exit(1);
  }
}

analyzeAttachments();
