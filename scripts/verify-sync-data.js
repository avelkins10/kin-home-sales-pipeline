#!/usr/bin/env node

/**
 * Verify RepCard sync data in production database
 */

import { config } from 'dotenv';
import { resolve, dirname } from 'path';
import { fileURLToPath } from 'url';
import postgres from 'postgres';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

// Load production environment variables
config({ path: resolve(__dirname, '../.env.production.local') });

const DATABASE_URL = process.env.DATABASE_URL || process.env.POSTGRES_URL;

if (!DATABASE_URL) {
  console.error('❌ DATABASE_URL not found in environment');
  process.exit(1);
}

const sql = postgres(DATABASE_URL);

async function verifyData() {
  try {
    console.log('🔍 Checking RepCard sync data in production database...\n');

    // Check sync log
    console.log('📊 Recent Sync History:');
    const syncLogs = await sql`
      SELECT
        entity_type,
        sync_type,
        records_fetched,
        records_inserted,
        records_updated,
        records_failed,
        started_at,
        completed_at,
        EXTRACT(EPOCH FROM (completed_at - started_at)) as duration_seconds
      FROM repcard_sync_log
      ORDER BY started_at DESC
      LIMIT 10
    `;

    console.table(syncLogs);

    // Check actual counts in tables
    console.log('\n📈 Actual Record Counts:');

    const customerCount = await sql`SELECT COUNT(*) as count FROM repcard_customers`;
    console.log(`✅ Customers: ${customerCount[0].count}`);

    const appointmentCount = await sql`SELECT COUNT(*) as count FROM repcard_appointments`;
    console.log(`✅ Appointments: ${appointmentCount[0].count}`);

    const statusLogCount = await sql`SELECT COUNT(*) as count FROM repcard_status_logs`;
    console.log(`✅ Status Logs: ${statusLogCount[0].count}`);

    // Check date ranges
    console.log('\n📅 Date Ranges:');

    const appointmentDates = await sql`
      SELECT
        MIN(appointment_date) as earliest,
        MAX(appointment_date) as latest
      FROM repcard_appointments
      WHERE appointment_date IS NOT NULL
    `;
    console.log(`Appointments: ${appointmentDates[0].earliest} to ${appointmentDates[0].latest}`);

    const statusLogDates = await sql`
      SELECT
        MIN(changed_at) as earliest,
        MAX(changed_at) as latest
      FROM repcard_status_logs
      WHERE changed_at IS NOT NULL
    `;
    console.log(`Status Logs: ${statusLogDates[0].earliest} to ${statusLogDates[0].latest}`);

    // Sample data check
    console.log('\n📝 Sample Appointments (latest 5):');
    const sampleAppointments = await sql`
      SELECT
        repcard_appointment_id,
        appointment_date,
        appointment_result,
        setter_name,
        closer_name
      FROM repcard_appointments
      ORDER BY appointment_date DESC
      LIMIT 5
    `;
    console.table(sampleAppointments);

    console.log('\n✅ Database verification complete!');

  } catch (error) {
    console.error('❌ Error verifying data:', error);
    throw error;
  } finally {
    await sql.end();
  }
}

verifyData().catch((error) => {
  console.error('Script failed:', error);
  process.exit(1);
});
