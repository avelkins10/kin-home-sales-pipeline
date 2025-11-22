import { sql } from '@vercel/postgres';
import { config } from 'dotenv';

config({ path: '.env.local' });

async function diagnoseData() {
  try {
    console.log('🔍 Diagnosing RepCard Dashboard Data...\n');

    // Check appointments
    const apptStats = await sql`
      SELECT
        COUNT(*)::int as total,
        COUNT(*) FILTER (WHERE scheduled_at IS NOT NULL)::int as has_scheduled_at,
        COUNT(*) FILTER (WHERE repcard_customer_id IS NOT NULL)::int as has_customer_id,
        COUNT(*) FILTER (WHERE is_within_48_hours = TRUE)::int as within_48h,
        COUNT(*) FILTER (WHERE is_reschedule = TRUE)::int as reschedules
      FROM repcard_appointments;
    `;

    console.log('📅 Appointments:');
    console.log(`   Total: ${apptStats.rows[0].total}`);
    console.log(`   With scheduled_at: ${apptStats.rows[0].has_scheduled_at}`);
    console.log(`   With customer_id: ${apptStats.rows[0].has_customer_id}`);
    console.log(`   Within 48h: ${apptStats.rows[0].within_48h}`);
    console.log(`   Reschedules: ${apptStats.rows[0].reschedules}\n`);

    // Check attachments
    const attachStats = await sql`
      SELECT
        (SELECT COUNT(*)::int FROM repcard_appointment_attachments) as appt_attachments,
        (SELECT COUNT(*)::int FROM repcard_appointment_attachments WHERE attachment_type = 'power_bill') as appt_power_bills,
        (SELECT COUNT(*)::int FROM repcard_customer_attachments) as cust_attachments,
        (SELECT COUNT(*)::int FROM repcard_customer_attachments WHERE attachment_type ILIKE '%power%' OR attachment_type ILIKE '%bill%') as cust_power_bills;
    `;

    console.log('📎 Attachments:');
    console.log(`   Appointment attachments: ${attachStats.rows[0].appt_attachments}`);
    console.log(`   - Power bills: ${attachStats.rows[0].appt_power_bills}`);
    console.log(`   Customer attachments: ${attachStats.rows[0].cust_attachments}`);
    console.log(`   - Power bills: ${attachStats.rows[0].cust_power_bills}\n`);

    // Check offices
    const officeStats = await sql`
      SELECT COUNT(*)::int as total_offices FROM repcard_offices;
    `;

    console.log(`🏢 Offices: ${officeStats.rows[0].total_offices}\n`);

    // Check users with appointments
    const userStats = await sql`
      SELECT COUNT(DISTINCT u.repcard_user_id)::int as users_with_appointments
      FROM users u
      INNER JOIN repcard_appointments a ON a.setter_user_id = u.repcard_user_id::TEXT
      WHERE u.repcard_user_id IS NOT NULL;
    `;

    console.log(`👥 Users with appointments: ${userStats.rows[0].users_with_appointments}\n`);

    // Check today's data
    const todayStats = await sql`
      SELECT
        COUNT(*)::int as appointments_today
      FROM repcard_appointments
      WHERE scheduled_at::date = CURRENT_DATE;
    `;

    console.log(`📊 Today's Data:`);
    console.log(`   Appointments scheduled for today: ${todayStats.rows[0].appointments_today}\n`);

    console.log('✅ Diagnosis complete!');
    process.exit(0);
  } catch (error) {
    console.error('❌ Error:', error);
    process.exit(1);
  }
}

diagnoseData();
