import { sql } from '@vercel/postgres';
import { config } from 'dotenv';

config({ path: '.env.local' });

async function diagnoseCloserAttribution() {
  try {
    console.log('🔍 Diagnosing Closer Attribution Methods...\n');

    // Method 1: Count sales from appointments table (closer_user_id)
    console.log('📊 Method 1: Sales from Appointments Table (closer_user_id)');
    const appointmentSales = await sql`
      SELECT
        u.repcard_user_id,
        u.name,
        COUNT(DISTINCT a.repcard_appointment_id)::int as sales_closed
      FROM repcard_appointments a
      JOIN users u ON u.repcard_user_id::TEXT = a.closer_user_id::TEXT
      WHERE a.disposition ILIKE '%closed%'
        AND a.closer_user_id IS NOT NULL
      GROUP BY u.repcard_user_id, u.name
      ORDER BY sales_closed DESC
      LIMIT 10
    `;

    console.log('   Top 10 Closers (by appointment.closer_user_id):');
    appointmentSales.rows.forEach((row, idx) => {
      console.log(`   ${idx + 1}. ${row.name}: ${row.sales_closed} sales`);
    });

    // Method 2: Count sales from status logs (changed_by_user_id)
    console.log('\n📊 Method 2: Sales from Status Logs (changed_by_user_id)');
    const statusLogSales = await sql`
      SELECT
        u.repcard_user_id,
        u.name,
        COUNT(DISTINCT sl.repcard_customer_id)::int as sales_closed
      FROM repcard_status_logs sl
      JOIN users u ON u.repcard_user_id::TEXT = sl.changed_by_user_id::TEXT
      WHERE (
        LOWER(sl.new_status) LIKE '%sold%'
        OR LOWER(sl.new_status) LIKE '%closed%'
        OR LOWER(sl.new_status) LIKE '%won%'
        OR LOWER(sl.new_status) LIKE '%install%'
      )
      GROUP BY u.repcard_user_id, u.name
      ORDER BY sales_closed DESC
      LIMIT 10
    `;

    console.log('   Top 10 Closers (by status_logs.changed_by_user_id):');
    statusLogSales.rows.forEach((row, idx) => {
      console.log(`   ${idx + 1}. ${row.name}: ${row.sales_closed} sales`);
    });

    // Compare the two methods side by side
    console.log('\n📊 Side-by-Side Comparison (All Users):');
    const comparison = await sql`
      WITH appointment_sales AS (
        SELECT
          u.repcard_user_id,
          u.name,
          COUNT(DISTINCT a.repcard_appointment_id)::int as sales_from_appointments
        FROM repcard_appointments a
        JOIN users u ON u.repcard_user_id::TEXT = a.closer_user_id::TEXT
        WHERE a.disposition ILIKE '%closed%'
          AND a.closer_user_id IS NOT NULL
        GROUP BY u.repcard_user_id, u.name
      ),
      status_log_sales AS (
        SELECT
          u.repcard_user_id,
          u.name,
          COUNT(DISTINCT sl.repcard_customer_id)::int as sales_from_logs
        FROM repcard_status_logs sl
        JOIN users u ON u.repcard_user_id::TEXT = sl.changed_by_user_id::TEXT
        WHERE (
          LOWER(sl.new_status) LIKE '%sold%'
          OR LOWER(sl.new_status) LIKE '%closed%'
          OR LOWER(sl.new_status) LIKE '%won%'
          OR LOWER(sl.new_status) LIKE '%install%'
        )
        GROUP BY u.repcard_user_id, u.name
      )
      SELECT
        COALESCE(a.name, s.name) as name,
        COALESCE(a.sales_from_appointments, 0) as appointments_method,
        COALESCE(s.sales_from_logs, 0) as status_logs_method,
        COALESCE(a.sales_from_appointments, 0) - COALESCE(s.sales_from_logs, 0) as difference
      FROM appointment_sales a
      FULL OUTER JOIN status_log_sales s ON a.repcard_user_id = s.repcard_user_id
      ORDER BY ABS(COALESCE(a.sales_from_appointments, 0) - COALESCE(s.sales_from_logs, 0)) DESC
      LIMIT 15
    `;

    console.log('\n   User Name                    | Appointments | Status Logs | Difference');
    console.log('   ' + '-'.repeat(75));
    comparison.rows.forEach(row => {
      const diff = row.difference > 0 ? `+${row.difference}` : row.difference.toString();
      const flag = Math.abs(row.difference) > 5 ? ' ⚠️' : '';
      console.log(`   ${row.name.padEnd(28)} | ${String(row.appointments_method).padStart(12)} | ${String(row.status_logs_method).padStart(11)} | ${diff.padStart(10)}${flag}`);
    });

    // Sample mismatches
    console.log('\n🔬 Sample Appointments with Attribution Mismatch:');
    const samples = await sql`
      SELECT
        a.repcard_appointment_id,
        c.name as customer_name,
        u1.name as closer_name,
        a.closer_user_id,
        a.disposition,
        sl.new_status,
        u2.name as status_updater_name,
        sl.changed_by_user_id
      FROM repcard_appointments a
      LEFT JOIN repcard_customers c ON c.repcard_customer_id = a.repcard_customer_id
      LEFT JOIN users u1 ON u1.repcard_user_id::TEXT = a.closer_user_id::TEXT
      LEFT JOIN repcard_status_logs sl ON sl.repcard_customer_id = a.repcard_customer_id
        AND (
          LOWER(sl.new_status) LIKE '%sold%'
          OR LOWER(sl.new_status) LIKE '%closed%'
          OR LOWER(sl.new_status) LIKE '%won%'
          OR LOWER(sl.new_status) LIKE '%install%'
        )
      LEFT JOIN users u2 ON u2.repcard_user_id::TEXT = sl.changed_by_user_id::TEXT
      WHERE a.disposition ILIKE '%closed%'
        AND a.closer_user_id IS NOT NULL
        AND a.closer_user_id::TEXT != COALESCE(sl.changed_by_user_id::TEXT, '')
      LIMIT 10
    `;

    if (samples.rows.length > 0) {
      samples.rows.forEach((row, idx) => {
        console.log(`\n   ${idx + 1}. Appointment ${row.repcard_appointment_id}:`);
        console.log(`      Customer: ${row.customer_name}`);
        console.log(`      Assigned Closer: ${row.closer_name || 'NULL'} (ID: ${row.closer_user_id})`);
        console.log(`      Disposition: ${row.disposition}`);
        console.log(`      Status Updater: ${row.status_updater_name || 'NULL'} (ID: ${row.changed_by_user_id || 'NULL'})`);
        console.log(`      Status: ${row.new_status || 'N/A'}`);
        console.log(`      ❌ MISMATCH: ${row.closer_name} ran appointment, but ${row.status_updater_name || 'someone else'} updated status`);
      });
    } else {
      console.log('   ✅ No mismatches found in sample');
    }

    // Summary statistics
    console.log('\n📈 Summary Statistics:');
    const stats = await sql`
      SELECT
        COUNT(DISTINCT a.repcard_appointment_id)::int as total_closed_appointments,
        COUNT(DISTINCT a.repcard_appointment_id) FILTER (WHERE a.closer_user_id IS NOT NULL)::int as with_closer_assigned,
        COUNT(DISTINCT a.repcard_appointment_id) FILTER (WHERE a.closer_user_id IS NULL)::int as without_closer,
        (SELECT COUNT(DISTINCT repcard_customer_id)::int FROM repcard_status_logs
         WHERE LOWER(new_status) LIKE '%closed%' OR LOWER(new_status) LIKE '%sold%') as status_log_closes
      FROM repcard_appointments a
      WHERE a.disposition ILIKE '%closed%'
    `;

    const s = stats.rows[0];
    console.log(`   Total closed appointments: ${s.total_closed_appointments}`);
    console.log(`   - With closer assigned: ${s.with_closer_assigned}`);
    console.log(`   - Without closer assigned: ${s.without_closer}`);
    console.log(`   Status log close events: ${s.status_log_closes}`);

    console.log('\n✅ Diagnosis complete!');
    console.log('\n💡 Recommendation: Use appointments table (closer_user_id) for accurate closer attribution.');
    console.log('   This credits the person who actually ran the appointment, not who updated the CRM.');

    process.exit(0);
  } catch (error) {
    console.error('❌ Error:', error);
    process.exit(1);
  }
}

diagnoseCloserAttribution();
