#!/usr/bin/env tsx
import { sql } from '../lib/db/client';

/**
 * Audit script to verify 48-hour calculation logic
 * Compares what the backfill calculates vs what should be calculated
 */
async function audit48hCalculation() {
  console.log('🔍 Auditing 48-Hour Calculation Logic...\n');

  try {
    // Get sample appointments with their customer data
    const sampleAppts = await sql`
      SELECT 
        a.repcard_appointment_id,
        a.scheduled_at,
        a.created_at as appointment_created_at,
        a.is_within_48_hours as stored_value,
        c.repcard_customer_id,
        c.created_at as customer_created_at,
        -- Calculate what it SHOULD be based on scheduled_at
        CASE 
          WHEN a.scheduled_at IS NOT NULL AND c.created_at IS NOT NULL
            AND (a.scheduled_at - c.created_at) <= INTERVAL '48 hours' 
            AND (a.scheduled_at - c.created_at) >= INTERVAL '0 hours'
          THEN TRUE 
          ELSE FALSE 
        END as should_be_48h_scheduled,
        -- Calculate what it SHOULD be based on appointment.created_at (sync service logic)
        CASE 
          WHEN a.created_at IS NOT NULL AND c.created_at IS NOT NULL
            AND (a.created_at - c.created_at) <= INTERVAL '48 hours' 
            AND (a.created_at - c.created_at) >= INTERVAL '0 hours'
          THEN TRUE 
          ELSE FALSE 
        END as should_be_48h_created,
        (a.scheduled_at - c.created_at) as time_diff_scheduled,
        (a.created_at - c.created_at) as time_diff_created
      FROM repcard_appointments a
      LEFT JOIN repcard_customers c ON c.repcard_customer_id = a.repcard_customer_id
      WHERE a.scheduled_at >= NOW() - INTERVAL '7 days'
        AND a.scheduled_at IS NOT NULL
        AND c.created_at IS NOT NULL
      ORDER BY a.scheduled_at DESC
      LIMIT 20
    `;

    const rows = Array.from(sampleAppts);
    console.log(`Found ${rows.length} recent appointments to audit:\n`);

    let mismatches = 0;
    let scheduledVsCreatedMismatches = 0;

    for (const row of rows) {
      const stored = row.stored_value;
      const shouldBeScheduled = row.should_be_48h_scheduled;
      const shouldBeCreated = row.should_be_48h_created;
      const timeDiffScheduled = row.time_diff_scheduled;
      const timeDiffCreated = row.time_diff_created;

      const storedMatchesScheduled = stored === shouldBeScheduled;
      const scheduledDiffersFromCreated = shouldBeScheduled !== shouldBeCreated;

      if (!storedMatchesScheduled) {
        mismatches++;
      }
      if (scheduledDiffersFromCreated) {
        scheduledVsCreatedMismatches++;
      }

      console.log(`Appointment ${row.repcard_appointment_id}:`);
      console.log(`  Customer created: ${row.customer_created_at}`);
      console.log(`  Appointment created: ${row.appointment_created_at}`);
      console.log(`  Appointment scheduled: ${row.scheduled_at}`);
      console.log(`  Time diff (scheduled): ${timeDiffScheduled}`);
      console.log(`  Time diff (created): ${timeDiffCreated}`);
      console.log(`  Stored value: ${stored}`);
      console.log(`  Should be (scheduled logic): ${shouldBeScheduled}`);
      console.log(`  Should be (created logic): ${shouldBeCreated}`);
      if (!storedMatchesScheduled) {
        console.log(`  ❌ MISMATCH: Stored (${stored}) != Should be (${shouldBeScheduled})`);
      }
      if (scheduledDiffersFromCreated) {
        console.log(`  ⚠️  NOTE: Scheduled logic differs from created logic`);
      }
      console.log('');
    }

    console.log(`\n📊 Summary:`);
    console.log(`  Total checked: ${rows.length}`);
    console.log(`  Mismatches (stored vs should be): ${mismatches}`);
    console.log(`  Cases where scheduled != created logic: ${scheduledVsCreatedMismatches}`);

    // Check what the sync service is actually using
    console.log(`\n🔍 Checking sync service logic...`);
    const syncServiceCheck = await sql`
      SELECT 
        COUNT(*)::int as total,
        COUNT(*) FILTER (WHERE 
          a.created_at IS NOT NULL 
          AND c.created_at IS NOT NULL
          AND (a.created_at - c.created_at) <= INTERVAL '48 hours'
          AND (a.created_at - c.created_at) >= INTERVAL '0 hours'
        )::int as would_be_48h_using_created_at,
        COUNT(*) FILTER (WHERE 
          a.scheduled_at IS NOT NULL 
          AND c.created_at IS NOT NULL
          AND (a.scheduled_at - c.created_at) <= INTERVAL '48 hours'
          AND (a.scheduled_at - c.created_at) >= INTERVAL '0 hours'
        )::int as would_be_48h_using_scheduled_at
      FROM repcard_appointments a
      LEFT JOIN repcard_customers c ON c.repcard_customer_id = a.repcard_customer_id
      WHERE a.scheduled_at >= NOW() - INTERVAL '30 days'
        AND a.scheduled_at IS NOT NULL
        AND c.created_at IS NOT NULL
    `;
    const check = Array.from(syncServiceCheck)[0];
    console.log(`  Using created_at logic: ${check?.would_be_48h_using_created_at || 0} appointments`);
    console.log(`  Using scheduled_at logic: ${check?.would_be_48h_using_scheduled_at || 0} appointments`);
    console.log(`  Total appointments: ${check?.total || 0}`);

    if (check?.would_be_48h_using_created_at !== check?.would_be_48h_using_scheduled_at) {
      console.log(`\n⚠️  WARNING: The two calculation methods give different results!`);
      console.log(`  This suggests we need to decide which is correct.`);
    }

  } catch (error) {
    console.error('❌ Error:', error);
    process.exit(1);
  }
}

audit48hCalculation();
