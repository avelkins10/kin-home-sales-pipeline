import { sql } from '@vercel/postgres';
import fs from 'fs';
import path from 'path';
import { config } from 'dotenv';

// Load environment variables
config({ path: '.env.local' });

async function runMigration031() {
  try {
    console.log('🚀 Running migration 031 (Fix 48-hour speed calculation) on PRODUCTION database...');
    console.log('⚠️  Database:', process.env.POSTGRES_URL?.substring(0, 50) + '...');

    // Read migration SQL
    const migrationSQL = fs.readFileSync(
      path.join(process.cwd(), 'lib/db/migrations/031_fix_48_hour_speed_calculation.sql'),
      'utf-8'
    );

    // Execute migration
    console.log('📝 Executing migration SQL...');
    console.log('   - Updating trigger function');
    console.log('   - Backfilling existing appointments (this may take a moment)...');

    await sql.query(migrationSQL);

    console.log('✅ Migration 031 completed successfully!');

    // Check results
    const statsResult = await sql`
      SELECT
        COUNT(*)::int as total_appointments,
        COUNT(*) FILTER (WHERE is_within_48_hours = TRUE)::int as within_48h,
        COUNT(*) FILTER (WHERE is_within_48_hours = FALSE)::int as not_within_48h
      FROM repcard_appointments
      WHERE scheduled_at IS NOT NULL;
    `;

    const stats = statsResult.rows[0];
    console.log('\n📊 48-Hour Speed Stats:');
    console.log(`   Total appointments with scheduled_at: ${stats.total_appointments}`);
    console.log(`   Within 48 hours: ${stats.within_48h} (${((stats.within_48h / stats.total_appointments) * 100).toFixed(1)}%)`);
    console.log(`   Not within 48 hours: ${stats.not_within_48h} (${((stats.not_within_48h / stats.total_appointments) * 100).toFixed(1)}%)`);

    console.log('\n✅ Production migration complete!');
    console.log('📌 Dashboard should now show accurate 48-hour speed metrics');

    process.exit(0);
  } catch (error) {
    console.error('❌ Migration failed:', error);
    process.exit(1);
  }
}

runMigration031();
