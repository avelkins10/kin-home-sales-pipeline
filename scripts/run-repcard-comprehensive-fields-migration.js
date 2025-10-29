/**
 * Run migration 015_repcard_comprehensive_fields.sql
 * Adds office_id, calculated fields, and status categories to appointments
 */

require('dotenv').config({ path: '.env.local' });
const { sql } = require('@vercel/postgres');
const fs = require('fs');
const path = require('path');

async function runMigration() {
  try {
    console.log('🔄 Running RepCard comprehensive fields migration...');
    
    // Execute migration SQL file directly (it handles BEGIN/COMMIT)
    const migrationPath = path.join(__dirname, '../lib/db/migrations/015_repcard_comprehensive_fields.sql');
    const migrationSQL = fs.readFileSync(migrationPath, 'utf8');
    
    console.log('📝 Executing migration SQL...');
    
    try {
      // Execute the entire migration (it has BEGIN/COMMIT blocks)
      await sql.query(migrationSQL);
      console.log('✅ Migration completed successfully!');
    } catch (error) {
      // If columns already exist, that's okay - continue
      if (error.message?.includes('already exists') || error.message?.includes('duplicate')) {
        console.log('⚠️  Some objects already exist, continuing...');
      } else {
        throw error;
      }
    }
    
    // Update existing appointments (run after migration)
    console.log('🔄 Updating existing appointments with calculated fields...');
    await sql.query(`
      UPDATE repcard_appointments a
      SET 
        office_id = COALESCE(
          (SELECT office_id FROM repcard_customers WHERE repcard_customer_id = a.repcard_customer_id LIMIT 1),
          (SELECT office_id FROM repcard_users WHERE repcard_user_id = a.setter_user_id LIMIT 1),
          (SELECT office_id FROM repcard_users WHERE repcard_user_id = a.closer_user_id LIMIT 1)
        ),
        is_within_48_hours = CASE
          WHEN EXISTS (
            SELECT 1 FROM repcard_customers c
            WHERE c.repcard_customer_id = a.repcard_customer_id
              AND (a.created_at - c.created_at) <= INTERVAL '48 hours'
          ) THEN TRUE
          ELSE FALSE
        END,
        has_power_bill = CASE
          WHEN EXISTS (
            SELECT 1 FROM repcard_customer_attachments ca
            WHERE ca.repcard_customer_id = a.repcard_customer_id
              AND (ca.attachment_type ILIKE '%power%' OR ca.attachment_type ILIKE '%bill%' OR ca.file_name ILIKE '%power%' OR ca.file_name ILIKE '%bill%')
          ) THEN TRUE
          ELSE FALSE
        END,
        status_category = CASE
          WHEN a.disposition IS NULL THEN 'pending'
          WHEN LOWER(a.disposition) LIKE '%cancel%' THEN 'cancelled'
          WHEN LOWER(a.disposition) LIKE '%reschedule%' THEN 'rescheduled'
          WHEN LOWER(a.disposition) LIKE '%no.show%' OR LOWER(a.disposition) LIKE '%no_show%' THEN 'no_show'
          WHEN LOWER(a.disposition) LIKE '%sat.closed%' OR LOWER(a.disposition) LIKE '%sat_closed%' OR LOWER(a.disposition) LIKE '%closed%' THEN 'sat_closed'
          WHEN LOWER(a.disposition) LIKE '%sat.no.close%' OR LOWER(a.disposition) LIKE '%sat_no_close%' THEN 'sat_no_close'
          WHEN a.completed_at IS NOT NULL THEN 'completed'
          WHEN a.scheduled_at IS NOT NULL THEN 'scheduled'
          ELSE 'pending'
        END
    `);
    
    console.log('✅ All appointments updated!');
    
  } catch (error) {
    console.error('❌ Migration failed:', error);
    process.exit(1);
  }
}

runMigration();

