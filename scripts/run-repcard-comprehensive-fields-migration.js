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
    // Use separate queries to avoid type casting issues
    console.log('🔄 Updating existing appointments with calculated fields...');
    
    // Update office_id
    console.log('  Updating office_id...');
    const officeUpdate = await sql.query(`
      UPDATE repcard_appointments a
      SET office_id = (
        SELECT office_id::INTEGER FROM repcard_customers 
        WHERE repcard_customer_id = a.repcard_customer_id 
        LIMIT 1
      )::INTEGER
      WHERE repcard_customer_id IS NOT NULL
        AND office_id IS NULL
    `);
    console.log(`    Updated ${officeUpdate.rowCount || 0} appointments with office from customer`);
    
    const officeUpdate2 = await sql.query(`
      UPDATE repcard_appointments a
      SET office_id = (
        SELECT office_id::INTEGER FROM repcard_users 
        WHERE repcard_user_id::text = a.setter_user_id::text 
        LIMIT 1
      )::INTEGER
      WHERE setter_user_id IS NOT NULL
        AND office_id IS NULL
    `);
    console.log(`    Updated ${officeUpdate2.rowCount || 0} appointments with office from setter`);
    
    const officeUpdate3 = await sql.query(`
      UPDATE repcard_appointments a
      SET office_id = (
        SELECT office_id::INTEGER FROM repcard_users 
        WHERE repcard_user_id::text = a.closer_user_id::text 
        LIMIT 1
      )::INTEGER
      WHERE closer_user_id IS NOT NULL
        AND office_id IS NULL
    `);
    console.log(`    Updated ${officeUpdate3.rowCount || 0} appointments with office from closer`);
    
    // Update is_within_48_hours
    console.log('  Updating is_within_48_hours...');
    const within48Update = await sql.query(`
      UPDATE repcard_appointments a
      SET is_within_48_hours = TRUE
      FROM repcard_customers c
      WHERE c.repcard_customer_id = a.repcard_customer_id
        AND (a.created_at - c.created_at) <= INTERVAL '48 hours'
        AND (a.created_at - c.created_at) >= INTERVAL '0 hours'
    `);
    console.log(`    Marked ${within48Update.rowCount || 0} appointments as within 48 hours`);
    
    // Update has_power_bill
    console.log('  Updating has_power_bill...');
    const powerBillUpdate = await sql.query(`
      UPDATE repcard_appointments a
      SET has_power_bill = TRUE
      WHERE EXISTS (
        SELECT 1 FROM repcard_customer_attachments ca
        WHERE ca.repcard_customer_id::text = a.repcard_customer_id::text
          AND (
            ca.attachment_type ILIKE '%power%' 
            OR ca.attachment_type ILIKE '%bill%' 
            OR ca.file_name ILIKE '%power%' 
            OR ca.file_name ILIKE '%bill%'
          )
      )
    `);
    console.log(`    Marked ${powerBillUpdate.rowCount || 0} appointments as having power bill`);
    
    // Update status_category
    console.log('  Updating status_category...');
    const statusUpdate = await sql.query(`
      UPDATE repcard_appointments
      SET status_category = CASE
        WHEN disposition IS NULL THEN 'pending'
        WHEN LOWER(disposition) LIKE '%cancel%' THEN 'cancelled'
        WHEN LOWER(disposition) LIKE '%reschedule%' THEN 'rescheduled'
        WHEN LOWER(disposition) LIKE '%no.show%' OR LOWER(disposition) LIKE '%no_show%' THEN 'no_show'
        WHEN LOWER(disposition) LIKE '%sat.closed%' OR LOWER(disposition) LIKE '%sat_closed%' OR LOWER(disposition) LIKE '%closed%' THEN 'sat_closed'
        WHEN LOWER(disposition) LIKE '%sat.no.close%' OR LOWER(disposition) LIKE '%sat_no_close%' THEN 'sat_no_close'
        WHEN completed_at IS NOT NULL THEN 'completed'
        WHEN scheduled_at IS NOT NULL THEN 'scheduled'
        ELSE 'pending'
      END
      WHERE status_category IS NULL
    `);
    console.log(`    Updated ${statusUpdate.rowCount || 0} appointments with status category`);
    
    console.log('✅ All appointments updated!');
    
  } catch (error) {
    console.error('❌ Migration failed:', error);
    process.exit(1);
  }
}

runMigration();

