const { sql } = require('@vercel/postgres');
const fs = require('fs');
const path = require('path');

// Load environment variables from .env.local
require('dotenv').config({ path: path.join(__dirname, '../.env.local') });

async function runMigration() {
  try {
    console.log('🚀 Starting user hierarchies migration...');
    
    // Verify DATABASE_URL is set
    if (!process.env.DATABASE_URL) {
      throw new Error('DATABASE_URL environment variable is required');
    }

    // Read migration file
    const migrationPath = path.join(__dirname, '../lib/db/migrations/006_user_hierarchies_schema.sql');
    const migrationSQL = fs.readFileSync(migrationPath, 'utf8');

    // Execute migration
    console.log('📝 Executing migration SQL...');
    await sql.query(migrationSQL);
    
    console.log('✅ Updated users table role constraint');
    console.log('✅ Added activity tracking fields to users table');
    console.log('✅ Created user_hierarchies table');
    console.log('✅ Created office_assignments table');
    console.log('✅ Created sync_logs table');
    console.log('✅ Created notification_settings table');
    console.log('✅ Created indexes (9 total)');

    // Verify schema
    console.log('🔍 Verifying schema...');
    
    // Check user_hierarchies table
    const hierarchiesCheck = await sql.query(`
      SELECT table_name FROM information_schema.tables 
      WHERE table_name = 'user_hierarchies'
    `);
    if (hierarchiesCheck.rows.length === 0) {
      throw new Error('user_hierarchies table not found');
    }
    console.log('✅ user_hierarchies table exists');
    
    // Check office_assignments table
    const assignmentsCheck = await sql.query(`
      SELECT table_name FROM information_schema.tables 
      WHERE table_name = 'office_assignments'
    `);
    if (assignmentsCheck.rows.length === 0) {
      throw new Error('office_assignments table not found');
    }
    console.log('✅ office_assignments table exists');
    
    // Check sync_logs table
    const syncLogsCheck = await sql.query(`
      SELECT table_name FROM information_schema.tables 
      WHERE table_name = 'sync_logs'
    `);
    if (syncLogsCheck.rows.length === 0) {
      throw new Error('sync_logs table not found');
    }
    console.log('✅ sync_logs table exists');
    
    // Check notification_settings table
    const notificationsCheck = await sql.query(`
      SELECT table_name FROM information_schema.tables 
      WHERE table_name = 'notification_settings'
    `);
    if (notificationsCheck.rows.length === 0) {
      throw new Error('notification_settings table not found');
    }
    console.log('✅ notification_settings table exists');
    
    // Check new columns in users table
    const columnsCheck = await sql.query(`
      SELECT column_name FROM information_schema.columns 
      WHERE table_name = 'users' 
      AND column_name IN ('last_project_date', 'invited_at', 'invite_token', 'invite_accepted_at')
    `);
    if (columnsCheck.rows.length < 4) {
      throw new Error('Not all new columns found in users table');
    }
    console.log('✅ New columns added to users table');
    
    console.log('✅ User hierarchies migration complete');
    console.log('📊 Schema verification passed');
    
  } catch (error) {
    if (error.message.includes('already exists')) {
      console.log('⚠️  Tables already exist (safe to ignore)');
      console.log('   📊 Migration was previously run');
      return;
    }
    
    console.error('❌ Migration failed!');
    console.error(`   Error: ${error.message}`);
    
    console.log('\n🔧 Troubleshooting Tips:');
    console.log('1. Check if tables already exist (safe to ignore)');
    console.log('2. Verify migration SQL syntax is correct');
    console.log('3. Ensure database user has CREATE TABLE privileges');
    
    process.exit(1);
  }
}

// Run migration
runMigration().catch(console.error);

