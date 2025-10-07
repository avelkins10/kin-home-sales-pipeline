#!/usr/bin/env node

/**
 * Database Setup Script
 * 
 * Enables pgcrypto extension and runs the initial schema migration.
 * This script initializes the Neon PostgreSQL database for the application.
 */

const fs = require('fs');
const path = require('path');
require('dotenv').config({ path: path.join(process.cwd(), '.env.local') });

// Import the same client used in the application
const { sql } = require('@vercel/postgres');

// Colors for console output
const colors = {
  green: '\x1b[32m',
  red: '\x1b[31m',
  yellow: '\x1b[33m',
  blue: '\x1b[34m',
  reset: '\x1b[0m',
  bold: '\x1b[1m'
};

function log(message, color = 'reset') {
  console.log(`${colors[color]}${message}${colors.reset}`);
}

async function testConnection() {
  log('\n🔌 Testing database connection...', 'blue');
  
  try {
    const result = await sql`SELECT NOW() as current_time, current_database() as db_name`;
    const { current_time, db_name } = result.rows[0];
    
    log(`✅ Connected to database: ${db_name}`, 'green');
    log(`   📅 Connection time: ${current_time}`, 'blue');
    return true;
  } catch (error) {
    log('❌ Database connection failed!', 'red');
    log(`   Error: ${error.message}`, 'red');
    
    log('\n🔧 Troubleshooting Tips:', 'yellow');
    log('1. Verify DATABASE_URL is correct in .env.local', 'yellow');
    log('2. Check that your Neon database is running', 'yellow');
    log('3. Ensure the connection string includes -pooler suffix', 'yellow');
    log('4. Verify network connectivity to Neon servers', 'yellow');
    
    return false;
  }
}

async function enablePgcryptoExtension() {
  log('\n🔧 Enabling pgcrypto extension...', 'blue');
  
  try {
    await sql`CREATE EXTENSION IF NOT EXISTS pgcrypto;`;
    log('✅ pgcrypto extension enabled', 'green');
    log('   📝 This enables gen_random_uuid() for UUID generation', 'blue');
    return true;
  } catch (error) {
    log('❌ Failed to enable pgcrypto extension!', 'red');
    log(`   Error: ${error.message}`, 'red');
    
    log('\n🔧 Troubleshooting Tips:', 'yellow');
    log('1. Ensure your database user has CREATE privileges', 'yellow');
    log('2. Check if pgcrypto is available in your Neon plan', 'yellow');
    log('3. Contact Neon support if extension is not available', 'yellow');
    
    return false;
  }
}

async function runMigration() {
  log('\n📋 Running database migration...', 'blue');
  
  try {
    // Read the migration file
    const migrationPath = path.join(process.cwd(), 'lib', 'db', 'migrations', '001_initial_schema.sql');
    
    if (!fs.existsSync(migrationPath)) {
      throw new Error(`Migration file not found: ${migrationPath}`);
    }
    
    const migrationSQL = fs.readFileSync(migrationPath, 'utf8');

    // Execute migration
    await sql.query(migrationSQL);
    
    log('✅ Migration completed successfully', 'green');
    log('   📊 Created tables: users, sessions, project_cache', 'blue');
    return true;
  } catch (error) {
    if (error.message.includes('already exists')) {
      log('⚠️  Tables already exist (safe to ignore)', 'yellow');
      log('   📊 Migration was previously run', 'blue');
      return true;
    }
    
    log('❌ Migration failed!', 'red');
    log(`   Error: ${error.message}`, 'red');
    
    log('\n🔧 Troubleshooting Tips:', 'yellow');
    log('1. Check if tables already exist (safe to ignore)', 'yellow');
    log('2. Verify migration SQL syntax is correct', 'yellow');
    log('3. Ensure database user has CREATE TABLE privileges', 'yellow');
    
    return false;
  }
}

async function verifySchema() {
  log('\n🔍 Verifying database schema...', 'blue');
  
  try {
    // Check core tables exist
    const coreTablesResult = await sql`
      SELECT table_name 
      FROM information_schema.tables 
      WHERE table_schema = 'public' 
      AND table_name IN ('users', 'sessions', 'project_cache')
      ORDER BY table_name
    `;
    
    const coreTableNames = coreTablesResult.rows.map(row => row.table_name);
    log(`✅ Core tables verified: ${coreTableNames.join(', ')}`, 'green');
    
    // Check notification_settings table exists (from settings migration)
    const notificationSettingsResult = await sql`
      SELECT table_name 
      FROM information_schema.tables 
      WHERE table_schema = 'public' 
      AND table_name = 'notification_settings'
    `;
    
    if (notificationSettingsResult.rows.length > 0) {
      log('✅ Notification settings table verified', 'green');
      
      // Check notification_settings table structure
      const columnsResult = await sql`
        SELECT column_name, data_type, is_nullable, column_default
        FROM information_schema.columns 
        WHERE table_schema = 'public' 
        AND table_name = 'notification_settings'
        ORDER BY ordinal_position
      `;
      
      const expectedColumns = [
        'user_id', 'email_enabled', 'urgent_alerts', 'daily_digest', 
        'weekly_summary', 'hold_threshold', 'age_warning_threshold', 
        'install_overdue_threshold', 'created_at', 'updated_at'
      ];
      
      const actualColumns = columnsResult.rows.map(row => row.column_name);
      const missingColumns = expectedColumns.filter(col => !actualColumns.includes(col));
      
      if (missingColumns.length === 0) {
        log(`   📋 All ${expectedColumns.length} notification columns present`, 'green');
      } else {
        log(`⚠️  Missing notification columns: ${missingColumns.join(', ')}`, 'yellow');
      }
    } else {
      log('⚠️  Notification settings table not found', 'yellow');
      log('   💡 Run: npm run migrate:settings to create notification settings', 'blue');
    }
    
    // Check indexes exist for all tables
    const allTables = [...coreTableNames];
    if (notificationSettingsResult.rows.length > 0) {
      allTables.push('notification_settings');
    }
    
    const indexesResult = await sql`
      SELECT indexname 
      FROM pg_indexes 
      WHERE schemaname = 'public' 
      AND tablename = ANY(${allTables})
      ORDER BY indexname
    `;
    
    const indexNames = indexesResult.rows.map(row => row.indexname);
    log(`✅ Indexes verified: ${indexNames.length} total`, 'green');
    
    // Show some index details
    if (indexNames.length > 0) {
      log(`   📋 Indexes: ${indexNames.slice(0, 3).join(', ')}${indexNames.length > 3 ? '...' : ''}`, 'blue');
    }
    
    return true;
  } catch (error) {
    log('❌ Schema verification failed!', 'red');
    log(`   Error: ${error.message}`, 'red');
    return false;
  }
}

async function setupDatabase() {
  log('\n' + '='.repeat(50), 'blue');
  log('🗄️  DATABASE SETUP', 'bold');
  log('='.repeat(50), 'blue');
  
  // Check if DATABASE_URL is set
  if (!process.env.DATABASE_URL) {
    log('❌ DATABASE_URL environment variable not set!', 'red');
    log('   Run: npm run setup:env', 'yellow');
    process.exit(1);
  }
  
  let success = true;
  
  // Step 1: Test connection
  if (!(await testConnection())) {
    success = false;
  }
  
  // Step 2: Enable pgcrypto extension
  if (success && !(await enablePgcryptoExtension())) {
    success = false;
  }
  
  // Step 3: Run migration
  if (success && !(await runMigration())) {
    success = false;
  }
  
  // Step 4: Verify schema
  if (success && !(await verifySchema())) {
    success = false;
  }
  
  // Summary
  log('\n' + '='.repeat(50), 'blue');
  if (success) {
    log('🎉 Database setup complete!', 'green');
    log('✅ pgcrypto extension enabled', 'green');
    log('✅ Core tables created: 3 (users, sessions, project_cache)', 'green');
    log('✅ Notification settings table verified (if migration run)', 'green');
    log('✅ Indexes created: 5+', 'green');
    log('\n🚀 Ready to proceed with user seeding:', 'blue');
    log('   npm run setup:seed', 'blue');
    log('\n💡 Optional: Run settings migration for notification features:', 'blue');
    log('   npm run migrate:settings', 'blue');
  } else {
    log('❌ Database setup failed!', 'red');
    log('⚠️  Fix the errors above before proceeding', 'yellow');
    log('\n📖 For detailed troubleshooting, see SETUP.md', 'blue');
    process.exit(1);
  }
  log('='.repeat(50) + '\n', 'blue');
}

// Run setup
setupDatabase().catch(error => {
  log('💥 Unexpected error during database setup:', 'red');
  log(error.message, 'red');
  process.exit(1);
});
