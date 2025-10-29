/**
 * Run RepCard Comprehensive Tables Migration
 * Creates tables for users, offices, and attachments
 */

const { readFileSync, existsSync } = require('fs');
const path = require('path');
const fs = require('fs');

// Load environment variables
require('dotenv').config({ path: path.join(__dirname, '../.env.local') });

async function runMigration() {
  try {
    console.log('🚀 Starting RepCard comprehensive tables migration...');
    
    // Check for DATABASE_URL
    if (!process.env.DATABASE_URL) {
      throw new Error('DATABASE_URL environment variable is not set');
    }

    // Import postgres client (same as other migration scripts)
    const { sql } = require('@vercel/postgres');

    // Read migration file
    const migrationPath = path.join(__dirname, '..', 'lib', 'db', 'migrations', '014_repcard_comprehensive_tables.sql');
    
    if (!fs.existsSync(migrationPath)) {
      throw new Error(`Migration file not found: ${migrationPath}`);
    }

    const migrationSQL = readFileSync(migrationPath, 'utf8');
    console.log('📝 Executing migration SQL...');
    
    // Execute migration
    await sql.query(migrationSQL);

    console.log('✅ RepCard comprehensive tables migration completed successfully');
    console.log('   📊 Created tables: repcard_users, repcard_offices, repcard_customer_attachments, repcard_appointment_attachments, repcard_leaderboard_snapshots');
    
    // Close database connection
    await sql.end();
    
    process.exit(0);
  } catch (error) {
    console.error('❌ Migration failed:', error.message);
    
    if (error.message.includes('already exists')) {
      console.log('⚠️  Tables already exist (safe to ignore)');
      process.exit(0);
    }
    
    process.exit(1);
  }
}

runMigration();

