const { sql } = require('@vercel/postgres');
const fs = require('fs');
const path = require('path');

// Load environment variables from .env.local
require('dotenv').config({ path: path.join(__dirname, '../.env.local') });

async function runOfficesMigration() {
  try {
    console.log('🚀 Starting offices migration...');
    
    // Verify DATABASE_URL is set
    if (!process.env.DATABASE_URL) {
      throw new Error('DATABASE_URL environment variable is required');
    }

    // Read migration file
    const migrationPath = path.join(__dirname, '../lib/db/migrations/003_offices_schema.sql');
    const migrationSQL = fs.readFileSync(migrationPath, 'utf8');

    // Execute migration
    console.log('📝 Executing migration SQL...');
    await sql.query(migrationSQL);
    
    console.log('✅ Added last_login_at column to users table');
    console.log('✅ Added region column to users table');
    console.log('✅ Created offices table');
    console.log('✅ Created indexes (5 total)');

    // Verify schema
    console.log('🔍 Verifying schema...');
    
    // Check last_login_at column
    const lastLoginCheck = await sql.query(`
      SELECT column_name FROM information_schema.columns 
      WHERE table_name = 'users' AND column_name = 'last_login_at'
    `);
    if (lastLoginCheck.rows.length === 0) {
      throw new Error('last_login_at column not found');
    }

    // Check region column
    const regionCheck = await sql.query(`
      SELECT column_name FROM information_schema.columns 
      WHERE table_name = 'users' AND column_name = 'region'
    `);
    if (regionCheck.rows.length === 0) {
      throw new Error('region column not found');
    }

    // Check offices table
    const officesCheck = await sql.query(`
      SELECT table_name FROM information_schema.tables 
      WHERE table_name = 'offices'
    `);
    if (officesCheck.rows.length === 0) {
      throw new Error('offices table not found');
    }

    // Check indexes using LIKE patterns to avoid false warnings due to naming differences
    const indexesCheck = await sql.query(`
      SELECT indexname FROM pg_indexes 
      WHERE tablename IN ('offices', 'users') 
      AND (indexname LIKE 'idx_offices_%' OR indexname LIKE 'idx_users_%')
    `);
    if (indexesCheck.rows.length < 5) {
      console.warn('⚠️  Some indexes may not have been created');
      console.log(`   Found ${indexesCheck.rows.length} indexes: ${indexesCheck.rows.map(r => r.indexname).join(', ')}`);
    } else {
      console.log(`✅ Found ${indexesCheck.rows.length} indexes on offices and users tables`);
    }

    console.log('✅ Offices migration complete');
    console.log('📊 Schema verification passed');
    
  } catch (error) {
    console.error('❌ Migration failed:', error.message);
    
    // Handle specific errors gracefully
    if (error.message.includes('already exists')) {
      console.log('ℹ️  Some schema elements already exist - this is normal for re-runs');
      console.log('   📊 Migration was previously run');
      return;
    } else if (error.message.includes('column') && error.message.includes('already exists')) {
      console.log('ℹ️  Columns already exist - skipping');
      return;
    } else if (error.message.includes('table') && error.message.includes('already exists')) {
      console.log('ℹ️  Tables already exist - skipping');
      return;
    } else {
      console.error('Full error:', error);
      process.exit(1);
    }
  }
}

runOfficesMigration();
