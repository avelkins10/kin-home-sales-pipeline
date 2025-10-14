const { sql } = require('@vercel/postgres');
const fs = require('fs');
const path = require('path');

// Load environment variables from .env.local
require('dotenv').config({ path: path.join(__dirname, '../.env.local') });

async function runOfficeIdMigration() {
  try {
    console.log('🚀 Starting office ID migration...');

    // Verify DATABASE_URL is set
    if (!process.env.DATABASE_URL) {
      throw new Error('DATABASE_URL environment variable is required');
    }

    // Read migration file
    const migrationPath = path.join(__dirname, '../lib/db/migrations/009_add_office_quickbase_id.sql');
    const migrationSQL = fs.readFileSync(migrationPath, 'utf8');

    // Execute migration
    console.log('📝 Executing migration SQL...');
    await sql.query(migrationSQL);

    console.log('✅ Added quickbase_office_id column to offices table');
    console.log('✅ Created index on quickbase_office_id');
    console.log('✅ Created unique constraint on quickbase_office_id');

    // Verify schema
    console.log('🔍 Verifying schema...');

    // Check quickbase_office_id column
    const columnCheck = await sql.query(`
      SELECT column_name FROM information_schema.columns
      WHERE table_name = 'offices' AND column_name = 'quickbase_office_id'
    `);
    if (columnCheck.rows.length === 0) {
      throw new Error('quickbase_office_id column not found');
    }

    // Check indexes
    const indexesCheck = await sql.query(`
      SELECT indexname FROM pg_indexes
      WHERE tablename = 'offices'
      AND indexname LIKE '%quickbase%'
    `);
    console.log(`✅ Found ${indexesCheck.rows.length} QuickBase ID indexes: ${indexesCheck.rows.map(r => r.indexname).join(', ')}`);

    console.log('✅ Office ID migration complete');
    console.log('📊 Schema verification passed');

  } catch (error) {
    console.error('❌ Migration failed:', error.message);

    // Handle specific errors gracefully
    if (error.message.includes('already exists')) {
      console.log('ℹ️  Schema elements already exist - this is normal for re-runs');
      console.log('   📊 Migration was previously run');
      return;
    } else {
      console.error('Full error:', error);
      process.exit(1);
    }
  }
}

runOfficeIdMigration();
