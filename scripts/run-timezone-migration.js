const { sql } = require('@vercel/postgres');
const fs = require('fs');
const path = require('path');

// Load environment variables from .env.local
require('dotenv').config({ path: path.join(__dirname, '../.env.local') });

async function runMigration() {
  try {
    console.log('🚀 Starting timezone migration...');

    // Verify DATABASE_URL is set
    if (!process.env.DATABASE_URL) {
      throw new Error('DATABASE_URL environment variable is required');
    }

    // Read migration file
    const migrationPath = path.join(__dirname, '../lib/db/migrations/008_add_user_timezone.sql');
    const migrationSQL = fs.readFileSync(migrationPath, 'utf8');

    // Execute migration
    console.log('📝 Executing migration SQL...');
    await sql.query(migrationSQL);

    console.log('✅ Added timezone column to users table');
    console.log('✅ Created index on timezone column');

    // Verify schema
    console.log('🔍 Verifying schema...');

    // Check timezone column exists
    const columnCheck = await sql.query(`
      SELECT column_name, column_default, data_type
      FROM information_schema.columns
      WHERE table_name = 'users'
      AND column_name = 'timezone'
    `);

    if (columnCheck.rows.length === 0) {
      throw new Error('timezone column not found in users table');
    }

    console.log('✅ timezone column exists');
    console.log(`   Type: ${columnCheck.rows[0].data_type}`);
    console.log(`   Default: ${columnCheck.rows[0].column_default}`);

    // Check index exists
    const indexCheck = await sql.query(`
      SELECT indexname
      FROM pg_indexes
      WHERE tablename = 'users'
      AND indexname = 'idx_users_timezone'
    `);

    if (indexCheck.rows.length === 0) {
      throw new Error('idx_users_timezone index not found');
    }

    console.log('✅ idx_users_timezone index exists');
    console.log('✅ Timezone migration complete');
    console.log('📊 Schema verification passed');

  } catch (error) {
    if (error.message.includes('already exists') || error.message.includes('duplicate column')) {
      console.log('⚠️  Column already exists (safe to ignore)');
      console.log('   📊 Migration was previously run');
      return;
    }

    console.error('❌ Migration failed!');
    console.error(`   Error: ${error.message}`);

    console.log('\n🔧 Troubleshooting Tips:');
    console.log('1. Check if column already exists (safe to ignore)');
    console.log('2. Verify migration SQL syntax is correct');
    console.log('3. Ensure database user has ALTER TABLE privileges');

    process.exit(1);
  }
}

// Run migration
runMigration().catch(console.error);
