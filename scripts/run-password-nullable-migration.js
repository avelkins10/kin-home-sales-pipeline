#!/usr/bin/env node

/**
 * Script to make password_hash nullable for external users
 *
 * External users from QuickBase are data records only, not login accounts.
 * They exist to link projects and metrics to stable IDs.
 */

const path = require('path');
require('dotenv').config({ path: path.join(__dirname, '../.env.local') });

const { sql } = require('@vercel/postgres');
const fs = require('fs');

async function runPasswordNullableMigration() {
  try {
    console.log('🔄 Making password_hash nullable for external users...');

    // Verify DATABASE_URL is set
    if (!process.env.DATABASE_URL) {
      console.error('❌ DATABASE_URL environment variable is not set');
      process.exit(1);
    }

    // Read migration file
    const migrationPath = path.join(__dirname, '..', 'migrations', 'make_password_hash_nullable.sql');

    if (!fs.existsSync(migrationPath)) {
      console.error('❌ Migration file not found:', migrationPath);
      process.exit(1);
    }

    const migrationSQL = fs.readFileSync(migrationPath, 'utf8');
    console.log('📄 Read migration file:', migrationPath);

    // Execute migration
    console.log('🚀 Executing migration...');
    await sql.query(migrationSQL);
    console.log('✅ password_hash column is now nullable');

    // Verify change
    console.log('🔍 Verifying schema...');
    const columnInfo = await sql`
      SELECT column_name, is_nullable, data_type
      FROM information_schema.columns
      WHERE table_name = 'users'
      AND column_name = 'password_hash'
    `;

    if (columnInfo.rows.length > 0 && columnInfo.rows[0].is_nullable === 'YES') {
      console.log('✅ Verified: password_hash is now nullable');
    } else {
      console.error('❌ Verification failed: password_hash is still NOT NULL');
      process.exit(1);
    }

    console.log('🎉 Migration complete!');
    console.log('📊 Summary:');
    console.log('   - password_hash column is now nullable');
    console.log('   - External users from QuickBase can now be created');
    console.log('\n💡 Next step:');
    console.log('   - Run seed script: node scripts/seed-active-reps-may-onwards.js');

  } catch (error) {
    console.error('❌ Migration failed:', error.message);
    console.error('Full error:', error);
    process.exit(1);
  } finally {
    // Close database connection
    await sql.end();
  }
}

// Run migration if called directly
if (require.main === module) {
  runPasswordNullableMigration()
    .then(() => {
      console.log('✅ Migration script completed successfully');
      process.exit(0);
    })
    .catch((error) => {
      console.error('❌ Migration script failed:', error);
      process.exit(1);
    });
}

module.exports = { runPasswordNullableMigration };
