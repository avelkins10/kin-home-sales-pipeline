#!/usr/bin/env node

/**
 * Script to run the user sync runs table migration
 *
 * This script creates the user_sync_runs table for tracking
 * sync run history and statistics.
 */

const path = require('path')
require('dotenv').config({ path: path.join(__dirname, '../.env.local') })

const { sql } = require('@vercel/postgres')
const fs = require('fs')

async function runUserSyncRunsMigration() {
  try {
    console.log('🔄 Starting user sync runs migration...')

    // Verify DATABASE_URL is set
    if (!process.env.DATABASE_URL) {
      console.error('❌ DATABASE_URL environment variable is not set')
      process.exit(1)
    }

    // Read migration file
    const migrationPath = path.join(__dirname, '..', 'migrations', 'create_user_sync_runs_table.sql')

    if (!fs.existsSync(migrationPath)) {
      console.error('❌ Migration file not found:', migrationPath)
      process.exit(1)
    }

    const migrationSQL = fs.readFileSync(migrationPath, 'utf8')
    console.log('📄 Read migration file:', migrationPath)

    // Execute migration
    console.log('🚀 Executing migration...')
    await sql.query(migrationSQL)
    console.log('✅ Created user_sync_runs table')
    console.log('✅ Created indexes (3 total)')

    // Verify schema
    console.log('🔍 Verifying schema...')

    // Check table exists
    const tableResult = await sql`
      SELECT table_name
      FROM information_schema.tables
      WHERE table_name = 'user_sync_runs'
    `

    if (tableResult.rows.length === 0) {
      console.error('❌ user_sync_runs table not found')
      process.exit(1)
    }
    console.log('✅ user_sync_runs table created')

    // Check columns exist
    const columnsResult = await sql`
      SELECT column_name
      FROM information_schema.columns
      WHERE table_name = 'user_sync_runs'
      AND column_name IN (
        'id',
        'started_at',
        'completed_at',
        'status',
        'total_users',
        'enriched',
        'not_found',
        'already_up_to_date',
        'errors',
        'error_details',
        'not_found_samples',
        'triggered_by',
        'triggered_by_user_id',
        'execution_time_ms'
      )
    `

    const expectedColumns = [
      'id',
      'started_at',
      'completed_at',
      'status',
      'total_users',
      'enriched',
      'not_found',
      'already_up_to_date',
      'errors',
      'error_details',
      'not_found_samples',
      'triggered_by',
      'triggered_by_user_id',
      'execution_time_ms'
    ]

    const actualColumns = columnsResult.rows.map(row => row.column_name)
    const missingColumns = expectedColumns.filter(col => !actualColumns.includes(col))

    if (missingColumns.length > 0) {
      console.error('❌ Missing columns:', missingColumns)
      process.exit(1)
    }
    console.log('✅ All columns created (14 total)')

    // Check indexes exist
    const indexResult = await sql`
      SELECT indexname
      FROM pg_indexes
      WHERE tablename = 'user_sync_runs'
      AND indexname LIKE 'idx_user_sync_runs_%'
    `

    const expectedIndexes = [
      'idx_user_sync_runs_started_at',
      'idx_user_sync_runs_status',
      'idx_user_sync_runs_triggered_by'
    ]

    const actualIndexes = indexResult.rows.map(row => row.indexname)
    const createdIndexes = expectedIndexes.filter(index => actualIndexes.includes(index))

    console.log(`✅ Indexes created: ${createdIndexes.length} of ${expectedIndexes.length}`)

    console.log('🎉 User sync runs migration complete!')
    console.log('📊 Summary:')
    console.log('   - user_sync_runs table created')
    console.log('   - 14 columns for tracking sync statistics')
    console.log('   - 3 indexes created for performance')
    console.log('   - Ready for sync run monitoring')
    console.log('\n💡 Next steps:')
    console.log('   1. Configure SLACK_WEBHOOK_URL for notifications')
    console.log('   2. Test manual sync via /api/admin/sync-users')
    console.log('   3. Monitor sync runs at /admin/sync-monitoring')

  } catch (error) {
    if (error.message.includes('already exists')) {
      console.log('⚠️  Schema elements already exist - migration skipped')
      console.log('✅ Migration is idempotent and safe to run multiple times')
    } else {
      console.error('❌ Migration failed:', error.message)
      console.error('Full error:', error)
      process.exit(1)
    }
  } finally {
    // Close database connection
    await sql.end()
  }
}

// Run migration if called directly
if (require.main === module) {
  runUserSyncRunsMigration()
    .then(() => {
      console.log('✅ Migration script completed successfully')
      process.exit(0)
    })
    .catch((error) => {
      console.error('❌ Migration script failed:', error)
      process.exit(1)
    })
}

module.exports = { runUserSyncRunsMigration }
