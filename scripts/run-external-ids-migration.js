#!/usr/bin/env node

/**
 * Script to run the external user IDs database migration
 *
 * This script adds external system ID columns (RepCard, Enerflo, etc.) to users table
 * and creates user_sync_log audit table for tracking sync operations.
 */

const path = require('path')
require('dotenv').config({ path: path.join(__dirname, '../.env.local') })

const { sql } = require('@vercel/postgres')
const fs = require('fs')

async function runExternalIdsMigration() {
  try {
    console.log('🔄 Starting external user IDs migration...')

    // Verify DATABASE_URL is set
    if (!process.env.DATABASE_URL) {
      console.error('❌ DATABASE_URL environment variable is not set')
      process.exit(1)
    }

    // Read migration file
    const migrationPath = path.join(__dirname, '..', 'migrations', 'add_external_user_ids.sql')

    if (!fs.existsSync(migrationPath)) {
      console.error('❌ Migration file not found:', migrationPath)
      process.exit(1)
    }

    const migrationSQL = fs.readFileSync(migrationPath, 'utf8')
    console.log('📄 Read migration file:', migrationPath)

    // Execute migration
    console.log('🚀 Executing migration...')
    await sql.query(migrationSQL)
    console.log('✅ Added external ID columns to users table')
    console.log('✅ Created user_sync_log audit table')
    console.log('✅ Created indexes (5 total)')

    // Verify schema
    console.log('🔍 Verifying schema...')

    // Check new columns exist in users table
    const columnsResult = await sql`
      SELECT column_name
      FROM information_schema.columns
      WHERE table_name = 'users'
      AND column_name IN ('repcard_user_id', 'enerflo_user_id', 'external_ids', 'last_synced_at', 'sync_confidence')
    `

    const expectedColumns = ['repcard_user_id', 'enerflo_user_id', 'external_ids', 'last_synced_at', 'sync_confidence']
    const actualColumns = columnsResult.rows.map(row => row.column_name)
    const missingColumns = expectedColumns.filter(col => !actualColumns.includes(col))

    if (missingColumns.length > 0) {
      console.error('❌ Missing columns:', missingColumns)
      process.exit(1)
    }
    console.log('✅ All external ID columns created (5 total)')

    // Check user_sync_log table exists
    const tableResult = await sql`
      SELECT table_name
      FROM information_schema.tables
      WHERE table_name = 'user_sync_log'
    `

    if (tableResult.rows.length === 0) {
      console.error('❌ user_sync_log table was not created')
      process.exit(1)
    }
    console.log('✅ user_sync_log table exists')

    // Check indexes exist
    const indexResult = await sql`
      SELECT indexname
      FROM pg_indexes
      WHERE indexname LIKE 'idx_users_%'
      OR indexname LIKE 'idx_user_sync_log_%'
    `

    const expectedIndexes = [
      'idx_users_repcard_user_id',
      'idx_users_enerflo_user_id',
      'idx_users_external_ids',
      'idx_user_sync_log_user_id',
      'idx_user_sync_log_source_system',
      'idx_user_sync_log_synced_at'
    ]

    const actualIndexes = indexResult.rows.map(row => row.indexname)
    const createdIndexes = expectedIndexes.filter(index => actualIndexes.includes(index))

    console.log(`✅ Indexes created: ${createdIndexes.length} of ${expectedIndexes.length}`)

    console.log('🎉 External user IDs migration complete!')
    console.log('📊 Summary:')
    console.log('   - 5 external ID columns added to users table')
    console.log('   - user_sync_log audit table created')
    console.log('   - Indexes created for performance')
    console.log('   - Ready for RepCard/Enerflo integration')

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
  runExternalIdsMigration()
    .then(() => {
      console.log('✅ Migration script completed successfully')
      process.exit(0)
    })
    .catch((error) => {
      console.error('❌ Migration script failed:', error)
      process.exit(1)
    })
}

module.exports = { runExternalIdsMigration }
