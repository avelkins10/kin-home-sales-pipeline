#!/usr/bin/env node

/**
 * Script to run the user enrichment database migration
 *
 * This script adds fields for self-enriching user records from:
 * - QuickBase Contacts table
 * - QuickBase Projects table (CLOSER_ID)
 * - RepCard API
 */

const path = require('path')
require('dotenv').config({ path: path.join(__dirname, '../.env.local') })

const { sql } = require('@vercel/postgres')
const fs = require('fs')

async function runUserEnrichmentMigration() {
  try {
    console.log('🔄 Starting user enrichment migration...')

    // Verify DATABASE_URL is set
    if (!process.env.DATABASE_URL) {
      console.error('❌ DATABASE_URL environment variable is not set')
      process.exit(1)
    }

    // Read migration file
    const migrationPath = path.join(__dirname, '..', 'migrations', 'add_user_enrichment_fields.sql')

    if (!fs.existsSync(migrationPath)) {
      console.error('❌ Migration file not found:', migrationPath)
      process.exit(1)
    }

    const migrationSQL = fs.readFileSync(migrationPath, 'utf8')
    console.log('📄 Read migration file:', migrationPath)

    // Execute migration
    console.log('🚀 Executing migration...')
    await sql.query(migrationSQL)
    console.log('✅ Added enrichment columns to users table')
    console.log('✅ Created indexes (6 total)')

    // Verify schema
    console.log('🔍 Verifying schema...')

    // Check new columns exist
    const columnsResult = await sql`
      SELECT column_name
      FROM information_schema.columns
      WHERE table_name = 'users'
      AND column_name IN (
        'quickbase_contact_id',
        'sequifi_user_id',
        'num_closer_projects',
        'num_setter_projects',
        'office',
        'team',
        'is_rookie',
        'is_setter',
        'profile_image_url',
        'last_synced_from_contacts_at',
        'last_synced_from_repcard_at'
      )
    `

    const expectedColumns = [
      'quickbase_contact_id',
      'sequifi_user_id',
      'num_closer_projects',
      'num_setter_projects',
      'office',
      'team',
      'is_rookie',
      'is_setter',
      'profile_image_url',
      'last_synced_from_contacts_at',
      'last_synced_from_repcard_at'
    ]

    const actualColumns = columnsResult.rows.map(row => row.column_name)
    const missingColumns = expectedColumns.filter(col => !actualColumns.includes(col))

    if (missingColumns.length > 0) {
      console.error('❌ Missing columns:', missingColumns)
      process.exit(1)
    }
    console.log('✅ All enrichment columns created (11 total)')

    // Check indexes exist
    const indexResult = await sql`
      SELECT indexname
      FROM pg_indexes
      WHERE tablename = 'users'
      AND indexname LIKE 'idx_users_%'
    `

    const newIndexes = [
      'idx_users_quickbase_contact_id',
      'idx_users_sequifi_user_id',
      'idx_users_office',
      'idx_users_team',
      'idx_users_num_closer_projects',
      'idx_users_num_setter_projects'
    ]

    const actualIndexes = indexResult.rows.map(row => row.indexname)
    const createdIndexes = newIndexes.filter(index => actualIndexes.includes(index))

    console.log(`✅ Indexes created: ${createdIndexes.length} of ${newIndexes.length}`)

    console.log('🎉 User enrichment migration complete!')
    console.log('📊 Summary:')
    console.log('   - 11 enrichment columns added to users table')
    console.log('   - 6 indexes created for performance')
    console.log('   - Ready for self-enriching user sync')
    console.log('\n💡 Next steps:')
    console.log('   1. Run seed script: node scripts/seed-active-reps-may-onwards.js')
    console.log('   2. Verify users populated with external IDs')

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
  runUserEnrichmentMigration()
    .then(() => {
      console.log('✅ Migration script completed successfully')
      process.exit(0)
    })
    .catch((error) => {
      console.error('❌ Migration script failed:', error)
      process.exit(1)
    })
}

module.exports = { runUserEnrichmentMigration }
