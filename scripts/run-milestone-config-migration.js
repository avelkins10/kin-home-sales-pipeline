#!/usr/bin/env node

/**
 * Script to run the milestone configurations database migration
 *
 * This script executes the 007_milestone_configurations_schema.sql migration
 * to add milestone_configurations and milestone_configuration_history tables.
 */

const path = require('path')
require('dotenv').config({ path: path.join(__dirname, '../.env.local') })

const { sql } = require('@vercel/postgres')
const fs = require('fs')

async function runMilestoneConfigMigration() {
  try {
    console.log('🔄 Starting milestone configurations migration...')

    // Verify DATABASE_URL is set
    if (!process.env.DATABASE_URL) {
      console.error('❌ DATABASE_URL environment variable is not set')
      process.exit(1)
    }

    // Read migration file
    const migrationPath = path.join(__dirname, '..', 'lib', 'db', 'migrations', '007_milestone_configurations_schema.sql')

    if (!fs.existsSync(migrationPath)) {
      console.error('❌ Migration file not found:', migrationPath)
      process.exit(1)
    }

    const migrationSQL = fs.readFileSync(migrationPath, 'utf8')
    console.log('📄 Read migration file:', migrationPath)

    // Execute migration
    console.log('🚀 Executing migration...')
    await sql.query(migrationSQL)
    console.log('✅ Created milestone_configurations table')
    console.log('✅ Created milestone_configuration_history table')
    console.log('✅ Created indexes and triggers')

    // Verify schema
    console.log('🔍 Verifying schema...')

    // Check milestone_configurations table exists
    const configTableResult = await sql`
      SELECT table_name
      FROM information_schema.tables
      WHERE table_name = 'milestone_configurations'
    `

    if (configTableResult.rows.length === 0) {
      console.error('❌ milestone_configurations table was not created')
      process.exit(1)
    }
    console.log('✅ milestone_configurations table exists')

    // Check milestone_configuration_history table exists
    const historyTableResult = await sql`
      SELECT table_name
      FROM information_schema.tables
      WHERE table_name = 'milestone_configuration_history'
    `

    if (historyTableResult.rows.length === 0) {
      console.error('❌ milestone_configuration_history table was not created')
      process.exit(1)
    }
    console.log('✅ milestone_configuration_history table exists')

    // Check indexes exist
    const indexResult = await sql`
      SELECT indexname
      FROM pg_indexes
      WHERE tablename = 'milestone_configurations'
    `

    const expectedIndexes = [
      'idx_milestone_configurations_milestone_id',
      'idx_milestone_configurations_active'
    ]

    const actualIndexes = indexResult.rows.map(row => row.indexname)
    const missingIndexes = expectedIndexes.filter(index => !actualIndexes.includes(index))

    if (missingIndexes.length > 0) {
      console.error('❌ Missing indexes:', missingIndexes)
      process.exit(1)
    }
    console.log('✅ All indexes created')

    // Check trigger exists
    const triggerResult = await sql`
      SELECT trigger_name
      FROM information_schema.triggers
      WHERE event_object_table = 'milestone_configurations'
        AND trigger_name = 'trigger_update_milestone_configuration'
    `

    if (triggerResult.rows.length === 0) {
      console.error('❌ Trigger was not created')
      process.exit(1)
    }
    console.log('✅ Update trigger created')

    console.log('🎉 Milestone configurations migration complete!')
    console.log('📊 Summary:')
    console.log('   - milestone_configurations table created')
    console.log('   - milestone_configuration_history table created')
    console.log('   - Indexes created for efficient lookups')
    console.log('   - Version tracking trigger configured')
    console.log('   - Ready for custom milestone configurations')

  } catch (error) {
    if (error.message.includes('already exists')) {
      console.log('⚠️  Tables already exist - migration skipped')
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
  runMilestoneConfigMigration()
    .then(() => {
      console.log('✅ Migration script completed successfully')
      process.exit(0)
    })
    .catch((error) => {
      console.error('❌ Migration script failed:', error)
      process.exit(1)
    })
}

module.exports = { runMilestoneConfigMigration }
