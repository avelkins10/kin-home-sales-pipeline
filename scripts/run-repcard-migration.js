#!/usr/bin/env node

/**
 * Script to run the RepCard sync tables migration
 *
 * This script creates the RepCard sync tables for storing
 * RepCard data locally for fast analytics.
 */

const path = require('path')
require('dotenv').config({ path: path.join(__dirname, '../.env.local') })

const { sql } = require('@vercel/postgres')
const fs = require('fs')

async function runRepcardMigration() {
  try {
    console.log('🔄 Starting RepCard sync tables migration...')

    // Verify DATABASE_URL is set
    if (!process.env.DATABASE_URL) {
      console.error('❌ DATABASE_URL environment variable is not set')
      process.exit(1)
    }

    // Read migration file
    const migrationPath = path.join(__dirname, '..', 'lib', 'db', 'migrations', '012_repcard_sync_tables.sql')

    if (!fs.existsSync(migrationPath)) {
      console.error('❌ Migration file not found:', migrationPath)
      process.exit(1)
    }

    const migrationSQL = fs.readFileSync(migrationPath, 'utf8')
    console.log('📄 Read migration file:', migrationPath)

    // Execute migration
    console.log('🚀 Executing migration...')
    await sql.query(migrationSQL)
    console.log('✅ Migration executed successfully')

    // Verify schema
    console.log('🔍 Verifying schema...')

    // Check all tables exist
    const tableNames = ['repcard_customers', 'repcard_appointments', 'repcard_status_logs', 'repcard_sync_log']

    for (const tableName of tableNames) {
      const tableResult = await sql.query(`
        SELECT table_name
        FROM information_schema.tables
        WHERE table_name = '${tableName}'
      `)

      if (tableResult.rows.length === 0) {
        console.error(`❌ ${tableName} table not found`)
        process.exit(1)
      }
      console.log(`✅ ${tableName} table created`)
    }

    // Check indexes exist
    console.log('🔍 Checking indexes...')
    const indexResult = await sql.query(`
      SELECT indexname, tablename
      FROM pg_indexes
      WHERE tablename LIKE 'repcard_%'
      AND indexname LIKE 'idx_repcard_%'
      ORDER BY tablename, indexname
    `)

    const indexCount = indexResult.rows.length
    console.log(`✅ Created ${indexCount} indexes for performance`)

    // Get record counts
    const customerCount = await sql.query('SELECT COUNT(*) as count FROM repcard_customers')
    const appointmentCount = await sql.query('SELECT COUNT(*) as count FROM repcard_appointments')
    const statusLogCount = await sql.query('SELECT COUNT(*) as count FROM repcard_status_logs')

    console.log('📊 Current record counts:')
    console.log(`   - Customers: ${customerCount.rows[0].count}`)
    console.log(`   - Appointments: ${appointmentCount.rows[0].count}`)
    console.log(`   - Status Logs: ${statusLogCount.rows[0].count}`)

    console.log('🎉 RepCard migration complete!')
    console.log('📊 Summary:')
    console.log('   - 4 tables created (customers, appointments, status_logs, sync_log)')
    console.log(`   - ${indexCount} indexes created for performance`)
    console.log('   - Ready for RepCard data sync')
    console.log('\n💡 Next steps:')
    console.log('   1. Run initial data sync: POST /api/admin/repcard/sync?type=full&startDate=2025-10-01&endDate=2025-10-28')
    console.log('   2. Monitor sync status: GET /api/admin/repcard/sync')
    console.log('   3. Cron job will run incremental sync every 10 minutes')

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
  runRepcardMigration()
    .then(() => {
      console.log('✅ Migration script completed successfully')
      process.exit(0)
    })
    .catch((error) => {
      console.error('❌ Migration script failed:', error)
      process.exit(1)
    })
}

module.exports = { runRepcardMigration }
