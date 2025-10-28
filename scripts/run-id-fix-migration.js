#!/usr/bin/env node

/**
 * Script to fix RepCard ID types from INTEGER to TEXT
 *
 * RepCard API returns IDs as hex strings (e.g., "6901159a7a2eb5b0ed0a231e")
 * but the initial migration incorrectly used INTEGER type.
 */

const path = require('path')
require('dotenv').config({ path: path.join(__dirname, '../.env.local') })

const { sql } = require('@vercel/postgres')
const fs = require('fs')

async function runIdFixMigration() {
  try {
    console.log('🔄 Starting RepCard ID type fix migration...')

    // Verify DATABASE_URL is set
    if (!process.env.DATABASE_URL) {
      console.error('❌ DATABASE_URL environment variable is not set')
      process.exit(1)
    }

    // Read migration file
    const migrationPath = path.join(__dirname, '..', 'lib', 'db', 'migrations', '013_fix_repcard_id_types.sql')

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

    // Verify column types
    console.log('🔍 Verifying column types...')

    const customerIdType = await sql.query(`
      SELECT data_type
      FROM information_schema.columns
      WHERE table_name = 'repcard_customers'
      AND column_name = 'repcard_customer_id'
    `)

    const appointmentIdType = await sql.query(`
      SELECT data_type
      FROM information_schema.columns
      WHERE table_name = 'repcard_appointments'
      AND column_name = 'repcard_appointment_id'
    `)

    const logIdType = await sql.query(`
      SELECT data_type
      FROM information_schema.columns
      WHERE table_name = 'repcard_status_logs'
      AND column_name = 'repcard_log_id'
    `)

    console.log('✅ repcard_customers.repcard_customer_id:', customerIdType.rows[0].data_type)
    console.log('✅ repcard_appointments.repcard_appointment_id:', appointmentIdType.rows[0].data_type)
    console.log('✅ repcard_status_logs.repcard_log_id:', logIdType.rows[0].data_type)

    // Check indexes were recreated
    console.log('🔍 Checking indexes...')
    const indexResult = await sql.query(`
      SELECT indexname, tablename
      FROM pg_indexes
      WHERE tablename LIKE 'repcard_%'
      AND indexname LIKE 'idx_repcard_%'
      ORDER BY tablename, indexname
    `)

    const indexCount = indexResult.rows.length
    console.log(`✅ ${indexCount} indexes verified`)

    console.log('🎉 ID type fix migration complete!')
    console.log('📊 Summary:')
    console.log('   - Changed repcard_customer_id from INTEGER → TEXT')
    console.log('   - Changed repcard_appointment_id from INTEGER → TEXT')
    console.log('   - Changed repcard_log_id from INTEGER → TEXT')
    console.log('   - Changed all user/office IDs from INTEGER → TEXT')
    console.log(`   - Recreated ${indexCount} indexes`)
    console.log('\n💡 Next step:')
    console.log('   Run full sync: POST /api/admin/repcard/sync?type=full&startDate=2025-10-01&endDate=2025-10-28')

  } catch (error) {
    console.error('❌ Migration failed:', error.message)
    console.error('Full error:', error)
    process.exit(1)
  } finally {
    // Close database connection
    await sql.end()
  }
}

// Run migration if called directly
if (require.main === module) {
  runIdFixMigration()
    .then(() => {
      console.log('✅ Migration script completed successfully')
      process.exit(0)
    })
    .catch((error) => {
      console.error('❌ Migration script failed:', error)
      process.exit(1)
    })
}

module.exports = { runIdFixMigration }
