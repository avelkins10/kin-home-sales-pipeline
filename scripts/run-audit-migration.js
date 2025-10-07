#!/usr/bin/env node

/**
 * Script to run the audit logs database migration
 *
 * This script executes the 005_audit_logs_schema.sql migration to add audit_logs table.
 */

const path = require('path')
require('dotenv').config({ path: path.join(__dirname, '../.env.local') })

const { sql } = require('@vercel/postgres')
const fs = require('fs')

async function runAuditMigration() {
  try {
    console.log('🔄 Starting audit logs migration...')
    
    // Verify DATABASE_URL is set
    if (!process.env.DATABASE_URL) {
      console.error('❌ DATABASE_URL environment variable is not set')
      process.exit(1)
    }

    // Read migration file
    const migrationPath = path.join(__dirname, '..', 'lib', 'db', 'migrations', '005_audit_logs_schema.sql')
    
    if (!fs.existsSync(migrationPath)) {
      console.error('❌ Migration file not found:', migrationPath)
      process.exit(1)
    }

    const migrationSQL = fs.readFileSync(migrationPath, 'utf8')
    console.log('📄 Read migration file:', migrationPath)

    // Execute migration
    console.log('🚀 Executing migration...')
    await sql.query(migrationSQL)
    console.log('✅ Created audit_logs table')
    console.log('✅ Created indexes (6 total)')

    // Verify schema
    console.log('🔍 Verifying schema...')
    
    // Check audit_logs table exists
    const tableResult = await sql`
      SELECT table_name 
      FROM information_schema.tables 
      WHERE table_name = 'audit_logs'
    `
    
    if (tableResult.rows.length === 0) {
      console.error('❌ audit_logs table was not created')
      process.exit(1)
    }
    console.log('✅ audit_logs table exists')

    // Check indexes exist
    const indexResult = await sql`
      SELECT indexname 
      FROM pg_indexes 
      WHERE tablename = 'audit_logs'
    `
    
    const expectedIndexes = [
      'idx_audit_logs_timestamp',
      'idx_audit_logs_user',
      'idx_audit_logs_action',
      'idx_audit_logs_resource',
      'idx_audit_logs_changes',
      'idx_audit_logs_timestamp_action'
    ]
    
    const actualIndexes = indexResult.rows.map(row => row.indexname)
    const missingIndexes = expectedIndexes.filter(index => !actualIndexes.includes(index))
    
    if (missingIndexes.length > 0) {
      console.error('❌ Missing indexes:', missingIndexes)
      process.exit(1)
    }
    console.log('✅ All indexes created (6 total)')

    console.log('🎉 Audit logs migration complete!')
    console.log('📊 Summary:')
    console.log('   - audit_logs table created')
    console.log('   - 6 indexes created for efficient filtering')
    console.log('   - Ready for audit logging')

  } catch (error) {
    if (error.message.includes('already exists')) {
      console.log('⚠️  audit_logs table already exists - migration skipped')
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
  runAuditMigration()
    .then(() => {
      console.log('✅ Migration script completed successfully')
      process.exit(0)
    })
    .catch((error) => {
      console.error('❌ Migration script failed:', error)
      process.exit(1)
    })
}

module.exports = { runAuditMigration }