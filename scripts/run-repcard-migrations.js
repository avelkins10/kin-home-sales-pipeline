#!/usr/bin/env node
/**
 * Run RepCard Complete Integration migrations
 * Usage: node scripts/run-repcard-migrations.js
 * Or: npm run migrate:repcard-complete
 */

const path = require('path')
require('dotenv').config({ path: path.join(__dirname, '../.env.local') })

const { sql } = require('@vercel/postgres')
const fs = require('fs')

async function runMigration(filePath, name) {
  console.log(`\n📦 Migration ${name}`)
  console.log('   Reading migration file...')
  
  if (!fs.existsSync(filePath)) {
    throw new Error(`Migration file not found: ${filePath}`)
  }
  
  const sqlContent = fs.readFileSync(filePath, 'utf-8')
  console.log('   Executing migration...')
  
  try {
    await sql.query(sqlContent)
    console.log(`✅ ${name} completed successfully`)
    return true
  } catch (error) {
    // Check if tables already exist (safe to ignore)
    if (error.message.includes('already exists') || error.message.includes('duplicate')) {
      console.log(`⚠️  ${name}: Some tables already exist (this is OK - safe to re-run)`)
      return true
    }
    throw error
  }
}

async function verifyMigrations() {
  console.log('\n🔍 Verifying migrations...')
  
  try {
    // Check for new tables
    const tables = await sql`
      SELECT table_name 
      FROM information_schema.tables 
      WHERE table_name LIKE 'repcard_%' 
      ORDER BY table_name
    `
    
    const tableNames = Array.isArray(tables) 
      ? tables.map(t => t.table_name)
      : tables.rows?.map(t => t.table_name) || []
    
    console.log(`\n📊 Found ${tableNames.length} RepCard tables:`)
    tableNames.forEach(name => console.log(`   ✅ ${name}`))
    
    // Check metrics are pre-populated
    const metrics = await sql`
      SELECT COUNT(*) as count FROM repcard_metric_definitions
    `
    const count = Array.isArray(metrics) ? metrics[0]?.count : metrics.rows?.[0]?.count || 0
    console.log(`\n📈 Metric definitions: ${count} (expected ~20)`)
    
    // Expected tables
    const expectedTables = [
      'repcard_calendars',
      'repcard_custom_fields',
      'repcard_customer_notes',
      'repcard_customer_statuses',
      'repcard_leaderboard_snapshots',
      'repcard_teams',
      'repcard_leaderboard_config',
      'repcard_analytics_config',
      'repcard_metric_definitions'
    ]
    
    const missing = expectedTables.filter(t => !tableNames.includes(t))
    if (missing.length > 0) {
      console.log(`\n⚠️  Missing tables: ${missing.join(', ')}`)
      return false
    }
    
    return true
  } catch (error) {
    console.error('❌ Verification failed:', error.message)
    return false
  }
}

async function main() {
  try {
    console.log('🚀 RepCard Complete Integration - Migrations')
    console.log('==========================================\n')
    
    // Verify DATABASE_URL is set
    if (!process.env.DATABASE_URL) {
      console.error('❌ DATABASE_URL environment variable is not set')
      console.error('💡 Make sure .env.local exists with DATABASE_URL configured')
      console.error('💡 Or set it: export DATABASE_URL="your-database-url"')
      process.exit(1)
    }
    
    console.log('✅ DATABASE_URL configured')
    
    // Run migration 016
    const migration016Path = path.join(__dirname, '..', 'lib', 'db', 'migrations', '016_repcard_complete_data.sql')
    await runMigration(migration016Path, '016_repcard_complete_data')
    
    // Run migration 017
    const migration017Path = path.join(__dirname, '..', 'lib', 'db', 'migrations', '017_repcard_settings.sql')
    await runMigration(migration017Path, '017_repcard_settings')
    
    // Verify
    const verified = await verifyMigrations()
    
    if (verified) {
      console.log('\n✅ All migrations completed successfully!')
      console.log('\n📋 Next steps:')
      console.log('1. ✅ Migrations complete')
      console.log('2. ⏭️  Wait for Vercel deployment to complete')
      console.log('3. ⏭️  Trigger sync: POST /api/admin/repcard/comprehensive-sync')
      console.log('4. ⏭️  Configure leaderboards in Settings → RepCard Config')
      process.exit(0)
    } else {
      console.log('\n⚠️  Migrations ran but verification found issues')
      console.log('Check the tables manually to ensure they exist')
      process.exit(1)
    }
  } catch (error) {
    console.error('\n❌ Migration failed:', error.message)
    console.error('\n🔧 Troubleshooting:')
    console.error('1. Check DATABASE_URL is correct')
    console.error('2. Verify database user has CREATE TABLE permissions')
    console.error('3. Check if tables already exist (safe to re-run migrations)')
    process.exit(1)
  }
}

main()

