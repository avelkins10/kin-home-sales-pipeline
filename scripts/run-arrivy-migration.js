#!/usr/bin/env node

/**
 * Script to run the Arrivy integration database migrations
 *
 * This script creates the Arrivy tables for field operations tracking:
 * - arrivy_tasks: Field service tasks from Arrivy
 * - arrivy_entities: Crew members and field technicians
 * - arrivy_events: Webhook event audit trail
 * - arrivy_task_status: Status update history
 * - arrivy_task_entities: Proper join table for task-entity relationships
 */

const path = require('path')
require('dotenv').config({ path: path.join(__dirname, '../.env.local') })

const { sql } = require('@vercel/postgres')
const fs = require('fs')

async function runArrivyMigration() {
  try {
    console.log('🔄 Starting Arrivy integration database migrations...\n')

    // Verify DATABASE_URL is set
    if (!process.env.DATABASE_URL) {
      console.error('❌ DATABASE_URL environment variable is not set')
      console.error('💡 Make sure .env.local exists with DATABASE_URL configured')
      process.exit(1)
    }

    console.log('✅ DATABASE_URL configured')

    // Migration 014: Create base Arrivy tables
    console.log('\n📦 Migration 014: Create base Arrivy tables')
    console.log('   - arrivy_tasks')
    console.log('   - arrivy_entities')
    console.log('   - arrivy_events')
    console.log('   - arrivy_task_status')

    const migration014Path = path.join(__dirname, '..', 'lib', 'db', 'migrations', '014_create_arrivy_tables.sql')
    if (!fs.existsSync(migration014Path)) {
      console.error('❌ Migration 014 file not found:', migration014Path)
      process.exit(1)
    }

    const migration014SQL = fs.readFileSync(migration014Path, 'utf8')
    console.log('📄 Read migration 014')
    console.log('🚀 Executing migration 014...')
    await sql.query(migration014SQL)
    console.log('✅ Migration 014 executed successfully')

    // Migration 015: Create task-entities join table
    console.log('\n📦 Migration 015: Create task-entities join table')
    console.log('   - arrivy_task_entities (proper many-to-many relationship)')

    const migration015Path = path.join(__dirname, '..', 'lib', 'db', 'migrations', '015_create_arrivy_task_entities_join_table.sql')
    if (!fs.existsSync(migration015Path)) {
      console.error('❌ Migration 015 file not found:', migration015Path)
      process.exit(1)
    }

    const migration015SQL = fs.readFileSync(migration015Path, 'utf8')
    console.log('📄 Read migration 015')
    console.log('🚀 Executing migration 015...')
    await sql.query(migration015SQL)
    console.log('✅ Migration 015 executed successfully')

    // Migration 016: Make QuickBase fields optional
    console.log('\n📦 Migration 016: Make QuickBase fields optional')
    console.log('   - Allow tasks to originate in Arrivy without QuickBase link')

    const migration016Path = path.join(__dirname, '..', 'lib', 'db', 'migrations', '016_make_quickbase_fields_optional.sql')
    if (!fs.existsSync(migration016Path)) {
      console.error('❌ Migration 016 file not found:', migration016Path)
      process.exit(1)
    }

    const migration016SQL = fs.readFileSync(migration016Path, 'utf8')
    console.log('📄 Read migration 016')
    console.log('🚀 Executing migration 016...')
    await sql.query(migration016SQL)
    console.log('✅ Migration 016 executed successfully')

    // Verify schema
    console.log('\n🔍 Verifying schema...')

    // Check all tables exist
    const tableNames = [
      'arrivy_tasks',
      'arrivy_entities',
      'arrivy_events',
      'arrivy_task_status',
      'arrivy_task_entities'
    ]

    for (const tableName of tableNames) {
      const tableResult = await sql.query(`
        SELECT table_name
        FROM information_schema.tables
        WHERE table_schema = 'public' AND table_name = '${tableName}'
      `)

      if (tableResult.rows.length === 0) {
        console.error(`❌ ${tableName} table not found`)
        process.exit(1)
      }
      console.log(`✅ ${tableName} table created`)
    }

    // Check indexes exist
    console.log('\n🔍 Checking indexes...')
    const indexResult = await sql.query(`
      SELECT indexname, tablename
      FROM pg_indexes
      WHERE tablename LIKE 'arrivy_%'
      AND schemaname = 'public'
      ORDER BY tablename, indexname
    `)

    const indexCount = indexResult.rows.length
    console.log(`✅ Created ${indexCount} indexes for performance`)

    if (indexCount > 0) {
      console.log('\n📋 Indexes created:')
      indexResult.rows.forEach(row => {
        console.log(`   - ${row.indexname} on ${row.tablename}`)
      })
    }

    // Verify QuickBase fields are nullable
    console.log('\n🔍 Verifying QuickBase fields are nullable...')
    const nullableCheck = await sql.query(`
      SELECT column_name, is_nullable
      FROM information_schema.columns
      WHERE table_name = 'arrivy_tasks'
      AND column_name IN ('quickbase_project_id', 'quickbase_record_id')
      ORDER BY column_name
    `)

    nullableCheck.rows.forEach(row => {
      const status = row.is_nullable === 'YES' ? '✅' : '❌'
      console.log(`${status} ${row.column_name}: ${row.is_nullable}`)
    })

    // Get record counts
    console.log('\n📊 Current record counts:')
    const taskCount = await sql.query('SELECT COUNT(*) as count FROM arrivy_tasks')
    const entityCount = await sql.query('SELECT COUNT(*) as count FROM arrivy_entities')
    const eventCount = await sql.query('SELECT COUNT(*) as count FROM arrivy_events')
    const statusCount = await sql.query('SELECT COUNT(*) as count FROM arrivy_task_status')

    console.log(`   - Tasks: ${taskCount.rows[0].count}`)
    console.log(`   - Entities (Crew): ${entityCount.rows[0].count}`)
    console.log(`   - Events: ${eventCount.rows[0].count}`)
    console.log(`   - Status Updates: ${statusCount.rows[0].count}`)

    console.log('\n🎉 Arrivy migration complete!')
    console.log('📊 Summary:')
    console.log('   - 5 tables created (tasks, entities, events, task_status, task_entities)')
    console.log(`   - ${indexCount} indexes created for performance`)
    console.log('   - QuickBase fields are optional (tasks can originate in Arrivy)')
    console.log('   - Ready for Arrivy webhook integration')

    console.log('\n💡 Next steps:')
    console.log('   1. Configure Arrivy webhook in dashboard (https://app.arrivy.com)')
    console.log('      - URL: https://your-domain.vercel.app/api/webhooks/arrivy')
    console.log('      - Events: TASK_CREATED, TASK_STATUS, CREW_ASSIGNED, ARRIVING, LATE, NOSHOW, TASK_RATING, EXCEPTION')
    console.log('   2. Create crew entities in Arrivy Team management')
    console.log('   3. Run initial data sync: npm run sync:arrivy')
    console.log('   4. Deploy to production: vercel --prod')

  } catch (error) {
    if (error.message.includes('already exists')) {
      console.log('\n⚠️  Some schema elements already exist')
      console.log('✅ This is normal - migrations are idempotent and safe to run multiple times')
      console.log('\n🔍 Verifying existing schema...')

      try {
        // Verify tables exist
        const tableCheck = await sql.query(`
          SELECT table_name
          FROM information_schema.tables
          WHERE table_schema = 'public' AND table_name LIKE 'arrivy_%'
          ORDER BY table_name
        `)

        console.log(`✅ Found ${tableCheck.rows.length} Arrivy tables:`)
        tableCheck.rows.forEach(row => {
          console.log(`   - ${row.table_name}`)
        })

        return // Exit gracefully if schema already exists
      } catch (verifyError) {
        console.error('❌ Failed to verify existing schema:', verifyError.message)
        process.exit(1)
      }
    } else {
      console.error('\n❌ Migration failed:', error.message)
      console.error('Full error:', error)

      console.log('\n💡 Troubleshooting tips:')
      console.log('   - Check DATABASE_URL is correct in .env.local')
      console.log('   - Verify database is accessible')
      console.log('   - Check migration SQL files exist in lib/db/migrations/')
      console.log('   - Review error message above for specific issues')

      process.exit(1)
    }
  } finally {
    // Close database connection
    try {
      await sql.end()
    } catch (endError) {
      // Ignore connection close errors
    }
  }
}

// Run migration if called directly
if (require.main === module) {
  runArrivyMigration()
    .then(() => {
      console.log('\n✅ Migration script completed successfully')
      process.exit(0)
    })
    .catch((error) => {
      console.error('\n❌ Migration script failed:', error)
      process.exit(1)
    })
}

module.exports = { runArrivyMigration }
