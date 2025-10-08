#!/usr/bin/env node
const path = require('path')
require('dotenv').config({ path: path.join(__dirname, '../.env.local') })
const { sql } = require('@vercel/postgres')
const fs = require('fs')

async function main() {
  try {
    if (!process.env.DATABASE_URL) {
      console.error('ERROR: DATABASE_URL is not set')
      process.exit(1)
    }

    const migrationPath = path.resolve(__dirname, 'migrations', '001-create-notifications.sql')
    const sqlContent = fs.readFileSync(migrationPath, 'utf8')

    console.log('Running notifications system migration...')
    await sql.query(sqlContent)
    console.log('✅ Executed migration SQL')

    // Verify table creation
    const tableCheck = await sql.query(`SELECT table_name FROM information_schema.tables WHERE table_name = 'notifications'`)
    if (tableCheck.rows.length > 0) {
      console.log('✅ Created notifications table')
    } else {
      console.warn('⚠️ notifications table not found after migration')
    }

    // Verify indexes
    const indexCheck = await sql.query(`SELECT indexname FROM pg_indexes WHERE tablename = 'notifications'`)
    const expectedIndexes = [
      'idx_notifications_user_unread',
      'idx_notifications_project',
      'idx_notifications_user_project',
      'idx_notifications_type'
    ]

    expectedIndexes.forEach(indexName => {
      const hasIndex = indexCheck.rows.some(r => r.indexname === indexName)
      if (hasIndex) {
        console.log(`✅ Created index: ${indexName}`)
      } else {
        console.warn(`⚠️ Index not found: ${indexName}`)
      }
    })

    // Verify trigger function
    const functionCheck = await sql.query(`
      SELECT proname FROM pg_proc
      WHERE proname = 'update_updated_at_column'
    `)
    if (functionCheck.rows.length > 0) {
      console.log('✅ Created update_updated_at_column function')
    } else {
      console.warn('⚠️ update_updated_at_column function not found')
    }

    // Verify trigger
    const triggerCheck = await sql.query(`
      SELECT tgname FROM pg_trigger
      WHERE tgname = 'update_notifications_updated_at'
    `)
    if (triggerCheck.rows.length > 0) {
      console.log('✅ Created update_notifications_updated_at trigger')
    } else {
      console.warn('⚠️ update_notifications_updated_at trigger not found')
    }

    console.log('✅ Notifications migration complete')
    process.exit(0)
  } catch (err) {
    console.error('Migration failed:', err)
    process.exit(1)
  }
}

main()
