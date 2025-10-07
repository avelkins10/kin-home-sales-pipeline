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

    const migrationPath = path.resolve(__dirname, '..', 'lib', 'db', 'migrations', '004_system_settings_schema.sql')
    const sqlContent = fs.readFileSync(migrationPath, 'utf8')

    console.log('Running system settings migration...')
    await sql.query(sqlContent)
    console.log('✅ Executed migration SQL')

    const tableCheck = await sql.query(`SELECT table_name FROM information_schema.tables WHERE table_name = 'system_settings'`)
    if (tableCheck.rows.length > 0) {
      console.log('✅ Created system_settings table')
    } else {
      console.warn('⚠️ system_settings table not found after migration')
    }

    const defaultRow = await sql.query(`SELECT COUNT(*)::int AS count FROM system_settings WHERE id = 1`)
    if (defaultRow.rows[0]?.count > 0) {
      console.log('✅ Inserted default system settings')
    } else {
      console.warn('⚠️ Default system settings row missing')
    }

    const indexCheck = await sql.query(`SELECT indexname FROM pg_indexes WHERE tablename = 'system_settings'`)
    const hasGin = indexCheck.rows.some(r => r.indexname === 'idx_system_settings_jsonb')
    if (hasGin) {
      console.log('✅ Created JSONB index')
    } else {
      console.warn('⚠️ JSONB index not found')
    }

    console.log('✅ System settings migration complete')
    process.exit(0)
  } catch (err) {
    console.error('Migration failed:', err)
    process.exit(1)
  }
}

main()


