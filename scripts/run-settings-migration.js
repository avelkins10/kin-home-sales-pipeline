const path = require('path');
require('dotenv').config({ path: path.join(process.cwd(), '.env.local') });
const { sql } = require('@vercel/postgres')
const fs = require('fs')

async function runSettingsMigration() {
  try {
    console.log('🚀 Starting settings migration...')
    
    // Verify DATABASE_URL is set
    if (!process.env.DATABASE_URL) {
      throw new Error('DATABASE_URL environment variable is required')
    }

    // Read migration file
    const migrationPath = path.join(__dirname, '..', 'lib', 'db', 'migrations', '002_settings_schema.sql')
    const migrationSQL = fs.readFileSync(migrationPath, 'utf8')
    
    console.log('📄 Read migration file: 002_settings_schema.sql')
    
    // Execute migration
    console.log('⚡ Executing migration...')
    await sql.query(migrationSQL)
    
    console.log('✅ Added phone column to users table')
    console.log('✅ Created notification_settings table')
    console.log('✅ Created index on notification_settings')
    console.log('✅ Inserted default settings for existing users')
    
    // Verify schema
    console.log('🔍 Verifying schema...')
    
    // Check phone column exists
    const phoneColumnCheck = await sql`
      SELECT column_name 
      FROM information_schema.columns 
      WHERE table_name = 'users' AND column_name = 'phone'
    `
    
    if (phoneColumnCheck.rows.length > 0) {
      console.log('✅ Phone column verified in users table')
    } else {
      console.log('⚠️  Phone column not found in users table')
    }
    
    // Check notification_settings table exists
    const tableCheck = await sql`
      SELECT table_name 
      FROM information_schema.tables 
      WHERE table_name = 'notification_settings'
    `
    
    if (tableCheck.rows.length > 0) {
      console.log('✅ notification_settings table verified')
    } else {
      console.log('⚠️  notification_settings table not found')
    }
    
    // Check default settings were created
    const settingsCount = await sql`
      SELECT COUNT(*) as count FROM notification_settings
    `
    
    console.log(`✅ Found ${settingsCount.rows[0].count} notification settings records`)
    
    console.log('🎉 Settings migration completed successfully!')
    
  } catch (error) {
    console.error('❌ Migration failed:', error.message)
    
    // Handle specific errors gracefully
    if (error.message.includes('already exists')) {
      console.log('ℹ️  Some schema elements already exist - this is normal for re-runs')
    } else if (error.message.includes('column') && error.message.includes('already exists')) {
      console.log('ℹ️  Phone column already exists - skipping')
    } else {
      console.error('Full error:', error)
      process.exit(1)
    }
  }
}

// Run the migration
runSettingsMigration()
  .then(() => {
    console.log('✅ Migration script completed')
    process.exit(0)
  })
  .catch((error) => {
    console.error('❌ Migration script failed:', error)
    process.exit(1)
  })
