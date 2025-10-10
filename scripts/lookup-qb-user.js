#!/usr/bin/env node
/**
 * Lookup QuickBase user by email or name
 * Usage: node scripts/lookup-qb-user.js <search_term>
 */

require('dotenv').config({ path: '.env.local' })
const { Pool } = require('pg')

const pool = new Pool({
  connectionString: process.env.DATABASE_URL,
})

async function lookupQBUser(searchTerm) {
  try {
    console.log(`\n🔍 Searching QuickBase for: "${searchTerm}"...\n`)

    // Search in quickbase_users table (if it exists)
    try {
      const qbUsersResult = await pool.query(`
        SELECT user_id, name, email, phone, office
        FROM quickbase_users
        WHERE name ILIKE $1 OR email ILIKE $1
        LIMIT 10
      `, [`%${searchTerm}%`])

      if (qbUsersResult.rows.length > 0) {
        console.log(`📋 Found ${qbUsersResult.rows.length} QuickBase users:\n`)
        qbUsersResult.rows.forEach((user, idx) => {
          console.log(`${idx + 1}. ${user.name}`)
          console.log(`   QB User ID: ${user.user_id}`)
          console.log(`   Email: ${user.email || '(none)'}`)
          console.log(`   Phone: ${user.phone || '(none)'}`)
          console.log(`   Office: ${user.office || '(none)'}`)
          console.log('')
        })
      } else {
        console.log('No users found in quickbase_users table.')
      }
    } catch (err) {
      console.log('Note: quickbase_users table not found, searching projects...')
    }

    // Also search in projects as closer/setter
    const projectsResult = await pool.query(`
      SELECT
        COALESCE(closer_id, setter_id) as user_id,
        COALESCE(closer_name, setter_name) as name,
        COUNT(*) as project_count
      FROM quickbase_projects
      WHERE closer_name ILIKE $1 OR setter_name ILIKE $1
      GROUP BY COALESCE(closer_id, setter_id), COALESCE(closer_name, setter_name)
      LIMIT 10
    `, [`%${searchTerm}%`])

    if (projectsResult.rows.length > 0) {
      console.log(`\n📊 Found users in projects:\n`)
      projectsResult.rows.forEach((user, idx) => {
        console.log(`${idx + 1}. ${user.name}`)
        console.log(`   QB User ID: ${user.user_id}`)
        console.log(`   Project Count: ${user.project_count}`)
        console.log('')
      })
    }

    // Check if user already exists in dashboard
    const dashboardUserResult = await pool.query(`
      SELECT id, name, email, quickbase_user_id
      FROM users
      WHERE email ILIKE $1 OR name ILIKE $1
    `, [`%${searchTerm}%`])

    if (dashboardUserResult.rows.length > 0) {
      console.log(`\n👤 Existing dashboard users:\n`)
      dashboardUserResult.rows.forEach((user, idx) => {
        console.log(`${idx + 1}. ${user.name}`)
        console.log(`   Email: ${user.email}`)
        console.log(`   Current QB ID: ${user.quickbase_user_id || '(not set)'}`)
        console.log('')
      })
    }

  } catch (error) {
    console.error('❌ Error looking up user:', error.message)
    throw error
  } finally {
    await pool.end()
  }
}

// Parse command line arguments
const args = process.argv.slice(2)
if (args.length !== 1) {
  console.log('Usage: node scripts/lookup-qb-user.js <search_term>')
  console.log('Example: node scripts/lookup-qb-user.js "John Smith"')
  console.log('Example: node scripts/lookup-qb-user.js john@example.com')
  process.exit(1)
}

const searchTerm = args[0]

lookupQBUser(searchTerm)
  .then(() => {
    console.log('\n✨ Done!')
    process.exit(0)
  })
  .catch((error) => {
    console.error('Fatal error:', error)
    process.exit(1)
  })
