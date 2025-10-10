#!/usr/bin/env node
/**
 * Update a user's QuickBase User ID
 * Usage: node scripts/update-user-qb-id.js <email> <qb_user_id>
 */

require('dotenv').config({ path: '.env.local' })
const { Pool } = require('pg')

const pool = new Pool({
  connectionString: process.env.DATABASE_URL,
})

async function updateUserQBId(email, qbUserId) {
  try {
    // Find user by email
    const userResult = await pool.query(
      'SELECT id, name, email, quickbase_user_id FROM users WHERE email = $1',
      [email]
    )

    if (userResult.rows.length === 0) {
      console.error(`❌ User not found with email: ${email}`)
      process.exit(1)
    }

    const user = userResult.rows[0]
    console.log(`\n📋 Found user:`)
    console.log(`   Name: ${user.name}`)
    console.log(`   Email: ${user.email}`)
    console.log(`   Current QB ID: ${user.quickbase_user_id || '(none)'}`)
    console.log(`   New QB ID: ${qbUserId}`)

    // Update the QuickBase User ID
    const updateResult = await pool.query(
      'UPDATE users SET quickbase_user_id = $1, updated_at = NOW() WHERE id = $2 RETURNING *',
      [qbUserId, user.id]
    )

    if (updateResult.rows.length > 0) {
      console.log(`\n✅ Successfully updated QuickBase ID!`)
      console.log(`   User: ${updateResult.rows[0].name}`)
      console.log(`   QB ID: ${updateResult.rows[0].quickbase_user_id}`)

      // Check projects that will now be visible
      const projectsResult = await pool.query(`
        SELECT COUNT(*) as count
        FROM quickbase_projects
        WHERE closer_id = $1 OR setter_id = $1
      `, [qbUserId])

      console.log(`\n📊 This user has ${projectsResult.rows[0].count} projects in QuickBase`)
    }

  } catch (error) {
    console.error('❌ Error updating user:', error.message)
    throw error
  } finally {
    await pool.end()
  }
}

// Parse command line arguments
const args = process.argv.slice(2)
if (args.length !== 2) {
  console.log('Usage: node scripts/update-user-qb-id.js <email> <qb_user_id>')
  console.log('Example: node scripts/update-user-qb-id.js john@example.com 12345')
  process.exit(1)
}

const [email, qbUserId] = args

updateUserQBId(email, qbUserId)
  .then(() => {
    console.log('\n✨ Done!')
    process.exit(0)
  })
  .catch((error) => {
    console.error('Fatal error:', error)
    process.exit(1)
  })
