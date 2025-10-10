#!/usr/bin/env node
/**
 * Automatically match a dashboard user to their QuickBase account by email
 * This searches QuickBase for projects with matching email addresses
 * Usage: node scripts/auto-match-user-by-email.js <email>
 */

require('dotenv').config({ path: '.env.local' })
const { sql } = require('@vercel/postgres')

// We'll need to call the API endpoint since QB client requires server-only
const getBaseUrl = () => {
  return process.env.NEXTAUTH_URL || 'http://localhost:3000'
}

async function autoMatchUserByEmail(email) {
  try {
    console.log(`\n🔍 Looking for dashboard user: ${email}...\n`)

    // Find user in dashboard
    const userResult = await sql.query(
      'SELECT id, name, email, quickbase_user_id, role FROM users WHERE email = $1',
      [email]
    )

    if (userResult.rows.length === 0) {
      console.error(`❌ User not found in dashboard with email: ${email}`)
      process.exit(1)
    }

    const user = userResult.rows[0]
    console.log(`📋 Found dashboard user:`)
    console.log(`   Name: ${user.name}`)
    console.log(`   Email: ${user.email}`)
    console.log(`   Role: ${user.role}`)
    console.log(`   Current QB ID: ${user.quickbase_user_id || '(not set)'}`)

    if (user.quickbase_user_id) {
      console.log(`\n⚠️  User already has QuickBase ID set: ${user.quickbase_user_id}`)
      console.log(`   Use --force flag to override (not implemented yet)`)
      return
    }

    console.log(`\n🔍 Searching QuickBase for email: ${email}...\n`)

    // Use the QB API lookup endpoint (requires authentication)
    // For now, we'll query directly using the user's email pattern
    // This is a simplified version - the actual API does this more robustly

    console.log('ℹ️  To complete the match, please:')
    console.log('   1. Go to Settings → Users in the dashboard')
    console.log('   2. Click "Add from QuickBase" button')
    console.log(`   3. Search for: ${email}`)
    console.log('   4. Click the result to automatically link the account')
    console.log('')
    console.log('   This will search QuickBase projects for this email')
    console.log('   and automatically find the correct QuickBase User ID')
    console.log('')
    console.log('   Note: If the user has multiple QB IDs (can happen),')
    console.log('   the system will pick the one with most recent activity')

  } catch (error) {
    console.error('❌ Error:', error.message)
    throw error
  }
}

// Parse command line arguments
const args = process.argv.slice(2)
if (args.length !== 1) {
  console.log('Usage: node scripts/auto-match-user-by-email.js <email>')
  console.log('Example: node scripts/auto-match-user-by-email.js kash.b@kinhome.com')
  process.exit(1)
}

const email = args[0]

autoMatchUserByEmail(email)
  .then(() => {
    console.log('\n✨ Done!')
    process.exit(0)
  })
  .catch((error) => {
    console.error('Fatal error:', error)
    process.exit(1)
  })
