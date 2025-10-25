#!/usr/bin/env node

/**
 * Bootstrap RepCard user IDs from QuickBase CLOSER_ID field
 *
 * This script:
 * 1. Fetches all projects from QuickBase with CLOSER_ID populated
 * 2. Extracts unique closer email + RepCard ID pairs
 * 3. Matches each closer email to users in our database
 * 4. Updates users.repcard_user_id with the RepCard ID from QuickBase
 * 5. Logs all sync operations to user_sync_log table
 *
 * This provides initial RepCard ID mapping before calling RepCard API
 */

const path = require('path')
require('dotenv').config({ path: path.join(__dirname, '../.env.local') })

const { sql } = require('@vercel/postgres')

// QuickBase API client setup
const QB_REALM = process.env.QUICKBASE_REALM
const QB_TOKEN = process.env.QUICKBASE_TOKEN
const QB_TABLE_PROJECTS = (process.env.QUICKBASE_TABLE_PROJECTS || 'br9kwm8na').trim()

// Field IDs from QuickBase
const PROJECT_FIELDS = {
  RECORD_ID: 3,
  CLOSER_ID: 516,
  CLOSER_NAME: 517,
  CLOSER_EMAIL: 356,
  SALES_OFFICE: 339,
}

async function fetchQuickbaseProjects() {
  console.log('📡 Fetching projects from QuickBase...')

  const response = await fetch(`https://api.quickbase.com/v1/records/query`, {
    method: 'POST',
    headers: {
      'QB-Realm-Hostname': QB_REALM,
      'Authorization': `QB-USER-TOKEN ${QB_TOKEN}`,
      'Content-Type': 'application/json',
    },
    body: JSON.stringify({
      from: QB_TABLE_PROJECTS,
      select: [
        PROJECT_FIELDS.RECORD_ID,
        PROJECT_FIELDS.CLOSER_ID,
        PROJECT_FIELDS.CLOSER_NAME,
        PROJECT_FIELDS.CLOSER_EMAIL,
        PROJECT_FIELDS.SALES_OFFICE,
      ],
      where: `{${PROJECT_FIELDS.CLOSER_ID}.XEX.''}`, // CLOSER_ID is not empty
      options: {
        top: 5000,
      },
    }),
  })

  if (!response.ok) {
    throw new Error(`QuickBase API error: ${response.status} ${response.statusText}`)
  }

  const data = await response.json()
  return data.data || []
}

async function bootstrapRepcardIds() {
  try {
    console.log('🚀 Starting RepCard ID bootstrap from QuickBase...')

    // Verify environment variables
    if (!process.env.DATABASE_URL) {
      console.error('❌ DATABASE_URL environment variable is not set')
      process.exit(1)
    }

    if (!QB_REALM || !QB_TOKEN) {
      console.error('❌ QuickBase credentials not configured')
      process.exit(1)
    }

    // Fetch projects from QuickBase
    const projects = await fetchQuickbaseProjects()
    console.log(`✅ Fetched ${projects.length} projects with CLOSER_ID`)

    // Group by closer email to get unique closers
    const closerMap = new Map()

    for (const project of projects) {
      const closerId = project[PROJECT_FIELDS.CLOSER_ID]?.value
      const closerEmail = project[PROJECT_FIELDS.CLOSER_EMAIL]?.value
      const closerName = project[PROJECT_FIELDS.CLOSER_NAME]?.value
      const office = project[PROJECT_FIELDS.SALES_OFFICE]?.value

      if (!closerId || !closerEmail) continue

      // Extract numeric ID from user object if needed
      // QuickBase user fields return objects like { id: 123, name: "John Doe" }
      let repcardId = closerId
      if (typeof closerId === 'object' && closerId.id) {
        repcardId = closerId.id.toString()
      } else if (typeof closerId === 'number') {
        repcardId = closerId.toString()
      }

      if (!closerMap.has(closerEmail)) {
        closerMap.set(closerEmail, {
          repcardId,
          name: closerName || 'Unknown',
          email: closerEmail.toLowerCase(),
          office: office || 'Unknown',
          projectCount: 0,
        })
      }

      closerMap.get(closerEmail).projectCount++
    }

    console.log(`📊 Found ${closerMap.size} unique closers with RepCard IDs`)

    // Match and update users in database
    let matched = 0
    let updated = 0
    let alreadySet = 0
    let notFound = 0

    for (const [email, closer] of closerMap) {
      try {
        // Find user by email
        const userResult = await sql`
          SELECT id, email, name, repcard_user_id
          FROM users
          WHERE LOWER(email) = ${email.toLowerCase()}
        `

        if (userResult.rows.length === 0) {
          console.warn(`⚠️  No user found for email: ${email}`)
          notFound++
          continue
        }

        const user = userResult.rows[0]
        matched++

        // Check if RepCard ID already set
        if (user.repcard_user_id) {
          if (user.repcard_user_id === closer.repcardId) {
            console.log(`✓ User ${email} already has correct RepCard ID: ${closer.repcardId}`)
            alreadySet++
          } else {
            console.warn(`⚠️  User ${email} has different RepCard ID: ${user.repcard_user_id} vs ${closer.repcardId}`)
            // Don't overwrite - manual review needed
          }
          continue
        }

        // Update user with RepCard ID
        await sql`
          UPDATE users
          SET repcard_user_id = ${closer.repcardId},
              last_synced_at = NOW()
          WHERE id = ${user.id}
        `

        // Log the sync operation
        await sql`
          INSERT INTO user_sync_log (
            user_id,
            source_system,
            external_id,
            match_method,
            confidence,
            synced_by,
            synced_at,
            notes
          ) VALUES (
            ${user.id},
            'repcard',
            ${closer.repcardId},
            'quickbase_closer_id',
            1.0,
            NULL,
            NOW(),
            ${`Bootstrapped from QuickBase CLOSER_ID. ${closer.projectCount} projects. Office: ${closer.office}`}
          )
        `

        console.log(`✅ Updated ${email} with RepCard ID: ${closer.repcardId} (${closer.projectCount} projects)`)
        updated++

      } catch (error) {
        console.error(`❌ Error processing ${email}:`, error.message)
      }
    }

    // Summary
    console.log('\n🎉 Bootstrap complete!')
    console.log('📊 Summary:')
    console.log(`   - Total closers in QuickBase: ${closerMap.size}`)
    console.log(`   - Matched to database users: ${matched}`)
    console.log(`   - Successfully updated: ${updated}`)
    console.log(`   - Already had correct ID: ${alreadySet}`)
    console.log(`   - Not found in database: ${notFound}`)
    console.log('\n💡 Next steps:')
    console.log('   1. Review any unmatched emails')
    console.log('   2. Run RepCard API sync to get full user profiles')
    console.log('   3. Validate office assignments across systems')

  } catch (error) {
    console.error('❌ Bootstrap failed:', error.message)
    console.error('Full error:', error)
    process.exit(1)
  } finally {
    // Close database connection
    await sql.end()
  }
}

// Run bootstrap if called directly
if (require.main === module) {
  bootstrapRepcardIds()
    .then(() => {
      console.log('✅ Bootstrap script completed successfully')
      process.exit(0)
    })
    .catch((error) => {
      console.error('❌ Bootstrap script failed:', error)
      process.exit(1)
    })
}

module.exports = { bootstrapRepcardIds }
