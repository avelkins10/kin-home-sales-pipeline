#!/usr/bin/env node

/**
 * Seed active sales reps (closers + setters) with projects since May 1, 2025
 *
 * Strategy:
 * 1. Query Projects table for all projects >= May 1, 2025
 * 2. Extract unique closer + setter emails
 * 3. Look up each in Contacts table for external IDs
 * 4. Insert/update in Neon users table with all available IDs
 * 5. Log to user_sync_log
 *
 * Uses COALESCE strategy: Only fills missing fields, never overwrites existing data
 */

const path = require('path');
require('dotenv').config({ path: path.join(__dirname, '../.env.local') });

const { sql } = require('@vercel/postgres');

const QB_REALM = process.env.QUICKBASE_REALM;
const QB_TOKEN = process.env.QUICKBASE_TOKEN;
const QB_TABLE_PROJECTS = (process.env.QUICKBASE_TABLE_PROJECTS || 'br9kwm8na').trim();
const QB_TABLE_CONTACTS = 'br9kwm8td';

// Project field IDs
const PROJECT_FIELDS = {
  RECORD_ID: 3,
  SALES_DATE: 522,
  CLOSER_ID: 516,       // RepCard ID from Projects
  CLOSER_NAME: 517,
  CLOSER_EMAIL: 356,
  SETTER_ID: 329,       // RepCard ID from Projects
  SETTER_NAME: 330,
  SETTER_EMAIL: 334,
};

// Contact field IDs
const CONTACT_FIELDS = {
  RECORD_ID: 3,
  FULL_NAME: 6,
  EMAIL: 17,
  PHONE: 18,
  REPCARD_ID: 235,
  ENERFLO_USER_ID: 168,
  SEQUIFI_USER_ID: 243,
  REPCARD_OFFICE: 253,
  REPCARD_TEAM: 255,
  REPCARD_IS_ROOKIE: 256,
  REPCARD_PROFILE_IMAGE: 261,
  NUM_CLOSER_PROJECTS: 114,
  NUM_SETTER_PROJECTS: 278,
};

async function qbQuery(tableId, select, where, options = {}) {
  const response = await fetch('https://api.quickbase.com/v1/records/query', {
    method: 'POST',
    headers: {
      'QB-Realm-Hostname': QB_REALM,
      'Authorization': `QB-USER-TOKEN ${QB_TOKEN}`,
      'Content-Type': 'application/json',
    },
    body: JSON.stringify({
      from: tableId,
      select,
      where,
      ...options,
    }),
  });

  if (!response.ok) {
    const errorText = await response.text();
    throw new Error(`QuickBase API error: ${response.status} - ${errorText}`);
  }

  return response.json();
}

async function seedActiveReps() {
  console.log('\n' + '='.repeat(80));
  console.log('SEED ACTIVE SALES REPS - Projects Since May 1, 2025');
  console.log('='.repeat(80));
  console.log('');

  try {
    // Step 1: Get all projects since May 1, 2025
    console.log('📡 Fetching projects since May 1, 2025 from QuickBase...');
    const projectsResponse = await qbQuery(
      QB_TABLE_PROJECTS,
      [
        PROJECT_FIELDS.RECORD_ID,
        PROJECT_FIELDS.SALES_DATE,
        PROJECT_FIELDS.CLOSER_ID,
        PROJECT_FIELDS.CLOSER_NAME,
        PROJECT_FIELDS.CLOSER_EMAIL,
        PROJECT_FIELDS.SETTER_ID,
        PROJECT_FIELDS.SETTER_NAME,
        PROJECT_FIELDS.SETTER_EMAIL,
      ],
      "{522.OAF.'2025-05-01'}", // SALES_DATE >= May 1, 2025
      { options: { top: 5000 } }
    );

    const projects = projectsResponse.data || [];
    console.log(`✅ Found ${projects.length} projects\n`);

    // Step 2: Extract unique reps (closers + setters)
    console.log('🔍 Extracting unique reps (closers + setters)...');
    const repMap = new Map();

    projects.forEach(project => {
      // Process closer
      const closerEmail = project[PROJECT_FIELDS.CLOSER_EMAIL]?.value;
      const closerName = project[PROJECT_FIELDS.CLOSER_NAME]?.value;
      const closerId = project[PROJECT_FIELDS.CLOSER_ID]?.value;

      if (closerEmail) {
        const key = closerEmail.toLowerCase().trim();
        if (!repMap.has(key)) {
          // Extract RepCard ID safely
          let repcardId = null;
          if (closerId) {
            repcardId = typeof closerId === 'object' && closerId.id ? closerId.id : closerId;
          }

          repMap.set(key, {
            email: closerEmail,
            name: closerName || 'Unknown',
            repcardIdFromProjects: repcardId,
            role: 'closer',
            projectCount: 0,
            isSetter: false,
          });
        }
        repMap.get(key).projectCount++;
      }

      // Process setter
      const setterEmail = project[PROJECT_FIELDS.SETTER_EMAIL]?.value;
      const setterName = project[PROJECT_FIELDS.SETTER_NAME]?.value;
      const setterId = project[PROJECT_FIELDS.SETTER_ID]?.value;

      if (setterEmail) {
        const key = setterEmail.toLowerCase().trim();
        if (!repMap.has(key)) {
          // Extract RepCard ID safely
          let repcardId = null;
          if (setterId) {
            repcardId = typeof setterId === 'object' && setterId.id ? setterId.id : setterId;
          }

          repMap.set(key, {
            email: setterEmail,
            name: setterName || 'Unknown',
            repcardIdFromProjects: repcardId,
            role: 'setter',
            projectCount: 0,
            isSetter: true,
          });
        } else {
          // Update existing entry
          repMap.get(key).isSetter = true;
        }
        repMap.get(key).projectCount++;
      }
    });

    const uniqueReps = Array.from(repMap.values());
    console.log(`✅ Found ${uniqueReps.length} unique reps\n`);

    // Step 3: Look up each rep in Contacts table
    console.log('🔍 Looking up reps in Contacts table...');
    let enriched = 0;
    let notFound = 0;
    let created = 0;
    let updated = 0;
    let errors = 0;

    for (const rep of uniqueReps) {
      try {
        // Look up in Contacts by email
        const contactResult = await qbQuery(
          QB_TABLE_CONTACTS,
          [
            CONTACT_FIELDS.RECORD_ID,
            CONTACT_FIELDS.FULL_NAME,
            CONTACT_FIELDS.EMAIL,
            CONTACT_FIELDS.PHONE,
            CONTACT_FIELDS.REPCARD_ID,
            CONTACT_FIELDS.ENERFLO_USER_ID,
            CONTACT_FIELDS.SEQUIFI_USER_ID,
            CONTACT_FIELDS.REPCARD_OFFICE,
            CONTACT_FIELDS.REPCARD_TEAM,
            CONTACT_FIELDS.REPCARD_IS_ROOKIE,
            CONTACT_FIELDS.REPCARD_PROFILE_IMAGE,
            CONTACT_FIELDS.NUM_CLOSER_PROJECTS,
            CONTACT_FIELDS.NUM_SETTER_PROJECTS,
          ],
          `{17.EX.'${rep.email.replace(/'/g, "\\'")}'}`,
          { options: { top: 1 } }
        );

        let contactData = {
          quickbase_contact_id: null,
          full_name: null,
          phone: null,
          repcard_user_id: rep.repcardIdFromProjects?.toString() || null,
          enerflo_user_id: null,
          sequifi_user_id: null,
          office: null,
          team: null,
          is_rookie: false,
          profile_image_url: null,
          num_closer_projects: 0,
          num_setter_projects: 0,
        };

        if (contactResult.data && contactResult.data.length > 0) {
          const contact = contactResult.data[0];
          contactData = {
            quickbase_contact_id: contact[CONTACT_FIELDS.RECORD_ID]?.value?.toString() || null,
            full_name: contact[CONTACT_FIELDS.FULL_NAME]?.value || rep.name,
            phone: contact[CONTACT_FIELDS.PHONE]?.value || null,
            repcard_user_id: contact[CONTACT_FIELDS.REPCARD_ID]?.value?.toString() || rep.repcardIdFromProjects?.toString() || null,
            enerflo_user_id: contact[CONTACT_FIELDS.ENERFLO_USER_ID]?.value || null,
            sequifi_user_id: contact[CONTACT_FIELDS.SEQUIFI_USER_ID]?.value || null,
            office: contact[CONTACT_FIELDS.REPCARD_OFFICE]?.value || null,
            team: contact[CONTACT_FIELDS.REPCARD_TEAM]?.value || null,
            is_rookie: contact[CONTACT_FIELDS.REPCARD_IS_ROOKIE]?.value || false,
            profile_image_url: contact[CONTACT_FIELDS.REPCARD_PROFILE_IMAGE]?.value || null,
            num_closer_projects: contact[CONTACT_FIELDS.NUM_CLOSER_PROJECTS]?.value || 0,
            num_setter_projects: contact[CONTACT_FIELDS.NUM_SETTER_PROJECTS]?.value || 0,
          };
          enriched++;
        } else {
          notFound++;
        }

        // Step 4: Upsert into Neon (COALESCE strategy - only fill missing fields)
        const existingUser = await sql`
          SELECT id FROM users WHERE LOWER(email) = ${rep.email.toLowerCase()}
        `;

        if (existingUser.rows.length > 0) {
          // Update existing user
          await sql`
            UPDATE users
            SET
              name = COALESCE(name, ${contactData.full_name || rep.name}),
              phone = COALESCE(phone, ${contactData.phone}),
              quickbase_contact_id = COALESCE(quickbase_contact_id, ${contactData.quickbase_contact_id}),
              repcard_user_id = COALESCE(repcard_user_id, ${contactData.repcard_user_id}),
              enerflo_user_id = COALESCE(enerflo_user_id, ${contactData.enerflo_user_id}),
              sequifi_user_id = COALESCE(sequifi_user_id, ${contactData.sequifi_user_id}),
              office = COALESCE(office, ${contactData.office}),
              team = COALESCE(team, ${contactData.team}),
              is_rookie = COALESCE(is_rookie, ${contactData.is_rookie}),
              is_setter = ${rep.isSetter},
              profile_image_url = COALESCE(profile_image_url, ${contactData.profile_image_url}),
              num_closer_projects = ${contactData.num_closer_projects},
              num_setter_projects = ${contactData.num_setter_projects},
              last_synced_from_contacts_at = NOW()
            WHERE id = ${existingUser.rows[0].id}
          `;
          updated++;
          console.log(`  ✓ Updated ${rep.email} (${rep.projectCount} projects)`);
        } else {
          // Create new user
          await sql`
            INSERT INTO users (
              email, name, phone, role,
              quickbase_contact_id,
              repcard_user_id, enerflo_user_id, sequifi_user_id,
              office, team,
              is_rookie, is_setter,
              profile_image_url,
              num_closer_projects, num_setter_projects,
              last_synced_from_contacts_at
            ) VALUES (
              ${rep.email}, ${contactData.full_name || rep.name}, ${contactData.phone}, ${rep.role},
              ${contactData.quickbase_contact_id},
              ${contactData.repcard_user_id}, ${contactData.enerflo_user_id}, ${contactData.sequifi_user_id},
              ${contactData.office}, ${contactData.team},
              ${contactData.is_rookie}, ${rep.isSetter},
              ${contactData.profile_image_url},
              ${contactData.num_closer_projects}, ${contactData.num_setter_projects},
              NOW()
            )
          `;
          created++;
          console.log(`  + Created ${rep.email} (${rep.projectCount} projects)`);
        }

        // Step 5: Log to user_sync_log
        await sql`
          INSERT INTO user_sync_log (
            user_id,
            source_system,
            external_id,
            match_method,
            confidence,
            synced_at,
            notes
          )
          SELECT
            id,
            'contacts',
            ${contactData.quickbase_contact_id || 'projects_only'},
            'email',
            1.0,
            NOW(),
            ${`Seeded from projects >= May 1, 2025. ${rep.projectCount} projects as ${rep.role}.`}
          FROM users
          WHERE LOWER(email) = ${rep.email.toLowerCase()}
        `;

      } catch (error) {
        errors++;
        console.error(`  ❌ Error processing ${rep.email}:`, error.message);
      }
    }

    // Summary
    console.log('\n' + '='.repeat(80));
    console.log('🎉 SEED COMPLETE!');
    console.log('='.repeat(80));
    console.log('');
    console.log('📊 Summary:');
    console.log(`   - Total unique reps: ${uniqueReps.length}`);
    console.log(`   - Enriched from Contacts: ${enriched}`);
    console.log(`   - Not in Contacts (projects only): ${notFound}`);
    console.log(`   - Created new users: ${created}`);
    console.log(`   - Updated existing users: ${updated}`);
    console.log(`   - Errors: ${errors}`);
    console.log('');
    console.log('💡 Next steps:');
    console.log('   1. Verify users table populated: SELECT COUNT(*) FROM users;');
    console.log('   2. Check RepCard IDs: SELECT email, repcard_user_id FROM users WHERE repcard_user_id IS NOT NULL;');
    console.log('   3. Review sync log: SELECT * FROM user_sync_log ORDER BY synced_at DESC LIMIT 10;');
    console.log('');

  } catch (error) {
    console.error('\n❌ Seed failed:', error);
    throw error;
  } finally {
    // Close database connection
    await sql.end();
  }
}

// Run seed
if (require.main === module) {
  seedActiveReps()
    .then(() => {
      console.log('✅ Seed script completed successfully\n');
      process.exit(0);
    })
    .catch(error => {
      console.error('❌ Seed script failed:', error);
      process.exit(1);
    });
}

module.exports = { seedActiveReps };
