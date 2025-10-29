/**
 * User Enrichment Helpers
 *
 * Self-enriching functions that automatically fill missing user data from:
 * - QuickBase Contacts table
 * - RepCard API
 * - QuickBase Projects table
 *
 * Uses COALESCE strategy: Only fills missing fields, never overwrites existing data
 */

import 'server-only';
import { sql } from '@/lib/db/client';
import { CONTACT_FIELDS, QB_TABLE_CONTACTS } from '@/lib/constants/contactFieldIds';

const QB_REALM = process.env.QUICKBASE_REALM;
const QB_TOKEN = process.env.QUICKBASE_TOKEN;

/**
 * Look up user in QuickBase Contacts and enrich local record
 * Only fills missing fields using COALESCE - never overwrites existing data
 *
 * @param userId - Local database user ID
 * @param email - User email to lookup in Contacts
 * @returns Result object with status and optional error message
 */
export async function enrichUserFromContacts(
  userId: string,
  email: string
): Promise<{ status: 'updated' | 'not_found' | 'error', error?: string }> {
  try {
    if (!QB_REALM || !QB_TOKEN) {
      console.warn('[enrichUserFromContacts] QuickBase credentials not configured');
      return { status: 'error', error: 'QuickBase credentials not configured' };
    }

    // Query Contacts table by email
    const response = await fetch('https://api.quickbase.com/v1/records/query', {
      method: 'POST',
      headers: {
        'QB-Realm-Hostname': QB_REALM,
        'Authorization': `QB-USER-TOKEN ${QB_TOKEN}`,
        'Content-Type': 'application/json',
      },
      body: JSON.stringify({
        from: QB_TABLE_CONTACTS,
        select: [
          CONTACT_FIELDS.RECORD_ID,
          CONTACT_FIELDS.FULL_NAME,
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
        where: `{${CONTACT_FIELDS.EMAIL}.EX.'${email.replace(/'/g, "\\'")}'}`,
        options: { top: 1 },
      }),
    });

    if (!response.ok) {
      const errorMsg = `QuickBase API error: ${response.status}`;
      console.warn(`[enrichUserFromContacts] ${errorMsg}`);
      return { status: 'error', error: errorMsg };
    }

    const data = await response.json();
    if (!data.data || data.data.length === 0) {
      console.log(`[enrichUserFromContacts] No contact found for ${email}`);
      return { status: 'not_found' };
    }

    const contact = data.data[0];

    // Update user with COALESCE (only fill missing fields)
    // IMPORTANT: Only update external IDs and non-critical metadata
    // Never overwrite name, role, sales_office - the app is the source of truth
    await sql`
      UPDATE users
      SET
        quickbase_contact_id = COALESCE(quickbase_contact_id, ${contact[CONTACT_FIELDS.RECORD_ID]?.value?.toString()}),
        phone = COALESCE(phone, ${contact[CONTACT_FIELDS.PHONE]?.value}),
        repcard_user_id = COALESCE(repcard_user_id, ${contact[CONTACT_FIELDS.REPCARD_ID]?.value?.toString()}),
        enerflo_user_id = COALESCE(enerflo_user_id, ${contact[CONTACT_FIELDS.ENERFLO_USER_ID]?.value}),
        sequifi_user_id = COALESCE(sequifi_user_id, ${contact[CONTACT_FIELDS.SEQUIFI_USER_ID]?.value}),
        office = COALESCE(office, ${contact[CONTACT_FIELDS.REPCARD_OFFICE]?.value}),
        team = COALESCE(team, ${contact[CONTACT_FIELDS.REPCARD_TEAM]?.value}),
        is_rookie = COALESCE(is_rookie, ${contact[CONTACT_FIELDS.REPCARD_IS_ROOKIE]?.value || false}),
        profile_image_url = COALESCE(profile_image_url, ${contact[CONTACT_FIELDS.REPCARD_PROFILE_IMAGE]?.value}),
        num_closer_projects = ${contact[CONTACT_FIELDS.NUM_CLOSER_PROJECTS]?.value || 0},
        num_setter_projects = ${contact[CONTACT_FIELDS.NUM_SETTER_PROJECTS]?.value || 0},
        last_synced_from_contacts_at = NOW()
      WHERE id = ${userId}
    `;

    // Log enrichment
    await sql`
      INSERT INTO user_sync_log (
        user_id,
        source_system,
        external_id,
        match_method,
        confidence,
        synced_at,
        notes
      ) VALUES (
        ${userId},
        'contacts',
        ${contact[CONTACT_FIELDS.RECORD_ID]?.value?.toString()},
        'email',
        1.0,
        NOW(),
        'Auto-enriched from Contacts table'
      )
    `;

    console.log(`[enrichUserFromContacts] ✓ Enriched user ${email} from Contacts`);
    return { status: 'updated' };
  } catch (error) {
    const errorMsg = error instanceof Error ? error.message : String(error);
    console.error('[enrichUserFromContacts] Error:', error);
    return { status: 'error', error: errorMsg };
  }
}

/**
 * Enrich user from RepCard API data
 * Called when fetching RepCard metrics - automatically adds RepCard ID if missing
 *
 * @param repcardUserId - RepCard user ID
 * @param repcardData - Data from RepCard API
 */
export async function enrichUserFromRepCard(
  repcardUserId: number,
  repcardData: {
    email?: string;
    firstName?: string;
    lastName?: string;
    officeName?: string;
    teamName?: string;
    profileImage?: string;
  }
): Promise<void> {
  try {
    const repcardIdStr = repcardUserId.toString();

    // Try to find user by RepCard ID
    let user = await sql`
      SELECT id, email FROM users WHERE repcard_user_id = ${repcardIdStr}
    `;

    if (user.rows.length === 0 && repcardData.email) {
      // Try email match
      user = await sql`
        SELECT id, email FROM users WHERE LOWER(email) = ${repcardData.email.toLowerCase()}
      `;

      if (user.rows.length > 0) {
        // Found by email - add RepCard ID!
        await sql`
          UPDATE users
          SET repcard_user_id = ${repcardIdStr},
              last_synced_from_repcard_at = NOW()
          WHERE id = ${user.rows[0].id}
        `;

        console.log(`[enrichUserFromRepCard] ✓ Added RepCard ID ${repcardUserId} to ${repcardData.email}`);

        // Log enrichment
        await sql`
          INSERT INTO user_sync_log (
            user_id,
            source_system,
            external_id,
            match_method,
            confidence,
            synced_at,
            notes
          ) VALUES (
            ${user.rows[0].id},
            'repcard',
            ${repcardIdStr},
            'email',
            1.0,
            NOW(),
            'Auto-enriched from RepCard API call'
          )
        `;
      } else {
        // User not found - create minimal record
        const newUser = await sql`
          INSERT INTO users (
            email,
            name,
            repcard_user_id,
            office,
            team,
            profile_image_url,
            last_synced_from_repcard_at
          ) VALUES (
            ${repcardData.email},
            ${repcardData.firstName && repcardData.lastName ? `${repcardData.firstName} ${repcardData.lastName}` : null},
            ${repcardIdStr},
            ${repcardData.officeName},
            ${repcardData.teamName},
            ${repcardData.profileImage},
            NOW()
          )
          RETURNING id
        `;

        console.log(`[enrichUserFromRepCard] + Created new user ${repcardData.email} from RepCard API`);

        // Log creation
        await sql`
          INSERT INTO user_sync_log (
            user_id,
            source_system,
            external_id,
            match_method,
            confidence,
            synced_at,
            notes
          ) VALUES (
            ${newUser.rows[0].id},
            'repcard',
            ${repcardIdStr},
            'api_create',
            0.8,
            NOW(),
            'Created from RepCard API (not in Contacts)'
          )
        `;

        return;
      }
    }

    // Update with RepCard data (COALESCE - only fill missing fields)
    if (user.rows.length > 0) {
      await sql`
        UPDATE users
        SET
          office = COALESCE(office, ${repcardData.officeName}),
          team = COALESCE(team, ${repcardData.teamName}),
          profile_image_url = COALESCE(profile_image_url, ${repcardData.profileImage}),
          last_synced_from_repcard_at = NOW()
        WHERE id = ${user.rows[0].id}
      `;
    }
  } catch (error) {
    console.error('[enrichUserFromRepCard] Error:', error);
  }
}

/**
 * Ensure user exists in database
 * Creates user if not found, enriches from Contacts if exists
 *
 * @param email - User email
 * @param name - User name (optional)
 * @returns User record
 */
export async function ensureUserExists(
  email: string,
  name?: string
): Promise<any> {
  try {
    // Check if user exists
    const existing = await sql`
      SELECT * FROM users WHERE LOWER(email) = ${email.toLowerCase()}
    `;

    if (existing.rows.length > 0) {
      const user = existing.rows[0];

      // Enrich if missing external IDs
      if (!user.quickbase_contact_id || !user.repcard_user_id) {
        await enrichUserFromContacts(user.id, email);
      }

      return user;
    }

    // User doesn't exist - try to find in Contacts first
    if (!QB_REALM || !QB_TOKEN) {
      // Can't query Contacts - create minimal record
      const newUser = await sql`
        INSERT INTO users (email, name)
        VALUES (${email}, ${name || null})
        RETURNING *
      `;
      return newUser.rows[0];
    }

    // Look up in Contacts
    const response = await fetch('https://api.quickbase.com/v1/records/query', {
      method: 'POST',
      headers: {
        'QB-Realm-Hostname': QB_REALM,
        'Authorization': `QB-USER-TOKEN ${QB_TOKEN}`,
        'Content-Type': 'application/json',
      },
      body: JSON.stringify({
        from: QB_TABLE_CONTACTS,
        select: [
          CONTACT_FIELDS.RECORD_ID,
          CONTACT_FIELDS.FULL_NAME,
          CONTACT_FIELDS.PHONE,
          CONTACT_FIELDS.REPCARD_ID,
          CONTACT_FIELDS.ENERFLO_USER_ID,
          CONTACT_FIELDS.SEQUIFI_USER_ID,
          CONTACT_FIELDS.REPCARD_OFFICE,
          CONTACT_FIELDS.REPCARD_TEAM,
        ],
        where: `{${CONTACT_FIELDS.EMAIL}.EX.'${email.replace(/'/g, "\\'")}'}`,
        options: { top: 1 },
      }),
    });

    if (response.ok) {
      const data = await response.json();
      if (data.data && data.data.length > 0) {
        const contact = data.data[0];

        // Create user with full Contact data
        const newUser = await sql`
          INSERT INTO users (
            email, name, phone,
            quickbase_contact_id,
            repcard_user_id, enerflo_user_id, sequifi_user_id,
            office, team,
            last_synced_from_contacts_at
          ) VALUES (
            ${email},
            ${contact[CONTACT_FIELDS.FULL_NAME]?.value || name || null},
            ${contact[CONTACT_FIELDS.PHONE]?.value},
            ${contact[CONTACT_FIELDS.RECORD_ID]?.value?.toString()},
            ${contact[CONTACT_FIELDS.REPCARD_ID]?.value?.toString()},
            ${contact[CONTACT_FIELDS.ENERFLO_USER_ID]?.value},
            ${contact[CONTACT_FIELDS.SEQUIFI_USER_ID]?.value},
            ${contact[CONTACT_FIELDS.REPCARD_OFFICE]?.value},
            ${contact[CONTACT_FIELDS.REPCARD_TEAM]?.value},
            NOW()
          )
          RETURNING *
        `;

        console.log(`[ensureUserExists] + Created user ${email} from Contacts`);
        return newUser.rows[0];
      }
    }

    // Not in Contacts - create minimal record
    const newUser = await sql`
      INSERT INTO users (email, name)
      VALUES (${email}, ${name || null})
      RETURNING *
    `;

    console.log(`[ensureUserExists] + Created minimal user ${email}`);
    return newUser.rows[0];
  } catch (error) {
    console.error('[ensureUserExists] Error:', error);
    throw error;
  }
}

/**
 * Get user role by email for message role determination
 * Returns 'pc' for operations roles, 'rep' for sales roles
 * 
 * @param email - User email to lookup
 * @returns 'pc' | 'rep' | null if user not found
 */
export async function getUserRoleByEmail(email: string): Promise<'pc' | 'rep' | null> {
  try {
    const result = await sql`
      SELECT role FROM users 
      WHERE LOWER(email) = ${email.toLowerCase()}
      LIMIT 1
    `;

    if (result.rows.length === 0) {
      return null;
    }

    const role = result.rows[0].role;
    
    // Map roles to PC/Rep classification
    if (['operations_coordinator', 'operations_manager', 'office_leader', 'regional', 'super_admin'].includes(role)) {
      return 'pc';
    } else if (['closer', 'setter'].includes(role)) {
      return 'rep';
    }
    
    // Default fallback - log for remediation
    console.warn(`[getUserRoleByEmail] Unknown role '${role}' for user ${email}, defaulting to 'rep'`);
    return 'rep';
    
  } catch (error) {
    console.error(`[getUserRoleByEmail] Error looking up role for ${email}:`, error);
    return null;
  }
}
