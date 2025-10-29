#!/usr/bin/env ts-node

/**
 * Comprehensive Rep Sync Script
 *
 * This script syncs ALL reps from RepCard and QuickBase into the users table
 * as the master source of truth, regardless of whether they have app login.
 *
 * Data Flow:
 * 1. Fetch all users from RepCard API
 * 2. Fetch all closers from QuickBase projects (CLOSER_ID field)
 * 3. Upsert into users table with repcard_user_id, quickbase_user_id, email, name, office
 * 4. password_hash stays NULL for non-app users (they can't login until invited)
 *
 * Usage:
 *   npx ts-node scripts/sync-all-reps.ts
 *   npx ts-node scripts/sync-all-reps.ts --dry-run
 */

import path from 'path';
import { config } from 'dotenv';

// Load environment variables
config({ path: path.join(__dirname, '../.env.local') });

import { sql } from '@vercel/postgres';

// Protected system accounts that should NEVER be synced
const PROTECTED_EMAILS = [
  'admin@kinhome.com',
  // Add other system accounts here as needed
];
import { RepCardClient } from '../lib/repcard/client.js';
import type { RepCardUser } from '../lib/repcard/types.js';

// QuickBase API configuration
const QB_REALM = process.env.QUICKBASE_REALM!;
const QB_TOKEN = process.env.QUICKBASE_TOKEN!;
const QB_TABLE_PROJECTS = (process.env.QUICKBASE_TABLE_PROJECTS || 'br9kwm8na').trim();

// Field IDs from QuickBase
const PROJECT_FIELDS = {
  RECORD_ID: 3,
  CLOSER_ID: 516,
  CLOSER_NAME: 517,
  CLOSER_EMAIL: 356,
  SALES_OFFICE: 339,
};

interface SyncStats {
  totalRepCardUsers: number;
  totalQBClosers: number;
  uniqueReps: number;
  inserted: number;
  updated: number;
  skipped: number;
  errors: number;
}

interface RepRecord {
  email: string;
  name: string;
  repcardUserId?: string;
  quickbaseUserId?: string;
  office?: string;
  role: 'closer' | 'setter';
  source: 'repcard' | 'quickbase' | 'both';
}

class ComprehensiveRepSync {
  private repcardClient: RepCardClient;
  private stats: SyncStats;
  private dryRun: boolean;
  private repsMap: Map<string, RepRecord>;

  constructor(dryRun: boolean = false) {
    this.repcardClient = new RepCardClient();
    this.stats = {
      totalRepCardUsers: 0,
      totalQBClosers: 0,
      uniqueReps: 0,
      inserted: 0,
      updated: 0,
      skipped: 0,
      errors: 0,
    };
    this.dryRun = dryRun;
    this.repsMap = new Map();
  }

  /**
   * Main sync function
   */
  async sync(): Promise<void> {
    console.log('🚀 Starting comprehensive rep sync...');
    if (this.dryRun) {
      console.log('⚠️  DRY RUN MODE - No changes will be made\n');
    }

    try {
      // Verify environment
      await this.verifyEnvironment();

      // Fetch from RepCard
      await this.fetchRepCardUsers();

      // Fetch from QuickBase
      await this.fetchQuickBaseClosers();

      // Deduplicate and merge
      this.stats.uniqueReps = this.repsMap.size;
      console.log(`\n📊 Found ${this.stats.uniqueReps} unique reps across all sources`);

      // Upsert into users table
      await this.upsertAllReps();

      // Print summary
      this.printSummary();

    } catch (error) {
      console.error('❌ Sync failed:', error);
      throw error;
    } finally {
      await sql.end();
    }
  }

  /**
   * Verify environment and prerequisites
   */
  private async verifyEnvironment(): Promise<void> {
    if (!process.env.DATABASE_URL) {
      throw new Error('DATABASE_URL environment variable is not set');
    }

    if (!process.env.REPCARD_API_KEY) {
      throw new Error('REPCARD_API_KEY environment variable is not set');
    }

    if (!QB_REALM || !QB_TOKEN) {
      throw new Error('QuickBase credentials not configured');
    }

    // Test database connection
    try {
      await sql`SELECT 1`;
    } catch (error) {
      throw new Error(`Database connection failed: ${error}`);
    }

    console.log('✅ Environment verified\n');
  }

  /**
   * Fetch all users from RepCard API
   */
  private async fetchRepCardUsers(): Promise<void> {
    console.log('📡 Fetching users from RepCard API...');

    let page = 1;
    const perPage = 100;
    let hasMore = true;
    let totalFetched = 0;

    while (hasMore) {
      try {
        const response = await this.repcardClient.getUsersMinimal({
          perPage,
          page,
        });

        if (response.result.data.length === 0) {
          hasMore = false;
          break;
        }

        // Fetch detailed info for each user
        for (const minimalUser of response.result.data) {
          try {
            const detailsResponse = await this.repcardClient.getUserDetails(minimalUser.id);
            const user = detailsResponse.result;

            if (user.email) {
              const email = user.email.toLowerCase();
              const existing = this.repsMap.get(email);

              this.repsMap.set(email, {
                email,
                name: `${user.firstName} ${user.lastName}`,
                repcardUserId: user.id.toString(),
                quickbaseUserId: existing?.quickbaseUserId,
                office: undefined, // Office comes from QuickBase
                role: user.role?.toLowerCase() === 'setter' ? 'setter' : 'closer',
                source: existing ? 'both' : 'repcard',
              });

              totalFetched++;
            }
          } catch (error) {
            console.warn(`⚠️  Failed to fetch details for RepCard user ${minimalUser.id}`);
          }
        }

        if (response.result.data.length < perPage) {
          hasMore = false;
        } else {
          page++;
        }
      } catch (error) {
        console.error(`❌ Failed to fetch RepCard page ${page}:`, error);
        hasMore = false;
      }
    }

    this.stats.totalRepCardUsers = totalFetched;
    console.log(`✅ Fetched ${totalFetched} users from RepCard\n`);
  }

  /**
   * Fetch all closers from QuickBase projects
   */
  private async fetchQuickBaseClosers(): Promise<void> {
    console.log('📡 Fetching closers from QuickBase...');

    try {
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
          where: `{${PROJECT_FIELDS.CLOSER_EMAIL}.XEX.''}`, // CLOSER_EMAIL is not empty
          options: {
            top: 5000,
          },
        }),
      });

      if (!response.ok) {
        throw new Error(`QuickBase API error: ${response.status} ${response.statusText}`);
      }

      const data = await response.json();
      const projects = data.data || [];

      // Group by closer email to get unique closers
      const closerMap = new Map<string, any>();

      for (const project of projects) {
        const closerId = project[PROJECT_FIELDS.CLOSER_ID]?.value;
        const closerEmail = project[PROJECT_FIELDS.CLOSER_EMAIL]?.value;
        const closerName = project[PROJECT_FIELDS.CLOSER_NAME]?.value;
        const office = project[PROJECT_FIELDS.SALES_OFFICE]?.value;

        if (!closerEmail) continue;

        const email = closerEmail.toLowerCase();

        if (!closerMap.has(email)) {
          closerMap.set(email, {
            closerId,
            name: closerName,
            email,
            office,
          });
        }
      }

      // Merge with RepCard data
      for (const [email, closer] of Array.from(closerMap.entries())) {
        const existing = this.repsMap.get(email);

        // Extract RepCard ID from QuickBase CLOSER_ID if it's an object
        let repcardId = closer.closerId;
        if (typeof repcardId === 'object' && repcardId?.id) {
          repcardId = repcardId.id.toString();
        } else if (typeof repcardId === 'number') {
          repcardId = repcardId.toString();
        }

        this.repsMap.set(email, {
          email,
          name: existing?.name || closer.name || 'Unknown',
          repcardUserId: existing?.repcardUserId || repcardId,
          quickbaseUserId: undefined, // QB doesn't have a stable user ID in projects
          office: existing?.office || closer.office,
          role: 'closer',
          source: existing ? 'both' : 'quickbase',
        });
      }

      this.stats.totalQBClosers = closerMap.size;
      console.log(`✅ Found ${closerMap.size} unique closers in QuickBase\n`);

    } catch (error) {
      console.error('❌ Failed to fetch QuickBase closers:', error);
      throw error;
    }
  }

  /**
   * Upsert all reps into users table
   */
  private async upsertAllReps(): Promise<void> {
    console.log('💾 Upserting reps into users table...\n');

    for (const [email, rep] of Array.from(this.repsMap.entries())) {
      try {
        // Skip protected system accounts (admin, etc.)
        if (PROTECTED_EMAILS.includes(email)) {
          console.log(`🔒 Skipping protected system account: ${email}`);
          this.stats.skipped++;
          continue;
        }

        if (this.dryRun) {
          console.log(`[DRY RUN] Would upsert: ${rep.name} (${email}) - RepCard ID: ${rep.repcardUserId || 'N/A'}`);
          continue;
        }

        // Check if user exists
        const existing = await sql`
          SELECT id, repcard_user_id, name
          FROM users
          WHERE LOWER(email) = ${email}
        `;

        if (existing.rows.length > 0) {
          // Update existing user
          const user = existing.rows[0];

          // Only update external IDs and sync metadata
          // NEVER overwrite name, role, or sales_office - the app is the source of truth
          await sql`
            UPDATE users
            SET repcard_user_id = COALESCE(${rep.repcardUserId}, repcard_user_id),
                last_synced_at = NOW(),
                sync_confidence = 1.0
            WHERE id = ${user.id}
          `;

          // Log to sync log
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
              ${rep.source},
              ${rep.repcardUserId || 'N/A'},
              'email',
              1.0,
              NULL,
              NOW(),
              ${`Synced from ${rep.source}. Office: ${rep.office || 'Unknown'}`}
            )
          `;

          this.stats.updated++;
          console.log(`✅ Updated: ${rep.name} (${email})`);

        } else {
          // Insert new user (no password, can't login)
          let result;
          if (rep.office) {
            result = await sql.query(
              `INSERT INTO users (
                email, password_hash, name, role, repcard_user_id, sales_office,
                last_synced_at, sync_confidence, is_active
              ) VALUES (
                $1, NULL, $2, $3, $4, ARRAY[$5]::text[], NOW(), 1.0, true
              )
              RETURNING id`,
              [email, rep.name, rep.role, rep.repcardUserId, rep.office]
            );
          } else {
            result = await sql`
              INSERT INTO users (
                email, password_hash, name, role, repcard_user_id,
                last_synced_at, sync_confidence, is_active
              ) VALUES (
                ${email}, NULL, ${rep.name}, ${rep.role}, ${rep.repcardUserId},
                NOW(), 1.0, true
              )
              RETURNING id
            `;
          }

          const userId = result.rows[0].id;

          // Log to sync log
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
              ${userId},
              ${rep.source},
              ${rep.repcardUserId || 'N/A'},
              'email',
              1.0,
              NULL,
              NOW(),
              ${`Created from ${rep.source}. Office: ${rep.office || 'Unknown'}. No login access.`}
            )
          `;

          this.stats.inserted++;
          console.log(`✅ Inserted: ${rep.name} (${email})`);
        }

      } catch (error) {
        this.stats.errors++;
        console.error(`❌ Failed to upsert ${email}:`, error);
      }
    }
  }

  /**
   * Print sync summary
   */
  private printSummary(): void {
    console.log('\n🎉 Sync complete!');
    console.log('📊 Summary:');
    console.log(`   - RepCard users fetched: ${this.stats.totalRepCardUsers}`);
    console.log(`   - QuickBase closers found: ${this.stats.totalQBClosers}`);
    console.log(`   - Unique reps total: ${this.stats.uniqueReps}`);
    console.log(`   - ${this.dryRun ? 'Would be inserted' : 'Inserted'}: ${this.stats.inserted}`);
    console.log(`   - ${this.dryRun ? 'Would be updated' : 'Updated'}: ${this.stats.updated}`);
    console.log(`   - Errors: ${this.stats.errors}`);

    console.log('\n💡 Next steps:');
    console.log('   1. Analytics will now show all reps (not just app users)');
    console.log('   2. Run this sync regularly to keep data fresh');
    console.log('   3. Invite reps to app - they\'ll inherit all historical data');
  }
}

// Parse command line arguments
function parseArgs(): { dryRun: boolean } {
  const args = process.argv.slice(2);
  return {
    dryRun: args.includes('--dry-run'),
  };
}

// Run sync if called directly
if (require.main === module) {
  const { dryRun } = parseArgs();
  const sync = new ComprehensiveRepSync(dryRun);

  sync
    .sync()
    .then(() => {
      console.log('✅ Sync script completed successfully');
      process.exit(0);
    })
    .catch(error => {
      console.error('❌ Sync script failed:', error);
      process.exit(1);
    });
}

export { ComprehensiveRepSync };
