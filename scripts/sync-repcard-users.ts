#!/usr/bin/env ts-node

/**
 * RepCard User Sync Script
 *
 * This script syncs users from RepCard API to our local database:
 * 1. Fetches all users from RepCard API
 * 2. Matches each RepCard user to local users by email
 * 3. Updates users.repcard_user_id with the RepCard ID
 * 4. Compares office assignments and logs discrepancies
 * 5. Logs all sync operations to user_sync_log table
 * 6. Provides detailed report of sync results
 *
 * Usage:
 *   npm run sync:repcard              # Sync all users
 *   npm run sync:repcard -- --company-id=597  # Sync specific company
 *   npm run sync:repcard -- --dry-run         # Preview changes without updating
 */

import path from 'path';
import { config } from 'dotenv';

// Load environment variables
config({ path: path.join(__dirname, '../.env.local') });

import { sql } from '@vercel/postgres';
import { RepCardClient } from '../lib/repcard/client';
import type { RepCardUser, RepCardUserMinimal, RepCardOffice } from '../lib/repcard/types';

interface SyncOptions {
  companyId?: number;
  dryRun?: boolean;
  verbose?: boolean;
}

interface SyncStats {
  totalRepcardUsers: number;
  matched: number;
  updated: number;
  alreadySynced: number;
  notFound: number;
  officeDiscrepancies: number;
  errors: number;
}

interface OfficeDiscrepancy {
  email: string;
  repcardOfficeId: number;
  repcardOfficeName: string;
  localOfficeName: string | null;
  quickbaseOfficeName: string | null;
}

class RepCardUserSync {
  private client: RepCardClient;
  private stats: SyncStats;
  private officeDiscrepancies: OfficeDiscrepancy[];
  private options: SyncOptions;

  constructor(options: SyncOptions = {}) {
    this.client = new RepCardClient();
    this.stats = {
      totalRepcardUsers: 0,
      matched: 0,
      updated: 0,
      alreadySynced: 0,
      notFound: 0,
      officeDiscrepancies: 0,
      errors: 0,
    };
    this.officeDiscrepancies = [];
    this.options = options;
  }

  /**
   * Main sync function
   */
  async sync(): Promise<void> {
    console.log('🚀 Starting RepCard user sync...');
    if (this.options.dryRun) {
      console.log('⚠️  DRY RUN MODE - No changes will be made');
    }

    try {
      // Verify environment
      await this.verifyEnvironment();

      // Fetch RepCard users
      const repcardUsers = await this.fetchRepCardUsers();
      this.stats.totalRepcardUsers = repcardUsers.length;
      console.log(`✅ Fetched ${repcardUsers.length} users from RepCard`);

      // Fetch RepCard offices for reference
      const offices = await this.fetchRepCardOffices();
      const officeMap = new Map(offices.map(o => [o.id, o]));
      console.log(`✅ Fetched ${offices.length} offices from RepCard`);

      // Process each user
      for (const repcardUser of repcardUsers) {
        await this.syncUser(repcardUser, officeMap);
      }

      // Print summary
      this.printSummary();

    } catch (error) {
      console.error('❌ Sync failed:', error);
      throw error;
    } finally {
      // Close database connection
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

    // Test database connection
    try {
      await sql`SELECT 1`;
    } catch (error) {
      throw new Error(`Database connection failed: ${error}`);
    }

    console.log('✅ Environment verified');
  }

  /**
   * Fetch all users from RepCard API
   */
  private async fetchRepCardUsers(): Promise<RepCardUser[]> {
    console.log('📡 Fetching users from RepCard API...');

    const allUsers: RepCardUser[] = [];
    let page = 1;
    const perPage = 100;
    let hasMore = true;

    while (hasMore) {
      try {
        const response = await this.client.getUsersMinimal({
          companyId: this.options.companyId,
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
            const detailsResponse = await this.client.getUserDetails(minimalUser.id);
            allUsers.push(detailsResponse.result);
          } catch (error) {
            console.warn(`⚠️  Failed to fetch details for user ${minimalUser.id}:`, error);
          }
        }

        if (this.options.verbose) {
          console.log(`  Fetched page ${page} (${allUsers.length} users so far)`);
        }

        // Check if there are more pages
        if (response.result.data.length < perPage) {
          hasMore = false;
        } else {
          page++;
        }
      } catch (error) {
        console.error(`❌ Failed to fetch page ${page}:`, error);
        hasMore = false;
      }
    }

    return allUsers;
  }

  /**
   * Fetch all offices from RepCard API
   */
  private async fetchRepCardOffices(): Promise<RepCardOffice[]> {
    try {
      const response = await this.client.getOffices(this.options.companyId);
      return response.result;
    } catch (error) {
      console.warn('⚠️  Failed to fetch offices:', error);
      return [];
    }
  }

  /**
   * Sync a single user
   */
  private async syncUser(
    repcardUser: RepCardUser,
    officeMap: Map<number, RepCardOffice>
  ): Promise<void> {
    try {
      if (!repcardUser.email) {
        if (this.options.verbose) {
          console.warn(`⚠️  RepCard user ${repcardUser.id} has no email, skipping`);
        }
        return;
      }

      const email = repcardUser.email.toLowerCase();

      // Find user in our database by email
      const userResult = await sql`
        SELECT id, email, name, repcard_user_id
        FROM users
        WHERE LOWER(email) = ${email}
      `;

      if (userResult.rows.length === 0) {
        this.stats.notFound++;
        if (this.options.verbose) {
          console.log(`⚠️  No local user found for email: ${email}`);
        }
        return;
      }

      const localUser = userResult.rows[0];
      this.stats.matched++;

      // Check if RepCard ID already set
      if (localUser.repcard_user_id) {
        if (localUser.repcard_user_id === repcardUser.id.toString()) {
          this.stats.alreadySynced++;
          if (this.options.verbose) {
            console.log(`✓ User ${email} already synced with RepCard ID: ${repcardUser.id}`);
          }
        } else {
          console.warn(
            `⚠️  User ${email} has different RepCard ID: ${localUser.repcard_user_id} vs ${repcardUser.id}`
          );
          // Don't overwrite - manual review needed
        }
        return;
      }

      // Check office assignment
      if (repcardUser.officeId) {
        const repcardOffice = officeMap.get(repcardUser.officeId);
        if (repcardOffice) {
          await this.checkOfficeAlignment(email, repcardOffice);
        }
      }

      // Update user with RepCard ID (unless dry run)
      if (!this.options.dryRun) {
        await sql`
          UPDATE users
          SET repcard_user_id = ${repcardUser.id.toString()},
              last_synced_at = NOW(),
              sync_confidence = 1.0
          WHERE id = ${localUser.id}
        `;

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
            ${localUser.id},
            'repcard',
            ${repcardUser.id.toString()},
            'email',
            1.0,
            NULL,
            NOW(),
            ${`Synced from RepCard API. Office: ${repcardUser.officeName || 'Unknown'}`}
          )
        `;
      }

      this.stats.updated++;
      console.log(
        `✅ ${this.options.dryRun ? '[DRY RUN] Would update' : 'Updated'} ${email} with RepCard ID: ${repcardUser.id}`
      );
    } catch (error) {
      this.stats.errors++;
      console.error(`❌ Error syncing user ${repcardUser.email}:`, error);
    }
  }

  /**
   * Check if office assignment aligns with QuickBase
   */
  private async checkOfficeAlignment(
    email: string,
    repcardOffice: RepCardOffice
  ): Promise<void> {
    try {
      // Get office from local database (from office_assignments)
      const localOfficeResult = await sql`
        SELECT o.name
        FROM office_assignments oa
        JOIN users u ON oa.user_id = u.id
        JOIN offices o ON oa.office_name = o.name
        WHERE LOWER(u.email) = ${email}
        LIMIT 1
      `;

      const localOfficeName = localOfficeResult.rows.length > 0
        ? localOfficeResult.rows[0].name
        : null;

      // Get office from QuickBase projects (most recent)
      // This would require querying QuickBase, but for now we'll skip
      const quickbaseOfficeName = null; // TODO: Implement QuickBase office lookup

      // Check for discrepancies
      if (localOfficeName && localOfficeName !== repcardOffice.name) {
        this.officeDiscrepancies.push({
          email,
          repcardOfficeId: repcardOffice.id,
          repcardOfficeName: repcardOffice.name,
          localOfficeName,
          quickbaseOfficeName,
        });
        this.stats.officeDiscrepancies++;
      }
    } catch (error) {
      if (this.options.verbose) {
        console.warn(`⚠️  Failed to check office alignment for ${email}:`, error);
      }
    }
  }

  /**
   * Print sync summary
   */
  private printSummary(): void {
    console.log('\n🎉 Sync complete!');
    console.log('📊 Summary:');
    console.log(`   - Total RepCard users: ${this.stats.totalRepcardUsers}`);
    console.log(`   - Matched to local users: ${this.stats.matched}`);
    console.log(`   - ${this.options.dryRun ? 'Would be updated' : 'Successfully updated'}: ${this.stats.updated}`);
    console.log(`   - Already synced (skipped): ${this.stats.alreadySynced}`);
    console.log(`   - Not found in local DB: ${this.stats.notFound}`);
    console.log(`   - Office discrepancies: ${this.stats.officeDiscrepancies}`);
    console.log(`   - Errors: ${this.stats.errors}`);

    if (this.officeDiscrepancies.length > 0) {
      console.log('\n⚠️  Office Discrepancies:');
      this.officeDiscrepancies.forEach(d => {
        console.log(
          `   ${d.email}: RepCard="${d.repcardOfficeName}" vs Local="${d.localOfficeName}"`
        );
      });
    }

    console.log('\n💡 Next steps:');
    console.log('   1. Review any office discrepancies');
    console.log('   2. Review users not found in local database');
    console.log('   3. Verify RepCard IDs in QuickBase match RepCard API');
  }
}

// Parse command line arguments
function parseArgs(): SyncOptions {
  const args = process.argv.slice(2);
  const options: SyncOptions = {
    dryRun: false,
    verbose: false,
  };

  for (const arg of args) {
    if (arg === '--dry-run') {
      options.dryRun = true;
    } else if (arg === '--verbose' || arg === '-v') {
      options.verbose = true;
    } else if (arg.startsWith('--company-id=')) {
      options.companyId = parseInt(arg.split('=')[1]);
    }
  }

  return options;
}

// Run sync if called directly
if (require.main === module) {
  const options = parseArgs();
  const sync = new RepCardUserSync(options);

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

export { RepCardUserSync };
