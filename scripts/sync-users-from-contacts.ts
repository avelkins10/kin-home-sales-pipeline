/**
 * Daily sync script to refresh all existing users from QuickBase Contacts table
 *
 * This script queries all users from the local database and refreshes their data
 * from the QuickBase Contacts table using the existing enrichUserFromContacts() function.
 *
 * PROTECTED FIELDS (never overwritten):
 * - name: User's display name (managed by admins in the app)
 * - role: User's role - closer, setter, office_leader, etc. (managed by admins)
 * - sales_office: Array of offices for access control (managed via office_assignments)
 * - password_hash: Security field (never sync from external sources)
 * - email: Primary identifier (should not change after creation)
 *
 * This script only updates external IDs and metadata using COALESCE (fill missing values only).
 * See lib/constants/protected-fields.ts for the complete list of protected fields.
 *
 * Usage:
 *   npm run sync:contacts                    # Full sync of all users
 *   npm run sync:contacts:dry-run          # Preview changes without updating
 *   npm run sync:contacts -- --limit=10    # Test with first 10 users
 *   npm run sync:contacts -- --force       # Force sync even if recently synced
 *   npm run sync:contacts -- --verbose     # Detailed logging
 *
 * @author Kin Home Sales Pipeline Team
 * @version 1.0.0
 */

import path from 'path';
import { sql } from '@vercel/postgres';

// Protected system accounts that should NEVER be synced
const PROTECTED_EMAILS = [
  'admin@kinhome.com',
  // Add other system accounts here as needed
];

interface SyncOptions {
  dryRun: boolean;
  verbose: boolean;
  force: boolean;
  includeTest: boolean;
  limit?: number;
}

interface SyncStats {
  totalUsers: number;
  enriched: number;
  wouldEnrich: number;
  notFoundInContacts: number;
  notFoundSamples: string[];
  alreadyUpToDate: number;
  errors: number;
  errorDetails: Array<{email: string, error: string}>;
}

interface SyncResult {
  success: boolean;
  stats: SyncStats;
  executionTimeMs: number;
  timestamp: string;
}

interface User {
  id: string;
  email: string;
  name: string;
  last_synced_from_contacts_at: Date | null;
}

class ContactsSyncService {
  private stats: SyncStats;

  constructor() {
    this.stats = {
      totalUsers: 0,
      enriched: 0,
      wouldEnrich: 0,
      notFoundInContacts: 0,
      notFoundSamples: [],
      alreadyUpToDate: 0,
      errors: 0,
      errorDetails: []
    };
  }

  async sync(options: SyncOptions): Promise<SyncResult> {
    const startTime = Date.now();
    const timestamp = new Date().toISOString();
    
    console.log('🔄 Starting Contacts sync...');
    console.log(`Options: ${JSON.stringify(options, null, 2)}`);
    
    // Verify environment
    await this.verifyEnvironment();

    try {
      // Query all users from local database
      const users = await this.getUsers(options.limit);
      this.stats.totalUsers = users.length;
      
      console.log(`📊 Found ${users.length} users to sync`);
      
      if (options.dryRun) {
        console.log('🔍 DRY RUN MODE - No changes will be made');
      }

      // Process each user
      for (let i = 0; i < users.length; i++) {
        const user = users[i];
        
        // Show progress
        if (!options.verbose && (i + 1) % 10 === 0) {
          console.log(`📈 Processed ${i + 1}/${users.length} users...`);
        }
        
        await this.syncUser(user, options);
      }

      // Print summary
      this.printSummary();
      
      const executionTimeMs = Date.now() - startTime;
      const success = this.stats.errors === 0; // Only mark as success when no errors
      
      return {
        success,
        stats: { ...this.stats },
        executionTimeMs,
        timestamp
      };
      
    } catch (error) {
      console.error('❌ Sync failed:', error);
      const executionTimeMs = Date.now() - startTime;
      
      return {
        success: false,
        stats: { ...this.stats },
        executionTimeMs,
        timestamp
      };
    }
  }

  private async getUsers(limit?: number): Promise<User[]> {
    const limitClause = limit ? `LIMIT ${limit}` : '';
    
    const { rows } = await sql`
      SELECT id, email, name, last_synced_from_contacts_at 
      FROM users 
      WHERE email IS NOT NULL AND email != ''
      ORDER BY last_synced_from_contacts_at ASC NULLS FIRST
      ${sql.unsafe(limitClause)}
    `;
    
    return rows as User[];
  }

  private async syncUser(user: User, options: SyncOptions): Promise<void> {
    try {
      // Skip protected system accounts (admin, etc.)
      if (PROTECTED_EMAILS.includes(user.email)) {
        if (options.verbose) {
          console.log(`🔒 Skipping protected system account: ${user.email}`);
        }
        return;
      }

      // Skip test users unless includeTest flag is set
      if (!options.includeTest && user.email.includes('@test.com')) {
        if (options.verbose) {
          console.log(`⏭️  Skipping test user: ${user.email}`);
        }
        return;
      }

      // Check if user was recently synced (within 23 hours)
      if (!options.force && user.last_synced_from_contacts_at) {
        const hoursSinceSync = (Date.now() - user.last_synced_from_contacts_at.getTime()) / (1000 * 60 * 60);
        if (hoursSinceSync < 23) {
          this.stats.alreadyUpToDate++;
          if (options.verbose) {
            console.log(`✅ Already up-to-date: ${user.email} (synced ${Math.round(hoursSinceSync)}h ago)`);
          }
          return;
        }
      }

      if (options.verbose) {
        console.log(`🔄 Syncing user: ${user.email}`);
      }

      if (options.dryRun) {
        console.log(`🔍 [DRY RUN] Would sync: ${user.email}`);
        this.stats.wouldEnrich++;
        return;
      }

      // Call the existing enrichment function with retry logic
      const { enrichUserFromContacts } = await import('../lib/users/enrich-user');
      const result = await this.syncUserWithRetry(user, enrichUserFromContacts, options);
      
      if (result.status === 'updated') {
        this.stats.enriched++;
        if (options.verbose) {
          console.log(`✅ Enriched: ${user.email}`);
        }
      } else if (result.status === 'not_found') {
        this.stats.notFoundInContacts++;
        // Collect sample of not-found emails (first 20)
        if (this.stats.notFoundSamples.length < 20) {
          this.stats.notFoundSamples.push(user.email);
        }
        if (options.verbose) {
          console.log(`⚠️  Not found in Contacts: ${user.email}`);
        }
      } else if (result.status === 'error') {
        this.stats.errors++;
        const errorMessage = result.error || 'Unknown error';
        this.stats.errorDetails.push({
          email: user.email,
          error: errorMessage
        });
        console.error(`❌ Error enriching ${user.email}:`, errorMessage);
      }

    } catch (error) {
      this.stats.errors++;
      const errorMessage = error instanceof Error ? error.message : String(error);
      this.stats.errorDetails.push({
        email: user.email,
        error: errorMessage
      });
      
      console.error(`❌ Error syncing ${user.email}:`, errorMessage);
      
      // Continue processing other users
    }
  }

  private async syncUserWithRetry(
    user: User, 
    enrichUserFromContacts: Function, 
    options: SyncOptions,
    maxRetries: number = 3
  ): Promise<any> {
    let lastError: Error | null = null;
    
    for (let attempt = 1; attempt <= maxRetries; attempt++) {
      try {
        const result = await enrichUserFromContacts(user.id, user.email);
        
        // If successful or not_found, return immediately
        if (result.status === 'updated' || result.status === 'not_found') {
          return result;
        }
        
        // If error, check if it's retryable
        if (result.status === 'error') {
          const errorMessage = result.error || 'Unknown error';
          
          // Check if error is retryable (network/timeout/5xx errors)
          const isRetryable = this.isRetryableError(errorMessage);
          
          if (!isRetryable || attempt === maxRetries) {
            return result; // Return the error result
          }
          
          // Wait with exponential backoff before retry
          const delayMs = Math.min(1000 * Math.pow(2, attempt - 1), 8000); // Max 8 seconds
          if (options.verbose) {
            console.log(`🔄 Retrying ${user.email} (attempt ${attempt}/${maxRetries}) after ${delayMs}ms delay`);
          }
          
          await new Promise(resolve => setTimeout(resolve, delayMs));
          lastError = new Error(errorMessage);
        }
        
      } catch (error) {
        lastError = error instanceof Error ? error : new Error(String(error));
        
        // Check if error is retryable
        const isRetryable = this.isRetryableError(lastError.message);
        
        if (!isRetryable || attempt === maxRetries) {
          break; // Exit retry loop
        }
        
        // Wait with exponential backoff before retry
        const delayMs = Math.min(1000 * Math.pow(2, attempt - 1), 8000); // Max 8 seconds
        if (options.verbose) {
          console.log(`🔄 Retrying ${user.email} (attempt ${attempt}/${maxRetries}) after ${delayMs}ms delay`);
        }
        
        await new Promise(resolve => setTimeout(resolve, delayMs));
      }
    }
    
    // All retries exhausted, return error result
    return {
      status: 'error',
      error: lastError?.message || 'Max retries exceeded'
    };
  }

  private isRetryableError(errorMessage: string): boolean {
    const retryablePatterns = [
      /timeout/i,
      /network/i,
      /connection/i,
      /5\d\d/i, // 5xx HTTP status codes
      /rate.?limit/i,
      /temporary/i,
      /unavailable/i
    ];
    
    return retryablePatterns.some(pattern => pattern.test(errorMessage));
  }

  private async verifyEnvironment(): Promise<void> {
    const requiredEnvVars = ['DATABASE_URL', 'QUICKBASE_REALM', 'QUICKBASE_TOKEN'];
    
    for (const envVar of requiredEnvVars) {
      if (!process.env[envVar]) {
        throw new Error(`Missing required environment variable: ${envVar}`);
      }
    }

    // Test database connection
    try {
      await sql`SELECT 1 as test`;
      console.log('✅ Database connection verified');
    } catch (error) {
      throw new Error(`Database connection failed: ${error}`);
    }

    console.log('✅ Environment verification complete');
  }

  private printSummary(): void {
    const timestamp = new Date().toISOString();
    const successRate = this.stats.totalUsers > 0 
      ? Math.round((this.stats.enriched / this.stats.totalUsers) * 100) 
      : 0;
    
    console.log('\n📊 SYNC SUMMARY');
    console.log('='.repeat(50));
    console.log(`Timestamp: ${timestamp}`);
    console.log(`Total users processed: ${this.stats.totalUsers}`);
    
    if (this.stats.wouldEnrich > 0) {
      console.log(`Would enrich: ${this.stats.wouldEnrich} (DRY RUN)`);
    } else {
      console.log(`Successfully enriched: ${this.stats.enriched} (${successRate}%)`);
    }
    
    console.log(`Not found in Contacts: ${this.stats.notFoundInContacts}`);
    console.log(`Already up-to-date: ${this.stats.alreadyUpToDate}`);
    console.log(`Errors: ${this.stats.errors}`);
    
    // Warnings
    if (this.stats.errors > 0) {
      const errorRate = Math.round((this.stats.errors / this.stats.totalUsers) * 100);
      if (errorRate > 5) {
        console.log(`\n⚠️  WARNING: High error rate (${errorRate}%)`);
        console.log('   Review errors and check QuickBase API credentials');
      }
    }
    
    if (this.stats.notFoundInContacts > 0) {
      const notFoundRate = Math.round((this.stats.notFoundInContacts / this.stats.totalUsers) * 100);
      if (notFoundRate > 20) {
        console.log(`\n⚠️  WARNING: High not-found rate (${notFoundRate}%)`);
        console.log('   Check email addresses match between systems');
      }
      
      // Show sample of not-found emails
      console.log(`\n📧 NOT-FOUND EMAILS (${this.stats.notFoundSamples.length} of ${this.stats.notFoundInContacts}):`);
      this.stats.notFoundSamples.forEach(email => {
        console.log(`   • ${email}`);
      });
      if (this.stats.notFoundInContacts > 20) {
        console.log(`   ... and ${this.stats.notFoundInContacts - 20} more`);
      }
    }
    
    // Error details
    if (this.stats.errorDetails.length > 0) {
      console.log('\n❌ ERROR DETAILS:');
      this.stats.errorDetails.forEach(({ email, error }) => {
        console.log(`   ${email}: ${error}`);
      });
    }
    
    // Next steps
    console.log('\n💡 NEXT STEPS:');
    if (this.stats.errors > 0) {
      console.log('   • Review error details above');
      console.log('   • Check QuickBase API credentials and rate limits');
      console.log('   • Run with --verbose for detailed logging');
    }
    if (this.stats.notFoundInContacts > 0) {
      console.log('   • Verify email addresses in QuickBase Contacts table');
      console.log('   • Check Contacts table data quality');
    }
    if (this.stats.enriched > 0) {
      console.log('   • User data successfully refreshed from Contacts');
    }
    
    console.log('\n✅ Sync completed');
  }
}

function parseArgs(): SyncOptions {
  const args = process.argv.slice(2);
  
  return {
    dryRun: args.includes('--dry-run'),
    verbose: args.includes('--verbose') || args.includes('-v'),
    force: args.includes('--force'),
    includeTest: args.includes('--include-test'),
    limit: args.find(arg => arg.startsWith('--limit='))?.split('=')[1] 
      ? parseInt(args.find(arg => arg.startsWith('--limit='))!.split('=')[1]) 
      : undefined
  };
}

// Export function for API routes
async function runSync(options: SyncOptions): Promise<SyncResult> {
  const syncService = new ContactsSyncService();
  const result = await syncService.sync(options);
  
  return result;
}

// Main execution
if (require.main === module) {
  // Load environment variables for CLI usage only
  const dotenv = require('dotenv');
  dotenv.config({ path: path.join(__dirname, '..', '.env.local') });
  
  (async () => {
    try {
      const options = parseArgs();
      const result = await runSync(options);
      
      // Close database connection for CLI usage
      await sql.end();
      
      if (result.success) {
        process.exit(0);
      } else {
        process.exit(1);
      }
    } catch (error) {
      console.error('❌ Script failed:', error);
      process.exit(1);
    }
  })();
}

export { ContactsSyncService, runSync, SyncResult, SyncOptions, SyncStats };
