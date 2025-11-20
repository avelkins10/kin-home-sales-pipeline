/**
 * Ensure RepCard tables exist before syncing
 * Automatically runs migrations if tables are missing
 */

import { sql } from '@/lib/db/client';
import { readFileSync } from 'fs';
import { resolve } from 'path';

const MIGRATION_FILES = [
  '012_repcard_sync_tables.sql',
  '014_repcard_comprehensive_tables.sql',
  '016_repcard_complete_data.sql',
  '017_repcard_settings.sql',
  '017_make_repcard_users_company_id_nullable.sql',
  '018_normalize_repcard_user_ids_to_integer.sql',
  '018_complete_normalize_user_ids.sql'
];

export async function ensureRepCardTables(): Promise<{ created: boolean; tablesCreated: string[] }> {
  try {
    // Check if any RepCard tables exist
    const tablesCheck = await sql`
      SELECT table_name 
      FROM information_schema.tables 
      WHERE table_schema = 'public'
        AND table_name LIKE 'repcard_%'
      LIMIT 1
    `;
    
    const tables = Array.from(tablesCheck);
    
    // If tables exist, we're good
    if (tables.length > 0) {
      console.log('[RepCard Tables] Tables already exist, skipping migrations');
      return { created: false, tablesCreated: [] };
    }

    console.log('[RepCard Tables] No tables found, running migrations...');
    const tablesCreated: string[] = [];

    // Run migrations in order
    for (const migrationFile of MIGRATION_FILES) {
      try {
        const migrationPath = resolve(process.cwd(), `lib/db/migrations/${migrationFile}`);
        const migrationSQL = readFileSync(migrationPath, 'utf-8');
        
        console.log(`[RepCard Tables] Running migration: ${migrationFile}`);
        await (sql as any).query(migrationSQL);
        tablesCreated.push(migrationFile);
        console.log(`[RepCard Tables] ✅ Migration ${migrationFile} completed`);
      } catch (error: any) {
        // Some migrations might fail if already applied or if dependencies aren't met
        // Log but continue - migrations use IF NOT EXISTS so they're safe to re-run
        if (error?.message?.includes('already exists') || 
            error?.message?.includes('does not exist') ||
            error?.code === '42P07' ||
            error?.code === '42704') {
          console.log(`[RepCard Tables] ⚠️ Migration ${migrationFile} skipped (already applied or dependency missing):`, error.message);
        } else {
          console.error(`[RepCard Tables] ❌ Migration ${migrationFile} failed:`, error.message);
          // Continue with other migrations even if one fails
        }
      }
    }

    // Verify tables were created
    const verifyCheck = await sql`
      SELECT table_name 
      FROM information_schema.tables 
      WHERE table_schema = 'public'
        AND table_name LIKE 'repcard_%'
      ORDER BY table_name
    `;
    
    const createdTables = Array.from(verifyCheck).map((t: any) => t.table_name);
    console.log(`[RepCard Tables] ✅ Created ${createdTables.length} tables:`, createdTables);

    return { created: true, tablesCreated: createdTables };
  } catch (error) {
    console.error('[RepCard Tables] Error ensuring tables:', error);
    throw error;
  }
}


