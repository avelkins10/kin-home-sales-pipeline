/**
 * Run Migration 019: Make repcard_offices.company_id nullable
 */

import { sql } from '@/lib/db/client';
import { readFileSync } from 'fs';
import { resolve } from 'path';

async function main() {
  const migrationPath = resolve(process.cwd(), 'lib/db/migrations/019_make_repcard_offices_company_id_nullable.sql');
  const migrationSQL = readFileSync(migrationPath, 'utf-8');

  console.log('🚀 Running Migration 019: Make repcard_offices.company_id nullable\n');
  
  try {
    // Execute the ALTER TABLE statement directly
    await sql`
      ALTER TABLE repcard_offices 
      ALTER COLUMN company_id DROP NOT NULL
    `;
    
    console.log('✅ Migration 019 completed successfully!\n');
    
    // Verify the change
    const result = await sql`
      SELECT is_nullable 
      FROM information_schema.columns 
      WHERE table_name = 'repcard_offices' 
        AND column_name = 'company_id'
    `;
    const getRows = (r: any) => r.rows || (Array.isArray(r) ? r : []);
    const row = getRows(result)[0];
    console.log(`Verification: company_id is_nullable = ${row?.is_nullable}`);
    
  } catch (error) {
    console.error('❌ Migration failed:', error);
    if (error instanceof Error) {
      console.error('Error message:', error.message);
    }
    process.exit(1);
  }
}

main()
  .then(() => process.exit(0))
  .catch((error) => {
    console.error('Fatal error:', error);
    process.exit(1);
  });

