#!/usr/bin/env tsx
import { config } from 'dotenv';
import { resolve } from 'path';
config({ path: resolve(process.cwd(), '.env.local') });
if (process.env.DATABASE_URL && !process.env.POSTGRES_URL) {
  process.env.POSTGRES_URL = process.env.DATABASE_URL;
}
import { sql } from '@/lib/db/client';
import { readFileSync } from 'fs';

(async () => {
  try {
    // Check if repcard_customers exists
    const check = await sql`
      SELECT EXISTS (
        SELECT 1 FROM information_schema.tables 
        WHERE table_schema = 'public' 
        AND table_name = 'repcard_customers'
      ) as exists;
    `;
    const exists = Array.from(check)[0]?.exists;
    console.log('repcard_customers exists:', exists);
    
    if (!exists) {
      console.log('\nTrying to create repcard_customers manually...');
      const migration = readFileSync('lib/db/migrations/012_repcard_sync_tables.sql', 'utf-8');
      // Extract just the repcard_customers table creation
      const createTableSQL = migration.match(/CREATE TABLE IF NOT EXISTS repcard_customers[^;]+;/s)?.[0];
      if (createTableSQL) {
        await (sql as any).query(createTableSQL);
        console.log('✅ Created repcard_customers');
        
        // Verify it was created
        const check2 = await sql`
          SELECT EXISTS (
            SELECT 1 FROM information_schema.tables 
            WHERE table_schema = 'public' 
            AND table_name = 'repcard_customers'
          ) as exists;
        `;
        console.log('After creation, exists:', Array.from(check2)[0]?.exists);
      }
    }
  } catch (error: any) {
    console.error('Error:', error.message);
    console.error(error);
  }
})();



