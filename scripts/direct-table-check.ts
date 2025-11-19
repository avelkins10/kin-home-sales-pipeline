#!/usr/bin/env tsx
import { config } from 'dotenv';
import { resolve } from 'path';
config({ path: resolve(process.cwd(), '.env.local') });
if (process.env.DATABASE_URL && !process.env.POSTGRES_URL) {
  process.env.POSTGRES_URL = process.env.DATABASE_URL;
}
import { sql } from '@/lib/db/client';

(async () => {
  try {
    // Direct query for all tables
    const result = await (sql as any).query(`
      SELECT table_name 
      FROM information_schema.tables 
      WHERE table_schema = 'public' 
      AND table_name LIKE 'repcard_%'
      ORDER BY table_name;
    `);
    
    console.log('Query result type:', typeof result);
    console.log('Query result:', result);
    console.log('Rows:', result.rows);
    console.log('Row count:', result.rows?.length || 0);
    
    if (result.rows && result.rows.length > 0) {
      console.log('\n✅ Found RepCard tables:');
      result.rows.forEach((row: any) => {
        console.log('  -', row.table_name);
      });
    } else {
      console.log('\n❌ No RepCard tables found');
    }
  } catch (error: any) {
    console.error('Error:', error.message);
  }
})();

