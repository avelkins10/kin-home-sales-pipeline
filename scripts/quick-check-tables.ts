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
    const result = await sql`SELECT table_name FROM information_schema.tables WHERE table_schema = 'public' AND table_name LIKE 'repcard_%' ORDER BY table_name`;
    const tables = Array.from(result);
    console.log('Found', tables.length, 'RepCard tables:');
    tables.forEach((t: any) => console.log('  -', t.table_name));
  } catch (error) {
    console.error('Error:', error);
  }
})();




