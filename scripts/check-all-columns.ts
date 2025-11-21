import { sql } from '@/lib/db/client';
import { config } from 'dotenv';
import { resolve } from 'path';

config({ path: resolve(process.cwd(), '.env.local') });
config({ path: resolve(process.cwd(), '.env') });
if (process.env.DATABASE_URL && !process.env.POSTGRES_URL) {
  process.env.POSTGRES_URL = process.env.DATABASE_URL;
}

async function main() {
  console.log('Checking all columns in RepCard tables...\n');

  const tables = ['repcard_customers', 'repcard_appointments', 'repcard_offices'];

  for (const table of tables) {
    try {
      const columns = await sql`
        SELECT column_name, data_type
        FROM information_schema.columns
        WHERE table_schema = 'public'
          AND table_name = ${table}
        ORDER BY ordinal_position
      `;
      const cols = Array.from(columns);
      console.log(`\n${table}: (${cols.length} columns)`);
      cols.forEach((col: any) => {
        if (col.column_name.includes('office')) {
          console.log(`  ⭐ ${col.column_name}: ${col.data_type}`);
        } else {
          console.log(`     ${col.column_name}: ${col.data_type}`);
        }
      });
    } catch (error: any) {
      console.error(`Error checking ${table}:`, error.message);
    }
  }
}

main().then(() => process.exit(0)).catch((e) => { console.error(e); process.exit(1); });
