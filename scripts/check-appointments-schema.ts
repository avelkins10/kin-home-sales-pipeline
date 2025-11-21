import { sql } from '@/lib/db/client';
import { config } from 'dotenv';
import { resolve } from 'path';

config({ path: resolve(process.cwd(), '.env.local') });
config({ path: resolve(process.cwd(), '.env') });
if (process.env.DATABASE_URL && !process.env.POSTGRES_URL) {
  process.env.POSTGRES_URL = process.env.DATABASE_URL;
}

async function main() {
  const result = await sql`
    SELECT column_name, data_type
    FROM information_schema.columns
    WHERE table_name = 'repcard_appointments'
    ORDER BY ordinal_position
  `;
  const cols = Array.from(result);
  console.log('repcard_appointments columns:');
  cols.forEach((col: any) => console.log(`  ${col.column_name}: ${col.data_type}`));
}

main().then(() => process.exit(0)).catch((e) => { console.error(e); process.exit(1); });
