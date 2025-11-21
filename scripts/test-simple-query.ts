import { sql } from '@/lib/db/client';
import { config } from 'dotenv';
import { resolve } from 'path';

config({ path: resolve(process.cwd(), '.env.local') });
config({ path: resolve(process.cwd(), '.env') });
if (process.env.DATABASE_URL && !process.env.POSTGRES_URL) {
  process.env.POSTGRES_URL = process.env.DATABASE_URL;
}

async function main() {
  try {
    // Just select one office to see what columns it has
    const result = await sql`
      SELECT * FROM repcard_offices LIMIT 1
    `;
    const row = Array.from(result)[0];
    if (row) {
      console.log('Office columns:', Object.keys(row));
      console.log('Sample row:', row);
    }
  } catch (error: any) {
    console.error('Error:', error.message);
  }
}

main().then(() => process.exit(0)).catch((e) => { console.error(e); process.exit(1); });
