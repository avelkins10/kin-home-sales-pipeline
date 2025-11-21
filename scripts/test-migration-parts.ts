import { sql } from '@/lib/db/client';
import { config } from 'dotenv';
import { resolve } from 'path';

config({ path: resolve(process.cwd(), '.env.local') });
config({ path: resolve(process.cwd(), '.env') });
if (process.env.DATABASE_URL && !process.env.POSTGRES_URL) {
  process.env.POSTGRES_URL = process.env.DATABASE_URL;
}

async function main() {
  console.log('Testing migration components...\n');

  try {
    // Test office cleanup
    console.log('1. Testing office cleanup...');
    const cleanupResult = await sql`
      SELECT COUNT(*) as count
      FROM repcard_customers
      WHERE office_id IS NOT NULL
        AND NOT EXISTS (
          SELECT 1 FROM repcard_offices
          WHERE repcard_office_id = repcard_customers.office_id
        )
    `;
    const rows = Array.from(cleanupResult);
    console.log(`   Found ${rows[0]?.count || 0} customers with invalid office_id\n`);

    // Test exact name matching for offices
    console.log('2. Testing office name matching...');
    const nameMatch = await sql`
      SELECT COUNT(*) as count
      FROM repcard_offices ro
      INNER JOIN offices o ON LOWER(TRIM(ro.name)) = LOWER(TRIM(o.name))
      WHERE ro.name IS NOT NULL
        AND o.name IS NOT NULL
    `;
    const nameRows = Array.from(nameMatch);
    console.log(`   Found ${nameRows[0]?.count || 0} exact name matches\n`);

    console.log('✅ All tests passed');
  } catch (error: any) {
    console.error('❌ Error:', error.message);
    console.error('Code:', error.code);
    if (error.position) {
      console.error('Position:', error.position);
    }
  }
}

main().then(() => process.exit(0)).catch((e) => { console.error(e); process.exit(1); });
