import { sql } from '@/lib/db/client';
import { config } from 'dotenv';
import { resolve } from 'path';

config({ path: resolve(process.cwd(), '.env.local') });
config({ path: resolve(process.cwd(), '.env') });
if (process.env.DATABASE_URL && !process.env.POSTGRES_URL) {
  process.env.POSTGRES_URL = process.env.DATABASE_URL;
}

async function main() {
  console.log('Checking office_id column types...\n');

  try {
    // Check repcard_customers.office_id type
    const customersType = await sql`
      SELECT data_type
      FROM information_schema.columns
      WHERE table_name = 'repcard_customers'
        AND column_name = 'office_id'
    `;
    console.log('repcard_customers.office_id type:', Array.from(customersType)[0]?.data_type);

    // Check repcard_appointments.office_id type
    const appointmentsType = await sql`
      SELECT data_type
      FROM information_schema.columns
      WHERE table_name = 'repcard_appointments'
        AND column_name = 'office_id'
    `;
    console.log('repcard_appointments.office_id type:', Array.from(appointmentsType)[0]?.data_type);

    // Check repcard_offices.repcard_office_id type
    const officesType = await sql`
      SELECT data_type
      FROM information_schema.columns
      WHERE table_name = 'repcard_offices'
        AND column_name = 'repcard_office_id'
    `;
    console.log('repcard_offices.repcard_office_id type:', Array.from(officesType)[0]?.data_type);

  } catch (error: any) {
    console.error('❌ Error:', error.message);
  }
}

main().then(() => process.exit(0)).catch((e) => { console.error(e); process.exit(1); });
