import dotenv from 'dotenv';
import path from 'path';
dotenv.config({ path: path.join(process.cwd(), '.env.local') });

import { sql } from '@/lib/db/client';

async function test() {
  console.log('Testing office filtering in leaderboard...\n');

  // Get office IDs
  const offices = await sql`
    SELECT quickbase_office_id, name
    FROM offices
    LIMIT 10
  `;
  const officeArray = Array.isArray(offices) ? offices : offices.rows || [];
  console.log(`Sample offices:`);
  officeArray.forEach((o: any) => {
    console.log(`  - ${o.name}: quickbase_office_id = ${o.quickbase_office_id}`);
  });

  if (officeArray.length === 0) {
    console.log('\n❌ No offices found!');
    process.exit(1);
  }

  const testOfficeIds = officeArray.slice(0, 3).map((o: any) => o.quickbase_office_id);
  console.log(`\nTesting with office IDs: ${testOfficeIds.join(', ')}\n`);

  // Test the actual query from leaderboard API
  const role = 'all';
  const result = await sql.query(
    `SELECT DISTINCT u.id, u.name, u.email, u.repcard_user_id, u.sales_office[1] as office, u.role
     FROM users u
     JOIN offices o ON o.name = ANY(u.sales_office)
     WHERE u.repcard_user_id IS NOT NULL
       AND o.quickbase_office_id = ANY($1::int[])`,
    [testOfficeIds]
  );
  
  const users = result.rows || [];
  console.log(`Found ${users.length} users with repcard_user_id for these offices:`);
  users.forEach((u: any) => {
    console.log(`  - ${u.name} (${u.email}): repcard_user_id = ${u.repcard_user_id}`);
  });

  if (users.length === 0) {
    console.log('\n⚠️  No users found! This could be why leaderboards are empty.');
    console.log('\nChecking if users have office assignments...');
    
    const usersWithOffices = await sql`
      SELECT id, name, email, repcard_user_id, sales_office
      FROM users
      WHERE repcard_user_id IS NOT NULL
      LIMIT 5
    `;
    const usersCheck = Array.isArray(usersWithOffices) ? usersWithOffices : usersWithOffices.rows || [];
    console.log(`Users with repcard_user_id:`);
    usersCheck.forEach((u: any) => {
      console.log(`  - ${u.name}: sales_office = ${JSON.stringify(u.sales_office)}`);
    });
  }

  process.exit(0);
}

test().catch(console.error);
