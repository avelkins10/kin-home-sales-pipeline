/**
 * Check Detailed Sync Information
 */

import { sql } from '@/lib/db/client';

async function main() {
  const getRows = (result: any) => result.rows || (Array.isArray(result) ? result : []);

  // Get latest users sync with full details
  const usersSync = getRows(await sql`
    SELECT * FROM repcard_sync_log 
    WHERE entity_type = 'users' 
    ORDER BY started_at DESC 
    LIMIT 1
  `)[0];

  console.log('Latest Users Sync:');
  console.log(JSON.stringify(usersSync, null, 2));
  console.log('');

  // Get latest offices sync with full details
  const officesSync = getRows(await sql`
    SELECT * FROM repcard_sync_log 
    WHERE entity_type = 'offices' 
    ORDER BY started_at DESC 
    LIMIT 1
  `)[0];

  console.log('Latest Offices Sync:');
  console.log(JSON.stringify(officesSync, null, 2));
  console.log('');

  // Check if there are any users/offices in the database
  const userCount = getRows(await sql`SELECT COUNT(*) as count FROM repcard_users`)[0]?.count || 0;
  const officeCount = getRows(await sql`SELECT COUNT(*) as count FROM repcard_offices`)[0]?.count || 0;

  console.log(`Database counts: Users=${userCount}, Offices=${officeCount}`);
}

main().catch(console.error);

