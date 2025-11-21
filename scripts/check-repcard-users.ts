/**
 * Check RepCard Users Table Status
 */

import { sql } from '@/lib/db/client';
import { config } from 'dotenv';
import { resolve } from 'path';

// Load environment variables
config({ path: resolve(process.cwd(), '.env.local') });
config({ path: resolve(process.cwd(), '.env') });

// Ensure DATABASE_URL is mapped to POSTGRES_URL for @vercel/postgres
if (process.env.DATABASE_URL && !process.env.POSTGRES_URL) {
  process.env.POSTGRES_URL = process.env.DATABASE_URL;
}

const getRows = (result: any) => result.rows || (Array.isArray(result) ? result : []);

async function main() {
  console.log('🔍 Checking RepCard Users Table\n');
  console.log('='.repeat(70));

  try {
    // Check total users and status breakdown
    const statusBreakdown = getRows(await sql`
      SELECT
        status,
        COUNT(*) as count
      FROM repcard_users
      GROUP BY status
      ORDER BY status
    `);

    console.log('📊 RepCard Users by Status:');
    statusBreakdown.forEach((row: any) => {
      console.log(`  Status ${row.status}: ${row.count} users`);
    });
    console.log('');

    // Check role breakdown for active users (status = 1)
    const roleBreakdown = getRows(await sql`
      SELECT
        role,
        COUNT(*) as count
      FROM repcard_users
      WHERE status = 1
      GROUP BY role
      ORDER BY role
    `);

    console.log('📊 Active RepCard Users (status=1) by Role:');
    roleBreakdown.forEach((row: any) => {
      console.log(`  ${row.role || 'null'}: ${row.count} users`);
    });
    console.log('');

    // Check users with emails
    const emailStats = getRows(await sql`
      SELECT
        COUNT(*) as total,
        COUNT(CASE WHEN email IS NOT NULL AND email != '' THEN 1 END) as with_email,
        COUNT(CASE WHEN email IS NULL OR email = '' THEN 1 END) as without_email
      FROM repcard_users
      WHERE status = 1
    `)[0];

    console.log('📧 Email Statistics (active users):');
    console.log(`  Total active users: ${emailStats?.total || 0}`);
    console.log(`  With email: ${emailStats?.with_email || 0}`);
    console.log(`  Without email: ${emailStats?.without_email || 0}`);
    console.log('');

    // Check linking status
    const linkingStats = getRows(await sql`
      SELECT
        COUNT(*) as total_repcard_users,
        COUNT(CASE WHEN u.id IS NOT NULL THEN 1 END) as linked_to_app,
        COUNT(CASE WHEN u.id IS NULL THEN 1 END) as not_linked
      FROM repcard_users ru
      LEFT JOIN users u ON u.repcard_user_id = ru.repcard_user_id
      WHERE ru.status = 1
    `)[0];

    console.log('🔗 Linking Statistics (active RepCard users):');
    console.log(`  Total active RepCard users: ${linkingStats?.total_repcard_users || 0}`);
    console.log(`  Linked to app users: ${linkingStats?.linked_to_app || 0}`);
    console.log(`  Not linked: ${linkingStats?.not_linked || 0}`);
    console.log('');

    // Sample some users
    const sampleUsers = getRows(await sql`
      SELECT
        ru.repcard_user_id,
        ru.first_name,
        ru.last_name,
        ru.email,
        ru.role,
        ru.status,
        ru.office_name,
        u.id as app_user_id,
        u.name as app_user_name
      FROM repcard_users ru
      LEFT JOIN users u ON u.repcard_user_id = ru.repcard_user_id
      WHERE ru.status = 1
      ORDER BY ru.repcard_user_id
      LIMIT 10
    `);

    console.log('👤 Sample Active Users:');
    sampleUsers.forEach((user: any) => {
      console.log(`  RepCard ID: ${user.repcard_user_id}`);
      console.log(`    Name: ${user.first_name} ${user.last_name}`);
      console.log(`    Email: ${user.email || 'N/A'}`);
      console.log(`    Role: ${user.role || 'N/A'}`);
      console.log(`    Office: ${user.office_name || 'N/A'}`);
      console.log(`    Linked to app user: ${user.app_user_id ? `Yes (${user.app_user_name})` : 'No'}`);
      console.log('');
    });

  } catch (error) {
    console.error('❌ Error querying database:', error);
    if (error instanceof Error) {
      console.error('Error message:', error.message);
      console.error('Stack:', error.stack);
    }
    process.exit(1);
  }
}

main()
  .then(() => process.exit(0))
  .catch((error) => {
    console.error('Fatal error:', error);
    process.exit(1);
  });
