/**
 * Check and update user role for Operations access
 */

import { sql } from '@vercel/postgres';
import dotenv from 'dotenv';

dotenv.config({ path: '.env.local' });

async function checkAndUpdateRole() {
  const userEmail = 'austin@kinhome.com';

  try {
    // Check current role
    const { rows } = await sql`
      SELECT id, email, name, role
      FROM users
      WHERE email = ${userEmail}
    `;

    if (rows.length === 0) {
      console.log(`❌ User not found: ${userEmail}`);
      return;
    }

    const user = rows[0];
    console.log('\n📋 Current User Info:');
    console.log(`   Email: ${user.email}`);
    console.log(`   Name: ${user.name}`);
    console.log(`   Role: ${user.role || 'NOT SET'}`);

    // Update to super_admin if needed
    if (user.role !== 'super_admin') {
      console.log('\n🔧 Updating role to super_admin...');

      await sql`
        UPDATE users
        SET role = 'super_admin'
        WHERE email = ${userEmail}
      `;

      console.log('✅ Role updated to super_admin');
    } else {
      console.log('\n✅ Role is already super_admin');
    }

  } catch (error) {
    console.error('❌ Error:', error);
  } finally {
    await sql.end();
  }
}

checkAndUpdateRole();
