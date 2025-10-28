/**
 * Script to fix admin@kinhome.com account
 * Restores name to "Austin Admin" and ensures role is "super_admin"
 */

import * as dotenv from 'dotenv';
import * as path from 'path';
import { sql } from '@vercel/postgres';

// Load environment variables from .env.local
dotenv.config({ path: path.resolve(process.cwd(), '.env.local') });

async function fixAdminAccount() {
  console.log('🔧 Starting admin account fix...\n');

  try {
    // Check current state
    console.log('📊 Current admin account state:');
    const currentState = await sql`
      SELECT id, email, name, role, sales_office, last_synced_from_contacts_at
      FROM users
      WHERE email = 'admin@kinhome.com'
    `;

    if (currentState.rows.length === 0) {
      console.error('❌ Admin account not found!');
      process.exit(1);
    }

    console.log(JSON.stringify(currentState.rows[0], null, 2));
    console.log('');

    // Update the account
    console.log('🔄 Updating admin account...');
    const result = await sql`
      UPDATE users
      SET name = 'Austin Admin',
          role = 'super_admin'
      WHERE email = 'admin@kinhome.com'
      RETURNING id, email, name, role
    `;

    console.log('✅ Admin account updated successfully!');
    console.log('\n📊 New admin account state:');
    console.log(JSON.stringify(result.rows[0], null, 2));

    // Verify the update
    const verifyState = await sql`
      SELECT id, email, name, role, sales_office, last_synced_from_contacts_at
      FROM users
      WHERE email = 'admin@kinhome.com'
    `;

    const admin = verifyState.rows[0];
    if (admin.name === 'Austin Admin' && admin.role === 'super_admin') {
      console.log('\n✅ Verification successful!');
      console.log('   - Name: Austin Admin ✓');
      console.log('   - Role: super_admin ✓');
    } else {
      console.warn('\n⚠️  Verification warning:');
      if (admin.name !== 'Austin Admin') {
        console.warn(`   - Name is "${admin.name}" (expected "Austin Admin")`);
      }
      if (admin.role !== 'super_admin') {
        console.warn(`   - Role is "${admin.role}" (expected "super_admin")`);
      }
    }

    console.log('\n✨ Admin account fix complete!');
    console.log('\n📝 Next steps:');
    console.log('   1. Log out of the application');
    console.log('   2. Log back in');
    console.log('   3. Verify name shows "Austin Admin" in navbar');
    console.log('   4. Verify Projects/Reports/Analytics are accessible');

  } catch (error) {
    console.error('❌ Error fixing admin account:', error);
    throw error;
  }
}

// Run the script
fixAdminAccount()
  .then(() => {
    console.log('\n🎉 Script completed successfully');
    process.exit(0);
  })
  .catch((error) => {
    console.error('\n💥 Script failed:', error);
    process.exit(1);
  });
