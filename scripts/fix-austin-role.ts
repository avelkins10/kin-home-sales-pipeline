import { sql } from '../lib/db/client';

async function fixAustinRole() {
  console.log('=== Checking Austin\'s User Role ===\n');

  // Find Austin's user
  const users = await sql`
    SELECT id, name, email, role
    FROM users
    WHERE email LIKE '%austin%' OR name LIKE '%Austin%'
  `;

  console.log('Found users:');
  Array.from(users).forEach((user: any) => {
    console.log(`  - ${user.name} (${user.email}): ${user.role}`);
  });

  // Update to super_admin
  const austinUser = Array.from(users).find((u: any) =>
    u.email?.toLowerCase().includes('austin')
  );

  if (austinUser) {
    console.log(`\nUpdating ${austinUser.name} to super_admin...`);

    await sql`
      UPDATE users
      SET role = 'super_admin',
          name = 'Austin Admin'
      WHERE id = ${austinUser.id}
    `;

    console.log('✓ Role updated to super_admin');
    console.log('✓ Name updated to Austin Admin');
  } else {
    console.log('No user found for Austin');
  }

  console.log('\n=== Done ===');
  process.exit(0);
}

fixAustinRole().catch(error => {
  console.error('Error:', error);
  process.exit(1);
});
