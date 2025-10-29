import { sql } from '../lib/db/client';

async function checkUserMapping() {
  console.log('=== Checking RepCard User ID Mapping ===\n');

  // 1. Check total users with repcard_user_id
  const usersWithRepcard = await sql`
    SELECT COUNT(*) as count
    FROM users
    WHERE repcard_user_id IS NOT NULL
  `;
  console.log(`Users with repcard_user_id: ${Array.from(usersWithRepcard)[0].count}`);

  // 2. Check sample repcard_user_ids from users table
  const sampleUsers = await sql`
    SELECT id, name, repcard_user_id
    FROM users
    WHERE repcard_user_id IS NOT NULL
    LIMIT 5
  `;
  console.log('\nSample users with RepCard IDs:');
  Array.from(sampleUsers).forEach((user: any) => {
    console.log(`  - ${user.name}: repcard_user_id = ${user.repcard_user_id}`);
  });

  // 3. Check repcard_customers table
  const totalCustomers = await sql`
    SELECT COUNT(*) as count
    FROM repcard_customers
  `;
  console.log(`\nTotal customers in repcard_customers: ${Array.from(totalCustomers)[0].count}`);

  // 4. Check sample setter_user_ids from repcard_customers
  const sampleCustomers = await sql`
    SELECT DISTINCT setter_user_id
    FROM repcard_customers
    WHERE setter_user_id IS NOT NULL
    LIMIT 10
  `;
  console.log('\nSample setter_user_ids from repcard_customers:');
  Array.from(sampleCustomers).forEach((customer: any) => {
    console.log(`  - ${customer.setter_user_id}`);
  });

  // 5. Check how many customers have setter_user_ids that match users
  const matchingCustomers = await sql`
    SELECT COUNT(DISTINCT rc.id) as count
    FROM repcard_customers rc
    INNER JOIN users u ON u.repcard_user_id::text = rc.setter_user_id::text
  `;
  console.log(`\nCustomers with matching setter_user_id: ${Array.from(matchingCustomers)[0].count}`);

  // 6. Check sample customers by specific user IDs
  const userIds = Array.from(sampleUsers).map((u: any) => u.repcard_user_id);
  if (userIds.length > 0) {
    const customersByUser = await sql`
      SELECT setter_user_id, COUNT(*) as count
      FROM repcard_customers
      WHERE setter_user_id = ANY(${userIds}::text[])
      GROUP BY setter_user_id
    `;
    console.log('\nCustomers per sample user:');
    Array.from(customersByUser).forEach((row: any) => {
      console.log(`  - User ID ${row.setter_user_id}: ${row.count} customers`);
    });
  }

  console.log('\n=== End of Report ===');
  process.exit(0);
}

checkUserMapping().catch(error => {
  console.error('Error:', error);
  process.exit(1);
});
