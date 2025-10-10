const { sql } = require('@vercel/postgres');
const path = require('path');

require('dotenv').config({ path: path.join(__dirname, '../.env.local') });

async function testEmailFiltering() {
  try {
    // 1. Get Kash's user ID and email
    const userResult = await sql`
      SELECT id, email, role
      FROM users
      WHERE email = 'kash.b@kinhome.com'
    `;

    if (userResult.rows.length === 0) {
      console.log('❌ User not found');
      return;
    }

    const user = userResult.rows[0];
    console.log('\n📋 User Info:');
    console.log('  ID:', user.id);
    console.log('  Email:', user.email);
    console.log('  Role:', user.role);

    // 2. Test getUserEmail function simulation
    const emailResult = await sql`
      SELECT email
      FROM users
      WHERE id = ${user.id}
      AND email IS NOT NULL
    `;

    console.log('\n🔍 getUserEmail simulation:');
    if (emailResult.rows.length === 0) {
      console.log('  ❌ No email found (this would cause ALL projects to show!)');
    } else {
      console.log('  ✅ Email found:', emailResult.rows[0].email);
    }

    // 3. Show what the QuickBase query should be
    const email = emailResult.rows[0]?.email;
    if (email) {
      const sanitizedEmail = email.replace(/'/g, "''");
      const expectedQuery = `({518}.EX.'${sanitizedEmail}') OR ({331}.EX.'${sanitizedEmail}')`;
      console.log('\n📊 Expected QuickBase WHERE clause:');
      console.log('  ', expectedQuery);
      console.log('  Where 518 = CLOSER_EMAIL, 331 = SETTER_EMAIL');
    }

  } catch (error) {
    console.error('❌ Error:', error.message);
    console.error(error);
  }
}

testEmailFiltering();
