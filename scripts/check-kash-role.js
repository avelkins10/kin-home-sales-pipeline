const { sql } = require('@vercel/postgres');
const path = require('path');

require('dotenv').config({ path: path.join(__dirname, '../.env.local') });

async function checkKash() {
  try {
    const result = await sql`
      SELECT id, name, email, role, quickbase_user_id, is_active, sales_office
      FROM users
      WHERE email = 'kash.b@kinhome.com'
    `;

    if (result.rows.length === 0) {
      console.log('❌ User not found');
      return;
    }

    const user = result.rows[0];
    console.log('\n👤 Kash Bitton User Info:');
    console.log('  ID:', user.id);
    console.log('  Name:', user.name);
    console.log('  Email:', user.email);
    console.log('  Role:', user.role);
    console.log('  QB User ID:', user.quickbase_user_id || '(not set)');
    console.log('  Active:', user.is_active);
    console.log('  Office:', user.sales_office || '(not set)');
    console.log('');

    // Check what buildRoleClause would return
    if (user.role === 'super_admin' || user.role === 'regional') {
      console.log('⚠️  ISSUE FOUND: User has admin role!');
      console.log('   Admin roles (super_admin, regional) see ALL projects');
      console.log('   This is why Kash sees every project');
    } else if (user.role === 'closer' || user.role === 'setter') {
      console.log('✅ Role is correct (closer/setter)');
      console.log('   Should filter by email: kash.b@kinhome.com');
      console.log('   If seeing all projects, there may be an issue with email filtering');
    }

  } catch (error) {
    console.error('❌ Error:', error.message);
  }
}

checkKash();
