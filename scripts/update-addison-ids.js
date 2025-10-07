#!/usr/bin/env node

const path = require('path');
require('dotenv').config({ path: path.join(process.cwd(), '.env.local') });
const { sql } = require('@vercel/postgres');

async function updateAddison() {
  try {
    console.log('🔄 Updating Addison Richards with all Quickbase closer IDs...');

    const allIds = '118,129,186,326,452,508,650,711,870,882,910';

    await sql`
      UPDATE users
      SET quickbase_user_id = ${allIds}
      WHERE email = 'addison.r@kinhome.com'
    `;

    console.log('✅ Updated Addison Richards');
    console.log(`   New Quickbase IDs: ${allIds}`);

    // Verify
    const result = await sql`
      SELECT email, quickbase_user_id, name
      FROM users
      WHERE email = 'addison.r@kinhome.com'
    `;

    console.log('\n📊 Verified record:');
    console.log(result.rows[0]);

    process.exit(0);
  } catch (error) {
    console.error('❌ Update failed:', error.message);
    process.exit(1);
  }
}

updateAddison();
