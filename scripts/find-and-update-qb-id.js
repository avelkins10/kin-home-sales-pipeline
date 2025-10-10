const { sql } = require('@vercel/postgres');
const path = require('path');

require('dotenv').config({ path: path.join(__dirname, '../.env.local') });

async function findAndUpdateQBId(email, qbUserId) {
  try {
    console.log(`\n🔍 Looking up user: ${email}...\n`);

    // Find user
    const userResult = await sql.query(
      'SELECT id, name, email, quickbase_user_id FROM users WHERE email = $1',
      [email]
    );

    if (userResult.rows.length === 0) {
      console.error(`❌ User not found with email: ${email}`);
      process.exit(1);
    }

    const user = userResult.rows[0];
    console.log(`📋 Found user:`);
    console.log(`   Name: ${user.name}`);
    console.log(`   Email: ${user.email}`);
    console.log(`   Current QB ID: ${user.quickbase_user_id || '(not set)'}`);

    if (qbUserId) {
      console.log(`   New QB ID: ${qbUserId}\n`);

      // Update the QuickBase User ID
      const updateResult = await sql.query(
        'UPDATE users SET quickbase_user_id = $1, updated_at = NOW() WHERE id = $2 RETURNING *',
        [qbUserId, user.id]
      );

      if (updateResult.rows.length > 0) {
        console.log(`✅ Successfully updated QuickBase ID!`);
        console.log(`   User: ${updateResult.rows[0].name}`);
        console.log(`   QB ID: ${updateResult.rows[0].quickbase_user_id}`);
      }
    } else {
      console.log('\n💡 To update, run:');
      console.log(`   node scripts/find-and-update-qb-id.js "${email}" <qb_user_id>`);
    }

  } catch (error) {
    console.error('❌ Error:', error.message);
    throw error;
  }
}

const args = process.argv.slice(2);
if (args.length === 0) {
  console.log('Usage:');
  console.log('  Check user:   node scripts/find-and-update-qb-id.js <email>');
  console.log('  Update user:  node scripts/find-and-update-qb-id.js <email> <qb_user_id>');
  console.log('\nExample:');
  console.log('  node scripts/find-and-update-qb-id.js kash.b@kinhome.com 12345');
  process.exit(1);
}

const [email, qbUserId] = args;
findAndUpdateQBId(email, qbUserId);
