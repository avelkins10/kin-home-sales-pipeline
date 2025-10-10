const { sql } = require('@vercel/postgres');
const path = require('path');

require('dotenv').config({ path: path.join(__dirname, '../.env.local') });

async function checkUser(searchTerm) {
  try {
    console.log(`\n🔍 Checking user: "${searchTerm}"...\n`);

    // Find user in dashboard
    const userResult = await sql.query(`
      SELECT id, name, email, quickbase_user_id, role, is_active, sales_office
      FROM users
      WHERE name ILIKE $1 OR email ILIKE $1
      LIMIT 5
    `, [`%${searchTerm}%`]);

    if (userResult.rows.length === 0) {
      console.log('❌ User not found in dashboard');
      return;
    }

    console.log('👤 Dashboard User Info:');
    userResult.rows.forEach((user, idx) => {
      console.log(`\n${idx + 1}. ${user.name}`);
      console.log(`   Email: ${user.email}`);
      console.log(`   Role: ${user.role}`);
      console.log(`   QuickBase User ID: ${user.quickbase_user_id || '❌ NOT SET'}`);
      console.log(`   Active: ${user.is_active ? '✅' : '❌'}`);
      console.log(`   Offices: ${user.sales_office ? user.sales_office.join(', ') : '(none)'}`);

      // Check projects if QB ID is set
      if (user.quickbase_user_id) {
        (async () => {
          const projectsResult = await sql.query(`
            SELECT COUNT(*) as count,
                   COUNT(*) FILTER (WHERE closer_id = $1) as closer_count,
                   COUNT(*) FILTER (WHERE setter_id = $1) as setter_count
            FROM quickbase_projects
            WHERE closer_id = $1 OR setter_id = $1
          `, [user.quickbase_user_id]);

          console.log(`\n   📊 Projects:`);
          console.log(`   - Total: ${projectsResult.rows[0].count}`);
          console.log(`   - As Closer: ${projectsResult.rows[0].closer_count}`);
          console.log(`   - As Setter: ${projectsResult.rows[0].setter_count}`);
        })();
      } else {
        console.log(`\n   ⚠️  Cannot see projects - QuickBase User ID not set!`);
        console.log(`   To fix: Use lookup-qb-user.js to find QB ID, then update-user-qb-id.js to set it`);
      }
    });

    // Also check QuickBase for this name
    console.log('\n\n🔍 Searching QuickBase for this user...');
    const qbResult = await sql.query(`
      SELECT DISTINCT
        COALESCE(closer_id, setter_id) as user_id,
        COALESCE(closer_name, setter_name) as name,
        COUNT(*) as project_count
      FROM quickbase_projects
      WHERE closer_name ILIKE $1 OR setter_name ILIKE $1
      GROUP BY COALESCE(closer_id, setter_id), COALESCE(closer_name, setter_name)
      LIMIT 5
    `, [`%${searchTerm}%`]);

    if (qbResult.rows.length > 0) {
      console.log('\n📋 Found in QuickBase Projects:');
      qbResult.rows.forEach((qbUser, idx) => {
        console.log(`\n${idx + 1}. ${qbUser.name}`);
        console.log(`   QB User ID: ${qbUser.user_id}`);
        console.log(`   Project Count: ${qbUser.project_count}`);
      });
    } else {
      console.log('   No projects found in QuickBase');
    }

  } catch (error) {
    console.error('❌ Error:', error.message);
    console.error(error.stack);
  }
}

const searchTerm = process.argv[2];
if (!searchTerm) {
  console.log('Usage: node scripts/check-user-projects.js "user name"');
  process.exit(1);
}

checkUser(searchTerm);
