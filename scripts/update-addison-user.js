#!/usr/bin/env node

/**
 * Update Addison's Quickbase User ID
 *
 * Updates the quickbase_user_id for addison.r@kinhome.com to include all their closer IDs
 */

const path = require('path');
require('dotenv').config({ path: path.join(process.cwd(), '.env.local') });
const { sql } = require('@vercel/postgres');

// Colors for console output
const colors = {
  green: '\x1b[32m',
  red: '\x1b[31m',
  yellow: '\x1b[33m',
  blue: '\x1b[34m',
  reset: '\x1b[0m',
  bold: '\x1b[1m'
};

function log(message, color = 'reset') {
  console.log(`${colors[color]}${message}${colors.reset}`);
}

async function updateAddisonUser() {
  log('\n' + '='.repeat(50), 'blue');
  log('🔄 UPDATE ADDISON USER', 'bold');
  log('='.repeat(50), 'blue');

  if (!process.env.DATABASE_URL) {
    log('❌ DATABASE_URL environment variable not set!', 'red');
    log('   Run: npm run setup:env', 'yellow');
    process.exit(1);
  }

  try {
    log('\n🔍 Updating addison.r@kinhome.com...', 'blue');

    // Update the quickbase_user_id to include all closer IDs
    const result = await sql`
      UPDATE users
      SET quickbase_user_id = '118,129,186,326,452,508,650,711,870,882,910'
      WHERE email = 'addison.r@kinhome.com'
    `;

    if (result.rowCount === 0) {
      log('⚠️  User not found', 'yellow');
      log('   Run: npm run setup:seed', 'yellow');
      process.exit(1);
    }

    log('✅ Updated Addison Richards', 'green');
    log('   Email: addison.r@kinhome.com', 'blue');
    log('   Quickbase IDs: 129, 186, 326', 'blue');

    log('\n' + '='.repeat(50), 'blue');
    log('🎉 Update complete!', 'green');
    log('\n💡 Next Steps:', 'yellow');
    log('1. Run the offline tests: npm run test:offline', 'yellow');
    log('2. Login as addison.r@kinhome.com to see projects', 'yellow');
    log('='.repeat(50) + '\n', 'blue');

    process.exit(0);
  } catch (error) {
    log('\n💥 Failed to update user:', 'red');
    log(`   ${error.message}`, 'red');
    process.exit(1);
  }
}

// Run update
updateAddisonUser();
