#!/usr/bin/env node

/**
 * Sync Closers from Quickbase
 *
 * Fetches all unique closers from Quickbase projects and displays their info
 * so we can create matching users in the app database.
 */

const path = require('path');
require('dotenv').config({ path: path.join(process.cwd(), '.env.local') });
const https = require('https');

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

async function fetchAllClosers() {
  const QB_REALM = process.env.QUICKBASE_REALM;
  const QB_TOKEN = process.env.QUICKBASE_TOKEN;
  const QB_TABLE_ID = process.env.QUICKBASE_TABLE_PROJECTS;

  if (!QB_REALM || !QB_TOKEN || !QB_TABLE_ID) {
    log('❌ Missing Quickbase credentials in .env.local', 'red');
    log('   Required: QUICKBASE_REALM, QUICKBASE_TOKEN, QUICKBASE_TABLE_PROJECTS', 'yellow');
    process.exit(1);
  }

  const query = {
    from: QB_TABLE_ID,
    select: [516, 517, 356], // CLOSER_ID, CLOSER_NAME, 356 (possibly closer email)
    sortBy: [{ fieldId: 516, order: 'ASC' }],
  };

  return new Promise((resolve, reject) => {
    const postData = JSON.stringify(query);

    const options = {
      hostname: 'api.quickbase.com',
      path: '/v1/records/query',
      method: 'POST',
      headers: {
        'QB-Realm-Hostname': QB_REALM,
        'Authorization': `QB-USER-TOKEN ${QB_TOKEN}`,
        'Content-Type': 'application/json',
        'Content-Length': Buffer.byteLength(postData)
      }
    };

    const req = https.request(options, (res) => {
      let data = '';

      res.on('data', (chunk) => {
        data += chunk;
      });

      res.on('end', () => {
        if (res.statusCode === 200) {
          resolve(JSON.parse(data));
        } else {
          reject(new Error(`Quickbase API error: ${res.statusCode} - ${data}`));
        }
      });
    });

    req.on('error', (error) => {
      reject(error);
    });

    req.write(postData);
    req.end();
  });
}

async function syncClosers() {
  log('\n' + '='.repeat(50), 'blue');
  log('👥 CLOSER SYNC FROM QUICKBASE', 'bold');
  log('='.repeat(50), 'blue');

  log('\n🔍 Fetching all closers from Quickbase projects...', 'blue');

  try {
    const response = await fetchAllClosers();
    const projects = response.data;

    if (!projects || projects.length === 0) {
      log('⚠️  No projects found in Quickbase', 'yellow');
      process.exit(0);
    }

    // Extract unique closers
    const closersMap = new Map();

    projects.forEach(project => {
      const closerId = project[516]?.value;
      const closerName = project[517]?.value;
      const closerEmail = project[356]?.value;

      if (closerId && !closersMap.has(closerId)) {
        closersMap.set(closerId, {
          name: closerName || 'Unknown',
          email: closerEmail || ''
        });
      }
    });

    const closers = Array.from(closersMap.entries()).map(([id, data]) => ({
      closerId: id,
      closerName: data.name,
      closerEmail: data.email
    }));

    log(`✅ Found ${closers.length} unique closers`, 'green');

    log('\n📋 Closers List:', 'blue');
    console.log(JSON.stringify(closers, null, 2));

    log('\n' + '='.repeat(50), 'blue');
    log('💡 Next Steps:', 'yellow');
    log('1. Identify which closer IDs map to which users', 'yellow');
    log('2. Update seed-users.js with the correct quickbase_user_id values', 'yellow');
    log('3. The quickbase_user_id should match the "Closer ID" shown above', 'yellow');
    log('='.repeat(50) + '\n', 'blue');

  } catch (error) {
    log('\n💥 Failed to fetch closers:', 'red');
    log(`   ${error.message}`, 'red');
    process.exit(1);
  }
}

// Run sync
syncClosers();
