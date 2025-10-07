#!/usr/bin/env node

/**
 * Fetch Addison's Projects
 *
 * Fetches projects specifically for Addison Richards (closer IDs: 129, 186, 326)
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

async function fetchAddisonProjects() {
  const QB_REALM = process.env.QUICKBASE_REALM;
  const QB_TOKEN = process.env.QUICKBASE_TOKEN;
  const QB_TABLE_ID = process.env.QUICKBASE_TABLE_PROJECTS;

  if (!QB_REALM || !QB_TOKEN || !QB_TABLE_ID) {
    log('❌ Missing Quickbase credentials in .env.local', 'red');
    process.exit(1);
  }

  // Query for projects where closer ID is 129, 186, or 326
  const query = {
    from: QB_TABLE_ID,
    select: [3, 6, 7, 516, 517], // Record ID, Project ID, Customer Name, Closer ID, Closer Name
    where: "{516.EX.'118'}OR{516.EX.'129'}OR{516.EX.'186'}OR{516.EX.'326'}OR{516.EX.'452'}OR{516.EX.'508'}OR{516.EX.'650'}OR{516.EX.'711'}OR{516.EX.'870'}OR{516.EX.'882'}OR{516.EX.'910'}",
    sortBy: [{ fieldId: 3, order: 'DESC' }]
    // No top limit - fetch all
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
      res.on('data', (chunk) => { data += chunk; });
      res.on('end', () => {
        if (res.statusCode === 200) {
          resolve(JSON.parse(data));
        } else {
          reject(new Error(`Quickbase API error: ${res.statusCode} - ${data}`));
        }
      });
    });

    req.on('error', (error) => reject(error));
    req.write(postData);
    req.end();
  });
}

async function run() {
  log('\n' + '='.repeat(50), 'blue');
  log('📋 FETCH ADDISON\'S PROJECTS', 'bold');
  log('='.repeat(50), 'blue');

  log('\n🔍 Querying projects for closer IDs: 129, 186, 326...', 'blue');

  try {
    const response = await fetchAddisonProjects();
    const projects = response.data || [];

    log(`✅ Found ${projects.length} projects for Addison Richards`, 'green');

    if (projects.length > 0) {
      log('\n📊 Projects:', 'blue');
      projects.forEach((project, index) => {
        const recordId = project[3]?.value;
        const projectId = project[6]?.value;
        const customerName = project[7]?.value;
        const closerId = project[516]?.value;
        const closerName = project[517]?.value;

        log(`   ${index + 1}. [${recordId}] ${customerName} - Closer ID: ${closerId} (${closerName})`, 'blue');
      });
    } else {
      log('\n⚠️  No projects found for Addison', 'yellow');
      log('   This means Addison (IDs: 129, 186, 326) has no active projects in Quickbase', 'yellow');
    }

    log('\n' + '='.repeat(50) + '\n', 'blue');
  } catch (error) {
    log('\n💥 Failed to fetch projects:', 'red');
    log(`   ${error.message}`, 'red');
    process.exit(1);
  }
}

run();
