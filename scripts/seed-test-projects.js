#!/usr/bin/env node

/**
 * Test Project Seeding Script
 *
 * Pulls recent projects from Quickbase and caches them locally for testing.
 * This allows offline tests and development to work with realistic data.
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

async function fetchRecentProjects() {
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
    select: [3, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 516, 514, 515],
    where: "{20.GT.'0'}",
    sortBy: [{ fieldId: 1, order: 'DESC' }],
    options: {
      skip: 0,
      top: 20
    }
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

async function saveProjectsToFile(projects) {
  const fs = require('fs');
  const testDataDir = path.join(process.cwd(), 'tests', 'fixtures');

  // Create fixtures directory if it doesn't exist
  if (!fs.existsSync(testDataDir)) {
    fs.mkdirSync(testDataDir, { recursive: true });
  }

  const filePath = path.join(testDataDir, 'test-projects.json');
  fs.writeFileSync(filePath, JSON.stringify(projects, null, 2));

  return filePath;
}

async function seedTestProjects() {
  log('\n' + '='.repeat(50), 'blue');
  log('📦 TEST PROJECT SEEDING', 'bold');
  log('='.repeat(50), 'blue');

  log('\n🔍 Fetching recent projects from Quickbase...', 'blue');

  try {
    const response = await fetchRecentProjects();
    const projects = response.data;

    if (!projects || projects.length === 0) {
      log('⚠️  No projects found in Quickbase', 'yellow');
      process.exit(0);
    }

    log(`✅ Fetched ${projects.length} projects`, 'green');

    log('\n💾 Saving projects to test fixtures...', 'blue');
    const filePath = await saveProjectsToFile(projects);

    log(`✅ Saved to ${filePath}`, 'green');

    log('\n📊 Project Summary:', 'blue');
    projects.slice(0, 5).forEach((project, index) => {
      const recordId = project[3]?.value;
      const customerName = project[7]?.value || 'Unknown';
      const status = project[20]?.value || 'Unknown';
      log(`   ${index + 1}. [${recordId}] ${customerName} - ${status}`, 'blue');
    });

    if (projects.length > 5) {
      log(`   ... and ${projects.length - 5} more`, 'blue');
    }

    log('\n' + '='.repeat(50), 'blue');
    log('🎉 Test project seeding complete!', 'green');
    log('\n💡 These projects are now available for:', 'yellow');
    log('   - Integration tests', 'yellow');
    log('   - Manual testing', 'yellow');
    log('   - Offline development', 'yellow');
    log('='.repeat(50) + '\n', 'blue');

  } catch (error) {
    log('\n💥 Failed to seed test projects:', 'red');
    log(`   ${error.message}`, 'red');
    process.exit(1);
  }
}

// Run seeding
seedTestProjects();
