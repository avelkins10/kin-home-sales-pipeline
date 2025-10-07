#!/usr/bin/env node

/**
 * QuickBase Connection Test
 * Tests if QuickBase API credentials are working
 */

const path = require('path');
require('dotenv').config({ path: path.join(process.cwd(), '.env.local') });

async function testQuickBase() {
  console.log('\n🔍 Testing QuickBase Connection...\n');

  // Check environment variables
  const realm = process.env.QUICKBASE_REALM;
  const token = process.env.QUICKBASE_TOKEN;
  const tableId = process.env.QUICKBASE_TABLE_PROJECTS;

  console.log('Environment Variables:');
  console.log('  QUICKBASE_REALM:', realm ? '✅ Set' : '❌ Missing');
  console.log('  QUICKBASE_TOKEN:', token ? '✅ Set' : '❌ Missing');
  console.log('  QUICKBASE_TABLE_PROJECTS:', tableId ? '✅ Set' : '❌ Missing');

  if (!realm || !token || !tableId) {
    console.log('\n❌ Missing required environment variables!');
    process.exit(1);
  }

  // Test API connection
  console.log('\n📡 Testing API connection...');
  console.log(`   URL: https://${realm}/api/v1/records/query`);
  console.log(`   Table: ${tableId}`);

  try {
    const response = await fetch(`https://api.quickbase.com/v1/records/query`, {
      method: 'POST',
      headers: {
        'QB-Realm-Hostname': realm,
        'Authorization': `QB-USER-TOKEN ${token}`,
        'Content-Type': 'application/json',
        'User-Agent': 'kin-sales-pipeline'
      },
      body: JSON.stringify({
        from: tableId,
        select: [3, 6, 7, 8], // Record ID and some basic fields
        options: {
          top: 5
        }
      })
    });

    const data = await response.json();

    if (response.ok) {
      console.log('✅ QuickBase connection successful!');
      console.log('   Response:', JSON.stringify(data, null, 2).substring(0, 200) + '...');
    } else {
      console.log('❌ QuickBase API error:');
      console.log('   Status:', response.status);
      console.log('   Response:', JSON.stringify(data, null, 2));
    }
  } catch (error) {
    console.log('❌ Connection failed:');
    console.log('   Error:', error.message);
  }

  console.log('\n');
}

testQuickBase();
