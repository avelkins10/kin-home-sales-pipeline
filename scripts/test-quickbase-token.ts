/**
 * Test QuickBase token validity
 */

import dotenv from 'dotenv';
import { qbClient } from '../lib/quickbase/client';
import { QB_TABLE_PROJECTS, PROJECT_FIELDS } from '../lib/constants/fieldIds';

dotenv.config({ path: '.env.local' });

async function testQuickBaseToken() {
  console.log('\n🔑 Testing QuickBase Token...\n');

  try {
    // Simple query to test token
    const testQuery = {
      from: QB_TABLE_PROJECTS,
      select: [PROJECT_FIELDS.RECORD_ID, PROJECT_FIELDS.PROJECT_ID],
      where: `{${PROJECT_FIELDS.RECORD_ID}.GT.0}`,
      options: {
        top: 1
      }
    };

    console.log('Attempting to query QuickBase...');
    const response = await qbClient.queryRecords(testQuery);

    if (response.data && response.data.length > 0) {
      console.log('✅ SUCCESS! QuickBase token is valid!');
      console.log(`   Retrieved ${response.data.length} record(s)`);
      console.log(`   Sample Project ID: ${response.data[0][PROJECT_FIELDS.PROJECT_ID]}`);
    } else {
      console.log('⚠️  Query succeeded but returned no data');
    }

  } catch (error) {
    console.log('❌ FAILED! QuickBase token is invalid or expired');
    if (error instanceof Error) {
      console.log(`   Error: ${error.message}`);
    }
    process.exit(1);
  }
}

testQuickBaseToken();
