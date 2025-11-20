/**
 * Test RepCard API Calls
 * 
 * Tests the actual API calls to see what they return
 */

import { repcardClient } from '@/lib/repcard/client';

async function main() {
  console.log('🔍 Testing RepCard API calls...\n');

  try {
    // Test 1: Get Users Minimal
    console.log('Test 1: Getting users (minimal)...');
    try {
      const usersResponse = await repcardClient.getUsersMinimal({ page: 1, perPage: 10 });
      console.log('✅ Users API call succeeded');
      console.log('Response structure:', {
        hasResult: !!usersResponse.result,
        hasData: !!usersResponse.result?.data,
        dataIsArray: Array.isArray(usersResponse.result?.data),
        dataLength: Array.isArray(usersResponse.result?.data) ? usersResponse.result.data.length : 'N/A',
        resultKeys: usersResponse.result ? Object.keys(usersResponse.result) : [],
        firstUser: usersResponse.result?.data?.[0] || 'N/A'
      });
      console.log('');
    } catch (error) {
      console.error('❌ Users API call failed:', error);
      if (error instanceof Error) {
        console.error('Error message:', error.message);
      }
      console.log('');
    }

    // Test 2: Get Offices
    console.log('Test 2: Getting offices...');
    try {
      const officesResponse = await repcardClient.getOffices();
      console.log('✅ Offices API call succeeded');
      console.log('Response structure:', {
        hasResult: !!officesResponse.result,
        resultIsArray: Array.isArray(officesResponse.result),
        resultLength: Array.isArray(officesResponse.result) ? officesResponse.result.length : 'N/A',
        resultKeys: officesResponse.result && !Array.isArray(officesResponse.result) ? Object.keys(officesResponse.result) : [],
        firstOffice: Array.isArray(officesResponse.result) ? officesResponse.result[0] : officesResponse.result
      });
      console.log('');
    } catch (error) {
      console.error('❌ Offices API call failed:', error);
      if (error instanceof Error) {
        console.error('Error message:', error.message);
      }
      console.log('');
    }

    console.log('✅ API tests complete');

  } catch (error) {
    console.error('❌ Fatal error:', error);
    if (error instanceof Error) {
      console.error('Error message:', error.message);
      console.error('Error stack:', error.stack);
    }
    process.exit(1);
  }
}

main()
  .then(() => {
    console.log('\n✅ Test complete');
    process.exit(0);
  })
  .catch((error) => {
    console.error('❌ Fatal error:', error);
    process.exit(1);
  });

