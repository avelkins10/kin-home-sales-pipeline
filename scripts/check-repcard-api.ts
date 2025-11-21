/**
 * Check RepCard API to see total users available
 */

import { repcardClient } from '@/lib/repcard/client';
import { config } from 'dotenv';
import { resolve } from 'path';

// Load environment variables
config({ path: resolve(process.cwd(), '.env.local') });
config({ path: resolve(process.cwd(), '.env') });

async function main() {
  console.log('🔍 Checking RepCard API\n');
  console.log('='.repeat(70));

  try {
    // Fetch first page
    console.log('Fetching page 1...');
    const response1 = await repcardClient.getUsersMinimal({ page: 1, perPage: 100 });

    console.log('\n📊 API Response Structure:');
    console.log('  Response keys:', Object.keys(response1 || {}));
    console.log('  Result keys:', Object.keys(response1?.result || {}));

    if (response1?.result) {
      const result = response1.result as any;
      console.log('\n📄 Pagination Info:');
      console.log('  Current Page:', result.currentPage || result.current_page || 'N/A');
      console.log('  Last Page:', result.lastPage || result.last_page || 'N/A');
      console.log('  Total Records:', result.total || 'N/A');
      console.log('  Per Page:', result.perPage || result.per_page || 'N/A');

      const users = Array.isArray(result.data) ? result.data : [];
      console.log('  Users on page 1:', users.length);

      // Sample first 3 users
      console.log('\n👤 Sample Users:');
      users.slice(0, 3).forEach((user: any) => {
        console.log(`  - ID: ${user.id}, Name: ${user.firstName} ${user.lastName}, Status: ${user.status}`);
      });

      // Check if there are more pages
      const currentPage = result.currentPage || result.current_page || 1;
      const lastPage = result.lastPage || result.last_page || 1;
      const hasMore = currentPage < lastPage;

      console.log('\n🔄 Pagination Status:');
      console.log('  Has more pages?:', hasMore);
      console.log('  Would fetch page 2?:', hasMore ? 'YES' : 'NO');

      if (hasMore) {
        console.log('\nFetching page 2 to verify pagination works...');
        const response2 = await repcardClient.getUsersMinimal({ page: 2, perPage: 100 });
        const users2 = Array.isArray(response2?.result?.data) ? response2.result.data : [];
        console.log('  Users on page 2:', users2.length);
      }
    }

  } catch (error) {
    console.error('❌ Error querying RepCard API:', error);
    if (error instanceof Error) {
      console.error('Error message:', error.message);
      console.error('Stack:', error.stack);
    }
    process.exit(1);
  }
}

main()
  .then(() => process.exit(0))
  .catch((error) => {
    console.error('Fatal error:', error);
    process.exit(1);
  });
