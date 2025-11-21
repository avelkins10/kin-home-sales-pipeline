/**
 * Test RepCard API Pagination
 * Manually call the API to see pagination structure
 */

import { repcardClient } from '@/lib/repcard/client';
import { config } from 'dotenv';
import { resolve } from 'path';

config({ path: resolve(process.cwd(), '.env.local') });
config({ path: resolve(process.cwd(), '.env') });

async function main() {
  console.log('🔍 Testing RepCard API Pagination\n');
  console.log('='.repeat(70));

  try {
    // Test page 1
    console.log('\n📄 Fetching Page 1...');
    const page1 = await repcardClient.getUsersMinimal({ page: 1, perPage: 100 });

    console.log('\n📊 Response Structure:');
    console.log('  Response keys:', Object.keys(page1 || {}));
    console.log('  Result keys:', Object.keys(page1?.result || {}));

    const result = page1?.result as any;
    if (result) {
      console.log('\n📈 Pagination Details:');
      console.log('  currentPage:', result.currentPage || result.current_page);
      console.log('  lastPage:', result.lastPage || result.last_page);
      console.log('  total:', result.total);
      console.log('  perPage:', result.perPage || result.per_page);
      console.log('  from:', result.from);
      console.log('  to:', result.to);

      const users = Array.isArray(result.data) ? result.data : [];
      console.log('  users on page:', users.length);

      // Sample users
      console.log('\n👤 Sample Users (first 3):');
      users.slice(0, 3).forEach((user: any, idx: number) => {
        console.log(`  ${idx + 1}. ID: ${user.id}, Name: ${user.firstName} ${user.lastName}, Status: ${user.status}`);
      });

      // Check if there's a page 2
      const currentPage = result.currentPage || result.current_page || 1;
      const lastPage = result.lastPage || result.last_page || 1;
      const hasMore = currentPage < lastPage;

      console.log('\n🔄 Pagination Check:');
      console.log('  Current page:', currentPage);
      console.log('  Last page:', lastPage);
      console.log('  Has more pages:', hasMore);

      if (hasMore) {
        console.log('\n📄 Fetching Page 2 to verify...');
        const page2 = await repcardClient.getUsersMinimal({ page: 2, perPage: 100 });
        const users2 = Array.isArray(page2?.result?.data) ? page2.result.data : [];
        console.log('  Users on page 2:', users2.length);

        if (users2.length > 0) {
          console.log('  ✅ Page 2 has data - pagination works!');
        } else {
          console.log('  ⚠️  Page 2 is empty');
        }
      } else {
        console.log('  ℹ️  Only 1 page of users exists');
        console.log('  ℹ️  RepCard likely has exactly', result.total, 'users');
      }
    }

  } catch (error: any) {
    console.error('❌ Error:', error.message);
    if (error.response) {
      console.error('Response status:', error.response.status);
      console.error('Response data:', JSON.stringify(error.response.data, null, 2).substring(0, 500));
    }
  }

  console.log('\n' + '='.repeat(70));
}

main()
  .then(() => {
    console.log('\n✅ Test complete\n');
    process.exit(0);
  })
  .catch((error) => {
    console.error('\n❌ Fatal error:', error);
    process.exit(1);
  });
