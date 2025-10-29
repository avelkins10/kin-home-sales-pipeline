import dotenv from 'dotenv';
import path from 'path';
dotenv.config({ path: path.join(process.cwd(), '.env.local') });

async function test() {
  console.log('Testing leaderboard API response...\n');

  // Test with different time ranges
  const timeRanges = ['month', 'last_30', 'last_90', 'last_12_months'];
  
  for (const timeRange of timeRanges) {
    console.log(`\nTesting timeRange: ${timeRange}`);
    
    try {
      const response = await fetch(`http://localhost:3000/api/repcard/leaderboard?metric=doors_knocked&timeRange=${timeRange}&role=all&limit=10`, {
        headers: {
          'Cookie': process.env.TEST_COOKIE || ''
        }
      });
      
      if (!response.ok) {
        console.log(`   ❌ Error ${response.status}: ${response.statusText}`);
        continue;
      }
      
      const data = await response.json();
      console.log(`   ✅ Success: ${data.leaderboard?.length || 0} entries`);
      
      if (data.leaderboard && data.leaderboard.length > 0) {
        console.log(`   Top 3 entries:`);
        data.leaderboard.slice(0, 3).forEach((entry: any, i: number) => {
          console.log(`     ${i + 1}. ${entry.userName}: ${entry.metricValue}`);
        });
      }
    } catch (error: any) {
      console.log(`   ❌ Error: ${error.message}`);
    }
  }
  
  process.exit(0);
}

test().catch(console.error);
