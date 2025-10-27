import dotenv from 'dotenv';
dotenv.config({ path: '.env.local' });

async function testToken() {
  const token = process.env.QUICKBASE_TOKEN;
  const realm = process.env.QUICKBASE_REALM;
  const tableId = process.env.QUICKBASE_TABLE_PROJECTS; // Use the one from .env

  console.log('\n🔑 Testing QuickBase Token...\n');
  console.log(`Realm: ${realm}`);
  console.log(`Table ID: ${tableId}`);
  console.log(`Token: ${token?.substring(0, 20)}...`);

  const response = await fetch(`https://api.quickbase.com/v1/records/query`, {
    method: 'POST',
    headers: {
      'QB-Realm-Hostname': realm!,
      'User-Agent': 'RepDashboard',
      'Authorization': `QB-USER-TOKEN ${token}`,
      'Content-Type': 'application/json'
    },
    body: JSON.stringify({
      from: tableId,
      select: [3, 11],
      where: '{3.GT.0}',
      options: { top: 1 }
    })
  });

  const data = await response.json();

  if (response.ok) {
    console.log('✅ SUCCESS! Token works!');
    console.log(`Records found: ${data.data?.length || 0}`);
    if (data.data && data.data.length > 0) {
      console.log(`Sample record:`, data.data[0]);
    }
  } else {
    console.log('❌ FAILED!');
    console.log(`Status: ${response.status}`);
    console.log(`Response:`, JSON.stringify(data, null, 2));
  }
}

testToken();
