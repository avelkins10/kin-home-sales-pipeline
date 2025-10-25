const https = require('https');

const QB_REALM = 'kin.quickbase.com';
const QB_USER_TOKEN = 'b6um6p_p3bs_0_did2y4mcxgmvm3d3k46nhdy9t68g';
const CONTACTS_TABLE_ID = 'br9kwm8td';

function qbRequest(path, method = 'GET', body = null) {
  return new Promise((resolve, reject) => {
    const options = {
      hostname: 'api.quickbase.com',
      path: path,
      method: method,
      headers: {
        'QB-Realm-Hostname': QB_REALM,
        'Authorization': `QB-USER-TOKEN ${QB_USER_TOKEN}`,
        'Content-Type': 'application/json'
      }
    };

    const req = https.request(options, (res) => {
      let data = '';
      res.on('data', (chunk) => { data += chunk; });
      res.on('end', () => {
        try {
          resolve({ statusCode: res.statusCode, data: JSON.parse(data) });
        } catch (e) {
          resolve({ statusCode: res.statusCode, data: data, error: e.message });
        }
      });
    });

    req.on('error', reject);
    if (body) req.write(JSON.stringify(body));
    req.end();
  });
}

async function findSalesReps() {
  console.log('='.repeat(80));
  console.log('TARGETED SEARCH FOR SALES REPS');
  console.log('='.repeat(80));
  console.log('\n');

  // Query 1: Find contacts with Sales Rep checkbox = true
  console.log('Query 1: Searching for contacts with "Sales Rep" checkbox...\n');

  const salesRepCheckboxQuery = await qbRequest('/v1/records/query', 'POST', {
    from: CONTACTS_TABLE_ID,
    select: [
      3, 6, 14, 15, 17, 18,  // ID, Full Name, First Name, Last Name, Email, Phone
      7, 37, 54,              // Status, Sales Rep checkbox, Contact Type
      235, 250, 251, 252, 253, 254, 255, 256, 261, // RepCard fields
      168, 32, 236,           // Enerflo fields
      113, 60,                // Office, Team
      102, 114, 140, 278,     // Role indicators (Setter, # Projects Closer/Setter)
      69, 70,                 // User fields
      243, 244                // Sequifi User ID
    ],
    where: '{37.EX.true}',  // Sales Rep checkbox = true
    sortBy: [{ fieldId: 6, order: 'ASC' }],
    options: { top: 50 }
  });

  if (salesRepCheckboxQuery.statusCode === 200) {
    const salesReps = salesRepCheckboxQuery.data.data || [];
    console.log(`✅ Found ${salesReps.length} contacts with "Sales Rep" checkbox = true\n`);

    if (salesReps.length > 0) {
      console.log('Sample Sales Reps:\n');
      salesReps.slice(0, 5).forEach((rep, idx) => {
        console.log(`${idx + 1}. ${rep[6]?.value || 'No name'} (ID: ${rep[3]?.value})`);
        console.log(`   Email: ${rep[17]?.value || 'N/A'}`);
        console.log(`   Phone: ${rep[18]?.value || 'N/A'}`);
        console.log(`   Status: ${rep[7]?.value || 'N/A'}`);
        console.log(`   Contact Type: ${rep[54]?.value || 'N/A'}`);
        console.log(`   RepCard ID: ${rep[235]?.value || 'Not populated'}`);
        console.log(`   Enerflo User ID: ${rep[168]?.value || 'Not populated'}`);
        console.log(`   Office: ${rep[113]?.value || 'N/A'}`);
        console.log(`   Team: ${rep[60]?.value || 'N/A'}`);
        console.log(`   Setter: ${rep[102]?.value ? 'Yes' : 'No'}`);
        console.log(`   # Projects (Closer): ${rep[114]?.value || 0}`);
        console.log(`   # Projects (Setter): ${rep[278]?.value || 0}`);
        console.log('');
      });

      // Analyze data quality
      let withRepcard = 0;
      let withEnerflo = 0;
      let withSequifi = 0;
      let withEmail = 0;
      let withPhone = 0;
      let activeCount = 0;

      salesReps.forEach(rep => {
        if (rep[235]?.value) withRepcard++;
        if (rep[168]?.value) withEnerflo++;
        if (rep[243]?.value) withSequifi++;
        if (rep[17]?.value) withEmail++;
        if (rep[18]?.value) withPhone++;
        if (rep[7]?.value === 'Active') activeCount++;
      });

      console.log('DATA QUALITY SUMMARY:');
      console.log(`  Total Sales Reps: ${salesReps.length}`);
      console.log(`  Active: ${activeCount} (${(activeCount/salesReps.length*100).toFixed(1)}%)`);
      console.log(`  With RepCard ID: ${withRepcard} (${(withRepcard/salesReps.length*100).toFixed(1)}%)`);
      console.log(`  With Enerflo User ID: ${withEnerflo} (${(withEnerflo/salesReps.length*100).toFixed(1)}%)`);
      console.log(`  With Sequifi User ID: ${withSequifi} (${(withSequifi/salesReps.length*100).toFixed(1)}%)`);
      console.log(`  With Email: ${withEmail} (${(withEmail/salesReps.length*100).toFixed(1)}%)`);
      console.log(`  With Phone: ${withPhone} (${(withPhone/salesReps.length*100).toFixed(1)}%)`);
    }
  } else {
    console.log(`❌ Query failed: ${salesRepCheckboxQuery.statusCode}`);
  }

  // Query 2: Find contacts with RepCard ID populated
  console.log('\n\n' + '='.repeat(80));
  console.log('Query 2: Searching for contacts with RepCard ID populated...\n');

  const repcardQuery = await qbRequest('/v1/records/query', 'POST', {
    from: CONTACTS_TABLE_ID,
    select: [
      3, 6, 14, 15, 17, 18,
      7, 37, 54,
      235, 250, 251, 252, 253, 254, 255, 256, 261,
      168, 32, 236,
      113, 60,
      102, 114, 140, 278,
      69, 70,
      243, 244
    ],
    where: '{235.XEX}',  // RepCard ID is not empty
    sortBy: [{ fieldId: 6, order: 'ASC' }],
    options: { top: 50 }
  });

  if (repcardQuery.statusCode === 200) {
    const repsWithRepcard = repcardQuery.data.data || [];
    console.log(`✅ Found ${repsWithRepcard.length} contacts with RepCard ID populated\n`);

    if (repsWithRepcard.length > 0) {
      console.log('Sample Records with RepCard ID:\n');
      repsWithRepcard.slice(0, 5).forEach((rep, idx) => {
        console.log(`${idx + 1}. ${rep[6]?.value || 'No name'} (ID: ${rep[3]?.value})`);
        console.log(`   Email: ${rep[17]?.value || 'N/A'}`);
        console.log(`   Status: ${rep[7]?.value || 'N/A'}`);
        console.log(`   RepCard ID: ${rep[235]?.value}`);
        console.log(`   RepCard First Name: ${rep[250]?.value || 'N/A'}`);
        console.log(`   RepCard Last Name: ${rep[251]?.value || 'N/A'}`);
        console.log(`   RepCard Office ID: ${rep[252]?.value || 'N/A'}`);
        console.log(`   RepCard Office: ${rep[253]?.value || 'N/A'}`);
        console.log(`   RepCard Team ID: ${rep[254]?.value || 'N/A'}`);
        console.log(`   RepCard Team: ${rep[255]?.value || 'N/A'}`);
        console.log(`   Is Rookie: ${rep[256]?.value ? 'Yes' : 'No'}`);
        console.log(`   Enerflo User ID: ${rep[168]?.value || 'N/A'}`);
        console.log(`   Sequifi User ID: ${rep[243]?.value || 'N/A'}`);
        console.log('');
      });
    }
  } else {
    console.log(`❌ Query failed: ${repcardQuery.statusCode}`);
  }

  // Query 3: Find contacts with Enerflo User ID (internal staff)
  console.log('\n' + '='.repeat(80));
  console.log('Query 3: Searching for contacts with Enerflo User ID...\n');

  const enerfloQuery = await qbRequest('/v1/records/query', 'POST', {
    from: CONTACTS_TABLE_ID,
    select: [
      3, 6, 14, 15, 17, 18,
      7, 37, 54,
      235, 250, 251, 252, 253,
      168, 32, 236,
      113, 60,
      102, 114, 278
    ],
    where: '{168.XEX}',  // enerflo_user_id is not empty
    sortBy: [{ fieldId: 6, order: 'ASC' }],
    options: { top: 50 }
  });

  if (enerfloQuery.statusCode === 200) {
    const repsWithEnerflo = enerfloQuery.data.data || [];
    console.log(`✅ Found ${repsWithEnerflo.length} contacts with Enerflo User ID\n`);

    if (repsWithEnerflo.length > 0) {
      // Check overlap with Sales Rep checkbox
      let alsoSalesRep = 0;
      let alsoRepcard = 0;

      repsWithEnerflo.forEach(rep => {
        if (rep[37]?.value === true) alsoSalesRep++;
        if (rep[235]?.value) alsoRepcard++;
      });

      console.log(`Overlap Analysis:`);
      console.log(`  Also have "Sales Rep" checkbox: ${alsoSalesRep} (${(alsoSalesRep/repsWithEnerflo.length*100).toFixed(1)}%)`);
      console.log(`  Also have RepCard ID: ${alsoRepcard} (${(alsoRepcard/repsWithEnerflo.length*100).toFixed(1)}%)`);

      console.log('\nSample Records with Enerflo User ID:\n');
      repsWithEnerflo.slice(0, 3).forEach((rep, idx) => {
        console.log(`${idx + 1}. ${rep[6]?.value || 'No name'} (ID: ${rep[3]?.value})`);
        console.log(`   Email: ${rep[17]?.value || 'N/A'}`);
        console.log(`   Status: ${rep[7]?.value || 'N/A'}`);
        console.log(`   Sales Rep checkbox: ${rep[37]?.value ? 'Yes' : 'No'}`);
        console.log(`   Contact Type: ${rep[54]?.value || 'N/A'}`);
        console.log(`   Enerflo User ID: ${rep[168]?.value}`);
        console.log(`   RepCard ID: ${rep[235]?.value || 'Not populated'}`);
        console.log(`   Office: ${rep[113]?.value || 'N/A'}`);
        console.log('');
      });
    }
  } else {
    console.log(`❌ Query failed: ${enerfloQuery.statusCode}`);
  }
}

async function generateSyncQuery() {
  console.log('\n\n' + '='.repeat(80));
  console.log('RECOMMENDED SYNC QUERY');
  console.log('='.repeat(80));
  console.log('\n');

  console.log('Based on the analysis, here\'s the recommended query for syncing sales reps:\n');

  console.log('```javascript');
  console.log('// Query all sales reps (contacts with Sales Rep checkbox = true)');
  console.log('const salesRepsResponse = await qbRequest(\'/v1/records/query\', \'POST\', {');
  console.log('  from: \'br9kwm8td\',');
  console.log('  select: [');
  console.log('    3,    // Record ID');
  console.log('    6,    // Full Name');
  console.log('    14,   // First Name');
  console.log('    15,   // Last Name');
  console.log('    17,   // Email');
  console.log('    18,   // Mobile Phone');
  console.log('    7,    // Status (Active/Inactive)');
  console.log('    37,   // Sales Rep checkbox');
  console.log('    54,   // Contact Type');
  console.log('    235,  // RepCard ID');
  console.log('    250,  // RepCard First Name');
  console.log('    251,  // RepCard Last Name');
  console.log('    252,  // RepCard Office ID');
  console.log('    253,  // RepCard Office');
  console.log('    254,  // RepCard Team ID');
  console.log('    255,  // RepCard Team');
  console.log('    256,  // Is Rookie');
  console.log('    261,  // RepCard Image URL');
  console.log('    168,  // Enerflo User ID');
  console.log('    32,   // Enerflo CX ID#');
  console.log('    236,  // Enerflo CX ID# (Text)');
  console.log('    113,  // Office Name');
  console.log('    60,   // Team Name');
  console.log('    102,  // Setter checkbox');
  console.log('    114,  // # of Projects (Closer)');
  console.log('    278,  // # of Projects (Setter)');
  console.log('    243,  // Sequifi User ID');
  console.log('    244,  // Sequifi User Record ID');
  console.log('    69,   // QuickBase User');
  console.log('    151,  // Dialpad User ID');
  console.log('    215,  // Zendesk User ID');
  console.log('  ],');
  console.log('  where: \'{37.EX.true}\',  // Sales Rep = true');
  console.log('  sortBy: [{ fieldId: 6, order: \'ASC\' }]  // Sort by Full Name');
  console.log('});');
  console.log('```');

  console.log('\n\nAlternative: Query by RepCard ID if you want only reps in RepCard system:\n');
  console.log('```javascript');
  console.log('// Query contacts with RepCard ID (reps in RepCard system)');
  console.log('const salesRepsResponse = await qbRequest(\'/v1/records/query\', \'POST\', {');
  console.log('  from: \'br9kwm8td\',');
  console.log('  select: [/* same fields as above */],');
  console.log('  where: \'{235.XEX}\',  // RepCard ID is not empty');
  console.log('  sortBy: [{ fieldId: 6, order: \'ASC\' }]');
  console.log('});');
  console.log('```');

  console.log('\n\nOR combine both criteria:\n');
  console.log('```javascript');
  console.log('// Query contacts that are Sales Reps OR have RepCard ID');
  console.log('const salesRepsResponse = await qbRequest(\'/v1/records/query\', \'POST\', {');
  console.log('  from: \'br9kwm8td\',');
  console.log('  select: [/* same fields as above */],');
  console.log('  where: \'{37.EX.true}OR{235.XEX}\',  // Sales Rep = true OR RepCard ID exists');
  console.log('  sortBy: [{ fieldId: 6, order: \'ASC\' }]');
  console.log('});');
  console.log('```');
}

async function main() {
  await findSalesReps();
  await generateSyncQuery();

  console.log('\n\n' + '='.repeat(80));
  console.log('SALES REP SEARCH COMPLETE');
  console.log('='.repeat(80));
  console.log('\n');
}

main().catch(console.error);
