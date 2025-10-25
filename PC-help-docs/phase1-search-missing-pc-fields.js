const https = require('https');

const QB_REALM = 'kin.quickbase.com';
const QB_USER_TOKEN = 'b6um6p_p3bs_0_did2y4mcxgmvm3d3k46nhdy9t68g';

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

const TABLES_TO_SEARCH = [
  { name: 'Projects', id: 'br9kwm8na' },
  { name: 'Install Communications', id: 'bsb6bqt3b' },
  { name: 'Task Groups', id: 'bu36gem4p' },
  { name: 'Tasks', id: 'bu36ggiht' },
  { name: 'Installations', id: 'bsbkmcgfs' },
  { name: 'Interconnections', id: 'bseufp79y' },
];

const SEARCH_KEYWORDS = {
  repcard: ['repcard', 'rep card', 'rep_card', 'rep id', 'rep_id', 'repid'],
  outreach: ['outreach', 'contact method', 'preferred contact', 'communication preference', 'touchpoint'],
  dueDate: ['due date', 'due_date', 'duedate', 'target date', 'deadline', 'scheduled date'],
  attempts: ['attempts', 'num attempts', 'attempt count', 'contact attempts', 'outreach attempts'],
  escalation: ['escalation', 'escalate', 'grace period', 'grace_period', 'sales aid', 'salesaid'],
  lastContact: ['last contact', 'last_contact', 'last attempt', 'last_attempt', 'previous contact'],
  overdue: ['overdue', 'over due', 'days overdue', 'days_overdue', 'past due'],
  responsive: ['responsive', 'unresponsive', 'no response', 'response status'],
};

async function searchFieldsInTable(tableName, tableId, searchTerms) {
  console.log(`\nSearching ${tableName} (${tableId})...`);

  const fieldsResponse = await qbRequest(`/v1/fields?tableId=${tableId}`, 'GET');

  if (fieldsResponse.statusCode !== 200) {
    console.log(`  ❌ Failed: ${fieldsResponse.statusCode}`);
    return [];
  }

  const fields = Array.isArray(fieldsResponse.data)
    ? fieldsResponse.data
    : (fieldsResponse.data.fields || []);

  const found = [];

  searchTerms.forEach(term => {
    const matches = fields.filter(f =>
      f.label.toLowerCase().includes(term.toLowerCase())
    );

    if (matches.length > 0) {
      found.push({ term, matches });
    }
  });

  return { tableName, tableId, found, totalFields: fields.length };
}

async function phase1SearchMissingFields() {
  console.log('='.repeat(80));
  console.log('PHASE 1: SEARCHING FOR MISSING PC FIELDS');
  console.log('='.repeat(80));
  console.log('\n');

  const allFindings = {};

  // Search each category
  for (const [category, terms] of Object.entries(SEARCH_KEYWORDS)) {
    console.log('\n' + '='.repeat(80));
    console.log(`SEARCHING FOR: ${category.toUpperCase()}`);
    console.log(`Keywords: ${terms.join(', ')}`);
    console.log('='.repeat(80));

    allFindings[category] = [];

    for (const table of TABLES_TO_SEARCH) {
      const result = await searchFieldsInTable(table.name, table.id, terms);

      if (result.found.length > 0) {
        console.log(`\n  ✅ ${table.name} - Found ${result.found.length} match(es):`);
        result.found.forEach(({ term, matches }) => {
          matches.forEach(m => {
            console.log(`     [${m.id}] ${m.label} (${m.fieldType})`);
            if (m.properties && m.properties.choices) {
              console.log(`         Values: ${m.properties.choices.slice(0, 5).join(', ')}${m.properties.choices.length > 5 ? '...' : ''}`);
            }

            allFindings[category].push({
              table: table.name,
              tableId: table.id,
              fieldId: m.id,
              fieldLabel: m.label,
              fieldType: m.fieldType,
              choices: m.properties?.choices || null
            });
          });
        });
      }

      await new Promise(resolve => setTimeout(resolve, 300));
    }

    if (allFindings[category].length === 0) {
      console.log(`\n  ❌ No fields found for ${category}`);
    }
  }

  // Summary
  console.log('\n\n' + '='.repeat(80));
  console.log('SUMMARY: MISSING FIELDS SEARCH RESULTS');
  console.log('='.repeat(80));
  console.log('\n');

  Object.entries(allFindings).forEach(([category, findings]) => {
    console.log(`\n${category.toUpperCase()} (${findings.length} fields found):`);
    if (findings.length > 0) {
      findings.forEach(f => {
        console.log(`  [${f.fieldId}] ${f.fieldLabel} (${f.table} - ${f.fieldType})`);
      });
    } else {
      console.log(`  ⚠️ NOT FOUND - May need to create or is calculated`);
    }
  });

  return allFindings;
}

async function getPCProjectData() {
  console.log('\n\n' + '='.repeat(80));
  console.log('GETTING ACTUAL PC PROJECT DATA');
  console.log('='.repeat(80));
  console.log('\n');

  // Get Emma's projects (she's one of the PCs)
  console.log('Querying Emma Martin\'s projects...\n');

  const emmaResponse = await qbRequest('/v1/records/query', 'POST', {
    from: 'br9kwm8na',
    select: [3, 6, 145, 255, 820], // ID, legacy customer, new customer, status, PC name
    where: '{820.EX."Emma Martin"}',
    options: { top: 150 } // Try to get all her projects
  });

  if (emmaResponse.statusCode === 200 && emmaResponse.data.data) {
    const projects = emmaResponse.data.data;
    console.log(`Emma Martin has ${projects.length} projects`);

    // Count by status
    const byStatus = {};
    projects.forEach(p => {
      const status = p['255']?.value || 'Unknown';
      byStatus[status] = (byStatus[status] || 0) + 1;
    });

    console.log('\nBreakdown by status:');
    Object.entries(byStatus)
      .sort(([,a], [,b]) => b - a)
      .forEach(([status, count]) => {
        console.log(`  ${status}: ${count}`);
      });
  }

  // Get Paige's projects
  console.log('\n\nQuerying Paige Elkins\'s projects...\n');

  const paigeResponse = await qbRequest('/v1/records/query', 'POST', {
    from: 'br9kwm8na',
    select: [3, 6, 145, 255, 820],
    where: '{820.EX."Paige Elkins"}',
    options: { top: 150 }
  });

  if (paigeResponse.statusCode === 200 && paigeResponse.data.data) {
    const projects = paigeResponse.data.data;
    console.log(`Paige Elkins has ${projects.length} projects`);

    const byStatus = {};
    projects.forEach(p => {
      const status = p['255']?.value || 'Unknown';
      byStatus[status] = (byStatus[status] || 0) + 1;
    });

    console.log('\nBreakdown by status:');
    Object.entries(byStatus)
      .sort(([,a], [,b]) => b - a)
      .forEach(([status, count]) => {
        console.log(`  ${status}: ${count}`);
      });
  }

  // Sample one project to see all fields
  console.log('\n\n' + '='.repeat(80));
  console.log('SAMPLE PROJECT - ALL POPULATED FIELDS');
  console.log('='.repeat(80));
  console.log('\n');

  const sampleResponse = await qbRequest('/v1/records/query', 'POST', {
    from: 'br9kwm8na',
    select: ['*'], // Get ALL fields
    where: '{820.EX."Emma Martin"}',
    options: { top: 1 }
  });

  if (sampleResponse.statusCode === 200 && sampleResponse.data.data && sampleResponse.data.data.length > 0) {
    const project = sampleResponse.data.data[0];
    console.log(`Project ${project['3']?.value} - ${project['145']?.value || project['6']?.value}`);
    console.log('\nAll populated fields:');

    // Get field definitions
    const fieldsResponse = await qbRequest('/v1/fields?tableId=br9kwm8na', 'GET');
    const fields = Array.isArray(fieldsResponse.data)
      ? fieldsResponse.data
      : (fieldsResponse.data.fields || []);

    const fieldMap = {};
    fields.forEach(f => {
      fieldMap[f.id] = f.label;
    });

    Object.keys(project)
      .filter(key => !isNaN(key)) // Only numeric field IDs
      .filter(key => project[key]?.value !== null && project[key]?.value !== '' && project[key]?.value !== false)
      .slice(0, 50) // Show first 50 populated fields
      .forEach(key => {
        const value = project[key].value;
        const displayValue = typeof value === 'object' ? JSON.stringify(value).substring(0, 100) : value;
        console.log(`  [${key}] ${fieldMap[key] || 'Unknown'}: ${displayValue.toString().substring(0, 100)}`);
      });
  }
}

async function main() {
  const findings = await phase1SearchMissingFields();
  await getPCProjectData();

  return findings;
}

main().catch(console.error);
