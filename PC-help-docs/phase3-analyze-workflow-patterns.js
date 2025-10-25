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

async function analyzeOutreachRecords() {
  console.log('='.repeat(80));
  console.log('PHASE 3: ANALYZING OUTREACH RECORDS STRUCTURE');
  console.log('='.repeat(80));
  console.log('\n');

  const tableId = 'btvik5kwi';

  console.log('Getting field schema for Outreach Records table...\n');

  const fieldsResponse = await qbRequest(`/v1/fields?tableId=${tableId}`, 'GET');

  if (fieldsResponse.statusCode !== 200) {
    console.log(`❌ Failed to get fields: ${fieldsResponse.statusCode}`);
    return null;
  }

  const fields = Array.isArray(fieldsResponse.data)
    ? fieldsResponse.data
    : (fieldsResponse.data.fields || []);

  console.log(`✅ Found ${fields.length} fields in Outreach Records table\n`);

  // Categorize fields
  const categories = {
    identifiers: [],
    dates: [],
    contact: [],
    outcome: [],
    notes: [],
    projectLink: [],
    other: []
  };

  fields.forEach(field => {
    const label = field.label.toLowerCase();

    if (label.includes('project') || label.includes('customer')) {
      categories.projectLink.push(field);
    } else if (label.includes('date') || label.includes('time') || field.fieldType === 'timestamp' || field.fieldType === 'date') {
      categories.dates.push(field);
    } else if (label.includes('method') || label.includes('phone') || label.includes('email') || label.includes('text') || label.includes('sms')) {
      categories.contact.push(field);
    } else if (label.includes('outcome') || label.includes('result') || label.includes('status') || label.includes('response')) {
      categories.outcome.push(field);
    } else if (label.includes('note') || label.includes('comment') || field.fieldType === 'text-multi-line') {
      categories.notes.push(field);
    } else if (field.id <= 10) {
      categories.identifiers.push(field);
    } else {
      categories.other.push(field);
    }
  });

  console.log('📋 OUTREACH RECORDS FIELD STRUCTURE:\n');

  console.log('Identifiers & System Fields:');
  categories.identifiers.forEach(f => {
    console.log(`  [${f.id}] ${f.label} (${f.fieldType})`);
  });

  console.log('\nProject Linking:');
  categories.projectLink.forEach(f => {
    console.log(`  [${f.id}] ${f.label} (${f.fieldType})`);
  });

  console.log('\nDate/Time Tracking:');
  categories.dates.forEach(f => {
    console.log(`  [${f.id}] ${f.label} (${f.fieldType})`);
  });

  console.log('\nContact Method:');
  categories.contact.forEach(f => {
    console.log(`  [${f.id}] ${f.label} (${f.fieldType})`);
    if (f.properties?.choices) {
      console.log(`     Values: ${f.properties.choices.join(', ')}`);
    }
  });

  console.log('\nOutcome Tracking:');
  categories.outcome.forEach(f => {
    console.log(`  [${f.id}] ${f.label} (${f.fieldType})`);
    if (f.properties?.choices) {
      console.log(`     Values: ${f.properties.choices.join(', ')}`);
    }
  });

  console.log('\nNotes:');
  categories.notes.forEach(f => {
    console.log(`  [${f.id}] ${f.label} (${f.fieldType})`);
  });

  console.log('\nOther Fields:');
  categories.other.slice(0, 20).forEach(f => {
    console.log(`  [${f.id}] ${f.label} (${f.fieldType})`);
  });
  if (categories.other.length > 20) {
    console.log(`  ... and ${categories.other.length - 20} more`);
  }

  // Get sample records
  console.log('\n\n📊 SAMPLE OUTREACH RECORDS:\n');

  const recordsResponse = await qbRequest('/v1/records/query', 'POST', {
    from: tableId,
    select: ['*'],
    sortBy: [{ fieldId: 3, order: 'DESC' }],
    options: { top: 10 }
  });

  if (recordsResponse.statusCode === 200 && recordsResponse.data.data) {
    const records = recordsResponse.data.data;
    console.log(`Retrieved ${records.length} recent outreach records\n`);

    records.slice(0, 5).forEach((record, idx) => {
      console.log(`\nOutreach Record ${idx + 1}:`);

      // Show key fields
      Object.keys(record)
        .filter(key => !isNaN(key))
        .slice(0, 15)
        .forEach(fieldId => {
          const field = fields.find(f => f.id === parseInt(fieldId));
          const value = record[fieldId]?.value;
          if (value !== null && value !== undefined && value !== '' && value !== false) {
            const displayValue = typeof value === 'object' ? JSON.stringify(value).substring(0, 80) : value;
            console.log(`  [${fieldId}] ${field?.label || 'Unknown'}: ${displayValue.toString().substring(0, 80)}`);
          }
        });
    });
  } else {
    console.log(`❌ Failed to get sample records: ${recordsResponse.statusCode}`);
  }

  return { fields, categories };
}

async function analyzeSalesAidRequests() {
  console.log('\n\n' + '='.repeat(80));
  console.log('ANALYZING SALES AID REQUEST STRUCTURE');
  console.log('='.repeat(80));
  console.log('\n');

  const tableId = 'bt3m39fgr';

  console.log('Getting field schema for Sales Aid Requests table...\n');

  const fieldsResponse = await qbRequest(`/v1/fields?tableId=${tableId}`, 'GET');

  if (fieldsResponse.statusCode !== 200) {
    console.log(`❌ Failed to get fields: ${fieldsResponse.statusCode}`);
    return null;
  }

  const fields = Array.isArray(fieldsResponse.data)
    ? fieldsResponse.data
    : (fieldsResponse.data.fields || []);

  console.log(`✅ Found ${fields.length} fields in Sales Aid Requests table\n`);

  // Key field categories
  console.log('🔍 KEY FIELDS FOR ESCALATION WORKFLOW:\n');

  const keyFields = {
    status: fields.filter(f => f.label.toLowerCase().includes('status')),
    priority: fields.filter(f => f.label.toLowerCase().includes('priority') || f.label.toLowerCase().includes('escalat')),
    reason: fields.filter(f => f.label.toLowerCase().includes('reason') || f.label.toLowerCase().includes('issue')),
    assignment: fields.filter(f => f.label.toLowerCase().includes('assign') || f.label.toLowerCase().includes('owner')),
    dates: fields.filter(f =>
      (f.label.toLowerCase().includes('date') || f.label.toLowerCase().includes('created') || f.label.toLowerCase().includes('closed'))
      && (f.fieldType === 'timestamp' || f.fieldType === 'date')
    ),
    project: fields.filter(f => f.label.toLowerCase().includes('project')),
    unresponsive: fields.filter(f => f.label.toLowerCase().includes('unresponsive') || f.label.toLowerCase().includes('responsive')),
  };

  console.log('Status Fields:');
  keyFields.status.forEach(f => {
    console.log(`  [${f.id}] ${f.label} (${f.fieldType})`);
    if (f.properties?.choices) {
      console.log(`     Values: ${f.properties.choices.join(', ')}`);
    }
  });

  console.log('\nPriority/Escalation Fields:');
  keyFields.priority.forEach(f => {
    console.log(`  [${f.id}] ${f.label} (${f.fieldType})`);
    if (f.properties?.choices) {
      console.log(`     Values: ${f.properties.choices.join(', ')}`);
    }
  });

  console.log('\nReason Fields:');
  keyFields.reason.forEach(f => {
    console.log(`  [${f.id}] ${f.label} (${f.fieldType})`);
    if (f.properties?.choices) {
      console.log(`     Values: ${f.properties.choices.join(', ')}`);
    }
  });

  console.log('\nAssignment Fields:');
  keyFields.assignment.forEach(f => {
    console.log(`  [${f.id}] ${f.label} (${f.fieldType})`);
  });

  console.log('\nDate Tracking:');
  keyFields.dates.forEach(f => {
    console.log(`  [${f.id}] ${f.label} (${f.fieldType})`);
  });

  console.log('\nProject Link:');
  keyFields.project.forEach(f => {
    console.log(`  [${f.id}] ${f.label} (${f.fieldType})`);
  });

  console.log('\nUnresponsive Tracking:');
  keyFields.unresponsive.forEach(f => {
    console.log(`  [${f.id}] ${f.label} (${f.fieldType})`);
  });

  // Get sample records
  console.log('\n\n📊 SAMPLE SALES AID REQUESTS:\n');

  const recordsResponse = await qbRequest('/v1/records/query', 'POST', {
    from: tableId,
    select: ['*'],
    sortBy: [{ fieldId: 3, order: 'DESC' }],
    options: { top: 10 }
  });

  if (recordsResponse.statusCode === 200 && recordsResponse.data.data) {
    const records = recordsResponse.data.data;
    console.log(`Retrieved ${records.length} recent sales aid requests\n`);

    // Analyze status distribution
    const statusCounts = {};
    const reasonCounts = {};

    records.forEach(record => {
      // Find status field
      const statusField = keyFields.status[0];
      if (statusField) {
        const status = record[statusField.id]?.value || 'Unknown';
        statusCounts[status] = (statusCounts[status] || 0) + 1;
      }

      // Find reason field
      const reasonField = keyFields.reason[0];
      if (reasonField) {
        const reason = record[reasonField.id]?.value;
        if (reason) {
          const reasonStr = Array.isArray(reason) ? reason.join(', ') : reason;
          reasonCounts[reasonStr] = (reasonCounts[reasonStr] || 0) + 1;
        }
      }
    });

    console.log('Status Distribution (sample):');
    Object.entries(statusCounts).forEach(([status, count]) => {
      console.log(`  ${status}: ${count}`);
    });

    if (Object.keys(reasonCounts).length > 0) {
      console.log('\nReason Distribution (sample):');
      Object.entries(reasonCounts)
        .sort(([, a], [, b]) => b - a)
        .forEach(([reason, count]) => {
          console.log(`  ${reason}: ${count}`);
        });
    }

    // Show sample records
    console.log('\n\nSample Records:\n');
    records.slice(0, 3).forEach((record, idx) => {
      console.log(`\nSales Aid Request ${idx + 1}:`);

      Object.keys(record)
        .filter(key => !isNaN(key))
        .slice(0, 15)
        .forEach(fieldId => {
          const field = fields.find(f => f.id === parseInt(fieldId));
          const value = record[fieldId]?.value;
          if (value !== null && value !== undefined && value !== '' && value !== false) {
            const displayValue = typeof value === 'object' ? JSON.stringify(value).substring(0, 60) : value;
            console.log(`  [${fieldId}] ${field?.label || 'Unknown'}: ${displayValue.toString().substring(0, 60)}`);
          }
        });
    });
  } else {
    console.log(`❌ Failed to get sample records: ${recordsResponse.statusCode}`);
  }

  return { fields, keyFields };
}

async function analyzeDashboardTriggers() {
  console.log('\n\n' + '='.repeat(80));
  console.log('ANALYZING DASHBOARD TRIGGER CONDITIONS');
  console.log('='.repeat(80));
  console.log('\n');

  // Get projects for Emma to analyze what makes them appear in each section
  console.log('Querying Emma Martin\'s projects with PC dashboard fields...\n');

  const dashboardFields = [
    3, 6, 255, 820, // ID, Customer, Status, PC Name
    2268, 2287, 2266, // # Due Milestone Outreach, Create Check-In, Incomplete Check-In
    2422, 1908, // # Escalated Sales Aid, # Active Unresponsive Sales Aid
    1909, 322, // Is Unresponsive, Project Escalation
    1731, 1732, 1733, 1734, // Last Contact Dates (by phase)
    1735, 1736, 1737, 1738, // Days Since Last Contact (by phase)
    1708, 1709, 1710, 1711, // Contact Attempts (by phase)
  ];

  const projectsResponse = await qbRequest('/v1/records/query', 'POST', {
    from: 'br9kwm8na',
    select: dashboardFields,
    where: '{820.EX."Emma Martin"}',
    sortBy: [{ fieldId: 255, order: 'ASC' }],
    options: { top: 150 }
  });

  if (projectsResponse.statusCode !== 200 || !projectsResponse.data.data) {
    console.log(`❌ Failed to get projects: ${projectsResponse.statusCode}`);
    return;
  }

  const projects = projectsResponse.data.data;
  console.log(`Analyzing ${projects.length} Emma Martin projects\n`);

  // Categorize projects by dashboard section
  const dashboardSections = {
    initialOutreach: [],
    unresponsive: [],
    escalation: [],
    multiple: []
  };

  projects.forEach(project => {
    const projectId = project['3']?.value;
    const customer = project['6']?.value;
    const status = project['255']?.value;

    const dueOutreach = project['2268']?.value || 0;
    const createCheckIn = project['2287']?.value;
    const incompleteCheckIn = project['2266']?.value;

    const escalatedSalesAid = project['2422']?.value || 0;
    const unresponsiveSalesAid = project['1908']?.value || 0;
    const isUnresponsive = project['1909']?.value;
    const projectEscalation = project['322']?.value;

    const sections = [];

    // Check Initial Outreach criteria
    if (dueOutreach > 0 || createCheckIn || incompleteCheckIn) {
      sections.push('initialOutreach');
      dashboardSections.initialOutreach.push({
        projectId, customer, status,
        dueOutreach, createCheckIn, incompleteCheckIn
      });
    }

    // Check Unresponsive criteria
    if (unresponsiveSalesAid > 0 || isUnresponsive) {
      sections.push('unresponsive');
      dashboardSections.unresponsive.push({
        projectId, customer, status,
        unresponsiveSalesAid, isUnresponsive
      });
    }

    // Check Escalation criteria
    if (escalatedSalesAid > 0 || ['High', 'ARC'].includes(projectEscalation)) {
      sections.push('escalation');
      dashboardSections.escalation.push({
        projectId, customer, status,
        escalatedSalesAid, projectEscalation
      });
    }

    if (sections.length > 1) {
      dashboardSections.multiple.push({ projectId, customer, status, sections });
    }
  });

  console.log('📊 DASHBOARD SECTION BREAKDOWN:\n');

  console.log(`Initial Outreach: ${dashboardSections.initialOutreach.length} projects`);
  if (dashboardSections.initialOutreach.length > 0) {
    console.log('  Sample projects:');
    dashboardSections.initialOutreach.slice(0, 5).forEach(p => {
      console.log(`    Project ${p.projectId} (${p.customer}) - Status: ${p.status}`);
      console.log(`      Due Outreach: ${p.dueOutreach}, Create Check-In: ${p.createCheckIn}, Incomplete: ${p.incompleteCheckIn}`);
    });
  }

  console.log(`\nUnresponsive Customers: ${dashboardSections.unresponsive.length} projects`);
  if (dashboardSections.unresponsive.length > 0) {
    console.log('  Sample projects:');
    dashboardSections.unresponsive.slice(0, 5).forEach(p => {
      console.log(`    Project ${p.projectId} (${p.customer}) - Status: ${p.status}`);
      console.log(`      Unresponsive Sales Aid: ${p.unresponsiveSalesAid}, Is Unresponsive: ${p.isUnresponsive}`);
    });
  }

  console.log(`\nPending Escalation: ${dashboardSections.escalation.length} projects`);
  if (dashboardSections.escalation.length > 0) {
    console.log('  Sample projects:');
    dashboardSections.escalation.slice(0, 5).forEach(p => {
      console.log(`    Project ${p.projectId} (${p.customer}) - Status: ${p.status}`);
      console.log(`      Escalated Sales Aid: ${p.escalatedSalesAid}, Project Escalation: ${p.projectEscalation}`);
    });
  }

  console.log(`\nProjects in Multiple Sections: ${dashboardSections.multiple.length}`);
  if (dashboardSections.multiple.length > 0) {
    dashboardSections.multiple.slice(0, 5).forEach(p => {
      console.log(`    Project ${p.projectId} appears in: ${p.sections.join(', ')}`);
    });
  }

  return dashboardSections;
}

async function analyzeOverdueCalculation() {
  console.log('\n\n' + '='.repeat(80));
  console.log('ANALYZING "OVERDUE" CALCULATION LOGIC');
  console.log('='.repeat(80));
  console.log('\n');

  console.log('Searching for "overdue", "grace", and "threshold" fields in Projects table...\n');

  const fieldsResponse = await qbRequest('/v1/fields?tableId=br9kwm8na', 'GET');

  if (fieldsResponse.statusCode !== 200) {
    console.log('❌ Failed to get fields');
    return;
  }

  const fields = Array.isArray(fieldsResponse.data)
    ? fieldsResponse.data
    : (fieldsResponse.data.fields || []);

  const relevantFields = fields.filter(f => {
    const label = f.label.toLowerCase();
    return label.includes('overdue') ||
           label.includes('grace') ||
           label.includes('threshold') ||
           label.includes('sla') ||
           label.includes('days since') ||
           label.includes('days until');
  });

  if (relevantFields.length > 0) {
    console.log(`✅ Found ${relevantFields.length} fields related to overdue/timing:\n`);
    relevantFields.forEach(f => {
      console.log(`  [${f.id}] ${f.label} (${f.fieldType})`);
      if (f.properties?.formula) {
        console.log(`     Formula: ${f.properties.formula.substring(0, 150)}...`);
      }
    });
  } else {
    console.log('⚠️  No "overdue" fields found in Projects table');
    console.log('   Likely calculated dynamically in dashboard or QB reports');
  }

  // Check if there are formula fields for days since last contact
  console.log('\n\nAnalyzing "Days Since Last Contact" calculations:\n');

  const daysSinceFields = fields.filter(f =>
    f.label.includes('Days Since') && f.fieldType === 'numeric'
  );

  daysSinceFields.slice(0, 10).forEach(f => {
    console.log(`  [${f.id}] ${f.label}`);
    if (f.properties?.formula) {
      console.log(`     Formula: ${f.properties.formula.substring(0, 200)}`);
    }
  });
}

async function main() {
  const outreachData = await analyzeOutreachRecords();
  const salesAidData = await analyzeSalesAidRequests();
  const dashboardData = await analyzeDashboardTriggers();
  await analyzeOverdueCalculation();

  console.log('\n\n' + '='.repeat(80));
  console.log('PHASE 3 COMPLETE');
  console.log('='.repeat(80));
  console.log('\n');
  console.log('✅ Analyzed Outreach Records structure');
  console.log('✅ Analyzed Sales Aid Requests structure');
  console.log('✅ Identified dashboard trigger conditions');
  console.log('✅ Investigated overdue calculation logic');
  console.log('\nNext: Phase 4 - Identify calculated vs stored fields');
}

main().catch(console.error);
