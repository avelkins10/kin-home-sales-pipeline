const https = require('https');

const QB_REALM = 'kin.quickbase.com';
const QB_USER_TOKEN = 'b6um6p_p3bs_0_did2y4mcxgmvm3d3k46nhdy9t68g';
const APP_ID = 'bqyx7fva3';

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

async function getAllAppTables() {
  console.log('='.repeat(80));
  console.log('PHASE 2: DISCOVERING ALL TABLES IN APP');
  console.log('='.repeat(80));
  console.log('\n');

  // Known tables from previous analysis
  const knownTables = [
    { id: 'br9kwm8na', name: 'Projects' },
    { id: 'bsb6bqt3b', name: 'Install Communications' },
    { id: 'bu36gem4p', name: 'Task Groups' },
    { id: 'bu36ggiht', name: 'Tasks' },
    { id: 'bsbkmcgfs', name: 'Installations' },
    { id: 'bseufp79y', name: 'Interconnections' },
    { id: 'bsc3v7tdg', name: 'Inspections' },
    { id: 'bscs3z866', name: 'Permits' },
    { id: 'bsc9kt8n5', name: 'PTO Record' },
    { id: 'bubi43m26', name: 'Install Availability' },
    { id: 'bsiz6sw8r', name: 'Cost Records' },
    { id: 'brh36h9mh', name: 'Task Submissions' },
  ];

  console.log(`Starting with ${knownTables.length} known tables:\n`);

  knownTables.forEach(table => {
    console.log(`  [${table.id}] ${table.name}`);
  });

  console.log('\n✅ Will discover additional tables by analyzing dblink fields\n');

  return knownTables;
}

async function findRelationshipFields(tables) {
  console.log('\n\n' + '='.repeat(80));
  console.log('ANALYZING TABLE RELATIONSHIPS (DBLINK & LOOKUP FIELDS)');
  console.log('='.repeat(80));
  console.log('\n');

  const relationships = [];
  const discoveredTables = new Set(tables.map(t => t.id));
  const newTables = [];

  for (const table of tables) {
    console.log(`\nAnalyzing ${table.name} (${table.id})...`);

    const fieldsResponse = await qbRequest(`/v1/fields?tableId=${table.id}`, 'GET');

    if (fieldsResponse.statusCode !== 200) {
      console.log(`  ❌ Failed to get fields`);
      continue;
    }

    const fields = Array.isArray(fieldsResponse.data)
      ? fieldsResponse.data
      : (fieldsResponse.data.fields || []);

    // Find relationship fields (dblink, lookup)
    const relationshipFields = fields.filter(f =>
      f.fieldType === 'dblink' ||
      (f.properties && (f.properties.foreignTableId || f.properties.targetTableId || f.properties.lookupTargetTableId))
    );

    if (relationshipFields.length > 0) {
      console.log(`  ✅ Found ${relationshipFields.length} relationship field(s):`);

      relationshipFields.forEach(field => {
        const targetTable = field.properties?.foreignTableId ||
                           field.properties?.targetTableId ||
                           field.properties?.lookupTargetTableId;
        const targetField = field.properties?.foreignFieldId ||
                           field.properties?.targetFieldId ||
                           field.properties?.lookupTargetFieldId;

        console.log(`     [${field.id}] ${field.label}`);
        console.log(`        Type: ${field.fieldType}`);
        if (targetTable) {
          console.log(`        → Links to table: ${targetTable}`);

          // Discover new tables
          if (!discoveredTables.has(targetTable)) {
            discoveredTables.add(targetTable);
            newTables.push({ id: targetTable, name: `Unknown (${targetTable})`, discoveredFrom: table.name });
          }
        }
        if (targetField) console.log(`        → Links to field: ${targetField}`);

        relationships.push({
          sourceTable: table.name,
          sourceTableId: table.id,
          sourceField: field.label,
          sourceFieldId: field.id,
          fieldType: field.fieldType,
          targetTableId: targetTable,
          targetFieldId: targetField
        });
      });
    }

    await new Promise(resolve => setTimeout(resolve, 300));
  }

  if (newTables.length > 0) {
    console.log('\n\n🔍 DISCOVERED NEW TABLES FROM RELATIONSHIPS:');
    newTables.forEach(t => {
      console.log(`   [${t.id}] Discovered from ${t.discoveredFrom}`);
    });
  }

  return { relationships, newTables };
}

async function findOutreachAndSalesAidTables(tables) {
  console.log('\n\n' + '='.repeat(80));
  console.log('SEARCHING FOR OUTREACH & SALES AID TABLES');
  console.log('='.repeat(80));
  console.log('\n');

  const keywords = ['outreach', 'sales aid', 'escalation', 'sales', 'aid'];
  const matches = [];

  keywords.forEach(keyword => {
    const found = tables.filter(t =>
      t.name.toLowerCase().includes(keyword)
    );

    if (found.length > 0) {
      console.log(`\n🔍 Tables matching "${keyword}":`);
      found.forEach(t => {
        console.log(`   [${t.id}] ${t.name}`);
        matches.push({ keyword, table: t });
      });
    }
  });

  if (matches.length === 0) {
    console.log('\n⚠️  No tables found with "outreach" or "sales aid" in name');
    console.log('    These may be external tables or named differently');
  }

  return matches;
}

async function mapPCWorkflowDataFlow(tables, relationships) {
  console.log('\n\n' + '='.repeat(80));
  console.log('MAPPING PC WORKFLOW DATA FLOW');
  console.log('='.repeat(80));
  console.log('\n');

  // Find Projects table relationships
  const projectsRelationships = relationships.filter(r =>
    r.sourceTable === 'Projects' || r.targetTableId === 'br9kwm8na'
  );

  console.log('📊 PROJECT TABLE CONNECTIONS:\n');

  const outgoingLinks = projectsRelationships.filter(r => r.sourceTable === 'Projects');
  const incomingLinks = relationships.filter(r => r.targetTableId === 'br9kwm8na');

  console.log(`Outgoing Links (${outgoingLinks.length}):`);
  outgoingLinks.slice(0, 20).forEach(link => {
    console.log(`  ${link.sourceField} [${link.sourceFieldId}]`);
    console.log(`    → ${link.fieldType} to table ${link.targetTableId}`);
  });

  console.log(`\nIncoming Links (${incomingLinks.length}):`);
  const groupedIncoming = {};
  incomingLinks.forEach(link => {
    if (!groupedIncoming[link.sourceTable]) {
      groupedIncoming[link.sourceTable] = [];
    }
    groupedIncoming[link.sourceTable].push(link);
  });

  Object.entries(groupedIncoming).forEach(([table, links]) => {
    console.log(`\n  FROM: ${table} (${links.length} field(s))`);
    links.slice(0, 5).forEach(link => {
      console.log(`    - ${link.sourceField} [${link.sourceFieldId}]`);
    });
    if (links.length > 5) {
      console.log(`    ... and ${links.length - 5} more`);
    }
  });

  // Find Install Communications relationships
  console.log('\n\n📞 INSTALL COMMUNICATIONS TABLE CONNECTIONS:\n');
  const commRelationships = relationships.filter(r =>
    r.sourceTable === 'Install Communications' || r.sourceTableId === 'bsb6bqt3b'
  );

  commRelationships.forEach(link => {
    console.log(`  ${link.sourceField} [${link.sourceFieldId}]`);
    console.log(`    → ${link.fieldType} to table ${link.targetTableId}`);
  });

  // Find Task Groups relationships
  console.log('\n\n📋 TASK GROUPS TABLE CONNECTIONS:\n');
  const taskGroupRelationships = relationships.filter(r =>
    r.sourceTable === 'Task Groups' || r.sourceTableId === 'bu36gem4p'
  );

  taskGroupRelationships.forEach(link => {
    console.log(`  ${link.sourceField} [${link.sourceFieldId}]`);
    console.log(`    → ${link.fieldType} to table ${link.targetTableId}`);
  });
}

async function analyzeCriticalMissingTables(tables) {
  console.log('\n\n' + '='.repeat(80));
  console.log('ANALYZING CRITICAL MISSING TABLES');
  console.log('='.repeat(80));
  console.log('\n');

  // Check if Outreach Records exists
  const outreachTable = tables.find(t =>
    t.name.toLowerCase().includes('outreach') &&
    !t.name.includes('Communication')
  );

  console.log('1. OUTREACH RECORDS TABLE:');
  if (outreachTable) {
    console.log(`   ✅ Found: [${outreachTable.id}] ${outreachTable.name}`);

    // Get sample data
    const sampleResponse = await qbRequest('/v1/records/query', 'POST', {
      from: outreachTable.id,
      select: ['*'],
      options: { top: 5 }
    });

    if (sampleResponse.statusCode === 200 && sampleResponse.data.data) {
      console.log(`   📊 Contains ${sampleResponse.data.data.length} sample records`);
    }
  } else {
    console.log('   ⚠️  NOT FOUND - Field 1640 "Outreach records" may link to external table or different name');
  }

  // Check if Sales Aid table exists
  const salesAidTable = tables.find(t =>
    t.name.toLowerCase().includes('sales aid') ||
    t.name.toLowerCase().includes('salesaid')
  );

  console.log('\n2. SALES AID REQUESTS TABLE:');
  if (salesAidTable) {
    console.log(`   ✅ Found: [${salesAidTable.id}] ${salesAidTable.name}`);

    const sampleResponse = await qbRequest('/v1/records/query', 'POST', {
      from: salesAidTable.id,
      select: ['*'],
      options: { top: 5 }
    });

    if (sampleResponse.statusCode === 200 && sampleResponse.data.data) {
      console.log(`   📊 Contains ${sampleResponse.data.data.length} sample records`);
    }
  } else {
    console.log('   ⚠️  NOT FOUND - Field 1792 "Sales Aid Request Records" may link to external table');
  }

  // Look for any table with "ZD" (Zendesk)
  const zdTable = tables.find(t =>
    t.name.toLowerCase().includes('zd') ||
    t.name.toLowerCase().includes('zendesk')
  );

  console.log('\n3. ZENDESK INTEGRATION TABLE:');
  if (zdTable) {
    console.log(`   ✅ Found: [${zdTable.id}] ${zdTable.name}`);
  } else {
    console.log('   ⚠️  NOT FOUND - Field 1936 "ZD Outreach Records" may be external');
  }
}

async function generateRelationshipDiagram(relationships) {
  console.log('\n\n' + '='.repeat(80));
  console.log('TABLE RELATIONSHIP DIAGRAM');
  console.log('='.repeat(80));
  console.log('\n');

  // Group relationships by source table
  const byTable = {};
  relationships.forEach(rel => {
    if (!byTable[rel.sourceTable]) {
      byTable[rel.sourceTable] = [];
    }
    byTable[rel.sourceTable].push(rel);
  });

  console.log('RELATIONSHIP MAP:\n');

  Object.entries(byTable).forEach(([table, links]) => {
    console.log(`\n${table}`);

    // Group by target table
    const byTarget = {};
    links.forEach(link => {
      const target = link.targetTableId || 'Unknown';
      if (!byTarget[target]) {
        byTarget[target] = [];
      }
      byTarget[target].push(link);
    });

    Object.entries(byTarget).forEach(([target, targetLinks]) => {
      console.log(`  ├─→ ${target} (${targetLinks.length} link${targetLinks.length > 1 ? 's' : ''})`);
      targetLinks.slice(0, 3).forEach((link, idx) => {
        const isLast = idx === Math.min(targetLinks.length, 3) - 1;
        const prefix = isLast ? '  └──' : '  ├──';
        console.log(`  ${prefix} ${link.sourceField}`);
      });
      if (targetLinks.length > 3) {
        console.log(`      ... and ${targetLinks.length - 3} more`);
      }
    });
  });
}

async function main() {
  const tables = await getAllAppTables();

  if (tables.length === 0) {
    console.log('❌ Could not retrieve tables. Exiting.');
    return;
  }

  const { relationships, newTables } = await findRelationshipFields(tables);

  // Combine known tables with discovered tables
  const allTables = [...tables, ...newTables];

  await findOutreachAndSalesAidTables(allTables);
  await analyzeCriticalMissingTables(allTables);
  await mapPCWorkflowDataFlow(allTables, relationships);
  await generateRelationshipDiagram(relationships);

  console.log('\n\n' + '='.repeat(80));
  console.log('PHASE 2 COMPLETE');
  console.log('='.repeat(80));
  console.log('\n');
  console.log(`✅ Known Tables: ${tables.length}`);
  console.log(`✅ Discovered Tables: ${newTables.length}`);
  console.log(`✅ Total Tables: ${allTables.length}`);
  console.log(`✅ Mapped ${relationships.length} table relationships`);
  console.log(`✅ Identified data flow for PC workflow`);
  console.log('\nNext: Phase 3 - Analyze PC workflow patterns and triggers');
}

main().catch(console.error);
