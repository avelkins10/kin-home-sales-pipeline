const https = require('https');

const QB_REALM = 'kin.quickbase.com';
const QB_USER_TOKEN = 'b6um6p_p3bs_0_did2y4mcxgmvm3d3k46nhdy9t68g';
const CONTACTS_TABLE_ID = 'br9kwm8td';
const APP_ID = 'br9kwm8bk';

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

async function getAllContactFields() {
  console.log('='.repeat(80));
  console.log('CONTACTS TABLE - COMPLETE FIELD MAPPING');
  console.log('='.repeat(80));
  console.log('\n');
  console.log(`Table ID: ${CONTACTS_TABLE_ID}`);
  console.log(`App ID: ${APP_ID}\n`);

  console.log('Getting all fields from Contacts table...\n');

  const fieldsResponse = await qbRequest(`/v1/fields?tableId=${CONTACTS_TABLE_ID}`, 'GET');

  if (fieldsResponse.statusCode !== 200) {
    console.log(`❌ Failed to get fields: ${fieldsResponse.statusCode}`);
    if (fieldsResponse.data) {
      console.log('Error:', JSON.stringify(fieldsResponse.data, null, 2));
    }
    return null;
  }

  const fields = Array.isArray(fieldsResponse.data)
    ? fieldsResponse.data
    : (fieldsResponse.data.fields || []);

  console.log(`✅ Found ${fields.length} fields in Contacts table\n`);

  return fields;
}

function categorizeFields(fields) {
  const categories = {
    identity: [],
    repcard: [],
    enerflo: [],
    contact: [],
    role: [],
    status: [],
    office: [],
    team: [],
    user: [],
    other: []
  };

  fields.forEach(field => {
    const label = field.label.toLowerCase();

    if (label.includes('repcard') || label.includes('rep card')) {
      categories.repcard.push(field);
    } else if (label.includes('enerflo')) {
      categories.enerflo.push(field);
    } else if (label.includes('email') || label.includes('phone') || label.includes('mobile')) {
      categories.contact.push(field);
    } else if (label.includes('role') || label.includes('position') || label.includes('closer') || label.includes('setter')) {
      categories.role.push(field);
    } else if (label.includes('status') || label.includes('active') || label.includes('inactive')) {
      categories.status.push(field);
    } else if (label.includes('office')) {
      categories.office.push(field);
    } else if (label.includes('team')) {
      categories.team.push(field);
    } else if (label.includes('user') || label.includes('quickbase')) {
      categories.user.push(field);
    } else if (field.id <= 10 || label.includes('name') || label.includes('first') || label.includes('last')) {
      categories.identity.push(field);
    } else {
      categories.other.push(field);
    }
  });

  return categories;
}

function printFieldCategories(categories) {
  console.log('📋 FIELD CATEGORIES:\n');

  console.log('IDENTITY FIELDS:');
  categories.identity.forEach(f => {
    console.log(`  [${f.id}] ${f.label} (${f.fieldType})`);
  });

  console.log('\nREPCARD FIELDS:');
  categories.repcard.forEach(f => {
    console.log(`  [${f.id}] ${f.label} (${f.fieldType})`);
    if (f.properties?.choices) {
      console.log(`     Values: ${f.properties.choices.slice(0, 5).join(', ')}${f.properties.choices.length > 5 ? '...' : ''}`);
    }
  });

  console.log('\nENERFLO FIELDS:');
  categories.enerflo.forEach(f => {
    console.log(`  [${f.id}] ${f.label} (${f.fieldType})`);
  });

  console.log('\nCONTACT INFO FIELDS:');
  categories.contact.forEach(f => {
    console.log(`  [${f.id}] ${f.label} (${f.fieldType})`);
  });

  console.log('\nROLE FIELDS:');
  categories.role.forEach(f => {
    console.log(`  [${f.id}] ${f.label} (${f.fieldType})`);
    if (f.properties?.choices) {
      console.log(`     Values: ${f.properties.choices.slice(0, 5).join(', ')}${f.properties.choices.length > 5 ? '...' : ''}`);
    }
  });

  console.log('\nSTATUS FIELDS:');
  categories.status.forEach(f => {
    console.log(`  [${f.id}] ${f.label} (${f.fieldType})`);
    if (f.properties?.choices) {
      console.log(`     Values: ${f.properties.choices.join(', ')}`);
    }
  });

  console.log('\nOFFICE FIELDS:');
  categories.office.forEach(f => {
    console.log(`  [${f.id}] ${f.label} (${f.fieldType})`);
    if (f.properties?.choices && f.properties.choices.length < 20) {
      console.log(`     Values: ${f.properties.choices.join(', ')}`);
    }
  });

  console.log('\nTEAM FIELDS:');
  categories.team.forEach(f => {
    console.log(`  [${f.id}] ${f.label} (${f.fieldType})`);
  });

  console.log('\nUSER/QUICKBASE FIELDS:');
  categories.user.forEach(f => {
    console.log(`  [${f.id}] ${f.label} (${f.fieldType})`);
  });

  console.log('\nOTHER FIELDS:');
  categories.other.slice(0, 30).forEach(f => {
    console.log(`  [${f.id}] ${f.label} (${f.fieldType})`);
  });
  if (categories.other.length > 30) {
    console.log(`  ... and ${categories.other.length - 30} more fields`);
  }
}

async function getSampleRecords(fields) {
  console.log('\n\n' + '='.repeat(80));
  console.log('SAMPLE RECORDS - ALL CONTACTS');
  console.log('='.repeat(80));
  console.log('\n');

  // Get 20 recent records to analyze
  const recordsResponse = await qbRequest('/v1/records/query', 'POST', {
    from: CONTACTS_TABLE_ID,
    select: fields.slice(0, 100).map(f => f.id), // First 100 fields
    sortBy: [{ fieldId: 1, order: 'DESC' }],
    options: { top: 20 }
  });

  if (recordsResponse.statusCode !== 200) {
    console.log(`❌ Failed to get records: ${recordsResponse.statusCode}`);
    return [];
  }

  const records = recordsResponse.data.data || [];
  console.log(`Retrieved ${records.length} sample records\n`);

  return records;
}

function analyzeRepVsCustomer(fields, records) {
  console.log('\n' + '='.repeat(80));
  console.log('IDENTIFYING SALES REPS VS CUSTOMERS');
  console.log('='.repeat(80));
  console.log('\n');

  // Look for fields that might distinguish reps from customers
  const potentialDistinguishers = fields.filter(f => {
    const label = f.label.toLowerCase();
    return label.includes('type') ||
           label.includes('category') ||
           label.includes('contact type') ||
           label.includes('customer') ||
           label.includes('internal') ||
           label.includes('employee') ||
           label.includes('rep') ||
           label.includes('staff');
  });

  console.log('🔍 POTENTIAL DISTINGUISHING FIELDS:\n');
  potentialDistinguishers.forEach(f => {
    console.log(`  [${f.id}] ${f.label} (${f.fieldType})`);
    if (f.properties?.choices) {
      console.log(`     Possible values: ${f.properties.choices.join(', ')}`);
    }
  });

  // Analyze records
  console.log('\n\n📊 ANALYZING SAMPLE RECORDS:\n');

  // Look for repcard or enerflo IDs as indicators
  const repcardFields = fields.filter(f => f.label.toLowerCase().includes('repcard'));
  const enerfloFields = fields.filter(f => f.label.toLowerCase().includes('enerflo'));

  let recordsWithRepcard = 0;
  let recordsWithEnerflo = 0;

  records.forEach((record, idx) => {
    let hasRepcard = false;
    let hasEnerflo = false;

    repcardFields.forEach(f => {
      const value = record[f.id]?.value;
      if (value !== null && value !== undefined && value !== '' && value !== false) {
        hasRepcard = true;
      }
    });

    enerfloFields.forEach(f => {
      const value = record[f.id]?.value;
      if (value !== null && value !== undefined && value !== '' && value !== false) {
        hasEnerflo = true;
      }
    });

    if (hasRepcard) recordsWithRepcard++;
    if (hasEnerflo) recordsWithEnerflo++;

    // Show first 3 records details
    if (idx < 3) {
      console.log(`\nRecord ${idx + 1} (ID: ${record[3]?.value}):`);

      // Show name
      const firstName = record[7]?.value;
      const lastName = record[8]?.value;
      const fullName = record[6]?.value;
      console.log(`  Name: ${fullName || `${firstName} ${lastName}` || 'N/A'}`);

      // Show type/category if exists
      potentialDistinguishers.forEach(f => {
        const value = record[f.id]?.value;
        if (value) {
          console.log(`  ${f.label}: ${value}`);
        }
      });

      // Show repcard/enerflo status
      console.log(`  Has RepCard ID: ${hasRepcard ? '✓' : '✗'}`);
      console.log(`  Has Enerflo ID: ${hasEnerflo ? '✓' : '✗'}`);

      // Show email
      const emailField = fields.find(f => f.label.toLowerCase() === 'email');
      if (emailField) {
        console.log(`  Email: ${record[emailField.id]?.value || 'N/A'}`);
      }
    }
  });

  console.log(`\n\nSUMMARY OF SAMPLE (${records.length} records):`);
  console.log(`  Records with RepCard ID: ${recordsWithRepcard} (${((recordsWithRepcard/records.length)*100).toFixed(1)}%)`);
  console.log(`  Records with Enerflo ID: ${recordsWithEnerflo} (${((recordsWithEnerflo/records.length)*100).toFixed(1)}%)`);

  console.log('\n💡 HYPOTHESIS:');
  if (recordsWithRepcard > 0) {
    console.log('  Sales reps likely have RepCard IDs populated');
    console.log('  Customers likely have RepCard ID empty/null');
  }
  if (recordsWithEnerflo > 0) {
    console.log('  Some contacts have Enerflo IDs (may indicate different system)');
  }
}

async function getSalesRepExamples(fields) {
  console.log('\n\n' + '='.repeat(80));
  console.log('SALES REP EXAMPLES - DETAILED RECORDS');
  console.log('='.repeat(80));
  console.log('\n');

  // Try to find sales reps by querying for records with RepCard IDs
  const repcardField = fields.find(f =>
    f.label.toLowerCase().includes('repcard') &&
    f.label.toLowerCase().includes('user') &&
    !f.label.toLowerCase().includes('image')
  );

  if (!repcardField) {
    console.log('⚠️  Could not find RepCard User ID field');
    console.log('   Attempting alternative search...\n');
  }

  // Get records with populated key fields
  const selectFields = [
    3, 6, 7, 8, // ID, Full Name, First Name, Last Name
    ...fields.filter(f => {
      const label = f.label.toLowerCase();
      return label.includes('repcard') ||
             label.includes('enerflo') ||
             label.includes('email') ||
             label.includes('phone') ||
             label.includes('role') ||
             label.includes('office') ||
             label.includes('team') ||
             label.includes('status');
    }).map(f => f.id).slice(0, 40)
  ];

  const recordsResponse = await qbRequest('/v1/records/query', 'POST', {
    from: CONTACTS_TABLE_ID,
    select: selectFields,
    sortBy: [{ fieldId: 1, order: 'DESC' }],
    options: { top: 50 }
  });

  if (recordsResponse.statusCode !== 200) {
    console.log(`❌ Failed to get sales rep examples: ${recordsResponse.statusCode}`);
    return;
  }

  const records = recordsResponse.data.data || [];

  // Filter for records that look like sales reps (have repcard or role info)
  const salesReps = records.filter(record => {
    // Check if has repcard ID
    const hasRepcard = fields.some(f => {
      if (f.label.toLowerCase().includes('repcard') && f.label.toLowerCase().includes('user')) {
        const value = record[f.id]?.value;
        return value !== null && value !== undefined && value !== '';
      }
      return false;
    });

    // Check if has role info
    const hasRole = fields.some(f => {
      if (f.label.toLowerCase().includes('role')) {
        const value = record[f.id]?.value;
        return value !== null && value !== undefined && value !== '';
      }
      return false;
    });

    return hasRepcard || hasRole;
  });

  console.log(`Found ${salesReps.length} potential sales reps in sample\n`);

  // Display detailed info for first 3 sales reps
  salesReps.slice(0, 3).forEach((record, idx) => {
    console.log(`\n${'─'.repeat(60)}`);
    console.log(`SALES REP EXAMPLE ${idx + 1}`);
    console.log('─'.repeat(60));

    // Show all populated fields
    Object.keys(record)
      .filter(key => !isNaN(key))
      .forEach(fieldId => {
        const field = fields.find(f => f.id === parseInt(fieldId));
        const value = record[fieldId]?.value;

        if (value !== null && value !== undefined && value !== '' && value !== false) {
          const displayValue = typeof value === 'object' ? JSON.stringify(value).substring(0, 100) : value;
          console.log(`[${fieldId}] ${field?.label || 'Unknown'}: ${displayValue.toString().substring(0, 100)}`);
        }
      });
  });
}

async function getDataQualityMetrics(fields) {
  console.log('\n\n' + '='.repeat(80));
  console.log('DATA QUALITY ASSESSMENT');
  console.log('='.repeat(80));
  console.log('\n');

  // Get total count
  console.log('Querying total contact count...\n');

  const countResponse = await qbRequest('/v1/records/query', 'POST', {
    from: CONTACTS_TABLE_ID,
    select: [3],
    options: { top: 1 }
  });

  if (countResponse.statusCode !== 200) {
    console.log('❌ Failed to get total count');
    return;
  }

  // QuickBase returns metadata with total records
  const metadata = countResponse.data.metadata;
  const totalRecords = metadata?.totalRecords || 'Unknown';

  console.log(`📊 TOTAL CONTACTS: ${totalRecords}\n`);

  // Sample analysis for rep vs customer distribution
  console.log('Analyzing sample of 100 records for quality metrics...\n');

  const sampleResponse = await qbRequest('/v1/records/query', 'POST', {
    from: CONTACTS_TABLE_ID,
    select: fields.slice(0, 60).map(f => f.id),
    sortBy: [{ fieldId: 1, order: 'DESC' }],
    options: { top: 100 }
  });

  if (sampleResponse.statusCode !== 200) {
    console.log('❌ Failed to get sample for analysis');
    return;
  }

  const sample = sampleResponse.data.data || [];

  // Find key fields
  const repcardUserField = fields.find(f =>
    f.label.toLowerCase().includes('repcard') &&
    f.label.toLowerCase().includes('user') &&
    !f.label.toLowerCase().includes('image')
  );

  const enerfloUserField = fields.find(f =>
    f.label.toLowerCase().includes('enerflo') &&
    f.label.toLowerCase().includes('user')
  );

  const emailField = fields.find(f => f.label.toLowerCase() === 'email');

  let repcardPopulated = 0;
  let enerfloPopulated = 0;
  let emailPopulated = 0;
  let potentialReps = 0;

  sample.forEach(record => {
    if (repcardUserField) {
      const value = record[repcardUserField.id]?.value;
      if (value !== null && value !== undefined && value !== '') {
        repcardPopulated++;
        potentialReps++;
      }
    }

    if (enerfloUserField) {
      const value = record[enerfloUserField.id]?.value;
      if (value !== null && value !== undefined && value !== '') {
        enerfloPopulated++;
      }
    }

    if (emailField) {
      const value = record[emailField.id]?.value;
      if (value !== null && value !== undefined && value !== '') {
        emailPopulated++;
      }
    }
  });

  console.log('SAMPLE QUALITY METRICS (100 records):');
  console.log(`  RepCard ID populated: ${repcardPopulated} (${(repcardPopulated/sample.length*100).toFixed(1)}%)`);
  console.log(`  Enerflo ID populated: ${enerfloPopulated} (${(enerfloPopulated/sample.length*100).toFixed(1)}%)`);
  console.log(`  Email populated: ${emailPopulated} (${(emailPopulated/sample.length*100).toFixed(1)}%)`);
  console.log(`  Potential sales reps: ${potentialReps} (${(potentialReps/sample.length*100).toFixed(1)}%)`);

  console.log(`\n💡 ESTIMATED TOTALS (based on sample):`);
  if (totalRecords !== 'Unknown') {
    const estReps = Math.round(totalRecords * (potentialReps / sample.length));
    const estCustomers = totalRecords - estReps;
    console.log(`  Estimated sales reps: ~${estReps}`);
    console.log(`  Estimated customers: ~${estCustomers}`);
  }
}

function generateFieldMapping(categories, fields) {
  console.log('\n\n' + '='.repeat(80));
  console.log('SUGGESTED FIELD MAPPING FOR SYNC SCRIPT');
  console.log('='.repeat(80));
  console.log('\n');

  console.log('```typescript');
  console.log('export const CONTACT_FIELDS = {');
  console.log('  // System fields');
  console.log('  RECORD_ID: 3,');
  console.log('  DATE_CREATED: 1,');
  console.log('  DATE_MODIFIED: 2,');
  console.log('  RECORD_OWNER: 4,');
  console.log('  LAST_MODIFIED_BY: 5,');
  console.log('');

  // Identity fields
  const fullNameField = fields.find(f => f.label === 'Full Name' || f.id === 6);
  const firstNameField = fields.find(f => f.label === 'First Name' || f.id === 7);
  const lastNameField = fields.find(f => f.label === 'Last Name' || f.id === 8);

  console.log('  // Name fields');
  if (fullNameField) console.log(`  FULL_NAME: ${fullNameField.id}, // ${fullNameField.label}`);
  if (firstNameField) console.log(`  FIRST_NAME: ${firstNameField.id}, // ${firstNameField.label}`);
  if (lastNameField) console.log(`  LAST_NAME: ${lastNameField.id}, // ${lastNameField.label}`);
  console.log('');

  // Contact info
  console.log('  // Contact information');
  categories.contact.forEach(f => {
    const varName = f.label.toUpperCase().replace(/[^A-Z0-9]/g, '_').replace(/_+/g, '_');
    console.log(`  ${varName}: ${f.id}, // ${f.label}`);
  });
  console.log('');

  // RepCard fields
  if (categories.repcard.length > 0) {
    console.log('  // RepCard fields');
    categories.repcard.forEach(f => {
      const varName = f.label.toUpperCase().replace(/[^A-Z0-9]/g, '_').replace(/_+/g, '_');
      console.log(`  ${varName}: ${f.id}, // ${f.label}`);
    });
    console.log('');
  }

  // Enerflo fields
  if (categories.enerflo.length > 0) {
    console.log('  // Enerflo fields');
    categories.enerflo.forEach(f => {
      const varName = f.label.toUpperCase().replace(/[^A-Z0-9]/g, '_').replace(/_+/g, '_');
      console.log(`  ${varName}: ${f.id}, // ${f.label}`);
    });
    console.log('');
  }

  // Role fields
  if (categories.role.length > 0) {
    console.log('  // Role fields');
    categories.role.forEach(f => {
      const varName = f.label.toUpperCase().replace(/[^A-Z0-9]/g, '_').replace(/_+/g, '_');
      console.log(`  ${varName}: ${f.id}, // ${f.label}`);
    });
    console.log('');
  }

  // Office/Team fields
  if (categories.office.length > 0) {
    console.log('  // Office fields');
    categories.office.forEach(f => {
      const varName = f.label.toUpperCase().replace(/[^A-Z0-9]/g, '_').replace(/_+/g, '_');
      console.log(`  ${varName}: ${f.id}, // ${f.label}`);
    });
    console.log('');
  }

  if (categories.team.length > 0) {
    console.log('  // Team fields');
    categories.team.forEach(f => {
      const varName = f.label.toUpperCase().replace(/[^A-Z0-9]/g, '_').replace(/_+/g, '_');
      console.log(`  ${varName}: ${f.id}, // ${f.label}`);
    });
    console.log('');
  }

  // Status fields
  if (categories.status.length > 0) {
    console.log('  // Status fields');
    categories.status.forEach(f => {
      const varName = f.label.toUpperCase().replace(/[^A-Z0-9]/g, '_').replace(/_+/g, '_');
      console.log(`  ${varName}: ${f.id}, // ${f.label}`);
    });
    console.log('');
  }

  // User fields
  if (categories.user.length > 0) {
    console.log('  // QuickBase user fields');
    categories.user.forEach(f => {
      const varName = f.label.toUpperCase().replace(/[^A-Z0-9]/g, '_').replace(/_+/g, '_');
      console.log(`  ${varName}: ${f.id}, // ${f.label}`);
    });
  }

  console.log('} as const;');
  console.log('```');

  // Generate sample query
  console.log('\n\n// Sample query to get sales reps:');
  console.log('```javascript');

  const repcardField = categories.repcard.find(f =>
    f.label.toLowerCase().includes('user') && !f.label.toLowerCase().includes('image')
  );

  if (repcardField) {
    console.log(`// Query all contacts with RepCard IDs (sales reps)`);
    console.log(`const salesRepsResponse = await qbRequest('/v1/records/query', 'POST', {`);
    console.log(`  from: '${CONTACTS_TABLE_ID}',`);
    console.log(`  select: [`);
    console.log(`    CONTACT_FIELDS.RECORD_ID,`);
    console.log(`    CONTACT_FIELDS.FULL_NAME,`);
    console.log(`    CONTACT_FIELDS.EMAIL,`);
    console.log(`    // Add all fields you need from CONTACT_FIELDS`);
    console.log(`  ],`);
    console.log(`  where: '{${repcardField.id}.XEX}', // Has RepCard ID (not empty)`);
    console.log(`  sortBy: [{ fieldId: CONTACT_FIELDS.LAST_NAME, order: 'ASC' }]`);
    console.log(`});`);
  } else {
    console.log(`// Query all contacts`);
    console.log(`const contactsResponse = await qbRequest('/v1/records/query', 'POST', {`);
    console.log(`  from: '${CONTACTS_TABLE_ID}',`);
    console.log(`  select: [/* field IDs */],`);
    console.log(`  sortBy: [{ fieldId: 1, order: 'DESC' }]`);
    console.log(`});`);
  }
  console.log('```');
}

async function main() {
  try {
    const fields = await getAllContactFields();

    if (!fields) {
      console.log('\n❌ Failed to retrieve fields. Exiting.');
      return;
    }

    const categories = categorizeFields(fields);

    printFieldCategories(categories);

    const sampleRecords = await getSampleRecords(fields);

    analyzeRepVsCustomer(fields, sampleRecords);

    await getSalesRepExamples(fields);

    await getDataQualityMetrics(fields);

    generateFieldMapping(categories, fields);

    console.log('\n\n' + '='.repeat(80));
    console.log('ANALYSIS COMPLETE');
    console.log('='.repeat(80));
    console.log('\n');
    console.log('✅ Field mapping generated');
    console.log('✅ Sales rep identification method identified');
    console.log('✅ Sample records retrieved');
    console.log('✅ Data quality metrics calculated');
    console.log('\nReady to build sync script!');

  } catch (error) {
    console.error('Error:', error);
  }
}

main();
