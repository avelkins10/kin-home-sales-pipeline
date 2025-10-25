/**
 * QUICKBASE API QUERIES - PC DASHBOARD
 * Ready-to-use API query functions for PC platform
 *
 * Based on comprehensive QuickBase analysis (Phases 1-5)
 * All field IDs and table relationships verified
 */

const https = require('https');

const QB_REALM = 'kin.quickbase.com';
const QB_USER_TOKEN = 'b6um6p_p3bs_0_did2y4mcxgmvm3d3k46nhdy9t68g';

// ============================================================================
// HELPER FUNCTION
// ============================================================================

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

// ============================================================================
// DASHBOARD SECTION QUERIES
// ============================================================================

/**
 * Get Initial Outreach projects for a PC
 * Appears when: Due milestone OR Create Check-In OR Incomplete Check-In
 */
async function getInitialOutreachProjects(pcName) {
  const response = await qbRequest('/v1/records/query', 'POST', {
    from: 'br9kwm8na',
    select: [
      3,      // Record ID
      6,      // Legacy Customer
      145,    // Customer Name (new)
      255,    // Status
      820,    // PC Name
      2268,   // # of Due Milestone Outreach records
      2287,   // PC Outreach: Create Check-In?
      2266,   // PC Outreach: Incomplete Check-In?
      1640,   // Outreach records (dblink)
      2404,   // Preferred Outreach Method
      2086,   // Manual Preferred Outreach
      1735,   // Days Since Last Contact - Intake
      1736,   // Days Since Last Contact - NEM
      1737,   // Days Since Last Contact - PTO
      1738,   // Days Since Last Contact - Install
    ],
    where: `{820.EX.'${pcName}'} AND {255.EX.'Active'} AND ({2268.GT.0} OR {2287.EX.true} OR {2266.EX.true})`,
    sortBy: [
      { fieldId: 2268, order: 'DESC' },  // Most due outreach first
      { fieldId: 1735, order: 'DESC' }   // Then by days since contact
    ]
  });

  return response.data.data || [];
}

/**
 * Get Unresponsive Customers for a PC
 * Appears when: Is Unresponsive OR Active Unresponsive Sales Aid
 */
async function getUnresponsiveCustomers(pcName) {
  const response = await qbRequest('/v1/records/query', 'POST', {
    from: 'br9kwm8na',
    select: [
      3,      // Record ID
      6,      // Customer
      145,    // Customer Name
      255,    // Status
      820,    // PC Name
      1909,   // Is Unresponsive?
      1908,   // # Active Unresponsive Sales Aid Records
      1708,   // Contact Attempts - Intake
      1709,   // Contact Attempts - NEM
      1710,   // Contact Attempts - PTO
      1711,   // Contact Attempts - Install
      1735,   // Days Since Contact - Intake
      1736,   // Days Since Contact - NEM
      1737,   // Days Since Contact - PTO
      1738,   // Days Since Contact - Install
      1731,   // Last Contact Date - Intake
      1732,   // Last Contact Date - NEM
      1733,   // Last Contact Date - PTO
      1734,   // Last Contact Date - Install
      2404,   // Preferred Outreach Method
    ],
    where: `{820.EX.'${pcName}'} AND {255.EX.'Active'} AND ({1909.EX.'Yes'} OR {1908.GT.0})`,
    sortBy: [
      { fieldId: 1908, order: 'DESC' },  // Active unresponsive sales aid count
      { fieldId: 1735, order: 'DESC' }   // Days since contact
    ]
  });

  return response.data.data || [];
}

/**
 * Get Pending Escalation projects for a PC
 * Queries Sales Aid Requests table filtered by PC
 */
async function getPendingEscalations(pcName) {
  const response = await qbRequest('/v1/records/query', 'POST', {
    from: 'bt3m39fgr',
    select: [
      3,      // Record ID
      85,     // Related Project
      86,     // Related Project - Full Name
      87,     // Related Project - Mobile Phone
      88,     // Related Project - Email
      84,     // Sales Aid Reason
      103,    // Sales Aid Status
      93,     // Escalate to Sales Aid (checkbox)
      108,    // Escalated Date/Time
      109,    // Assigned Escalation Rep
      113,    // Related Project - Project Coordinator
      91,     // Rep 72 hour period end date/time
      83,     // Requested Date/Time
    ],
    where: `{113.EX.'${pcName}'} AND {103.EX.'Escalated to Sales Aid'} AND {103.XEX.'Task Completed'} AND {103.XEX.'Task Cancelled'}`,
    sortBy: [
      { fieldId: 108, order: 'ASC' }  // Oldest escalation first
    ]
  });

  return response.data.data || [];
}

/**
 * Alternative: Get Pending Escalations from Projects table
 * Uses calculated field for count
 */
async function getPendingEscalationsViaProjects(pcName) {
  const response = await qbRequest('/v1/records/query', 'POST', {
    from: 'br9kwm8na',
    select: [
      3,      // Record ID
      6,      // Customer
      145,    // Customer Name
      255,    // Status
      820,    // PC Name
      2422,   // # of Escalated Sales Aid Records
      322,    // Project Escalation (ARC/High/Medium/Low)
      1792,   // Sales Aid Request Records (dblink)
    ],
    where: `{820.EX.'${pcName}'} AND {255.EX.'Active'} AND {2422.GT.0}`,
    sortBy: [
      { fieldId: 2422, order: 'DESC' }
    ]
  });

  return response.data.data || [];
}

/**
 * Get Blocked NEM projects for a PC
 * Queries Install Communications table
 */
async function getBlockedNEM(pcUserObjectId) {
  const response = await qbRequest('/v1/records/query', 'POST', {
    from: 'bsb6bqt3b',
    select: [
      3,      // Record ID
      8,      // Date
      9,      // Note by
      13,     // Related Project
      15,     // Communication Note
      167,    // PC User
      135,    // NEM Blocker Outreach
      35,     // Project Status
      64,     // Related Project Coordinator
    ],
    where: `{167.EX.${pcUserObjectId}} AND {135.EX.true} AND ({35.CT.'NEM'} OR {35.CT.'PTO'})`,
    sortBy: [
      { fieldId: 8, order: 'DESC' }  // Most recent first
    ]
  });

  return response.data.data || [];
}

// ============================================================================
// OUTREACH MANAGEMENT
// ============================================================================

/**
 * Get all outreach records for a project
 */
async function getProjectOutreachRecords(projectId) {
  const response = await qbRequest('/v1/records/query', 'POST', {
    from: 'btvik5kwi',
    select: [
      3,      // Record ID
      10,     // Related Project
      6,      // Touchpoint Name
      18,     // Outreach Completed Date
      43,     // Outreach Status
      44,     // # of Attempts
      45,     // Next Attempt #
      54,     // Reporting - Due Date
      86,     // Next Outreach Due Date
      8,      // Note
      37,     // Attempt Note
      19,     // Outreach Completed By
      17,     // Project Coordinator
    ],
    where: `{10.EX.${projectId}}`,
    sortBy: [
      { fieldId: 18, order: 'DESC' }
    ],
    options: { top: 50 }
  });

  return response.data.data || [];
}

/**
 * Get due outreach records for a project
 */
async function getDueOutreach(projectId) {
  const today = new Date().toISOString().split('T')[0];

  const response = await qbRequest('/v1/records/query', 'POST', {
    from: 'btvik5kwi',
    select: [
      3, 10, 6, 18, 43, 44, 54, 86, 8, 37
    ],
    where: `{10.EX.${projectId}} AND {54.OBF.'${today}'} AND ({43.EX.''}  OR {43.XEX.'Complete'})`,
    sortBy: [
      { fieldId: 54, order: 'ASC' }
    ]
  });

  return response.data.data || [];
}

/**
 * Create new outreach record
 */
async function createOutreachRecord(data) {
  const response = await qbRequest('/v1/records', 'POST', {
    to: 'btvik5kwi',
    data: [{
      10: { value: data.projectId },           // Related Project
      6: { value: data.touchpointName },       // Touchpoint Name
      54: { value: data.dueDate },            // Reporting - Due Date
      8: { value: data.note || '' },          // Note
      86: { value: data.nextOutreachDueDate } // Next Outreach Due Date (optional)
    }]
  });

  return response.data;
}

/**
 * Complete an outreach record
 */
async function completeOutreach(recordId, data) {
  const response = await qbRequest('/v1/records', 'POST', {
    to: 'btvik5kwi',
    data: [{
      3: { value: recordId },                                // Record ID
      43: { value: data.status },                            // Outreach Status
      18: { value: data.completedDate || new Date().toISOString() },  // Outreach Completed Date
      37: { value: data.attemptNote || '' },                 // Attempt Note
      44: { value: data.attemptCount }                       // # of Attempts (optional)
    }],
    mergeFieldId: 3
  });

  return response.data;
}

/**
 * Update outreach due date (snooze)
 */
async function snoozeOutreach(recordId, newDueDate) {
  const response = await qbRequest('/v1/records', 'POST', {
    to: 'btvik5kwi',
    data: [{
      3: { value: recordId },
      54: { value: newDueDate },      // Reporting - Due Date
      86: { value: newDueDate }       // Next Outreach Due Date
    }],
    mergeFieldId: 3
  });

  return response.data;
}

// ============================================================================
// SALES AID / ESCALATION MANAGEMENT
// ============================================================================

/**
 * Get all sales aid requests for a project
 */
async function getProjectSalesAidRequests(projectId) {
  const response = await qbRequest('/v1/records/query', 'POST', {
    from: 'bt3m39fgr',
    select: [
      3,      // Record ID
      85,     // Related Project
      84,     // Sales Aid Reason
      103,    // Sales Aid Status
      93,     // Escalate to Sales Aid
      108,    // Escalated Date/Time
      109,    // Assigned Escalation Rep
      83,     // Requested Date/Time
      91,     // Rep 72 hour period end
      119,    // Completed Date
    ],
    where: `{85.EX.${projectId}}`,
    sortBy: [
      { fieldId: 83, order: 'DESC' }
    ]
  });

  return response.data.data || [];
}

/**
 * Create sales aid request
 */
async function createSalesAidRequest(data) {
  const response = await qbRequest('/v1/records', 'POST', {
    to: 'bt3m39fgr',
    data: [{
      85: { value: data.projectId },                      // Related Project
      84: { value: data.reason },                         // Sales Aid Reason
      103: { value: 'Waiting for Rep' },                  // Sales Aid Status (default)
      // Field 83 (Requested Date/Time) auto-populated by QB
    }]
  });

  return response.data;
}

/**
 * Escalate sales aid request
 */
async function escalateSalesAidRequest(recordId, assignedRepUserId = null) {
  const data = {
    3: { value: recordId },
    93: { value: true },                                  // Escalate to Sales Aid
    103: { value: 'Escalated to Sales Aid' },            // Sales Aid Status
    108: { value: new Date().toISOString() }             // Escalated Date/Time
  };

  if (assignedRepUserId) {
    data[109] = { value: assignedRepUserId };            // Assigned Escalation Rep
  }

  const response = await qbRequest('/v1/records', 'POST', {
    to: 'bt3m39fgr',
    data: [data],
    mergeFieldId: 3
  });

  return response.data;
}

/**
 * Complete sales aid request
 */
async function completeSalesAidRequest(recordId, status = 'Task Completed') {
  const response = await qbRequest('/v1/records', 'POST', {
    to: 'bt3m39fgr',
    data: [{
      3: { value: recordId },
      103: { value: status },                             // Sales Aid Status
      119: { value: new Date().toISOString() }           // Completed Date
    }],
    mergeFieldId: 3
  });

  return response.data;
}

// ============================================================================
// PROJECT QUERIES
// ============================================================================

/**
 * Get detailed project info for PC dashboard
 */
async function getProjectDetails(projectId) {
  const response = await qbRequest('/v1/records/query', 'POST', {
    from: 'br9kwm8na',
    select: [
      // Identity
      3, 6, 145, 255,
      // PC Info
      819, 820, 2303,
      // Rep Info
      516, 517, 356, 357, 2277,
      // Outreach
      2404, 2086, 1640, 2267, 2268, 2149, 2264, 2287, 2266, 1980,
      // Contact Attempts
      1708, 1709, 1710, 1711,
      // Last Contact
      1731, 1732, 1733, 1734,
      // Days Since Contact
      1735, 1736, 1737, 1738,
      // Escalation
      322, 1909, 1792, 1912, 2422, 1908, 2423,
      // Communications
      186, 496, 2304
    ],
    where: `{3.EX.${projectId}}`
  });

  return response.data.data?.[0] || null;
}

/**
 * Get all active projects for a PC
 */
async function getPCActiveProjects(pcName) {
  const response = await qbRequest('/v1/records/query', 'POST', {
    from: 'br9kwm8na',
    select: [
      3, 6, 145, 255, 820,
      2268, 2422, 1908, 1909,
      1735, 1736, 1737, 1738,
      2404, 322
    ],
    where: `{820.EX.'${pcName}'} AND {255.EX.'Active'}`,
    sortBy: [
      { fieldId: 2268, order: 'DESC' }
    ]
  });

  return response.data.data || [];
}

/**
 * Get PC project counts by status
 */
async function getPCProjectCountsByStatus(pcName) {
  const response = await qbRequest('/v1/records/query', 'POST', {
    from: 'br9kwm8na',
    select: [3, 255],
    where: `{820.EX.'${pcName}'}`,
    options: { top: 1000 }
  });

  const projects = response.data.data || [];

  const counts = {};
  projects.forEach(p => {
    const status = p['255']?.value || 'Unknown';
    counts[status] = (counts[status] || 0) + 1;
  });

  return counts;
}

// ============================================================================
// COMMUNICATION / NOTES
// ============================================================================

/**
 * Get recent communications for a project
 */
async function getProjectCommunications(projectId, limit = 20) {
  const response = await qbRequest('/v1/records/query', 'POST', {
    from: 'bsb6bqt3b',
    select: [
      3,      // Record ID
      8,      // Date
      9,      // Note by
      13,     // Related Project
      15,     // Communication Note
      64,     // Related Project Coordinator
      167,    // PC User
      35,     // Project Status
      135,    // NEM Blocker Outreach
    ],
    where: `{13.EX.${projectId}}`,
    sortBy: [
      { fieldId: 8, order: 'DESC' }
    ],
    options: { top: limit }
  });

  return response.data.data || [];
}

/**
 * Create communication/note
 */
async function createCommunication(data) {
  const response = await qbRequest('/v1/records', 'POST', {
    to: 'bsb6bqt3b',
    data: [{
      13: { value: data.projectId },                    // Related Project
      15: { value: data.note },                         // Communication Note
      64: { value: data.pcId },                         // Related Project Coordinator (optional)
      167: { value: data.pcUserId },                    // PC User (optional)
      135: { value: data.isNEMBlocker || false }        // NEM Blocker Outreach (optional)
    }]
  });

  return response.data;
}

// ============================================================================
// PROJECT UPDATES
// ============================================================================

/**
 * Set project as unresponsive
 */
async function markProjectUnresponsive(projectId, isUnresponsive = true) {
  const response = await qbRequest('/v1/records', 'POST', {
    to: 'br9kwm8na',
    data: [{
      3: { value: projectId },
      1909: { value: isUnresponsive ? 'Yes' : 'No' }    // Is Unresponsive?
    }],
    mergeFieldId: 3
  });

  return response.data;
}

/**
 * Set preferred outreach method
 */
async function setPreferredOutreachMethod(projectId, methods) {
  // methods should be array like ['Text', 'Email', 'Call']
  const response = await qbRequest('/v1/records', 'POST', {
    to: 'br9kwm8na',
    data: [{
      3: { value: projectId },
      2086: { value: methods }                          // Manual Preferred Outreach
    }],
    mergeFieldId: 3
  });

  return response.data;
}

/**
 * Set PC check-in flags
 */
async function setCheckInFlags(projectId, createCheckIn, incompleteCheckIn) {
  const response = await qbRequest('/v1/records', 'POST', {
    to: 'br9kwm8na',
    data: [{
      3: { value: projectId },
      2287: { value: createCheckIn },                   // PC Outreach: Create Check-In?
      2266: { value: incompleteCheckIn }                // PC Outreach: Incomplete Check-In?
    }],
    mergeFieldId: 3
  });

  return response.data;
}

// ============================================================================
// USAGE EXAMPLES
// ============================================================================

async function exampleUsage() {
  console.log('='.repeat(80));
  console.log('PC DASHBOARD API QUERIES - EXAMPLE USAGE');
  console.log('='.repeat(80));
  console.log('\n');

  // Example 1: Get Emma's initial outreach projects
  console.log('1. Getting Initial Outreach for Emma Martin...\n');
  const initialOutreach = await getInitialOutreachProjects('Emma Martin');
  console.log(`   Found ${initialOutreach.length} projects needing outreach\n`);

  // Example 2: Get pending escalations
  console.log('2. Getting Pending Escalations for Emma Martin...\n');
  const escalations = await getPendingEscalations('Emma Martin');
  console.log(`   Found ${escalations.length} escalated projects\n`);

  // Example 3: Get unresponsive customers
  console.log('3. Getting Unresponsive Customers for Emma Martin...\n');
  const unresponsive = await getUnresponsiveCustomers('Emma Martin');
  console.log(`   Found ${unresponsive.length} unresponsive projects\n`);

  // Example 4: Get project details
  if (initialOutreach.length > 0) {
    const firstProject = initialOutreach[0];
    const projectId = firstProject['3'].value;

    console.log(`4. Getting details for Project ${projectId}...\n`);
    const details = await getProjectDetails(projectId);
    console.log(`   Customer: ${details['145']?.value || details['6']?.value}`);
    console.log(`   Status: ${details['255']?.value}`);
    console.log(`   Due Outreach: ${details['2268']?.value}`);
    console.log(`   Preferred Method: ${details['2404']?.value || 'Not set'}\n`);

    // Example 5: Get outreach records for this project
    console.log(`5. Getting outreach records for Project ${projectId}...\n`);
    const outreachRecords = await getProjectOutreachRecords(projectId);
    console.log(`   Found ${outreachRecords.length} outreach records\n`);
    if (outreachRecords.length > 0) {
      const latest = outreachRecords[0];
      console.log(`   Latest: ${latest['6']?.value} - ${latest['43']?.value || 'Pending'}\n`);
    }
  }

  // Example 6: Get PC project counts
  console.log('6. Getting project counts for Emma Martin...\n');
  const counts = await getPCProjectCountsByStatus('Emma Martin');
  console.log('   Status breakdown:');
  Object.entries(counts).forEach(([status, count]) => {
    console.log(`   ${status}: ${count}`);
  });
  console.log('\n');

  console.log('='.repeat(80));
  console.log('EXAMPLE COMPLETE');
  console.log('='.repeat(80));
}

// ============================================================================
// EXPORTS
// ============================================================================

module.exports = {
  // Dashboard queries
  getInitialOutreachProjects,
  getUnresponsiveCustomers,
  getPendingEscalations,
  getPendingEscalationsViaProjects,
  getBlockedNEM,

  // Outreach management
  getProjectOutreachRecords,
  getDueOutreach,
  createOutreachRecord,
  completeOutreach,
  snoozeOutreach,

  // Sales aid / escalation
  getProjectSalesAidRequests,
  createSalesAidRequest,
  escalateSalesAidRequest,
  completeSalesAidRequest,

  // Project queries
  getProjectDetails,
  getPCActiveProjects,
  getPCProjectCountsByStatus,

  // Communications
  getProjectCommunications,
  createCommunication,

  // Project updates
  markProjectUnresponsive,
  setPreferredOutreachMethod,
  setCheckInFlags,

  // Run examples
  exampleUsage
};

// Run example if executed directly
if (require.main === module) {
  exampleUsage().catch(console.error);
}
