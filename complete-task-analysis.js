const https = require('https');
const fs = require('fs');

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

async function main() {
  console.log('╔' + '═'.repeat(78) + '╗');
  console.log('║' + ' '.repeat(25) + 'COMPLETE TASK ANALYSIS' + ' '.repeat(30) + '║');
  console.log('╚' + '═'.repeat(78) + '╝\n');

  const results = {
    taskTemplates: [],
    projectsToRepsMapping: null,
    sampleDataByType: {}
  };

  // ============================================================================
  // PART 1: Get All 38 Task Templates
  // ============================================================================
  console.log('\n' + '='.repeat(80));
  console.log('PART 1: Querying All 38 Task Templates');
  console.log('='.repeat(80) + '\n');

  const templatesResponse = await qbRequest('/v1/records/query', 'POST', {
    from: 'bu36jyuf9', // Sales Task Templates
    select: [3, 6, 9, 10, 18, 19, 20], // ID, Name, Category, Missing Item, Description, Field Maps
    options: { top: 50 }
  });

  if (templatesResponse.statusCode === 200 && templatesResponse.data.data) {
    results.taskTemplates = templatesResponse.data.data;
    console.log(`✅ Retrieved ${results.taskTemplates.length} task templates\n`);

    // Group by category
    const byCategory = {};
    results.taskTemplates.forEach(template => {
      const category = template['9']?.value || 'Unknown';
      if (!byCategory[category]) byCategory[category] = [];
      byCategory[category].push(template);
    });

    console.log('Task Templates by Category:\n');
    Object.keys(byCategory).sort().forEach(category => {
      console.log(`\n📁 ${category} (${byCategory[category].length} templates):`);
      byCategory[category].forEach((template, idx) => {
        const name = template['6']?.value || 'Unknown';
        const missingItem = template['10']?.value || '';
        const description = template['18']?.value || '';
        console.log(`\n  ${idx + 1}. ${name}`);
        if (missingItem) console.log(`     Missing Item: ${missingItem}`);
        if (description) {
          const desc = description.substring(0, 150);
          console.log(`     Description: ${desc}${description.length > 150 ? '...' : ''}`);
        }
      });
    });
  } else {
    console.log(`❌ Failed to get templates: ${templatesResponse.statusCode}`);
    console.log(JSON.stringify(templatesResponse.data).substring(0, 200));
  }

  // ============================================================================
  // PART 2: Find Projects to Sales Reps Mapping
  // ============================================================================
  console.log('\n\n' + '='.repeat(80));
  console.log('PART 2: Finding Projects to Sales Reps Mapping');
  console.log('='.repeat(80) + '\n');

  console.log('Step 1: Getting Projects table field list...\n');
  const projectFieldsResponse = await qbRequest('/v1/fields?tableId=br9kwm8na', 'GET');

  let repFields = [];
  if (projectFieldsResponse.statusCode === 200) {
    const fields = Array.isArray(projectFieldsResponse.data)
      ? projectFieldsResponse.data
      : (projectFieldsResponse.data.fields || []);

    // Look for fields related to reps/closers/sales
    repFields = fields.filter(f => {
      const label = f.label.toLowerCase();
      return (
        label.includes('rep') ||
        label.includes('closer') ||
        label.includes('sales') ||
        label.includes('owner') ||
        label.includes('assigned')
      ) && (
        label.includes('email') ||
        label.includes('name') ||
        label.includes('user') ||
        !label.includes('date')
      );
    });

    console.log(`Found ${repFields.length} potential rep-related fields:\n`);
    repFields.forEach(f => {
      console.log(`  [${f.id}] ${f.label} (${f.fieldType})`);
    });
  }

  console.log('\n\nStep 2: Querying sample projects to see rep data...\n');

  // Get field IDs to query
  const projectFieldIds = [
    3,    // Record ID
    255,  // Status
    ...repFields.slice(0, 15).map(f => f.id)
  ];

  const projectsResponse = await qbRequest('/v1/records/query', 'POST', {
    from: 'br9kwm8na', // Projects
    select: projectFieldIds,
    where: '{255.EX."Active"}',
    options: { top: 5 }
  });

  if (projectsResponse.statusCode === 200 && projectsResponse.data.data) {
    console.log(`✅ Retrieved ${projectsResponse.data.data.length} sample projects\n`);

    projectsResponse.data.data.forEach((project, idx) => {
      console.log(`\nProject ${idx + 1} (ID: ${project['3']?.value}):`);
      repFields.forEach(field => {
        const value = project[field.id]?.value;
        if (value !== null && value !== undefined && value !== '') {
          const displayValue = typeof value === 'object' ? JSON.stringify(value) : value;
          console.log(`  [${field.id}] ${field.label}: ${displayValue.toString().substring(0, 100)}`);
        }
      });
    });

    results.projectsToRepsMapping = {
      potentialFields: repFields.map(f => ({
        id: f.id,
        label: f.label,
        type: f.fieldType
      })),
      sampleData: projectsResponse.data.data
    };
  }

  // ============================================================================
  // PART 3: Get Sample Data for Each Task Type
  // ============================================================================
  console.log('\n\n' + '='.repeat(80));
  console.log('PART 3: Getting Sample Data for Each Task Type in Production');
  console.log('='.repeat(80) + '\n');

  // Get all tasks with their details
  const tasksResponse = await qbRequest('/v1/records/query', 'POST', {
    from: 'bu36ggiht', // Sales Tasks
    select: [3, 6, 9, 10, 11, 17, 27, 30, 31, 36, 37], // Key fields
    options: { top: 100 }
  });

  if (tasksResponse.statusCode === 200 && tasksResponse.data.data) {
    console.log(`✅ Retrieved ${tasksResponse.data.data.length} tasks\n`);

    // Group by task name/type
    const tasksByType = {};
    tasksResponse.data.data.forEach(task => {
      const taskName = task['10']?.value || task['17']?.value || 'Unknown';
      if (!tasksByType[taskName]) {
        tasksByType[taskName] = [];
      }
      tasksByType[taskName].push(task);
    });

    console.log(`Found ${Object.keys(tasksByType).length} unique task types in production:\n`);

    for (const [taskName, tasks] of Object.entries(tasksByType)) {
      console.log(`\n📋 ${taskName}`);
      console.log(`   Count: ${tasks.length} task(s)`);

      // Show first example
      const sample = tasks[0];
      console.log(`   Status: ${sample['9']?.value || 'N/A'}`);
      console.log(`   Category: ${sample['30']?.value || sample['27']?.value || 'N/A'}`);
      console.log(`   Missing Item: ${sample['31']?.value || 'N/A'}`);
      console.log(`   # of Submissions: ${sample['37']?.value || 0}`);
      console.log(`   # of Open Submissions: ${sample['36']?.value || 0}`);

      // Get submissions for this task type (from first example)
      const taskId = sample['3']?.value;
      if (taskId) {
        const submissionsResponse = await qbRequest('/v1/records/query', 'POST', {
          from: 'bu36g8j99', // Sales Task Submissions
          select: [1, 3, 7, 8, 24], // Date, ID, Status, Disposition, Note
          where: `{6.EX.${taskId}}`,
          options: { top: 3 }
        });

        if (submissionsResponse.statusCode === 200 && submissionsResponse.data.data) {
          const subs = submissionsResponse.data.data;
          if (subs.length > 0) {
            console.log(`   Submissions: ${subs.length} found`);
            subs.forEach((sub, idx) => {
              console.log(`     ${idx + 1}. ${sub['7']?.value || 'N/A'} - ${sub['8']?.value || 'N/A'}`);
              if (sub['24']?.value) {
                console.log(`        Note: "${sub['24'].value.substring(0, 80)}"`);
              }
            });
          }
        }

        await new Promise(resolve => setTimeout(resolve, 200)); // Rate limit
      }

      results.sampleDataByType[taskName] = {
        count: tasks.length,
        sample: sample,
        statuses: [...new Set(tasks.map(t => t['9']?.value).filter(Boolean))]
      };
    }
  }

  // ============================================================================
  // Save Results
  // ============================================================================
  console.log('\n\n' + '='.repeat(80));
  console.log('Saving Results...');
  console.log('='.repeat(80) + '\n');

  const outputPath = '/Users/austinelkins/quickbase-complete-data/complete-task-analysis.json';
  fs.writeFileSync(outputPath, JSON.stringify(results, null, 2));
  console.log(`✅ Full results saved to: ${outputPath}`);

  // ============================================================================
  // Generate Comprehensive Markdown Report
  // ============================================================================
  let markdown = `# Complete Task System Analysis\n\n`;
  markdown += `**Generated**: ${new Date().toISOString()}\n`;
  markdown += `**API Realm**: ${QB_REALM}\n\n`;
  markdown += `---\n\n`;

  // Part 1: Task Templates
  markdown += `## Part 1: All ${results.taskTemplates.length} Task Templates\n\n`;

  const byCategory = {};
  results.taskTemplates.forEach(template => {
    const category = template['9']?.value || 'Unknown';
    if (!byCategory[category]) byCategory[category] = [];
    byCategory[category].push(template);
  });

  Object.keys(byCategory).sort().forEach(category => {
    markdown += `### ${category}\n\n`;
    markdown += `| ID | Task Name | Missing Item | Description |\n`;
    markdown += `|----|-----------|--------------|-------------|\n`;

    byCategory[category].forEach(template => {
      const id = template['3']?.value || '';
      const name = template['6']?.value || 'Unknown';
      const missingItem = template['10']?.value || '';
      const description = (template['18']?.value || '').substring(0, 100);

      markdown += `| ${id} | ${name} | ${missingItem} | ${description}${description.length >= 100 ? '...' : ''} |\n`;
    });
    markdown += `\n`;
  });

  // Part 2: Projects to Reps Mapping
  markdown += `## Part 2: Projects to Sales Reps Mapping\n\n`;
  markdown += `### Potential Rep Fields in Projects Table\n\n`;
  markdown += `| Field ID | Label | Type |\n`;
  markdown += `|----------|-------|------|\n`;

  if (results.projectsToRepsMapping?.potentialFields) {
    results.projectsToRepsMapping.potentialFields.forEach(field => {
      markdown += `| ${field.id} | ${field.label} | ${field.type} |\n`;
    });
  }
  markdown += `\n`;

  markdown += `### Recommendation\n\n`;
  markdown += `Based on the data, the most likely fields for identifying the sales rep are:\n\n`;
  if (results.projectsToRepsMapping?.potentialFields) {
    const likelyFields = results.projectsToRepsMapping.potentialFields.filter(f =>
      f.label.toLowerCase().includes('closer') && f.label.toLowerCase().includes('email')
    );
    likelyFields.forEach(f => {
      markdown += `- **Field ${f.id}**: ${f.label} (${f.type})\n`;
    });
  }
  markdown += `\n`;

  // Part 3: Sample Data by Type
  markdown += `## Part 3: Task Types in Production\n\n`;
  markdown += `Found ${Object.keys(results.sampleDataByType).length} unique task types being used.\n\n`;

  Object.entries(results.sampleDataByType).forEach(([taskName, data]) => {
    markdown += `### ${taskName}\n\n`;
    markdown += `- **Count**: ${data.count} task(s) in system\n`;
    markdown += `- **Statuses**: ${data.statuses.join(', ')}\n`;
    markdown += `- **Category**: ${data.sample['30']?.value || data.sample['27']?.value || 'N/A'}\n`;
    markdown += `- **# of Submissions**: ${data.sample['37']?.value || 0}\n`;
    markdown += `\n`;
  });

  // Summary
  markdown += `---\n\n`;
  markdown += `## Summary\n\n`;
  markdown += `- **Task Templates Defined**: ${results.taskTemplates.length}\n`;
  markdown += `- **Task Types in Production**: ${Object.keys(results.sampleDataByType).length}\n`;
  markdown += `- **Potential Rep Fields**: ${results.projectsToRepsMapping?.potentialFields?.length || 0}\n`;

  const mdPath = '/Users/austinelkins/COMPLETE_TASK_ANALYSIS_REPORT.md';
  fs.writeFileSync(mdPath, markdown);
  console.log(`✅ Markdown report saved to: ${mdPath}\n`);

  console.log('\n' + '='.repeat(80));
  console.log('✅ ANALYSIS COMPLETE!');
  console.log('='.repeat(80));
}

main().catch(console.error);
