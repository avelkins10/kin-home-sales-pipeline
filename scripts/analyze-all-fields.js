#!/usr/bin/env node
/**
 * Complete Field Analysis Script
 * Analyzes ALL 2000+ fields in Quickbase Projects table
 *
 * Outputs:
 * 1. Field usage percentages (based on last 500 records)
 * 2. Field categorization (core, milestone, metadata, calculated, etc.)
 * 3. Recommendations (must-include, nice-to-have, skip)
 * 4. Data quality analysis (null rates, data types, samples)
 */

const https = require('https');
const fs = require('fs');

// ⚠️ UPDATE THIS TOKEN - Generate fresh token at:
// https://kin.quickbase.com → My Preferences → My User Token
const QB_USER_TOKEN = 'YOUR_TOKEN_HERE';
const QB_REALM = 'kin.quickbase.com';
const PROJECTS_TABLE_ID = 'br9kwm8na';
const SAMPLE_SIZE = 500; // Last 500 records

// Make Quickbase API request
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
        if (res.statusCode >= 400) {
          reject(new Error(`HTTP ${res.statusCode}: ${data}`));
        } else {
          try {
            resolve(JSON.parse(data));
          } catch (e) {
            reject(new Error(`Failed to parse JSON: ${e.message}`));
          }
        }
      });
    });

    req.on('error', reject);
    if (body) req.write(JSON.stringify(body));
    req.end();
  });
}

// Get all field metadata
async function getAllFields() {
  console.log('📋 Fetching all field metadata...\n');
  const response = await qbRequest(`/v1/fields?tableId=${PROJECTS_TABLE_ID}`, 'GET');
  return response;
}

// Get sample records with ALL fields
async function getSampleRecords(fieldIds) {
  console.log(`📊 Fetching ${SAMPLE_SIZE} most recent records...\n`);

  // Quickbase has a limit on select array size, so we'll fetch in batches
  const batchSize = 300;
  const batches = [];

  for (let i = 0; i < fieldIds.length; i += batchSize) {
    const batchFields = fieldIds.slice(i, i + batchSize);
    batches.push(batchFields);
  }

  console.log(`Fetching in ${batches.length} batches (${batchSize} fields each)...\n`);

  const allRecords = [];

  for (let i = 0; i < batches.length; i++) {
    console.log(`Batch ${i + 1}/${batches.length}...`);

    const response = await qbRequest('/v1/records/query', 'POST', {
      from: PROJECTS_TABLE_ID,
      select: batches[i],
      options: {
        top: SAMPLE_SIZE,
        skip: 0
      }
    });

    // Merge records
    if (i === 0) {
      allRecords.push(...response.data);
    } else {
      // Merge fields into existing records
      response.data.forEach((record, idx) => {
        Object.assign(allRecords[idx], record);
      });
    }

    // Rate limiting - wait 100ms between requests
    await new Promise(resolve => setTimeout(resolve, 100));
  }

  return allRecords;
}

// Analyze field usage
function analyzeFieldUsage(fields, records) {
  console.log('\n🔍 Analyzing field usage across 500 records...\n');

  const analysis = fields.map(field => {
    const fieldId = field.id;

    // Count non-null values
    let filledCount = 0;
    const samples = [];

    records.forEach(record => {
      const value = record[fieldId]?.value;

      if (value !== null && value !== undefined && value !== '') {
        filledCount++;

        // Collect sample values (max 5)
        if (samples.length < 5) {
          if (typeof value === 'object') {
            samples.push(JSON.stringify(value));
          } else {
            samples.push(String(value).substring(0, 100)); // Truncate long values
          }
        }
      }
    });

    const usagePercent = ((filledCount / records.length) * 100).toFixed(1);

    return {
      id: fieldId,
      label: field.label,
      type: field.fieldType,
      usagePercent: parseFloat(usagePercent),
      filledCount,
      totalCount: records.length,
      samples: samples.slice(0, 3), // Top 3 samples

      // Metadata
      required: field.required || false,
      unique: field.unique || false,
      properties: field.properties || {}
    };
  });

  return analysis;
}

// Categorize fields
function categorizeFields(analysis) {
  console.log('\n📂 Categorizing fields...\n');

  const categories = {
    // System/Core (always filled)
    system: [],

    // Customer/Project Info
    customer: [],

    // Team/User Fields
    team: [],

    // Milestone Dates (critical for timeline)
    milestone: [],

    // Financial/Pricing
    financial: [],

    // Adders
    adders: [],

    // Holds/Blockers
    holds: [],

    // Tasks/Workflow
    tasks: [],

    // Calculated/Formula Fields
    calculated: [],

    // Metadata/Audit
    metadata: [],

    // Legacy/Unused
    legacy: [],

    // Unknown/Other
    other: []
  };

  analysis.forEach(field => {
    const label = field.label.toLowerCase();
    const type = field.type;

    // Categorization rules
    if (['recordid', 'date created', 'date modified', 'record owner'].some(s => label.includes(s))) {
      categories.system.push(field);
    } else if (label.includes('customer') || label.includes('address') || label.includes('phone')) {
      categories.customer.push(field);
    } else if (label.includes('closer') || label.includes('setter') || label.includes('coordinator') || label.includes('owner')) {
      categories.team.push(field);
    } else if (label.includes('date') || label.includes('scheduled') || label.includes('completed') || label.includes('submitted') || label.includes('approved')) {
      categories.milestone.push(field);
    } else if (label.includes('price') || label.includes('ppw') || label.includes('cost') || label.includes('commission') || label.includes('funding') || label.includes('payment')) {
      categories.financial.push(field);
    } else if (label.includes('adder')) {
      categories.adders.push(field);
    } else if (label.includes('hold') || label.includes('block') || label.includes('reason')) {
      categories.holds.push(field);
    } else if (label.includes('task')) {
      categories.tasks.push(field);
    } else if (type === 'formula' || type === 'summary' || label.includes('total') || label.includes('count') || label.includes('average') || label.includes('maximum') || label.includes('minimum')) {
      categories.calculated.push(field);
    } else if (label.includes('created') || label.includes('modified') || label.includes('log') || label.includes('audit') || label.includes('trigger') || label.includes('webhook')) {
      categories.metadata.push(field);
    } else if (field.usagePercent < 1 && !field.required) {
      categories.legacy.push(field);
    } else {
      categories.other.push(field);
    }
  });

  return categories;
}

// Generate recommendations
function generateRecommendations(analysis, categories) {
  console.log('\n💡 Generating recommendations...\n');

  const recommendations = {
    mustInclude: [],      // ≥50% usage OR critical milestone
    recommended: [],      // 10-50% usage
    optional: [],         // 5-10% usage, may be useful
    skip: []             // <5% usage, legacy, or redundant
  };

  analysis.forEach(field => {
    const label = field.label.toLowerCase();
    const usage = field.usagePercent;

    // Critical milestones (even if low usage)
    const isCriticalMilestone = [
      'pto', 'inspection', 'install completed', 'permit approved', 'nem approved',
      'hoa approved', 'survey approved', 'design completed'
    ].some(keyword => label.includes(keyword));

    // Core fields
    const isCoreField = [
      'record id', 'project id', 'customer name', 'project status', 'sales date',
      'closer', 'setter', 'coordinator', 'system size', 'system price'
    ].some(keyword => label === keyword || label.includes(keyword));

    // Recommendation logic
    if (usage >= 50 || isCoreField) {
      recommendations.mustInclude.push({
        ...field,
        reason: usage >= 50 ? `High usage (${usage}%)` : 'Core field'
      });
    } else if (usage >= 10 || isCriticalMilestone) {
      recommendations.recommended.push({
        ...field,
        reason: isCriticalMilestone ? 'Critical milestone (low usage is normal)' : `Good usage (${usage}%)`
      });
    } else if (usage >= 5) {
      recommendations.optional.push({
        ...field,
        reason: `Moderate usage (${usage}%) - edge cases`
      });
    } else {
      recommendations.skip.push({
        ...field,
        reason: `Low usage (${usage}%) - likely legacy/unused`
      });
    }
  });

  return recommendations;
}

// Generate markdown report
function generateReport(analysis, categories, recommendations) {
  console.log('\n📝 Generating comprehensive report...\n');

  const timestamp = new Date().toISOString().split('T')[0];

  let report = `# Quickbase Field Analysis - Complete Report\n\n`;
  report += `**Generated:** ${timestamp}\n`;
  report += `**Table:** Projects (${PROJECTS_TABLE_ID})\n`;
  report += `**Sample Size:** ${SAMPLE_SIZE} most recent records\n`;
  report += `**Total Fields Analyzed:** ${analysis.length}\n\n`;
  report += `---\n\n`;

  // Executive Summary
  report += `## Executive Summary\n\n`;
  report += `### Field Count by Category\n\n`;
  report += `| Category | Count | Avg Usage % |\n`;
  report += `|----------|-------|-------------|\n`;

  Object.entries(categories).forEach(([category, fields]) => {
    const avgUsage = fields.length > 0
      ? (fields.reduce((sum, f) => sum + f.usagePercent, 0) / fields.length).toFixed(1)
      : '0.0';
    report += `| ${category.charAt(0).toUpperCase() + category.slice(1)} | ${fields.length} | ${avgUsage}% |\n`;
  });

  report += `\n### Recommendations Summary\n\n`;
  report += `- **Must Include:** ${recommendations.mustInclude.length} fields (≥50% usage or core fields)\n`;
  report += `- **Recommended:** ${recommendations.recommended.length} fields (10-50% usage or critical milestones)\n`;
  report += `- **Optional:** ${recommendations.optional.length} fields (5-10% usage, edge cases)\n`;
  report += `- **Skip:** ${recommendations.skip.length} fields (<5% usage, legacy)\n\n`;

  report += `---\n\n`;

  // Must Include Fields
  report += `## Must Include Fields (${recommendations.mustInclude.length})\n\n`;
  report += `**These are essential for the dashboard - high usage or core functionality.**\n\n`;

  recommendations.mustInclude
    .sort((a, b) => b.usagePercent - a.usagePercent)
    .forEach(field => {
      report += `### ${field.label} (Field ${field.id})\n`;
      report += `- **Type:** ${field.type}\n`;
      report += `- **Usage:** ${field.usagePercent}% (${field.filledCount}/${field.totalCount} records)\n`;
      report += `- **Why:** ${field.reason}\n`;
      if (field.samples.length > 0) {
        report += `- **Samples:** ${field.samples.join(', ')}\n`;
      }
      report += `\n`;
    });

  report += `---\n\n`;

  // Recommended Fields
  report += `## Recommended Fields (${recommendations.recommended.length})\n\n`;
  report += `**Include these for complete milestone tracking and functionality.**\n\n`;

  recommendations.recommended
    .sort((a, b) => b.usagePercent - a.usagePercent)
    .forEach(field => {
      report += `### ${field.label} (Field ${field.id})\n`;
      report += `- **Type:** ${field.type}\n`;
      report += `- **Usage:** ${field.usagePercent}% (${field.filledCount}/${field.totalCount} records)\n`;
      report += `- **Why:** ${field.reason}\n`;
      if (field.samples.length > 0) {
        report += `- **Samples:** ${field.samples.join(', ')}\n`;
      }
      report += `\n`;
    });

  report += `---\n\n`;

  // Category Breakdown
  report += `## Fields by Category\n\n`;

  Object.entries(categories).forEach(([category, fields]) => {
    if (fields.length === 0) return;

    report += `### ${category.charAt(0).toUpperCase() + category.slice(1)} (${fields.length} fields)\n\n`;

    fields
      .sort((a, b) => b.usagePercent - a.usagePercent)
      .forEach(field => {
        report += `- **${field.label}** (${field.id}): ${field.usagePercent}% usage\n`;
      });

    report += `\n`;
  });

  report += `---\n\n`;

  // Legacy/Skip Fields
  report += `## Fields to Skip (${recommendations.skip.length})\n\n`;
  report += `**Low usage (<5%) - likely legacy, test, or redundant fields.**\n\n`;

  const skipSample = recommendations.skip.slice(0, 50); // Show first 50
  skipSample.forEach(field => {
    report += `- ${field.label} (${field.id}): ${field.usagePercent}% usage\n`;
  });

  if (recommendations.skip.length > 50) {
    report += `\n*... and ${recommendations.skip.length - 50} more*\n`;
  }

  return report;
}

// Main execution
async function main() {
  try {
    console.log('🚀 Starting comprehensive field analysis...\n');
    console.log(`Table: ${PROJECTS_TABLE_ID}`);
    console.log(`Sample Size: ${SAMPLE_SIZE} records\n`);
    console.log('='.repeat(60) + '\n');

    // Step 1: Get all fields
    const fieldsResponse = await getAllFields();
    const fields = fieldsResponse;
    console.log(`✅ Found ${fields.length} total fields\n`);

    // Step 2: Get field IDs
    const fieldIds = fields.map(f => f.id);

    // Step 3: Fetch sample records
    const records = await getSampleRecords(fieldIds);
    console.log(`✅ Fetched ${records.length} sample records\n`);

    // Step 4: Analyze usage
    const analysis = analyzeFieldUsage(fields, records);

    // Step 5: Categorize
    const categories = categorizeFields(analysis);

    // Step 6: Generate recommendations
    const recommendations = generateRecommendations(analysis, categories);

    // Step 7: Generate report
    const report = generateReport(analysis, categories, recommendations);

    // Step 8: Save outputs
    const outputDir = '/Users/austinelkins/Rep Dashboard/analysis-output';

    // Create directory if it doesn't exist
    if (!fs.existsSync(outputDir)) {
      fs.mkdirSync(outputDir, { recursive: true });
    }

    // Save JSON data
    const jsonOutput = {
      metadata: {
        generatedAt: new Date().toISOString(),
        tableId: PROJECTS_TABLE_ID,
        sampleSize: SAMPLE_SIZE,
        totalFields: fields.length
      },
      analysis,
      categories,
      recommendations
    };

    fs.writeFileSync(`${outputDir}/complete-field-analysis.json`, JSON.stringify(jsonOutput, null, 2));
    console.log(`✅ Saved JSON: ${outputDir}/complete-field-analysis.json`);

    // Save markdown report
    fs.writeFileSync(`${outputDir}/COMPLETE-FIELD-ANALYSIS.md`, report);
    console.log(`✅ Saved Report: ${outputDir}/COMPLETE-FIELD-ANALYSIS.md`);

    // Save simplified CSV for quick reference
    const csv = [
      'Field ID,Label,Type,Usage %,Filled Count,Recommendation'
    ];

    analysis
      .sort((a, b) => b.usagePercent - a.usagePercent)
      .forEach(field => {
        let rec = 'skip';
        if (recommendations.mustInclude.find(f => f.id === field.id)) rec = 'must-include';
        else if (recommendations.recommended.find(f => f.id === field.id)) rec = 'recommended';
        else if (recommendations.optional.find(f => f.id === field.id)) rec = 'optional';

        csv.push(`${field.id},"${field.label}",${field.type},${field.usagePercent},${field.filledCount},${rec}`);
      });

    fs.writeFileSync(`${outputDir}/field-analysis.csv`, csv.join('\n'));
    console.log(`✅ Saved CSV: ${outputDir}/field-analysis.csv`);

    console.log('\n' + '='.repeat(60));
    console.log('\n✅ Analysis Complete!\n');
    console.log('Summary:');
    console.log(`  - Total Fields: ${fields.length}`);
    console.log(`  - Must Include: ${recommendations.mustInclude.length}`);
    console.log(`  - Recommended: ${recommendations.recommended.length}`);
    console.log(`  - Optional: ${recommendations.optional.length}`);
    console.log(`  - Skip: ${recommendations.skip.length}`);
    console.log(`\nView report at: ${outputDir}/COMPLETE-FIELD-ANALYSIS.md\n`);

  } catch (error) {
    console.error('\n❌ Error:', error.message);

    if (error.message.includes('401')) {
      console.error('\n⚠️  Token expired or invalid!');
      console.error('Generate a fresh token at: https://kin.quickbase.com → My Preferences → My User Token');
      console.error('Then update QB_USER_TOKEN in this script.\n');
    }

    process.exit(1);
  }
}

// Run it
main();
