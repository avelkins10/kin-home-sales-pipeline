#!/usr/bin/env node
/**
 * Generate Excel-friendly CSV for operations team
 * Shows all 2032 fields with recommendations for cleanup
 */

const fs = require('fs');

// Load existing analysis
const usageData = require('/Users/austinelkins/quickbase-complete-data/usage-analysis.json');

console.log('📊 Generating cleanup spreadsheet...\n');

// Convert to array and add recommendations
const fields = Object.values(usageData).map(field => {
  const usage = parseFloat(field.usagePercent);
  let recommendation = '';
  let priority = '';
  let reason = '';

  // Categorize
  if (usage === 0) {
    recommendation = 'DELETE';
    priority = 'HIGH';
    reason = 'Completely unused - no data in 500 records';
  } else if (usage < 1) {
    recommendation = 'REVIEW → LIKELY DELETE';
    priority = 'HIGH';
    reason = `Only ${field.populated}/${field.samples?.length || 0} records have data - likely legacy/test field`;
  } else if (usage < 5) {
    recommendation = 'REVIEW → POSSIBLY ARCHIVE';
    priority = 'MEDIUM';
    reason = 'Very low usage - verify if intentional edge case';
  } else if (usage < 10) {
    recommendation = 'REVIEW';
    priority = 'LOW';
    reason = 'Low usage - may be valid for specific scenarios';
  } else if (usage >= 10 && usage < 50) {
    recommendation = 'KEEP';
    priority = 'N/A';
    reason = 'Valid milestone or workflow field';
  } else {
    recommendation = 'KEEP';
    priority = 'N/A';
    reason = 'High usage - core field';
  }

  // Check for duplicate indicators
  const label = field.label.toLowerCase();
  let duplicateFlags = [];

  if (label.includes('maximum')) duplicateFlags.push('Possible duplicate (Maximum prefix)');
  if (label.includes('minimum')) duplicateFlags.push('Possible duplicate (Minimum prefix)');
  if (label.includes('capture')) duplicateFlags.push('May be duplicate capture field');
  if (label.includes('2.0') || label.includes('old') || label.includes('new')) {
    duplicateFlags.push('Version number in name - check for duplicates');
  }
  if (label.includes('import') || label.includes('backup')) {
    duplicateFlags.push('Import/backup field - may be outdated');
  }

  const duplicateWarning = duplicateFlags.length > 0 ? duplicateFlags.join('; ') : '';

  return {
    id: field.id,
    label: field.label,
    type: field.type,
    usagePercent: usage,
    populated: field.populated || 0,
    recommendation,
    priority,
    reason,
    duplicateWarning,
    samples: field.samples ? field.samples.slice(0, 2).join(' | ') : ''
  };
});

// Sort by priority (HIGH first) then by usage (lowest first)
const priorityOrder = { 'HIGH': 1, 'MEDIUM': 2, 'LOW': 3, 'N/A': 4 };
fields.sort((a, b) => {
  const priorityDiff = priorityOrder[a.priority] - priorityOrder[b.priority];
  if (priorityDiff !== 0) return priorityDiff;
  return a.usagePercent - b.usagePercent;
});

// Generate CSV
const csvRows = [
  // Header
  [
    'Priority',
    'Recommendation',
    'Field ID',
    'Field Label',
    'Type',
    'Usage %',
    'Records with Data',
    'Reason',
    'Duplicate Warning',
    'Sample Values'
  ].join(','),

  // Data rows
  ...fields.map(field => {
    return [
      field.priority,
      field.recommendation,
      field.id,
      `"${field.label.replace(/"/g, '""')}"`, // Escape quotes
      field.type,
      field.usagePercent.toFixed(1),
      field.populated,
      `"${field.reason.replace(/"/g, '""')}"`,
      `"${field.duplicateWarning.replace(/"/g, '""')}"`,
      `"${field.samples.replace(/"/g, '""')}"`
    ].join(',');
  })
];

const csv = csvRows.join('\n');

// Save CSV
const outputPath = '/Users/austinelkins/Rep Dashboard/analysis-output/quickbase-cleanup-plan.csv';
fs.writeFileSync(outputPath, csv);

console.log(`✅ Saved: ${outputPath}\n`);

// Generate summary stats
const stats = {
  total: fields.length,
  delete: fields.filter(f => f.recommendation === 'DELETE').length,
  reviewDelete: fields.filter(f => f.recommendation === 'REVIEW → LIKELY DELETE').length,
  reviewArchive: fields.filter(f => f.recommendation === 'REVIEW → POSSIBLY ARCHIVE').length,
  review: fields.filter(f => f.recommendation === 'REVIEW').length,
  keep: fields.filter(f => f.recommendation === 'KEEP').length,
};

console.log('📋 Summary Statistics:\n');
console.log(`Total Fields:                    ${stats.total}`);
console.log(`DELETE (0% usage):               ${stats.delete} (${((stats.delete/stats.total)*100).toFixed(1)}%)`);
console.log(`REVIEW → LIKELY DELETE (<1%):    ${stats.reviewDelete} (${((stats.reviewDelete/stats.total)*100).toFixed(1)}%)`);
console.log(`REVIEW → POSSIBLY ARCHIVE (1-5%): ${stats.reviewArchive} (${((stats.reviewArchive/stats.total)*100).toFixed(1)}%)`);
console.log(`REVIEW (5-10%):                  ${stats.review} (${((stats.review/stats.total)*100).toFixed(1)}%)`);
console.log(`KEEP (≥10%):                     ${stats.keep} (${((stats.keep/stats.total)*100).toFixed(1)}%)`);
console.log('');
console.log(`Potential for cleanup:           ${stats.delete + stats.reviewDelete} fields (${(((stats.delete + stats.reviewDelete)/stats.total)*100).toFixed(1)}%)`);
console.log(`Need operations review:          ${stats.reviewArchive + stats.review} fields (${(((stats.reviewArchive + stats.review)/stats.total)*100).toFixed(1)}%)`);
console.log('');

// Create separate sheets for each priority
const highPriority = fields.filter(f => f.priority === 'HIGH');
const mediumPriority = fields.filter(f => f.priority === 'MEDIUM');

// Save high-priority CSV
const highPriorityCSV = [
  csvRows[0], // Header
  ...highPriority.map(field => {
    return [
      field.priority,
      field.recommendation,
      field.id,
      `"${field.label.replace(/"/g, '""')}"`,
      field.type,
      field.usagePercent.toFixed(1),
      field.populated,
      `"${field.reason.replace(/"/g, '""')}"`,
      `"${field.duplicateWarning.replace(/"/g, '""')}"`,
      `"${field.samples.replace(/"/g, '""')}"`
    ].join(',');
  })
].join('\n');

const highPriorityPath = '/Users/austinelkins/Rep Dashboard/analysis-output/HIGH-PRIORITY-cleanup.csv';
fs.writeFileSync(highPriorityPath, highPriorityCSV);
console.log(`✅ Saved high-priority list: ${highPriorityPath}`);
console.log(`   (${highPriority.length} fields - immediate deletion candidates)\n`);

// Generate deletion script
const deleteFieldIds = fields
  .filter(f => f.recommendation === 'DELETE')
  .map(f => f.id);

const deletionScript = `# Quickbase Field Deletion Script
# Generated: ${new Date().toISOString()}
# Total fields to delete: ${deleteFieldIds.length}
#
# IMPORTANT: Review with IT team before running!
# Test in development environment first!

# Field IDs to delete (0% usage - completely unused):
FIELD_IDS=(${deleteFieldIds.join(' ')})

# Note: Use Quickbase API or UI to delete these fields
# This is a reference list only - implement deletion via your preferred method

echo "Fields to delete: ${deleteFieldIds.length}"
echo "Review the HIGH-PRIORITY-cleanup.csv file for details"
`;

const scriptPath = '/Users/austinelkins/Rep Dashboard/analysis-output/deletion-reference.sh';
fs.writeFileSync(scriptPath, deletionScript);
console.log(`✅ Saved deletion reference: ${scriptPath}\n`);

console.log('🎯 Next Steps:');
console.log('1. Open quickbase-cleanup-plan.csv in Excel/Google Sheets');
console.log('2. Review HIGH-PRIORITY-cleanup.csv with IT team');
console.log('3. Get stakeholder approval for deletions');
console.log('4. Delete unused fields in batches');
console.log('5. Monitor for any issues\n');
