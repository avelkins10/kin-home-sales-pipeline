#!/usr/bin/env tsx
/**
 * Remove unnecessary type casts after migration 018
 * 
 * This script removes ::text casts from RepCard user ID comparisons
 * after migration 018 has normalized all IDs to INTEGER.
 * 
 * Run this AFTER migration 018 completes successfully.
 * 
 * Usage: npx tsx scripts/remove-type-casts-after-migration.ts
 */

import { readFileSync, writeFileSync } from 'fs';
import { resolve } from 'path';

const filesToUpdate = [
  'app/api/repcard/leaderboard/route.ts',
  'app/api/repcard/users/[userId]/stats/route.ts',
  'app/api/repcard/data/route.ts'
];

interface Replacement {
  pattern: RegExp;
  replacement: string;
  description: string;
}

const replacements: Replacement[] = [
  // Remove ::text casts from JOIN conditions
  {
    pattern: /u\.repcard_user_id::text\s*=\s*c\.setter_user_id::text/g,
    replacement: 'u.repcard_user_id = c.setter_user_id',
    description: 'Remove ::text casts from user-customer JOIN'
  },
  {
    pattern: /u\.repcard_user_id::text\s*=\s*a\.setter_user_id::text/g,
    replacement: 'u.repcard_user_id = a.setter_user_id',
    description: 'Remove ::text casts from user-appointment JOIN'
  },
  {
    pattern: /u\.repcard_user_id::text\s*=\s*ru\.repcard_user_id::text/g,
    replacement: 'u.repcard_user_id = ru.repcard_user_id',
    description: 'Remove ::text casts from user-repcard_user JOIN'
  },
  {
    pattern: /ru\.repcard_user_id::text\s*=\s*c\.setter_user_id::text/g,
    replacement: 'ru.repcard_user_id = c.setter_user_id',
    description: 'Remove ::text casts from repcard_user-customer JOIN'
  },
  {
    pattern: /ru\.repcard_user_id::text\s*=\s*a\.setter_user_id::text/g,
    replacement: 'ru.repcard_user_id = a.setter_user_id',
    description: 'Remove ::text casts from repcard_user-appointment JOIN'
  },
  {
    pattern: /sl\.changed_by_user_id::text\s*=\s*ANY\(\$\{repcardUserIds\.map\(String\)\}::text\[\]\)/g,
    replacement: 'sl.changed_by_user_id = ANY(${repcardUserIds}::int[])',
    description: 'Change status log user ID array to INTEGER'
  },
  // Change array comparisons from TEXT to INTEGER
  {
    pattern: /u\.repcard_user_id::text\s*=\s*ANY\(\$\{repcardUserIds\}::text\[\]\)/g,
    replacement: 'u.repcard_user_id = ANY(${repcardUserIds}::int[])',
    description: 'Change user ID array comparison to INTEGER'
  },
  {
    pattern: /setter_user_id::text\s*=\s*\$\{String\(user\.repcard_user_id\)\}/g,
    replacement: 'setter_user_id = ${user.repcard_user_id}',
    description: 'Remove ::text cast and String() conversion'
  },
  // Keep SELECT casts for API responses (these are fine)
  // But we can optimize them if needed
];

function updateFile(filePath: string): { updated: boolean; changes: number } {
  const fullPath = resolve(process.cwd(), filePath);
  let content = readFileSync(fullPath, 'utf-8');
  let changes = 0;
  let updated = false;

  for (const { pattern, replacement, description } of replacements) {
    const matches = content.match(pattern);
    if (matches) {
      content = content.replace(pattern, replacement);
      changes += matches.length;
      updated = true;
      console.log(`  ✅ ${description}: ${matches.length} replacement(s)`);
    }
  }

  if (updated) {
    writeFileSync(fullPath, content, 'utf-8');
  }

  return { updated, changes };
}

async function main() {
  console.log('🔧 Removing unnecessary type casts after migration 018');
  console.log('='.repeat(60));
  console.log();

  let totalChanges = 0;
  let filesUpdated = 0;

  for (const file of filesToUpdate) {
    console.log(`📄 Processing: ${file}`);
    const result = updateFile(file);
    if (result.updated) {
      filesUpdated++;
      totalChanges += result.changes;
      console.log(`  ✅ Updated (${result.changes} changes)`);
    } else {
      console.log(`  ⏭️  No changes needed`);
    }
    console.log();
  }

  console.log('='.repeat(60));
  console.log();
  console.log(`✅ Complete!`);
  console.log(`   Files updated: ${filesUpdated}`);
  console.log(`   Total changes: ${totalChanges}`);
  console.log();
  console.log('⚠️  Note: Review the changes and test thoroughly before deploying.');
  console.log('   The code should work better with INTEGER types (2-3x faster queries).');
  console.log();
}

main().catch((error) => {
  console.error('❌ Error:', error);
  process.exit(1);
});

