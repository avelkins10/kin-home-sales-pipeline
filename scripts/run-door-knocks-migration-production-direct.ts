#!/usr/bin/env tsx
/**
 * Run Door Knocks Migration (036) in Production - Direct Approach
 * Uses Node.js to properly read .env.production and handle connection strings
 */

import { config } from 'dotenv';
import { resolve } from 'path';
import { readFileSync } from 'fs';
import { sql } from '@/lib/db/client';
import * as readline from 'readline';

// Load environment variables from .env.production
const envPath = resolve(process.cwd(), '.env.production');
config({ path: envPath });

// Ensure DATABASE_URL is mapped to POSTGRES_URL
if (process.env.DATABASE_URL && !process.env.POSTGRES_URL) {
  process.env.POSTGRES_URL = process.env.DATABASE_URL;
}

async function askQuestion(query: string): Promise<string> {
  const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout,
  });

  return new Promise((resolve) => {
    rl.question(query, (answer) => {
      rl.close();
      resolve(answer);
    });
  });
}

async function runMigration() {
  try {
    console.log('\n🚀 Door Knocks Migration (036) - Production');
    console.log('============================================');
    console.log('');

    // Check if DATABASE_URL is set
    if (!process.env.DATABASE_URL) {
      console.error('❌ DATABASE_URL not found in .env.production');
      console.error('Please run: vercel env pull .env.production --environment=production');
      process.exit(1);
    }

    // Show database info (without password)
    const dbUrl = process.env.DATABASE_URL;
    const dbInfo = dbUrl.replace(/:[^:@]*@/, ':***@');
    console.log('📊 Database:', dbInfo);
    console.log('');

    // Confirm
    const confirm = await askQuestion('⚠️  WARNING: This will run on PRODUCTION database. Continue? (type "yes"): ');
    if (confirm !== 'yes') {
      console.log('❌ Cancelled');
      process.exit(0);
    }

    console.log('');
    console.log('🚀 Running door knocks migration...');
    console.log('');

    // Read migration file
    const migrationFile = '036_create_repcard_door_knocks_table.sql';
    const migrationPath = resolve(process.cwd(), 'lib/db/migrations', migrationFile);
    const migrationSQL = readFileSync(migrationPath, 'utf-8');

    console.log(`📄 Running: ${migrationFile}`);

    // Execute migration using sql.query for raw SQL
    await (sql as any).query(migrationSQL);

    console.log(`   ✅ ${migrationFile} completed successfully\n`);
    console.log('='.repeat(70));
    console.log('');
    console.log('✅ Migration complete!');
    console.log('');
    console.log('📋 Next steps:');
    console.log('1. ✅ Migration complete');
    console.log('2. ⏭️  Wait for Vercel deployment (if not already deployed)');
    console.log('3. ⏭️  Run a full sync to extract door knocks from existing customer data');
    console.log('4. ⏭️  Verify dashboard shows accurate doors/hours metrics');
    console.log('');

    process.exit(0);
  } catch (error: any) {
    // Check if error is because table already exists
    if (
      error?.message?.includes('already exists') ||
      error?.code === '42P07' ||
      error?.code === '42710'
    ) {
      console.log(`   ⚠️  Migration - Table already exists (skipping)\n`);
      console.log('   This is safe to ignore if the table was already created.\n');
      process.exit(0);
    } else {
      console.error(`   ❌ Migration failed:`);
      console.error(`   ${error instanceof Error ? error.message : String(error)}\n`);
      if (error?.stack) {
        console.error('Stack trace:');
        console.error(error.stack);
      }
      process.exit(1);
    }
  }
}

runMigration();
