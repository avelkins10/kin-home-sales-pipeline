/**
 * Check Production Database Schema
 * Verifies that migrations have been applied
 */

import { sql } from '@/lib/db/client';
import { config } from 'dotenv';
import { resolve } from 'path';

// Load production environment variables
config({ path: resolve(process.cwd(), '.env.production') });
config({ path: resolve(process.cwd(), '.env.local') });
config({ path: resolve(process.cwd(), '.env') });

if (process.env.DATABASE_URL && !process.env.POSTGRES_URL) {
  process.env.POSTGRES_URL = process.env.DATABASE_URL;
}

const getRows = (result: any) => result.rows || (Array.isArray(result) ? result : []);

async function main() {
  console.log('🔍 Checking Production Database Schema\n');
  console.log('='.repeat(70));

  try {
    // Check repcard_users.company_id nullable
    const usersSchema = getRows(await sql`
      SELECT 
        column_name,
        data_type,
        is_nullable
      FROM information_schema.columns
      WHERE table_name = 'repcard_users'
        AND column_name = 'company_id'
    `);

    console.log('repcard_users.company_id:');
    if (usersSchema.length > 0) {
      const col = usersSchema[0];
      console.log(`  Type: ${col.data_type}`);
      console.log(`  Nullable: ${col.is_nullable}`);
      console.log(`  ✅ ${col.is_nullable === 'YES' ? 'CORRECT (nullable)' : '❌ WRONG (should be nullable)'}`);
    } else {
      console.log('  ❌ Column not found');
    }
    console.log('');

    // Check repcard_offices.company_id nullable
    const officesSchema = getRows(await sql`
      SELECT 
        column_name,
        data_type,
        is_nullable
      FROM information_schema.columns
      WHERE table_name = 'repcard_offices'
        AND column_name = 'company_id'
    `);

    console.log('repcard_offices.company_id:');
    if (officesSchema.length > 0) {
      const col = officesSchema[0];
      console.log(`  Type: ${col.data_type}`);
      console.log(`  Nullable: ${col.is_nullable}`);
      console.log(`  ${col.is_nullable === 'YES' ? '✅ CORRECT (nullable)' : '❌ WRONG (should be nullable)'}`);
    } else {
      console.log('  ❌ Column not found');
    }
    console.log('');

    // Check repcard_user_id types
    const userIdTypes = getRows(await sql`
      SELECT 
        table_name,
        column_name,
        data_type
      FROM information_schema.columns
      WHERE column_name IN ('repcard_user_id', 'setter_user_id', 'closer_user_id')
        AND table_name LIKE 'repcard_%'
      ORDER BY table_name, column_name
    `);

    console.log('RepCard User ID Column Types:');
    userIdTypes.forEach((col: any) => {
      const isCorrect = col.data_type === 'integer';
      console.log(`  ${col.table_name}.${col.column_name}: ${col.data_type} ${isCorrect ? '✅' : '❌'}`);
    });

  } catch (error) {
    console.error('❌ Error:', error);
    if (error instanceof Error) {
      console.error('Error message:', error.message);
    }
    process.exit(1);
  }
}

main()
  .then(() => process.exit(0))
  .catch((error) => {
    console.error('Fatal error:', error);
    process.exit(1);
  });

