/**
 * Check Table Schema
 */

import { sql } from '@/lib/db/client';

async function main() {
  const getRows = (result: any) => result.rows || (Array.isArray(result) ? result : []);

  // Check repcard_users schema
  const usersSchema = getRows(await sql`
    SELECT 
      column_name,
      data_type,
      is_nullable,
      column_default
    FROM information_schema.columns
    WHERE table_name = 'repcard_users'
    ORDER BY ordinal_position
  `);

  console.log('repcard_users table schema:');
  usersSchema.forEach((col: any) => {
    console.log(`  ${col.column_name}: ${col.data_type} (nullable: ${col.is_nullable})`);
  });
  console.log('');

  // Check repcard_offices schema
  const officesSchema = getRows(await sql`
    SELECT 
      column_name,
      data_type,
      is_nullable,
      column_default
    FROM information_schema.columns
    WHERE table_name = 'repcard_offices'
    ORDER BY ordinal_position
  `);

  console.log('repcard_offices table schema:');
  officesSchema.forEach((col: any) => {
    console.log(`  ${col.column_name}: ${col.data_type} (nullable: ${col.is_nullable})`);
  });
}

main().catch(console.error);

