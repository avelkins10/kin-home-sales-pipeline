import { sql } from '@vercel/postgres';
import fs from 'fs';
import path from 'path';
import { config } from 'dotenv';

// Load environment variables
config({ path: '.env.local' });

async function runProductionMigration() {
  try {
    console.log('🚀 Running migration 019 on PRODUCTION database...');
    console.log('⚠️  Database:', process.env.POSTGRES_URL?.substring(0, 50) + '...');

    // Read migration SQL
    const migrationSQL = fs.readFileSync(
      path.join(process.cwd(), 'lib/db/migrations/019_create_arrivy_task_attachments.sql'),
      'utf-8'
    );

    // Execute migration
    console.log('📝 Executing migration SQL...');
    await sql.query(migrationSQL);

    console.log('✅ Migration 019 completed successfully!');

    // Verify table exists
    const checkTable = await sql`
      SELECT EXISTS (
        SELECT FROM information_schema.tables
        WHERE table_name = 'arrivy_task_attachments'
      );
    `;

    if (checkTable.rows[0].exists) {
      console.log('✅ arrivy_task_attachments table exists in production');

      // Check if there are any existing rows
      const countResult = await sql`
        SELECT COUNT(*) as count FROM arrivy_task_attachments;
      `;

      console.log(`📊 Current attachment count: ${countResult.rows[0].count}`);
    } else {
      console.log('❌ Table was not created');
      process.exit(1);
    }

    console.log('\n✅ Production migration complete!');
    console.log('📌 Next step: Run backfill sync to populate data');

  } catch (error) {
    console.error('❌ Migration failed:', error);
    process.exit(1);
  } finally {
    // Close connection
    process.exit(0);
  }
}

runProductionMigration();
