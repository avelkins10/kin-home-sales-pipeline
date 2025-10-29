import { sql } from '@vercel/postgres';
import fs from 'fs';
import path from 'path';
import { config } from 'dotenv';

// Load environment variables
config({ path: '.env.local' });

async function removeTaskTypeConstraint() {
  try {
    console.log('🚀 Removing task_type CHECK constraint from PRODUCTION database...');
    console.log('⚠️  Database:', process.env.POSTGRES_URL?.substring(0, 50) + '...');

    // Read migration SQL
    const migrationSQL = fs.readFileSync(
      path.join(process.cwd(), 'lib/db/migrations/027_remove_task_type_constraint.sql'),
      'utf-8'
    );

    // Execute migration
    console.log('📝 Executing migration SQL...');
    await sql.query(migrationSQL);

    console.log('✅ Migration 027 completed successfully!');

    // Verify constraint is removed by checking constraints
    const checkConstraint = await sql`
      SELECT conname
      FROM pg_constraint
      WHERE conrelid = 'arrivy_tasks'::regclass
      AND conname = 'arrivy_tasks_task_type_check';
    `;

    if (checkConstraint.rows.length === 0) {
      console.log('✅ task_type CHECK constraint successfully removed');
    } else {
      console.log('⚠️  Constraint still exists:', checkConstraint.rows[0].conname);
    }

    console.log('\n✅ Production migration complete!');
    console.log('📌 Next step: Run sync to update tasks with detailed types');
    console.log('📌 Example: curl -X GET "https://kineticsales.app/api/cron/sync-arrivy?days=1"');

  } catch (error) {
    console.error('❌ Migration failed:', error);
    process.exit(1);
  } finally {
    // Close connection
    process.exit(0);
  }
}

removeTaskTypeConstraint();
