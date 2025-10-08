#!/usr/bin/env node
const path = require('path');
require('dotenv').config({ path: path.join(__dirname, '../.env.local') });
const { sql } = require('@vercel/postgres');
const fs = require('fs');

async function main() {
  try {
    if (!process.env.DATABASE_URL) {
      console.error('ERROR: DATABASE_URL is not set');
      process.exit(1);
    }

    const migrationPath = path.resolve(__dirname, 'migrations', '002-create-project-messages.sql');
    const sqlContent = fs.readFileSync(migrationPath, 'utf8');

    console.log('Running project messages migration...');
    await sql.query(sqlContent);
    console.log('✅ Executed migration SQL');

    // Verify table creation
    const tableCheck = await sql.query(`SELECT table_name FROM information_schema.tables WHERE table_name = 'project_messages'`);
    if (tableCheck.rows.length > 0) {
      console.log('✅ Created project_messages table');
    } else {
      console.warn('⚠️  project_messages table not found after migration');
    }

    // Verify indexes
    const indexCheck = await sql.query(`SELECT indexname FROM pg_indexes WHERE tablename = 'project_messages'`);
    const expectedIndexes = [
      'idx_project_messages_project',
      'idx_project_messages_sender',
      'idx_project_messages_created',
      'idx_project_messages_not_deleted'
    ];

    expectedIndexes.forEach(indexName => {
      const hasIndex = indexCheck.rows.some(r => r.indexname === indexName);
      if (hasIndex) {
        console.log(`✅ Created index: ${indexName}`);
      } else {
        console.warn(`⚠️  Index not found: ${indexName}`);
      }
    });

    // Verify trigger
    const triggerCheck = await sql.query(`
      SELECT tgname FROM pg_trigger
      WHERE tgname = 'update_project_messages_updated_at'
    `);
    if (triggerCheck.rows.length > 0) {
      console.log('✅ Created update_project_messages_updated_at trigger');
    } else {
      console.warn('⚠️  update_project_messages_updated_at trigger not found');
    }

    console.log('✅ Project messages migration complete');
    process.exit(0);
  } catch (err) {
    console.error('Migration failed:', err);
    process.exit(1);
  }
}

main();
