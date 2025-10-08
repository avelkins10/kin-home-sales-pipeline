#!/usr/bin/env node

/**
 * Migration runner script for user hierarchies schema
 * Run with: node scripts/run-user-hierarchies-migration.js
 */

require('dotenv').config({ path: '.env.local' });
const { sql } = require('@/lib/db/client');
const fs = require('fs');
const path = require('path');

async function runMigration() {
  console.log('🚀 Starting user hierarchies migration...');
  
  try {
    // Read migration file
    const migrationPath = path.join(__dirname, '../lib/db/migrations/006_user_hierarchies_schema.sql');
    const migrationSQL = fs.readFileSync(migrationPath, 'utf8');
    
    console.log('📄 Migration file loaded:', migrationPath);
    
    // Check if migration already ran
    const checkResult = await sql.query(`
      SELECT migration_name FROM migrations_log 
      WHERE migration_name = '006_user_hierarchies_schema'
    `);
    
    if (checkResult.rows.length > 0) {
      console.log('✅ Migration already executed, skipping...');
      return;
    }
    
    // Execute migration in transaction
    console.log('🔄 Executing migration...');
    await sql.query('BEGIN');
    
    try {
      // Split SQL into individual statements and execute
      const statements = migrationSQL
        .split(';')
        .map(stmt => stmt.trim())
        .filter(stmt => stmt.length > 0 && !stmt.startsWith('--'));
      
      for (const statement of statements) {
        if (statement.trim()) {
          console.log('  Executing:', statement.substring(0, 50) + '...');
          await sql.query(statement);
        }
      }
      
      await sql.query('COMMIT');
      console.log('✅ Migration completed successfully!');
      
      // Verify tables were created
      const tablesResult = await sql.query(`
        SELECT table_name 
        FROM information_schema.tables 
        WHERE table_schema = 'public' 
        AND table_name IN ('user_hierarchies', 'office_assignments')
      `);
      
      console.log('📊 Created tables:', tablesResult.rows.map(row => row.table_name));
      
      // Check constraints
      const constraintsResult = await sql.query(`
        SELECT constraint_name, table_name 
        FROM information_schema.table_constraints 
        WHERE table_schema = 'public' 
        AND table_name IN ('user_hierarchies', 'office_assignments')
        AND constraint_type = 'CHECK'
      `);
      
      console.log('🔒 Created constraints:', constraintsResult.rows.length);
      
    } catch (error) {
      await sql.query('ROLLBACK');
      throw error;
    }
    
  } catch (error) {
    console.error('❌ Migration failed:', error.message);
    console.error('Stack trace:', error.stack);
    process.exit(1);
  } finally {
    await sql.end();
  }
}

// Run migration
runMigration().catch(console.error);
