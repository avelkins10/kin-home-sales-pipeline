#!/usr/bin/env tsx

/**
 * Script to seed the test database with hierarchy data
 * This script loads test data for authorization testing
 * 
 * Usage:
 *   npm run seed-test-db
 *   or
 *   tsx scripts/seed-test-db.ts
 */

import { readFileSync } from 'fs';
import { join } from 'path';
import { sql } from '../lib/db/client';

async function seedTestDatabase() {
  console.log('🌱 Starting test database seeding...');
  
  try {
    // Read the seed SQL file
    const seedFilePath = join(__dirname, '..', 'tests', 'fixtures', 'seeds', 'hierarchy.sql');
    const seedSQL = readFileSync(seedFilePath, 'utf8');
    
    console.log('📄 Loaded seed file:', seedFilePath);
    
    // Execute the seed SQL
    console.log('⚡ Executing seed SQL...');
    await (sql as any).unsafe(seedSQL);
    
    console.log('✅ Test database seeded successfully!');
    
    // Verify the data was inserted
    console.log('🔍 Verifying seed data...');
    
    const userCount = await sql`SELECT COUNT(*) as count FROM users WHERE email LIKE '%@test.com'`;
    const hierarchyCount = await sql`SELECT COUNT(*) as count FROM user_hierarchies`;
    const officeCount = await sql`SELECT COUNT(*) as count FROM office_assignments`;

    console.log(`📊 Seed data verification:`);
    console.log(`   - Test users: ${userCount.rows[0].count}`);
    console.log(`   - User hierarchies: ${hierarchyCount.rows[0].count}`);
    console.log(`   - Office assignments: ${officeCount.rows[0].count}`);
    
    // Show some sample data
    console.log('\n📋 Sample test users:');
    const sampleUsers = await sql`
      SELECT id, email, role, name, is_active 
      FROM users 
      WHERE email LIKE '%@test.com' 
      ORDER BY role, email 
      LIMIT 10
    `;
    
    sampleUsers.rows.forEach((user: any) => {
      console.log(`   - ${user.name} (${user.email}) - ${user.role} - ${user.is_active ? 'active' : 'inactive'}`);
    });

    console.log('\n📋 Sample user hierarchies:');
    const sampleHierarchies = await sql`
      SELECT
        m.name as manager_name,
        m.email as manager_email,
        u.name as user_name,
        u.email as user_email
      FROM user_hierarchies uh
      JOIN users m ON uh.manager_id = m.id
      JOIN users u ON uh.user_id = u.id
      LIMIT 5
    `;

    sampleHierarchies.rows.forEach((hierarchy: any) => {
      console.log(`   - ${hierarchy.manager_name} manages ${hierarchy.user_name} (${hierarchy.user_email})`);
    });

    console.log('\n📋 Sample office assignments:');
    const sampleOffices = await sql`
      SELECT
        u.name as user_name,
        u.email as user_email,
        oa.office_name,
        oa.access_level
      FROM office_assignments oa
      JOIN users u ON oa.user_id = u.id
      LIMIT 5
    `;

    sampleOffices.rows.forEach((office: any) => {
      console.log(`   - ${office.user_name} has ${office.access_level} access to ${office.office_name}`);
    });
    
    console.log('\n🎉 Test database seeding completed successfully!');
    
  } catch (error) {
    console.error('❌ Error seeding test database:', error);
    process.exit(1);
  }
}

// Run the seeding if this script is executed directly
if (require.main === module) {
  seedTestDatabase()
    .then(() => {
      console.log('✅ Script completed successfully');
      process.exit(0);
    })
    .catch((error) => {
      console.error('❌ Script failed:', error);
      process.exit(1);
    });
}

export { seedTestDatabase };
