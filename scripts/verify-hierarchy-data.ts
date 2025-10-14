#!/usr/bin/env tsx

/**
 * Script to verify hierarchy data in the database
 * This script provides a manual sanity check for staging/production
 * 
 * Usage:
 *   npm run verify-hierarchy-data
 *   or
 *   tsx scripts/verify-hierarchy-data.ts
 */

import { sql } from '../lib/db/client';

interface HierarchyStats {
  totalUsers: number;
  testUsers: number;
  totalHierarchies: number;
  totalOfficeAssignments: number;
  teamLeads: number;
  officeLeaders: number;
  closers: number;
  setters: number;
  inactiveUsers: number;
}

async function verifyHierarchyData() {
  console.log('🔍 Verifying hierarchy data in database...');
  console.log('=' .repeat(50));
  
  try {
    // Get basic statistics
    const stats = await getHierarchyStats();
    
    console.log('📊 Database Statistics:');
    console.log(`   Total users: ${stats.totalUsers}`);
    console.log(`   Test users (@test.com): ${stats.testUsers}`);
    console.log(`   User hierarchies: ${stats.totalHierarchies}`);
    console.log(`   Office assignments: ${stats.totalOfficeAssignments}`);
    console.log('');
    
    console.log('👥 Role Distribution:');
    console.log(`   Team leads: ${stats.teamLeads}`);
    console.log(`   Office leaders: ${stats.officeLeaders}`);
    console.log(`   Closers: ${stats.closers}`);
    console.log(`   Setters: ${stats.setters}`);
    console.log(`   Inactive users: ${stats.inactiveUsers}`);
    console.log('');
    
    // Check data quality
    await checkDataQuality();
    
    // Check relationships
    await checkRelationships();
    
    // Check performance data
    await checkPerformanceData();
    
    console.log('✅ Hierarchy data verification completed successfully!');
    
  } catch (error) {
    console.error('❌ Error verifying hierarchy data:', error);
    process.exit(1);
  }
}

async function getHierarchyStats(): Promise<HierarchyStats> {
  const [
    totalUsers,
    testUsers,
    totalHierarchies,
    totalOfficeAssignments,
    roleStats,
    inactiveUsers
  ] = await Promise.all([
    sql`SELECT COUNT(*) as count FROM users`,
    sql`SELECT COUNT(*) as count FROM users WHERE email LIKE '%@test.com'`,
    sql`SELECT COUNT(*) as count FROM user_hierarchies`,
    sql`SELECT COUNT(*) as count FROM office_assignments`,
    sql`
      SELECT role, COUNT(*) as count
      FROM users
      WHERE email LIKE '%@test.com'
      GROUP BY role
    `,
    sql`SELECT COUNT(*) as count FROM users WHERE email LIKE '%@test.com' AND is_active = false`
  ]);

  const roleMap = roleStats.rows.reduce((acc: Record<string, number>, row: any) => {
    acc[row.role] = row.count;
    return acc;
  }, {} as Record<string, number>);

  return {
    totalUsers: totalUsers.rows[0].count,
    testUsers: testUsers.rows[0].count,
    totalHierarchies: totalHierarchies.rows[0].count,
    totalOfficeAssignments: totalOfficeAssignments.rows[0].count,
    teamLeads: roleMap.team_lead || 0,
    officeLeaders: roleMap.office_leader || 0,
    closers: roleMap.closer || 0,
    setters: roleMap.setter || 0,
    inactiveUsers: inactiveUsers.rows[0].count
  };
}

async function checkDataQuality() {
  console.log('🔍 Data Quality Checks:');
  
  // Check for users with valid emails
  const validEmails = await sql`
    SELECT COUNT(*) as count
    FROM users
    WHERE email LIKE '%@test.com'
    AND email IS NOT NULL
    AND email != ''
    AND email != '   '
  `;
  
  console.log(`   ✅ Users with valid emails: ${validEmails.rows[0].count}`);

  // Check for office assignments with valid names
  const validOffices = await sql`
    SELECT COUNT(*) as count
    FROM office_assignments
    WHERE office_name IS NOT NULL
    AND office_name != ''
    AND office_name != '   '
  `;

  console.log(`   ✅ Office assignments with valid names: ${validOffices.rows[0].count}`);

  // Check for special characters in emails
  const specialEmails = await sql`
    SELECT COUNT(*) as count
    FROM users
    WHERE email LIKE '%@test.com'
    AND (email LIKE '%+%' OR email LIKE '%''%' OR email LIKE '%.%')
  `;

  console.log(`   ✅ Users with special email characters: ${specialEmails.rows[0].count}`);
  console.log('');
}

async function checkRelationships() {
  console.log('🔗 Relationship Checks:');
  
  // Check valid user hierarchies
  const validHierarchies = await sql`
    SELECT COUNT(*) as count
    FROM user_hierarchies uh
    JOIN users m ON uh.manager_id = m.id
    JOIN users u ON uh.user_id = u.id
    WHERE m.email LIKE '%@test.com'
    AND u.email LIKE '%@test.com'
  `;
  
  console.log(`   ✅ Valid user hierarchies: ${validHierarchies.rows[0].count}`);

  // Check valid office assignments
  const validOffices = await sql`
    SELECT COUNT(*) as count
    FROM office_assignments oa
    JOIN users u ON oa.user_id = u.id
    WHERE u.email LIKE '%@test.com'
  `;

  console.log(`   ✅ Valid office assignments: ${validOffices.rows[0].count}`);

  // Check team leads with managed users
  const teamLeadsWithUsers = await sql`
    SELECT
      m.name as manager_name,
      m.email as manager_email,
      COUNT(u.id) as managed_count
    FROM user_hierarchies uh
    JOIN users m ON uh.manager_id = m.id
    JOIN users u ON uh.user_id = u.id
    WHERE m.role = 'team_lead'
    AND m.email LIKE '%@test.com'
    GROUP BY m.id, m.name, m.email
    HAVING COUNT(u.id) > 0
    ORDER BY COUNT(u.id) DESC
  `;

  console.log(`   ✅ Team leads with managed users: ${teamLeadsWithUsers.rows.length}`);
  teamLeadsWithUsers.rows.forEach((teamLead: any) => {
    console.log(`      - ${teamLead.manager_name} manages ${teamLead.managed_count} users`);
  });

  // Check office leaders with office assignments
  const officeLeadersWithOffices = await sql`
    SELECT
      u.name as user_name,
      u.email as user_email,
      COUNT(oa.office_name) as office_count
    FROM office_assignments oa
    JOIN users u ON oa.user_id = u.id
    WHERE u.role = 'office_leader'
    AND u.email LIKE '%@test.com'
    GROUP BY u.id, u.name, u.email
    HAVING COUNT(oa.office_name) > 0
    ORDER BY COUNT(oa.office_name) DESC
  `;

  console.log(`   ✅ Office leaders with office assignments: ${officeLeadersWithOffices.rows.length}`);
  officeLeadersWithOffices.rows.forEach((officeLeader: any) => {
    console.log(`      - ${officeLeader.user_name} has access to ${officeLeader.office_count} offices`);
  });
  
  console.log('');
}

async function checkPerformanceData() {
  console.log('⚡ Performance Data Checks:');
  
  // Check for team lead with many managed users
  const teamLeadWithManyUsers = await sql`
    SELECT 
      m.name as manager_name,
      COUNT(u.id) as managed_count
    FROM user_hierarchies uh
    JOIN users m ON uh.manager_id = m.id
    JOIN users u ON uh.user_id = u.id
    WHERE m.role = 'team_lead'
    AND m.email LIKE '%@test.com'
    GROUP BY m.id, m.name
    HAVING COUNT(u.id) >= 5
    ORDER BY COUNT(u.id) DESC
    LIMIT 1
  `;
  
  if (teamLeadWithManyUsers.rows.length > 0) {
    console.log(`   ✅ Team lead with many users: ${teamLeadWithManyUsers.rows[0].manager_name} (${teamLeadWithManyUsers.rows[0].managed_count} users)`);
  } else {
    console.log(`   ⚠️  No team lead with 5+ managed users found`);
  }

  // Check for office leader with many offices
  const officeLeaderWithManyOffices = await sql`
    SELECT
      u.name as user_name,
      COUNT(oa.office_name) as office_count
    FROM office_assignments oa
    JOIN users u ON oa.user_id = u.id
    WHERE u.role = 'office_leader'
    AND u.email LIKE '%@test.com'
    GROUP BY u.id, u.name
    HAVING COUNT(oa.office_name) >= 3
    ORDER BY COUNT(oa.office_name) DESC
    LIMIT 1
  `;

  if (officeLeaderWithManyOffices.rows.length > 0) {
    console.log(`   ✅ Office leader with many offices: ${officeLeaderWithManyOffices.rows[0].user_name} (${officeLeaderWithManyOffices.rows[0].office_count} offices)`);
  } else {
    console.log(`   ⚠️  No office leader with 3+ offices found`);
  }
  
  console.log('');
}

// Run the verification if this script is executed directly
if (require.main === module) {
  verifyHierarchyData()
    .then(() => {
      console.log('✅ Verification completed successfully');
      process.exit(0);
    })
    .catch((error) => {
      console.error('❌ Verification failed:', error);
      process.exit(1);
    });
}

export { verifyHierarchyData };
