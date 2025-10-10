#!/usr/bin/env node

/**
 * Script to update lastProjectDate for all users based on their QuickBase projects
 * Run with: node scripts/update-user-activity.js
 */

const { sql } = require('@vercel/postgres');
const fs = require('fs');
const path = require('path');
const https = require('https');

// Load environment variables from .env.local
require('dotenv').config({ path: path.join(__dirname, '../.env.local') });

// QuickBase field IDs (from lib/constants/fieldIds.ts)
const FIELD_IDS = {
  CLOSER_ID: 516,
  SETTER_ID: 329,
  SALES_DATE: 522,
  PROJECT_STATUS: 255,
  SALES_OFFICE: 2087
};

/**
 * Query QuickBase API using HTTPS
 * @param {Array} select - Array of field IDs to select
 * @param {string} where - Query string for filtering
 * @param {Object} options - Optional query options (top, skip)
 * @returns {Promise<Object>} QuickBase response
 */
async function queryQuickbase(select, where, options = {}) {
  const QB_REALM = process.env.QUICKBASE_REALM;
  const QB_TOKEN = process.env.QUICKBASE_TOKEN;
  const QB_TABLE_ID = process.env.QUICKBASE_TABLE_PROJECTS;

  if (!QB_REALM || !QB_TOKEN || !QB_TABLE_ID) {
    throw new Error('Missing QuickBase credentials in .env.local');
  }

  const query = {
    from: QB_TABLE_ID,
    select: select,
    where: where,
    ...options
  };

  return new Promise((resolve, reject) => {
    const postData = JSON.stringify(query);

    const options = {
      hostname: 'api.quickbase.com',
      path: '/v1/records/query',
      method: 'POST',
      headers: {
        'QB-Realm-Hostname': QB_REALM,
        'Authorization': `QB-USER-TOKEN ${QB_TOKEN}`,
        'Content-Type': 'application/json',
        'Content-Length': Buffer.byteLength(postData)
      }
    };

    const req = https.request(options, (res) => {
      let data = '';

      res.on('data', (chunk) => {
        data += chunk;
      });

      res.on('end', () => {
        if (res.statusCode === 200) {
          resolve(JSON.parse(data));
        } else {
          reject(new Error(`QuickBase API error: ${res.statusCode} - ${data}`));
        }
      });
    });

    req.on('error', (error) => {
      reject(error);
    });

    req.write(postData);
    req.end();
  });
}

/**
 * Query QuickBase API with pagination to get all records
 * @param {Array} select - Array of field IDs to select
 * @param {string} where - Query string for filtering
 * @returns {Promise<Array>} All records from all pages
 */
async function queryQuickbaseAll(select, where) {
  const top = 100; // Page size
  let skip = 0;
  let allRecords = [];
  let hasMore = true;

  while (hasMore) {
    const response = await queryQuickbase(select, where, { top, skip });
    const records = response.data || [];
    
    allRecords = allRecords.concat(records);
    
    // If we got fewer records than requested, we've reached the end
    hasMore = records.length === top;
    skip += top;
  }

  return allRecords;
}

/**
 * Get user's project activity from QuickBase
 * @param {string} quickbaseUserId - QuickBase user ID
 * @returns {Promise<Object>} Activity statistics
 */
async function getUserProjectActivity(quickbaseUserId) {
  try {
    console.log(`    🔍 Querying QuickBase for user ${quickbaseUserId}...`);
    
    // Query for projects where user is closer
    const closerQuery = `{${FIELD_IDS.CLOSER_ID}}.EX.'${quickbaseUserId}'`;
    const closerProjects = await queryQuickbaseAll(
      [FIELD_IDS.SALES_DATE, FIELD_IDS.PROJECT_STATUS, FIELD_IDS.SALES_OFFICE],
      closerQuery
    );
    
    // Query for projects where user is setter
    const setterQuery = `{${FIELD_IDS.SETTER_ID}}.EX.'${quickbaseUserId}'`;
    const setterProjects = await queryQuickbaseAll(
      [FIELD_IDS.SALES_DATE, FIELD_IDS.PROJECT_STATUS, FIELD_IDS.SALES_OFFICE],
      setterQuery
    );
    
    // Combine results
    const allProjects = [
      ...closerProjects,
      ...setterProjects
    ];
    
    const totalProjects = allProjects.length;
    
    if (totalProjects === 0) {
      console.log(`    ⚠️  No projects found for user ${quickbaseUserId}`);
      return { totalProjects: 0, activeProjects: 0, lastProjectDate: null, offices: [] };
    }
    
    // Calculate active projects (last 6 months)
    const sixMonthsAgo = new Date();
    sixMonthsAgo.setMonth(sixMonthsAgo.getMonth() - 6);
    
    const activeProjects = allProjects.filter(project => {
      const salesDate = project[FIELD_IDS.SALES_DATE]?.value;
      if (!salesDate) return false;
      const projectDate = new Date(salesDate);
      // Guard against invalid dates
      if (isNaN(projectDate.getTime())) return false;
      return projectDate >= sixMonthsAgo;
    }).length;
    
    // Find most recent project date
    const projectDates = allProjects
      .map(project => project[FIELD_IDS.SALES_DATE]?.value)
      .filter(date => date)
      .map(date => new Date(date))
      .filter(date => !isNaN(date.getTime())) // Guard against invalid dates
      .sort((a, b) => b - a);
    
    const lastProjectDate = projectDates.length > 0 ? projectDates[0].toISOString() : null;
    
    // Extract unique offices
    const offices = [...new Set(
      allProjects
        .map(project => project[FIELD_IDS.SALES_OFFICE]?.value)
        .filter(office => office)
    )];
    
    console.log(`    ✅ Found ${totalProjects} projects, ${activeProjects} active, last: ${lastProjectDate}`);
    
    return {
      totalProjects,
      activeProjects,
      lastProjectDate,
      offices
    };
    
  } catch (error) {
    console.error(`    ❌ Error querying QuickBase for user ${quickbaseUserId}:`, error.message);
    return { totalProjects: 0, activeProjects: 0, lastProjectDate: null, offices: [] };
  }
}

async function updateUserActivity() {
  console.log('🚀 Starting user activity update...');
  
  // Validate required environment variables
  const QB_REALM = process.env.QUICKBASE_REALM;
  const QB_TOKEN = process.env.QUICKBASE_TOKEN;
  const QB_TABLE_ID = process.env.QUICKBASE_TABLE_PROJECTS;

  if (!QB_REALM || !QB_TOKEN || !QB_TABLE_ID) {
    console.error('❌ Missing QuickBase credentials in .env.local');
    console.error('   Required: QUICKBASE_REALM, QUICKBASE_TOKEN, QUICKBASE_TABLE_PROJECTS');
    process.exit(1);
  }
  
  try {
    // Get all users with quickbase_user_id
    // Check if USER_ACTIVITY_ROLES env var is set to filter by specific roles
    const roleFilter = process.env.USER_ACTIVITY_ROLES;
    let roleCondition = '';
    
    if (roleFilter) {
      const roles = roleFilter.split(',').map(role => `'${role.trim()}'`).join(',');
      roleCondition = `AND role IN (${roles})`;
    }
    
    const usersResult = await sql.query(`
      SELECT id, email, name, quickbase_user_id, last_project_date
      FROM users 
      WHERE quickbase_user_id IS NOT NULL 
      AND quickbase_user_id != ''
      ${roleCondition}
      ORDER BY created_at DESC
    `);
    
    const users = usersResult.rows;
    console.log(`📊 Found ${users.length} users to update`);
    
    if (users.length === 0) {
      console.log('✅ No users found to update');
      return;
    }
    
    let updated = 0;
    let errors = 0;
    const activityStats = {
      active: 0,      // 0-6 months
      inactive: 0,    // 6-12 months  
      dormant: 0      // 12+ months
    };
    
    // Process users in batches to avoid overwhelming QuickBase API
    const batchSize = 10;
    for (let i = 0; i < users.length; i += batchSize) {
      const batch = users.slice(i, i + batchSize);
      
      console.log(`🔄 Processing batch ${Math.floor(i / batchSize) + 1}/${Math.ceil(users.length / batchSize)} (${batch.length} users)`);
      
      // Process batch in parallel
      const batchPromises = batch.map(async (user) => {
        try {
          console.log(`  📋 Updating ${user.name} (${user.email})`);
          
          // Get user's project activity from QuickBase
          const activity = await getUserProjectActivity(user.quickbase_user_id);
          
          if (activity.lastProjectDate) {
            // Update user's last_project_date
            await sql.query(`
              UPDATE users 
              SET last_project_date = $1, updated_at = NOW()
              WHERE id = $2
            `, [activity.lastProjectDate, user.id]);
            
            // Categorize activity
            const lastProject = new Date(activity.lastProjectDate);
            // Guard against invalid dates
            if (!isNaN(lastProject.getTime())) {
              const now = new Date();
              const monthsSince = (now.getTime() - lastProject.getTime()) / (1000 * 60 * 60 * 24 * 30);
              
              if (monthsSince <= 6) {
                activityStats.active++;
              } else if (monthsSince <= 12) {
                activityStats.inactive++;
              } else {
                activityStats.dormant++;
              }
            } else {
              // Invalid date, categorize as dormant
              activityStats.dormant++;
            }
            
            console.log(`    ✅ Updated: ${activity.totalProjects} projects, last: ${activity.lastProjectDate}`);
            updated++;
          } else {
            console.log(`    ⚠️  No projects found for ${user.name}`);
            activityStats.dormant++;
          }
          
        } catch (error) {
          console.error(`    ❌ Error updating ${user.name} (QB ID: ${user.quickbase_user_id}):`, error.message);
          errors++;
        }
      });
      
      // Wait for batch to complete
      await Promise.all(batchPromises);
      
      // Small delay between batches to be respectful to QuickBase API
      if (i + batchSize < users.length) {
        console.log('  ⏳ Waiting 2 seconds before next batch...');
        await new Promise(resolve => setTimeout(resolve, 2000));
      }
    }
    
    // Display summary
    console.log('\n📊 Activity Update Summary:');
    console.log(`  ✅ Updated: ${updated} users`);
    console.log(`  ❌ Errors: ${errors} users`);
    console.log(`  📈 Active (0-6 months): ${activityStats.active} users`);
    console.log(`  📉 Inactive (6-12 months): ${activityStats.inactive} users`);
    console.log(`  💤 Dormant (12+ months): ${activityStats.dormant} users`);
    
    // Update sync log
    await sql.query(`
      INSERT INTO sync_logs (sync_type, results, created_by, created_at)
      VALUES ($1, $2, $3, NOW())
    `, [
      'user_activity_update',
      JSON.stringify({
        total: users.length,
        updated,
        errors,
        activityStats
      }),
      'system'
    ]);
    
    console.log('✅ User activity update completed successfully!');
    
  } catch (error) {
    console.error('❌ User activity update failed:', error.message);
    console.error('Stack trace:', error.stack);
    process.exit(1);
  } finally {
    await sql.end();
  }
}

// Run update
updateUserActivity().catch(console.error);


