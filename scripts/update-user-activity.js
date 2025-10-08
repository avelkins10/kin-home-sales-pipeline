#!/usr/bin/env node

/**
 * Script to update lastProjectDate for all users based on their QuickBase projects
 * Run with: node scripts/update-user-activity.js
 */

const { sql } = require('@vercel/postgres');
const fs = require('fs');
const path = require('path');

// Load environment variables from .env.local
require('dotenv').config({ path: path.join(__dirname, '../.env.local') });

async function updateUserActivity() {
  console.log('🚀 Starting user activity update...');
  
  try {
    // Get all users with quickbase_user_id
    const usersResult = await sql.query(`
      SELECT id, email, name, quickbase_user_id, last_project_date
      FROM users 
      WHERE quickbase_user_id IS NOT NULL 
      AND quickbase_user_id != ''
      AND role IN ('closer', 'setter')
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
            const now = new Date();
            const monthsSince = (now.getTime() - lastProject.getTime()) / (1000 * 60 * 60 * 24 * 30);
            
            if (monthsSince <= 6) {
              activityStats.active++;
            } else if (monthsSince <= 12) {
              activityStats.inactive++;
            } else {
              activityStats.dormant++;
            }
            
            console.log(`    ✅ Updated: ${activity.totalProjects} projects, last: ${activity.lastProjectDate}`);
            updated++;
          } else {
            console.log(`    ⚠️  No projects found for ${user.name}`);
            activityStats.dormant++;
          }
          
        } catch (error) {
          console.error(`    ❌ Error updating ${user.name}:`, error.message);
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
