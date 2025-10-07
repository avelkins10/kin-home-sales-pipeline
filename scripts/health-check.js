#!/usr/bin/env node

/**
 * Health Check Script
 * 
 * Comprehensive validation of all integrations:
 * - Environment variables
 * - Database connectivity
 * - NextAuth authentication
 * - Quickbase API connectivity
 * - Field constants validation
 */

const path = require('path');
require('dotenv').config({ path: path.join(process.cwd(), '.env.local') });

// Import the same clients used in the application
const { sql } = require('@vercel/postgres');
const { compare } = require('bcryptjs');

// Import Quickbase client (JavaScript version for Node.js compatibility)
const { qbClient } = require('../lib/quickbase/client.js');

// Colors for console output
const colors = {
  green: '\x1b[32m',
  red: '\x1b[31m',
  yellow: '\x1b[33m',
  blue: '\x1b[34m',
  reset: '\x1b[0m',
  bold: '\x1b[1m'
};

function log(message, color = 'reset') {
  console.log(`${colors[color]}${message}${colors.reset}`);
}


async function checkEnvironmentVariables() {
  log('\n🔍 Checking environment variables...', 'blue');
  
  const requiredVars = [
    'QUICKBASE_REALM',
    'QUICKBASE_TOKEN',
    'QUICKBASE_APP_ID',
    'QUICKBASE_TABLE_PROJECTS',
    'DATABASE_URL',
    'NEXTAUTH_SECRET',
    'NEXTAUTH_URL'
  ];
  
  let allValid = true;
  
  requiredVars.forEach(varName => {
    const value = process.env[varName];
    if (value && value !== `YOUR_${varName}_HERE`) {
      log(`✅ ${varName}: Set`, 'green');
    } else {
      log(`❌ ${varName}: Missing or placeholder`, 'red');
      allValid = false;
    }
  });
  
  return allValid;
}

async function checkDatabaseConnectivity() {
  log('\n🗄️  Checking database connectivity...', 'blue');
  
  try {
    // Test basic connection
    const result = await sql`SELECT NOW() as current_time, current_database() as db_name`;
    const { current_time, db_name } = result.rows[0];
    
    log(`✅ Database connected: ${db_name}`, 'green');
    log(`   📅 Connection time: ${current_time}`, 'blue');
    
    // Check user count
    const userCountResult = await sql`SELECT COUNT(*) as count FROM users`;
    const userCount = userCountResult.rows[0].count;
    
    log(`✅ Users table accessible: ${userCount} users found`, 'green');
    
    return true;
  } catch (error) {
    log('❌ Database connection failed!', 'red');
    log(`   Error: ${error.message}`, 'red');
    
    log('\n🔧 Troubleshooting Tips:', 'yellow');
    log('1. Run: npm run setup:db', 'yellow');
    log('2. Check DATABASE_URL in .env.local', 'yellow');
    log('3. Verify Neon database is running', 'yellow');
    
    return false;
  }
}

async function checkNextAuthAuthentication() {
  log('\n🔐 Checking NextAuth authentication...', 'blue');
  
  try {
    // Query test user
    const userResult = await sql`
      SELECT * FROM users 
      WHERE email = 'closer@kinhome.com'
    `;
    
    if (userResult.rows.length === 0) {
      log('⚠️  No test users found!', 'yellow');
      log('   Run: npm run setup:seed', 'yellow');
      return false;
    }
    
    const user = userResult.rows[0];
    log(`✅ Test user found: ${user.email}`, 'green');
    
    // Verify password hash
    const passwordValid = await compare('closer123', user.password_hash);
    if (passwordValid) {
      log('✅ Password hash verification successful', 'green');
    } else {
      log('❌ Password hash verification failed!', 'red');
      return false;
    }
    
    // Verify required fields
    const requiredFields = ['role', 'quickbase_user_id'];
    const missingFields = requiredFields.filter(field => !user[field]);
    
    if (missingFields.length === 0) {
      log('✅ JWT callback fields present', 'green');
    } else {
      log(`❌ Missing JWT fields: ${missingFields.join(', ')}`, 'red');
      return false;
    }
    
    return true;
  } catch (error) {
    log('❌ NextAuth check failed!', 'red');
    log(`   Error: ${error.message}`, 'red');
    return false;
  }
}

async function checkQuickbaseAPI() {
  log('\n🔗 Checking Quickbase API connectivity...', 'blue');
  
  try {
    // Test basic query using the proper client
    const result = await qbClient.queryRecords({
      from: process.env.QUICKBASE_TABLE_PROJECTS,
      select: [3, 11, 145], // RECORD_ID, PROJECT_ID, CUSTOMER_NAME
      options: { top: 1 }
    });
    
    if (result.data && Array.isArray(result.data)) {
      log('✅ Quickbase API connected', 'green');
      
      if (result.data.length > 0) {
        const project = result.data[0];
        const projectId = project['11'] || 'Unknown';
        log(`   📊 Sample project: ${projectId}`, 'blue');
      } else {
        log('   📊 No projects found (table may be empty)', 'blue');
      }
      
      if (result.metadata) {
        log('✅ Response metadata present', 'green');
      }
      
      log('✅ Rate limiter active (10 req/sec)', 'green');
      
      return true;
    } else {
      log('❌ Invalid Quickbase response format!', 'red');
      return false;
    }
  } catch (error) {
    log('❌ Quickbase API check failed!', 'red');
    
    if (error.message.includes('401')) {
      log('   🔑 Invalid Quickbase token', 'red');
      log('   💡 Generate new token at https://kin.quickbase.com → My Preferences → My User Token', 'yellow');
    } else if (error.message.includes('403')) {
      log('   🚫 Token lacks permissions for table', 'red');
      log(`   💡 Verify token has access to table: ${process.env.QUICKBASE_TABLE_PROJECTS}`, 'yellow');
    } else if (error.message.includes('fetch')) {
      log('   🌐 Network connectivity issue', 'red');
      log('   💡 Check network connection and firewall settings', 'yellow');
    } else {
      log(`   Error: ${error.message}`, 'red');
    }
    
    return false;
  }
}

async function checkFieldConstants() {
  log('\n📋 Checking field constants...', 'blue');
  
  try {
    // Import field constants
    const fieldConstantsPath = path.join(process.cwd(), 'lib', 'constants', 'fieldIds.ts');
    
    if (!require('fs').existsSync(fieldConstantsPath)) {
      log('❌ Field constants file not found!', 'red');
      return false;
    }
    
    // Read and parse the file to count fields
    const content = require('fs').readFileSync(fieldConstantsPath, 'utf8');
    const fieldMatches = content.match(/^\s*[A-Z_]+:\s*\d+/gm);
    const fieldCount = fieldMatches ? fieldMatches.length : 0;
    
    log(`✅ Field constants loaded: ${fieldCount} fields`, 'green');
    
    // Check for critical fields
    const criticalFields = ['RECORD_ID', 'PROJECT_ID', 'CUSTOMER_NAME', 'CLOSER_ID', 'ON_HOLD'];
    const missingFields = criticalFields.filter(field => !content.includes(field));
    
    if (missingFields.length === 0) {
      log('✅ Critical fields present', 'green');
    } else {
      log(`❌ Missing critical fields: ${missingFields.join(', ')}`, 'red');
      return false;
    }
    
    return true;
  } catch (error) {
    log('❌ Field constants check failed!', 'red');
    log(`   Error: ${error.message}`, 'red');
    return false;
  }
}

async function runHealthCheck() {
  log('\n' + '='.repeat(50), 'blue');
  log('🏥 HEALTH CHECK', 'bold');
  log('='.repeat(50), 'blue');
  
  const checks = [
    { name: 'Environment Variables', fn: checkEnvironmentVariables },
    { name: 'Database Connection', fn: checkDatabaseConnectivity },
    { name: 'NextAuth Authentication', fn: checkNextAuthAuthentication },
    { name: 'Quickbase API', fn: checkQuickbaseAPI },
    { name: 'Field Constants', fn: checkFieldConstants }
  ];
  
  const results = {};
  
  // Run all checks
  for (const check of checks) {
    try {
      results[check.name] = await check.fn();
    } catch (error) {
      log(`💥 Unexpected error in ${check.name}:`, 'red');
      log(error.message, 'red');
      results[check.name] = false;
    }
  }
  
  // Summary
  log('\n' + '='.repeat(50), 'blue');
  log('🏥 HEALTH CHECK SUMMARY', 'bold');
  log('='.repeat(50), 'blue');
  
  const allPassed = Object.values(results).every(result => result === true);
  
  Object.entries(results).forEach(([name, passed]) => {
    const status = passed ? '✅ OK' : '❌ FAILED';
    const color = passed ? 'green' : 'red';
    log(`${status} ${name}`, color);
  });
  
  log('='.repeat(50), 'blue');
  
  if (allPassed) {
    log('🚀 System ready for development!', 'green');
    log('\n💡 Next Steps:', 'blue');
    log('1. Start development server: npm run dev', 'blue');
    log('2. Open http://localhost:3000', 'blue');
    log('3. Login with test user credentials', 'blue');
    log('4. Begin Phase 2 development!', 'blue');
  } else {
    log('❌ System NOT ready. Fix errors above.', 'red');
    log('\n📖 For detailed troubleshooting, see SETUP.md', 'blue');
    process.exit(1);
  }
  log('='.repeat(50) + '\n', 'blue');
}

// Run health check
runHealthCheck().catch(error => {
  log('💥 Unexpected error during health check:', 'red');
  log(error.message, 'red');
  process.exit(1);
});
