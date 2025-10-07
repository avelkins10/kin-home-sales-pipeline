#!/usr/bin/env node

/**
 * User Seeding Script
 * 
 * Creates 5 test users (one for each role) with bcrypt-hashed passwords for testing authentication.
 * These are TEST credentials only. Never use these passwords in production!
 */

const path = require('path');
require('dotenv').config({ path: path.join(process.cwd(), '.env.local') });

// Import the same client and bcrypt used in the application
const { sql } = require('@vercel/postgres');
const { hash } = require('bcryptjs');

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

// Test users to create
const testUsers = [
  {
    email: 'addison.r@kinhome.com',
    password: 'password',
    name: 'Addison Richards',
    role: 'closer',
    quickbase_user_id: '118,129,186,326,452,508,650,711,870,882,910', // All Addison's Quickbase closer IDs
    sales_office: null
  },
  {
    email: 'closer@kinhome.com',
    password: 'closer123',
    name: 'Test Closer',
    role: 'closer',
    quickbase_user_id: 'closer-qb-001',
    sales_office: null
  },
  {
    email: 'setter@kinhome.com',
    password: 'setter123',
    name: 'Test Setter',
    role: 'setter',
    quickbase_user_id: 'setter-qb-001',
    sales_office: null
  },
  {
    email: 'office@kinhome.com',
    password: 'office123',
    name: 'Test Office Leader',
    role: 'office_leader',
    quickbase_user_id: 'office-qb-001',
    sales_office: ['Phoenix']
  },
  {
    email: 'regional@kinhome.com',
    password: 'regional123',
    name: 'Test Regional Manager',
    role: 'regional',
    quickbase_user_id: 'regional-qb-001',
    sales_office: ['Phoenix', 'Tucson', 'Las Vegas']
  },
  {
    email: 'admin@kinhome.com',
    password: 'admin123',
    name: 'Test Super Admin',
    role: 'super_admin',
    quickbase_user_id: 'admin-qb-001',
    sales_office: null
  }
];

async function createUser(user) {
  try {
    // Hash password with bcrypt (10 salt rounds, same as NextAuth config)
    const passwordHash = await hash(user.password, 10);
    
    // Insert user into database
    await sql`
      INSERT INTO users (
        id,
        email,
        password_hash,
        name,
        role,
        quickbase_user_id,
        sales_office,
        created_at,
        updated_at
      ) VALUES (
        gen_random_uuid(),
        ${user.email},
        ${passwordHash},
        ${user.name},
        ${user.role},
        ${user.quickbase_user_id},
        ${user.sales_office},
        NOW(),
        NOW()
      )
    `;
    
    log(`✅ Created ${user.email} (${user.role})`, 'green');
    return true;
  } catch (error) {
    if (error.message.includes('duplicate key') || error.message.includes('unique constraint')) {
      log(`⚠️  User ${user.email} already exists (skipping)`, 'yellow');
      return true; // Not a failure, user already exists
    }
    
    log(`❌ Failed to create ${user.email}:`, 'red');
    log(`   Error: ${error.message}`, 'red');
    return false;
  }
}

async function seedUsers() {
  log('\n' + '='.repeat(50), 'blue');
  log('👥 USER SEEDING', 'bold');
  log('='.repeat(50), 'blue');
  
  // Check if DATABASE_URL is set
  if (!process.env.DATABASE_URL) {
    log('❌ DATABASE_URL environment variable not set!', 'red');
    log('   Run: npm run setup:env', 'yellow');
    process.exit(1);
  }
  
  log('\n🔐 Creating test users with bcrypt-hashed passwords...', 'blue');
  log('⚠️  These are TEST credentials only. Never use in production!\n', 'yellow');
  
  let successCount = 0;
  let totalCount = testUsers.length;
  
  // Create each user
  for (const user of testUsers) {
    const success = await createUser(user);
    if (success) {
      successCount++;
    }
  }
  
  // Summary
  log('\n' + '='.repeat(50), 'blue');
  if (successCount === totalCount) {
    log('🎉 User seeding complete!', 'green');
    log(`✅ Successfully processed ${successCount}/${totalCount} users`, 'green');
    
    // Display credentials table
    log('\n📋 Test User Credentials:', 'blue');
    log('┌─────────────────┬─────────────────────┬─────────────┬─────────────────┐', 'blue');
    log('│ Role            │ Email               │ Password    │ Quickbase ID    │', 'blue');
    log('├─────────────────┼─────────────────────┼─────────────┼─────────────────┤', 'blue');
    
    testUsers.forEach(user => {
      const role = user.role.padEnd(15);
      const email = user.email.padEnd(19);
      const password = user.password.padEnd(11);
      const qbId = user.quickbase_user_id.padEnd(15);
      log(`│ ${role} │ ${email} │ ${password} │ ${qbId} │`, 'blue');
    });
    
    log('└─────────────────┴─────────────────────┴─────────────┴─────────────────┘', 'blue');
    
    log('\n🚀 Ready to proceed with health check:', 'blue');
    log('   npm run setup:health', 'blue');
    
    log('\n💡 Next Steps:', 'yellow');
    log('1. Run health check: npm run setup:health', 'yellow');
    log('2. Start dev server: npm run dev', 'yellow');
    log('3. Login with any test user credentials', 'yellow');
  } else {
    log('❌ User seeding failed!', 'red');
    log(`⚠️  Only ${successCount}/${totalCount} users created successfully`, 'yellow');
    log('\n📖 For detailed troubleshooting, see SETUP.md', 'blue');
    process.exit(1);
  }
  log('='.repeat(50) + '\n', 'blue');
}

// Run seeding
seedUsers().catch(error => {
  log('💥 Unexpected error during user seeding:', 'red');
  log(error.message, 'red');
  process.exit(1);
});
