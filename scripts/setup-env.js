#!/usr/bin/env node

/**
 * Environment Validation Script
 * 
 * Validates that .env.local exists and contains real credentials (not placeholders).
 * Run this script before starting development to ensure environment is configured correctly.
 */

const fs = require('fs');
const path = require('path');

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

function parseEnvFile(filePath) {
  const content = fs.readFileSync(filePath, 'utf8');
  const env = {};
  
  content.split('\n').forEach(line => {
    const trimmed = line.trim();
    if (trimmed && !trimmed.startsWith('#')) {
      const [key, ...valueParts] = trimmed.split('=');
      if (key && valueParts.length > 0) {
        env[key.trim()] = valueParts.join('=').trim();
      }
    }
  });
  
  return env;
}

function validateEnvironment() {
  log('\n🔍 Validating Environment Configuration...\n', 'blue');
  
  const envPath = path.join(process.cwd(), '.env.local');
  
  // Check if .env.local exists
  if (!fs.existsSync(envPath)) {
    log('❌ .env.local file not found!', 'red');
    log('\n📋 Setup Instructions:', 'yellow');
    log('1. Copy the example file: cp env.example .env.local');
    log('2. Edit .env.local with your actual credentials');
    log('3. Run this script again: npm run setup:env\n');
    process.exit(1);
  }
  
  log('✅ .env.local file found', 'green');
  
  // Parse environment variables
  let env;
  try {
    env = parseEnvFile(envPath);
  } catch (error) {
    log('❌ Error reading .env.local file:', 'red');
    log(error.message, 'red');
    process.exit(1);
  }
  
  // Validation rules
  const validations = [
    {
      key: 'QUICKBASE_REALM',
      expected: 'kin.quickbase.com',
      description: 'Quickbase realm URL'
    },
    {
      key: 'QUICKBASE_TOKEN',
      validator: (value) => {
        if (!value || value === 'YOUR_QUICKBASE_TOKEN_HERE' || value.length < 20) {
          return false;
        }
        return true;
      },
      description: 'Quickbase API token',
      instructions: 'Generate at https://kin.quickbase.com → My Preferences → My User Token'
    },
    {
      key: 'QUICKBASE_APP_ID',
      expected: 'br9kwm8bk',
      description: 'Quickbase application ID'
    },
    {
      key: 'QUICKBASE_TABLE_PROJECTS',
      expected: 'br9kwm8na',
      description: 'Quickbase projects table ID'
    },
    {
      key: 'DATABASE_URL',
      validator: (value) => {
        if (!value || value.includes('YOUR_DATABASE_URL_HERE') || !value.startsWith('postgresql://')) {
          return false;
        }
        return true;
      },
      description: 'Neon PostgreSQL connection URL',
      instructions: 'Get from Neon dashboard at https://console.neon.tech'
    },
    {
      key: 'NEXTAUTH_SECRET',
      validator: (value) => {
        if (!value || value === 'YOUR_NEXTAUTH_SECRET_HERE' || value.length < 32) {
          return false;
        }
        return true;
      },
      description: 'NextAuth JWT secret',
      instructions: 'Generate with: openssl rand -base64 32'
    },
    {
      key: 'NEXTAUTH_URL',
      expected: 'http://localhost:3000',
      description: 'NextAuth application URL'
    }
  ];
  
  let allValid = true;
  
  // Validate each variable
  validations.forEach(({ key, expected, validator, description, instructions }) => {
    const value = env[key];
    
    let isValid = false;
    if (expected) {
      isValid = value === expected;
    } else if (validator) {
      isValid = validator(value);
    }
    
    if (isValid) {
      log(`✅ ${key}: ${description}`, 'green');
    } else {
      log(`❌ ${key}: ${description}`, 'red');
      if (instructions) {
        log(`   💡 ${instructions}`, 'yellow');
      }
      allValid = false;
    }
  });
  
  // Summary
  log('\n' + '='.repeat(50), 'blue');
  if (allValid) {
    log('🎉 Environment validation passed!', 'green');
    log('✅ All required variables are properly configured', 'green');
    log('\n🚀 Ready to proceed with database setup:', 'blue');
    log('   npm run setup:db', 'blue');
  } else {
    log('❌ Environment validation failed!', 'red');
    log('⚠️  Fix the errors above before proceeding', 'yellow');
    log('\n📖 For detailed setup instructions, see SETUP.md', 'blue');
    process.exit(1);
  }
  log('='.repeat(50) + '\n', 'blue');
}

// Run validation
validateEnvironment();
