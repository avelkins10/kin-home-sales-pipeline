#!/usr/bin/env node

// Interactive script to rotate Quickbase token
// Run with: node scripts/rotate-quickbase-token.js

const fs = require('fs');
const path = require('path');

console.log('\n🔐 Quickbase Token Rotation Utility\n');
console.log('⚠️  CRITICAL: The Quickbase token was exposed via NEXT_PUBLIC_ prefix.');
console.log('   You MUST rotate the token immediately.\n');

console.log('📋 Steps to rotate token:\n');
console.log('1. Go to https://kin.quickbase.com');
console.log('2. Click your profile → My Preferences');
console.log('3. Click "My User Token" tab');
console.log('4. Click "Deactivate" on the old token');
console.log('5. Click "Generate New Token"');
console.log('6. Copy the new token\n');

console.log('📝 After generating new token:\n');
console.log('Local Development:');
console.log('  - Update .env.local: QUICKBASE_TOKEN=<new-token>');
console.log('  - Remove: NEXT_PUBLIC_QUICKBASE_TOKEN (if present)');
console.log('  - Test: npm run setup:health\n');

console.log('Vercel Production:');
console.log('  - Go to Vercel dashboard → Settings → Environment Variables');
console.log('  - Edit QUICKBASE_TOKEN → Enter new token → Save');
console.log('  - Verify no NEXT_PUBLIC_QUICKBASE_TOKEN exists (delete if found)');
console.log('  - Redeploy: Deployments → Latest → Redeploy\n');

console.log('Vercel Preview:');
console.log('  - Update QUICKBASE_TOKEN for Preview environment');
console.log('  - Use a separate token for preview if possible\n');

console.log('✅ Verification:');
console.log('  - Run smoke tests: npm run test:smoke');
console.log('  - Check Vercel logs for Quickbase API errors');
console.log('  - Verify projects list loads in production\n');

console.log('📚 Documentation:');
console.log('  - Update docs/TOKEN-ROTATION.md with rotation date');
console.log('  - Schedule next rotation in 90 days\n');

// Check current .env.local for exposed token
const envPath = path.join(__dirname, '..', '.env.local');
if (fs.existsSync(envPath)) {
  const envContent = fs.readFileSync(envPath, 'utf8');
  // Check for actual assignment (not comments)
  const hasToken = /^NEXT_PUBLIC_QUICKBASE_TOKEN=/m.test(envContent);
  if (hasToken) {
    console.log('❌ FOUND: NEXT_PUBLIC_QUICKBASE_TOKEN in .env.local');
    console.log('   This MUST be removed before deployment.\n');
  } else {
    console.log('✅ GOOD: No NEXT_PUBLIC_QUICKBASE_TOKEN found in .env.local\n');
  }
}

console.log('Press Ctrl+C when complete.\n');
