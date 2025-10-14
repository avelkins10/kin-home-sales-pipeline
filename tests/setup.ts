import { expect } from 'vitest'
import '@testing-library/jest-dom'

// Extend Vitest matchers with jest-dom
expect.extend({})

// Mock environment variables for tests
process.env.QUICKBASE_REALM = 'test.quickbase.com'
process.env.QUICKBASE_TOKEN = 'test-token'
process.env.QUICKBASE_APP_ID = 'test-app'
process.env.QUICKBASE_TABLE_PROJECTS = 'test-table'
// Base URLs for stable absolute URL construction in Node/JSDOM
process.env.TEST_BASE_URL = 'http://localhost:3000'
process.env.NEXT_PUBLIC_APP_URL = 'http://localhost:3000'

// Seed test database if SEED_TEST_DB environment variable is set
if (process.env.SEED_TEST_DB === 'true') {
  console.log('🌱 SEED_TEST_DB is enabled - loading test hierarchy data...');
  
  // Import and run the seed function
  import('../scripts/seed-test-db').then(({ seedTestDatabase }) => {
    seedTestDatabase().catch((error) => {
      console.error('❌ Failed to seed test database:', error);
      // Don't exit the process, just log the error
      // Tests can still run without seed data
    });
  });
}
