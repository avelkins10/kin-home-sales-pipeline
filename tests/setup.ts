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
