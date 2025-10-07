import { expect } from 'vitest'
import '@testing-library/jest-dom'

// Extend Vitest matchers with jest-dom
expect.extend({})

// Mock environment variables for tests
process.env.QUICKBASE_REALM = 'test.quickbase.com'
process.env.QUICKBASE_TOKEN = 'test-token'
process.env.QUICKBASE_APP_ID = 'test-app'
process.env.QUICKBASE_TABLE_PROJECTS = 'test-table'
