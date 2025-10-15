import { defineConfig, devices } from '@playwright/test';

export default defineConfig({
  testDir: './tests/integration',
  fullyParallel: false, // Run tests sequentially for offline tests
  forbidOnly: !!process.env.CI, // Prevent .only in CI
  retries: process.env.CI ? 2 : 0, // Retry flaky tests in CI
  workers: 1, // Single worker for offline tests to avoid conflicts
  reporter: 'html', // Generate HTML report
  
  use: {
    baseURL: process.env.BASE_URL || 'http://localhost:3000',
    trace: 'on-first-retry', // Capture trace on retry
    screenshot: 'only-on-failure',
    video: 'retain-on-failure',
    actionTimeout: 10000, // 10 seconds
    navigationTimeout: 30000, // 30 seconds
  },

  projects: [
    {
      name: 'chromium',
      use: {
        ...devices['Desktop Chrome'],
        viewport: { width: 1280, height: 720 }
      },
    },
    {
      name: 'iPad',
      use: {
        ...devices['iPad Pro'],
        viewport: { width: 1024, height: 768 } // Primary target
      },
    },
    {
      name: 'iPhone 14 Pro',
      use: {
        ...devices['iPhone 14 Pro'],
        viewport: { width: 393, height: 852 } // Mobile phone testing
      },
    },
    {
      name: 'iPhone SE',
      use: {
        ...devices['iPhone SE'],
        viewport: { width: 375, height: 667 } // Small phone testing
      },
    },
  ],

  webServer: process.env.BASE_URL ? undefined : {
    command: 'npm run dev',
    url: 'http://localhost:3000',
    reuseExistingServer: !process.env.CI, // Reuse in CI
    timeout: 120000, // 2 minutes
  },
});
