import { defineConfig } from 'vitest/config'
import { resolve } from 'path'

export default defineConfig({
  test: {
    environment: 'jsdom',
    globals: true,
    setupFiles: ['./tests/setup.ts'],
    include: ['tests/unit/**/*.test.ts', 'tests/utils/**/*.test.ts'],
    exclude: ['tests/integration/**', 'playwright.config.ts'],
    coverage: {
      provider: 'v8',
      reporter: ['text', 'json', 'html', 'lcov'],
      reportsDirectory: './tests/coverage',
      exclude: [
        'node_modules/',
        'tests/',
        '*.config.ts',
        '.next/',
        'scripts/',
        'app/layout.tsx',
        'app/providers.tsx',
        'app/**/layout.tsx',
        'app/**/page.tsx',
        'components/ui/**',
        'public/**',
      ],
      include: [
        'lib/**/*.ts',
        'components/dashboard/**/*.tsx',
        'components/projects/**/*.tsx',
        'components/milestones/**/*.tsx',
      ],
      thresholds: {
        lines: 80,
        functions: 80,
        branches: 75,
        statements: 80,
      },
    },
  },
  resolve: {
    alias: {
      '@': resolve(__dirname, './'),
    },
  },
})
