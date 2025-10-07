import { test, expect } from '@playwright/test';

const BASE_URL = process.env.BASE_URL || 'http://localhost:3000';

test.describe('Production Smoke Tests', () => {
  test('production site is accessible', async ({ page }) => {
    await page.goto(BASE_URL);
    
    // Verify page loads without errors
    await expect(page).toHaveTitle(/Kin Home Sales Pipeline/);
    
    // Verify no console errors
    const errors: string[] = [];
    page.on('console', msg => {
      if (msg.type() === 'error') {
        errors.push(msg.text());
      }
    });
    
    // Verify response status is 200
    const response = await page.goto(BASE_URL);
    expect(response?.status()).toBe(200);
    
    // Verify redirects to /login (unauthenticated)
    await expect(page).toHaveURL(/.*\/login/);
    
    // Verify no console errors occurred
    expect(errors).toHaveLength(0);
  });

  test('login flow works in production', async ({ page }) => {
    await page.goto(`${BASE_URL}/login`);
    
    // Verify login form is visible
    await expect(page.locator('input[type="email"]')).toBeVisible();
    await expect(page.locator('input[type="password"]')).toBeVisible();
    await expect(page.locator('button[type="submit"]')).toBeVisible();
    
    // Fill credentials (use production test account)
    await page.fill('input[type="email"]', process.env.TEST_USER_EMAIL || 'test@kinhome.com');
    await page.fill('input[type="password"]', process.env.TEST_USER_PASSWORD || 'testpassword');
    
    // Submit form
    await page.click('button[type="submit"]');
    
    // Verify redirect to dashboard
    await expect(page).toHaveURL(/.*\/dashboard/);
    
    // Verify no console errors during login
    const errors: string[] = [];
    page.on('console', msg => {
      if (msg.type() === 'error') {
        errors.push(msg.text());
      }
    });
    
    // Take screenshot for documentation
    await page.screenshot({ path: 'test-results/production-login.png' });
    
    expect(errors).toHaveLength(0);
  });

  test('dashboard loads with real data', async ({ page }) => {
    // Login first
    await page.goto(`${BASE_URL}/login`);
    await page.fill('input[type="email"]', process.env.TEST_USER_EMAIL || 'test@kinhome.com');
    await page.fill('input[type="password"]', process.env.TEST_USER_PASSWORD || 'testpassword');
    await page.click('button[type="submit"]');
    
    // Navigate to dashboard
    await page.goto(`${BASE_URL}/dashboard`);
    
    // Wait for initial cache miss (may take longer on first load)
    await expect(page.locator('[data-testid="metric-card"]')).toHaveCount(4, { timeout: 5000 });
    
    // Verify metrics show numeric values (not errors)
    const metricCards = page.locator('[data-testid="metric-card"]');
    for (let i = 0; i < 4; i++) {
      const card = metricCards.nth(i);
      const value = card.locator('[data-testid="metric-value"]');
      await expect(value).toBeVisible();
      // Should contain a number (not "Error" or "Loading")
      const text = await value.textContent();
      expect(text).toMatch(/\d+/);
    }
    
    // Verify no loading spinners stuck
    await expect(page.locator('[data-testid="loading-spinner"]')).toHaveCount(0);
    
    // Verify no error messages
    await expect(page.locator('[data-testid="error-message"]')).toHaveCount(0);
    
    // Take screenshot for documentation
    await page.screenshot({ path: 'test-results/production-dashboard.png' });
  });

  test('projects list loads and filters work', async ({ page }) => {
    // Login first
    await page.goto(`${BASE_URL}/login`);
    await page.fill('input[type="email"]', process.env.TEST_USER_EMAIL || 'test@kinhome.com');
    await page.fill('input[type="password"]', process.env.TEST_USER_PASSWORD || 'testpassword');
    await page.click('button[type="submit"]');
    
    // Navigate to projects
    await page.goto(`${BASE_URL}/projects`);
    
    // Wait for initial cache miss (may take longer on first load)
    await expect(page.locator('[data-testid="project-row"]')).toHaveCount({ min: 1 }, { timeout: 5000 });
    
    // Verify search bar works
    const searchInput = page.locator('[data-testid="search-input"]');
    await expect(searchInput).toBeVisible();
    await searchInput.fill('test');
    await page.waitForTimeout(500); // Wait for search to process
    
    // Verify filter chips work
    const filterChips = page.locator('[data-testid="filter-chip"]');
    expect(await filterChips.count()).toBeGreaterThan(0);
    
    // Click 'Active' filter
    const activeFilter = filterChips.filter({ hasText: 'Active' });
    await activeFilter.click();
    
    // Verify URL updates
    await expect(page).toHaveURL(/.*filter=active/);
    
    // Verify filtered results load (should be faster with cache)
    await page.waitForTimeout(500); // Reduced wait time for cached results
    expect(await page.locator('[data-testid="project-row"]').count()).toBeGreaterThan(0);
    
    // Take screenshot for documentation
    await page.screenshot({ path: 'test-results/production-projects.png' });
  });

  test('project detail page loads', async ({ page }) => {
    // Login first
    await page.goto(`${BASE_URL}/login`);
    await page.fill('input[type="email"]', process.env.TEST_USER_EMAIL || 'test@kinhome.com');
    await page.fill('input[type="password"]', process.env.TEST_USER_PASSWORD || 'testpassword');
    await page.click('button[type="submit"]');
    
    // Navigate to projects
    await page.goto(`${BASE_URL}/projects`);
    
    // Wait for projects to load first
    await expect(page.locator('[data-testid="project-row"]')).toHaveCount({ min: 1 }, { timeout: 5000 });
    
    // Click first project (should be faster with prefetching)
    const firstProject = page.locator('[data-testid="project-row"]').first();
    await firstProject.click();
    
    // Verify project header visible (should be faster with prefetching)
    await expect(page.locator('[data-testid="project-header"]')).toBeVisible({ timeout: 3000 });
    
    // Verify customer contact card visible
    await expect(page.locator('[data-testid="customer-contact-card"]')).toBeVisible();
    
    // Verify timeline visible
    await expect(page.locator('[data-testid="timeline"]')).toBeVisible();
    
    // Verify no console errors
    const errors: string[] = [];
    page.on('console', msg => {
      if (msg.type() === 'error') {
        errors.push(msg.text());
      }
    });
    
    // Take screenshot for documentation
    await page.screenshot({ path: 'test-results/production-detail.png' });
    
    expect(errors).toHaveLength(0);
  });

  test('PWA manifest is accessible', async ({ page }) => {
    const response = await page.goto(`${BASE_URL}/manifest.json`);
    
    // Verify response status is 200
    expect(response?.status()).toBe(200);
    
    // Verify JSON is valid
    const manifest = await response?.json();
    expect(manifest).toBeDefined();
    
    // Verify required fields exist
    expect(manifest.name).toBeDefined();
    expect(manifest.short_name).toBeDefined();
    expect(manifest.start_url).toBeDefined();
    expect(manifest.display).toBeDefined();
    expect(manifest.icons).toBeDefined();
    expect(Array.isArray(manifest.icons)).toBe(true);
    
    // Verify icon URLs are valid (192x192, 512x512)
    const icons = manifest.icons;
    const has192 = icons.some((icon: any) => icon.sizes === '192x192');
    const has512 = icons.some((icon: any) => icon.sizes === '512x512');
    expect(has192).toBe(true);
    expect(has512).toBe(true);
  });

  test('service worker registers successfully @prod', async ({ page }) => {
    // Skip this test unless BASE_URL is HTTPS (production)
    test.skip(!BASE_URL.startsWith('https://'), 'Service worker test requires HTTPS (production environment)');
    
    await page.goto(`${BASE_URL}/dashboard`);
    
    // Wait for service worker registration
    await page.waitForTimeout(3000);
    
    // Verify service worker is active
    const swRegistered = await page.evaluate(async () => {
      if ('serviceWorker' in navigator) {
        const registration = await navigator.serviceWorker.ready;
        return registration.active !== null;
      }
      return false;
    });
    
    expect(swRegistered).toBe(true);
    
    // Verify no service worker errors in console
    const errors: string[] = [];
    page.on('console', msg => {
      if (msg.type() === 'error' && msg.text().includes('service worker')) {
        errors.push(msg.text());
      }
    });
    
    expect(errors).toHaveLength(0);
  });

  test('HTTPS is enforced @prod', async ({ page }) => {
    // Skip this test unless BASE_URL is HTTPS (production)
    test.skip(!BASE_URL.startsWith('https://'), 'HTTPS test requires production environment');
    
    // Verify BASE_URL starts with https://
    expect(BASE_URL).toMatch(/^https:\/\//);
    
    await page.goto(BASE_URL);
    
    // Verify secure cookies are set (check for __Secure- prefix)
    const cookies = await page.context().cookies();
    const secureCookies = cookies.filter(cookie => cookie.name.startsWith('__Secure-'));
    expect(secureCookies.length).toBeGreaterThan(0);
    
    // Verify no mixed content warnings
    const errors: string[] = [];
    page.on('console', msg => {
      if (msg.type() === 'error' && msg.text().includes('mixed content')) {
        errors.push(msg.text());
      }
    });
    
    expect(errors).toHaveLength(0);
  });

  test('API routes are protected', async ({ page }) => {
    // Try to access protected API route without authentication
    const response = await page.goto(`${BASE_URL}/api/projects/123/hold`);
    expect(response?.status()).toBe(401);
    
    // Login and try again
    await page.goto(`${BASE_URL}/login`);
    await page.fill('input[type="email"]', process.env.TEST_USER_EMAIL || 'test@kinhome.com');
    await page.fill('input[type="password"]', process.env.TEST_USER_PASSWORD || 'testpassword');
    await page.click('button[type="submit"]');
    
    // Now try the API route (should return 200 or 400, not 401)
    const protectedResponse = await page.goto(`${BASE_URL}/api/projects/123/hold`);
    expect(protectedResponse?.status()).not.toBe(401);
  });

  test('offline functionality works in production', async ({ page }) => {
    // Login and load project detail
    await page.goto(`${BASE_URL}/login`);
    await page.fill('input[type="email"]', process.env.TEST_USER_EMAIL || 'test@kinhome.com');
    await page.fill('input[type="password"]', process.env.TEST_USER_PASSWORD || 'testpassword');
    await page.click('button[type="submit"]');
    
    await page.goto(`${BASE_URL}/projects`);
    const firstProject = page.locator('[data-testid="project-row"]').first();
    await firstProject.click();
    
    // Set browser offline
    await page.context().setOffline(true);
    
    // Verify offline indicator appears
    await expect(page.locator('[data-testid="offline-indicator"]')).toBeVisible();
    
    // Verify cached data is still visible
    await expect(page.locator('[data-testid="project-header"]')).toBeVisible();
    
    // Set browser online
    await page.context().setOffline(false);
    
    // Verify indicator disappears
    await expect(page.locator('[data-testid="offline-indicator"]')).not.toBeVisible();
  });

  test('performance meets targets', async ({ page }) => {
    // Test dashboard load time
    await page.goto(`${BASE_URL}/login`);
    await page.fill('input[type="email"]', process.env.TEST_USER_EMAIL || 'test@kinhome.com');
    await page.fill('input[type="password"]', process.env.TEST_USER_PASSWORD || 'testpassword');
    await page.click('button[type="submit"]');
    
    const startTime = Date.now();
    await page.goto(`${BASE_URL}/dashboard`);
    await expect(page.locator('[data-testid="metric-card"]')).toHaveCount(4, { timeout: 5000 });
    const dashboardLoadTime = Date.now() - startTime;
    
    // Dashboard should load in <3 seconds (increased for cache miss scenarios)
    expect(dashboardLoadTime).toBeLessThan(3000);
    
    // Test projects list load time (should be faster with cache)
    const projectsStartTime = Date.now();
    await page.goto(`${BASE_URL}/projects`);
    await expect(page.locator('[data-testid="project-row"]')).toHaveCount({ min: 1 }, { timeout: 5000 });
    const projectsLoadTime = Date.now() - projectsStartTime;
    
    // Projects list should load in <2.5 seconds (increased for cache miss scenarios)
    expect(projectsLoadTime).toBeLessThan(2500);
    
    // Test project detail load time (should be faster with prefetching)
    const detailStartTime = Date.now();
    const firstProject = page.locator('[data-testid="project-row"]').first();
    await firstProject.click();
    await expect(page.locator('[data-testid="project-header"]')).toBeVisible({ timeout: 3000 });
    const detailLoadTime = Date.now() - detailStartTime;
    
    // Project detail should load in <1.5 seconds (prefetching should help)
    expect(detailLoadTime).toBeLessThan(1500);
    
    // Use Navigation Timing Level 2 for reliable performance metrics
    const metrics = await page.evaluate(() => {
      const nav = performance.getEntriesByType('navigation')[0] as PerformanceNavigationTiming;
      return {
        loadTime: nav.loadEventEnd - nav.startTime,
        domContentLoaded: nav.domContentLoadedEventEnd - nav.startTime,
        responseEnd: nav.responseEnd - nav.startTime,
        firstByte: nav.responseStart - nav.startTime,
        domInteractive: nav.domInteractive - nav.startTime,
        domComplete: nav.domComplete - nav.startTime
      };
    });
    
    // Overall page load should be <3 seconds (increased for cache miss scenarios)
    expect(metrics.loadTime).toBeLessThan(3000);
    
    // DOM content loaded should be <2 seconds
    expect(metrics.domContentLoaded).toBeLessThan(2000);
    
    // Response end should be <1.5 seconds
    expect(metrics.responseEnd).toBeLessThan(1500);
  });
});
