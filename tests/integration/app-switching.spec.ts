import { test, expect } from '@playwright/test';

test.describe('App Switching and Role Gating', () => {
  test.beforeEach(async ({ page }) => {
    // Login as a user with cross-app access
    await page.goto('/login');
    await page.fill('[data-testid="email-input"]', 'office.leader@kinhome.com');
    await page.fill('[data-testid="password-input"]', 'password123');
    await page.click('[data-testid="login-button"]');
    await page.waitForURL('/');
  });

  test('should show app switcher for cross-app roles', async ({ page }) => {
    // App switcher should be visible for office_leader role
    await expect(page.locator('[data-testid="app-switcher"]')).toBeVisible();
    
    // Should show current app in switcher
    await expect(page.locator('[data-testid="app-switcher"]')).toContainText('Sales App');
  });

  test('should switch between Sales and Operations apps', async ({ page }) => {
    // Start in Sales app
    await expect(page).toHaveURL('/');
    await expect(page.locator('[data-testid="app-switcher"]')).toContainText('Sales App');

    // Switch to Operations app
    await page.click('[data-testid="app-switcher"]');
    await page.click('[data-testid="operations-app-option"]');
    
    // Should navigate to operations dashboard
    await expect(page).toHaveURL('/operations');
    await expect(page.locator('[data-testid="app-switcher"]')).toContainText('Operations App');
    
    // Should show operations navigation
    await expect(page.locator('nav')).toContainText('Work Orders');
    await expect(page.locator('nav')).toContainText('Inventory');
    await expect(page.locator('nav')).toContainText('Scheduling');

    // Switch back to Sales app
    await page.click('[data-testid="app-switcher"]');
    await page.click('[data-testid="sales-app-option"]');
    
    // Should navigate back to sales dashboard
    await expect(page).toHaveURL('/');
    await expect(page.locator('[data-testid="app-switcher"]')).toContainText('Sales App');
    
    // Should show sales navigation
    await expect(page.locator('nav')).toContainText('Projects');
    await expect(page.locator('nav')).toContainText('Analytics');
    await expect(page.locator('nav')).toContainText('Reports');
  });

  test('should hide app switcher for single-app roles', async ({ page }) => {
    // Logout and login as a sales-only role
    await page.click('[data-testid="user-menu"]');
    await page.click('[data-testid="logout-button"]');
    
    await page.goto('/login');
    await page.fill('[data-testid="email-input"]', 'closer@kinhome.com');
    await page.fill('[data-testid="password-input"]', 'password123');
    await page.click('[data-testid="login-button"]');
    await page.waitForURL('/');

    // App switcher should not be visible for closer role
    await expect(page.locator('[data-testid="app-switcher"]')).not.toBeVisible();
  });

  test('should redirect operations-only roles to operations app', async ({ page }) => {
    // Logout and login as operations-only role
    await page.click('[data-testid="user-menu"]');
    await page.click('[data-testid="logout-button"]');
    
    await page.goto('/login');
    await page.fill('[data-testid="email-input"]', 'operations.coordinator@kinhome.com');
    await page.fill('[data-testid="password-input"]', 'password123');
    await page.click('[data-testid="login-button"]');
    
    // Should redirect to operations app
    await expect(page).toHaveURL('/operations');
    
    // App switcher should not be visible for operations-only role
    await expect(page.locator('[data-testid="app-switcher"]')).not.toBeVisible();
  });

  test('should prevent access to operations routes with sales-only role', async ({ page }) => {
    // Login as sales-only role
    await page.goto('/login');
    await page.fill('[data-testid="email-input"]', 'closer@kinhome.com');
    await page.fill('[data-testid="password-input"]', 'password123');
    await page.click('[data-testid="login-button"]');
    await page.waitForURL('/');

    // Try to access operations route directly
    await page.goto('/operations');
    
    // Should redirect back to sales app
    await expect(page).toHaveURL('/');
  });

  test('should prevent access to sales routes with operations-only role', async ({ page }) => {
    // Login as operations-only role
    await page.goto('/login');
    await page.fill('[data-testid="email-input"]', 'operations.coordinator@kinhome.com');
    await page.fill('[data-testid="password-input"]', 'password123');
    await page.click('[data-testid="login-button"]');
    await page.waitForURL('/operations');

    // Try to access sales route directly
    await page.goto('/projects');
    
    // Should redirect back to operations app
    await expect(page).toHaveURL('/operations');
  });

  test('should show correct navigation items per app', async ({ page }) => {
    // Start in Sales app
    await expect(page.locator('nav')).toContainText('Projects');
    await expect(page.locator('nav')).toContainText('Analytics');
    await expect(page.locator('nav')).toContainText('Reports');
    await expect(page.locator('nav')).not.toContainText('Work Orders');

    // Switch to Operations app
    await page.click('[data-testid="app-switcher"]');
    await page.click('[data-testid="operations-app-option"]');
    await expect(page).toHaveURL('/operations');

    // Should show operations navigation
    await expect(page.locator('nav')).toContainText('Work Orders');
    await expect(page.locator('nav')).toContainText('Inventory');
    await expect(page.locator('nav')).toContainText('Scheduling');
    await expect(page.locator('nav')).not.toContainText('Projects');
  });

  test('should maintain app context across page refreshes', async ({ page }) => {
    // Switch to Operations app
    await page.click('[data-testid="app-switcher"]');
    await page.click('[data-testid="operations-app-option"]');
    await expect(page).toHaveURL('/operations');

    // Refresh the page
    await page.reload();

    // Should stay in Operations app
    await expect(page).toHaveURL('/operations');
    await expect(page.locator('[data-testid="app-switcher"]')).toContainText('Operations App');
  });
});
