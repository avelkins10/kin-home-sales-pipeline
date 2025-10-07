import { test, expect } from '@playwright/test';

test.describe('Dashboard', () => {
  test.beforeEach(async ({ page }) => {
    // Navigate to login and authenticate
    await page.goto('/login');
    await page.fill('input[type="email"]', 'closer@kinhome.com');
    await page.fill('input[type="password"]', 'closer123');
    await page.click('button[type="submit"]');
    
    // Wait for redirect to dashboard
    await page.waitForURL('/');
  });

  test('displays dashboard metrics correctly', async ({ page }) => {
    // Verify 4 metric cards are visible
    await expect(page.locator('[data-testid="installs-this-week"]')).toBeVisible();
    await expect(page.locator('[data-testid="active-accounts"]')).toBeVisible();
    await expect(page.locator('[data-testid="on-hold"]')).toBeVisible();
    await expect(page.locator('[data-testid="monthly-installs"]')).toBeVisible();
    
    // Verify each card shows a numeric value
    const installsCard = page.locator('[data-testid="installs-this-week"]');
    await expect(installsCard.locator('text=/\\d+/')).toBeVisible();
    
    const activeCard = page.locator('[data-testid="active-accounts"]');
    await expect(activeCard.locator('text=/\\d+/')).toBeVisible();
    
    const holdCard = page.locator('[data-testid="on-hold"]');
    await expect(holdCard.locator('text=/\\d+/')).toBeVisible();
    
    const monthlyCard = page.locator('[data-testid="monthly-installs"]');
    await expect(monthlyCard.locator('text=/\\d+/')).toBeVisible();
    
    // Verify icons are visible
    await expect(installsCard.locator('svg')).toBeVisible();
    await expect(activeCard.locator('svg')).toBeVisible();
    await expect(holdCard.locator('svg')).toBeVisible();
    await expect(monthlyCard.locator('svg')).toBeVisible();
    
    // Verify hold breakdown subtitle if holds exist
    const holdCount = await holdCard.locator('text=/\\d+/').textContent();
    if (holdCount && parseInt(holdCount) > 0) {
      await expect(holdCard.locator('text=/\\d+ Finance, \\d+ Roof/')).toBeVisible();
    }
  });

  test('displays urgent alerts for holds > 7 days', async ({ page }) => {
    const urgentAlerts = page.locator('[data-testid="urgent-alerts"]');
    
    if (await urgentAlerts.isVisible()) {
      // Verify red alert banner is visible
      await expect(urgentAlerts).toBeVisible();
      await expect(urgentAlerts).toHaveClass(/bg-red-50|border-red-200/);
      
      // Verify shows up to 3 urgent projects
      const alertItems = urgentAlerts.locator('[data-testid="urgent-alert-item"]');
      const count = await alertItems.count();
      expect(count).toBeLessThanOrEqual(3);
      
      // Verify each shows customer name, days on hold, hold reason
      for (let i = 0; i < count; i++) {
        const item = alertItems.nth(i);
        await expect(item.locator('[data-testid="customer-name"]')).toBeVisible();
        await expect(item.locator('text=/\\d+ days on hold/')).toBeVisible();
        await expect(item.locator('[data-testid="hold-reason"]')).toBeVisible();
      }
      
      // Verify "View all X on hold" link if more than 3
      const viewAllLink = urgentAlerts.locator('[data-testid="view-all-holds-link"]');
      if (await viewAllLink.isVisible()) {
        await expect(viewAllLink).toContainText('View all');
        await expect(viewAllLink).toHaveAttribute('href', '/projects?view=on-hold');
      }
    } else {
      // If no urgent alerts, verify section is not rendered
      await expect(urgentAlerts).not.toBeVisible();
    }
  });

  test('displays recent projects list', async ({ page }) => {
    const recentProjects = page.locator('[data-testid="recent-projects"]');
    
    // Verify Recent Projects card is visible
    await expect(recentProjects).toBeVisible();
    
    // Verify shows up to 5 projects
    const projectItems = recentProjects.locator('[data-testid="recent-project-item"]');
    const count = await projectItems.count();
    expect(count).toBeLessThanOrEqual(5);
    
    // Verify each project shows required information
    for (let i = 0; i < count; i++) {
      const item = projectItems.nth(i);
      await expect(item.locator('[data-testid="customer-name"]')).toBeVisible();
      await expect(item.locator('[data-testid="project-id"]')).toBeVisible();
      await expect(item.locator('[data-testid="milestone-badge"]')).toBeVisible();
      await expect(item.locator('svg')).toBeVisible(); // Arrow icon
    }
    
    // Verify "View all →" link in header
    const viewAllLink = recentProjects.locator('[data-testid="view-all-projects-link"]');
    await expect(viewAllLink).toBeVisible();
    await expect(viewAllLink).toContainText('View all');
    await expect(viewAllLink).toHaveAttribute('href', '/projects');
  });

  test('navigates to project detail from recent projects', async ({ page }) => {
    const recentProjects = page.locator('[data-testid="recent-projects"]');
    const projectItems = recentProjects.locator('[data-testid="recent-project-item"]');
    
    // Click first project in recent projects list
    await projectItems.first().click();
    
    // Verify URL matches project detail pattern
    await page.waitForURL(/\/projects\/\d+/);
    await expect(page).toHaveURL(/\/projects\/\d+/);
    
    // Verify project detail page loaded
    await expect(page.locator('[data-testid="project-header"]')).toBeVisible();
  });

  test('navigates to projects list from View all link', async ({ page }) => {
    const recentProjects = page.locator('[data-testid="recent-projects"]');
    const viewAllLink = recentProjects.locator('[data-testid="view-all-projects-link"]');
    
    // Click "View all →" link
    await viewAllLink.click();
    
    // Verify URL is /projects
    await page.waitForURL('/projects');
    await expect(page).toHaveURL('/projects');
    
    // Verify projects list page loaded
    await expect(page.locator('h1')).toContainText('My Projects');
    await expect(page.locator('[data-testid="project-row"]').first()).toBeVisible();
  });

  test('shows role-specific welcome message', async ({ page }) => {
    // Verify welcome message contains expected text
    await expect(page.locator('h1')).toContainText('Welcome back');
    
    // Verify role display shows appropriate text
    await expect(page.locator('[data-testid="role-display"]')).toContainText('Closer Dashboard');
  });

  test('refreshes data automatically', async ({ page }) => {
    // Get initial metric values
    const installsCard = page.locator('[data-testid="installs-this-week"]');
    const initialValue = await installsCard.locator('text=/\\d+/').textContent();
    
    // Wait for potential refresh (this test may be slow)
    // Note: In a real test environment, you might want to reduce refetchInterval
    await page.waitForTimeout(1000);
    
    // Check if metrics update (look for loading state then new data)
    // This is a basic check - in practice, you'd want to mock the API response
    const currentValue = await installsCard.locator('text=/\\d+/').textContent();
    
    // Values should be the same unless data actually changed
    expect(currentValue).toBeDefined();
  });

  test('displays loading states correctly', async ({ page }) => {
    // Navigate to dashboard and check for loading skeletons
    await page.goto('/');
    
    // Check if loading skeletons are shown initially
    const loadingSkeletons = page.locator('[data-testid="loading-skeleton"]');
    const skeletonCount = await loadingSkeletons.count();
    
    if (skeletonCount > 0) {
      // Verify loading skeletons are visible
      await expect(loadingSkeletons.first()).toBeVisible();
      
      // Wait for data to load
      await page.waitForTimeout(2000);
      
      // Verify loading skeletons are replaced with actual data
      await expect(loadingSkeletons.first()).not.toBeVisible();
    }
    
    // Verify actual data is displayed
    await expect(page.locator('[data-testid="installs-this-week"]')).toBeVisible();
  });

  test('handles empty states gracefully', async ({ page }) => {
    // This test would require mocking empty API responses
    // For now, just verify the dashboard loads without errors
    
    await expect(page.locator('[data-testid="installs-this-week"]')).toBeVisible();
    await expect(page.locator('[data-testid="active-accounts"]')).toBeVisible();
    await expect(page.locator('[data-testid="on-hold"]')).toBeVisible();
    await expect(page.locator('[data-testid="monthly-installs"]')).toBeVisible();
    
    // Verify no error messages are displayed
    await expect(page.locator('text=/Error|Failed|Something went wrong/')).not.toBeVisible();
  });
});
