import { test, expect } from '@playwright/test';

test.describe('User Journey - Complete Flow', () => {
  test('closer can login, view dashboard, filter projects, view detail, and manage hold', async ({ page }) => {
    // Step 1: Login
    await page.goto('/login');
    
    // Fill login form
    await page.fill('input[type="email"]', 'closer@kinhome.com');
    await page.fill('input[type="password"]', 'closer123');
    await page.click('button[type="submit"]');
    
    // Wait for redirect to dashboard
    await page.waitForURL('/');
    await expect(page).toHaveURL('/');
    
    // Verify dashboard loaded
    await expect(page.locator('h1')).toContainText('Welcome back');
    
    // Step 2: View Dashboard
    // Verify dashboard metrics are visible
    await expect(page.locator('[data-testid="installs-this-week"]')).toBeVisible();
    await expect(page.locator('[data-testid="active-accounts"]')).toBeVisible();
    await expect(page.locator('[data-testid="on-hold"]')).toBeVisible();
    await expect(page.locator('[data-testid="monthly-installs"]')).toBeVisible();
    
    // Verify each metric shows a number (not loading skeleton)
    const installsCard = page.locator('[data-testid="installs-this-week"]');
    await expect(installsCard.locator('text=/\\d+/')).toBeVisible();
    
    const activeCard = page.locator('[data-testid="active-accounts"]');
    await expect(activeCard.locator('text=/\\d+/')).toBeVisible();
    
    const holdCard = page.locator('[data-testid="on-hold"]');
    await expect(holdCard.locator('text=/\\d+/')).toBeVisible();
    
    const monthlyCard = page.locator('[data-testid="monthly-installs"]');
    await expect(monthlyCard.locator('text=/\\d+/')).toBeVisible();
    
    // Verify UrgentAlerts section is visible (if any holds exist)
    const urgentAlerts = page.locator('[data-testid="urgent-alerts"]');
    if (await urgentAlerts.isVisible()) {
      await expect(urgentAlerts).toBeVisible();
    }
    
    // Verify RecentProjects section is visible
    await expect(page.locator('[data-testid="recent-projects"]')).toBeVisible();
    
    // Take screenshot
    await page.screenshot({ path: 'test-results/dashboard-view.png' });
    
    // Step 3: Navigate to Projects List
    await page.click('a[href="/projects"]');
    await page.waitForURL('/projects');
    await expect(page).toHaveURL('/projects');
    
    // Verify page header
    await expect(page.locator('h1')).toContainText('My Projects');
    
    // Verify search bar is visible
    await expect(page.locator('[data-testid="search-bar"]')).toBeVisible();
    
    // Verify filter chips are visible
    await expect(page.locator('[data-testid="filter-chips"]')).toBeVisible();
    
    // Verify at least 1 project row is visible
    await expect(page.locator('[data-testid="project-row"]').first()).toBeVisible();
    
    // Take screenshot
    await page.screenshot({ path: 'test-results/projects-list.png' });
    
    // Step 4: Search for Project
    const searchInput = page.locator('[data-testid="search-bar"] input');
    await searchInput.fill('Smith');
    
    // Wait for debounce
    await page.waitForTimeout(400);
    
    // Verify URL contains search parameter
    await expect(page).toHaveURL(/.*search=Smith.*/);
    
    // Verify filtered results show only matching projects
    const projectRows = page.locator('[data-testid="project-row"]');
    const count = await projectRows.count();
    
    for (let i = 0; i < count; i++) {
      const customerName = projectRows.nth(i).locator('[data-testid="customer-name"]');
      await expect(customerName).toContainText('Smith');
    }
    
    // Step 5: Filter by View
    // Click Active filter chip
    await page.click('[data-testid="filter-chip-active"]');
    await expect(page).toHaveURL(/.*view=active.*/);
    
    // Verify all visible projects show Active status (not On Hold)
    const activeProjectRows = page.locator('[data-testid="project-row"]');
    const activeCount = await activeProjectRows.count();
    
    for (let i = 0; i < activeCount; i++) {
      const row = activeProjectRows.nth(i);
      // Should not have hold banner
      await expect(row.locator('[data-testid="hold-banner"]')).not.toBeVisible();
    }
    
    // Click On Hold filter chip
    await page.click('[data-testid="filter-chip-on-hold"]');
    await expect(page).toHaveURL(/.*view=on-hold.*/);
    
    // Verify all visible projects show hold banners
    const holdProjectRows = page.locator('[data-testid="project-row"]');
    const holdCount = await holdProjectRows.count();
    
    for (let i = 0; i < holdCount; i++) {
      const row = holdProjectRows.nth(i);
      await expect(row.locator('[data-testid="hold-banner"]')).toBeVisible();
    }
    
    // Step 6: View Project Detail
    // Click first project row
    await holdProjectRows.first().click();
    
    // Wait for project detail page
    await page.waitForURL(/\/projects\/\d+/);
    await expect(page).toHaveURL(/\/projects\/\d+/);
    
    // Verify project header visible
    await expect(page.locator('[data-testid="project-header"]')).toBeVisible();
    
    // Verify customer contact card visible
    await expect(page.locator('[data-testid="customer-contact"]')).toBeVisible();
    
    // Verify system specs card visible
    await expect(page.locator('[data-testid="system-specs"]')).toBeVisible();
    
    // Verify team members card visible
    await expect(page.locator('[data-testid="team-members"]')).toBeVisible();
    
    // Verify timeline visible with traffic lights
    await expect(page.locator('[data-testid="timeline"]')).toBeVisible();
    
    // Verify hold management card visible
    await expect(page.locator('[data-testid="hold-management"]')).toBeVisible();
    
    // Take screenshot
    await page.screenshot({ path: 'test-results/project-detail.png' });
    
    // Step 7: Place Project on Hold
    // Click Place on Hold button
    await page.click('[data-testid="place-on-hold-button"]');
    
    // Verify dialog opens
    await expect(page.locator('[data-testid="hold-dialog"]')).toBeVisible();
    
    // Fill hold reason
    await page.fill('[data-testid="hold-reason-textarea"]', 'Customer requested delay');
    
    // Fill block reason
    await page.fill('[data-testid="block-reason-textarea"]', 'Waiting for HOA approval');
    
    // Submit dialog
    await page.click('[data-testid="hold-dialog-submit"]');
    
    // Wait for success toast
    await expect(page.locator('text=Hold status updated')).toBeVisible();
    
    // Verify dialog closes
    await expect(page.locator('[data-testid="hold-dialog"]')).not.toBeVisible();
    
    // Verify hold banner appears
    await expect(page.locator('[data-testid="hold-banner"]')).toBeVisible();
    
    // Verify hold management card shows On Hold status
    await expect(page.locator('[data-testid="hold-management"]')).toContainText('On Hold');
    
    // Verify hold reason displays correctly
    await expect(page.locator('[data-testid="hold-management"]')).toContainText('Customer requested delay');
    
    // Step 8: Update Hold Reason
    // Click Update Reason button
    await page.click('[data-testid="update-reason-button"]');
    
    // Verify dialog opens with existing reason pre-filled
    await expect(page.locator('[data-testid="hold-dialog"]')).toBeVisible();
    await expect(page.locator('[data-testid="hold-reason-textarea"]')).toHaveValue('Customer requested delay');
    
    // Update hold reason
    await page.fill('[data-testid="hold-reason-textarea"]', 'Customer rescheduled to next month');
    
    // Submit
    await page.click('[data-testid="hold-dialog-submit"]');
    
    // Wait for success toast
    await expect(page.locator('text=Hold status updated')).toBeVisible();
    
    // Verify updated reason displays
    await expect(page.locator('[data-testid="hold-management"]')).toContainText('Customer rescheduled to next month');
    
    // Step 9: Release Hold
    // Click Release Hold button
    await page.click('[data-testid="release-hold-button"]');
    
    // Wait for success toast
    await expect(page.locator('text=Hold status updated')).toBeVisible();
    
    // Verify hold banner disappears
    await expect(page.locator('[data-testid="hold-banner"]')).not.toBeVisible();
    
    // Verify hold management card shows Active status
    await expect(page.locator('[data-testid="hold-management"]')).toContainText('Active');
    
    // Step 10: Navigate Back to Projects
    // Click Back to Projects button
    await page.click('[data-testid="back-to-projects-button"]');
    
    // Verify URL is /projects
    await expect(page).toHaveURL('/projects');
    
    // Verify project list reloaded
    await expect(page.locator('[data-testid="project-row"]').first()).toBeVisible();
    
    // Step 11: Logout
    // Click user menu or sign out button
    await page.click('[data-testid="user-menu"]');
    await page.click('[data-testid="sign-out-button"]');
    
    // Verify redirected to login
    await page.waitForURL('/login');
    await expect(page).toHaveURL('/login');
    
    // Verify session cleared (cannot access dashboard without login)
    await page.goto('/');
    await page.waitForURL('/login');
    await expect(page).toHaveURL('/login');
  });
});
