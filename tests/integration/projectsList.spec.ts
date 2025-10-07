import { test, expect } from '@playwright/test';

test.describe('Projects List', () => {
  test.beforeEach(async ({ page }) => {
    // Login as closer
    await page.goto('/login');
    await page.fill('input[type="email"]', 'closer@kinhome.com');
    await page.fill('input[type="password"]', 'closer123');
    await page.click('button[type="submit"]');
    
    // Navigate to projects list
    await page.waitForURL('/');
    await page.goto('/projects');
    await page.waitForURL('/projects');
  });

  test('displays project table with rows', async ({ page }) => {
    // Verify page header
    await expect(page.locator('h1')).toContainText('My Projects');
    
    // Verify search bar is visible
    await expect(page.locator('[data-testid="search-bar"]')).toBeVisible();
    
    // Verify filter chips are visible
    await expect(page.locator('[data-testid="filter-chips"]')).toBeVisible();
    
    // Verify at least 1 project row is visible
    await expect(page.locator('[data-testid="project-row"]').first()).toBeVisible();
    
    // Verify each row has required columns
    const firstRow = page.locator('[data-testid="project-row"]').first();
    
    // Customer info column
    await expect(firstRow.locator('[data-testid="customer-name"]')).toBeVisible();
    
    // Traffic lights column
    await expect(firstRow.locator('[data-testid="traffic-lights"]')).toBeVisible();
    
    // Metrics column (size, PPW, value)
    await expect(firstRow.locator('text=/Size:/')).toBeVisible();
    await expect(firstRow.locator('text=/PPW:/')).toBeVisible();
    await expect(firstRow.locator('text=/Value:/')).toBeVisible();
    
    // Age indicator column
    await expect(firstRow.locator('[data-testid="project-age"]')).toBeVisible();
  });

  test('filters projects by view chip', async ({ page }) => {
    const filterChips = page.locator('[data-testid="filter-chips"]');
    
    // Test All chip
    await filterChips.locator('[data-testid="filter-chip-all"]').click();
    await expect(page).toHaveURL(/.*view=all.*|^\/projects$/);
    
    // Test Active chip
    await filterChips.locator('[data-testid="filter-chip-active"]').click();
    await expect(page).toHaveURL(/.*view=active.*/);
    
    // Verify active chip has blue background
    await expect(filterChips.locator('[data-testid="filter-chip-active"]')).toHaveClass(/bg-blue-500|bg-primary/);
    
    // Test On Hold chip
    await filterChips.locator('[data-testid="filter-chip-on-hold"]').click();
    await expect(page).toHaveURL(/.*view=on-hold.*/);
    
    // Test Install Ready chip
    await filterChips.locator('[data-testid="filter-chip-install-ready"]').click();
    await expect(page).toHaveURL(/.*view=install-ready.*/);
    
    // Test Install Scheduled chip
    await filterChips.locator('[data-testid="filter-chip-install-scheduled"]').click();
    await expect(page).toHaveURL(/.*view=install-scheduled.*/);
    
    // Verify inactive chips have gray background
    await expect(filterChips.locator('[data-testid="filter-chip-active"]')).toHaveClass(/bg-gray-200|bg-secondary/);
  });

  test('searches projects by customer name', async ({ page }) => {
    const searchInput = page.locator('[data-testid="search-bar"] input');
    
    // Type search term
    await searchInput.fill('Smith');
    
    // Wait for debounce
    await page.waitForTimeout(400);
    
    // Verify URL contains search parameter
    await expect(page).toHaveURL(/.*search=Smith.*/);
    
    // Verify visible projects contain search term
    const projectRows = page.locator('[data-testid="project-row"]');
    const count = await projectRows.count();
    
    for (let i = 0; i < count; i++) {
      const customerName = projectRows.nth(i).locator('[data-testid="customer-name"]');
      await expect(customerName).toContainText('Smith');
    }
    
    // Clear search
    await searchInput.fill('');
    await page.waitForTimeout(400);
    
    // Verify all projects visible again
    await expect(page).toHaveURL(/^\/projects(\?.*)?$/);
  });

  test('searches projects by project ID', async ({ page }) => {
    const searchInput = page.locator('[data-testid="search-bar"] input');
    
    // Type project ID
    await searchInput.fill('KIN-12345');
    
    // Wait for debounce
    await page.waitForTimeout(400);
    
    // Verify URL contains search parameter
    await expect(page).toHaveURL(/.*search=KIN-12345.*/);
    
    // Verify matching project is visible
    const projectRows = page.locator('[data-testid="project-row"]');
    const count = await projectRows.count();
    
    if (count > 0) {
      // At least one project should match the search
      const firstRow = projectRows.first();
      await expect(firstRow).toBeVisible();
    }
  });

  test('displays traffic lights correctly', async ({ page }) => {
    const firstRow = page.locator('[data-testid="project-row"]').first();
    const trafficLights = firstRow.locator('[data-testid="traffic-lights"]');
    
    // Verify traffic lights container is visible
    await expect(trafficLights).toBeVisible();
    
    // Verify 7 circles are visible (intake, survey, design, NEM, permit, install, inspection)
    const circles = trafficLights.locator('div[class*="rounded-full"]');
    await expect(circles).toHaveCount(7);
    
    // Verify circles have correct colors (green for complete, amber for in-progress, gray for pending)
    const circleColors = await circles.all();
    for (const circle of circleColors) {
      const className = await circle.getAttribute('class');
      expect(className).toMatch(/bg-(green|yellow|amber|gray|blue|red)-/);
    }
    
    // Verify status text below lights
    const statusText = trafficLights.locator('text=/\\w+: .*/');
    await expect(statusText).toBeVisible();
  });

  test('displays hold banner for on-hold projects', async ({ page }) => {
    // Filter by On Hold view
    await page.locator('[data-testid="filter-chip-on-hold"]').click();
    await page.waitForURL(/.*view=on-hold.*/);
    
    const projectRows = page.locator('[data-testid="project-row"]');
    const count = await projectRows.count();
    
    if (count > 0) {
      const firstRow = projectRows.first();
      
      // Verify hold banner is visible
      await expect(firstRow.locator('[data-testid="hold-banner"]')).toBeVisible();
      
      // Verify banner shows hold type
      const holdBanner = firstRow.locator('[data-testid="hold-banner"]');
      await expect(holdBanner).toContainText(/Finance|Roof|Customer|Permit|HOA|On Hold/);
      
      // Verify banner shows hold reason
      await expect(holdBanner.locator('[data-testid="hold-reason"]')).toBeVisible();
      
      // Verify banner shows days on hold
      await expect(holdBanner.locator('text=/\\d+ days on hold/')).toBeVisible();
      
      // Verify banner color matches hold type
      const bannerClass = await holdBanner.getAttribute('class');
      expect(bannerClass).toMatch(/bg-(red|amber|yellow|orange)-/);
    }
  });

  test('displays PPW with delta', async ({ page }) => {
    const firstRow = page.locator('[data-testid="project-row"]').first();
    
    // Verify PPW is displayed in metrics column
    await expect(firstRow.locator('text=/PPW:/')).toBeVisible();
    
    // Verify shows "Sold:" and "Yours:" labels
    const ppwDisplay = firstRow.locator('[data-testid="ppw-display"]');
    await expect(ppwDisplay).toBeVisible();
    
    // Verify shows delta with arrow icon
    await expect(ppwDisplay.locator('svg')).toBeVisible(); // Arrow icon
    
    // Verify delta color (green for positive, red for negative)
    const deltaElement = ppwDisplay.locator('[data-testid="ppw-delta"]');
    if (await deltaElement.isVisible()) {
      const deltaClass = await deltaElement.getAttribute('class');
      expect(deltaClass).toMatch(/text-(green|red)-/);
    }
  });

  test('displays age indicator with warnings', async ({ page }) => {
    const projectRows = page.locator('[data-testid="project-row"]');
    const count = await projectRows.count();
    
    for (let i = 0; i < count; i++) {
      const row = projectRows.nth(i);
      const ageIndicator = row.locator('[data-testid="project-age"]');
      
      await expect(ageIndicator).toBeVisible();
      
      const ageText = await ageIndicator.textContent();
      const ageNumber = parseInt(ageText?.replace(/\D/g, '') || '0');
      
      if (ageNumber > 120) {
        // Critical age - should show in red
        await expect(ageIndicator).toHaveClass(/text-red-|bg-red-/);
        await expect(ageIndicator).toContainText('CRITICAL');
      } else if (ageNumber > 90) {
        // Warning age - should show in amber
        await expect(ageIndicator).toHaveClass(/text-amber-|text-yellow-|bg-amber-|bg-yellow-/);
        await expect(ageIndicator).toContainText('WARNING');
      } else {
        // Normal age - should show in gray
        await expect(ageIndicator).toHaveClass(/text-gray-|bg-gray-/);
      }
    }
  });

  test('navigates to project detail on row click', async ({ page }) => {
    const firstRow = page.locator('[data-testid="project-row"]').first();
    
    // Click first project row
    await firstRow.click();
    
    // Verify URL matches project detail pattern
    await page.waitForURL(/\/projects\/\d+/);
    await expect(page).toHaveURL(/\/projects\/\d+/);
    
    // Verify project detail page loaded
    await expect(page.locator('[data-testid="project-header"]')).toBeVisible();
    
    // Navigate back
    await page.goBack();
    await page.waitForURL('/projects');
    
    // Verify returned to projects list
    await expect(page.locator('[data-testid="project-row"]').first()).toBeVisible();
  });

  test('phone link works without navigating', async ({ page }) => {
    const firstRow = page.locator('[data-testid="project-row"]').first();
    const phoneLink = firstRow.locator('[data-testid="customer-phone"]');
    
    if (await phoneLink.isVisible()) {
      // Click phone link
      await phoneLink.click();
      
      // Verify URL is still /projects (didn't navigate to detail)
      await expect(page).toHaveURL('/projects');
      
      // Verify phone link has tel: href
      const href = await phoneLink.getAttribute('href');
      expect(href).toMatch(/^tel:/);
    }
  });

  test('shows empty state when no projects match filters', async ({ page }) => {
    const searchInput = page.locator('[data-testid="search-bar"] input');
    
    // Type non-existent search term
    await searchInput.fill('NONEXISTENT');
    
    // Wait for debounce
    await page.waitForTimeout(400);
    
    // Verify empty state message
    await expect(page.locator('text=No projects found')).toBeVisible();
    
    // Verify Clear Filters button
    const clearFiltersButton = page.locator('[data-testid="clear-filters-button"]');
    await expect(clearFiltersButton).toBeVisible();
    
    // Click Clear Filters
    await clearFiltersButton.click();
    
    // Verify search is cleared and projects are visible
    await expect(searchInput).toHaveValue('');
    await expect(page.locator('[data-testid="project-row"]').first()).toBeVisible();
  });

  test('handles pagination correctly', async ({ page }) => {
    // This test assumes pagination exists
    const pagination = page.locator('[data-testid="pagination"]');
    
    if (await pagination.isVisible()) {
      // Verify pagination controls are visible
      await expect(pagination.locator('[data-testid="prev-page"]')).toBeVisible();
      await expect(pagination.locator('[data-testid="next-page"]')).toBeVisible();
      
      // Test next page
      await pagination.locator('[data-testid="next-page"]').click();
      await page.waitForTimeout(500);
      
      // Verify URL contains page parameter
      await expect(page).toHaveURL(/.*page=2.*/);
      
      // Test previous page
      await pagination.locator('[data-testid="prev-page"]').click();
      await page.waitForTimeout(500);
      
      // Verify URL is back to page 1
      await expect(page).toHaveURL(/^\/projects(\?.*)?$/);
    }
  });

  test('displays loading states during data fetch', async ({ page }) => {
    // Navigate to projects page
    await page.goto('/projects');
    
    // Check for loading skeletons
    const loadingSkeletons = page.locator('[data-testid="loading-skeleton"]');
    const skeletonCount = await loadingSkeletons.count();
    
    if (skeletonCount > 0) {
      // Verify loading skeletons are visible initially
      await expect(loadingSkeletons.first()).toBeVisible();
      
      // Wait for data to load
      await page.waitForTimeout(2000);
      
      // Verify loading skeletons are replaced with actual data
      await expect(loadingSkeletons.first()).not.toBeVisible();
    }
    
    // Verify actual project rows are displayed
    await expect(page.locator('[data-testid="project-row"]').first()).toBeVisible();
  });
});
