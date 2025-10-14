import { test, expect } from '@playwright/test';

test.describe('Hierarchy Bulk Assignment Flow', () => {
  test.beforeEach(async ({ page }) => {
    // Navigate to the hierarchy page
    await page.goto('/settings');
    await page.waitForLoadState('networkidle');
  });

  test('should successfully assign multiple users to manager', async ({ page }) => {
    // Wait for the hierarchy tree to load
    await page.waitForSelector('[data-testid="hierarchy-tree"]', { timeout: 10000 });
    
    // Click the "Bulk Assign" button
    await page.click('button:has-text("Bulk Assign")');
    
    // Wait for the assign dialog to open
    await page.waitForSelector('[role="dialog"]');
    
    // Select a manager from the dropdown
    await page.click('button:has-text("Select manager")');
    await page.click('text=Office Leader');
    
    // Open the user multi-select
    await page.click('button:has-text("Select users to assign")');
    
    // Select multiple users by clicking checkboxes
    await page.check('input[type="checkbox"][value="user1"]');
    await page.check('input[type="checkbox"][value="user2"]');
    await page.check('input[type="checkbox"][value="user3"]');
    
    // Verify the selected count is displayed
    await expect(page.locator('text=3 users selected')).toBeVisible();
    
    // Verify the preview message
    await expect(page.locator('text=You are about to assign 3 users to Office Leader')).toBeVisible();
    
    // Click the assign button
    await page.click('button:has-text("Assign 3 Users")');
    
    // Wait for the API call to complete
    await page.waitForResponse(response => 
      response.url().includes('/api/admin/hierarchies') && response.request().method() === 'POST'
    );
    
    // Verify success toast appears
    await expect(page.locator('text=Users assigned successfully')).toBeVisible();
    
    // Verify the dialog closes
    await expect(page.locator('[role="dialog"]')).not.toBeVisible();
    
    // Verify the hierarchy tree updates to show the assigned users
    await expect(page.locator('text=Manages 3 users')).toBeVisible();
  });

  test('should prevent invalid role assignments', async ({ page }) => {
    await page.waitForSelector('[data-testid="hierarchy-tree"]');
    
    // Click the "Bulk Assign" button
    await page.click('button:has-text("Bulk Assign")');
    await page.waitForSelector('[role="dialog"]');
    
    // Select a team lead as manager
    await page.click('button:has-text("Select manager")');
    await page.click('text=Team Lead');
    
    // Open the user multi-select
    await page.click('button:has-text("Select users to assign")');
    
    // Try to select an office leader (invalid - team lead can't manage office leader)
    await page.check('input[type="checkbox"][value="office-leader"]');
    
    // Verify validation error appears
    await expect(page.locator('text=team_lead cannot manage office_leader')).toBeVisible();
    
    // Verify the assign button is disabled
    await expect(page.locator('button:has-text("Assign")')).toBeDisabled();
  });

  test('should prevent circular hierarchy creation', async ({ page }) => {
    await page.waitForSelector('[data-testid="hierarchy-tree"]');
    
    // Click the "Bulk Assign" button
    await page.click('button:has-text("Bulk Assign")');
    await page.waitForSelector('[role="dialog"]');
    
    // Select a user who is already managed
    await page.click('button:has-text("Select manager")');
    await page.click('text=Closer');
    
    // Open the user multi-select
    await page.click('button:has-text("Select users to assign")');
    
    // Try to select the user's manager (would create circular hierarchy)
    await page.check('input[type="checkbox"][value="team-lead"]');
    
    // Verify validation error appears
    await expect(page.locator('text=Cannot create circular hierarchy')).toBeVisible();
    
    // Verify the assign button is disabled
    await expect(page.locator('button:has-text("Assign")')).toBeDisabled();
  });

  test('should prevent duplicate assignments', async ({ page }) => {
    await page.waitForSelector('[data-testid="hierarchy-tree"]');
    
    // Click the "Bulk Assign" button
    await page.click('button:has-text("Bulk Assign")');
    await page.waitForSelector('[role="dialog"]');
    
    // Select a manager who already has users
    await page.click('button:has-text("Select manager")');
    await page.click('text=Office Leader');
    
    // Open the user multi-select
    await page.click('button:has-text("Select users to assign")');
    
    // Try to select a user who is already managed by this manager
    await page.check('input[type="checkbox"][value="already-managed-user"]');
    
    // Verify validation error appears
    await expect(page.locator('text=is already managed by')).toBeVisible();
    
    // Verify the assign button is disabled
    await expect(page.locator('button:has-text("Assign")')).toBeDisabled();
  });

  test('should enforce maximum selection limit', async ({ page }) => {
    await page.waitForSelector('[data-testid="hierarchy-tree"]');
    
    // Click the "Bulk Assign" button
    await page.click('button:has-text("Bulk Assign")');
    await page.waitForSelector('[role="dialog"]');
    
    // Select a manager
    await page.click('button:has-text("Select manager")');
    await page.click('text=Super Admin');
    
    // Open the user multi-select
    await page.click('button:has-text("Select users to assign")');
    
    // Try to select more than 50 users
    for (let i = 1; i <= 51; i++) {
      const checkbox = page.locator(`input[type="checkbox"][value="user${i}"]`);
      if (await checkbox.isVisible()) {
        await checkbox.check();
      }
    }
    
    // Verify the 51st user checkbox is disabled
    await expect(page.locator('input[type="checkbox"][value="user51"]')).toBeDisabled();
    
    // Verify warning message about maximum limit
    await expect(page.locator('text=Maximum 50 users allowed')).toBeVisible();
  });

  test('should filter users in multi-select search', async ({ page }) => {
    await page.waitForSelector('[data-testid="hierarchy-tree"]');
    
    // Click the "Bulk Assign" button
    await page.click('button:has-text("Bulk Assign")');
    await page.waitForSelector('[role="dialog"]');
    
    // Select a manager
    await page.click('button:has-text("Select manager")');
    await page.click('text=Office Leader');
    
    // Open the user multi-select
    await page.click('button:has-text("Select users to assign")');
    
    // Type in the search box
    await page.fill('input[placeholder="Search users..."]', 'john');
    
    // Verify only users with "john" in their name/email are shown
    const visibleUsers = page.locator('[data-testid="user-item"]');
    const count = await visibleUsers.count();
    
    for (let i = 0; i < count; i++) {
      const userText = await visibleUsers.nth(i).textContent();
      expect(userText?.toLowerCase()).toContain('john');
    }
    
    // Clear search and verify all users are shown again
    await page.fill('input[placeholder="Search users..."]', '');
    await expect(page.locator('[data-testid="user-item"]')).toHaveCount(10); // Assuming 10 total users
  });

  test('should support bulk selection mode in tree view', async ({ page }) => {
    await page.waitForSelector('[data-testid="hierarchy-tree"]');
    
    // Click the "Bulk Select" button in toolbar
    await page.click('button:has-text("Bulk Select")');
    
    // Verify checkboxes appear on all assignable users
    await expect(page.locator('input[type="checkbox"]').first()).toBeVisible();
    
    // Click on 3 user cards to select them
    await page.click('[data-testid="user-card"]:nth-child(1)');
    await page.click('[data-testid="user-card"]:nth-child(2)');
    await page.click('[data-testid="user-card"]:nth-child(3)');
    
    // Verify selected count shows
    await expect(page.locator('text=3 users selected')).toBeVisible();
    
    // Click "Assign Selected" button
    await page.click('button:has-text("Assign Selected")');
    
    // Verify assign dialog opens with users pre-selected
    await page.waitForSelector('[role="dialog"]');
    await expect(page.locator('text=3 users selected')).toBeVisible();
  });

  test('should handle API errors gracefully', async ({ page }) => {
    // Mock API to return error
    await page.route('**/api/admin/hierarchies', route => {
      route.fulfill({
        status: 400,
        contentType: 'application/json',
        body: JSON.stringify({ error: 'Invalid assignment data' })
      });
    });
    
    await page.waitForSelector('[data-testid="hierarchy-tree"]');
    
    // Perform bulk assignment
    await page.click('button:has-text("Bulk Assign")');
    await page.waitForSelector('[role="dialog"]');
    
    await page.click('button:has-text("Select manager")');
    await page.click('text=Office Leader');
    
    await page.click('button:has-text("Select users to assign")');
    await page.check('input[type="checkbox"][value="user1"]');
    
    await page.click('button:has-text("Assign 1 User")');
    
    // Verify error toast appears
    await expect(page.locator('text=Failed to assign users')).toBeVisible();
    await expect(page.locator('text=Invalid assignment data')).toBeVisible();
    
    // Verify dialog remains open (doesn't close on error)
    await expect(page.locator('[role="dialog"]')).toBeVisible();
  });

  test('should update UI immediately after successful assignment', async ({ page }) => {
    await page.waitForSelector('[data-testid="hierarchy-tree"]');
    
    // Get initial unassigned count
    const initialUnassigned = await page.locator('text=Unassigned Users').textContent();
    const initialCount = parseInt(initialUnassigned?.match(/\((\d+)\)/)?.[1] || '0');
    
    // Perform bulk assignment
    await page.click('button:has-text("Bulk Assign")');
    await page.waitForSelector('[role="dialog"]');
    
    await page.click('button:has-text("Select manager")');
    await page.click('text=Office Leader');
    
    await page.click('button:has-text("Select users to assign")');
    await page.check('input[type="checkbox"][value="user1"]');
    await page.check('input[type="checkbox"][value="user2"]');
    
    await page.click('button:has-text("Assign 2 Users")');
    
    // Wait for success
    await expect(page.locator('text=Users assigned successfully')).toBeVisible();
    
    // Verify hierarchy tree re-renders
    await expect(page.locator('text=Manages 2 users')).toBeVisible();
    
    // Verify unassigned count decreases
    const newUnassigned = await page.locator('text=Unassigned Users').textContent();
    const newCount = parseInt(newUnassigned?.match(/\((\d+)\)/)?.[1] || '0');
    expect(newCount).toBe(initialCount - 2);
    
    // Verify assigned users appear under manager
    await expect(page.locator('text=User 1')).toBeVisible();
    await expect(page.locator('text=User 2')).toBeVisible();
  });

  test('should support keyboard shortcuts', async ({ page }) => {
    await page.waitForSelector('[data-testid="hierarchy-tree"]');
    
    // Test Cmd+K to focus search
    await page.keyboard.press('Meta+k');
    await expect(page.locator('#hierarchy-search')).toBeFocused();
    
    // Test Cmd+E to expand all
    await page.keyboard.press('Meta+e');
    // Verify all nodes are expanded (check for expanded state)
    
    // Test Cmd+Shift+E to collapse all
    await page.keyboard.press('Meta+Shift+e');
    // Verify all nodes are collapsed
    
    // Test Cmd+B to toggle bulk selection
    await page.keyboard.press('Meta+b');
    await expect(page.locator('input[type="checkbox"]').first()).toBeVisible();
    
    // Test Cmd+R to refresh
    await page.keyboard.press('Meta+r');
    // Verify data refreshes (could check for loading state)
  });

  test('should export hierarchy data', async ({ page }) => {
    await page.waitForSelector('[data-testid="hierarchy-tree"]');
    
    // Set up download promise
    const downloadPromise = page.waitForEvent('download');
    
    // Click export button
    await page.click('button:has-text("Export")');
    
    // Wait for download
    const download = await downloadPromise;
    
    // Verify download filename
    expect(download.suggestedFilename()).toBe('hierarchy-export.csv');
    
    // Verify download content (basic check)
    const path = await download.path();
    expect(path).toBeTruthy();
  });
});
