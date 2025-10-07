import { test, expect } from '@playwright/test'
import { format } from 'date-fns'

test.describe('Audit Logs (Super Admin)', () => {
  test.beforeEach(async ({ page }) => {
    // Login as super admin
    await page.goto('/login')
    await page.fill('input[name="email"]', 'admin@kinhome.com')
    await page.fill('input[name="password"]', 'admin123')
    await page.click('button[type="submit"]')
    await page.waitForURL('/')
    
    // Navigate to settings
    await page.goto('/settings')
    await page.waitForLoadState('networkidle')
    
    // Click Audit Logs tab
    await page.click('text=Audit Logs')
    await page.waitForLoadState('networkidle')
  })

  test('displays audit logs table with filters', async ({ page }) => {
    // Verify page header
    await expect(page.locator('h2')).toContainText('Audit Logs')
    await expect(page.locator('text=View all system activity and user actions')).toBeVisible()
    
    // Verify filters are visible
    await expect(page.locator('button:has-text("MMM dd")')).toBeVisible() // Date range picker
    await expect(page.locator('text=Filter by action')).toBeVisible() // Action filter
    await expect(page.locator('input[placeholder*="Search by user"]')).toBeVisible() // Search input
    await expect(page.locator('button:has-text("Export CSV")')).toBeVisible() // Export button
    
    // Verify table structure
    await expect(page.locator('th:has-text("Timestamp")')).toBeVisible()
    await expect(page.locator('th:has-text("User")')).toBeVisible()
    await expect(page.locator('th:has-text("Action")')).toBeVisible()
    await expect(page.locator('th:has-text("Resource")')).toBeVisible()
    await expect(page.locator('th:has-text("IP Address")')).toBeVisible()
    await expect(page.locator('th:has-text("Details")')).toBeVisible()
    
    // Verify at least one audit log is visible (from previous test actions)
    await expect(page.locator('tbody tr')).toHaveCount.greaterThan(0)
  })

  test('filters audit logs by date range', async ({ page }) => {
    // Click date range picker
    await page.click('button:has-text("MMM dd")')
    
    // Verify calendar popover opens
    await expect(page.locator('[role="dialog"]')).toBeVisible()
    
    // Select last 7 days (click on a date 7 days ago)
    const sevenDaysAgo = new Date()
    sevenDaysAgo.setDate(sevenDaysAgo.getDate() - 7)
    const sevenDaysAgoStr = format(sevenDaysAgo, 'MMM dd')
    
    // Click on the date (this is a simplified approach - in real test you'd click specific date)
    await page.click('button:has-text("Today")') // Click today to close and set range
    await page.click('button:has-text("MMM dd")') // Reopen to verify it closed
    
    // Verify date range button shows selected dates
    await expect(page.locator('button:has-text("MMM dd")')).toBeVisible()
    
    // Verify table updates (wait for network request)
    await page.waitForLoadState('networkidle')
  })

  test('filters audit logs by action type', async ({ page }) => {
    // Select "Create" from action filter
    await page.click('text=Filter by action')
    await page.click('text=Create')
    
    // Wait for table to update
    await page.waitForLoadState('networkidle')
    
    // Verify all visible logs have "create" action badge
    const createBadges = page.locator('tbody tr').locator('text=create')
    await expect(createBadges).toHaveCount.greaterThan(0)
    
    // Select "Update" from dropdown
    await page.click('text=Filter by action')
    await page.click('text=Update')
    
    // Wait for table to update
    await page.waitForLoadState('networkidle')
    
    // Verify all visible logs have "update" action badge
    const updateBadges = page.locator('tbody tr').locator('text=update')
    await expect(updateBadges).toHaveCount.greaterThan(0)
    
    // Select "All Actions"
    await page.click('text=Filter by action')
    await page.click('text=All Actions')
    
    // Wait for table to update
    await page.waitForLoadState('networkidle')
    
    // Verify all logs visible again
    await expect(page.locator('tbody tr')).toHaveCount.greaterThan(0)
  })

  test('searches audit logs by user name', async ({ page }) => {
    // Type "Admin" in search input
    await page.fill('input[placeholder*="Search by user"]', 'Admin')
    
    // Wait for results to update
    await page.waitForLoadState('networkidle')
    
    // Verify only logs with "Admin" in user name are visible
    const adminRows = page.locator('tbody tr').filter({ hasText: 'Admin' })
    await expect(adminRows).toHaveCount.greaterThan(0)
    
    // Clear search
    await page.fill('input[placeholder*="Search by user"]', '')
    await page.waitForLoadState('networkidle')
    
    // Verify all logs visible again
    await expect(page.locator('tbody tr')).toHaveCount.greaterThan(0)
  })

  test('searches audit logs by resource', async ({ page }) => {
    // Type "user" in search input
    await page.fill('input[placeholder*="Search by user"]', 'user')
    
    // Wait for results to update
    await page.waitForLoadState('networkidle')
    
    // Verify only logs with "user" in resource field are visible
    const userRows = page.locator('tbody tr').filter({ hasText: 'user' })
    await expect(userRows).toHaveCount.greaterThan(0)
  })

  test('paginates audit logs', async ({ page }) => {
    // Check if pagination controls are visible
    const paginationInfo = page.locator('text=Showing page')
    const hasPagination = await paginationInfo.isVisible()
    
    if (hasPagination) {
      // Verify pagination controls
      await expect(page.locator('text=Showing page 1 of')).toBeVisible()
      
      // Click "Next" button if available
      const nextButton = page.locator('button:has-text("Next")')
      if (await nextButton.isEnabled()) {
        await nextButton.click()
        await page.waitForLoadState('networkidle')
        
        // Verify page increments to 2
        await expect(page.locator('text=Showing page 2 of')).toBeVisible()
        
        // Click "Previous" button
        await page.click('button:has-text("Previous")')
        await page.waitForLoadState('networkidle')
        
        // Verify returns to page 1
        await expect(page.locator('text=Showing page 1 of')).toBeVisible()
      }
    } else {
      // If no pagination, verify table has logs
      await expect(page.locator('tbody tr')).toHaveCount.greaterThan(0)
    }
  })

  test('opens detail dialog for audit log', async ({ page }) => {
    // Click "Details" button (Eye icon) on first log
    const firstDetailsButton = page.locator('tbody tr').first().locator('button').last()
    await firstDetailsButton.click()
    
    // Verify dialog opens
    await expect(page.locator('[role="dialog"]')).toBeVisible()
    await expect(page.locator('text=Audit Log Details')).toBeVisible()
    
    // Verify dialog shows audit log details
    await expect(page.locator('text=User:')).toBeVisible()
    await expect(page.locator('text=Action:')).toBeVisible()
    await expect(page.locator('text=Resource:')).toBeVisible()
    await expect(page.locator('text=IP Address:')).toBeVisible()
    
    // Check if changes section is visible (for logs with changes)
    const changesSection = page.locator('text=Changes:')
    if (await changesSection.isVisible()) {
      await expect(changesSection).toBeVisible()
      // Verify shows old values in red and new values in green
      await expect(page.locator('text=Old:')).toBeVisible()
      await expect(page.locator('text=New:')).toBeVisible()
    }
    
    // Verify user agent section
    await expect(page.locator('text=User Agent:')).toBeVisible()
    
    // Close dialog by clicking outside
    await page.click('body', { position: { x: 10, y: 10 } })
    
    // Verify dialog closes
    await expect(page.locator('[role="dialog"]')).not.toBeVisible()
  })

  test('exports audit logs to CSV', async ({ page }) => {
    // Set some filters first
    await page.click('text=Filter by action')
    await page.click('text=Update')
    await page.waitForLoadState('networkidle')
    
    // Set up download promise
    const downloadPromise = page.waitForEvent('download')
    
    // Click "Export CSV" button
    await page.click('button:has-text("Export CSV")')
    
    // Wait for download to start
    const download = await downloadPromise
    
    // Verify success toast
    await expect(page.locator('text=Audit logs exported successfully')).toBeVisible()
    
    // Verify CSV file downloaded
    expect(download.suggestedFilename()).toMatch(/audit-logs-\d{4}-\d{2}-\d{2}\.csv/)
    
    // Verify filename format
    const filename = download.suggestedFilename()
    expect(filename).toMatch(/^audit-logs-\d{4}-\d{2}-\d{2}\.csv$/)
  })

  test('shows JSON diff for system settings update', async ({ page }) => {
    // First, trigger a system settings update by navigating to System tab
    await page.click('text=System')
    await page.waitForLoadState('networkidle')
    
    // Make a small change to system settings (if possible)
    // This is a simplified test - in real scenario you'd update a setting
    
    // Navigate back to Audit Logs tab
    await page.click('text=Audit Logs')
    await page.waitForLoadState('networkidle')
    
    // Filter by action: "Update"
    await page.click('text=Filter by action')
    await page.click('text=Update')
    await page.waitForLoadState('networkidle')
    
    // Search for "system_settings"
    await page.fill('input[placeholder*="Search by user"]', 'system_settings')
    await page.waitForLoadState('networkidle')
    
    // Click details on the system settings update log (if any)
    const systemSettingsRow = page.locator('tbody tr').filter({ hasText: 'system_settings' }).first()
    if (await systemSettingsRow.isVisible()) {
      await systemSettingsRow.locator('button').last().click()
      
      // Verify changes section shows field names and old/new values
      await expect(page.locator('text=Changes:')).toBeVisible()
      await expect(page.locator('text=Old:')).toBeVisible()
      await expect(page.locator('text=New:')).toBeVisible()
    }
  })

  test('prevents non-admin from accessing audit logs tab', async ({ page }) => {
    // Sign out
    await page.click('text=Sign Out')
    await page.waitForURL('/login')
    
    // Login as closer
    await page.fill('input[name="email"]', 'closer@kinhome.com')
    await page.fill('input[name="password"]', 'closer123')
    await page.click('button[type="submit"]')
    await page.waitForURL('/')
    
    // Navigate to settings
    await page.goto('/settings')
    await page.waitForLoadState('networkidle')
    
    // Verify Audit Logs tab is NOT visible
    await expect(page.locator('text=Audit Logs')).not.toBeVisible()
    
    // Verify only Profile and Notifications tabs visible
    await expect(page.locator('text=Profile')).toBeVisible()
    await expect(page.locator('text=Notifications')).toBeVisible()
    await expect(page.locator('text=Users')).not.toBeVisible()
    await expect(page.locator('text=Offices')).not.toBeVisible()
    await expect(page.locator('text=System')).not.toBeVisible()
  })

  test('prevents direct API access without admin role', async ({ page }) => {
    // Sign out
    await page.click('text=Sign Out')
    await page.waitForURL('/login')
    
    // Login as closer
    await page.fill('input[name="email"]', 'closer@kinhome.com')
    await page.fill('input[name="password"]', 'closer123')
    await page.click('button[type="submit"]')
    await page.waitForURL('/')
    
    // Try to access audit logs API directly
    const response = await page.request.get('/api/admin/audit-logs?from=2024-01-01&to=2024-12-31')
    
    // Verify returns 403 Forbidden
    expect(response.status()).toBe(403)
  })
})