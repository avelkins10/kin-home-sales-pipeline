import { test, expect } from '@playwright/test'

test.describe('User Management (Super Admin)', () => {
  test.beforeEach(async ({ page }) => {
    // Navigate to login
    await page.goto('/login')
    
    // Login as super admin
    await page.fill('input[name="email"]', 'admin@kinhome.com')
    await page.fill('input[name="password"]', 'admin123')
    await page.click('button[type="submit"]')
    
    // Wait for redirect to dashboard
    await page.waitForURL('/')
    
    // Navigate to settings
    await page.goto('/settings')
    
    // Click Users tab
    await page.click('text=Users')
  })

  test('displays users table with all users', async ({ page }) => {
    // Verify page header
    await expect(page.locator('h2')).toContainText('User Management')
    await expect(page.locator('text=Manage user accounts and permissions')).toBeVisible()
    
    // Verify search bar
    await expect(page.locator('input[placeholder*="Search users"]')).toBeVisible()
    
    // Verify role filter dropdown
    await expect(page.locator('text=All Roles')).toBeVisible()
    
    // Verify Add User button
    await expect(page.locator('button:has-text("Add User")')).toBeVisible()
    
    // Verify table headers
    await expect(page.locator('th:has-text("Name")')).toBeVisible()
    await expect(page.locator('th:has-text("Email")')).toBeVisible()
    await expect(page.locator('th:has-text("Role")')).toBeVisible()
    await expect(page.locator('th:has-text("Office")')).toBeVisible()
    await expect(page.locator('th:has-text("Status")')).toBeVisible()
    await expect(page.locator('th:has-text("Last Login")')).toBeVisible()
    await expect(page.locator('th:has-text("Actions")')).toBeVisible()
    
    // Verify at least some users are visible (seeded test users)
    await expect(page.locator('tbody tr')).toHaveCount({ min: 1 })
  })

  test('searches users by name', async ({ page }) => {
    // Type in search bar
    await page.fill('input[placeholder*="Search users"]', 'Closer')
    
    // Wait for results to update
    await page.waitForTimeout(500)
    
    // Verify only users with "Closer" in name are visible
    const rows = page.locator('tbody tr')
    const count = await rows.count()
    
    for (let i = 0; i < count; i++) {
      const nameCell = rows.nth(i).locator('td').first()
      const name = await nameCell.textContent()
      expect(name?.toLowerCase()).toContain('closer')
    }
    
    // Clear search
    await page.fill('input[placeholder*="Search users"]', '')
    await page.waitForTimeout(500)
    
    // Verify all users visible again
    await expect(page.locator('tbody tr')).toHaveCount({ min: 1 })
  })

  test('filters users by role', async ({ page }) => {
    // Select "Closers" from role filter
    await page.click('text=All Roles')
    await page.click('text=Closers')
    
    // Wait for results to update
    await page.waitForTimeout(500)
    
    // Verify only users with closer role are visible
    const rows = page.locator('tbody tr')
    const count = await rows.count()
    
    for (let i = 0; i < count; i++) {
      const roleCell = rows.nth(i).locator('td').nth(2) // Role column
      await expect(roleCell.locator('span')).toContainText('closer')
    }
    
    // Select "All Roles"
    await page.click('text=Closers')
    await page.click('text=All Roles')
    
    // Wait for results to update
    await page.waitForTimeout(500)
    
    // Verify all users visible again
    await expect(page.locator('tbody tr')).toHaveCount({ min: 1 })
  })

  test('creates new user successfully', async ({ page }) => {
    // Click Add User button
    await page.click('button:has-text("Add User")')
    
    // Verify dialog opens
    await expect(page.locator('text=Add New User')).toBeVisible()
    
    // Fill form
    await page.fill('input[id="name"]', 'Test New User')
    await page.fill('input[id="email"]', 'newuser@kinhome.com')
    await page.fill('input[id="phone"]', '555-9999')
    
    // Select role
    await page.click('text=Closer')
    await page.click('text=Closer') // Select from dropdown
    
    await page.fill('input[id="quickbaseUserId"]', 'test-qb-999')
    await page.fill('input[id="office"]', 'Phoenix')
    await page.fill('input[id="temporaryPassword"]', 'temppass123')
    
    // Click Create User
    await page.click('button:has-text("Create User")')
    
    // Wait for success toast
    await expect(page.locator('text=User created successfully')).toBeVisible()
    
    // Verify dialog closes
    await expect(page.locator('text=Add New User')).not.toBeVisible()
    
    // Verify new user appears in table
    await expect(page.locator('text=Test New User')).toBeVisible()
    
    // Search for the new user
    await page.fill('input[placeholder*="Search users"]', 'Test New User')
    await page.waitForTimeout(500)
    
    // Verify user found with correct details
    await expect(page.locator('text=Test New User')).toBeVisible()
    await expect(page.locator('text=newuser@kinhome.com')).toBeVisible()
  })

  test('validates required fields when creating user', async ({ page }) => {
    // Click Add User button
    await page.click('button:has-text("Add User")')
    
    // Leave name empty and try to create
    await page.fill('input[id="email"]', 'test@kinhome.com')
    await page.fill('input[id="quickbaseUserId"]', 'test-qb-123')
    await page.fill('input[id="temporaryPassword"]', 'temppass123')
    
    await page.click('button:has-text("Create User")')
    
    // Verify error toast
    await expect(page.locator('text=Name, email, Quickbase User ID, and temporary password are required')).toBeVisible()
    
    // Verify dialog stays open
    await expect(page.locator('text=Add New User')).toBeVisible()
  })

  test('prevents duplicate email', async ({ page }) => {
    // Click Add User button
    await page.click('button:has-text("Add User")')
    
    // Fill form with existing email
    await page.fill('input[id="name"]', 'Test User')
    await page.fill('input[id="email"]', 'closer@kinhome.com') // Existing email
    await page.fill('input[id="quickbaseUserId"]', 'test-qb-123')
    await page.fill('input[id="temporaryPassword"]', 'temppass123')
    
    await page.click('button:has-text("Create User")')
    
    // Verify error toast
    await expect(page.locator('text=Email already in use')).toBeVisible()
  })

  test('toggles user active status', async ({ page }) => {
    // Find first user in table
    const firstRow = page.locator('tbody tr').first()
    const statusSwitch = firstRow.locator('button[role="switch"]')
    
    // Note current state
    const isChecked = await statusSwitch.getAttribute('data-state')
    
    // Click switch
    await statusSwitch.click()
    
    // Wait for success toast
    await expect(page.locator('text=User status updated')).toBeVisible()
    
    // Verify switch toggled
    const newState = await statusSwitch.getAttribute('data-state')
    expect(newState).not.toBe(isChecked)
    
    // Refresh page to verify persistence
    await page.reload()
    await page.click('text=Users')
    
    // Verify status persisted
    const persistedState = await statusSwitch.getAttribute('data-state')
    expect(persistedState).toBe(newState)
  })

  test('resets user password', async ({ page }) => {
    // Find first user in table
    const firstRow = page.locator('tbody tr').first()
    const resetButton = firstRow.locator('button:has(svg)').last() // Key icon button
    
    // Click reset password button
    await resetButton.click()
    
    // Wait for success toast with temporary password
    await expect(page.locator('text=Temp password:')).toBeVisible()
    
    // Verify toast shows temporary password
    const toastText = await page.locator('[data-sonner-toast]').textContent()
    expect(toastText).toMatch(/Temp password: [A-Za-z0-9]+/)
  })

  test('prevents non-admin from accessing users tab', async ({ page }) => {
    // Sign out
    await page.click('button:has-text("Sign Out")')
    
    // Login as closer
    await page.goto('/login')
    await page.fill('input[name="email"]', 'closer@kinhome.com')
    await page.fill('input[name="password"]', 'closer123')
    await page.click('button[type="submit"]')
    
    // Navigate to settings
    await page.goto('/settings')
    
    // Verify Users tab is NOT visible
    await expect(page.locator('text=Users')).not.toBeVisible()
    
    // Verify only Profile and Notifications tabs visible
    await expect(page.locator('text=Profile')).toBeVisible()
    await expect(page.locator('text=Notifications')).toBeVisible()
  })

  test('prevents direct API access without admin role', async ({ page }) => {
    // Sign out
    await page.click('button:has-text("Sign Out")')
    
    // Login as closer
    await page.goto('/login')
    await page.fill('input[name="email"]', 'closer@kinhome.com')
    await page.fill('input[name="password"]', 'closer123')
    await page.click('button[type="submit"]')
    
    // Try to access admin API directly
    const response = await page.request.get('/api/admin/users')
    
    // Verify returns 403 Forbidden
    expect(response.status()).toBe(403)
  })
})
