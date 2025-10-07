import { test, expect } from '@playwright/test'

test.describe('Settings Page', () => {
  test.beforeEach(async ({ page }) => {
    // Navigate to login and authenticate
    await page.goto('/login')
    await page.fill('input[name="email"]', 'closer@kinhome.com')
    await page.fill('input[name="password"]', 'closer123')
    await page.click('button[type="submit"]')
    
    // Wait for redirect to dashboard
    await page.waitForURL('/')
    
    // Navigate to settings
    await page.goto('/settings')
    await page.waitForLoadState('networkidle')
  })

  test('displays profile tab by default', async ({ page }) => {
    await expect(page).toHaveURL('/settings')
    
    // Check that Profile tab is active
    const profileTab = page.locator('[role="tab"][data-state="active"]')
    await expect(profileTab).toContainText('Profile')
    
    // Check that profile form is visible
    await expect(page.locator('text=Profile Information')).toBeVisible()
    await expect(page.locator('text=Change Password')).toBeVisible()
    await expect(page.locator('text=Danger Zone')).toBeVisible()
  })

  test('updates profile information', async ({ page }) => {
    // Fill profile form
    await page.fill('input[id="name"]', 'John Updated')
    await page.fill('input[id="phone"]', '555-1234')
    
    // Click save button
    await page.click('button:has-text("Save Profile")')
    
    // Wait for success toast
    await expect(page.locator('text=Profile updated successfully')).toBeVisible()
    
    // Verify form shows updated values
    await expect(page.locator('input[id="name"]')).toHaveValue('John Updated')
    await expect(page.locator('input[id="phone"]')).toHaveValue('555-1234')
    
    // Refresh page and verify persistence
    await page.reload()
    await expect(page.locator('input[id="name"]')).toHaveValue('John Updated')
    await expect(page.locator('input[id="phone"]')).toHaveValue('555-1234')
  })

  test('validates profile form', async ({ page }) => {
    // Clear name field
    await page.fill('input[id="name"]', '')
    
    // Click save button
    await page.click('button:has-text("Save Profile")')
    
    // Check for validation error
    await expect(page.locator('text=Name and email are required')).toBeVisible()
  })

  test('changes password successfully', async ({ page }) => {
    // Fill password form
    await page.fill('input[id="current-password"]', 'closer123')
    await page.fill('input[id="new-password"]', 'newpassword123')
    await page.fill('input[id="confirm-password"]', 'newpassword123')
    
    // Click change password button
    await page.click('button:has-text("Change Password")')
    
    // Wait for success toast
    await expect(page.locator('text=Password changed successfully')).toBeVisible()
    
    // Verify password fields are cleared
    await expect(page.locator('input[id="current-password"]')).toHaveValue('')
    await expect(page.locator('input[id="new-password"]')).toHaveValue('')
    await expect(page.locator('input[id="confirm-password"]')).toHaveValue('')
  })

  test('validates password change', async ({ page }) => {
    // Fill with short password
    await page.fill('input[id="current-password"]', 'closer123')
    await page.fill('input[id="new-password"]', 'short')
    await page.fill('input[id="confirm-password"]', 'short')
    
    // Click change password button
    await page.click('button:has-text("Change Password")')
    
    // Check for validation error
    await expect(page.locator('text=Password must be at least 8 characters')).toBeVisible()
  })

  test('validates password match', async ({ page }) => {
    // Fill with mismatched passwords
    await page.fill('input[id="current-password"]', 'closer123')
    await page.fill('input[id="new-password"]', 'newpassword123')
    await page.fill('input[id="confirm-password"]', 'differentpassword123')
    
    // Click change password button
    await page.click('button:has-text("Change Password")')
    
    // Check for validation error
    await expect(page.locator('text=New passwords do not match')).toBeVisible()
  })

  test('switches to notifications tab', async ({ page }) => {
    // Click Notifications tab
    await page.click('text=Notifications')
    
    // Check that notifications form is visible
    await expect(page.locator('text=Email Notifications')).toBeVisible()
    await expect(page.locator('text=Test Notifications')).toBeVisible()
    
    // Check that email toggles are visible
    await expect(page.locator('text=Enable Email Notifications')).toBeVisible()
    await expect(page.locator('text=Urgent Alerts')).toBeVisible()
  })

  test('updates notification settings', async ({ page }) => {
    // Click Notifications tab
    await page.click('text=Notifications')
    
    // Toggle email notifications
    await page.click('input[id="email-enabled"]')
    await page.click('input[id="urgent-alerts"]')
    
    // Click save button
    await page.click('button:has-text("Save Notification Settings")')
    
    // Wait for success toast
    await expect(page.locator('text=Notification settings updated')).toBeVisible()
    
    // Refresh and verify persistence
    await page.reload()
    await page.click('text=Notifications')
    
    // Check that toggles reflect saved state
    await expect(page.locator('input[id="email-enabled"]')).toBeChecked()
    await expect(page.locator('input[id="urgent-alerts"]')).toBeChecked()
  })

  test('sends test notification', async ({ page }) => {
    // Click Notifications tab
    await page.click('text=Notifications')
    
    // Click test notification button
    await page.click('button:has-text("Send Test Email")')
    
    // Wait for success toast
    await expect(page.locator('text=Test notification sent! Check your email.')).toBeVisible()
  })

  test('shows alert thresholds for office leaders', async ({ page }) => {
    // Sign out and login as office leader
    await page.click('text=Sign out')
    await page.waitForURL('/login')
    
    await page.fill('input[name="email"]', 'office@kinhome.com')
    await page.fill('input[name="password"]', 'office123')
    await page.click('button[type="submit"]')
    
    await page.waitForURL('/')
    await page.goto('/settings')
    
    // Click Notifications tab
    await page.click('text=Notifications')
    
    // Check that Alert Thresholds card is visible
    await expect(page.locator('text=Alert Thresholds')).toBeVisible()
    await expect(page.locator('input[id="hold-threshold"]')).toBeVisible()
    await expect(page.locator('input[id="age-warning-threshold"]')).toBeVisible()
    await expect(page.locator('input[id="install-overdue-threshold"]')).toBeVisible()
    
    // Update hold threshold
    await page.fill('input[id="hold-threshold"]', '10')
    await page.click('button:has-text("Save Notification Settings")')
    
    // Verify settings updated
    await expect(page.locator('text=Notification settings updated')).toBeVisible()
  })

  test('hides alert thresholds for closers', async ({ page }) => {
    // Click Notifications tab
    await page.click('text=Notifications')
    
    // Check that Alert Thresholds card is NOT visible
    await expect(page.locator('text=Alert Thresholds')).not.toBeVisible()
  })

  test('shows only profile and notifications tabs for closers', async ({ page }) => {
    // Check that only 2 tabs are visible
    const tabs = page.locator('[role="tab"]')
    await expect(tabs).toHaveCount(2)
    
    await expect(page.locator('text=Profile')).toBeVisible()
    await expect(page.locator('text=Notifications')).toBeVisible()
    
    // Check that admin tabs are not visible
    await expect(page.locator('text=Users')).not.toBeVisible()
    await expect(page.locator('text=Offices')).not.toBeVisible()
    await expect(page.locator('text=System')).not.toBeVisible()
    await expect(page.locator('text=Audit Logs')).not.toBeVisible()
  })

  test('shows all tabs for super admin', async ({ page }) => {
    // Sign out and login as super admin
    await page.click('text=Sign out')
    await page.waitForURL('/login')
    
    await page.fill('input[name="email"]', 'admin@kinhome.com')
    await page.fill('input[name="password"]', 'admin123')
    await page.click('button[type="submit"]')
    
    await page.waitForURL('/')
    await page.goto('/settings')
    
    // Check that all tabs are visible
    await expect(page.locator('text=Profile')).toBeVisible()
    await expect(page.locator('text=Notifications')).toBeVisible()
    await expect(page.locator('text=Users')).toBeVisible()
    await expect(page.locator('text=Offices')).toBeVisible()
    await expect(page.locator('text=System')).toBeVisible()
    await expect(page.locator('text=Audit Logs')).toBeVisible()
    
    // Click each admin tab and verify placeholder content
    await page.click('text=Users')
    await expect(page.locator('text=Coming soon in Phase 5B')).toBeVisible()
    
    await page.click('text=Offices')
    await expect(page.locator('text=Coming soon in Phase 5B')).toBeVisible()
    
    await page.click('text=System')
    await expect(page.locator('text=Coming soon in Phase 5C')).toBeVisible()
    
    await page.click('text=Audit Logs')
    await expect(page.locator('text=Coming soon in Phase 5D')).toBeVisible()
  })
})
