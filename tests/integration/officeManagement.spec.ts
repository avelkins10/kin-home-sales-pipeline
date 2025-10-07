import { test, expect } from '@playwright/test'

test.describe('Office Management (Super Admin)', () => {
  test.beforeEach(async ({ page }) => {
    await page.goto('/login')
    await page.fill('input[name="email"]', 'admin@kinhome.com')
    await page.fill('input[name="password"]', 'admin123')
    await page.click('button[type="submit"]')
    await page.waitForURL('/')
    await page.goto('/settings')
    await page.click('text=Offices')
  })

  test('displays offices grid with office cards', async ({ page }) => {
    await expect(page.locator('h2')).toContainText('Office Management')
    await expect(page.locator('text=Manage sales offices and their leadership')).toBeVisible()
    await expect(page.locator('button:has-text("Add Office")')).toBeVisible()
  })

  test('creates new office successfully', async ({ page }) => {
    await page.click('button:has-text("Add Office")')
    await expect(page.locator('text=Add New Office')).toBeVisible()

    await page.fill('#officeName', 'Test Office Phoenix')
    await page.click('[role="combobox"]')
    await page.click('text=Southwest')

    // Select first leader option (if any)
    const leaderDropdown = page.locator('[role="combobox"]').nth(1)
    await leaderDropdown.click()
    const options = page.locator('[role="option"]')
    if (await options.count()) {
      await options.first().click()
    }

    await page.click('button:has-text("Create Office")')
    await expect(page.locator('text=Office created successfully')).toBeVisible()
    await expect(page.locator('text=Test Office Phoenix')).toBeVisible()
  })

  test('validates required fields when creating office', async ({ page }) => {
    await page.click('button:has-text("Add Office")')
    await page.click('button:has-text("Create Office")')
    await expect(page.locator('text=All fields are required')).toBeVisible()
  })

  test('prevents duplicate office name', async ({ page }) => {
    await page.click('button:has-text("Add Office")')
    await page.fill('#officeName', 'Duplicate Office Test')
    await page.click('[role="combobox"]')
    await page.click('text=Southwest')

    const leaderDropdown = page.locator('[role="combobox"]').nth(1)
    await leaderDropdown.click()
    const options = page.locator('[role="option"]')
    if (await options.count()) {
      await options.first().click()
    }

    await page.click('button:has-text("Create Office")')
    await expect(page.locator('text=Office created successfully')).toBeVisible()

    await page.click('button:has-text("Add Office")')
    await page.fill('#officeName', 'Duplicate Office Test')
    await page.click('[role="combobox"]')
    await page.click('text=Southwest')
    await leaderDropdown.click()
    if (await options.count()) {
      await options.first().click()
    }
    await page.click('button:has-text("Create Office")')
    await expect(page.locator('text=Office name already exists')).toBeVisible()
  })

  test('deletes office with confirmation', async ({ page }) => {
    // Ensure there is an office to delete
    await page.click('button:has-text("Add Office")')
    await page.fill('#officeName', 'Office To Delete')
    await page.click('[role="combobox"]')
    await page.click('text=Southwest')
    const leaderDropdown = page.locator('[role="combobox"]').nth(1)
    await leaderDropdown.click()
    const options = page.locator('[role="option"]')
    if (await options.count()) {
      await options.first().click()
    }
    await page.click('button:has-text("Create Office")')
    await expect(page.locator('text=Office created successfully')).toBeVisible()

    // Open delete dialog
    const card = page.locator('text=Office To Delete').first()
    const cardContainer = card.locator('..').locator('..').locator('..').locator('..')
    await cardContainer.locator('button:has(svg)').last().click()

    // Confirm dialog
    await expect(page.locator('text=Delete Office')).toBeVisible()
    await page.click('button:has-text("Delete")')

    await expect(page.locator('text=Office deleted successfully')).toBeVisible()
    await expect(page.locator('text=Office To Delete')).not.toBeVisible()
  })

  test('prevents non-admin from accessing offices tab', async ({ page }) => {
    // Sign out if button exists
    const signOutBtn = page.locator('button:has-text("Sign Out")')
    if (await signOutBtn.count()) {
      await signOutBtn.click()
    }

    await page.goto('/login')
    await page.fill('input[name="email"]', 'closer@kinhome.com')
    await page.fill('input[name="password"]', 'closer123')
    await page.click('button[type="submit"]')
    await page.waitForURL('/')
    await page.goto('/settings')

    await expect(page.locator('text=Offices')).not.toBeVisible()
    await expect(page.locator('text=Profile')).toBeVisible()
    await expect(page.locator('text=Notifications')).toBeVisible()
  })
})
