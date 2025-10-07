import { test, expect } from '@playwright/test'

test.describe('System Settings (Super Admin)', () => {
  test.beforeEach(async ({ page }) => {
    await page.goto('/login')
    await page.fill('input[type="email"]', 'admin@kinhome.com')
    await page.fill('input[type="password"]', 'admin123')
    await page.click('button[type="submit"]')
    await page.waitForURL('**/settings')
    await page.getByRole('tab', { name: 'System' }).click()
  })

  test('displays system settings form with all cards', async ({ page }) => {
    await expect(page.getByText('Quickbase Connection')).toBeVisible()
    await expect(page.getByText('Milestone SLA Defaults')).toBeVisible()
    await expect(page.getByText('Hold Reasons')).toBeVisible()
    await expect(page.getByText('General Settings')).toBeVisible()
    await expect(page.getByRole('button', { name: 'Save System Settings' })).toBeVisible()
  })

  test('loads current system settings', async ({ page }) => {
    await expect(page.getByLabel('Realm Hostname')).toHaveValue(/.+/) // not empty
    await expect(page.getByLabel('User Token')).toHaveValue(/\*+/)
    const inputs = ['survey','design','permit','nem','install','inspection','pto']
    for (const key of inputs) {
      await expect(page.locator(`#sla-${key}`)).toHaveAttribute('type', 'number')
    }
    await expect(page.locator('#warning-threshold')).toHaveValue(/\d+/)
    await expect(page.locator('#critical-threshold')).toHaveValue(/\d+/)
    await expect(page.getByText(/Hold Reasons/i)).toBeVisible()
    await expect(page.getByRole('combobox').first()).toBeVisible()
    await expect(page.getByLabel('Session Timeout (minutes)')).toHaveValue(/\d+/)
  })

  test('updates milestone SLA defaults', async ({ page }) => {
    await page.fill('#sla-survey', '')
    await page.type('#sla-survey', '10')
    await page.fill('#sla-design', '')
    await page.type('#sla-design', '14')
    await page.getByRole('button', { name: 'Save System Settings' }).click()
    await expect(page.getByText('System settings updated')).toBeVisible()
    await page.reload()
    await page.getByRole('tab', { name: 'System' }).click()
    await expect(page.locator('#sla-survey')).toHaveValue('10')
    await expect(page.locator('#sla-design')).toHaveValue('14')
  })

  test('validates SLA input ranges', async ({ page }) => {
    await page.fill('#sla-survey', '0')
    await page.getByRole('button', { name: 'Save System Settings' }).click()
    // Expect API validation error toast
    await expect(page.getByText(/Validation failed|Failed to update/i)).toBeVisible()
    await page.fill('#sla-permit', '100')
    await page.getByRole('button', { name: 'Save System Settings' }).click()
    await expect(page.getByText(/Validation failed|Failed to update/i)).toBeVisible()
  })

  test('adds new hold reason', async ({ page }) => {
    const initialCount = await page.locator('[data-state] .badge, .inline-flex.items-center.gap-2 .badge').count().catch(() => 0)
    await page.getByPlaceholder('Add a hold reason').fill('Equipment Hold')
    await page.keyboard.press('Enter')
    await expect(page.getByText('Equipment Hold')).toBeVisible()
    await page.getByRole('button', { name: 'Save System Settings' }).click()
    await expect(page.getByText('System settings updated')).toBeVisible()
    await page.reload()
    await page.getByRole('tab', { name: 'System' }).click()
    await expect(page.getByText('Equipment Hold')).toBeVisible()
  })

  test('removes hold reason', async ({ page }) => {
    const badge = page.locator('text=Finance Hold').first()
    const exists = await badge.isVisible().catch(() => false)
    if (exists) {
      await badge.click()
      await page.getByRole('button', { name: 'Save System Settings' }).click()
      await expect(page.getByText('System settings updated')).toBeVisible()
      await page.reload()
      await page.getByRole('tab', { name: 'System' }).click()
      await expect(page.locator('text=Finance Hold')).toHaveCount(0)
    }
  })

  test('prevents duplicate hold reasons', async ({ page }) => {
    const reason = 'Roof Hold'
    await page.getByPlaceholder('Add a hold reason').fill(reason)
    await page.keyboard.press('Enter')
    await expect(page.getByText('Hold reason already exists')).toBeVisible()
  })

  test('updates general settings', async ({ page }) => {
    await page.getByText('Date Format').click()
    await page.getByRole('option', { name: 'DD/MM/YYYY' }).click()
    await page.getByText('Timezone').click()
    await page.getByRole('option', { name: 'America/Los_Angeles' }).click()
    await page.fill('#session-timeout', '')
    await page.type('#session-timeout', '120')
    await page.getByRole('button', { name: 'Save System Settings' }).click()
    await expect(page.getByText('System settings updated')).toBeVisible()
    await page.reload()
    await page.getByRole('tab', { name: 'System' }).click()
    await expect(page.getByRole('combobox').first()).toContainText('DD/MM/YYYY')
    await expect(page.getByText('America/Los_Angeles')).toBeVisible()
    await expect(page.locator('#session-timeout')).toHaveValue('120')
  })
})

test.describe('System Settings RBAC', () => {
  test('prevents non-admin from accessing system tab', async ({ page }) => {
    await page.goto('/login')
    await page.fill('input[type="email"]', 'closer@kinhome.com')
    await page.fill('input[type="password"]', 'closer123')
    await page.click('button[type="submit"]')
    await page.waitForURL('**/settings')
    await expect(page.getByRole('tab', { name: 'System' })).toHaveCount(0)
  })
})



