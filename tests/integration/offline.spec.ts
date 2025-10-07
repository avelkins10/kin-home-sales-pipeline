import { test, expect } from '@playwright/test';

test.describe('Offline Support', () => {
  test.beforeEach(async ({ page }) => {
    // Navigate to login page
    await page.goto('/login');

    // Fill credentials and submit
    await page.fill('input[name="email"]', 'addison.r@kinhome.com');
    await page.fill('input[name="password"]', 'password');
    await page.click('button[type="submit"]');

    // Wait for dashboard to load
    await page.waitForURL('/');

    // Navigate to a project detail page
    await page.goto('/projects');

    // Check if projects exist
    const hasProjects = await page.locator('[data-testid="project-row"]').count() > 0;
    if (!hasProjects) {
      throw new Error('No projects found. Please seed the database with test projects for addison.r@kinhome.com');
    }

    await page.click('[data-testid="project-row"]:first-child');
    await page.waitForURL(/\/projects\/\d+/);
  });

  test('displays offline indicator when going offline', async ({ page }) => {
    // Set browser to offline
    await page.context().setOffline(true);
    
    // Wait for offline indicator to appear
    await expect(page.locator('text=You are offline')).toBeVisible();
    await expect(page.locator('[data-testid="wifi-off-icon"]')).toBeVisible();
    
    // Set browser back online
    await page.context().setOffline(false);
    
    // Wait for indicator to disappear
    await expect(page.locator('text=You are offline')).not.toBeVisible();
  });

  test('queues hold update when offline', async ({ page }) => {
    // Set browser to offline
    await page.context().setOffline(true);
    
    // Click 'Place on Hold' button
    await page.click('button:has-text("Place on Hold")');
    
    // Fill hold reason
    await page.fill('textarea[placeholder*="reason"]', 'Test offline hold');
    
    // Submit
    await page.click('button:has-text("Place on Hold")');
    
    // Verify success toast shows 'queued for sync' message
    await expect(page.locator('text=queued for sync')).toBeVisible();
    
    // Verify hold banner appears with optimistic update
    await expect(page.locator('text=Project on Hold')).toBeVisible();
    
    // Verify offline indicator shows pending update
    await expect(page.locator('text=1 pending update')).toBeVisible();
    
    // Check IndexedDB for queued mutation
    const queuedMutations = await page.evaluate(async () => {
      const dbName = 'kin-solar-db';
      const storeName = 'mutations';

      return new Promise((resolve, reject) => {
        const request = indexedDB.open(dbName);
        request.onsuccess = () => {
          const db = request.result;
          const transaction = db.transaction([storeName], 'readonly');
          const store = transaction.objectStore(storeName);
          const getAllRequest = store.getAll();

          getAllRequest.onsuccess = () => resolve(getAllRequest.result);
          getAllRequest.onerror = () => reject(getAllRequest.error);
        };
        request.onerror = () => reject(request.error);
      });
    });
    
    expect(queuedMutations).toHaveLength(1);
    expect(queuedMutations[0].type).toBe('hold-update');
  });

  test('syncs queued mutations when coming back online', async ({ page }) => {
    // Set browser to offline
    await page.context().setOffline(true);
    
    // Queue a hold update
    await page.click('button:has-text("Place on Hold")');
    await page.fill('textarea[placeholder*="reason"]', 'Test sync hold');
    await page.click('button:has-text("Place on Hold")');
    
    // Verify mutation is queued
    await expect(page.locator('text=queued for sync')).toBeVisible();
    
    // Set browser back online
    await page.context().setOffline(false);
    
    // Wait for sync to complete
    await expect(page.locator('text=Syncing')).toBeVisible();
    await expect(page.locator('text=Synced')).toBeVisible();
    
    // Verify offline indicator disappears
    await expect(page.locator('text=You are offline')).not.toBeVisible();
    
    // Verify hold update was applied
    await expect(page.locator('text=Project on Hold')).toBeVisible();
    
    // Verify IndexedDB queue is empty
    const queuedMutations = await page.evaluate(async () => {
      const dbName = 'kin-solar-db';
      const storeName = 'mutations';

      return new Promise((resolve, reject) => {
        const request = indexedDB.open(dbName);
        request.onsuccess = () => {
          const db = request.result;
          const transaction = db.transaction([storeName], 'readonly');
          const store = transaction.objectStore(storeName);
          const getAllRequest = store.getAll();

          getAllRequest.onsuccess = () => resolve(getAllRequest.result);
          getAllRequest.onerror = () => reject(getAllRequest.error);
        };
        request.onerror = () => reject(request.error);
      });
    });
    
    expect(queuedMutations).toHaveLength(0);
  });

  test('shows cached data when offline', async ({ page }) => {
    // Load project detail page while online (caches data)
    await page.waitForSelector('[data-testid="project-header"]');
    
    // Set browser to offline
    await page.context().setOffline(true);
    
    // Reload page
    await page.reload();
    
    // Verify project data is still displayed (from cache)
    await expect(page.locator('[data-testid="project-header"]')).toBeVisible();
    await expect(page.locator('[data-testid="timeline"]')).toBeVisible();
    await expect(page.locator('[data-testid="customer-contact"]')).toBeVisible();
    
    // Verify offline indicator is shown
    await expect(page.locator('text=You are offline')).toBeVisible();
  });

  test('handles multiple queued mutations', async ({ page }) => {
    // Set browser to offline
    await page.context().setOffline(true);
    
    // Queue 3 hold updates on different projects
    for (let i = 0; i < 3; i++) {
      await page.click('button:has-text("Place on Hold")');
      await page.fill('textarea[placeholder*="reason"]', `Test hold ${i + 1}`);
      await page.click('button:has-text("Place on Hold")');
      
      // Navigate to next project if not the last iteration
      if (i < 2) {
        await page.goto('/projects');
        await page.click(`[data-testid="project-row"]:nth-child(${i + 2})`);
        await page.waitForURL(/\/projects\/\d+/);
      }
    }
    
    // Verify offline indicator shows '3 pending updates'
    await expect(page.locator('text=3 pending updates')).toBeVisible();
    
    // Set browser back online
    await page.context().setOffline(false);
    
    // Wait for all syncs to complete
    await expect(page.locator('text=Syncing 3 pending')).toBeVisible();
    await expect(page.locator('text=Synced')).toBeVisible();
    
    // Verify all updates were applied
    await expect(page.locator('text=Project on Hold')).toBeVisible();
    
    // Verify queue is empty
    const queuedMutations = await page.evaluate(async () => {
      const dbName = 'kin-solar-db';
      const storeName = 'mutations';

      return new Promise((resolve, reject) => {
        const request = indexedDB.open(dbName);
        request.onsuccess = () => {
          const db = request.result;
          const transaction = db.transaction([storeName], 'readonly');
          const store = transaction.objectStore(storeName);
          const getAllRequest = store.getAll();

          getAllRequest.onsuccess = () => resolve(getAllRequest.result);
          getAllRequest.onerror = () => reject(getAllRequest.error);
        };
        request.onerror = () => reject(request.error);
      });
    });
    
    expect(queuedMutations).toHaveLength(0);
  });

  test('retries failed mutations', async ({ page }) => {
    // Set browser to offline
    await page.context().setOffline(true);
    
    // Queue a hold update
    await page.click('button:has-text("Place on Hold")');
    await page.fill('textarea[placeholder*="reason"]', 'Test retry hold');
    await page.click('button:has-text("Place on Hold")');
    
    // Mock API to return 500 error on first attempt
    await page.route('**/api/projects/*/hold', route => {
      route.fulfill({
        status: 500,
        contentType: 'application/json',
        body: JSON.stringify({ error: 'Internal Server Error' })
      });
    });
    
    // Set browser back online
    await page.context().setOffline(false);
    
    // Wait for retry
    await expect(page.locator('text=Some offline changes failed')).toBeVisible();
    
    // Mock API to return success on second attempt
    await page.route('**/api/projects/*/hold', route => {
      route.fulfill({
        status: 200,
        contentType: 'application/json',
        body: JSON.stringify({ success: true })
      });
    });
    
    // Trigger manual sync
    await page.evaluate(() => {
      window.dispatchEvent(new Event('online'));
    });
    
    // Verify mutation succeeds after retry
    await expect(page.locator('text=Synced')).toBeVisible();
  });

  test('handles cache expiration', async ({ page }) => {
    // Load project data (caches with 5-minute TTL)
    await page.waitForSelector('[data-testid="project-header"]');
    
    // Manipulate time to make cache stale
    await page.evaluate(() => {
      // Mock Date.now to return time 6 minutes in the future
      const originalNow = Date.now;
      Date.now = () => originalNow() + 6 * 60 * 1000;
    });
    
    // Set browser to offline
    await page.context().setOffline(true);
    
    // Reload page
    await page.reload();
    
    // Verify stale cache is not used
    await expect(page.locator('text=No cached project')).toBeVisible();
    
    // Verify appropriate error message is shown
    await expect(page.locator('text=You are offline')).toBeVisible();
  });

  // Helper functions
  async function getIndexedDBData(page: any, storeName: string) {
    return await page.evaluate(async (store: string) => {
      const db = await indexedDB.open('kin-solar-db', 1);
      return new Promise((resolve) => {
        const transaction = db.transaction([store], 'readonly');
        const objectStore = transaction.objectStore(store);
        const request = objectStore.getAll();
        request.onsuccess = () => resolve(request.result);
      });
    }, storeName);
  }

  async function clearIndexedDB(page: any) {
    await page.evaluate(async () => {
      const dbName = 'kin-solar-db';
      const storeName = 'mutations';

      return new Promise((resolve, reject) => {
        const request = indexedDB.open(dbName);
        request.onsuccess = () => {
          const db = request.result;
          const transaction = db.transaction([storeName], 'readwrite');
          const store = transaction.objectStore(storeName);
          const clearRequest = store.clear();

          clearRequest.onsuccess = () => resolve();
          clearRequest.onerror = () => reject(clearRequest.error);
        };
        request.onerror = () => reject(request.error);
      });
    });
  }

  async function waitForSync(page: any) {
    await expect(page.locator('text=Syncing')).toBeVisible();
    await expect(page.locator('text=Synced')).toBeVisible();
  }

  async function mockAPIResponse(page: any, status: number, body: any) {
    await page.route('**/api/**', route => {
      route.fulfill({
        status,
        contentType: 'application/json',
        body: JSON.stringify(body)
      });
    });
  }

  test.afterEach(async ({ page }) => {
    // Cleanup
    await clearIndexedDB(page);
    await page.context().setOffline(false);
    
    // Clear service worker cache
    await page.evaluate(async () => {
      if ('caches' in window) {
        const cacheNames = await caches.keys();
        await Promise.all(
          cacheNames.map(cacheName => caches.delete(cacheName))
        );
      }
    });
  });
});
