// tests/integration/teamActivityFeed.spec.ts
import { test, expect } from '@playwright/test';
import { mockNextAuthSession } from '../utils/auth-helpers';
import { mockQuickBaseResponse } from '../utils/quickbase-helpers';

test.describe('Team Activity Feed Integration', () => {
  test.beforeEach(async ({ page }) => {
    // Mock NextAuth session for manager user
    await mockNextAuthSession(page, {
      user: {
        id: 'manager-123',
        quickbaseUserId: 'manager-123',
        email: 'manager@example.com',
        role: 'office_leader',
        salesOffice: ['Office A'],
      },
    });
  });

  test('API returns activities for manager', async ({ page }) => {
    // Mock QuickBase response with test projects
    const mockProjects = [
      {
        [1]: { value: 1 }, // RECORD_ID
        [2]: { value: 'P-12345' }, // PROJECT_ID
        [3]: { value: 'John Smith' }, // CUSTOMER_NAME
        [4]: { value: 'Active' }, // PROJECT_STATUS
        [5]: { value: 'Jane Doe' }, // CLOSER_NAME
        [6]: { value: null }, // SETTER_NAME
        [7]: { value: null }, // DATE_ON_HOLD
        [8]: { value: '2024-01-15T10:00:00Z' }, // INSTALL_COMPLETED_DATE
        [9]: { value: null }, // PTO_APPROVED
      },
      {
        [1]: { value: 2 }, // RECORD_ID
        [2]: { value: 'P-67890' }, // PROJECT_ID
        [3]: { value: 'Sarah Johnson' }, // CUSTOMER_NAME
        [4]: { value: 'Active' }, // PROJECT_STATUS
        [5]: { value: null }, // CLOSER_NAME
        [6]: { value: 'Bob Wilson' }, // SETTER_NAME
        [7]: { value: '2024-01-14T14:30:00Z' }, // DATE_ON_HOLD
        [8]: { value: null }, // INSTALL_COMPLETED_DATE
        [9]: { value: null }, // PTO_APPROVED
      },
    ];

    await mockQuickBaseResponse(page, '/api/dashboard/team-activity', {
      data: mockProjects,
    });

    // Make API request
    const response = await page.request.get('/api/dashboard/team-activity');
    
    expect(response.status()).toBe(200);
    const data = await response.json();
    
    expect(data.activities).toHaveLength(2);
    expect(data.totalCount).toBe(2);
    expect(data.hasMore).toBe(false);
    
    // Verify first activity
    expect(data.activities[0].recordId).toBe(1);
    expect(data.activities[0].activityType).toBe('install_completed');
    expect(data.activities[0].teamMemberName).toBe('Jane Doe');
    expect(data.activities[0].customerName).toBe('John Smith');
    
    // Verify second activity
    expect(data.activities[1].recordId).toBe(2);
    expect(data.activities[1].activityType).toBe('placed_on_hold');
    expect(data.activities[1].teamMemberName).toBe('Bob Wilson');
    expect(data.activities[1].customerName).toBe('Sarah Johnson');
  });

  test('API returns empty array for non-manager', async ({ page }) => {
    // Mock session for non-manager
    await mockNextAuthSession(page, {
      user: {
        id: 'closer-123',
        quickbaseUserId: 'closer-123',
        email: 'closer@example.com',
        role: 'closer',
      },
    });

    const response = await page.request.get('/api/dashboard/team-activity');
    
    expect(response.status()).toBe(200);
    const data = await response.json();
    
    expect(data.activities).toEqual([]);
    expect(data.totalCount).toBe(0);
    expect(data.hasMore).toBe(false);
  });

  test('API requires authentication', async ({ page }) => {
    // No session mock - unauthenticated request
    
    const response = await page.request.get('/api/dashboard/team-activity');
    
    expect(response.status()).toBe(401);
  });

  test('Component renders activities', async ({ page }) => {
    const mockActivities = [
      {
        recordId: 1,
        projectId: 'P-12345',
        customerName: 'John Smith',
        activityType: 'install_completed',
        activityDescription: 'Install completed',
        teamMemberName: 'Jane Doe',
        teamMemberRole: 'closer',
        timestamp: '2024-01-15T10:00:00Z',
        daysAgo: 2,
      },
      {
        recordId: 2,
        projectId: 'P-67890',
        customerName: 'Sarah Johnson',
        activityType: 'placed_on_hold',
        activityDescription: 'Placed on hold',
        teamMemberName: 'Bob Wilson',
        teamMemberRole: 'setter',
        timestamp: '2024-01-14T14:30:00Z',
        daysAgo: 3,
      },
    ];

    await mockQuickBaseResponse(page, '/api/dashboard/team-activity', {
      activities: mockActivities,
      totalCount: 2,
      hasMore: false,
    });

    await page.goto('/dashboard');

    // Wait for component to load
    await expect(page.locator('text=Team Activity')).toBeVisible();
    
    // Verify card title
    await expect(page.locator('h3:has-text("Team Activity")')).toBeVisible();
    
    // Verify activity items are rendered
    await expect(page.locator('text=Install completed')).toBeVisible();
    await expect(page.locator('text=Placed on hold')).toBeVisible();
    
    // Verify team member names
    await expect(page.locator('text=Jane Doe')).toBeVisible();
    await expect(page.locator('text=Bob Wilson')).toBeVisible();
    
    // Verify customer names
    await expect(page.locator('text=John Smith')).toBeVisible();
    await expect(page.locator('text=Sarah Johnson')).toBeVisible();
  });

  test('Component shows loading state', async ({ page }) => {
    // Mock delayed response
    await page.route('/api/dashboard/team-activity', async (route) => {
      await new Promise(resolve => setTimeout(resolve, 1000));
      await route.fulfill({
        status: 200,
        contentType: 'application/json',
        body: JSON.stringify({ activities: [], totalCount: 0, hasMore: false }),
      });
    });

    await page.goto('/dashboard');

    // Verify skeleton is visible during loading
    await expect(page.locator('[data-testid="team-activity-skeleton"]')).toBeVisible();
  });

  test('Component shows empty state', async ({ page }) => {
    await mockQuickBaseResponse(page, '/api/dashboard/team-activity', {
      activities: [],
      totalCount: 0,
      hasMore: false,
    });

    await page.goto('/dashboard');

    // Wait for component to load
    await expect(page.locator('text=Team Activity')).toBeVisible();
    
    // Verify empty state message
    await expect(page.locator('text=No recent team activity in the last 7 days.')).toBeVisible();
  });

  test('Component shows error state', async ({ page }) => {
    await page.route('/api/dashboard/team-activity', (route) => {
      route.fulfill({
        status: 500,
        contentType: 'application/json',
        body: JSON.stringify({ error: 'Internal Server Error' }),
      });
    });

    await page.goto('/dashboard');

    // Wait for component to load
    await expect(page.locator('text=Team Activity')).toBeVisible();
    
    // Verify error message
    await expect(page.locator('text=Unable to load team activity')).toBeVisible();
  });

  test('Activity items are clickable', async ({ page }) => {
    const mockActivities = [
      {
        recordId: 123,
        projectId: 'P-12345',
        customerName: 'John Smith',
        activityType: 'install_completed',
        activityDescription: 'Install completed',
        teamMemberName: 'Jane Doe',
        teamMemberRole: 'closer',
        timestamp: '2024-01-15T10:00:00Z',
        daysAgo: 2,
      },
    ];

    await mockQuickBaseResponse(page, '/api/dashboard/team-activity', {
      activities: mockActivities,
      totalCount: 1,
      hasMore: false,
    });

    await page.goto('/dashboard');

    // Wait for component to load
    await expect(page.locator('text=Team Activity')).toBeVisible();
    
    // Click on activity item
    await page.locator('text=Install completed').click();
    
    // Verify navigation to project detail page
    await expect(page).toHaveURL('/projects/123');
  });

  test('Activity icons match types', async ({ page }) => {
    const mockActivities = [
      {
        recordId: 1,
        projectId: 'P-1',
        customerName: 'Customer 1',
        activityType: 'install_completed',
        activityDescription: 'Install completed',
        teamMemberName: 'Closer 1',
        teamMemberRole: 'closer',
        timestamp: '2024-01-15T10:00:00Z',
        daysAgo: 2,
      },
      {
        recordId: 2,
        projectId: 'P-2',
        customerName: 'Customer 2',
        activityType: 'placed_on_hold',
        activityDescription: 'Placed on hold',
        teamMemberName: 'Setter 1',
        teamMemberRole: 'setter',
        timestamp: '2024-01-14T10:00:00Z',
        daysAgo: 3,
      },
      {
        recordId: 3,
        projectId: 'P-3',
        customerName: 'Customer 3',
        activityType: 'pto_approved',
        activityDescription: 'PTO approved',
        teamMemberName: 'Closer 2',
        teamMemberRole: 'closer',
        timestamp: '2024-01-13T10:00:00Z',
        daysAgo: 4,
      },
      {
        recordId: 4,
        projectId: 'P-4',
        customerName: 'Customer 4',
        activityType: 'cancelled',
        activityDescription: 'Project cancelled',
        teamMemberName: 'Setter 2',
        teamMemberRole: 'setter',
        timestamp: '2024-01-12T10:00:00Z',
        daysAgo: 5,
      },
    ];

    await mockQuickBaseResponse(page, '/api/dashboard/team-activity', {
      activities: mockActivities,
      totalCount: 4,
      hasMore: false,
    });

    await page.goto('/dashboard');

    // Wait for component to load
    await expect(page.locator('text=Team Activity')).toBeVisible();
    
    // Verify icons are present (using data attributes or classes)
    await expect(page.locator('[data-testid="activity-icon-install_completed"]')).toBeVisible();
    await expect(page.locator('[data-testid="activity-icon-placed_on_hold"]')).toBeVisible();
    await expect(page.locator('[data-testid="activity-icon-pto_approved"]')).toBeVisible();
    await expect(page.locator('[data-testid="activity-icon-cancelled"]')).toBeVisible();
  });

  test('Dashboard integration (manager only)', async ({ page }) => {
    const mockActivities = [
      {
        recordId: 1,
        projectId: 'P-12345',
        customerName: 'John Smith',
        activityType: 'install_completed',
        activityDescription: 'Install completed',
        teamMemberName: 'Jane Doe',
        teamMemberRole: 'closer',
        timestamp: '2024-01-15T10:00:00Z',
        daysAgo: 2,
      },
    ];

    await mockQuickBaseResponse(page, '/api/dashboard/team-activity', {
      activities: mockActivities,
      totalCount: 1,
      hasMore: false,
    });

    await page.goto('/dashboard');

    // Verify TeamActivityFeed component is rendered
    await expect(page.locator('text=Team Activity')).toBeVisible();
    
    // Verify component is in right column (below NotificationsFeed)
    const rightColumn = page.locator('.lg\\:col-span-1');
    await expect(rightColumn.locator('text=Team Activity')).toBeVisible();
    
    // Verify NotificationsFeed is also present
    await expect(page.locator('text=Recent Notifications')).toBeVisible();
  });

  test('Dashboard integration (non-manager)', async ({ page }) => {
    // Mock session for non-manager
    await mockNextAuthSession(page, {
      user: {
        id: 'closer-123',
        quickbaseUserId: 'closer-123',
        email: 'closer@example.com',
        role: 'closer',
      },
    });

    await page.goto('/dashboard');

    // Verify TeamActivityFeed component is NOT rendered
    await expect(page.locator('text=Team Activity')).not.toBeVisible();
    
    // Verify NotificationsFeed is still rendered
    await expect(page.locator('text=Recent Notifications')).toBeVisible();
  });

  test('Activity timestamp formatting', async ({ page }) => {
    const twoDaysAgo = new Date();
    twoDaysAgo.setDate(twoDaysAgo.getDate() - 2);
    
    const mockActivities = [
      {
        recordId: 1,
        projectId: 'P-12345',
        customerName: 'John Smith',
        activityType: 'install_completed',
        activityDescription: 'Install completed',
        teamMemberName: 'Jane Doe',
        teamMemberRole: 'closer',
        timestamp: twoDaysAgo.toISOString(),
        daysAgo: 2,
      },
    ];

    await mockQuickBaseResponse(page, '/api/dashboard/team-activity', {
      activities: mockActivities,
      totalCount: 1,
      hasMore: false,
    });

    await page.goto('/dashboard');

    // Wait for component to load
    await expect(page.locator('text=Team Activity')).toBeVisible();
    
    // Verify timestamp displays "2 days ago"
    await expect(page.locator('text=2 days ago')).toBeVisible();
  });
});
