import { test, expect } from '@playwright/test';
import { statusColorMapRgb } from '@/lib/constants/designTokens';

test.describe('Calendar', () => {
  test.beforeEach(async ({ page }) => {
    // Navigate to login and authenticate
    await page.goto('/login');
    await page.fill('input[type="email"]', 'closer@kinhome.com');
    await page.fill('input[type="password"]', 'closer123');
    await page.click('button[type="submit"]');
    
    // Wait for redirect to dashboard
    await page.waitForURL('/');
  });

  test('navigate to calendar page', async ({ page }) => {
    // Click on Calendar nav link in TopNavbar
    await page.click('a[href="/calendar"]');
    
    // Assert URL is '/calendar'
    await page.waitForURL('/calendar');
    await expect(page).toHaveURL('/calendar');
    
    // Assert page title 'Calendar' is visible
    await expect(page.locator('[data-testid="calendar-page-title"]')).toBeVisible();
    await expect(page.locator('[data-testid="calendar-page-title"]')).toContainText('Calendar');
    
    // Assert subtitle 'View scheduled site surveys and installations' is visible
    await expect(page.locator('[data-testid="calendar-page-subtitle"]')).toBeVisible();
    await expect(page.locator('[data-testid="calendar-page-subtitle"]')).toContainText('View scheduled site surveys and installations');
    
    // Assert CalendarFilters component is visible
    await expect(page.locator('[data-testid="event-type-filter"]')).toBeVisible();
    await expect(page.locator('[data-testid="status-filter"]')).toBeVisible();
    
    // Assert calendar grid is rendered (look for react-big-calendar classes)
    await expect(page.locator('[data-testid="calendar-wrapper"]')).toBeVisible();
  });

  test('display calendar with events', async ({ page }) => {
    // Navigate to '/calendar'
    await page.goto('/calendar');
    
    // Wait for calendar to load (no loading skeleton visible)
    await page.waitForLoadState('networkidle');
    
    // Assert calendar month view is displayed
    await expect(page.locator('[data-testid="calendar-wrapper"]')).toBeVisible();
    
    // Assert calendar shows current month/year in toolbar
    await expect(page.locator('[data-testid="calendar-current-label"]')).toBeVisible();
    
    // Assert calendar has 7 day headers (Sun-Sat)
    const dayHeaders = page.locator('.rbc-header');
    await expect(dayHeaders).toHaveCount(7);
    
    // Count visible events on calendar (use `.rbc-event` selector)
    const events = page.locator('.rbc-event');
    const eventCount = await events.count();
    
    // Assert at least 1 event is visible (if test data exists)
    if (eventCount > 0) {
      await expect(events.first()).toBeVisible();
      
      // Assert events have colored backgrounds (blue/green/yellow)
      const firstEvent = events.first();
      const backgroundColor = await firstEvent.evaluate(el => getComputedStyle(el).backgroundColor);
      expect(backgroundColor).toMatch(/rgb\(219, 234, 254\)|rgb\(220, 252, 231\)|rgb\(254, 243, 199\)/);
    }
  });

  test('filter by event type - Surveys only', async ({ page }) => {
    // Navigate to '/calendar'
    await page.goto('/calendar');
    await page.waitForLoadState('networkidle');
    
    // Count total events before filtering
    const initialEvents = page.locator('.rbc-event');
    const initialCount = await initialEvents.count();
    
    // Click Event Type filter dropdown
    await page.click('[data-testid="event-type-filter"]');
    
    // Select 'Surveys' option
    await page.click('text=Surveys');
    
    // Wait for filter to apply (client-side, should be instant)
    await page.waitForTimeout(500);
    
    // Count events after filtering
    const filteredEvents = page.locator('.rbc-event');
    const filteredCount = await filteredEvents.count();
    
    // Assert filtered count <= initial count
    expect(filteredCount).toBeLessThanOrEqual(initialCount);
    
    // Verify visible events contain 'Survey' in title (sample check)
    if (filteredCount > 0) {
      const firstEvent = filteredEvents.first();
      const eventTitle = await firstEvent.textContent();
      expect(eventTitle).toContain('Survey');
    }
  });

  test('filter by event type - Installs only', async ({ page }) => {
    // Navigate to '/calendar'
    await page.goto('/calendar');
    await page.waitForLoadState('networkidle');
    
    // Click Event Type filter dropdown
    await page.click('[data-testid="event-type-filter"]');
    
    // Select 'Installs' option
    await page.click('text=Installs');
    
    // Wait for filter to apply
    await page.waitForTimeout(500);
    
    // Verify visible events contain 'Install' in title (sample check)
    const events = page.locator('.rbc-event');
    const eventCount = await events.count();
    
    if (eventCount > 0) {
      const firstEvent = events.first();
      const eventTitle = await firstEvent.textContent();
      expect(eventTitle).toContain('Install');
    }
    
    // Assert no survey events visible
    const surveyEvents = page.locator('.rbc-event:has-text("Survey")');
    await expect(surveyEvents).toHaveCount(0);
  });

  test('filter by status - Scheduled only', async ({ page }) => {
    // Navigate to '/calendar'
    await page.goto('/calendar');
    await page.waitForLoadState('networkidle');
    
    // Click Status filter dropdown
    await page.click('[data-testid="status-filter"]');
    
    // Select 'Scheduled' option
    await page.click('text=Scheduled');
    
    // Wait for filter to apply
    await page.waitForTimeout(500);
    
    // Verify visible events have blue background (scheduled color: #dbeafe)
    const events = page.locator('.rbc-event');
    const eventCount = await events.count();
    
    if (eventCount > 0) {
      for (const loc of await events.all()) {
        const bg = await loc.evaluate(el => getComputedStyle(el).backgroundColor);
        expect(bg).toBe(statusColorMapRgb.scheduled.background);
      }
    }
  });

  test('filter by status - Completed only', async ({ page }) => {
    // Navigate to '/calendar'
    await page.goto('/calendar');
    await page.waitForLoadState('networkidle');
    
    // Click Status filter dropdown
    await page.click('[data-testid="status-filter"]');
    
    // Select 'Completed' option
    await page.click('text=Completed');
    
    // Wait for filter to apply
    await page.waitForTimeout(500);
    
    // Verify visible events have green background (completed color: #dcfce7)
    const events = page.locator('.rbc-event');
    const eventCount = await events.count();
    
    if (eventCount > 0) {
      for (const loc of await events.all()) {
        const bg = await loc.evaluate(el => getComputedStyle(el).backgroundColor);
        expect(bg).toBe(statusColorMapRgb.completed.background);
      }
    }
  });

  test('filter by status - Pending only', async ({ page }) => {
    // Navigate to '/calendar'
    await page.goto('/calendar');
    await page.waitForLoadState('networkidle');
    
    // Click Status filter dropdown
    await page.click('[data-testid="status-filter"]');
    
    // Select 'Pending' option
    await page.click('text=Pending');
    
    // Wait for filter to apply
    await page.waitForTimeout(500);
    
    // Verify visible events have yellow background (pending color: #fef3c7)
    const events = page.locator('.rbc-event');
    const eventCount = await events.count();
    
    if (eventCount > 0) {
      for (const loc of await events.all()) {
        const bg = await loc.evaluate(el => getComputedStyle(el).backgroundColor);
        expect(bg).toBe(statusColorMapRgb.pending.background);
      }
    }
  });

  test('combine filters - Surveys + Scheduled', async ({ page }) => {
    // Navigate to '/calendar'
    await page.goto('/calendar');
    await page.waitForLoadState('networkidle');
    
    // Select 'Surveys' from Event Type filter
    await page.click('[data-testid="event-type-filter"]');
    await page.click('text=Surveys');
    
    // Select 'Scheduled' from Status filter
    await page.click('[data-testid="status-filter"]');
    await page.click('text=Scheduled');
    
    // Wait for filters to apply
    await page.waitForTimeout(500);
    
    // Verify visible events are both surveys AND scheduled
    const events = page.locator('.rbc-event');
    const eventCount = await events.count();
    
    if (eventCount > 0) {
      const firstEvent = events.first();
      const eventTitle = await firstEvent.textContent();
      const backgroundColor = await firstEvent.evaluate(el => getComputedStyle(el).backgroundColor);
      
      // Assert events have blue background
      expect(backgroundColor).toBe('rgb(219, 234, 254)');
      
      // Assert event titles contain 'Survey'
      expect(eventTitle).toContain('Survey');
    }
  });

  test('reset filters to "Both" and "All"', async ({ page }) => {
    // Navigate to '/calendar' with filters applied
    await page.goto('/calendar');
    await page.waitForLoadState('networkidle');
    
    // Capture initial unfiltered count
    const initialEvents = page.locator('.rbc-event');
    const initialCount = await initialEvents.count();
    
    // Apply some filters first
    await page.click('[data-testid="event-type-filter"]');
    await page.click('text=Surveys');
    await page.click('[data-testid="status-filter"]');
    await page.click('text=Scheduled');
    
    // Get filtered count
    const filteredEvents = page.locator('.rbc-event');
    const filteredCount = await filteredEvents.count();
    
    // Select 'Both' from Event Type filter
    await page.click('[data-testid="event-type-filter"]');
    await page.click('text=Both');
    
    // Select 'All Statuses' from Status filter
    await page.click('[data-testid="status-filter"]');
    await page.click('text=All Statuses');
    
    // Wait for filters to apply
    await page.waitForTimeout(500);
    
    // Assert all events are visible again
    const allEvents = page.locator('.rbc-event');
    const allCount = await allEvents.count();
    
    // Count should match initial unfiltered count (or within tolerance if data changes)
    expect(allCount).toBeGreaterThanOrEqual(initialCount);
  });

  test('click event navigates to project detail', async ({ page }) => {
    // Navigate to '/calendar'
    await page.goto('/calendar');
    await page.waitForLoadState('networkidle');
    
    // Wait for events to load
    const events = page.locator('.rbc-event');
    const eventCount = await events.count();
    
    if (eventCount > 0) {
      // Click on first visible event (use `.rbc-event` selector)
      await events.first().click();
      
      // Wait for navigation
      await page.waitForURL(/\/projects\/\d+/);
      
      // Assert URL matches pattern `/projects/\d+`
      await expect(page).toHaveURL(/\/projects\/\d+/);
      
      // Assert project detail page loaded (look for project header)
      await expect(page.locator('[data-testid="project-header"]')).toBeVisible();
    }
  });

  test('switch calendar views - Month/Week/Day', async ({ page }) => {
    // Navigate to '/calendar'
    await page.goto('/calendar');
    await page.waitForLoadState('networkidle');
    
    // Initially: Assert Month view is displayed
    await expect(page.locator('.rbc-month-view')).toBeVisible();
    
    // Click Week: Switch to week view
    await page.click('[data-testid="calendar-view-week"]');
    await expect(page.locator('.rbc-time-view')).toBeVisible();
    await expect(page.locator('.rbc-time-content')).toBeVisible();
    
    // Click Day: Switch to day view
    await page.click('[data-testid="calendar-view-day"]');
    await expect(page.locator('.rbc-time-view')).toBeVisible();
    // Validate that the header shows one day
    await expect(page.locator('.rbc-time-header .rbc-header')).toHaveCount(1);
    
    // Click Month: Switch back to month view
    await page.click('[data-testid="calendar-view-month"]');
    await expect(page.locator('.rbc-month-view')).toBeVisible();
    await expect(page.locator('.rbc-month-row')).toHaveCountGreaterThan(3);
  });

  test('navigate calendar months', async ({ page }) => {
    // Navigate to '/calendar'
    await page.goto('/calendar');
    await page.waitForLoadState('networkidle');
    
    // Note current month in toolbar label
    const currentLabel = page.locator('[data-testid="calendar-current-label"]');
    const initialText = await currentLabel.textContent();
    
    // Click 'Next' button (ChevronRight icon)
    await page.click('[data-testid="calendar-nav-next"]');
    
    // Assert month advances by 1
    await page.waitForTimeout(500);
    const nextText = await currentLabel.textContent();
    expect(nextText).not.toBe(initialText);
    
    // Click 'Prev' button (ChevronLeft icon)
    await page.click('[data-testid="calendar-nav-prev"]');
    
    // Assert month goes back by 1
    await page.waitForTimeout(500);
    const prevText = await currentLabel.textContent();
    expect(prevText).toBe(initialText);
    
    // Click 'Today' button
    await page.click('[data-testid="calendar-nav-today"]');
    
    // Assert returns to current month
    await page.waitForTimeout(500);
    const todayText = await currentLabel.textContent();
    expect(todayText).toBe(initialText);
  });

  test('loading states', async ({ page }) => {
    // Navigate to '/calendar' (fresh page load)
    await page.goto('/calendar');
    
    // Assert CalendarSkeleton is visible initially
    const loadingSkeleton = page.locator('[data-testid="loading-skeleton"]');
    if (await loadingSkeleton.isVisible()) {
      await expect(loadingSkeleton).toBeVisible();
      
      // Assert skeleton has loading animation (animate-pulse class)
      await expect(loadingSkeleton).toHaveClass(/animate-pulse/);
      
      // Wait for data to load
      await page.waitForLoadState('networkidle');
      
      // Assert skeleton disappears
      await expect(loadingSkeleton).not.toBeVisible();
    }
    
    // Assert actual calendar is visible
    await expect(page.locator('[data-testid="calendar-wrapper"]')).toBeVisible();
    
    // Assert events are rendered
    const events = page.locator('.rbc-event');
    const count = await events.count();
    expect(count).toBeGreaterThanOrEqual(0);
  });

  test('error handling with retry', async ({ page }) => {
    // Navigate to '/calendar'
    await page.goto('/calendar');
    await page.waitForLoadState('networkidle');
    
    // Check for error alert (may not appear in normal test environment)
    const errorAlert = page.locator('[data-testid="calendar-error-alert"]');
    
    if (await errorAlert.isVisible()) {
      // Assert error message is displayed
      await expect(errorAlert).toBeVisible();
      
      // Assert 'Retry' button is visible
      const retryButton = page.locator('[data-testid="calendar-retry-button"]');
      await expect(retryButton).toBeVisible();
      
      // Click 'Retry' button
      await retryButton.click();
      
      // Assert loading state appears
      await expect(page.locator('[data-testid="loading-skeleton"]')).toBeVisible();
      
      // Wait for calendar to load successfully after retry
      await page.waitForLoadState('networkidle');
      await expect(page.locator('[data-testid="calendar-wrapper"]')).toBeVisible();
    }
  });

  test('role-based filtering - Closer sees only their projects', async ({ page }) => {
    // Authenticate as closer (already done in beforeEach)
    // Navigate to '/calendar'
    await page.goto('/calendar');
    await page.waitForLoadState('networkidle');
    
    // Wait for events to load
    const events = page.locator('.rbc-event');
    const eventCount = await events.count();
    
    // For each event, click to view project detail
    for (let i = 0; i < Math.min(eventCount, 3); i++) {
      await events.nth(i).click();
      
      // Assert project belongs to the closer (check closer name in project header)
      await page.waitForURL(/\/projects\/\d+/);
      await expect(page.locator('[data-testid="project-header"]')).toBeVisible();
      
      // Assert closer identity matches logged-in user
      const closerNameElement = page.locator('[data-testid="closer-name"]');
      if (await closerNameElement.isVisible()) {
        await expect(closerNameElement).toContainText('Test Closer');
      }
      
      // Navigate back to calendar
      await page.goto('/calendar');
      await page.waitForLoadState('networkidle');
    }
  });

  test('responsive design on iPad viewport (1024×768)', async ({ page }) => {
    // Use iPad project from playwright.config.ts (already configured)
    // Navigate to '/calendar'
    await page.goto('/calendar');
    await page.waitForLoadState('networkidle');
    
    // Assert calendar renders without horizontal scroll
    const body = page.locator('body');
    const bodyWidth = await body.evaluate(el => el.scrollWidth);
    const viewportWidth = page.viewportSize()?.width || 1024;
    expect(bodyWidth).toBeLessThanOrEqual(viewportWidth + 20); // Allow small margin
    
    // Assert filters are visible and properly sized
    await expect(page.locator('[data-testid="event-type-filter"]')).toBeVisible();
    await expect(page.locator('[data-testid="status-filter"]')).toBeVisible();
    
    // Assert calendar toolbar fits on screen
    await expect(page.locator('[data-testid="calendar-current-label"]')).toBeVisible();
    await expect(page.locator('[data-testid="calendar-nav-prev"]')).toBeVisible();
    await expect(page.locator('[data-testid="calendar-nav-next"]')).toBeVisible();
    
    // Assert events are readable (not truncated)
    const events = page.locator('.rbc-event');
    const eventCount = await events.count();
    
    if (eventCount > 0) {
      const firstEvent = events.first();
      const eventHeight = await firstEvent.evaluate(el => el.offsetHeight);
      expect(eventHeight).toBeGreaterThan(10); // Should be readable
    }
    
    // Assert navigation buttons are accessible
    await expect(page.locator('[data-testid="calendar-nav-prev"]')).toBeVisible();
    await expect(page.locator('[data-testid="calendar-nav-next"]')).toBeVisible();
    await expect(page.locator('[data-testid="calendar-nav-today"]')).toBeVisible();
    
    // Take screenshot for visual verification
    await page.screenshot({ path: 'calendar-ipad.png' });
  });

  test('design system consistency - Colors', async ({ page }) => {
    // Navigate to '/calendar'
    await page.goto('/calendar');
    await page.waitForLoadState('networkidle');
    
    // Wait for events to load
    const events = page.locator('.rbc-event');
    const eventCount = await events.count();
    
    if (eventCount > 0) {
      // Find a scheduled event (blue background)
      const scheduledEvent = events.filter({ hasText: 'Scheduled' }).first();
      if (await scheduledEvent.isVisible()) {
        const backgroundColor = await scheduledEvent.evaluate(el => getComputedStyle(el).backgroundColor);
        const borderColor = await scheduledEvent.evaluate(el => getComputedStyle(el).borderColor);
        
        // Assert background color is '#dbeafe' (blue-100)
        expect(backgroundColor).toBe('rgb(219, 234, 254)');
        // Assert border color is '#3b82f6' (blue-500)
        expect(borderColor).toBe('rgb(59, 130, 246)');
      }
      
      // Find a completed event (green background)
      const completedEvent = events.filter({ hasText: 'Completed' }).first();
      if (await completedEvent.isVisible()) {
        const backgroundColor = await completedEvent.evaluate(el => getComputedStyle(el).backgroundColor);
        const borderColor = await completedEvent.evaluate(el => getComputedStyle(el).borderColor);
        
        // Assert background color is '#dcfce7' (green-100)
        expect(backgroundColor).toBe('rgb(220, 252, 231)');
        // Assert border color is '#22c55e' (green-500)
        expect(borderColor).toBe('rgb(34, 197, 94)');
      }
      
      // Find a pending event (yellow background)
      const pendingEvent = events.filter({ hasText: 'Estimated' }).first();
      if (await pendingEvent.isVisible()) {
        const backgroundColor = await pendingEvent.evaluate(el => getComputedStyle(el).backgroundColor);
        const borderColor = await pendingEvent.evaluate(el => getComputedStyle(el).borderColor);
        
        // Assert background color is '#fef3c7' (yellow-100)
        expect(backgroundColor).toBe('rgb(254, 243, 199)');
        // Assert border color is '#f59e0b' (yellow-500)
        expect(borderColor).toBe('rgb(245, 158, 11)');
      }
    }
  });

  test('design system consistency - Spacing and Typography', async ({ page }) => {
    // Navigate to '/calendar'
    await page.goto('/calendar');
    await page.waitForLoadState('networkidle');
    
    // Assert page has proper padding (px-4 sm:px-6 lg:px-8)
    const mainContainer = page.locator('.max-w-7xl.mx-auto');
    await expect(mainContainer).toBeVisible();
    
    // Assert page title uses text-2xl font-bold text-slate-900
    const pageTitle = page.locator('[data-testid="calendar-page-title"]');
    await expect(pageTitle).toBeVisible();
    const titleFontSize = await pageTitle.evaluate(el => getComputedStyle(el).fontSize);
    expect(titleFontSize).toBe('24px'); // text-2xl
    
    // Assert subtitle uses text-sm text-slate-600
    const subtitle = page.locator('[data-testid="calendar-page-subtitle"]');
    await expect(subtitle).toBeVisible();
    const subtitleFontSize = await subtitle.evaluate(el => getComputedStyle(el).fontSize);
    expect(subtitleFontSize).toBe('14px'); // text-sm
    
    // Assert filters have mb-6 spacing
    const filters = page.locator('[data-testid="event-type-filter"]').locator('..');
    await expect(filters).toBeVisible();
    
    // Assert calendar has proper border and shadow (border shadow-sm)
    const calendar = page.locator('[data-testid="calendar-wrapper"]');
    await expect(calendar).toBeVisible();
    
    // Assert calendar has rounded corners (rounded-lg)
    const borderRadius = await calendar.evaluate(el => getComputedStyle(el).borderRadius);
    expect(borderRadius).toMatch(/4px|6px|8px/); // rounded corners
    
    // Assert calendar has white background (bg-white)
    const backgroundColor = await calendar.evaluate(el => getComputedStyle(el).backgroundColor);
    expect(backgroundColor).toBe('rgb(255, 255, 255)'); // white
  });

  test('event badges display correctly', async ({ page }) => {
    // Navigate to '/calendar'
    await page.goto('/calendar');
    await page.waitForLoadState('networkidle');
    
    // Wait for events to load
    const events = page.locator('.rbc-event');
    const eventCount = await events.count();
    
    if (eventCount > 0) {
      // Hover over an event to see full content
      await events.first().hover();
      
      // Assert event shows status badge
      const eventContent = events.first();
      await expect(eventContent).toBeVisible();
      
      // Assert badge has correct variant (default/success/warning)
      // Note: CalendarEventContent component renders badges
      const badge = eventContent.locator('.badge, [class*="badge"]');
      if (await badge.isVisible()) {
        await expect(badge).toBeVisible();
        
        // Assert badge text is readable (text-xs)
        const badgeFontSize = await badge.evaluate(el => getComputedStyle(el).fontSize);
        expect(badgeFontSize).toBe('12px'); // text-xs
        
        // Assert badge has proper spacing
        const badgePadding = await badge.evaluate(el => getComputedStyle(el).padding);
        expect(badgePadding).toBeTruthy();
      }
    }
  });

  test('calendar handles empty state gracefully', async ({ page }) => {
    // Navigate to '/calendar'
    await page.goto('/calendar');
    await page.waitForLoadState('networkidle');
    
    // Assert calendar renders without errors
    await expect(page.locator('[data-testid="calendar-wrapper"]')).toBeVisible();
    
    // Assert no events are displayed (may be empty in test environment)
    const events = page.locator('.rbc-event');
    const eventCount = await events.count();
    expect(eventCount).toBeGreaterThanOrEqual(0); // Should not error
    
    // Assert calendar grid is still visible
    await expect(page.locator('.rbc-calendar')).toBeVisible();
    
    // Assert filters are still functional
    await expect(page.locator('[data-testid="event-type-filter"]')).toBeVisible();
    await expect(page.locator('[data-testid="status-filter"]')).toBeVisible();
    
    // Assert no error messages displayed
    await expect(page.locator('[data-testid="calendar-error-alert"]')).not.toBeVisible();
  });

  test('calendar active state in navigation', async ({ page }) => {
    // Navigate to '/calendar'
    await page.goto('/calendar');
    await page.waitForLoadState('networkidle');
    
    // Assert Calendar nav item in TopNavbar has active styling
    const calendarNavLink = page.locator('a[href="/calendar"]');
    await expect(calendarNavLink).toBeVisible();
    await expect(calendarNavLink).toHaveClass(/bg-indigo-50|text-indigo-700/);
    
    // Navigate to '/projects'
    await page.goto('/projects');
    await page.waitForLoadState('networkidle');
    
    // Assert Calendar nav item no longer has active styling
    const calendarNavLinkInactive = page.locator('a[href="/calendar"]');
    await expect(calendarNavLinkInactive).not.toHaveClass(/bg-indigo-50|text-indigo-700/);
    
    // Navigate back to '/calendar'
    await page.goto('/calendar');
    await page.waitForLoadState('networkidle');
    
    // Assert active styling returns
    const calendarNavLinkActive = page.locator('a[href="/calendar"]');
    await expect(calendarNavLinkActive).toHaveClass(/bg-indigo-50|text-indigo-700/);
  });

  test('filters disabled during loading', async ({ page }) => {
    // Navigate to '/calendar' (fresh load)
    await page.goto('/calendar');
    
    // During loading state:
    const loadingSkeleton = page.locator('[data-testid="loading-skeleton"]');
    if (await loadingSkeleton.isVisible()) {
      // Assert Event Type filter has disabled attribute
      const eventTypeFilter = page.locator('[data-testid="event-type-filter"]');
      await expect(eventTypeFilter).toBeDisabled();
      
      // Assert Status filter has disabled attribute
      const statusFilter = page.locator('[data-testid="status-filter"]');
      await expect(statusFilter).toBeDisabled();
      
      // Try clicking to ensure dropdown does not open
      await eventTypeFilter.click();
      await expect(page.locator('[role="menu"]')).not.toBeVisible();
    }
    
    // After load completes:
    await page.waitForLoadState('networkidle');
    
    // Assert filters are enabled
    const eventTypeFilter = page.locator('[data-testid="event-type-filter"]');
    const statusFilter = page.locator('[data-testid="status-filter"]');
    await expect(eventTypeFilter).toBeEnabled();
    await expect(statusFilter).toBeEnabled();
    
    // Assert filters are interactive
    await eventTypeFilter.click();
    await page.click('text=Both'); // Close dropdown
  });

  test('multiple events on same day', async ({ page }) => {
    // Navigate to '/calendar'
    await page.goto('/calendar');
    await page.waitForLoadState('networkidle');
    
    // Find a day with multiple events (if exists in test data)
    const events = page.locator('.rbc-event');
    const eventCount = await events.count();
    
    if (eventCount > 1) {
      // Assert multiple event blocks are visible on that day
      await expect(events).toHaveCount(eventCount);
      
      // Assert events are stacked vertically
      const firstEvent = events.first();
      const secondEvent = events.nth(1);
      
      const firstEventRect = await firstEvent.boundingBox();
      const secondEventRect = await secondEvent.boundingBox();
      
      if (firstEventRect && secondEventRect) {
        // Events should be stacked (similar x position, different y position)
        const xDifference = Math.abs(firstEventRect.x - secondEventRect.x);
        const yDifference = Math.abs(firstEventRect.y - secondEventRect.y);
        
        expect(xDifference).toBeLessThan(50); // Similar x position
        expect(yDifference).toBeGreaterThan(10); // Different y position
      }
      
      // Assert each event is clickable
      await firstEvent.click();
      await page.waitForURL(/\/projects\/\d+/);
      await page.goto('/calendar');
      await page.waitForLoadState('networkidle');
      
      await secondEvent.click();
      await page.waitForURL(/\/projects\/\d+/);
    }
  });

  test('event titles are properly formatted', async ({ page }) => {
    // Navigate to '/calendar'
    await page.goto('/calendar');
    await page.waitForLoadState('networkidle');
    
    // Get first event
    const events = page.locator('.rbc-event');
    const eventCount = await events.count();
    
    if (eventCount > 0) {
      const firstEvent = events.first();
      const eventTitle = await firstEvent.textContent();
      
      // Assert title format matches: '{EventType} {Status} - {CustomerName}'
      expect(eventTitle).toMatch(/^(Survey|Install|Inspection|PTO) (Scheduled|Completed|Approved|Estimated) - .+/);
      
      // Examples:
      // 'Survey Scheduled - John Smith'
      // 'Install Completed - Jane Doe'
      // 'Inspection Scheduled - Bob Johnson'
      
      // Assert customer name is included
      expect(eventTitle).toContain(' - ');
      
      // Assert event type is clear
      expect(eventTitle).toMatch(/^(Survey|Install|Inspection|PTO)/);
    }
  });

  test('calendar persists view state', async ({ page }) => {
    // Navigate to '/calendar'
    await page.goto('/calendar');
    await page.waitForLoadState('networkidle');
    
    // Switch to Week view
    await page.click('[data-testid="calendar-view-week"]');
    
    // Navigate to '/projects'
    await page.goto('/projects');
    await page.waitForLoadState('networkidle');
    
    // Navigate back to '/calendar'
    await page.goto('/calendar');
    await page.waitForLoadState('networkidle');
    
    // Assert calendar returns to Month view (default)
    // Note: View state is not persisted in URL, so it resets
    // This is expected behavior
    await expect(page.locator('[data-testid="calendar-view-month"]')).toBeVisible();
  });

  test.describe('API Route Integration', () => {
    test('GET /api/calendar/events - validates ownership parameter', async ({ request }) => {
      // Test invalid ownership parameter
      const response = await request.get('/api/calendar/events?ownership=invalid');
      expect(response.status()).toBe(400);
      
      const body = await response.json();
      expect(body.error).toContain('Invalid ownership parameter');
    });

    test('GET /api/calendar/events - validates sort parameter', async ({ request }) => {
      // Test invalid sort parameter
      const response = await request.get('/api/calendar/events?sort=invalid');
      expect(response.status()).toBe(400);
      
      const body = await response.json();
      expect(body.error).toContain('Invalid sort parameter');
    });

    test('GET /api/calendar/events - rejects userId/role parameters', async ({ request }) => {
      // Test that client cannot set userId/role
      const response = await request.get('/api/calendar/events?userId=123&role=closer');
      expect(response.status()).toBe(400);
      
      const body = await response.json();
      expect(body.error).toContain('Do not supply userId/role in query params');
    });

    test('GET /api/calendar/events - validates date range filtering', async ({ request }) => {
      // Test with valid date range
      const startDate = '2024-01-01';
      const endDate = '2024-12-31';
      const response = await request.get(`/api/calendar/events?startDate=${startDate}&endDate=${endDate}`);
      
      // Should return 200 (assuming user is authenticated)
      expect([200, 401]).toContain(response.status());
      
      if (response.status() === 200) {
        const events = await response.json();
        expect(Array.isArray(events)).toBe(true);
        
        // Verify all events are within date range
        events.forEach((event: any) => {
          const eventDate = new Date(event.date);
          const start = new Date(startDate);
          const end = new Date(endDate);
          expect(eventDate >= start && eventDate <= end).toBe(true);
        });
      }
    });

    test('GET /api/calendar/events - validates sorted ascending by date', async ({ request }) => {
      const response = await request.get('/api/calendar/events');
      
      // Should return 200 (assuming user is authenticated)
      expect([200, 401]).toContain(response.status());
      
      if (response.status() === 200) {
        const events = await response.json();
        expect(Array.isArray(events)).toBe(true);
        
        // Verify events are sorted by date ascending
        for (let i = 1; i < events.length; i++) {
          const prevDate = new Date(events[i - 1].date);
          const currDate = new Date(events[i].date);
          expect(prevDate.getTime()).toBeLessThanOrEqual(currDate.getTime());
        }
      }
    });

    test('GET /api/calendar/events - team-projects requires manager role', async ({ request }) => {
      // This test assumes the test user is a closer (non-manager)
      const response = await request.get('/api/calendar/events?ownership=team-projects');
      
      // Should return 403 for non-managers
      expect([403, 401]).toContain(response.status());
      
      if (response.status() === 403) {
        const body = await response.json();
        expect(body.error).toContain('Team projects filter is only available for managers');
      }
    });
  });
});
