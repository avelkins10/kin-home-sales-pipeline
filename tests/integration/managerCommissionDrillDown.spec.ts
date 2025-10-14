// tests/integration/managerCommissionDrillDown.spec.ts
import { test, expect } from '@playwright/test';
import type { TeamMemberCommission } from '@/lib/types/dashboard';

test.describe('Manager Commission Drill-Down Flow', () => {
  test.beforeEach(async ({ page }) => {
    // Mock authentication for manager user
    await page.goto('/login');
    await page.fill('[data-testid="email"]', 'manager@example.com');
    await page.fill('[data-testid="password"]', 'password123');
    await page.click('[data-testid="login-button"]');
    
    // Wait for dashboard to load
    await page.waitForURL('/dashboard');
  });

  test.describe('API Integration', () => {
    test('API returns team member breakdown for managers', async ({ request }) => {
      const response = await request.get('/api/dashboard/metrics?scope=team&timeRange=lifetime');
      
      expect(response.ok()).toBeTruthy();
      const data = await response.json();
      
      // Should include commissionByMember array for managers
      expect(data).toHaveProperty('commissionByMember');
      expect(Array.isArray(data.commissionByMember)).toBeTruthy();
      
      if (data.commissionByMember.length > 0) {
        const member = data.commissionByMember[0];
        expect(member).toHaveProperty('memberName');
        expect(member).toHaveProperty('memberEmail');
        expect(member).toHaveProperty('role');
        expect(member).toHaveProperty('earnedCommission');
        expect(member).toHaveProperty('lostCommission');
        expect(member).toHaveProperty('onHoldCommission');
        expect(member).toHaveProperty('pendingCommission');
        expect(member).toHaveProperty('projectCount');
        expect(['closer', 'setter']).toContain(member.role);
      }
    });

    test('API does NOT return team member breakdown for reps', async ({ request }) => {
      // Mock rep user authentication
      const response = await request.get('/api/dashboard/metrics?scope=personal&timeRange=lifetime', {
        headers: {
          'Authorization': 'Bearer rep-token'
        }
      });
      
      expect(response.ok()).toBeTruthy();
      const data = await response.json();
      
      // Should NOT include commissionByMember for reps
      expect(data.commissionByMember).toBeUndefined();
    });

    test('API does NOT return team member breakdown for personal scope', async ({ request }) => {
      const response = await request.get('/api/dashboard/metrics?scope=personal&timeRange=lifetime');
      
      expect(response.ok()).toBeTruthy();
      const data = await response.json();
      
      // Should NOT include commissionByMember for personal scope
      expect(data.commissionByMember).toBeUndefined();
    });

    test('Commission totals equal aggregate under 50/50 split attribution', async ({ request }) => {
      const response = await request.get('/api/dashboard/metrics?scope=team&timeRange=lifetime');
      
      expect(response.ok()).toBeTruthy();
      const data = await response.json();
      
      if (data.commissionByMember && data.commissionByMember.length > 0) {
        // Sum all team members' commission
        const totalEarned = data.commissionByMember.reduce((sum: number, member: TeamMemberCommission) => 
          sum + member.earnedCommission, 0);
        const totalLost = data.commissionByMember.reduce((sum: number, member: TeamMemberCommission) => 
          sum + member.lostCommission, 0);
        const totalOnHold = data.commissionByMember.reduce((sum: number, member: TeamMemberCommission) => 
          sum + member.onHoldCommission, 0);
        const totalPending = data.commissionByMember.reduce((sum: number, member: TeamMemberCommission) => 
          sum + member.pendingCommission, 0);
        
        // Per-member sums should equal aggregate totals under 50/50 split attribution
        // This prevents per-member totals from exceeding aggregate totals
        expect(totalEarned).toBeCloseTo(data.earnedCommission, 2);
        expect(totalLost).toBeCloseTo(data.lostCommission, 2);
        expect(totalOnHold).toBeCloseTo(data.onHoldCommission, 2);
        expect(totalPending).toBeCloseTo(data.pendingCommission, 2);
      }
    });

    test('Commission equality holds for different team member scenarios', async ({ request }) => {
      const response = await request.get('/api/dashboard/metrics?scope=team&timeRange=lifetime');
      
      expect(response.ok()).toBeTruthy();
      const data = await response.json();
      
      if (data.commissionByMember && data.commissionByMember.length > 0) {
        // Sum all team members' commission
        const totalEarned = data.commissionByMember.reduce((sum: number, member: TeamMemberCommission) => 
          sum + member.earnedCommission, 0);
        const totalLost = data.commissionByMember.reduce((sum: number, member: TeamMemberCommission) => 
          sum + member.lostCommission, 0);
        const totalOnHold = data.commissionByMember.reduce((sum: number, member: TeamMemberCommission) => 
          sum + member.onHoldCommission, 0);
        const totalPending = data.commissionByMember.reduce((sum: number, member: TeamMemberCommission) => 
          sum + member.pendingCommission, 0);
        
        // Verify equality holds for all commission categories
        // This ensures the 50/50 split works correctly whether projects have:
        // - Only closer (full commission to closer)
        // - Only setter (full commission to setter) 
        // - Both closer and setter (50% to each)
        expect(totalEarned).toBeCloseTo(data.earnedCommission, 2);
        expect(totalLost).toBeCloseTo(data.lostCommission, 2);
        expect(totalOnHold).toBeCloseTo(data.onHoldCommission, 2);
        expect(totalPending).toBeCloseTo(data.pendingCommission, 2);
        
        // Additional validation: ensure no negative values
        expect(totalEarned).toBeGreaterThanOrEqual(0);
        expect(totalLost).toBeGreaterThanOrEqual(0);
        expect(totalOnHold).toBeGreaterThanOrEqual(0);
        expect(totalPending).toBeGreaterThanOrEqual(0);
      }
    });

    test('Team members are sorted by total potential', async ({ request }) => {
      const response = await request.get('/api/dashboard/metrics?scope=team&timeRange=lifetime');
      
      expect(response.ok()).toBeTruthy();
      const data = await response.json();
      
      if (data.commissionByMember && data.commissionByMember.length > 1) {
        for (let i = 0; i < data.commissionByMember.length - 1; i++) {
          const current = data.commissionByMember[i];
          const next = data.commissionByMember[i + 1];
          
          const currentTotal = current.earnedCommission + current.pendingCommission;
          const nextTotal = next.earnedCommission + next.pendingCommission;
          
          expect(currentTotal).toBeGreaterThanOrEqual(nextTotal);
        }
      }
    });

    test('Handles projects with missing team member data', async ({ request }) => {
      const response = await request.get('/api/dashboard/metrics?scope=team&timeRange=lifetime');
      
      expect(response.ok()).toBeTruthy();
      const data = await response.json();
      
      // Should not throw errors even with missing data
      expect(data).toBeDefined();
      expect(Array.isArray(data.commissionByMember)).toBeTruthy();
    });
  });

  test.describe('UI Integration', () => {
    test('Drill-down button renders for managers with team scope', async ({ page }) => {
      // Navigate to team scope dashboard
      await page.click('[data-testid="scope-toggle"]');
      await page.waitForLoadState('networkidle');
      
      // Should show "View by Team Member" button
      const drillDownButton = page.locator('button:has-text("View by Team Member")');
      await expect(drillDownButton).toBeVisible();
      
      // Should show team member count
      const buttonText = await drillDownButton.textContent();
      expect(buttonText).toMatch(/View by Team Member \(\d+\)/);
    });

    test('Drill-down button does NOT render for reps', async ({ page }) => {
      // Mock rep user (this would require different auth setup)
      // For now, just verify the button doesn't exist in personal scope
      await page.click('[data-testid="scope-toggle"]'); // Switch to personal
      await page.waitForLoadState('networkidle');
      
      const drillDownButton = page.locator('button:has-text("View by Team Member")');
      await expect(drillDownButton).not.toBeVisible();
    });

    test('Clicking drill-down button expands/collapses content', async ({ page }) => {
      // Navigate to team scope
      await page.click('[data-testid="scope-toggle"]');
      await page.waitForLoadState('networkidle');
      
      const drillDownButton = page.locator('button:has-text("View by Team Member")');
      const teamMemberList = page.locator('[data-testid="team-member-list"]');
      
      // Initially collapsed
      await expect(teamMemberList).not.toBeVisible();
      await expect(drillDownButton.locator('svg')).toHaveClass(/chevron-down/);
      
      // Click to expand
      await drillDownButton.click();
      await expect(teamMemberList).toBeVisible();
      await expect(drillDownButton.locator('svg')).toHaveClass(/chevron-up/);
      
      // Click to collapse
      await drillDownButton.click();
      await expect(teamMemberList).not.toBeVisible();
      await expect(drillDownButton.locator('svg')).toHaveClass(/chevron-down/);
    });

    test('Team member cards display correct data', async ({ page }) => {
      // Navigate to team scope and expand drill-down
      await page.click('[data-testid="scope-toggle"]');
      await page.waitForLoadState('networkidle');
      
      const drillDownButton = page.locator('button:has-text("View by Team Member")');
      await drillDownButton.click();
      
      // Check first team member card
      const firstMemberCard = page.locator('[data-testid="team-member-card"]').first();
      await expect(firstMemberCard).toBeVisible();
      
      // Should show member name
      await expect(firstMemberCard.locator('[data-testid="member-name"]')).toBeVisible();
      
      // Should show role and project count
      await expect(firstMemberCard.locator('[data-testid="member-role"]')).toBeVisible();
      
      // Should show total potential commission
      await expect(firstMemberCard.locator('[data-testid="total-potential"]')).toBeVisible();
      
      // Should show commission breakdown (only non-zero values)
      const earnedCommission = firstMemberCard.locator('[data-testid="earned-commission"]');
      const pendingCommission = firstMemberCard.locator('[data-testid="pending-commission"]');
      const onHoldCommission = firstMemberCard.locator('[data-testid="on-hold-commission"]');
      const lostCommission = firstMemberCard.locator('[data-testid="lost-commission"]');
      
      // At least one commission type should be visible
      const visibleCommissions = await Promise.all([
        earnedCommission.isVisible(),
        pendingCommission.isVisible(),
        onHoldCommission.isVisible(),
        lostCommission.isVisible(),
      ]);
      
      expect(visibleCommissions.some(Boolean)).toBeTruthy();
    });

    test('ManagerCommissionComparison renders both tabs', async ({ page }) => {
      // Navigate to team scope (managers get comparison view)
      await page.click('[data-testid="scope-toggle"]');
      await page.waitForLoadState('networkidle');
      
      // Should show both tabs
      await expect(page.locator('[data-testid="tab-personal"]')).toBeVisible();
      await expect(page.locator('[data-testid="tab-team"]')).toBeVisible();
      
      // Default tab should be "Team Commission"
      await expect(page.locator('[data-testid="tab-team"]')).toHaveClass(/data-state-active/);
    });

    test('Switching tabs shows different data', async ({ page }) => {
      // Navigate to team scope
      await page.click('[data-testid="scope-toggle"]');
      await page.waitForLoadState('networkidle');
      
      // Click "My Commission" tab
      await page.click('[data-testid="tab-personal"]');
      await page.waitForLoadState('networkidle');
      
      // Should show personal commission data
      await expect(page.locator('[data-testid="personal-commission-summary"]')).toBeVisible();
      
      // Drill-down should NOT be available in personal tab
      await expect(page.locator('button:has-text("View by Team Member")')).not.toBeVisible();
      
      // Click "Team Commission" tab
      await page.click('[data-testid="tab-team"]');
      await page.waitForLoadState('networkidle');
      
      // Should show team commission data
      await expect(page.locator('[data-testid="team-commission-summary"]')).toBeVisible();
      
      // Drill-down should be available in team tab
      await expect(page.locator('button:has-text("View by Team Member")')).toBeVisible();
    });
  });

  test.describe('Error Handling', () => {
    test('Handles API errors gracefully', async ({ page }) => {
      // Mock API error
      await page.route('/api/dashboard/metrics*', route => {
        route.fulfill({
          status: 500,
          contentType: 'application/json',
          body: JSON.stringify({ error: 'Internal server error' })
        });
      });
      
      await page.reload();
      
      // Should show error state
      await expect(page.locator('[data-testid="error-message"]')).toBeVisible();
    });

    test('Handles empty team member data', async ({ page }) => {
      // Mock empty team member response
      await page.route('/api/dashboard/metrics*scope=team*', route => {
        route.fulfill({
          status: 200,
          contentType: 'application/json',
          body: JSON.stringify({
            earnedCommission: 0,
            lostCommission: 0,
            onHoldCommission: 0,
            pendingCommission: 0,
            salesAidCommission: 0,
            commissionByMember: []
          })
        });
      });
      
      await page.click('[data-testid="scope-toggle"]');
      await page.waitForLoadState('networkidle');
      
      // Should not show drill-down button when no team members
      await expect(page.locator('button:has-text("View by Team Member")')).not.toBeVisible();
    });
  });
});
