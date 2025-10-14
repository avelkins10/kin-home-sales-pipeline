// tests/unit/calculateCommissionByTeamMember.test.ts
import { describe, it, expect } from 'vitest';
import { calculateCommissionByTeamMember } from '@/lib/quickbase/queries';
import { PROJECT_FIELDS } from '@/lib/constants/fieldIds';
import type { TeamMemberCommission } from '@/lib/types/dashboard';

describe('calculateCommissionByTeamMember', () => {
  // Helper to create mock project data
  const createMockProject = (overrides: any = {}) => ({
    [PROJECT_FIELDS.RECORD_ID]: { value: Math.random().toString() },
    [PROJECT_FIELDS.CLOSER_NAME]: { value: 'John Doe' },
    [PROJECT_FIELDS.CLOSER_EMAIL]: { value: 'john@example.com' },
    [PROJECT_FIELDS.SETTER_NAME]: { value: 'Jane Smith' },
    [PROJECT_FIELDS.SETTER_EMAIL]: { value: 'jane@example.com' },
    [PROJECT_FIELDS.COMMISSIONABLE_PPW]: { value: '0.5' },
    [PROJECT_FIELDS.SYSTEM_SIZE_KW]: { value: '10' },
    [PROJECT_FIELDS.PROJECT_STATUS]: { value: 'Active' },
    [PROJECT_FIELDS.ON_HOLD]: { value: 'No' },
    [PROJECT_FIELDS.INSTALL_COMPLETED_DATE]: { value: null },
    ...overrides,
  });

  describe('Basic Grouping', () => {
    it('groups projects by closer email', () => {
      const projects = [
        createMockProject({
          [PROJECT_FIELDS.CLOSER_NAME]: { value: 'John Doe' },
          [PROJECT_FIELDS.CLOSER_EMAIL]: { value: 'john@example.com' },
          [PROJECT_FIELDS.SETTER_NAME]: { value: null },
          [PROJECT_FIELDS.SETTER_EMAIL]: { value: null },
        }),
        createMockProject({
          [PROJECT_FIELDS.CLOSER_NAME]: { value: 'Jane Smith' },
          [PROJECT_FIELDS.CLOSER_EMAIL]: { value: 'jane@example.com' },
          [PROJECT_FIELDS.SETTER_NAME]: { value: null },
          [PROJECT_FIELDS.SETTER_EMAIL]: { value: null },
        }),
        createMockProject({
          [PROJECT_FIELDS.CLOSER_NAME]: { value: 'Bob Wilson' },
          [PROJECT_FIELDS.CLOSER_EMAIL]: { value: 'bob@example.com' },
          [PROJECT_FIELDS.SETTER_NAME]: { value: null },
          [PROJECT_FIELDS.SETTER_EMAIL]: { value: null },
        }),
      ];

      const result = calculateCommissionByTeamMember(projects, []);
      
      expect(result).toHaveLength(3);
      expect(result[0].memberName).toBe('John Doe');
      expect(result[0].memberEmail).toBe('john@example.com');
      expect(result[0].role).toBe('closer');
      expect(result[1].memberName).toBe('Jane Smith');
      expect(result[2].memberName).toBe('Bob Wilson');
    });

    it('groups projects by setter email', () => {
      const projects = [
        createMockProject({
          [PROJECT_FIELDS.CLOSER_NAME]: { value: null },
          [PROJECT_FIELDS.CLOSER_EMAIL]: { value: null },
          [PROJECT_FIELDS.SETTER_NAME]: { value: 'Alice Johnson' },
          [PROJECT_FIELDS.SETTER_EMAIL]: { value: 'alice@example.com' },
        }),
        createMockProject({
          [PROJECT_FIELDS.CLOSER_NAME]: { value: null },
          [PROJECT_FIELDS.CLOSER_EMAIL]: { value: null },
          [PROJECT_FIELDS.SETTER_NAME]: { value: 'Charlie Brown' },
          [PROJECT_FIELDS.SETTER_EMAIL]: { value: 'charlie@example.com' },
        }),
      ];

      const result = calculateCommissionByTeamMember(projects, []);
      
      expect(result).toHaveLength(2);
      expect(result[0].role).toBe('setter');
      expect(result[1].role).toBe('setter');
    });

    it('handles projects with both closer and setter', () => {
      const projects = [
        createMockProject({
          [PROJECT_FIELDS.CLOSER_NAME]: { value: 'John Doe' },
          [PROJECT_FIELDS.CLOSER_EMAIL]: { value: 'john@example.com' },
          [PROJECT_FIELDS.SETTER_NAME]: { value: 'Jane Smith' },
          [PROJECT_FIELDS.SETTER_EMAIL]: { value: 'jane@example.com' },
        }),
      ];

      const result = calculateCommissionByTeamMember(projects, []);
      
      expect(result).toHaveLength(2);
      expect(result[0].memberName).toBe('John Doe');
      expect(result[0].role).toBe('closer');
      expect(result[1].memberName).toBe('Jane Smith');
      expect(result[1].role).toBe('setter');
      expect(result[0].projectCount).toBe(1);
      expect(result[1].projectCount).toBe(1);
    });

    it('avoids double-counting when closer and setter are same person', () => {
      const projects = [
        createMockProject({
          [PROJECT_FIELDS.CLOSER_NAME]: { value: 'John Doe' },
          [PROJECT_FIELDS.CLOSER_EMAIL]: { value: 'john@example.com' },
          [PROJECT_FIELDS.SETTER_NAME]: { value: 'John Doe' },
          [PROJECT_FIELDS.SETTER_EMAIL]: { value: 'john@example.com' },
        }),
      ];

      const result = calculateCommissionByTeamMember(projects, []);
      
      expect(result).toHaveLength(1);
      expect(result[0].memberName).toBe('John Doe');
      expect(result[0].memberEmail).toBe('john@example.com');
    });
  });

  describe('Commission Categories', () => {
    it('calculates earned commission for funded projects', () => {
      const project = createMockProject({
        [PROJECT_FIELDS.COMMISSIONABLE_PPW]: { value: '0.5' },
        [PROJECT_FIELDS.SYSTEM_SIZE_KW]: { value: '10' },
      });
      const fundedProjects = [project];

      const result = calculateCommissionByTeamMember([project], fundedProjects);
      
      expect(result).toHaveLength(1);
      expect(result[0].earnedCommission).toBe(5000); // 0.5 * 10 * 1000
      expect(result[0].lostCommission).toBe(0);
      expect(result[0].onHoldCommission).toBe(0);
      expect(result[0].pendingCommission).toBe(0);
    });

    it('calculates lost commission for cancelled projects', () => {
      const project = createMockProject({
        [PROJECT_FIELDS.PROJECT_STATUS]: { value: 'Cancelled' },
        [PROJECT_FIELDS.COMMISSIONABLE_PPW]: { value: '0.5' },
        [PROJECT_FIELDS.SYSTEM_SIZE_KW]: { value: '10' },
      });

      const result = calculateCommissionByTeamMember([project], []);
      
      expect(result).toHaveLength(1);
      expect(result[0].earnedCommission).toBe(0);
      expect(result[0].lostCommission).toBe(5000);
      expect(result[0].onHoldCommission).toBe(0);
      expect(result[0].pendingCommission).toBe(0);
    });

    it('calculates on hold commission', () => {
      const project = createMockProject({
        [PROJECT_FIELDS.ON_HOLD]: { value: 'Yes' },
        [PROJECT_FIELDS.COMMISSIONABLE_PPW]: { value: '0.5' },
        [PROJECT_FIELDS.SYSTEM_SIZE_KW]: { value: '10' },
      });

      const result = calculateCommissionByTeamMember([project], []);
      
      expect(result).toHaveLength(1);
      expect(result[0].earnedCommission).toBe(0);
      expect(result[0].lostCommission).toBe(0);
      expect(result[0].onHoldCommission).toBe(5000);
      expect(result[0].pendingCommission).toBe(0);
    });

    it('calculates pending commission for active projects', () => {
      const project = createMockProject({
        [PROJECT_FIELDS.PROJECT_STATUS]: { value: 'Active' },
        [PROJECT_FIELDS.ON_HOLD]: { value: 'No' },
        [PROJECT_FIELDS.INSTALL_COMPLETED_DATE]: { value: null },
        [PROJECT_FIELDS.COMMISSIONABLE_PPW]: { value: '0.5' },
        [PROJECT_FIELDS.SYSTEM_SIZE_KW]: { value: '10' },
      });

      const result = calculateCommissionByTeamMember([project], []);
      
      expect(result).toHaveLength(1);
      expect(result[0].earnedCommission).toBe(0);
      expect(result[0].lostCommission).toBe(0);
      expect(result[0].onHoldCommission).toBe(0);
      expect(result[0].pendingCommission).toBe(5000);
    });
  });

  describe('Edge Cases', () => {
    it('handles missing closer email (uses name as fallback)', () => {
      const project = createMockProject({
        [PROJECT_FIELDS.CLOSER_NAME]: { value: 'John Doe' },
        [PROJECT_FIELDS.CLOSER_EMAIL]: { value: null },
        [PROJECT_FIELDS.SETTER_NAME]: { value: null },
        [PROJECT_FIELDS.SETTER_EMAIL]: { value: null },
      });

      const result = calculateCommissionByTeamMember([project], []);
      
      expect(result).toHaveLength(1);
      expect(result[0].memberName).toBe('John Doe');
      expect(result[0].memberEmail).toBe(null);
    });

    it('handles missing closer name and email (Unassigned)', () => {
      const project = createMockProject({
        [PROJECT_FIELDS.CLOSER_NAME]: { value: null },
        [PROJECT_FIELDS.CLOSER_EMAIL]: { value: null },
        [PROJECT_FIELDS.SETTER_NAME]: { value: null },
        [PROJECT_FIELDS.SETTER_EMAIL]: { value: null },
      });

      const result = calculateCommissionByTeamMember([project], []);
      
      expect(result).toHaveLength(0); // No team members, so no entries
    });

    it('handles invalid commission values (NaN, negative)', () => {
      const project = createMockProject({
        [PROJECT_FIELDS.COMMISSIONABLE_PPW]: { value: 'invalid' },
        [PROJECT_FIELDS.SYSTEM_SIZE_KW]: { value: '-5' },
      });

      const result = calculateCommissionByTeamMember([project], []);
      
      expect(result).toHaveLength(1);
      expect(result[0].earnedCommission).toBe(0);
      expect(result[0].lostCommission).toBe(0);
      expect(result[0].onHoldCommission).toBe(0);
      expect(result[0].pendingCommission).toBe(0);
    });

    it('handles empty project array', () => {
      const result = calculateCommissionByTeamMember([], []);
      
      expect(result).toHaveLength(0);
    });
  });

  describe('Sorting', () => {
    it('sorts by total potential commission descending', () => {
      const projects = [
        createMockProject({
          [PROJECT_FIELDS.CLOSER_NAME]: { value: 'Low Performer' },
          [PROJECT_FIELDS.CLOSER_EMAIL]: { value: 'low@example.com' },
          [PROJECT_FIELDS.COMMISSIONABLE_PPW]: { value: '0.1' },
          [PROJECT_FIELDS.SYSTEM_SIZE_KW]: { value: '10' },
        }),
        createMockProject({
          [PROJECT_FIELDS.CLOSER_NAME]: { value: 'High Performer' },
          [PROJECT_FIELDS.CLOSER_EMAIL]: { value: 'high@example.com' },
          [PROJECT_FIELDS.COMMISSIONABLE_PPW]: { value: '0.5' },
          [PROJECT_FIELDS.SYSTEM_SIZE_KW]: { value: '10' },
        }),
        createMockProject({
          [PROJECT_FIELDS.CLOSER_NAME]: { value: 'Medium Performer' },
          [PROJECT_FIELDS.CLOSER_EMAIL]: { value: 'medium@example.com' },
          [PROJECT_FIELDS.COMMISSIONABLE_PPW]: { value: '0.3' },
          [PROJECT_FIELDS.SYSTEM_SIZE_KW]: { value: '10' },
        }),
      ];

      const result = calculateCommissionByTeamMember(projects, []);
      
      expect(result).toHaveLength(3);
      expect(result[0].memberName).toBe('High Performer'); // 5000 total potential
      expect(result[1].memberName).toBe('Medium Performer'); // 3000 total potential
      expect(result[2].memberName).toBe('Low Performer'); // 1000 total potential
    });
  });

  describe('Commission Formula', () => {
    it('uses correct formula (PPW × kW × 1000)', () => {
      const project = createMockProject({
        [PROJECT_FIELDS.COMMISSIONABLE_PPW]: { value: '0.5' },
        [PROJECT_FIELDS.SYSTEM_SIZE_KW]: { value: '10' },
      });

      const result = calculateCommissionByTeamMember([project], []);
      
      expect(result).toHaveLength(1);
      expect(result[0].pendingCommission).toBe(5000); // 0.5 × 10 × 1000
    });

    it('handles decimal values correctly', () => {
      const project = createMockProject({
        [PROJECT_FIELDS.COMMISSIONABLE_PPW]: { value: '0.375' },
        [PROJECT_FIELDS.SYSTEM_SIZE_KW]: { value: '7.5' },
      });

      const result = calculateCommissionByTeamMember([project], []);
      
      expect(result).toHaveLength(1);
      expect(result[0].pendingCommission).toBe(2812.5); // 0.375 × 7.5 × 1000
    });
  });
});
