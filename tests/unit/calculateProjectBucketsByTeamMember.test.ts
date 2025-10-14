import { describe, it, expect, beforeEach } from 'vitest';
import { calculateProjectBucketsByTeamMember } from '@/lib/quickbase/queries';
import { PROJECT_FIELDS } from '@/lib/constants/fieldIds';

describe('calculateProjectBucketsByTeamMember', () => {
  // Note: Function now creates separate entries for closer and setter
  // For tests expecting single result, set setter fields to null
  // Note: Function still uses PROJECT_AGE field (not SALES_DATE calculation)
  // Note: Set permits to null by default so projects aren't automatically readyForInstall
  const createMockProject = (overrides: any = {}) => ({
    [PROJECT_FIELDS.RECORD_ID]: { value: '1' },
    [PROJECT_FIELDS.CLOSER_NAME]: { value: 'John Doe' },
    [PROJECT_FIELDS.CLOSER_EMAIL]: { value: 'john@example.com' },
    [PROJECT_FIELDS.SETTER_NAME]: { value: null },  // Set to null for closer-only tests
    [PROJECT_FIELDS.SETTER_EMAIL]: { value: null }, // Set to null for closer-only tests
    [PROJECT_FIELDS.PROJECT_STATUS]: { value: 'Active' },
    [PROJECT_FIELDS.ON_HOLD]: { value: 'No' },
    [PROJECT_FIELDS.PROJECT_AGE]: { value: '45' }, // Still uses PROJECT_AGE field
    [PROJECT_FIELDS.SALES_DATE]: { value: new Date(Date.now() - 45 * 24 * 60 * 60 * 1000).toISOString() },
    [PROJECT_FIELDS.INSTALL_COMPLETED_DATE]: { value: null },
    [PROJECT_FIELDS.PTO_APPROVED]: { value: null },
    [PROJECT_FIELDS.NEM_APPROVED]: { value: null }, // null by default to avoid auto-readyForInstall
    [PROJECT_FIELDS.PERMIT_APPROVED]: { value: null }, // null by default to avoid auto-readyForInstall
    [PROJECT_FIELDS.INSTALL_SCHEDULED_DATE_CAPTURE]: { value: null },
    [PROJECT_FIELDS.DATE_ON_HOLD]: { value: null },
    ...overrides,
  });

  describe('Basic Grouping', () => {
    it('groups projects by closer email', () => {
      const projects = [
        createMockProject({
          [PROJECT_FIELDS.RECORD_ID]: { value: '1' },
          [PROJECT_FIELDS.CLOSER_NAME]: { value: 'John Doe' },
          [PROJECT_FIELDS.CLOSER_EMAIL]: { value: 'john@example.com' },
          [PROJECT_FIELDS.ON_HOLD]: { value: 'Yes' },
        }),
        createMockProject({
          [PROJECT_FIELDS.RECORD_ID]: { value: '2' },
          [PROJECT_FIELDS.CLOSER_NAME]: { value: 'Jane Smith' },
          [PROJECT_FIELDS.CLOSER_EMAIL]: { value: 'jane@example.com' },
          [PROJECT_FIELDS.ON_HOLD]: { value: 'Yes' },
        }),
        createMockProject({
          [PROJECT_FIELDS.RECORD_ID]: { value: '3' },
          [PROJECT_FIELDS.CLOSER_NAME]: { value: 'Bob Wilson' },
          [PROJECT_FIELDS.CLOSER_EMAIL]: { value: 'bob@example.com' },
          [PROJECT_FIELDS.ON_HOLD]: { value: 'Yes' },
        }),
      ];

      const result = calculateProjectBucketsByTeamMember(projects);

      expect(result).toHaveLength(3);
      expect(result[0]).toMatchObject({
        memberName: 'John Doe',
        memberEmail: 'john@example.com',
        role: 'closer',
        onHold: 1,
        totalProjects: 1,
      });
      expect(result[1]).toMatchObject({
        memberName: 'Jane Smith',
        memberEmail: 'jane@example.com',
        role: 'closer',
        onHold: 1,
        totalProjects: 1,
      });
      expect(result[2]).toMatchObject({
        memberName: 'Bob Wilson',
        memberEmail: 'bob@example.com',
        role: 'closer',
        onHold: 1,
        totalProjects: 1,
      });
    });

    it('groups projects by setter email', () => {
      const projects = [
        createMockProject({
          [PROJECT_FIELDS.RECORD_ID]: { value: '1' },
          [PROJECT_FIELDS.CLOSER_NAME]: { value: null }, // No closer for setter-only test
          [PROJECT_FIELDS.CLOSER_EMAIL]: { value: null },
          [PROJECT_FIELDS.SETTER_NAME]: { value: 'Alice Johnson' },
          [PROJECT_FIELDS.SETTER_EMAIL]: { value: 'alice@example.com' },
          [PROJECT_FIELDS.ON_HOLD]: { value: 'Yes' },
        }),
        createMockProject({
          [PROJECT_FIELDS.RECORD_ID]: { value: '2' },
          [PROJECT_FIELDS.CLOSER_NAME]: { value: null }, // No closer for setter-only test
          [PROJECT_FIELDS.CLOSER_EMAIL]: { value: null },
          [PROJECT_FIELDS.SETTER_NAME]: { value: 'Charlie Brown' },
          [PROJECT_FIELDS.SETTER_EMAIL]: { value: 'charlie@example.com' },
          [PROJECT_FIELDS.ON_HOLD]: { value: 'Yes' },
        }),
      ];

      const result = calculateProjectBucketsByTeamMember(projects);

      expect(result).toHaveLength(2);
      expect(result[0]).toMatchObject({
        memberName: 'Alice Johnson',
        memberEmail: 'alice@example.com',
        role: 'setter',
        onHold: 1,
        totalProjects: 1,
      });
      expect(result[1]).toMatchObject({
        memberName: 'Charlie Brown',
        memberEmail: 'charlie@example.com',
        role: 'setter',
        onHold: 1,
        totalProjects: 1,
      });
    });

    it('handles projects with both closer and setter', () => {
      const projects = [
        createMockProject({
          [PROJECT_FIELDS.RECORD_ID]: { value: '1' },
          [PROJECT_FIELDS.CLOSER_NAME]: { value: 'John Doe' },
          [PROJECT_FIELDS.CLOSER_EMAIL]: { value: 'john@example.com' },
          [PROJECT_FIELDS.SETTER_NAME]: { value: 'Jane Smith' },
          [PROJECT_FIELDS.SETTER_EMAIL]: { value: 'jane@example.com' },
          [PROJECT_FIELDS.ON_HOLD]: { value: 'Yes' },
        }),
      ];

      const result = calculateProjectBucketsByTeamMember(projects);

      expect(result).toHaveLength(2);
      expect(result[0]).toMatchObject({
        memberName: 'John Doe',
        memberEmail: 'john@example.com',
        role: 'closer',
        onHold: 1,
        totalProjects: 1,
      });
      expect(result[1]).toMatchObject({
        memberName: 'Jane Smith',
        memberEmail: 'jane@example.com',
        role: 'setter',
        onHold: 1,
        totalProjects: 1,
      });
    });

    it('avoids double-counting when closer and setter are same person', () => {
      const projects = [
        createMockProject({
          [PROJECT_FIELDS.RECORD_ID]: { value: '1' },
          [PROJECT_FIELDS.CLOSER_NAME]: { value: 'John Doe' },
          [PROJECT_FIELDS.CLOSER_EMAIL]: { value: 'john@example.com' },
          [PROJECT_FIELDS.SETTER_NAME]: { value: 'John Doe' },
          [PROJECT_FIELDS.SETTER_EMAIL]: { value: 'john@example.com' },
          [PROJECT_FIELDS.ON_HOLD]: { value: 'Yes' },
        }),
      ];

      const result = calculateProjectBucketsByTeamMember(projects);

      expect(result).toHaveLength(1);
      expect(result[0]).toMatchObject({
        memberName: 'John Doe',
        memberEmail: 'john@example.com',
        role: 'closer',
        onHold: 1,
        totalProjects: 1,
      });
    });
  });

  describe('Bucket Categories', () => {
    it('counts installs bucket correctly', () => {
      const projects = [
        createMockProject({
          [PROJECT_FIELDS.RECORD_ID]: { value: '1' },
          [PROJECT_FIELDS.CLOSER_NAME]: { value: 'John Doe' },
          [PROJECT_FIELDS.CLOSER_EMAIL]: { value: 'john@example.com' },
          [PROJECT_FIELDS.INSTALL_COMPLETED_DATE]: { value: '2024-01-15' },
          [PROJECT_FIELDS.PTO_APPROVED]: { value: null },
        }),
        createMockProject({
          [PROJECT_FIELDS.RECORD_ID]: { value: '2' },
          [PROJECT_FIELDS.CLOSER_NAME]: { value: 'John Doe' },
          [PROJECT_FIELDS.CLOSER_EMAIL]: { value: 'john@example.com' },
          [PROJECT_FIELDS.INSTALL_COMPLETED_DATE]: { value: '2024-01-20' },
          [PROJECT_FIELDS.PTO_APPROVED]: { value: null },
        }),
      ];

      const result = calculateProjectBucketsByTeamMember(projects);

      expect(result).toHaveLength(1);
      expect(result[0]).toMatchObject({
        memberName: 'John Doe',
        memberEmail: 'john@example.com',
        role: 'closer',
        installs: 2,
        rejected: 0,
        onHold: 0,
        repAttention: 0,
        pendingCancel: 0,
        readyForInstall: 0,
        totalProjects: 2,
      });
    });

    it('counts rejected bucket correctly', () => {
      const projects = [
        createMockProject({
          [PROJECT_FIELDS.RECORD_ID]: { value: '1' },
          [PROJECT_FIELDS.CLOSER_NAME]: { value: 'John Doe' },
          [PROJECT_FIELDS.CLOSER_EMAIL]: { value: 'john@example.com' },
          [PROJECT_FIELDS.PROJECT_STATUS]: { value: 'Rejected' },
        }),
        createMockProject({
          [PROJECT_FIELDS.RECORD_ID]: { value: '2' },
          [PROJECT_FIELDS.CLOSER_NAME]: { value: 'John Doe' },
          [PROJECT_FIELDS.CLOSER_EMAIL]: { value: 'john@example.com' },
          [PROJECT_FIELDS.PROJECT_STATUS]: { value: 'Customer Rejected' },
        }),
      ];

      const result = calculateProjectBucketsByTeamMember(projects);

      expect(result).toHaveLength(1);
      expect(result[0]).toMatchObject({
        memberName: 'John Doe',
        memberEmail: 'john@example.com',
        role: 'closer',
        installs: 0,
        rejected: 2,
        onHold: 0,
        repAttention: 0,
        pendingCancel: 0,
        readyForInstall: 0,
        totalProjects: 2,
      });
    });

    it('counts on hold bucket correctly', () => {
      const projects = [
        createMockProject({
          [PROJECT_FIELDS.RECORD_ID]: { value: '1' },
          [PROJECT_FIELDS.CLOSER_NAME]: { value: 'John Doe' },
          [PROJECT_FIELDS.CLOSER_EMAIL]: { value: 'john@example.com' },
          [PROJECT_FIELDS.ON_HOLD]: { value: 'Yes' },
        }),
        createMockProject({
          [PROJECT_FIELDS.RECORD_ID]: { value: '2' },
          [PROJECT_FIELDS.CLOSER_NAME]: { value: 'John Doe' },
          [PROJECT_FIELDS.CLOSER_EMAIL]: { value: 'john@example.com' },
          [PROJECT_FIELDS.ON_HOLD]: { value: 'Yes' },
        }),
      ];

      const result = calculateProjectBucketsByTeamMember(projects);

      expect(result).toHaveLength(1);
      expect(result[0]).toMatchObject({
        memberName: 'John Doe',
        memberEmail: 'john@example.com',
        role: 'closer',
        installs: 0,
        rejected: 0,
        onHold: 2,
        repAttention: 0,
        pendingCancel: 0,
        readyForInstall: 0,
        totalProjects: 2,
      });
    });

    it('counts rep attention bucket correctly - age > 90', () => {
      const projects = [
        createMockProject({
          [PROJECT_FIELDS.RECORD_ID]: { value: '1' },
          [PROJECT_FIELDS.CLOSER_NAME]: { value: 'John Doe' },
          [PROJECT_FIELDS.CLOSER_EMAIL]: { value: 'john@example.com' },
          [PROJECT_FIELDS.PROJECT_AGE]: { value: '95' },
        }),
        createMockProject({
          [PROJECT_FIELDS.RECORD_ID]: { value: '2' },
          [PROJECT_FIELDS.CLOSER_NAME]: { value: 'John Doe' },
          [PROJECT_FIELDS.CLOSER_EMAIL]: { value: 'john@example.com' },
          [PROJECT_FIELDS.PROJECT_AGE]: { value: '120' },
        }),
      ];

      const result = calculateProjectBucketsByTeamMember(projects);

      expect(result).toHaveLength(1);
      expect(result[0]).toMatchObject({
        memberName: 'John Doe',
        memberEmail: 'john@example.com',
        role: 'closer',
        installs: 0,
        rejected: 0,
        onHold: 0,
        repAttention: 2,
        pendingCancel: 0,
        readyForInstall: 0,
        totalProjects: 2,
      });
    });

    it('counts rep attention bucket correctly - on hold > 7 days', () => {
      const sevenDaysAgo = new Date();
      sevenDaysAgo.setDate(sevenDaysAgo.getDate() - 8);
      const holdDate = sevenDaysAgo.toISOString().split('T')[0];

      const projects = [
        createMockProject({
          [PROJECT_FIELDS.RECORD_ID]: { value: '1' },
          [PROJECT_FIELDS.CLOSER_NAME]: { value: 'John Doe' },
          [PROJECT_FIELDS.CLOSER_EMAIL]: { value: 'john@example.com' },
          [PROJECT_FIELDS.ON_HOLD]: { value: 'Yes' },
          [PROJECT_FIELDS.DATE_ON_HOLD]: { value: holdDate },
        }),
      ];

      const result = calculateProjectBucketsByTeamMember(projects);

      expect(result).toHaveLength(1);
      expect(result[0]).toMatchObject({
        memberName: 'John Doe',
        memberEmail: 'john@example.com',
        role: 'closer',
        installs: 0,
        rejected: 0,
        onHold: 1,
        repAttention: 1,
        pendingCancel: 0,
        readyForInstall: 0,
        totalProjects: 1,
      });
    });

    it('counts pending cancel bucket correctly', () => {
      const projects = [
        createMockProject({
          [PROJECT_FIELDS.RECORD_ID]: { value: '1' },
          [PROJECT_FIELDS.CLOSER_NAME]: { value: 'John Doe' },
          [PROJECT_FIELDS.CLOSER_EMAIL]: { value: 'john@example.com' },
          [PROJECT_FIELDS.PROJECT_STATUS]: { value: 'Pending Cancel' },
        }),
        createMockProject({
          [PROJECT_FIELDS.RECORD_ID]: { value: '2' },
          [PROJECT_FIELDS.CLOSER_NAME]: { value: 'John Doe' },
          [PROJECT_FIELDS.CLOSER_EMAIL]: { value: 'john@example.com' },
          [PROJECT_FIELDS.PROJECT_STATUS]: { value: 'Customer Pending Cancel' },
        }),
      ];

      const result = calculateProjectBucketsByTeamMember(projects);

      expect(result).toHaveLength(1);
      expect(result[0]).toMatchObject({
        memberName: 'John Doe',
        memberEmail: 'john@example.com',
        role: 'closer',
        installs: 0,
        rejected: 0,
        onHold: 0,
        repAttention: 0,
        pendingCancel: 2,
        readyForInstall: 0,
        totalProjects: 2,
      });
    });

    it('counts ready for install bucket correctly', () => {
      const projects = [
        createMockProject({
          [PROJECT_FIELDS.RECORD_ID]: { value: '1' },
          [PROJECT_FIELDS.CLOSER_NAME]: { value: 'John Doe' },
          [PROJECT_FIELDS.CLOSER_EMAIL]: { value: 'john@example.com' },
          [PROJECT_FIELDS.NEM_APPROVED]: { value: 'Yes' },
          [PROJECT_FIELDS.PERMIT_APPROVED]: { value: 'Yes' },
          [PROJECT_FIELDS.INSTALL_SCHEDULED_DATE_CAPTURE]: { value: null },
        }),
        createMockProject({
          [PROJECT_FIELDS.RECORD_ID]: { value: '2' },
          [PROJECT_FIELDS.CLOSER_NAME]: { value: 'John Doe' },
          [PROJECT_FIELDS.CLOSER_EMAIL]: { value: 'john@example.com' },
          [PROJECT_FIELDS.NEM_APPROVED]: { value: 'Yes' },
          [PROJECT_FIELDS.PERMIT_APPROVED]: { value: 'Yes' },
          [PROJECT_FIELDS.INSTALL_SCHEDULED_DATE_CAPTURE]: { value: null },
        }),
      ];

      const result = calculateProjectBucketsByTeamMember(projects);

      expect(result).toHaveLength(1);
      expect(result[0]).toMatchObject({
        memberName: 'John Doe',
        memberEmail: 'john@example.com',
        role: 'closer',
        installs: 0,
        rejected: 0,
        onHold: 0,
        repAttention: 0,
        pendingCancel: 0,
        readyForInstall: 2,
        totalProjects: 2,
      });
    });
  });

  describe('Multiple Bucket Membership', () => {
    it('project can be in multiple buckets simultaneously', () => {
      const sevenDaysAgo = new Date();
      sevenDaysAgo.setDate(sevenDaysAgo.getDate() - 8);
      const holdDate = sevenDaysAgo.toISOString().split('T')[0];

      const projects = [
        createMockProject({
          [PROJECT_FIELDS.RECORD_ID]: { value: '1' },
          [PROJECT_FIELDS.CLOSER_NAME]: { value: 'John Doe' },
          [PROJECT_FIELDS.CLOSER_EMAIL]: { value: 'john@example.com' },
          [PROJECT_FIELDS.ON_HOLD]: { value: 'Yes' },
          [PROJECT_FIELDS.DATE_ON_HOLD]: { value: holdDate },
        }),
      ];

      const result = calculateProjectBucketsByTeamMember(projects);

      expect(result).toHaveLength(1);
      expect(result[0]).toMatchObject({
        memberName: 'John Doe',
        memberEmail: 'john@example.com',
        role: 'closer',
        installs: 0,
        rejected: 0,
        onHold: 1,
        repAttention: 1,
        pendingCancel: 0,
        readyForInstall: 0,
        totalProjects: 1,
      });
    });

    it('project in rep attention due to age only', () => {
      const projects = [
        createMockProject({
          [PROJECT_FIELDS.RECORD_ID]: { value: '1' },
          [PROJECT_FIELDS.CLOSER_NAME]: { value: 'John Doe' },
          [PROJECT_FIELDS.CLOSER_EMAIL]: { value: 'john@example.com' },
          [PROJECT_FIELDS.PROJECT_AGE]: { value: '95' },
          [PROJECT_FIELDS.ON_HOLD]: { value: 'No' },
        }),
      ];

      const result = calculateProjectBucketsByTeamMember(projects);

      expect(result).toHaveLength(1);
      expect(result[0]).toMatchObject({
        memberName: 'John Doe',
        memberEmail: 'john@example.com',
        role: 'closer',
        installs: 0,
        rejected: 0,
        onHold: 0,
        repAttention: 1,
        pendingCancel: 0,
        readyForInstall: 0,
        totalProjects: 1,
      });
    });
  });

  describe('Edge Cases', () => {
    it('handles missing closer email (uses name as fallback)', () => {
      const projects = [
        createMockProject({
          [PROJECT_FIELDS.RECORD_ID]: { value: '1' },
          [PROJECT_FIELDS.CLOSER_NAME]: { value: 'John Doe' },
          [PROJECT_FIELDS.CLOSER_EMAIL]: { value: null },
          [PROJECT_FIELDS.ON_HOLD]: { value: 'Yes' },
        }),
      ];

      const result = calculateProjectBucketsByTeamMember(projects);

      expect(result).toHaveLength(1);
      expect(result[0]).toMatchObject({
        memberName: 'John Doe',
        memberEmail: null,
        role: 'closer',
        onHold: 1,
        totalProjects: 1,
      });
    });

    it('handles missing closer name and email (skips unassigned)', () => {
      // Note: Function doesn't create entries for projects with NO team members
      // Projects without closer or setter are not included in member buckets
      const projects = [
        createMockProject({
          [PROJECT_FIELDS.RECORD_ID]: { value: '1' },
          [PROJECT_FIELDS.CLOSER_NAME]: { value: null },
          [PROJECT_FIELDS.CLOSER_EMAIL]: { value: null },
          [PROJECT_FIELDS.ON_HOLD]: { value: 'Yes' },
        }),
      ];

      const result = calculateProjectBucketsByTeamMember(projects);

      // Projects with no team members are skipped
      expect(result).toHaveLength(0);
    });

    it('handles empty project array', () => {
      const result = calculateProjectBucketsByTeamMember([]);
      expect(result).toEqual([]);
    });

    it('handles projects with no bucket membership', () => {
      const projects = [
        createMockProject({
          [PROJECT_FIELDS.RECORD_ID]: { value: '1' },
          [PROJECT_FIELDS.CLOSER_NAME]: { value: 'John Doe' },
          [PROJECT_FIELDS.CLOSER_EMAIL]: { value: 'john@example.com' },
          [PROJECT_FIELDS.PROJECT_STATUS]: { value: 'Active' },
          [PROJECT_FIELDS.ON_HOLD]: { value: 'No' },
          [PROJECT_FIELDS.PROJECT_AGE]: { value: '30' },
          [PROJECT_FIELDS.INSTALL_COMPLETED_DATE]: { value: null },
          [PROJECT_FIELDS.PTO_APPROVED]: { value: 'Yes' },
          [PROJECT_FIELDS.NEM_APPROVED]: { value: 'No' },
          [PROJECT_FIELDS.PERMIT_APPROVED]: { value: 'No' },
        }),
      ];

      const result = calculateProjectBucketsByTeamMember(projects);
      expect(result).toEqual([]);
    });
  });

  describe('Sorting', () => {
    it('sorts by total projects descending', () => {
      const projects = [
        // Member A: 1 project
        createMockProject({
          [PROJECT_FIELDS.RECORD_ID]: { value: '1' },
          [PROJECT_FIELDS.CLOSER_NAME]: { value: 'Alice' },
          [PROJECT_FIELDS.CLOSER_EMAIL]: { value: 'alice@example.com' },
          [PROJECT_FIELDS.ON_HOLD]: { value: 'Yes' },
        }),
        // Member B: 3 projects
        createMockProject({
          [PROJECT_FIELDS.RECORD_ID]: { value: '2' },
          [PROJECT_FIELDS.CLOSER_NAME]: { value: 'Bob' },
          [PROJECT_FIELDS.CLOSER_EMAIL]: { value: 'bob@example.com' },
          [PROJECT_FIELDS.ON_HOLD]: { value: 'Yes' },
        }),
        createMockProject({
          [PROJECT_FIELDS.RECORD_ID]: { value: '3' },
          [PROJECT_FIELDS.CLOSER_NAME]: { value: 'Bob' },
          [PROJECT_FIELDS.CLOSER_EMAIL]: { value: 'bob@example.com' },
          [PROJECT_FIELDS.ON_HOLD]: { value: 'Yes' },
        }),
        createMockProject({
          [PROJECT_FIELDS.RECORD_ID]: { value: '4' },
          [PROJECT_FIELDS.CLOSER_NAME]: { value: 'Bob' },
          [PROJECT_FIELDS.CLOSER_EMAIL]: { value: 'bob@example.com' },
          [PROJECT_FIELDS.ON_HOLD]: { value: 'Yes' },
        }),
        // Member C: 2 projects
        createMockProject({
          [PROJECT_FIELDS.RECORD_ID]: { value: '5' },
          [PROJECT_FIELDS.CLOSER_NAME]: { value: 'Charlie' },
          [PROJECT_FIELDS.CLOSER_EMAIL]: { value: 'charlie@example.com' },
          [PROJECT_FIELDS.ON_HOLD]: { value: 'Yes' },
        }),
        createMockProject({
          [PROJECT_FIELDS.RECORD_ID]: { value: '6' },
          [PROJECT_FIELDS.CLOSER_NAME]: { value: 'Charlie' },
          [PROJECT_FIELDS.CLOSER_EMAIL]: { value: 'charlie@example.com' },
          [PROJECT_FIELDS.ON_HOLD]: { value: 'Yes' },
        }),
      ];

      const result = calculateProjectBucketsByTeamMember(projects);

      expect(result).toHaveLength(3);
      expect(result[0].memberName).toBe('Bob');
      expect(result[0].totalProjects).toBe(3);
      expect(result[1].memberName).toBe('Charlie');
      expect(result[1].totalProjects).toBe(2);
      expect(result[2].memberName).toBe('Alice');
      expect(result[2].totalProjects).toBe(1);
    });
  });

  describe('Total Projects Calculation', () => {
    it('totalProjects equals sum of unique projects', () => {
      const sevenDaysAgo = new Date();
      sevenDaysAgo.setDate(sevenDaysAgo.getDate() - 8);
      const holdDate = sevenDaysAgo.toISOString().split('T')[0];

      const projects = [
        // Project 1: on hold AND rep attention (counts as 1 unique project)
        createMockProject({
          [PROJECT_FIELDS.RECORD_ID]: { value: '1' },
          [PROJECT_FIELDS.CLOSER_NAME]: { value: 'John Doe' },
          [PROJECT_FIELDS.CLOSER_EMAIL]: { value: 'john@example.com' },
          [PROJECT_FIELDS.ON_HOLD]: { value: 'Yes' },
          [PROJECT_FIELDS.DATE_ON_HOLD]: { value: holdDate },
        }),
        // Project 2: just on hold
        createMockProject({
          [PROJECT_FIELDS.RECORD_ID]: { value: '2' },
          [PROJECT_FIELDS.CLOSER_NAME]: { value: 'John Doe' },
          [PROJECT_FIELDS.CLOSER_EMAIL]: { value: 'john@example.com' },
          [PROJECT_FIELDS.ON_HOLD]: { value: 'Yes' },
        }),
      ];

      const result = calculateProjectBucketsByTeamMember(projects);

      expect(result).toHaveLength(1);
      expect(result[0]).toMatchObject({
        memberName: 'John Doe',
        memberEmail: 'john@example.com',
        role: 'closer',
        onHold: 2,
        repAttention: 1,
        totalProjects: 2, // 2 unique projects, not 3 (sum of bucket counts)
      });
    });
  });
});
