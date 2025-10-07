import { describe, it, expect } from 'vitest';
import { parseCustomerName, formatAddress, getProjectAge, getInstallDate, getRescheduleHistory } from '@/lib/utils/project-helpers';
import { PROJECT_FIELDS } from '@/lib/constants/fieldIds';
import { QuickbaseProject } from '@/lib/types/project';

// Helper function to create mock project
function createMockProject(fields: Record<string, any>): QuickbaseProject {
  const project: QuickbaseProject = {};
  
  Object.entries(fields).forEach(([key, value]) => {
    project[key] = { value: String(value) };
  });
  
  return project;
}

describe('Project Helpers', () => {
  describe('parseCustomerName', () => {
    it('parses single word name', () => {
      const result = parseCustomerName('John');
      expect(result).toEqual({ firstName: 'John', lastName: '' });
    });

    it('parses two word name', () => {
      const result = parseCustomerName('John Smith');
      expect(result).toEqual({ firstName: 'John', lastName: 'Smith' });
    });

    it('parses three word name', () => {
      const result = parseCustomerName('John Michael Smith');
      expect(result).toEqual({ firstName: 'John Michael', lastName: 'Smith' });
    });

    it('handles empty string', () => {
      const result = parseCustomerName('');
      expect(result).toEqual({ firstName: 'Unknown', lastName: '' });
    });

    it('handles whitespace only', () => {
      const result = parseCustomerName('   ');
      expect(result).toEqual({ firstName: 'Unknown', lastName: '' });
    });

    it('trims extra whitespace', () => {
      const result = parseCustomerName('  John   Smith  ');
      expect(result).toEqual({ firstName: 'John', lastName: 'Smith' });
    });

    it('handles multiple middle names', () => {
      const result = parseCustomerName('John Michael David Smith');
      expect(result).toEqual({ firstName: 'John Michael David', lastName: 'Smith' });
    });

    it('handles single character names', () => {
      const result = parseCustomerName('J S');
      expect(result).toEqual({ firstName: 'J', lastName: 'S' });
    });
  });

  describe('formatAddress', () => {
    it('formats single part address', () => {
      const result = formatAddress('123 Main St');
      expect(result).toEqual({ line1: '123 Main St', line2: '' });
    });

    it('formats two part address', () => {
      const result = formatAddress('123 Main St, Phoenix');
      expect(result).toEqual({ line1: '123 Main St', line2: 'Phoenix' });
    });

    it('formats full address', () => {
      const result = formatAddress('123 Main St, Phoenix, AZ 85001');
      expect(result).toEqual({ line1: '123 Main St', line2: 'Phoenix, AZ 85001' });
    });

    it('handles empty string', () => {
      const result = formatAddress('');
      expect(result).toEqual({ line1: '', line2: '' });
    });

    it('trims whitespace from parts', () => {
      const result = formatAddress('123 Main St , Phoenix , AZ');
      expect(result).toEqual({ line1: '123 Main St', line2: 'Phoenix, AZ' });
    });

    it('handles multiple comma-separated parts', () => {
      const result = formatAddress('123 Main St, Phoenix, AZ, 85001, USA');
      expect(result).toEqual({ line1: '123 Main St', line2: 'Phoenix, AZ, 85001, USA' });
    });

    it('handles whitespace-only string', () => {
      const result = formatAddress('   ');
      expect(result).toEqual({ line1: '', line2: '' });
    });

    it('handles single comma', () => {
      const result = formatAddress('123 Main St,');
      expect(result).toEqual({ line1: '123 Main St', line2: '' });
    });
  });

  describe('getProjectAge', () => {
    it('returns age from PROJECT_AGE field', () => {
      const project = createMockProject({
        [PROJECT_FIELDS.PROJECT_AGE]: '45',
      });

      const result = getProjectAge(project);
      expect(result).toBe(45);
    });

    it('returns 0 for missing field', () => {
      const project = createMockProject({});

      const result = getProjectAge(project);
      expect(result).toBe(0);
    });

    it('returns 0 for invalid number', () => {
      const project = createMockProject({
        [PROJECT_FIELDS.PROJECT_AGE]: 'invalid',
      });

      const result = getProjectAge(project);
      expect(result).toBe(0);
    });

    it('parses string to integer', () => {
      const project = createMockProject({
        [PROJECT_FIELDS.PROJECT_AGE]: '45.7',
      });

      const result = getProjectAge(project);
      expect(result).toBe(45);
    });

    it('handles zero age', () => {
      const project = createMockProject({
        [PROJECT_FIELDS.PROJECT_AGE]: '0',
      });

      const result = getProjectAge(project);
      expect(result).toBe(0);
    });

    it('handles negative age', () => {
      const project = createMockProject({
        [PROJECT_FIELDS.PROJECT_AGE]: '-5',
      });

      const result = getProjectAge(project);
      expect(result).toBe(-5);
    });
  });

  describe('getInstallDate', () => {
    it('returns INSTALL_COMPLETED_DATE if exists (highest priority)', () => {
      const project = createMockProject({
        [PROJECT_FIELDS.INSTALL_COMPLETED_DATE]: '2024-01-15',
        [PROJECT_FIELDS.INSTALL_SCHEDULED_DATE_CAPTURE]: '2024-01-10',
        [PROJECT_FIELDS.INSTALL_SCHEDULED_START_DATE]: '2024-01-12',
        [PROJECT_FIELDS.ESTIMATED_INSTALL_DATE]: '2024-01-20',
      });

      const result = getInstallDate(project);
      expect(result).toBe('2024-01-15');
    });

    it('returns INSTALL_SCHEDULED_DATE_CAPTURE if completed missing', () => {
      const project = createMockProject({
        [PROJECT_FIELDS.INSTALL_COMPLETED_DATE]: '',
        [PROJECT_FIELDS.INSTALL_SCHEDULED_DATE_CAPTURE]: '2024-01-10',
        [PROJECT_FIELDS.INSTALL_SCHEDULED_START_DATE]: '2024-01-12',
        [PROJECT_FIELDS.ESTIMATED_INSTALL_DATE]: '2024-01-20',
      });

      const result = getInstallDate(project);
      expect(result).toBe('2024-01-10');
    });

    it('returns INSTALL_SCHEDULED_START_DATE as fallback', () => {
      const project = createMockProject({
        [PROJECT_FIELDS.INSTALL_COMPLETED_DATE]: '',
        [PROJECT_FIELDS.INSTALL_SCHEDULED_DATE_CAPTURE]: '',
        [PROJECT_FIELDS.INSTALL_SCHEDULED_START_DATE]: '2024-01-12',
        [PROJECT_FIELDS.ESTIMATED_INSTALL_DATE]: '2024-01-20',
      });

      const result = getInstallDate(project);
      expect(result).toBe('2024-01-12');
    });

    it('returns ESTIMATED_INSTALL_DATE as last fallback', () => {
      const project = createMockProject({
        [PROJECT_FIELDS.INSTALL_COMPLETED_DATE]: '',
        [PROJECT_FIELDS.INSTALL_SCHEDULED_DATE_CAPTURE]: '',
        [PROJECT_FIELDS.INSTALL_SCHEDULED_START_DATE]: '',
        [PROJECT_FIELDS.ESTIMATED_INSTALL_DATE]: '2024-01-20',
      });

      const result = getInstallDate(project);
      expect(result).toBe('2024-01-20');
    });

    it('returns null when no install dates exist', () => {
      const project = createMockProject({
        [PROJECT_FIELDS.INSTALL_COMPLETED_DATE]: '',
        [PROJECT_FIELDS.INSTALL_SCHEDULED_DATE_CAPTURE]: '',
        [PROJECT_FIELDS.INSTALL_SCHEDULED_START_DATE]: '',
        [PROJECT_FIELDS.ESTIMATED_INSTALL_DATE]: '',
      });

      const result = getInstallDate(project);
      expect(result).toBe(null);
    });

    it('handles missing fields entirely', () => {
      const project = createMockProject({});

      const result = getInstallDate(project);
      expect(result).toBe(null);
    });
  });

  describe('getRescheduleHistory', () => {
    it('returns reschedule count if field exists', () => {
      const project = createMockProject({
        [PROJECT_FIELDS.RESCHEDULE_COUNT]: '2',
      });

      const result = getRescheduleHistory(project);
      expect(result).toBe(2);
    });

    it('returns 0 for missing field', () => {
      const project = createMockProject({});

      const result = getRescheduleHistory(project);
      expect(result).toBe(0);
    });

    it('returns 0 for invalid number', () => {
      const project = createMockProject({
        [PROJECT_FIELDS.RESCHEDULE_COUNT]: 'invalid',
      });

      const result = getRescheduleHistory(project);
      expect(result).toBe(0);
    });

    it('parses string to integer', () => {
      const project = createMockProject({
        [PROJECT_FIELDS.RESCHEDULE_COUNT]: '3.7',
      });

      const result = getRescheduleHistory(project);
      expect(result).toBe(3);
    });

    it('handles zero reschedules', () => {
      const project = createMockProject({
        [PROJECT_FIELDS.RESCHEDULE_COUNT]: '0',
      });

      const result = getRescheduleHistory(project);
      expect(result).toBe(0);
    });

    it('handles negative reschedules', () => {
      const project = createMockProject({
        [PROJECT_FIELDS.RESCHEDULE_COUNT]: '-1',
      });

      const result = getRescheduleHistory(project);
      expect(result).toBe(-1);
    });
  });
});
