import { describe, it, expect } from 'vitest';
import { detectHoldStatus, extractHoldType } from '@/lib/utils/hold-detection';

describe('Hold Detection', () => {
  describe('detectHoldStatus', () => {
    it('returns true for On Hold status', () => {
      const result = detectHoldStatus('On Hold');
      expect(result).toBe(true);
    });

    it('returns true for Finance Hold', () => {
      const result = detectHoldStatus('Finance Hold');
      expect(result).toBe(true);
    });

    it('returns true for Roof Hold', () => {
      const result = detectHoldStatus('Roof Hold');
      expect(result).toBe(true);
    });

    it('returns true for Customer Hold', () => {
      const result = detectHoldStatus('Customer Hold');
      expect(result).toBe(true);
    });

    it('returns true for Permit Hold', () => {
      const result = detectHoldStatus('Permit Hold');
      expect(result).toBe(true);
    });

    it('returns true for HOA Hold', () => {
      const result = detectHoldStatus('HOA Hold');
      expect(result).toBe(true);
    });

    it('returns false for Active status', () => {
      const result = detectHoldStatus('Active');
      expect(result).toBe(false);
    });

    it('returns false for Completed status', () => {
      const result = detectHoldStatus('Completed');
      expect(result).toBe(false);
    });

    it('returns false for empty string', () => {
      const result = detectHoldStatus('');
      expect(result).toBe(false);
    });

    it('is case-sensitive', () => {
      const result = detectHoldStatus('on hold');
      expect(result).toBe(false);
    });

    it('handles partial matches', () => {
      const result = detectHoldStatus('Project is On Hold due to issues');
      expect(result).toBe(true);
    });

    it('handles multiple hold types in one status', () => {
      const result = detectHoldStatus('Finance Hold - Customer Issue');
      expect(result).toBe(true);
    });
  });

  describe('extractHoldType', () => {
    it('returns finance for Finance Hold', () => {
      const result = extractHoldType('Finance Hold');
      expect(result).toBe('finance');
    });

    it('returns roof for Roof Hold', () => {
      const result = extractHoldType('Roof Hold');
      expect(result).toBe('roof');
    });

    it('returns customer for Customer Hold', () => {
      const result = extractHoldType('Customer Hold');
      expect(result).toBe('customer');
    });

    it('returns permit for Permit Hold', () => {
      const result = extractHoldType('Permit Hold');
      expect(result).toBe('permit');
    });

    it('returns hoa for HOA Hold', () => {
      const result = extractHoldType('HOA Hold');
      expect(result).toBe('hoa');
    });

    it('returns generic for On Hold', () => {
      const result = extractHoldType('On Hold');
      expect(result).toBe('generic');
    });

    it('returns generic for Active status', () => {
      const result = extractHoldType('Active');
      expect(result).toBe('generic');
    });

    it('prioritizes most specific match', () => {
      const result = extractHoldType('Finance Hold - Customer Issue');
      expect(result).toBe('finance');
    });

    it('handles partial matches', () => {
      const result = extractHoldType('Project is Finance Hold due to issues');
      expect(result).toBe('finance');
    });

    it('returns generic for empty string', () => {
      const result = extractHoldType('');
      expect(result).toBe('generic');
    });

    it('is case-sensitive', () => {
      const result = extractHoldType('finance hold');
      expect(result).toBe('generic');
    });

    it('handles multiple hold types - returns first match', () => {
      const result = extractHoldType('Roof Hold - Finance Issue');
      expect(result).toBe('roof');
    });

    it('handles complex status strings', () => {
      const result = extractHoldType('Customer Hold - Waiting for approval');
      expect(result).toBe('customer');
    });
  });
});
