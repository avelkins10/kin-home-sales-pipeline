import { describe, it, expect } from 'vitest';
import { formatDate, formatCurrency, formatSystemSize, formatPPW, formatDaysAgo } from '@/lib/utils/formatters';

describe('Formatters', () => {
  describe('formatDate', () => {
    it('formats valid date string (with timezone conversion)', () => {
      // Date strings without time are parsed as UTC midnight, then converted to America/New_York
      // '2024-01-15' UTC midnight becomes '2024-01-14' in EST (UTC-5)
      const result = formatDate('2024-01-15');
      expect(result).toBe('Jan 14, 2024');
    });

    it('formats Date object (with timezone conversion)', () => {
      // Same timezone conversion applies to Date objects
      const result = formatDate(new Date('2024-01-15'));
      expect(result).toBe('Jan 14, 2024');
    });

    it('returns N/A for null', () => {
      const result = formatDate(null);
      expect(result).toBe('N/A');
    });

    it('returns N/A for undefined', () => {
      const result = formatDate(undefined);
      expect(result).toBe('N/A');
    });

    it('returns N/A for invalid date string', () => {
      const result = formatDate('invalid-date');
      expect(result).toBe('N/A');
    });

    it('handles different date formats (with timezone conversion)', () => {
      // UTC midnight dates converted to America/New_York timezone
      expect(formatDate('2024-12-25')).toBe('Dec 24, 2024');
      expect(formatDate('2024-03-01')).toBe('Feb 29, 2024');
    });
  });

  describe('formatCurrency', () => {
    it('formats positive amount', () => {
      const result = formatCurrency(45000);
      expect(result).toBe('$45,000');
    });

    it('formats zero', () => {
      const result = formatCurrency(0);
      expect(result).toBe('$0');
    });

    it('formats large amount with commas', () => {
      const result = formatCurrency(1234567);
      expect(result).toBe('$1,234,567');
    });

    it('returns $0 for null', () => {
      const result = formatCurrency(null);
      expect(result).toBe('$0');
    });

    it('returns $0 for undefined', () => {
      const result = formatCurrency(undefined);
      expect(result).toBe('$0');
    });

    it('rounds to no decimal places', () => {
      const result = formatCurrency(45000.99);
      expect(result).toBe('$45,001');
    });

    it('handles negative amounts', () => {
      const result = formatCurrency(-1000);
      expect(result).toBe('-$1,000');
    });

    it('handles decimal amounts', () => {
      const result = formatCurrency(1234.56);
      expect(result).toBe('$1,235');
    });
  });

  describe('formatSystemSize', () => {
    it('formats size with 1 decimal', () => {
      const result = formatSystemSize(7.2);
      expect(result).toBe('7.2 kW');
    });

    it('formats whole number', () => {
      const result = formatSystemSize(10);
      expect(result).toBe('10.0 kW');
    });

    it('returns N/A for null', () => {
      const result = formatSystemSize(null);
      expect(result).toBe('N/A');
    });

    it('returns N/A for undefined', () => {
      const result = formatSystemSize(undefined);
      expect(result).toBe('N/A');
    });

    it('handles zero', () => {
      const result = formatSystemSize(0);
      expect(result).toBe('0.0 kW');
    });

    it('handles decimal precision', () => {
      const result = formatSystemSize(7.234);
      expect(result).toBe('7.2 kW');
    });

    it('handles large numbers', () => {
      const result = formatSystemSize(100.5);
      expect(result).toBe('100.5 kW');
    });
  });

  describe('formatPPW', () => {
    it('formats PPW with 2 decimals', () => {
      const result = formatPPW(2.85);
      expect(result).toBe('$2.85/W');
    });

    it('formats whole number with decimals', () => {
      const result = formatPPW(3);
      expect(result).toBe('$3.00/W');
    });

    it('returns N/A for null', () => {
      const result = formatPPW(null);
      expect(result).toBe('N/A');
    });

    it('returns N/A for undefined', () => {
      const result = formatPPW(undefined);
      expect(result).toBe('N/A');
    });

    it('handles zero (returns N/A)', () => {
      // Implementation treats 0 as invalid/N/A since PPW of 0 doesn't make sense
      const result = formatPPW(0);
      expect(result).toBe('N/A');
    });

    it('handles decimal precision', () => {
      const result = formatPPW(2.856);
      expect(result).toBe('$2.86/W');
    });

    it('handles negative values', () => {
      const result = formatPPW(-1.5);
      expect(result).toBe('-$1.50/W');
    });
  });

  describe('formatDaysAgo', () => {
    it('returns Today for today', () => {
      const today = new Date();
      const result = formatDaysAgo(today);
      expect(result).toBe('Today');
    });

    it('returns Yesterday for yesterday', () => {
      const yesterday = new Date(Date.now() - 24 * 60 * 60 * 1000);
      const result = formatDaysAgo(yesterday);
      expect(result).toBe('Yesterday');
    });

    it('returns days for < 7 days', () => {
      const fiveDaysAgo = new Date(Date.now() - 5 * 24 * 60 * 60 * 1000);
      const result = formatDaysAgo(fiveDaysAgo);
      expect(result).toBe('5 days ago');
    });

    it('returns weeks for < 30 days', () => {
      const fourteenDaysAgo = new Date(Date.now() - 14 * 24 * 60 * 60 * 1000);
      const result = formatDaysAgo(fourteenDaysAgo);
      expect(result).toBe('2 weeks ago');
    });

    it('returns months for < 365 days', () => {
      const sixtyDaysAgo = new Date(Date.now() - 60 * 24 * 60 * 60 * 1000);
      const result = formatDaysAgo(sixtyDaysAgo);
      expect(result).toBe('2 months ago');
    });

    it('returns years for >= 365 days', () => {
      const fourHundredDaysAgo = new Date(Date.now() - 400 * 24 * 60 * 60 * 1000);
      const result = formatDaysAgo(fourHundredDaysAgo);
      expect(result).toBe('1 years ago');
    });

    it('returns N/A for null', () => {
      const result = formatDaysAgo(null);
      expect(result).toBe('N/A');
    });

    it('returns N/A for undefined', () => {
      const result = formatDaysAgo(undefined);
      expect(result).toBe('N/A');
    });

    it('returns N/A for invalid date', () => {
      const result = formatDaysAgo('invalid');
      expect(result).toBe('N/A');
    });

    it('handles date strings', () => {
      const threeDaysAgo = new Date(Date.now() - 3 * 24 * 60 * 60 * 1000);
      const result = formatDaysAgo(threeDaysAgo.toISOString());
      expect(result).toBe('3 days ago');
    });

    it('handles edge cases for weeks', () => {
      const sevenDaysAgo = new Date(Date.now() - 7 * 24 * 60 * 60 * 1000);
      const result = formatDaysAgo(sevenDaysAgo);
      expect(result).toBe('1 weeks ago');
    });

    it('handles edge cases for months', () => {
      const thirtyDaysAgo = new Date(Date.now() - 30 * 24 * 60 * 60 * 1000);
      const result = formatDaysAgo(thirtyDaysAgo);
      expect(result).toBe('1 months ago');
    });

    it('handles edge cases for years', () => {
      const threeHundredSixtyFiveDaysAgo = new Date(Date.now() - 365 * 24 * 60 * 60 * 1000);
      const result = formatDaysAgo(threeHundredSixtyFiveDaysAgo);
      expect(result).toBe('1 years ago');
    });
  });
});
