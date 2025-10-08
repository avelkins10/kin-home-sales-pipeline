import { normalizeOfficeName } from '@/lib/constants/offices';

describe('Office Name Normalization', () => {
  test('should map PIE to Clearwater', () => {
    expect(normalizeOfficeName('PIE')).toBe('Clearwater');
  });

  test('should map GTR to Columbus', () => {
    expect(normalizeOfficeName('GTR')).toBe('Columbus');
  });

  test('should map RIC to Richmond', () => {
    expect(normalizeOfficeName('RIC')).toBe('Richmond');
  });

  test('should map STL to St. Louis', () => {
    expect(normalizeOfficeName('STL')).toBe('St. Louis');
  });

  test('should map GPT to Gulfport', () => {
    expect(normalizeOfficeName('GPT')).toBe('Gulfport');
  });

  test('should map BIX to Biloxi', () => {
    expect(normalizeOfficeName('BIX')).toBe('Biloxi');
  });

  test('should handle case insensitive input', () => {
    expect(normalizeOfficeName('pie')).toBe('Clearwater');
    expect(normalizeOfficeName('gtr')).toBe('Columbus');
    expect(normalizeOfficeName('ric')).toBe('Richmond');
  });

  test('should return original string for unknown codes', () => {
    expect(normalizeOfficeName('UNKNOWN')).toBe('UNKNOWN');
    expect(normalizeOfficeName('XYZ')).toBe('XYZ');
  });

  test('should handle whitespace', () => {
    expect(normalizeOfficeName('  PIE  ')).toBe('Clearwater');
    expect(normalizeOfficeName(' GTR ')).toBe('Columbus');
  });
});
