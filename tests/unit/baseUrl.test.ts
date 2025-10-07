import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import { getBaseUrl } from '@/lib/utils/baseUrl';

// Mock environment variables
const originalEnv = process.env;

describe('getBaseUrl', () => {
  beforeEach(() => {
    // Reset environment
    process.env = { ...originalEnv };
    // Clear any cached values
    vi.clearAllMocks();
  });

  afterEach(() => {
    process.env = originalEnv;
  });

  describe('precedence order', () => {
    it('should use BASE_URL when set', () => {
      process.env.BASE_URL = 'https://test.example.com';
      process.env.NEXT_PUBLIC_APP_URL = 'https://app.example.com';
      process.env.NEXTAUTH_URL = 'https://auth.example.com';

      const result = getBaseUrl(true);
      expect(result).toBe('https://test.example.com');
    });

    it('should fall back to NEXT_PUBLIC_APP_URL when BASE_URL not set', () => {
      delete process.env.BASE_URL;
      process.env.NEXT_PUBLIC_APP_URL = 'https://app.example.com';
      process.env.NEXTAUTH_URL = 'https://auth.example.com';

      const result = getBaseUrl(true);
      expect(result).toBe('https://app.example.com');
    });

    it('should fall back to NEXTAUTH_URL when others not set', () => {
      delete process.env.BASE_URL;
      delete process.env.NEXT_PUBLIC_APP_URL;
      process.env.NEXTAUTH_URL = 'https://auth.example.com';

      const result = getBaseUrl(true);
      expect(result).toBe('https://auth.example.com');
    });

    it('should use localhost fallback when no env vars set', () => {
      delete process.env.BASE_URL;
      delete process.env.NEXT_PUBLIC_APP_URL;
      delete process.env.NEXTAUTH_URL;

      const result = getBaseUrl(true);
      expect(result).toBe('http://localhost:3000');
    });
  });

  describe('server context (forServer=true)', () => {
    it('should always return absolute URL in server context', () => {
      process.env.NEXT_PUBLIC_APP_URL = 'https://app.example.com';
      
      const result = getBaseUrl(true);
      expect(result).toBe('https://app.example.com');
    });

    it('should remove trailing slash', () => {
      process.env.NEXT_PUBLIC_APP_URL = 'https://app.example.com/';
      
      const result = getBaseUrl(true);
      expect(result).toBe('https://app.example.com');
    });
  });

  describe('browser context (forServer=false)', () => {
    beforeEach(() => {
      // Mock browser environment
      Object.defineProperty(window, 'location', {
        value: {
          origin: 'https://browser.example.com'
        },
        writable: true
      });
    });

    it('should return empty string for relative URLs when no BASE_URL conflicts', () => {
      delete process.env.BASE_URL;
      
      const result = getBaseUrl(false);
      expect(result).toBe('');
    });

    it('should return window.origin when BASE_URL differs from current origin', () => {
      process.env.BASE_URL = 'https://test.example.com';
      
      const result = getBaseUrl(false);
      expect(result).toBe('https://browser.example.com');
    });

    it('should return empty string when BASE_URL matches current origin', () => {
      process.env.BASE_URL = 'https://browser.example.com';
      
      const result = getBaseUrl(false);
      expect(result).toBe('');
    });
  });

  describe('test environment', () => {
    it('should use absolute URLs in test environment', () => {
      process.env.NODE_ENV = 'test';
      process.env.NEXT_PUBLIC_APP_URL = 'https://test.example.com';
      
      const result = getBaseUrl(false);
      expect(result).toBe('https://test.example.com');
    });

    it('should use absolute URLs when VITEST is set', () => {
      (process as any).env.VITEST = 'true';
      process.env.NEXT_PUBLIC_APP_URL = 'https://vitest.example.com';
      
      const result = getBaseUrl(false);
      expect(result).toBe('https://vitest.example.com');
    });

    it('should use absolute URLs when JEST is set', () => {
      (process as any).env.JEST = 'true';
      process.env.NEXT_PUBLIC_APP_URL = 'https://jest.example.com';
      
      const result = getBaseUrl(false);
      expect(result).toBe('https://jest.example.com');
    });
  });

  describe('edge cases', () => {
    it('should handle undefined window.location.origin', () => {
      Object.defineProperty(window, 'location', {
        value: {
          origin: undefined
        },
        writable: true
      });
      
      const result = getBaseUrl(false);
      expect(result).toBe('');
    });

    it('should handle empty string environment variables', () => {
      process.env.BASE_URL = '';
      process.env.NEXT_PUBLIC_APP_URL = '';
      process.env.NEXTAUTH_URL = '';
      
      const result = getBaseUrl(true);
      expect(result).toBe('http://localhost:3000');
    });

    it('should handle URLs with multiple trailing slashes', () => {
      process.env.NEXT_PUBLIC_APP_URL = 'https://app.example.com///';
      
      const result = getBaseUrl(true);
      expect(result).toBe('https://app.example.com');
    });
  });
});