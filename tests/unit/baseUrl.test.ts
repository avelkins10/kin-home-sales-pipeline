import { describe, it, expect, vi, beforeEach, afterEach } from 'vitest';
import { getBaseUrl } from '@/lib/utils/baseUrl';

describe('getBaseUrl', () => {
  const originalEnv = process.env;
  const originalWindow = global.window;

  beforeEach(() => {
    vi.resetModules();
    process.env = { ...originalEnv };
    delete (global as any).window;
  });

  afterEach(() => {
    process.env = originalEnv;
    global.window = originalWindow;
  });

  describe('server context (forServer: true)', () => {
    it('should use BASE_URL when available', () => {
      process.env.BASE_URL = 'https://test.example.com';
      expect(getBaseUrl(true)).toBe('https://test.example.com');
    });

    it('should fall back to NEXT_PUBLIC_APP_URL', () => {
      process.env.NEXT_PUBLIC_APP_URL = 'https://app.example.com';
      expect(getBaseUrl(true)).toBe('https://app.example.com');
    });

    it('should fall back to NEXTAUTH_URL', () => {
      process.env.NEXTAUTH_URL = 'https://auth.example.com';
      expect(getBaseUrl(true)).toBe('https://auth.example.com');
    });

    it('should use localhost fallback when no env vars', () => {
      expect(getBaseUrl(true)).toBe('http://localhost:3000');
    });

    it('should remove trailing slash', () => {
      process.env.BASE_URL = 'https://test.example.com/';
      expect(getBaseUrl(true)).toBe('https://test.example.com');
    });
  });

  describe('test environment', () => {
    it('should use BASE_URL in test environment', () => {
      process.env.NODE_ENV = 'test';
      process.env.BASE_URL = 'https://test.example.com';
      expect(getBaseUrl()).toBe('https://test.example.com');
    });

    it('should use VITEST flag', () => {
      (process as any).env.VITEST = true;
      process.env.BASE_URL = 'https://vitest.example.com';
      expect(getBaseUrl()).toBe('https://vitest.example.com');
    });

    it('should use JEST flag', () => {
      (process as any).env.JEST = true;
      process.env.BASE_URL = 'https://jest.example.com';
      expect(getBaseUrl()).toBe('https://jest.example.com');
    });
  });

  describe('browser context (non-test)', () => {
    beforeEach(() => {
      global.window = {
        location: {
          origin: 'https://browser.example.com'
        }
      } as any;
    });

    it('should use window.location.origin when BASE_URL differs', () => {
      process.env.BASE_URL = 'https://different.example.com';
      expect(getBaseUrl()).toBe('https://browser.example.com');
    });

    it('should return empty string for relative URLs when BASE_URL matches', () => {
      process.env.BASE_URL = 'https://browser.example.com';
      expect(getBaseUrl()).toBe('');
    });

    it('should return empty string when no BASE_URL is set', () => {
      expect(getBaseUrl()).toBe('');
    });

    it('should handle missing window.location.origin', () => {
      global.window = {} as any;
      expect(getBaseUrl()).toBe('');
    });
  });

  describe('edge cases', () => {
    it('should handle undefined window', () => {
      global.window = undefined as any;
      expect(getBaseUrl()).toBe('http://localhost:3000');
    });

    it('should handle empty string env vars', () => {
      process.env.BASE_URL = '';
      process.env.NEXT_PUBLIC_APP_URL = '';
      process.env.NEXTAUTH_URL = '';
      expect(getBaseUrl(true)).toBe('http://localhost:3000');
    });
  });
});
