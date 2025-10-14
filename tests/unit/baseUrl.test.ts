import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import { getBaseUrl } from '@/lib/utils/baseUrl';

describe('getBaseUrl', () => {
  beforeEach(() => {
    // Clear any mocked globals
    vi.unstubAllGlobals();
    // Clear any cached values
    vi.clearAllMocks();
    // Reset env vars
    delete process.env.BASE_URL;
    delete process.env.NEXT_PUBLIC_APP_URL;
    delete process.env.NEXTAUTH_URL;
  });

  afterEach(() => {
    vi.unstubAllGlobals();
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
      // Mock browser environment using vi.stubGlobal
      vi.stubGlobal('window', {
        location: {
          origin: 'https://browser.example.com'
        }
      });
    });

    it('should return localhost fallback when no env vars set in test environment', () => {
      // Note: In test environment (inTest=true), browser logic is not reached
      // The function returns absolute URLs from env vars
      delete process.env.BASE_URL;
      delete process.env.NEXT_PUBLIC_APP_URL;
      delete process.env.NEXTAUTH_URL;

      const result = getBaseUrl(false);
      // In test env, returns fallback
      expect(result).toBe('http://localhost:3000');
    });

    it('should return BASE_URL when set in test environment', () => {
      // Note: In test environment (inTest=true), browser logic is not reached
      process.env.BASE_URL = 'https://test.example.com';

      const result = getBaseUrl(false);
      // In test env, returns BASE_URL from env
      expect(result).toBe('https://test.example.com');
    });

    it('should return BASE_URL even when it matches window.origin', () => {
      // Note: In test environment (inTest=true), browser logic is not reached
      process.env.BASE_URL = 'https://browser.example.com';

      const result = getBaseUrl(false);
      // In test env, returns BASE_URL from env (not empty string)
      expect(result).toBe('https://browser.example.com');
    });
  });

  describe('test environment', () => {
    it('should use absolute URLs in test environment', () => {
      // NODE_ENV is already 'test' in Vitest, so just set the URL
      process.env.NEXT_PUBLIC_APP_URL = 'https://test.example.com';

      // forServer=false but inTest=true, so should return absolute URL
      const result = getBaseUrl(false);
      expect(result).toBe('https://test.example.com');
    });

    it('should use absolute URLs when VITEST is set', () => {
      // VITEST is already set in Vitest environment
      process.env.NEXT_PUBLIC_APP_URL = 'https://vitest.example.com';

      // forServer=false but inTest=true, so should return absolute URL
      const result = getBaseUrl(false);
      expect(result).toBe('https://vitest.example.com');
    });

    it('should use absolute URLs when JEST is set', () => {
      // Simulate JEST environment
      const originalJest = (process as any).env.JEST;
      (process as any).env.JEST = 'true';
      process.env.NEXT_PUBLIC_APP_URL = 'https://jest.example.com';

      // forServer=false but inTest=true, so should return absolute URL
      const result = getBaseUrl(false);
      expect(result).toBe('https://jest.example.com');

      // Cleanup
      if (originalJest === undefined) {
        delete (process as any).env.JEST;
      } else {
        (process as any).env.JEST = originalJest;
      }
    });
  });

  describe('edge cases', () => {
    it('should handle undefined window.location.origin', () => {
      // Mock browser environment with undefined origin
      vi.stubGlobal('window', {
        location: {
          origin: undefined
        }
      });

      // In test environment, it still returns absolute URL from env
      // because inTest is true, which takes precedence over browser check
      process.env.NEXT_PUBLIC_APP_URL = 'https://fallback.example.com';
      const result = getBaseUrl(false);
      expect(result).toBe('https://fallback.example.com');
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