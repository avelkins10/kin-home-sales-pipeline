/**
 * Centralized base URL resolver to avoid divergence across modules.
 * 
 * Precedence:
 * 1. process.env.BASE_URL (tests)
 * 2. (if browser) window.location.origin
 * 3. process.env.NEXT_PUBLIC_APP_URL
 * 4. (server) process.env.NEXTAUTH_URL
 * 5. 'http://localhost:3000' (fallback)
 * 
 * @param forServer - Whether this is being called from server context
 * @returns Origin string without trailing slash
 */
export function getBaseUrl(forServer: boolean = false): string {
  const inBrowser = typeof window !== 'undefined';
  const inTest = process.env.NODE_ENV === 'test' || (process as any).env.VITEST || (process as any).env.JEST;

  // In tests or on the server, always return an absolute base URL using precedence
  if (!inBrowser || inTest || forServer) {
    const base =
      process.env.BASE_URL ||
      process.env.NEXT_PUBLIC_APP_URL ||
      process.env.NEXTAUTH_URL ||
      'http://localhost:3000';
    return base.replace(/\/+$/, ''); // Remove all trailing slashes
  }

  // In the browser (non-test): prefer relative URLs for same-origin calls
  // If BASE_URL is defined and differs from current origin, use absolute origin to avoid ambiguity
  const origin = window.location?.origin || '';
  const testBase = process.env.BASE_URL;
  if (testBase && origin && origin !== testBase) {
    return origin.replace(/\/+$/, ''); // Remove all trailing slashes
  }
  // Otherwise allow relative URLs
  return '';
}
