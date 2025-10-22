// lib/utils/baseUrl.ts
export function getBaseUrl(): string {
  if (typeof window !== 'undefined') {
    // Client-side
    return window.location.origin;
  }
  
  if (process.env.VERCEL_URL) {
    // Vercel deployment
    return `https://${process.env.VERCEL_URL}`;
  }
  
  if (process.env.NEXTAUTH_URL) {
    // NextAuth URL
    return process.env.NEXTAUTH_URL;
  }
  
  // Fallback for local development
  return 'http://localhost:3000';
}