/**
 * Shared cache instance for escalations across routes
 * This ensures cache invalidation works properly between GET and PATCH routes
 */

import { PCEscalation, PCEscalationStats } from '@/lib/types/operations';

// Shared cache instance
export const cache = new Map<string, { 
  data: PCEscalation[], 
  stats: PCEscalationStats, 
  timestamp: number 
}>();

/**
 * Invalidate cache entries for a specific PC email
 * @param pcEmail - The PC email to invalidate cache for
 */
export function invalidateForUser(pcEmail: string): void {
  const keysToDelete: string[] = [];
  
  for (const key of cache.keys()) {
    if (key.startsWith(`pc-escalations:${pcEmail}:`)) {
      keysToDelete.push(key);
    }
  }
  
  keysToDelete.forEach(key => cache.delete(key));
  
  console.log(`Invalidated ${keysToDelete.length} escalation cache entries for ${pcEmail}`);
}

/**
 * Clean up old cache entries to prevent memory leaks
 * @param maxAge - Maximum age in milliseconds (default: 5 minutes)
 */
export function cleanupCache(maxAge: number = 5 * 60 * 1000): void {
  const now = Date.now();
  const keysToDelete: string[] = [];
  
  for (const [key, value] of cache.entries()) {
    if (now - value.timestamp > maxAge) {
      keysToDelete.push(key);
    }
  }
  
  keysToDelete.forEach(key => cache.delete(key));
  
  if (keysToDelete.length > 0) {
    console.log(`Cleaned up ${keysToDelete.length} expired escalation cache entries`);
  }
}
