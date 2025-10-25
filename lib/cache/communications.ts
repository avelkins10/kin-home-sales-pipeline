/**
 * Communications cache helper functions
 * Provides centralized cache management for communications data
 */

import { PCInboundQueueData, PCCommunicationsData, PCConversationFilters } from '@/lib/types/operations';

// Cache configuration
const CACHE_TTL = 30 * 1000; // 30 seconds
const MAX_INBOUND_CACHE_SIZE = 50;
const MAX_CONVERSATIONS_CACHE_SIZE = 100;

// Centralized caches
const inboundQueueCache = new Map<string, { data: PCInboundQueueData; timestamp: number }>();
const conversationCache = new Map<string, { data: PCCommunicationsData; timestamp: number }>();

// Cache keys for different data types
const CACHE_KEYS = {
  INBOUND_QUEUE: 'pc-inbound-queue',
  CONVERSATIONS: 'pc-conversations',
} as const;

/**
 * Get cache key for inbound queue
 */
export function getInboundQueueCacheKey(pcEmail: string): string {
  return `${CACHE_KEYS.INBOUND_QUEUE}:${pcEmail}`;
}

/**
 * Get cache key for conversations with filters
 */
export function getConversationsCacheKey(pcEmail: string, filters: PCConversationFilters): string {
  const filtersKey = JSON.stringify(filters);
  return `${CACHE_KEYS.CONVERSATIONS}:${pcEmail}:${filtersKey}`;
}

/**
 * Get cached inbound queue data
 */
export function getInboundCache(pcEmail: string): { data: PCInboundQueueData; timestamp: number } | null {
  const cacheKey = getInboundQueueCacheKey(pcEmail);
  const cached = inboundQueueCache.get(cacheKey);
  
  if (cached && (Date.now() - cached.timestamp) < CACHE_TTL) {
    console.log(`Cache HIT for inbound queue: ${pcEmail}`);
    return cached;
  }
  
  console.log(`Cache MISS for inbound queue: ${pcEmail}`);
  return null;
}

/**
 * Set cached inbound queue data
 */
export function setInboundCache(pcEmail: string, data: PCInboundQueueData): void {
  const cacheKey = getInboundQueueCacheKey(pcEmail);
  inboundQueueCache.set(cacheKey, {
    data,
    timestamp: Date.now()
  });
  
  // Clean up cache if needed
  cleanupInboundCache();
  console.log(`Cache SET for inbound queue: ${pcEmail}`);
}

/**
 * Get cached conversations data
 */
export function getConversationsCache(pcEmail: string, filters: PCConversationFilters): { data: PCCommunicationsData; timestamp: number } | null {
  const cacheKey = getConversationsCacheKey(pcEmail, filters);
  const cached = conversationCache.get(cacheKey);
  
  if (cached && (Date.now() - cached.timestamp) < CACHE_TTL) {
    console.log(`Cache HIT for conversations: ${pcEmail}`);
    return cached;
  }
  
  console.log(`Cache MISS for conversations: ${pcEmail}`);
  return null;
}

/**
 * Set cached conversations data
 */
export function setConversationsCache(pcEmail: string, filters: PCConversationFilters, data: PCCommunicationsData): void {
  const cacheKey = getConversationsCacheKey(pcEmail, filters);
  conversationCache.set(cacheKey, {
    data,
    timestamp: Date.now()
  });
  
  // Clean up cache if needed
  cleanupConversationsCache();
  console.log(`Cache SET for conversations: ${pcEmail}`);
}

/**
 * Clear inbound queue cache for a specific PC
 */
export function clearInboundQueue(pcEmail: string): void {
  const cacheKey = getInboundQueueCacheKey(pcEmail);
  const deleted = inboundQueueCache.delete(cacheKey);
  console.log(`Clearing inbound queue cache for ${pcEmail}: ${deleted ? 'FOUND and DELETED' : 'NOT FOUND'}`);
}

/**
 * Clear conversations cache for a specific PC
 */
export function clearConversations(pcEmail: string): void {
  // Clear all conversation caches for this PC (regardless of filters)
  const keysToDelete: string[] = [];
  for (const key of conversationCache.keys()) {
    if (key.startsWith(`${CACHE_KEYS.CONVERSATIONS}:${pcEmail}:`)) {
      keysToDelete.push(key);
    }
  }
  
  let deletedCount = 0;
  for (const key of keysToDelete) {
    if (conversationCache.delete(key)) {
      deletedCount++;
    }
  }
  
  console.log(`Clearing conversations cache for ${pcEmail}: ${deletedCount} entries ${deletedCount > 0 ? 'FOUND and DELETED' : 'NOT FOUND'}`);
}

/**
 * Clear all communications caches for a specific PC
 */
export function clearAllCommunicationsCache(pcEmail: string): void {
  clearInboundQueue(pcEmail);
  clearConversations(pcEmail);
}

/**
 * Clean up inbound cache when it gets too large
 */
function cleanupInboundCache(): void {
  if (inboundQueueCache.size > MAX_INBOUND_CACHE_SIZE) {
    const entries = Array.from(inboundQueueCache.entries());
    entries.sort((a, b) => a[1].timestamp - b[1].timestamp);
    
    // Remove oldest 25% of entries
    const toRemove = Math.floor(entries.length * 0.25);
    for (let i = 0; i < toRemove; i++) {
      inboundQueueCache.delete(entries[i][0]);
    }
    console.log(`Cleaned up ${toRemove} inbound cache entries`);
  }
}

/**
 * Clean up conversations cache when it gets too large
 */
function cleanupConversationsCache(): void {
  if (conversationCache.size > MAX_CONVERSATIONS_CACHE_SIZE) {
    const entries = Array.from(conversationCache.entries());
    entries.sort((a, b) => a[1].timestamp - b[1].timestamp);
    
    // Remove oldest 25% of entries
    const toRemove = Math.floor(entries.length * 0.25);
    for (let i = 0; i < toRemove; i++) {
      conversationCache.delete(entries[i][0]);
    }
    console.log(`Cleaned up ${toRemove} conversations cache entries`);
  }
}

/**
 * Get cache statistics for monitoring
 */
export function getCacheStats(): {
  inbound: { size: number; maxSize: number };
  conversations: { size: number; maxSize: number };
} {
  return {
    inbound: { size: inboundQueueCache.size, maxSize: MAX_INBOUND_CACHE_SIZE },
    conversations: { size: conversationCache.size, maxSize: MAX_CONVERSATIONS_CACHE_SIZE }
  };
}
