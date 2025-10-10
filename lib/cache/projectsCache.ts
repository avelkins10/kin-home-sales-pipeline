// Simple in-memory cache for projects with configurable TTL
const projectsCache = new Map<string, { data: any; timestamp: number }>();
const CACHE_TTL = parseInt(process.env.PROJECTS_CACHE_TTL_MS || '60000'); // Default 60 seconds
const MAX_CACHE_ENTRIES = parseInt(process.env.PROJECTS_CACHE_MAX || '100'); // Default 100 entries

// Cache statistics for monitoring
let cacheStats = {
  hits: 0,
  misses: 0,
  evictions: 0,
  expiredRemovals: 0,
  currentSize: 0
};

export function getCacheStats() {
  return {
    ...cacheStats,
    currentSize: projectsCache.size,
    ttl: CACHE_TTL,
    maxEntries: MAX_CACHE_ENTRIES
  };
}

export function getCachedProjects(cacheKey: string): { data: any; timestamp: number } | undefined {
  const cached = projectsCache.get(cacheKey);
  if (cached && Date.now() - cached.timestamp < CACHE_TTL) {
    // Refresh timestamp to approximate LRU behavior
    projectsCache.set(cacheKey, { data: cached.data, timestamp: Date.now() });
    cacheStats.hits++;
    cacheStats.currentSize = projectsCache.size;
    return cached;
  }
  return undefined;
}

export function setCachedProjects(cacheKey: string, data: any) {
  cacheStats.misses++;

  // Cache the result
  projectsCache.set(cacheKey, { data, timestamp: Date.now() });

  // Enforce strict 100-entry cap after every cache set
  const now = Date.now();
  const entries = Array.from(projectsCache.entries());

  // First, remove expired entries
  let expiredCount = 0;
  for (const [key, value] of entries) {
    if (now - value.timestamp > CACHE_TTL) {
      projectsCache.delete(key);
      expiredCount++;
    }
  }
  cacheStats.expiredRemovals += expiredCount;

  // If still over MAX_CACHE_ENTRIES, evict oldest entries by timestamp
  if (projectsCache.size > MAX_CACHE_ENTRIES) {
    const remainingEntries = Array.from(projectsCache.entries());
    const toEvict = projectsCache.size - MAX_CACHE_ENTRIES;

    // Sort by timestamp (oldest first) and remove excess
    remainingEntries
      .sort((a, b) => a[1].timestamp - b[1].timestamp)
      .slice(0, toEvict)
      .forEach(([key]) => projectsCache.delete(key));

    cacheStats.evictions += toEvict;
  }

  cacheStats.currentSize = projectsCache.size;
}

export function getCacheTTL() {
  return CACHE_TTL;
}

export function clearProjectsCache() {
  const previousSize = projectsCache.size;
  projectsCache.clear();
  cacheStats.currentSize = 0;
  console.log(`[Cache] Cleared ${previousSize} cached entries`);
  return { cleared: previousSize };
}

export { cacheStats };
