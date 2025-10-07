// Service Worker for Kin Solar PWA
const CACHE_VERSION = 2;
const CACHE_NAME = `kin-solar-v${CACHE_VERSION}`;
const STATIC_CACHE_NAME = `kin-solar-static-v${CACHE_VERSION}`;
const API_CACHE_NAME = `kin-solar-api-v${CACHE_VERSION}`;

// Assets to cache (static)
const STATIC_ASSETS = [
  '/',
  '/login',
  '/manifest.json',
  '/icons/icon-192x192.png',
  '/icons/icon-512x512.png',
  '/icons/icon-180x180.png'
];

// Install event - cache static assets
self.addEventListener('install', (event) => {
  console.log('Service Worker installing...');
  
  event.waitUntil(
    caches.open(STATIC_CACHE_NAME)
      .then((cache) => {
        console.log('Caching static assets');
        return cache.addAll(STATIC_ASSETS);
      })
      .then(() => {
        console.log('Static assets cached successfully');
        return self.skipWaiting();
      })
      .catch((error) => {
        console.error('Failed to cache static assets:', error);
      })
  );
});

// Activate event - clean up old caches
self.addEventListener('activate', (event) => {
  console.log('Service Worker activating...');
  
  event.waitUntil(
    caches.keys()
      .then((cacheNames) => {
        return Promise.all(
          cacheNames.map((cacheName) => {
            // Delete old caches that don't match current version
            if (![
              STATIC_CACHE_NAME,
              API_CACHE_NAME,
              CACHE_NAME
            ].includes(cacheName)) {
              console.log('Deleting old cache:', cacheName);
              return caches.delete(cacheName);
            }
          })
        );
      })
      .then(() => {
        console.log('Service Worker activated');
        return self.clients.claim();
      })
  );
});

// Fetch event - implement caching strategies
self.addEventListener('fetch', (event) => {
  const { request } = event;
  const url = new URL(request.url);
  
  // Skip non-GET requests
  if (request.method !== 'GET') {
    return;
  }
  
  // Skip chrome-extension and other non-http requests
  if (!url.protocol.startsWith('http')) {
    return;
  }
  
  event.respondWith(handleRequest(request));
});

async function handleRequest(request) {
  const url = new URL(request.url);
  
  // Static assets (JS, CSS, images) - Cache First
  if (isStaticAsset(url)) {
    return cacheFirst(request, STATIC_CACHE_NAME);
  }
  
  // API routes - Network First with cache fallback
  if (url.pathname.startsWith('/api/')) {
    return networkFirstWithCache(request, API_CACHE_NAME);
  }
  
  // Navigation requests (HTML pages) - Network First
  if (request.mode === 'navigate') {
    return networkFirstWithFallback(request);
  }
  
  // Default: try network first, fallback to cache
  return networkFirstWithCache(request, CACHE_NAME);
}

function isStaticAsset(url) {
  return (
    url.pathname.startsWith('/_next/static/') ||
    url.pathname.startsWith('/icons/') ||
    url.pathname.endsWith('.js') ||
    url.pathname.endsWith('.css') ||
    url.pathname.endsWith('.png') ||
    url.pathname.endsWith('.jpg') ||
    url.pathname.endsWith('.jpeg') ||
    url.pathname.endsWith('.gif') ||
    url.pathname.endsWith('.svg') ||
    url.pathname.endsWith('.ico')
  );
}

// Cache First strategy
async function cacheFirst(request, cacheName) {
  try {
    const cachedResponse = await caches.match(request);
    if (cachedResponse) {
      return cachedResponse;
    }
    
    const networkResponse = await fetch(request);
    if (networkResponse.ok) {
      const cache = await caches.open(cacheName);
      cache.put(request, networkResponse.clone());
    }
    
    return networkResponse;
  } catch (error) {
    console.error('Cache first strategy failed:', error);
    return new Response('Offline', { status: 503 });
  }
}

// Network First with cache fallback
async function networkFirstWithCache(request, cacheName) {
  try {
    const networkResponse = await fetch(request);
    
    if (networkResponse.ok) {
      // Cache successful responses with 5-minute TTL
      const cache = await caches.open(cacheName);
      const responseToCache = networkResponse.clone();
      
      // Add timestamp header for TTL
      const headers = new Headers(responseToCache.headers);
      headers.set('sw-cached-at', Date.now().toString());
      
      const cachedResponse = new Response(responseToCache.body, {
        status: responseToCache.status,
        statusText: responseToCache.statusText,
        headers: headers
      });
      
      cache.put(request, cachedResponse);
    }
    
    return networkResponse;
  } catch (error) {
    console.log('Network failed, trying cache:', request.url);
    
    const cachedResponse = await caches.match(request);
    if (cachedResponse) {
      // Check if cache is within 5 minutes
      const cachedAt = cachedResponse.headers.get('sw-cached-at');
      if (cachedAt) {
        const age = Date.now() - parseInt(cachedAt);
        if (age < 300000) { // 5 minutes
          return cachedResponse;
        }
      }
    }
    
    // Return offline fallback for API requests
    if (request.url.includes('/api/')) {
      return new Response(JSON.stringify({ 
        offline: true, 
        cached: false,
        error: 'Network unavailable'
      }), {
        status: 503,
        headers: { 'Content-Type': 'application/json' }
      });
    }
    
    return new Response('Offline', { status: 503 });
  }
}

// Network First with offline fallback
async function networkFirstWithFallback(request) {
  try {
    const networkResponse = await fetch(request);
    return networkResponse;
  } catch (error) {
    console.log('Navigation request failed, serving offline fallback');
    
    // Try to serve cached root page
    const cachedResponse = await caches.match('/');
    if (cachedResponse) {
      return cachedResponse;
    }
    
    // Return basic offline page
    return new Response(`
      <!DOCTYPE html>
      <html>
        <head>
          <title>Kin Solar - Offline</title>
          <meta name="viewport" content="width=device-width, initial-scale=1">
        </head>
        <body>
          <div style="text-align: center; padding: 50px; font-family: Arial, sans-serif;">
            <h1>You're offline</h1>
            <p>Please check your internet connection and try again.</p>
            <button onclick="window.location.reload()">Retry</button>
          </div>
        </body>
      </html>
    `, {
      status: 200,
      headers: { 'Content-Type': 'text/html' }
    });
  }
}

// Message handling for cache management
self.addEventListener('message', (event) => {
  const { data } = event;
  
  switch (data.command) {
    case 'SKIP_WAITING':
      self.skipWaiting();
      break;
      
    case 'CLEAR_CACHE':
      clearAllCaches();
      break;
      
    case 'GET_CACHE_STATUS':
      getCacheStatus().then((status) => {
        event.ports[0].postMessage(status);
      });
      break;
  }
});

// Notify clients when a new SW is waiting
self.addEventListener('statechange', (event) => {
  // No-op: statechange is on ServiceWorker instances; use controllerchange below
});

self.addEventListener('controllerchange', () => {
  // When a new service worker takes control, notify clients
  self.clients.matchAll().then((clients) => {
    for (const client of clients) {
      client.postMessage({ type: 'SW_UPDATED', version: CACHE_VERSION });
    }
  });
});

async function clearAllCaches() {
  const cacheNames = await caches.keys();
  await Promise.all(
    cacheNames.map(cacheName => caches.delete(cacheName))
  );
  console.log('All caches cleared');
}

async function getCacheStatus() {
  const cacheNames = await caches.keys();
  const status = {};
  
  for (const cacheName of cacheNames) {
    const cache = await caches.open(cacheName);
    const keys = await cache.keys();
    status[cacheName] = keys.length;
  }
  
  return status;
}

// Clean up old cache entries (LRU)
async function cleanupCache(cacheName, maxEntries = 50) {
  const cache = await caches.open(cacheName);
  const keys = await cache.keys();
  
  if (keys.length > maxEntries) {
    // Remove oldest entries (simple LRU)
    const entriesToDelete = keys.slice(0, keys.length - maxEntries);
    await Promise.all(
      entriesToDelete.map(key => cache.delete(key))
    );
    console.log(`Cleaned up ${entriesToDelete.length} old cache entries`);
  }
}

// Periodic cache cleanup
setInterval(() => {
  cleanupCache(API_CACHE_NAME);
  cleanupCache(CACHE_NAME);
}, 300000); // Every 5 minutes
