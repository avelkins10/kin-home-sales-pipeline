// lib/logging/logger.ts

// Database access removed to keep this module client-safe

const isDev = process.env.NODE_ENV === 'development';
const isProd = process.env.NODE_ENV === 'production';
const SENTRY_DSN = process.env.NEXT_PUBLIC_SENTRY_DSN;

function redactSensitive(input: any): any {
  const replacer = (key: string, value: any) => {
    if (typeof value === 'string') {
      if (/([A-Za-z0-9_]*TOKEN[A-Za-z0-9_]*)|API_KEY|PASSWORD|SECRET/i.test(key) || /QB-USER-TOKEN\s+[A-Za-z0-9-_]+/i.test(value)) {
        return '***REDACTED***';
      }
      if (/^[A-Za-z0-9-_]{24,}\.[A-Za-z0-9-_]{6,}\.[A-Za-z0-9-_]{24,}$/.test(value)) {
        return '***REDACTED***';
      }
    }
    return value;
  };
  try {
    return JSON.parse(JSON.stringify(input, replacer));
  } catch {
    return input;
  }
}

// Sentry stub - disabled for now
let Sentry: any = null;

// Throttle dev warnings for missing configuration
let lastMissingSecretWarnAt: number | null = null;

/**
 * Log general information
 * @param message - Log message
 * @param context - Additional context object
 */
export function logInfo(message: string, context?: Record<string, any>) {
  if (isDev) {
    console.log('[INFO]', message, redactSensitive(context));
  } else if (context?.important) {
    // Only log important info in production
    console.log(message, redactSensitive(context));
  }
}

/**
 * Log warnings
 * @param message - Warning message
 * @param context - Additional context object
 */
export function logWarn(message: string, context?: Record<string, any>) {
  if (isDev) {
    console.warn('[WARN]', message, redactSensitive(context));
  } else {
    console.warn(message, redactSensitive(context));
  }
  
  // Optionally capture to Sentry
  if (Sentry && SENTRY_DSN) {
    try {
      Sentry.addBreadcrumb({
        message,
        level: 'warning',
        data: context,
      });
    } catch (error) {
      // Don't let Sentry errors break the app
    }
  }
}

/**
 * Log errors with optional Sentry capture
 * @param message - Error message
 * @param error - Error object
 * @param context - Additional context object
 */
export function logError(message: string, error?: Error, context?: Record<string, any>) {
  if (isDev) {
    console.error('[ERROR]', message, error, redactSensitive(context));
  } else {
    console.error(message, error, redactSensitive(context));
  }
  
  // Capture to Sentry if configured
  if (Sentry && SENTRY_DSN && error) {
    try {
      Sentry.captureException(error, {
        tags: context,
        extra: { message },
      });
    } catch (sentryError) {
      // Don't let Sentry errors break the app
      console.error('Failed to capture error to Sentry:', sentryError);
    }
  }
}

/**
 * Log debug information (development only)
 * @param message - Debug message
 * @param context - Additional context object
 */
export function logDebug(message: string, context?: Record<string, any>) {
  if (isDev) {
    console.debug('[DEBUG]', message, redactSensitive(context));
  }
  // No-op in production
}

/**
 * Log Quickbase API requests
 * @param method - HTTP method
 * @param endpoint - API endpoint
 * @param params - Request parameters (sanitized in production)
 */
export function logQuickbaseRequest(method: string, endpoint: string, params?: any) {
  if (isDev) {
    console.log(`[QB] ${method} ${endpoint}`, redactSensitive(params));
  } else {
    // In production, only log method and endpoint (no sensitive data)
    console.log(`[QB] ${method} ${endpoint}`);
  }
}

/**
 * Log Quickbase API responses with timing
 * @param method - HTTP method
 * @param endpoint - API endpoint
 * @param duration - Request duration in milliseconds
 * @param recordCount - Number of records returned
 */
export function logQuickbaseResponse(method: string, endpoint: string, duration: number, recordCount?: number) {
  const message = `[QB] ${method} ${endpoint} - ${duration}ms${recordCount ? ` - ${recordCount} records` : ''}`;
  
  if (duration > 2000) {
    // Log slow queries as warnings
    logWarn(`Slow Quickbase query: ${message}`);
  } else {
    console.log(message);
  }
}

/**
 * Log Quickbase API errors
 * @param method - HTTP method
 * @param endpoint - API endpoint
 * @param error - Error object
 */
export function logQuickbaseError(method: string, endpoint: string, error: Error) {
  const context = {
    service: 'quickbase',
    method,
    endpoint,
  };
  
  logError(`Quickbase API error: ${method} ${endpoint}`, error, context);
}

/**
 * Standard API request log. Includes method, path, optional requestId and metadata.
 */
export function logApiRequest(
  method: string,
  path: string,
  meta?: Record<string, any>,
  requestId?: string
) {
  const context = { requestId, path, method, ...meta };
  // Dev: verbose, Prod: info level minimal
  logInfo(`[API] ${method} ${path} -> request`, isDev ? context : { requestId, path, method });
}

/**
 * Standard API response log. Includes duration, status/meta, warns when slow (>2s).
 */
export function logApiResponse(
  method: string,
  path: string,
  durationMs: number,
  meta?: Record<string, any>,
  requestId?: string
) {
  const base = { requestId, path, method, durationMs };
  const context = { ...base, ...meta };
  if (durationMs > 2000) {
    logWarn(`[API] ${method} ${path} -> slow response`, context);
  } else {
    logInfo(`[API] ${method} ${path} -> response`, isDev ? context : base);
  }
}

/**
 * Log offline sync events
 * @param event - Sync event type
 * @param context - Event context with synced/failed counts or error
 */
export function logSyncEvent(event: 'start' | 'success' | 'failure', context: { synced?: number; failed?: number; error?: Error; pendingCount?: number }) {
  if (event === 'start') {
    logInfo(`[SYNC] Starting sync - ${context.synced || 0} pending mutations`);
  } else if (event === 'success') {
    logInfo(`[SYNC] Success - synced: ${context.synced || 0}, failed: ${context.failed || 0}`);
  } else if (event === 'failure') {
    logError('[SYNC] Failed to sync pending mutations', context.error, {
      synced: context.synced || 0,
      failed: context.failed || 0,
    });
  }
}

export async function logAudit(
  action: string,
  resource: string,
  resourceId: string,
  userId: string,
  changes?: Record<string, { old: any; new: any }>,
  meta?: { ipAddress?: string; userAgent?: string; requestId?: string }
): Promise<void> {
  const auditEvent = {
    timestamp: new Date().toISOString(),
    action,
    resource,
    resourceId,
    userId,
    changes: changes || {},
    requestId: meta?.requestId,
  };

  // Log to console in development
  if (isDev) {
    console.log('[AUDIT]', auditEvent);
  }

  // Client-side: warn and skip audit API call
  if (typeof window !== 'undefined') {
    try {
      console.warn('[AUDIT] logAudit called in browser; skipping network call');
    } catch {}
    return;
  }

  // Skip if internal secret is missing; warn in development (throttled)
  const internalSecret = process.env.INTERNAL_API_SECRET;
  if (!internalSecret) {
    if (isDev) {
      const now = Date.now();
      if (!lastMissingSecretWarnAt || now - lastMissingSecretWarnAt > 60_000) {
        lastMissingSecretWarnAt = now;
        console.warn(
          '[AUDIT] INTERNAL_API_SECRET is missing. Skipping audit API call. Configure INTERNAL_API_SECRET in .env.local and Vercel envs.'
        );
      }
    }
    return;
  }

  // Server-side: best-effort fire-and-forget with enhanced error capture
  const { getBaseUrl } = await import('@/lib/utils/baseUrl');
  const baseUrl = getBaseUrl(true);

  const startTime = Date.now();

  const attempt = async (attemptNumber: number): Promise<Response> => {
    const controller = new AbortController();
    const timeout = setTimeout(() => controller.abort(), 5000);
    try {
      const res = await fetch(`${baseUrl}/api/internal/audit`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
          'x-internal-secret': internalSecret,
        },
        body: JSON.stringify({
          action,
          resource,
          resourceId,
          userId,
          changes: changes || {},
          ipAddress: meta?.ipAddress,
          userAgent: meta?.userAgent,
          requestId: meta?.requestId,
        }),
        signal: controller.signal,
      });
      return res;
    } finally {
      clearTimeout(timeout);
    }
  };

  void (async () => {
    let response: Response | null = null;
    let lastError: any = null;
    const maxAttempts = 3; // initial + 2 retries
    for (let i = 1; i <= maxAttempts; i++) {
      try {
        response = await attempt(i);
        if (!response.ok) {
          lastError = new Error(`HTTP ${response.status}`);
          if (i < maxAttempts) {
            await new Promise(r => setTimeout(r, 250 * i));
            continue;
          }
        }
        break;
      } catch (err) {
        lastError = err;
        if (i < maxAttempts) {
          await new Promise(r => setTimeout(r, 250 * i));
          continue;
        }
      }
    }

    if (!response) {
      // All attempts failed before a response
      const duration = Date.now() - startTime;
      const errorContext = { action, resource, requestId: meta?.requestId, duration };
      logWarn('[AUDIT] failed', errorContext);
      return;
    }

    const duration = Date.now() - startTime;
    if (!response.ok) {
      // Log audit failure with structured error capture
      const errorContext = {
        action,
        resource,
        requestId: meta?.requestId,
        status: response.status,
        statusText: response.statusText,
        duration,
      };
      
      if (isDev) {
        console.warn('[AUDIT] Failed to log audit event:', errorContext);
      } else {
        // In production, emit a single warning for ops visibility
        logWarn('[AUDIT] failed', errorContext);
      }
      
      // Optionally integrate with Sentry for audit failures
      if (isProd && SENTRY_DSN) {
        try {
          if (Sentry) {
            Sentry.addBreadcrumb({
              message: 'Audit logging failed',
              level: 'warning',
              data: errorContext,
            });
          }
        } catch (sentryError) {
          // Don't let Sentry errors break the app
        }
      }
    } else if (duration > 2000) {
      // Log slow audit requests
      logWarn('[AUDIT] slow request', { action, resource, requestId: meta?.requestId, duration });
    }
  })().catch(() => {});
}
