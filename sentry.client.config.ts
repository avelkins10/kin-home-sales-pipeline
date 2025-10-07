// sentry.client.config.ts

// Only initialize if DSN is configured
if (process.env.NEXT_PUBLIC_SENTRY_DSN) {
  // Dynamic import to avoid errors if @sentry/nextjs not installed
  import('@sentry/nextjs').then((Sentry) => {
    Sentry.init({
      dsn: process.env.NEXT_PUBLIC_SENTRY_DSN,
      
      // Environment
      environment: process.env.NODE_ENV,
      
      // Performance monitoring
      tracesSampleRate: 0.1, // 10% of transactions
      
      // Session replay (optional)
      replaysSessionSampleRate: 0.1, // 10% of sessions
      replaysOnErrorSampleRate: 1.0, // 100% of sessions with errors
      
      // Integrations
      integrations: [],
      
      // Filter out noise
      ignoreErrors: [
        'ResizeObserver loop limit exceeded',
        'Non-Error promise rejection captured',
      ],
      
      // Add user context from session (if available)
      beforeSend(event, hint) {
        // Add custom tags
        event.tags = {
          ...event.tags,
          app: 'kin-solar-pipeline',
        };
        return event;
      },
    });
  }).catch(() => {
    // Sentry not installed - graceful degradation
    console.warn('Sentry not available - error tracking disabled');
  });
}
