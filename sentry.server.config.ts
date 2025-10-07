// sentry.server.config.ts

// Only initialize if DSN is configured
if (process.env.SENTRY_DSN) {
  // Dynamic import to avoid errors if @sentry/nextjs not installed
  import('@sentry/nextjs').then((Sentry) => {
    Sentry.init({
      dsn: process.env.SENTRY_DSN,
      
      // Environment
      environment: process.env.NODE_ENV,
      
      // Performance monitoring
      tracesSampleRate: 0.1, // 10% of transactions
      
      // Server-specific settings
      integrations: [
        // Add custom integrations if needed
      ],
      
      // Filter out noise
      ignoreErrors: [
        'ECONNRESET',
        'ETIMEDOUT',
      ],
      
      // Add context to errors
      beforeSend(event, hint) {
        // Add custom tags
        event.tags = {
          ...event.tags,
          app: 'kin-solar-pipeline',
          runtime: 'server',
        };
        
        // Add server context
        event.contexts = {
          ...event.contexts,
          runtime: {
            name: 'node',
            version: process.version,
          },
        };
        
        return event;
      },
    });
  }).catch(() => {
    // Sentry not installed - graceful degradation
    console.warn('Sentry not available - server error tracking disabled');
  });
}
