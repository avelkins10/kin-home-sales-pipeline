export const runtime = 'nodejs'

import { NextRequest, NextResponse } from 'next/server';
import { logInfo } from '@/lib/logging/logger';

export async function POST(req: NextRequest) {
  // Only allow in production or when explicitly enabled
  if (process.env.NODE_ENV !== 'production' && process.env.ENABLE_WEB_VITALS !== 'true') {
    return NextResponse.json({ error: 'Not available' }, { status: 404 });
  }

  // TODO: Temporarily accept metrics without secret validation
  // In the future, implement server-signed HMAC payload for security
  // For now, reject client-origin requests unless implementing proper authentication
  const userAgent = req.headers.get('user-agent') || '';
  const isClientOrigin = userAgent.includes('Mozilla') || userAgent.includes('Chrome') || userAgent.includes('Safari');
  
  if (isClientOrigin) {
    // Reject direct client calls until proper authentication is implemented
    return NextResponse.json({ error: 'Client-origin requests not allowed' }, { status: 403 });
  }

  try {
    const body = await req.json();
    
    if (body.type === 'web-vitals' && body.metric) {
      const metric = body.metric;
      
      // Log web vitals metrics for monitoring
      logInfo('[WEB_VITALS]', {
        name: metric.name,
        value: metric.value,
        delta: metric.delta,
        id: metric.id,
        navigationType: metric.navigationType,
        timestamp: metric.timestamp,
        url: metric.url,
        userAgent: metric.userAgent,
      });

      // In a real implementation, you might want to:
      // 1. Store metrics in a time-series database (InfluxDB, TimescaleDB)
      // 2. Send to analytics service (Google Analytics, Mixpanel)
      // 3. Alert on performance regressions
      // 4. Aggregate metrics for dashboards
      
      return NextResponse.json({ success: true });
    }

    return NextResponse.json({ error: 'Invalid metric type' }, { status: 400 });
  } catch (error) {
    logInfo('[WEB_VITALS] Error processing metrics', { error: error instanceof Error ? error.message : String(error) });
    return NextResponse.json({ error: 'Internal Server Error' }, { status: 500 });
  }
}
