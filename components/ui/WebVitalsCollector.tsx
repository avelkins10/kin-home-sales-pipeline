'use client';

import { useEffect } from 'react';
import { getBaseUrl } from '@/lib/utils/baseUrl';

interface WebVitalsMetric {
  name: string;
  value: number;
  delta: number;
  id: string;
  navigationType: string;
}

// Sample 10% of sessions for performance monitoring
function shouldSample(): boolean {
  // Use a simple hash of sessionStorage to ensure consistent sampling per session
  if (typeof window === 'undefined') return false;
  
  const sessionKey = 'web-vitals-sample';
  let sampleDecision = sessionStorage.getItem(sessionKey);
  
  if (sampleDecision === null) {
    // Generate consistent sample decision for this session
    sampleDecision = Math.random() < 0.1 ? 'true' : 'false';
    sessionStorage.setItem(sessionKey, sampleDecision);
  }
  
  return sampleDecision === 'true';
}

async function sendWebVitals(metric: WebVitalsMetric): Promise<void> {
  try {
    const baseUrl = getBaseUrl();
    const response = await fetch(`${baseUrl}/api/internal/metrics`, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify({
        type: 'web-vitals',
        metric: {
          name: metric.name,
          value: metric.value,
          delta: metric.delta,
          id: metric.id,
          navigationType: metric.navigationType,
          timestamp: Date.now(),
          url: window.location.href,
          userAgent: navigator.userAgent,
        },
      }),
    });

    if (!response.ok) {
      console.warn('[WebVitals] Failed to send metric:', response.status);
    }
  } catch (error) {
    console.warn('[WebVitals] Error sending metric:', error);
  }
}

export function WebVitalsCollector() {
  useEffect(() => {
    // Check if web vitals collection is enabled
    if (process.env.NEXT_PUBLIC_ENABLE_WEB_VITALS !== 'true') {
      return;
    }

    // Check if we should sample this session
    if (!shouldSample()) {
      return;
    }

    // Dynamically import web-vitals to avoid bundle bloat
    // Note: onFID was replaced with onINP in web-vitals v3+
    import('web-vitals').then(({ onCLS, onINP, onFCP, onLCP, onTTFB }) => {
      onCLS(sendWebVitals);
      onINP(sendWebVitals); // Interaction to Next Paint (replaces FID)
      onFCP(sendWebVitals);
      onLCP(sendWebVitals);
      onTTFB(sendWebVitals);
    }).catch((error) => {
      console.warn('[WebVitals] Failed to load web-vitals library:', error);
    });
  }, []);

  return null; // This component doesn't render anything
}
