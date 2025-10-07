// lib/quickbase/client.ts
// This module should only be imported in server-side code (API routes, server components, server actions)
import 'server-only'

import { logQuickbaseRequest, logQuickbaseResponse, logQuickbaseError } from '@/lib/logging/logger';

interface RequestQueueItem {
  request: () => Promise<any>;
  resolve: (value: any) => void;
  reject: (error: any) => void;
  timestamp: number;
}

class QuickbaseClient {
  private realm: string;
  private token: string;
  private requestQueue: RequestQueueItem[] = [];
  private isProcessing = false;
  private requestsThisSecond = 0;
  private lastRequestTime = 0;

  // Quickbase rate limit: 10 requests/second
  private readonly MAX_REQUESTS_PER_SECOND = 10;
  private readonly REQUEST_WINDOW_MS = 1000;

  constructor() {
    if (typeof window !== 'undefined') {
      console.error('QuickbaseClient should not be instantiated on the client-side');
      this.realm = '';
      this.token = '';
      return;
    }
    this.realm = process.env.QUICKBASE_REALM || '';
    this.token = process.env.QUICKBASE_TOKEN || '';

    if (!this.realm || !this.token) {
      console.warn('Quickbase credentials not configured');
    }
  }

  private async waitForRateLimit() {
    const now = Date.now();
    const timeSinceLastRequest = now - this.lastRequestTime;

    // Reset counter if we're in a new second
    if (timeSinceLastRequest >= this.REQUEST_WINDOW_MS) {
      this.requestsThisSecond = 0;
      this.lastRequestTime = now;
      return;
    }

    // If we've hit the limit, wait until the next second
    if (this.requestsThisSecond >= this.MAX_REQUESTS_PER_SECOND) {
      const waitTime = this.REQUEST_WINDOW_MS - timeSinceLastRequest;
      await new Promise(resolve => setTimeout(resolve, waitTime));
      this.requestsThisSecond = 0;
      this.lastRequestTime = Date.now();
    }
  }

  private async processQueue() {
    if (this.isProcessing || this.requestQueue.length === 0) {
      return;
    }

    this.isProcessing = true;

    while (this.requestQueue.length > 0) {
      await this.waitForRateLimit();

      const item = this.requestQueue.shift();
      if (!item) continue;

      try {
        const result = await item.request();
        item.resolve(result);
        this.requestsThisSecond++;
      } catch (error) {
        item.reject(error);
      }
    }

    this.isProcessing = false;
  }

  private queueRequest<T>(request: () => Promise<T>): Promise<T> {
    return new Promise((resolve, reject) => {
      this.requestQueue.push({
        request,
        resolve,
        reject,
        timestamp: Date.now(),
      });
      this.processQueue();
    });
  }

  private async fetchWithRetry(input: RequestInfo | URL, init: RequestInit, attempt: number = 1): Promise<Response> {
    const controller = new AbortController();
    const timeout = setTimeout(() => controller.abort(), 10000);
    try {
      const res = await fetch(input, { ...init, signal: controller.signal });
      if (res.status === 429 || res.status >= 500) {
        if (attempt <= 3) {
          const backoffMs = Math.pow(2, attempt - 1) * 1000; // 1s, 2s, 4s
          await new Promise(r => setTimeout(r, backoffMs));
          return this.fetchWithRetry(input, init, attempt + 1);
        }
      }
      return res;
    } finally {
      clearTimeout(timeout);
    }
  }

  async queryRecords(params: {
    from: string;
    select: number[];
    where?: string;
    sortBy?: { fieldId: number; order: 'ASC' | 'DESC' }[];
    options?: { skip?: number; top?: number };
  }) {
    const startTime = Date.now();
    logQuickbaseRequest('POST', '/v1/records/query', { from: params.from, selectCount: params.select.length });
    
    return this.queueRequest(async () => {
      const response = await this.fetchWithRetry('https://api.quickbase.com/v1/records/query', {
        method: 'POST',
        headers: {
          'QB-Realm-Hostname': this.realm,
          'Authorization': `QB-USER-TOKEN ${this.token}`,
          'Content-Type': 'application/json',
        },
        body: JSON.stringify(params),
      });

      if (!response.ok) {
        const error = await response.json();
        const errorMessage = error.message || response.statusText;
        const errorDetails = {
          status: response.status,
          message: errorMessage,
          description: error.description,
          errorCode: error.errorCode
        };
        console.error('[QuickbaseClient] API Error:', errorDetails);
        logQuickbaseError('POST', '/v1/records/query', new Error(errorMessage));
        throw new Error(`Quickbase API error (${response.status}): ${errorMessage}`);
      }

      const json = await response.json();
      const duration = Date.now() - startTime;
      console.log('[QuickbaseClient] Query successful - returned', json.data?.length || 0, 'records in', duration, 'ms');
      logQuickbaseResponse('POST', '/v1/records/query', duration, json.data?.length);
      return json;
    });
  }

  async updateRecord(params: {
    to: string;
    data: any[];
    fieldsToReturn?: number[];
  }) {
    const startTime = Date.now();
    logQuickbaseRequest('POST', '/v1/records', { to: params.to, recordCount: params.data.length });
    
    return this.queueRequest(async () => {
      const response = await this.fetchWithRetry('https://api.quickbase.com/v1/records', {
        method: 'POST',
        headers: {
          'QB-Realm-Hostname': this.realm,
          'Authorization': `QB-USER-TOKEN ${this.token}`,
          'Content-Type': 'application/json',
        },
        body: JSON.stringify(params),
      });

      if (!response.ok) {
        const error = await response.json();
        const errorMessage = error.message || response.statusText;
        logQuickbaseError('POST', '/v1/records', new Error(errorMessage));
        throw new Error(`Quickbase API error: ${errorMessage}`);
      }

      const json = await response.json();
      const duration = Date.now() - startTime;
      logQuickbaseResponse('POST', '/v1/records', duration, params.data.length);
      return json;
    });
  }

  async getFieldInfo(tableId: string, fieldId: number) {
    const startTime = Date.now();
    logQuickbaseRequest('GET', `/v1/fields/${fieldId}`, { tableId, fieldId });
    
    return this.queueRequest(async () => {
      const response = await this.fetchWithRetry(
        `https://api.quickbase.com/v1/fields/${fieldId}?tableId=${tableId}`,
        {
          method: 'GET',
          headers: {
            'QB-Realm-Hostname': this.realm,
            'Authorization': `QB-USER-TOKEN ${this.token}`,
          },
        }
      );

      if (!response.ok) {
        const errorMessage = `Failed to get field info: ${response.statusText}`;
        logQuickbaseError('GET', `/v1/fields/${fieldId}`, new Error(errorMessage));
        throw new Error(errorMessage);
      }

      const json = await response.json();
      const duration = Date.now() - startTime;
      logQuickbaseResponse('GET', `/v1/fields/${fieldId}`, duration);
      return json;
    });
  }
}

export const qbClient = new QuickbaseClient();
