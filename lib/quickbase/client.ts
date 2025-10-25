// lib/quickbase/client.ts
import { logQuickbaseRequest, logQuickbaseResponse, logQuickbaseError } from '@/lib/logging/logger';

interface QueueItem {
  request: () => Promise<any>;
  resolve: (value: any) => void;
  reject: (reason: any) => void;
  timestamp: number;
}

interface QueryRecordsParams {
  from: string;
  select: number[];
  where?: string;
  sortBy?: Array<{ fieldId: number; order?: 'ASC' | 'DESC' }>;
  groupBy?: Array<{ fieldId: number; grouping?: string }>;
  options?: {
    skip?: number;
    top?: number;
  };
}

interface UpdateRecordParams {
  to: string;
  data: Array<Record<number, { value: any }>>;
  mergeFieldId?: number;
  fieldsToReturn?: number[];
}

export class QuickbaseClient {
  private realm: string;
  private token: string;
  private appId: string;
  private requestQueue: QueueItem[] = [];
  private isProcessing: boolean = false;
  private requestsThisSecond: number = 0;
  private lastRequestTime: number = 0;

  // Quickbase rate limit: 10 requests/second
  private readonly MAX_REQUESTS_PER_SECOND = 10;
  private readonly REQUEST_WINDOW_MS = 1000;
  private readonly MAX_RETRIES = 3;
  private readonly RETRY_DELAY_MS = 1000;
  private readonly CLIENT_VERSION = '2.0.0'; // Force rebuild

  constructor(realm: string, token: string, appId: string) {
    this.realm = realm;
    this.token = token;
    this.appId = appId;

    if (!this.realm || !this.token) {
      console.warn('[QuickbaseClient] Credentials not configured');
    }
  }

  private async waitForRateLimit(): Promise<void> {
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

  private async processQueue(): Promise<void> {
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

  private async fetchWithRetry(url: string, options: RequestInit, retryCount = 0): Promise<Response> {
    try {
      const response = await fetch(url, options);

      // Retry on 429 (rate limit) or 5xx errors
      if ((response.status === 429 || response.status >= 500) && retryCount < this.MAX_RETRIES) {
        const delay = this.RETRY_DELAY_MS * Math.pow(2, retryCount); // Exponential backoff
        console.log(`[QuickbaseClient] Retrying request after ${delay}ms (attempt ${retryCount + 1}/${this.MAX_RETRIES})`);
        await new Promise(resolve => setTimeout(resolve, delay));
        return this.fetchWithRetry(url, options, retryCount + 1);
      }

      return response;
    } catch (error) {
      if (retryCount < this.MAX_RETRIES) {
        const delay = this.RETRY_DELAY_MS * Math.pow(2, retryCount);
        console.log(`[QuickbaseClient] Network error, retrying after ${delay}ms (attempt ${retryCount + 1}/${this.MAX_RETRIES})`);
        await new Promise(resolve => setTimeout(resolve, delay));
        return this.fetchWithRetry(url, options, retryCount + 1);
      }
      throw error;
    }
  }

  async queryRecords(params: QueryRecordsParams): Promise<any> {
    const startTime = Date.now();
    logQuickbaseRequest('POST', '/v1/records/query', params);

    return this.queueRequest(async () => {
      const response = await this.fetchWithRetry(
        'https://api.quickbase.com/v1/records/query',
        {
          method: 'POST',
          headers: {
            'QB-Realm-Hostname': this.realm,
            'Authorization': `QB-USER-TOKEN ${this.token}`,
            'Content-Type': 'application/json',
          },
          body: JSON.stringify(params),
        }
      );

      if (!response.ok) {
        let errorDetails: any = {
          status: response.status,
          statusText: response.statusText
        };

        // Try to parse error response as JSON
        let error: any;
        try {
          error = await response.json();
          errorDetails.error = error;
        } catch (jsonError) {
          // Response isn't JSON - try to get text
          let errorText = response.statusText;
          try {
            errorText = await response.text();
          } catch (textError) {
            // Can't even get text
          }

          console.error('[QuickbaseClient] Query failed - Non-JSON Response:', {
            status: response.status,
            statusText: response.statusText,
            responseText: errorText,
            requestFrom: params.from,
            requestWhere: params.where,
            requestSelect: params.select
          });

          const fullErrorMessage = `QB API Error [${response.status}]: ${errorText} | Request: FROM=${params.from} WHERE=${params.where}`;
          throw new Error(fullErrorMessage);
        }

        // Log the full request and error for debugging
        console.error('[QuickbaseClient] Query failed - Full Details:', JSON.stringify({
          status: response.status,
          qbError: error,
          requestFrom: params.from,
          requestWhere: params.where,
          requestSelect: params.select
        }, null, 2));

        logQuickbaseError('POST', '/v1/records/query', new Error(JSON.stringify(error)));

        // Include full QB error in message for debugging
        const errorMessage = error.message || error.description || response.statusText;
        const fullErrorMessage = `QB API Error [${response.status}]: ${errorMessage} | Full QB Response: ${JSON.stringify(error)} | WHERE: ${params.where}`;

        throw new Error(fullErrorMessage);
      }

      const json = await response.json();
      const duration = Date.now() - startTime;
      logQuickbaseResponse('POST', '/v1/records/query', duration, json.data?.length || 0);

      return json;
    });
  }

  async updateRecord(params: UpdateRecordParams): Promise<any> {
    const startTime = Date.now();
    logQuickbaseRequest('POST', '/v1/records', params);

    return this.queueRequest(async () => {
      const response = await this.fetchWithRetry(
        'https://api.quickbase.com/v1/records',
        {
          method: 'POST',
          headers: {
            'QB-Realm-Hostname': this.realm,
            'Authorization': `QB-USER-TOKEN ${this.token}`,
            'Content-Type': 'application/json',
          },
          body: JSON.stringify(params),
        }
      );

      if (!response.ok) {
        const error = await response.json().catch(() => ({ message: response.statusText }));
        const errorMessage = error.message || error.description || response.statusText;

        console.error('[QuickbaseClient] Update failed:', {
          status: response.status,
          error: error,
          requestParams: params
        });

        logQuickbaseError('POST', '/v1/records', new Error(errorMessage));
        throw new Error(`Quickbase API error: ${errorMessage} - ${JSON.stringify(error)}`);
      }

      const json = await response.json();
      const duration = Date.now() - startTime;
      logQuickbaseResponse('POST', '/v1/records', duration, json.metadata?.totalNumberOfRecordsProcessed || 0);

      return json;
    });
  }

  async uploadFileToRecord(payload: {
    tableId: string;
    recordId: number;
    fieldId: number;
    fileName: string;
    fileData: Buffer
  }): Promise<any> {
    const startTime = Date.now();
    logQuickbaseRequest('POST', `/v1/files`, {
      tableId: payload.tableId,
      recordId: payload.recordId,
      fieldId: payload.fieldId,
      fileName: payload.fileName,
      fileSize: payload.fileData.length
    });

    return this.queueRequest(async () => {
      // Convert buffer to Base64 for QuickBase API
      const base64Data = payload.fileData.toString('base64');

      const response = await this.fetchWithRetry(
        `https://api.quickbase.com/v1/files`,
        {
          method: 'POST',
          headers: {
            'QB-Realm-Hostname': this.realm,
            'Authorization': `QB-USER-TOKEN ${this.token}`,
            'Content-Type': 'application/json',
          },
          body: JSON.stringify({
            to: payload.tableId,
            data: [{
              [3]: { value: payload.recordId }, // Record ID field
              [payload.fieldId]: {
                value: {
                  fileName: payload.fileName,
                  data: base64Data
                }
              }
            }]
          }),
        }
      );

      if (!response.ok) {
        const error = await response.json().catch(() => ({ message: response.statusText }));
        const errorMessage = error.message || error.description || response.statusText;

        console.error('[QuickbaseClient] File upload failed:', {
          status: response.status,
          error: error,
          fileName: payload.fileName,
          fileSize: payload.fileData.length
        });

        logQuickbaseError('POST', '/v1/files', new Error(errorMessage));
        throw new Error(`Quickbase file upload error: ${errorMessage} - ${JSON.stringify(error)}`);
      }

      const json = await response.json();
      const duration = Date.now() - startTime;
      logQuickbaseResponse('POST', '/v1/files', duration, payload.fileData.length);

      console.log(`[QuickbaseClient] Uploaded file ${payload.fileName} (${payload.fileData.length} bytes) to record ${payload.recordId}`);
      return json;
    });
  }

  async getFieldInfo(tableId: string, fieldId: number): Promise<any> {
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

  async deleteRecord(params: { from: string; where: string }): Promise<any> {
    const startTime = Date.now();
    logQuickbaseRequest('DELETE', '/v1/records', params);

    return this.queueRequest(async () => {
      const response = await this.fetchWithRetry(
        'https://api.quickbase.com/v1/records',
        {
          method: 'DELETE',
          headers: {
            'QB-Realm-Hostname': this.realm,
            'Authorization': `QB-USER-TOKEN ${this.token}`,
            'Content-Type': 'application/json',
          },
          body: JSON.stringify({
            from: params.from,
            where: params.where
          }),
        }
      );

      if (!response.ok) {
        const error = await response.json().catch(() => ({ message: response.statusText }));
        const errorMessage = error.message || error.description || response.statusText;

        console.error('[QuickbaseClient] Delete failed:', {
          status: response.status,
          error: error,
          requestParams: params
        });

        logQuickbaseError('DELETE', '/v1/records', new Error(errorMessage));
        throw new Error(`Quickbase delete error: ${errorMessage} - ${JSON.stringify(error)}`);
      }

      const json = await response.json();
      const duration = Date.now() - startTime;
      logQuickbaseResponse('DELETE', '/v1/records', duration, json.numberDeleted || 0);

      console.log(`[QuickbaseClient] Deleted ${json.numberDeleted || 0} record(s) from ${params.from}`);
      return json;
    });
  }
}

// Create and export a configured client instance
const realm = process.env.QUICKBASE_REALM || '';
const token = process.env.QUICKBASE_TOKEN || '';
const appId = process.env.QUICKBASE_APP_ID || '';

export const qbClient = new QuickbaseClient(realm, token, appId);
