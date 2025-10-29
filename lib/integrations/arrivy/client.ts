// lib/integrations/arrivy/client.ts
import { logArrivyRequest, logArrivyResponse, logArrivyError } from '@/lib/logging/logger';
import type {
  ArrivyConfig,
  ArrivyTask,
  ArrivyTaskCreateParams,
  ArrivyTaskUpdateParams,
  ArrivyTaskStatus,
  ArrivyTaskStatusCreateParams,
  ArrivyEntity,
  ArrivyEntityCreateParams,
  ArrivyLocationReport,
  ArrivyApiResponse,
  ArrivyApiError,
} from './types';

interface QueueItem {
  request: () => Promise<any>;
  resolve: (value: any) => void;
  reject: (reason: any) => void;
  timestamp: number;
}

export class ArrivyClient {
  private config: ArrivyConfig;
  private requestQueue: QueueItem[] = [];
  private isProcessing: boolean = false;
  private requestsThisMinute: number = 0;
  private lastRequestTime: number = 0;

  // Arrivy rate limit: 30 requests per minute (configurable)
  private readonly MAX_REQUESTS_PER_MINUTE: number;
  private readonly REQUEST_WINDOW_MS = 60000; // 1 minute
  private readonly MAX_RETRIES = 3;
  private readonly RETRY_DELAY_MS = 1000;

  constructor(config: ArrivyConfig) {
    this.config = config;
    this.MAX_REQUESTS_PER_MINUTE = config.rateLimit || 30;

    if (!this.config.authKey || !this.config.authToken) {
      console.warn('[ArrivyClient] Credentials not configured');
    }
  }

  private async waitForRateLimit(): Promise<void> {
    const now = Date.now();
    const timeSinceLastRequest = now - this.lastRequestTime;

    // Reset counter if we're in a new minute
    if (timeSinceLastRequest >= this.REQUEST_WINDOW_MS) {
      this.requestsThisMinute = 0;
      this.lastRequestTime = now;
      return;
    }

    // If we've hit the limit, wait until the next minute
    if (this.requestsThisMinute >= this.MAX_REQUESTS_PER_MINUTE) {
      const waitTime = this.REQUEST_WINDOW_MS - timeSinceLastRequest;
      await new Promise(resolve => setTimeout(resolve, waitTime));
      this.requestsThisMinute = 0;
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
        this.requestsThisMinute++;
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
        console.log(`[ArrivyClient] Retrying request after ${delay}ms (attempt ${retryCount + 1}/${this.MAX_RETRIES})`);
        await new Promise(resolve => setTimeout(resolve, delay));
        return this.fetchWithRetry(url, options, retryCount + 1);
      }

      return response;
    } catch (error) {
      if (retryCount < this.MAX_RETRIES) {
        const delay = this.RETRY_DELAY_MS * Math.pow(2, retryCount);
        console.log(`[ArrivyClient] Network error, retrying after ${delay}ms (attempt ${retryCount + 1}/${this.MAX_RETRIES})`);
        await new Promise(resolve => setTimeout(resolve, delay));
        return this.fetchWithRetry(url, options, retryCount + 1);
      }
      throw error;
    }
  }

  private getHeaders(): Record<string, string> {
    return {
      'X-Auth-Key': this.config.authKey,
      'X-Auth-Token': this.config.authToken,
      'Content-Type': 'application/json',
    };
  }

  private async handleResponse<T>(response: Response, method: string, endpoint: string): Promise<T> {
    if (!response.ok) {
      let errorDetails: ArrivyApiError = {
        message: response.statusText,
        status: response.status,
      };

      try {
        const error = await response.json();
        errorDetails = {
          message: error.message || error.description || response.statusText,
          status: response.status,
          code: error.code,
          details: error,
        };
      } catch (jsonError) {
        // Response isn't JSON
        try {
          const errorText = await response.text();
          errorDetails.message = errorText || response.statusText;
        } catch (textError) {
          // Can't even get text
        }
      }

      console.error('[ArrivyClient] Request failed:', {
        status: response.status,
        endpoint,
        error: errorDetails,
      });

      logArrivyError(method, endpoint, new Error(errorDetails.message));
      throw new Error(`Arrivy API error: ${errorDetails.message}`);
    }

    const json = await response.json();
    return json as T;
  }

  /**
   * Create a new task
   */
  async createTask(params: ArrivyTaskCreateParams): Promise<ArrivyTask> {
    const startTime = Date.now();
    const endpoint = '/tasks/new';
    logArrivyRequest('POST', endpoint, params);

    return this.queueRequest(async () => {
      const response = await this.fetchWithRetry(
        `${this.config.baseUrl}${endpoint}`,
        {
          method: 'POST',
          headers: this.getHeaders(),
          body: JSON.stringify(params),
        }
      );

      const task = await this.handleResponse<ArrivyTask>(response, 'POST', endpoint);
      const duration = Date.now() - startTime;
      logArrivyResponse('POST', endpoint, duration, 1);

      return task;
    });
  }

  /**
   * Get a specific task by ID
   */
  async getTask(taskId: number): Promise<ArrivyTask> {
    const startTime = Date.now();
    const endpoint = `/tasks/${taskId}`;
    logArrivyRequest('GET', endpoint, { taskId });

    return this.queueRequest(async () => {
      const response = await this.fetchWithRetry(
        `${this.config.baseUrl}${endpoint}`,
        {
          method: 'GET',
          headers: this.getHeaders(),
        }
      );

      const task = await this.handleResponse<ArrivyTask>(response, 'GET', endpoint);
      const duration = Date.now() - startTime;
      logArrivyResponse('GET', endpoint, duration, 1);

      return task;
    });
  }

  /**
   * Update a specific task
   */
  async updateTask(taskId: number, params: ArrivyTaskUpdateParams): Promise<ArrivyTask> {
    const startTime = Date.now();
    const endpoint = `/tasks/${taskId}`;
    logArrivyRequest('PUT', endpoint, params);

    return this.queueRequest(async () => {
      const response = await this.fetchWithRetry(
        `${this.config.baseUrl}${endpoint}`,
        {
          method: 'PUT',
          headers: this.getHeaders(),
          body: JSON.stringify(params),
        }
      );

      const task = await this.handleResponse<ArrivyTask>(response, 'PUT', endpoint);
      const duration = Date.now() - startTime;
      logArrivyResponse('PUT', endpoint, duration, 1);

      return task;
    });
  }

  /**
   * Delete a specific task
   */
  async deleteTask(taskId: number): Promise<void> {
    const startTime = Date.now();
    const endpoint = `/tasks/${taskId}`;
    logArrivyRequest('DELETE', endpoint, { taskId });

    return this.queueRequest(async () => {
      const response = await this.fetchWithRetry(
        `${this.config.baseUrl}${endpoint}`,
        {
          method: 'DELETE',
          headers: this.getHeaders(),
        }
      );

      await this.handleResponse<void>(response, 'DELETE', endpoint);
      const duration = Date.now() - startTime;
      logArrivyResponse('DELETE', endpoint, duration, 1);
    });
  }

  /**
   * List all tasks with optional filters
   *
   * NOTE: Arrivy API does not explicitly document pagination parameters.
   * If the API enforces an implicit limit (suspected ~1000 tasks per call),
   * the caller should implement adaptive date range sub-chunking to ensure
   * complete data retrieval.
   */
  async listTasks(filters?: {
    external_id?: string;
    customer_id?: number;
    entity_ids?: number[];
    group_id?: number;
    start_date?: string | Date;
    end_date?: string | Date;
  }): Promise<ArrivyTask[]> {
    const startTime = Date.now();
    const endpoint = '/tasks';

    // Build query string
    const queryParams = new URLSearchParams();
    if (filters) {
      if (filters.external_id) queryParams.append('external_id', filters.external_id);
      if (filters.customer_id) queryParams.append('customer_id', filters.customer_id.toString());
      if (filters.entity_ids && filters.entity_ids.length > 0) {
        queryParams.append('entity_ids', filters.entity_ids.join(','));
      }
      if (filters.group_id) queryParams.append('group_id', filters.group_id.toString());
      if (filters.start_date) {
        const startDateStr = filters.start_date instanceof Date
          ? filters.start_date.toISOString().split('T')[0]
          : filters.start_date;
        queryParams.append('start_date', startDateStr);
      }
      if (filters.end_date) {
        const endDateStr = filters.end_date instanceof Date
          ? filters.end_date.toISOString().split('T')[0]
          : filters.end_date;
        queryParams.append('end_date', endDateStr);
      }
    }

    const queryString = queryParams.toString();
    const fullEndpoint = queryString ? `${endpoint}?${queryString}` : endpoint;

    logArrivyRequest('GET', fullEndpoint, filters);

    return this.queueRequest(async () => {
      const response = await this.fetchWithRetry(
        `${this.config.baseUrl}${fullEndpoint}`,
        {
          method: 'GET',
          headers: this.getHeaders(),
        }
      );

      const tasks = await this.handleResponse<ArrivyTask[]>(response, 'GET', fullEndpoint);
      const duration = Date.now() - startTime;
      logArrivyResponse('GET', fullEndpoint, duration, tasks.length);

      return tasks;
    });
  }

  /**
   * Alias for listTasks - for consistency with service layer naming
   * Accepts Date objects or ISO strings for start_date and end_date
   */
  async getTasks(filters?: {
    external_id?: string;
    customer_id?: number;
    entity_ids?: number[];
    group_id?: number;
    start_date?: string | Date;
    end_date?: string | Date;
  }): Promise<ArrivyTask[]> {
    return this.listTasks(filters);
  }

  /**
   * Create a new task status
   */
  async createTaskStatus(taskId: number, params: ArrivyTaskStatusCreateParams): Promise<ArrivyTaskStatus> {
    const startTime = Date.now();
    const endpoint = `/tasks/${taskId}/status/new`;
    logArrivyRequest('POST', endpoint, params);

    return this.queueRequest(async () => {
      const response = await this.fetchWithRetry(
        `${this.config.baseUrl}${endpoint}`,
        {
          method: 'POST',
          headers: this.getHeaders(),
          body: JSON.stringify(params),
        }
      );

      const status = await this.handleResponse<ArrivyTaskStatus>(response, 'POST', endpoint);
      const duration = Date.now() - startTime;
      logArrivyResponse('POST', endpoint, duration, 1);

      return status;
    });
  }

  /**
   * Get status history for a task
   */
  async getTaskStatuses(taskId: number): Promise<ArrivyTaskStatus[]> {
    const startTime = Date.now();
    const endpoint = `/tasks/${taskId}/status`;
    logArrivyRequest('GET', endpoint, { taskId });

    return this.queueRequest(async () => {
      const response = await this.fetchWithRetry(
        `${this.config.baseUrl}${endpoint}`,
        {
          method: 'GET',
          headers: this.getHeaders(),
        }
      );

      const statuses = await this.handleResponse<ArrivyTaskStatus[]>(response, 'GET', endpoint);
      const duration = Date.now() - startTime;
      logArrivyResponse('GET', endpoint, duration, statuses.length);

      return statuses;
    });
  }

  /**
   * Create a new entity (crew member)
   */
  async createEntity(params: ArrivyEntityCreateParams): Promise<ArrivyEntity> {
    const startTime = Date.now();
    const endpoint = '/entities/new';
    logArrivyRequest('POST', endpoint, params);

    return this.queueRequest(async () => {
      const response = await this.fetchWithRetry(
        `${this.config.baseUrl}${endpoint}`,
        {
          method: 'POST',
          headers: this.getHeaders(),
          body: JSON.stringify(params),
        }
      );

      const entity = await this.handleResponse<ArrivyEntity>(response, 'POST', endpoint);
      const duration = Date.now() - startTime;
      logArrivyResponse('POST', endpoint, duration, 1);

      return entity;
    });
  }

  /**
   * Get a specific entity by ID
   */
  async getEntity(entityId: number): Promise<ArrivyEntity> {
    const startTime = Date.now();
    const endpoint = `/entities/${entityId}`;
    logArrivyRequest('GET', endpoint, { entityId });

    return this.queueRequest(async () => {
      const response = await this.fetchWithRetry(
        `${this.config.baseUrl}${endpoint}`,
        {
          method: 'GET',
          headers: this.getHeaders(),
        }
      );

      const entity = await this.handleResponse<ArrivyEntity>(response, 'GET', endpoint);
      const duration = Date.now() - startTime;
      logArrivyResponse('GET', endpoint, duration, 1);

      return entity;
    });
  }

  /**
   * List all entities
   */
  async listEntities(): Promise<ArrivyEntity[]> {
    const startTime = Date.now();
    const endpoint = '/entities';
    logArrivyRequest('GET', endpoint, {});

    return this.queueRequest(async () => {
      const response = await this.fetchWithRetry(
        `${this.config.baseUrl}${endpoint}`,
        {
          method: 'GET',
          headers: this.getHeaders(),
        }
      );

      const entities = await this.handleResponse<ArrivyEntity[]>(response, 'GET', endpoint);
      const duration = Date.now() - startTime;
      logArrivyResponse('GET', endpoint, duration, entities.length);

      return entities;
    });
  }

  /**
   * Alias for listEntities - for consistency with service layer naming
   */
  async getEntities(): Promise<ArrivyEntity[]> {
    return this.listEntities();
  }

  /**
   * Get location reports for an entity
   */
  async getEntityLocation(entityId: number, startTime?: string, endTime?: string): Promise<ArrivyLocationReport[]> {
    const start = Date.now();
    const endpoint = `/entities/${entityId}/readings`;
    
    // Build query string
    const queryParams = new URLSearchParams();
    if (startTime) queryParams.append('start_time', startTime);
    if (endTime) queryParams.append('end_time', endTime);
    
    const queryString = queryParams.toString();
    const fullEndpoint = queryString ? `${endpoint}?${queryString}` : endpoint;
    
    logArrivyRequest('GET', fullEndpoint, { entityId, startTime, endTime });

    return this.queueRequest(async () => {
      const response = await this.fetchWithRetry(
        `${this.config.baseUrl}${fullEndpoint}`,
        {
          method: 'GET',
          headers: this.getHeaders(),
        }
      );

      const locations = await this.handleResponse<ArrivyLocationReport[]>(response, 'GET', fullEndpoint);
      const duration = Date.now() - start;
      logArrivyResponse('GET', fullEndpoint, duration, locations.length);

      return locations;
    });
  }
}

// Get configuration from environment variables
function getArrivyConfig(): ArrivyConfig | null {
  const authKey = process.env.ARRIVY_AUTH_KEY;
  const authToken = process.env.ARRIVY_AUTH_TOKEN;
  const baseUrl = process.env.ARRIVY_BASE_URL || 'https://app.arrivy.com/api';
  const companyName = process.env.ARRIVY_COMPANY_NAME || '';
  const webhookSecret = process.env.ARRIVY_WEBHOOK_SECRET || '';
  const rateLimit = parseInt(process.env.ARRIVY_RATE_LIMIT || '30', 10);

  if (!authKey || !authToken) {
    console.warn('[ArrivyClient] Arrivy credentials not configured. Set ARRIVY_AUTH_KEY and ARRIVY_AUTH_TOKEN.');
    return null;
  }

  return {
    authKey,
    authToken,
    baseUrl,
    companyName,
    webhookSecret,
    rateLimit,
  };
}

// Create and export a configured client instance
const config = getArrivyConfig();
export const arrivyClient = config ? new ArrivyClient(config) : null;

// Helper function to check if Arrivy is configured
export function isArrivyConfigured(): boolean {
  return arrivyClient !== null;
}

