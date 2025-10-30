/**
 * RepCard API Client
 *
 * Provides typed interface to RepCard API for user management and customer data
 * API Documentation: https://www.repcard.com/api-docs
 *
 * Authentication: x-api-key header
 * Rate Limit: 100 requests per period
 */

import {
  RepCardStatusLogQueryParams,
  RepCardStatusLogResponse,
  RepCardAppointmentQueryParams,
  RepCardAppointmentResponse,
  RepCardCustomerAttachmentQueryParams,
  RepCardAppointmentAttachmentQueryParams,
  RepCardAttachmentResponse,
  RepCardCustomerAttachment,
  RepCardAppointmentAttachment,
  RepCardCustomerNote,
  RepCardCustomerStatus,
  RepCardCalendar,
  RepCardCustomField,
  RepCardLeaderboard,
  RepCardPaginatedResponse,
  RepCardApiResponse,
} from './types';

export interface RepCardUser {
  id: number;
  firstName: string;
  lastName: string;
  email: string;
  phone?: string;
  companyId: number;
  officeId?: number;
  role?: string;
  status: number; // 1 = active, 0 = inactive
  profileImage?: string;
  createdAt: string;
  updatedAt: string;
}

export interface RepCardUserMinimal {
  id: number;
  firstName: string;
  lastName: string;
  email: string;
  companyId: number;
  officeId?: number;
}

export interface RepCardOffice {
  id: number;
  name: string;
  companyId: number;
  address?: string;
  city?: string;
  state?: string;
  zipCode?: string;
  createdAt: string;
  updatedAt: string;
}

export interface RepCardCustomer {
  id: number;
  firstName: string;
  lastName: string;
  email?: string;
  phone?: string;
  address?: string;
  city?: string;
  state?: string;
  zipCode?: string;
  assignedUserId?: number;
  statusId?: number;
  companyId: number;
  createdAt: string;
  updatedAt: string;
  customFields?: {
    systemSizeKW?: number;
    systemCost?: number;
    financier?: string;
    offset?: number;
  };
}

export interface RepCardApiResponse<T> {
  status: number;
  statusCode: number;
  message: string;
  result: T;
}

export interface RepCardPaginatedResponse<T> {
  status: number;
  statusCode: number;
  message: string;
  result: {
    data: T[];
    total: number;
    perPage: number;
    currentPage: number;
    lastPage: number;
  };
}

export class RepCardClient {
  private baseUrl: string;
  private apiKey: string;

  constructor(baseUrl?: string, apiKey?: string) {
    this.baseUrl = baseUrl || process.env.REPCARD_API_URL || 'https://api.repcard.com';
    this.apiKey = apiKey || process.env.REPCARD_API_KEY || '';

    // Don't throw on construction - let methods fail gracefully if needed
    // This allows the module to be imported during build without requiring the API key
  }

  private checkApiKey() {
    if (!this.apiKey) {
      throw new Error('RepCard API key is required. Set REPCARD_API_KEY environment variable.');
    }
  }

  private async request<T>(
    endpoint: string,
    options: RequestInit = {},
    retryCount = 0
  ): Promise<RepCardApiResponse<T>> {
    this.checkApiKey(); // Check API key before making request

    const url = `${this.baseUrl}${endpoint}`;

    const headers = {
      'x-api-key': this.apiKey,
      'Content-Type': 'application/json',
      ...options.headers,
    };

    const response = await fetch(url, {
      ...options,
      headers,
    });

    // Handle rate limiting with exponential backoff
    if (response.status === 429 && retryCount < 3) {
      const delay = Math.pow(2, retryCount) * 1000; // 1s, 2s, 4s
      console.log(`[RepCard] Rate limited, retrying in ${delay}ms (attempt ${retryCount + 1}/3)`);
      await new Promise(resolve => setTimeout(resolve, delay));
      return this.request<T>(endpoint, options, retryCount + 1);
    }

    if (!response.ok) {
      const errorText = await response.text();
      throw new Error(
        `RepCard API error: ${response.status} ${response.statusText} - ${errorText}`
      );
    }

    return response.json();
  }

  /**
   * Get all users (minimal data)
   * @param params Query parameters for filtering
   */
  async getUsersMinimal(params?: {
    companyId?: number;
    perPage?: number;
    page?: number;
    search?: string;
    firstVerifiedDoorKnock?: 0 | 1;
    firstAppointment?: 0 | 1;
  }): Promise<RepCardPaginatedResponse<RepCardUserMinimal>> {
    const searchParams = new URLSearchParams();
    if (params) {
      Object.entries(params).forEach(([key, value]) => {
        if (value !== undefined) {
          searchParams.append(key, value.toString());
        }
      });
    }

    const endpoint = `/users/minimal${searchParams.toString() ? `?${searchParams.toString()}` : ''}`;
    return this.request(endpoint);
  }

  /**
   * Get detailed user information by ID
   * @param userId User ID
   * @param params Optional query parameters
   */
  async getUserDetails(
    userId: number,
    params?: {
      firstVerifiedDoorKnock?: 0 | 1;
      firstAppointment?: 0 | 1;
    }
  ): Promise<RepCardApiResponse<RepCardUser>> {
    const searchParams = new URLSearchParams();
    if (params) {
      Object.entries(params).forEach(([key, value]) => {
        if (value !== undefined) {
          searchParams.append(key, value.toString());
        }
      });
    }

    const endpoint = `/users/${userId}/details${searchParams.toString() ? `?${searchParams.toString()}` : ''}`;
    return this.request(endpoint);
  }

  /**
   * Get event users (users with specific event data)
   * @param userId User ID
   */
  async getEventUsers(userId: number): Promise<RepCardApiResponse<any>> {
    return this.request(`/event-users/${userId}`);
  }

  /**
   * Activate or deactivate a user
   * @param userId User ID
   * @param status 1 = activate, 0 = deactivate
   */
  async setUserStatus(
    userId: number,
    status: 0 | 1
  ): Promise<RepCardApiResponse<any>> {
    return this.request(`/users/${userId}/activate-deactivate`, {
      method: 'PUT',
      body: JSON.stringify({ status }),
    });
  }

  /**
   * Get all offices
   * @param companyId Optional company ID filter
   */
  async getOffices(companyId?: number): Promise<RepCardApiResponse<RepCardOffice[]>> {
    const endpoint = companyId ? `/offices?company_id=${companyId}` : '/offices';
    return this.request(endpoint);
  }

  /**
   * Get customers
   * @param params Query parameters for filtering
   */
  async getCustomers(params?: RepCardCustomerQueryParams): Promise<RepCardPaginatedResponse<RepCardCustomer>> {
    const searchParams = new URLSearchParams();
    if (params) {
      Object.entries(params).forEach(([key, value]) => {
        if (value !== undefined) {
          // Map parameter names to RepCard API format
          let apiKey: string;
          if (key === 'startDate') {
            apiKey = 'last_created_from';
          } else if (key === 'endDate') {
            apiKey = 'last_created_to';
          } else {
            // Convert camelCase to snake_case for other params
            apiKey = key.replace(/([A-Z])/g, '_$1').toLowerCase();
          }
          searchParams.append(apiKey, value.toString());
        }
      });
    }

    const endpoint = `/customers${searchParams.toString() ? `?${searchParams.toString()}` : ''}`;
    return this.request(endpoint);
  }

  /**
   * Get customer by ID
   * @param customerId Customer ID
   */
  async getCustomerById(customerId: number): Promise<RepCardApiResponse<RepCardCustomer>> {
    return this.request(`/customers/${customerId}`);
  }

  /**
   * Get leaderboard data
   * @param params Query parameters for filtering
   */
  async getLeaderboard(params?: {
    companyId?: number;
    officeId?: number;
    startDate?: string; // YYYY-MM-DD
    endDate?: string; // YYYY-MM-DD
    metric?: 'sales' | 'revenue' | 'appointments';
  }): Promise<RepCardApiResponse<any>> {
    const searchParams = new URLSearchParams();
    if (params) {
      Object.entries(params).forEach(([key, value]) => {
        if (value !== undefined) {
          searchParams.append(key, value.toString());
        }
      });
    }

    const endpoint = `/leaderboard${searchParams.toString() ? `?${searchParams.toString()}` : ''}`;
    return this.request(endpoint);
  }

  /**
   * Get customer status logs with filtering
   * @param params Query parameters for filtering status logs
   */
  async getCustomerStatusLogs(params?: RepCardStatusLogQueryParams): Promise<RepCardStatusLogResponse> {
    const searchParams = new URLSearchParams();
    if (params) {
      Object.entries(params).forEach(([key, value]) => {
        if (value !== undefined) {
          // Convert camelCase to snake_case
          const apiKey = key.replace(/([A-Z])/g, '_$1').toLowerCase();
          searchParams.append(apiKey, value.toString());
        }
      });
    }

    const endpoint = `/customers/status-logs${searchParams.toString() ? `?${searchParams.toString()}` : ''}`;
    return this.request<RepCardStatusLogResponse['result']>(endpoint);
  }

  /**
   * Get appointments with comprehensive filtering
   * @param params Query parameters for filtering appointments
   */
  async getAppointments(params?: RepCardAppointmentQueryParams): Promise<RepCardAppointmentResponse> {
    const searchParams = new URLSearchParams();
    if (params) {
      Object.entries(params).forEach(([key, value]) => {
        if (value !== undefined) {
          // Convert camelCase to snake_case
          const apiKey = key.replace(/([A-Z])/g, '_$1').toLowerCase();
          searchParams.append(apiKey, value.toString());
        }
      });
    }

    const endpoint = `/appointments${searchParams.toString() ? `?${searchParams.toString()}` : ''}`;
    return this.request<RepCardAppointmentResponse['result']>(endpoint);
  }

  /**
   * Get customer attachments (e.g., power bills) with filtering
   * @param params Query parameters for filtering customer attachments
   */
  async getCustomerAttachments(params?: RepCardCustomerAttachmentQueryParams): Promise<RepCardAttachmentResponse<RepCardCustomerAttachment>> {
    const searchParams = new URLSearchParams();
    if (params) {
      Object.entries(params).forEach(([key, value]) => {
        if (value !== undefined) {
          // Convert camelCase to snake_case
          const apiKey = key.replace(/([A-Z])/g, '_$1').toLowerCase();
          searchParams.append(apiKey, value.toString());
        }
      });
    }

    const endpoint = `/customers/attachments${searchParams.toString() ? `?${searchParams.toString()}` : ''}`;
    return this.request<RepCardAttachmentResponse<RepCardCustomerAttachment>['result']>(endpoint);
  }

  /**
   * Get appointment-specific attachments with filtering
   * @param params Query parameters for filtering appointment attachments
   */
  async getAppointmentAttachments(params?: RepCardAppointmentAttachmentQueryParams): Promise<RepCardAttachmentResponse<RepCardAppointmentAttachment>> {
    const searchParams = new URLSearchParams();
    if (params) {
      Object.entries(params).forEach(([key, value]) => {
        if (value !== undefined) {
          // Convert camelCase to snake_case
          const apiKey = key.replace(/([A-Z])/g, '_$1').toLowerCase();
          searchParams.append(apiKey, value.toString());
        }
      });
    }

    const endpoint = `/appointments/attachments${searchParams.toString() ? `?${searchParams.toString()}` : ''}`;
    return this.request<RepCardAttachmentResponse<RepCardAppointmentAttachment>['result']>(endpoint);
  }

  /**
   * Get customer notes with filtering
   * @param params Query parameters for filtering notes
   */
  async getCustomerNotes(params?: {
    customerId?: number;
    userId?: number;
    page?: number;
    perPage?: number;
  }): Promise<RepCardPaginatedResponse<RepCardCustomerNote>> {
    const searchParams = new URLSearchParams();
    if (params) {
      Object.entries(params).forEach(([key, value]) => {
        if (value !== undefined) {
          // Convert camelCase to snake_case
          const apiKey = key.replace(/([A-Z])/g, '_$1').toLowerCase();
          searchParams.append(apiKey, value.toString());
        }
      });
    }

    const endpoint = `/customers/notes${searchParams.toString() ? `?${searchParams.toString()}` : ''}`;
    return this.request(endpoint);
  }

  /**
   * Get customer status definitions
   */
  async getCustomerStatuses(): Promise<RepCardApiResponse<RepCardCustomerStatus[]>> {
    return this.request('/customers/status');
  }

  /**
   * Get calendar lists
   * @param params Query parameters
   */
  async getCalendars(params?: {
    status?: 'active' | 'inactive';
  }): Promise<RepCardApiResponse<RepCardCalendar[]>> {
    const searchParams = new URLSearchParams();
    if (params?.status) {
      searchParams.append('status', params.status);
    }

    const endpoint = `/calendar/lists${searchParams.toString() ? `?${searchParams.toString()}` : ''}`;
    return this.request(endpoint);
  }

  /**
   * Get calendar details with setters, closers, dispatchers
   * @param calendarId Calendar ID
   */
  async getCalendarDetails(calendarId: number): Promise<RepCardApiResponse<RepCardCalendar>> {
    return this.request(`/calendar/${calendarId}?with=setters,closers,dispatchers`);
  }

  /**
   * Get custom fields for a specific entity type
   * @param entityType Entity type: 'lead', 'customer', 'recruit', 'other'
   */
  async getCustomFields(entityType: 'lead' | 'customer' | 'recruit' | 'other'): Promise<RepCardApiResponse<RepCardCustomField[]>> {
    return this.request(`/custom-fields/${entityType}`);
  }

  /**
   * Get leaderboards with date filtering
   * @param params Query parameters for filtering leaderboards
   */
  async getLeaderboards(params?: {
    fromDate?: string; // YYYY-MM-DD
    toDate?: string; // YYYY-MM-DD
  }): Promise<RepCardApiResponse<RepCardLeaderboard[]>> {
    const searchParams = new URLSearchParams();
    if (params?.fromDate) {
      searchParams.append('from_date', params.fromDate);
    }
    if (params?.toDate) {
      searchParams.append('to_date', params.toDate);
    }

    const endpoint = `/leaderboards${searchParams.toString() ? `?${searchParams.toString()}` : ''}`;
    return this.request(endpoint);
  }
}

// Export singleton instance
export const repcardClient = new RepCardClient();
