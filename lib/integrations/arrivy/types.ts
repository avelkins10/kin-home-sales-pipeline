// lib/integrations/arrivy/types.ts

/**
 * Arrivy Configuration
 */
export interface ArrivyConfig {
  authKey: string;
  authToken: string;
  baseUrl: string;
  companyName: string;
  webhookSecret: string;
  rateLimit: number; // requests per minute
}

/**
 * Arrivy Task - Main task object from Arrivy API
 */
export interface ArrivyTask {
  id: number;
  url_safe_id: string;
  external_id?: string;
  customer_id?: number;
  customer_first_name?: string;
  customer_last_name?: string;
  customer_name?: string;
  customer_email?: string;
  customer_phone?: string;
  customer_mobile_number?: string;
  customer_address_line_1?: string;
  customer_address_line_2?: string;
  customer_city?: string;
  customer_state?: string;
  customer_zipcode?: string;
  customer_country?: string;
  customer_exact_location?: {
    lat: number;
    lng: number;
  };
  additional_addresses?: Array<{
    title?: string;
    address_line_1?: string;
    address_line_2?: string;
    city?: string;
    state?: string;
    zipcode?: string;
    country?: string;
    exact_location?: {
      lat: number;
      lng: number;
    };
  }>;
  start_datetime?: string; // ISO 8601 datetime
  end_datetime?: string; // ISO 8601 datetime
  start_datetime_original_iso_str?: string;
  end_datetime_original_iso_str?: string;
  enable_time_window_display?: boolean;
  time_window_start?: number;
  duration?: number; // minutes
  entity_ids?: number[];
  resource_ids?: number[];
  template_id?: number;
  template?: string;
  notifications?: {
    email?: boolean;
    sms?: boolean;
    phone_call?: boolean;
  };
  details?: string;
  title?: string;
  extra_fields?: Record<string, any>;
  group_id?: number;
  status_title?: string;
  status?: string;
  items?: Array<{
    id?: string;
    name?: string;
    quantity?: number;
    price?: number;
  }>;
  unscheduled?: boolean;
  enable_customer_task_deletion?: boolean;
  document_ids?: number[];
  group?: {
    id: number;
    name: string;
  };
  created_at?: string;
  updated_at?: string;
}

/**
 * Parameters for creating a new Arrivy task
 */
export interface ArrivyTaskCreateParams {
  customer_first_name?: string;
  customer_last_name?: string;
  customer_name?: string;
  customer_email?: string;
  customer_phone?: string;
  customer_mobile_number?: string;
  customer_address_line_1?: string;
  customer_address_line_2?: string;
  customer_city?: string;
  customer_state?: string;
  customer_zipcode?: string;
  customer_country?: string;
  customer_exact_location?: {
    lat: number;
    lng: number;
  };
  additional_addresses?: Array<{
    title?: string;
    address_line_1?: string;
    address_line_2?: string;
    city?: string;
    state?: string;
    zipcode?: string;
    country?: string;
    exact_location?: {
      lat: number;
      lng: number;
    };
  }>;
  start_datetime?: string; // ISO 8601 datetime
  end_datetime?: string; // ISO 8601 datetime
  start_datetime_original_iso_str?: string;
  end_datetime_original_iso_str?: string;
  enable_time_window_display?: boolean;
  time_window_start?: number;
  duration?: number; // minutes
  entity_ids?: number[];
  resource_ids?: number[];
  template_id?: number;
  template?: string;
  notifications?: {
    email?: boolean;
    sms?: boolean;
    phone_call?: boolean;
  };
  details?: string;
  title?: string;
  extra_fields?: Record<string, any>;
  external_id?: string;
  customer_id?: number;
  group_id?: number;
  unscheduled?: boolean;
  enable_customer_task_deletion?: boolean;
}

/**
 * Parameters for updating an existing Arrivy task
 */
export interface ArrivyTaskUpdateParams {
  customer_first_name?: string;
  customer_last_name?: string;
  customer_name?: string;
  customer_email?: string;
  customer_phone?: string;
  customer_mobile_number?: string;
  customer_address_line_1?: string;
  customer_address_line_2?: string;
  customer_city?: string;
  customer_state?: string;
  customer_zipcode?: string;
  customer_country?: string;
  customer_exact_location?: {
    lat: number;
    lng: number;
  };
  additional_addresses?: Array<{
    title?: string;
    address_line_1?: string;
    address_line_2?: string;
    city?: string;
    state?: string;
    zipcode?: string;
    country?: string;
    exact_location?: {
      lat: number;
      lng: number;
    };
  }>;
  start_datetime?: string;
  end_datetime?: string;
  enable_time_window_display?: boolean;
  time_window_start?: number;
  duration?: number;
  entity_ids?: number[];
  resource_ids?: number[];
  template_id?: number;
  template?: string;
  notifications?: {
    email?: boolean;
    sms?: boolean;
    phone_call?: boolean;
  };
  details?: string;
  title?: string;
  extra_fields?: Record<string, any>;
  external_id?: string;
  group_id?: number;
  unscheduled?: boolean;
  enable_customer_task_deletion?: boolean;
}

/**
 * Arrivy Task Status - Status report on a task
 */
export interface ArrivyTaskStatus {
  id: number;
  task_id: number;
  type: string; // ENROUTE, STARTED, COMPLETE, EXCEPTION, etc.
  title?: string;
  time: string; // ISO 8601 datetime
  time_original_iso_str?: string;
  reporter_name?: string;
  reporter_id?: number;
  source?: string; // mobile_app, web, api
  status_text?: string;
  notes?: string;
  extra_fields?: Record<string, any>;
  color?: string;
  type_title?: string;
  files?: Array<{
    file_id: number;
    file_path: string;
    filename: string;
  }>;
  visible_to_customer?: boolean;
  created_at?: string;
  updated_at?: string;
}

/**
 * Parameters for creating a task status
 */
export interface ArrivyTaskStatusCreateParams {
  type: string; // ENROUTE, STARTED, COMPLETE, EXCEPTION, etc.
  time?: string; // ISO 8601 datetime, defaults to now
  notes?: string;
  source?: string; // mobile_app, web, api
  extra_fields?: Record<string, any>;
  visible_to_customer?: boolean;
  files?: Array<{
    filename: string;
    file_data: string; // base64 encoded
  }>;
}

/**
 * Arrivy Entity - Crew member or service technician
 */
export interface ArrivyEntity {
  id: number;
  owner?: number;
  name?: string;
  type?: string; // CREW, TECHNICIAN, DRIVER, etc.
  details?: string;
  email?: string;
  phone?: string;
  mobile_number?: string;
  extra_fields?: Record<string, any>;
  image_id?: number;
  image_path?: string;
  group_ids?: number[];
  permission?: string;
  can_create_tasks?: boolean;
  can_turnoff_location?: boolean;
  can_add_exception?: boolean;
  can_delete_task?: boolean;
  created_at?: string;
  updated_at?: string;
}

/**
 * Parameters for creating an entity
 */
export interface ArrivyEntityCreateParams {
  name: string;
  type?: string;
  details?: string;
  email?: string;
  phone?: string;
  mobile_number?: string;
  extra_fields?: Record<string, any>;
  group_ids?: number[];
  permission?: string;
  can_create_tasks?: boolean;
  can_turnoff_location?: boolean;
  can_add_exception?: boolean;
  can_delete_task?: boolean;
}

/**
 * Arrivy Location Report - GPS location update from entity
 */
export interface ArrivyLocationReport {
  id: number;
  entity_id: number;
  entity_name?: string;
  lat: number;
  lng: number;
  time: string; // ISO 8601 datetime
  address?: string;
  city?: string;
  state?: string;
  zipcode?: string;
  country?: string;
  street?: string;
  accuracy?: number;
  heading?: number;
  speed?: number;
  altitude?: number;
  extra_fields?: Record<string, any>;
}

/**
 * Arrivy Webhook Payload - Webhook event from Arrivy
 */
export interface ArrivyWebhookPayload {
  TITLE: string;
  MESSAGE: string;
  EVENT_ID: number;
  EVENT_TYPE: ArrivyWebhookEventType;
  EVENT_SUB_TYPE?: string | null;
  EVENT_TIME: string; // ISO 8601 datetime
  REPORTER_ID: number;
  REPORTER_NAME: string;
  OBJECT_ID: number; // Task ID
  OBJECT_TYPE: string; // Always "TASK"
  OBJECT_DATE?: string;
  OBJECT_GROUP_ID?: number;
  OBJECT_EXTERNAL_ID?: string;
  OBJECT_CUSTOMER_ID?: number;
  IS_TRANSIENT_STATUS?: boolean;
  EXCEPTION?: {
    type?: string;
    notes?: string;
    reason?: string;
  } | null;
  OBJECT_FIELDS?: {
    END_DATETIME?: string;
    DURATION?: number;
    LINKED_INTERNAL_ID?: number;
    LINKED_EXTERNAL_ID?: string;
    RESPONSE_FROM_CUSTOMER?: boolean;
    HAS_ATTACHMENT?: boolean;
    ITEMS?: any;
    RATING_TYPE?: string;
  };
  EXTRA_FIELDS?: Record<string, any>;
}

/**
 * Arrivy Webhook Event Types
 */
export type ArrivyWebhookEventType =
  | 'TASK_CREATED'
  | 'TASK_DELETED'
  | 'TASK_STATUS'
  | 'CREW_ASSIGNED'
  | 'CREW_REMOVED'
  | 'EQUIPMENT_ASSIGNED'
  | 'EQUIPMENT_REMOVED'
  | 'TASK_RATING'
  | 'TASK_RESCHEDULED'
  | 'TASK_GROUP_CHANGED'
  | 'ARRIVING'
  | 'LATE'
  | 'NOSHOW';

/**
 * Customer Tracker URL Helper
 */
export interface ArrivyCustomerTrackerUrl {
  url: string;
  shortUrl?: string;
}

/**
 * Arrivy API Response - Standard response wrapper
 */
export interface ArrivyApiResponse<T = any> {
  data?: T;
  message?: string;
  status?: number;
  error?: string;
}

/**
 * Arrivy API Error
 */
export interface ArrivyApiError {
  message: string;
  status: number;
  code?: string;
  details?: any;
}

