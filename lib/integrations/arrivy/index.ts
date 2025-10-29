// lib/integrations/arrivy/index.ts
/**
 * Arrivy Integration Module
 * Barrel export for Arrivy client, types, and service functions
 */

// Export client
export { arrivyClient, ArrivyClient, isArrivyConfigured } from './client';

// Export all types
export type {
  ArrivyConfig,
  ArrivyTask,
  ArrivyTaskCreateParams,
  ArrivyTaskUpdateParams,
  ArrivyTaskStatus,
  ArrivyTaskStatusCreateParams,
  ArrivyEntity,
  ArrivyEntityCreateParams,
  ArrivyLocationReport,
  ArrivyWebhookPayload,
  ArrivyWebhookEventType,
  ArrivyCustomerTrackerUrl,
  ArrivyApiResponse,
  ArrivyApiError,
} from './types';

// Export service functions
export {
  syncProjectToArrivy,
  syncEntityFromQuickBase,
  getCustomerTrackerUrl,
  getBusinessTrackerUrl,
  processWebhookEvent,
  getFieldTrackingData,
  mapQuickBaseToArrivyTask,
} from './service';

