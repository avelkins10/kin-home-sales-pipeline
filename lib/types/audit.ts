/**
 * Audit logging types for compliance and security monitoring
 * 
 * These types support the AuditLogsTab component and audit logs API contracts
 * per SETTINGS-SPEC.md section 8 (lines 2491-2508).
 */

export type AuditAction = 'login' | 'logout' | 'create' | 'update' | 'delete' | 'export';

export interface AuditLog {
  /** Audit log ID (UUID from database) */
  id: string;
  /** ISO timestamp of the action */
  timestamp: string;
  /** ID of user who performed the action */
  userId: string;
  /** Full name of user (joined from users table) */
  userName: string;
  /** Type of action performed */
  action: AuditAction;
  /** Resource type affected (user, office, system_settings, project, etc.) */
  resource: string;
  /** ID of the specific resource affected */
  resourceId: string;
  /** Object mapping field names to old/new values for update actions */
  changes: Record<string, { old: any; new: any }>;
  /** IP address of the request */
  ipAddress: string;
  /** Browser user agent string */
  userAgent: string;
}

export interface AuditLogFilters {
  /** Start date for filtering */
  from: Date;
  /** End date for filtering */
  to: Date;
  /** Filter by action type (optional) */
  action?: string;
  /** Search by user, resource, or IP (optional) */
  search?: string;
  /** Page number for pagination (optional) */
  page?: number;
  /** Results per page (optional) */
  limit?: number;
}