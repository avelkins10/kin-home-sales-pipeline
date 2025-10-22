// Notification System Types
// Matches database schema from 001-create-notifications.sql

export type NotificationPriority = 'critical' | 'normal' | 'info';
export type NotificationType = 
  | 'quickbase_note' 
  | 'internal_message' 
  | 'system_alert'
  | 'task_submitted'
  | 'task_approved'
  | 'task_revision_needed'
  | 'all_tasks_complete';
export type NotificationSource = 'quickbase' | 'internal' | 'system';

export interface Notification {
  id: number;

  // User and Project associations
  user_id: string;
  project_id: number;

  // Type and Priority
  type: NotificationType;
  priority: NotificationPriority;
  source: NotificationSource;

  // Content
  title: string;
  message: string | null;
  metadata: Record<string, any>;

  // Sender information (null for system notifications)
  sender_id: string | null;
  sender_name: string | null;
  sender_role: string | null;

  // Display properties
  icon: string;
  color: string;
  action_url: string | null;

  // Read status
  is_read: boolean;
  read_at: Date | null;

  // Timestamps
  created_at: Date;
  updated_at: Date;
}

// Input type for creating new notifications
export interface CreateNotificationInput {
  user_id: string;
  project_id: number;
  type: NotificationType;
  priority: NotificationPriority;
  source: NotificationSource;
  title: string;
  message?: string;
  metadata?: Record<string, any>;
  sender_id?: string;
  sender_name?: string;
  sender_role?: string;
  icon?: string;
  color?: string;
  action_url?: string;
}

// Metadata structures for different notification types
export interface QuickbaseNoteMetadata {
  note_id: number;
  category: string;
  quickbase_url?: string;
}

export interface InternalMessageMetadata {
  message_id?: string;
  thread_id?: string;
  is_reply?: boolean;
}

export interface SystemAlertMetadata {
  alert_type: string;
  severity?: string;
  requires_action?: boolean;
}

export interface TaskNotificationMetadata {
  task_id: number;
  task_name: string;
  task_category?: string;
  submission_id?: number;
  ops_disposition?: 'Approved' | 'Needs Revision';
  ops_feedback?: string;
  total_tasks?: number;
  approved_tasks?: number;
}

// Unread count responses
export interface UnreadCounts {
  total: number;
  by_priority: {
    critical: number;
    normal: number;
    info: number;
  };
  by_project: Record<number, number>;
  by_type?: Record<string, number>; // Optional for backward compatibility
  task_notifications?: number; // Total unread task notifications
}

// API response types
export interface NotificationsResponse {
  notifications: Notification[];
  unread_count: number;
  has_more: boolean;
}

export interface MarkAsReadResponse {
  success: boolean;
  notification_id: number;
}
