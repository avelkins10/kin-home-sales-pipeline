// Project Messages Types
// Internal messaging system for project collaboration

export interface ProjectMessage {
  id: number;
  project_id: number;
  sender_id: string;          // User email or ID
  sender_name: string;
  sender_role: string;         // closer, setter, coordinator, office_leader, admin
  message: string;
  is_system_message: boolean;  // Automated system messages
  metadata: Record<string, any>;
  is_deleted: boolean;
  deleted_at: Date | null;
  deleted_by: string | null;
  created_at: Date;
  updated_at: Date;
}

// Input type for creating new messages
export interface CreateMessageInput {
  project_id: number;
  sender_id: string;
  sender_name: string;
  sender_role: string;
  message: string;
  is_system_message?: boolean;
  metadata?: Record<string, any>;
}

// Message metadata structures
export interface MessageAttachmentMetadata {
  attachments?: Array<{
    filename: string;
    url: string;
    size: number;
    type: string;
  }>;
}

export interface MessageMentionMetadata {
  mentions?: Array<{
    user_id: string;
    user_name: string;
    user_email: string;
  }>;
}

export interface MentionedUser {
  id: string;
  name: string;
  email: string;
  role: string;
  office: string | null;
}

// Combined metadata type
export interface MessageMetadata extends MessageAttachmentMetadata, MessageMentionMetadata {}

// API response types
export interface MessagesResponse {
  messages: ProjectMessage[];
  count: number;
  has_more: boolean;
}

export interface CreateMessageResponse {
  success: boolean;
  message: ProjectMessage;
  notifications_sent: number;
}
