/**
 * PC (Project Coordinator) Operations-specific type definitions
 * These types define the data structures for the PC Operations dashboard
 */

import { SmsTemplateType } from '@/lib/integrations/twilio/templates';

// PC Dashboard Metrics
export interface PCDashboardMetrics {
  totalProjects: number;
  pendingOutreach: number;
  unresponsiveCustomers: number;
  activeEscalations: number;
  todaysInstalls: number;
  slaCompliance: number;
}

// PC Priority Queue Item
export interface PCPriorityQueueItem {
  recordId: number;
  projectId: string;
  customerName: string;
  customerPhone: string;
  currentStage: string;
  daysInStage: number;
  priorityScore: number;
  priorityReason: string;
  daysSinceContact: number;
  contactAttempts: number;
  dueMilestones: number;
  isUnresponsive: boolean;
  escalationLevel: number;
  preferredContactMethod: string;
  lastContactDate: string | null;
  coordinatorEmail: string;
  salesRepName: string;
  salesRepEmail: string;
}

// PC Project Pipeline Stage
export interface PCProjectPipelineStage {
  stageName: string;
  projectCount: number;
  projects: Array<{
    projectId: string;
    customerName: string;
    daysInStage: number;
  }>;
}

// PC Activity Feed Item
export interface PCActivityFeedItem {
  recordId: number;
  date: string;
  noteBy: string;
  projectId: string;
  customerName: string;
  note: string;
  isNemBlocker: boolean;
}

// PC Dashboard Data
export interface PCDashboardData {
  metrics: PCDashboardMetrics;
  priorityQueue: PCPriorityQueueItem[];
  pipeline: PCProjectPipelineStage[];
}

// Helper Types
export type PCContactPhase = 'intake' | 'install' | 'nem' | 'pto';

export type PCOutreachStatus = 
  | 'Complete' 
  | 'No Answer Left Message' 
  | 'Complete - No Answer'
  | 'Pending'
  | 'Failed'
  | 'Scheduled'
  | 'Cancelled'
  | 'Unknown';

export type PCSalesAidStatus = 
  | 'Waiting for Rep' 
  | 'Working With Rep' 
  | 'Resolved by Rep' 
  | 'Escalated to Sales Aid' 
  | 'Task Completed' 
  | 'Task Cancelled';

// PC Notification Preferences
export interface PCNotificationPreferences {
  email_enabled: boolean;
  email_frequency: 'immediate' | 'daily_digest' | 'never';
  notification_types: string[];
  quiet_hours_start?: string; // HH:MM format
  quiet_hours_end?: string; // HH:MM format
}

// PC Milestone Check
export interface PCMilestoneCheck {
  project_id: string;
  record_id: number;
  customer_name: string;
  coordinator_email: string;
  milestone_type: 'survey' | 'install' | 'nem' | 'pto' | 'unresponsive';
  scheduled_date: Date | null;
  completion_date: Date | null;
  days_overdue: number;
  should_notify: boolean;
  notification_priority: 'critical' | 'normal' | 'info';
}

// PC Notification Summary
export interface PCNotificationSummary {
  total_notifications: number;
  by_type: Record<string, number>;
  by_priority: Record<string, number>;
  created_count: number;
  skipped_count: number;
  error_count: number;
}

// PC Outreach Management Types

// PC Outreach Record
export interface PCOutreachRecord {
  recordId: number; // field 3
  dateCreated: string; // field 1
  relatedProject: number; // field 10
  projectId: string; // from Projects lookup
  customerName: string; // field 145 from Projects
  customerPhone: string; // from Projects
  outreachCompletedDate: string | null; // field 18
  outreachStatus: PCOutreachStatus; // field 43
  numAttempts: number; // field 44
  reportingDueDate: string; // field 54
  nextOutreachDueDate: string | null; // field 86
  note: string; // field 8
  attemptNote: string; // field 37
  projectCoordinator: string; // field 17 lookup
  projectStage: string; // field 55 lookup
  projectStatus: string; // field 56 lookup
  preferredContactMethod: string; // field 2404 from Projects
  lenderName: string; // field 344 from Projects
  salesRepName: string; // field 517 from Projects
  salesRepEmail: string; // field 356 from Projects
  daysOverdue: number; // calculated
  daysInStage?: number; // calculated - days in current stage
  isUnresponsive: boolean; // field 1909 from Projects
}

// PC Outreach Filters
export interface PCOutreachFilters {
  tab: 'initial' | 'followups' | 'welcome';
  status: PCOutreachStatus | 'all';
  daysOverdue: 'all' | '1-3' | '4-7' | '8+';
  lender: string | 'all';
  salesRep: string | 'all';
  search: string;
}

// PC Outreach Tab Data
export interface PCOutreachTabData {
  initial: PCOutreachRecord[];
  followups: PCOutreachRecord[];
  welcome: PCOutreachRecord[];
  counts: {
    initial: number;
    followups: number;
    welcome: number;
  };
}

// PC Outreach Bulk Action Types
export type PCOutreachBulkAction = 
  | 'mark_contacted' 
  | 'assign_to_rep' 
  | 'bulk_sms' 
  | 'schedule_checkin';

export interface PCOutreachBulkActionPayload {
  action: PCOutreachBulkAction;
  recordIds: number[];
  data?: Record<string, any>; // for action-specific data
}

export interface PCOutreachBulkActionResult {
  success: boolean;
  processed: number;
  failed: number;
  errors: Array<{
    recordId: number;
    error: string;
  }>;
  message: string;
}

// =============================================================================
// TWILIO INTEGRATION TYPES
// =============================================================================

export interface TwilioSmsParams {
  to: string; // E.164 format phone number
  body: string; // SMS message content, max 1600 chars
  from?: string; // Optional, uses config default if not provided
  statusCallback?: string; // Optional webhook URL for delivery status
  metadata?: Record<string, any>; // Optional data to include in callbacks
}

export interface TwilioCallParams {
  to: string; // E.164 format phone number
  from?: string; // Optional, uses config default
  url?: string; // Optional TwiML URL, uses default if not provided
  statusCallback?: string; // Optional webhook URL for call status
  statusCallbackEvent?: Array<'initiated' | 'ringing' | 'answered' | 'completed'>;
  record?: boolean; // Optional, uses config default
  recordingStatusCallback?: string; // Optional webhook for recording status
  metadata?: Record<string, any>;
}

export interface TwilioSmsResult {
  success: boolean;
  messageSid: string | null; // Twilio message SID
  status: string; // queued, sent, delivered, failed, etc.
  to: string;
  body: string;
  error?: string;
  errorCode?: number; // Twilio error code
}

export interface TwilioCallResult {
  success: boolean;
  callSid: string | null; // Twilio call SID
  status: string; // queued, ringing, in-progress, completed, etc.
  to: string;
  duration?: number; // Call duration in seconds, if completed
  error?: string;
  errorCode?: number;
}

export interface TwilioWebhookPayload {
  MessageSid?: string; // For SMS webhooks
  CallSid?: string; // For voice webhooks
  MessageStatus?: string; // SMS delivery status
  CallStatus?: string; // Call status
  From: string;
  To: string;
  Body?: string; // SMS body
  ErrorCode?: string;
  ErrorMessage?: string;
}

export interface PCCommunicationLog {
  projectId: string;
  recordId: number;
  communicationType: 'sms' | 'call' | 'email';
  direction: 'outbound' | 'inbound';
  status: string;
  twilioSid: string | null;
  phoneNumber: string;
  content: string | null;
  duration?: number; // For calls
  coordinatorEmail: string;
  timestamp: string; // ISO date
}

// =============================================================================
// COMMUNICATION HUB TYPES
// =============================================================================

// PC Sales Aid Request (Inbound Rep Queue)
export interface PCSalesAidRequest {
  recordId: number; // field 3
  dateCreated: string; // field 1
  relatedProject: number; // field 85
  projectId: string; // from Projects lookup
  customerName: string; // from Projects lookup
  salesRepName: string; // from Projects lookup
  salesRepEmail: string; // from Projects lookup
  salesAidStatus: PCSalesAidStatus; // field 103
  salesAidReason: string; // field 84
  escalateToSalesAid: boolean; // field 93
  escalatedDateTime: string | null; // field 108
  assignedEscalationRep: string; // field 109
  rep72HourDeadline: string; // field 91
  completedDate: string | null; // field 119
  urgency: 'critical' | 'high' | 'normal'; // calculated from deadline
  timeWaiting: number; // days since creation
  messagePreview: string; // truncated reason
}

// PC Conversation Item (Communication History)
export interface PCConversationItem {
  recordId: number;
  date: string;
  noteBy: string;
  projectId: string;
  customerName: string;
  customerPhone: string;
  communicationType: 'sms' | 'call' | 'email' | 'note';
  direction: 'outbound' | 'inbound';
  content: string;
  twilioSid: string | null;
  duration: number | null; // for calls
  status: string | null; // for SMS/calls
  isNemBlocker: boolean;
  needsResponse: boolean;
  projectStage: string;
}

// PC Conversation Filters
export interface PCConversationFilters {
  tab: 'recent' | 'needs_response' | 'scheduled';
  projectStage: string | 'all';
  communicationType: 'all' | 'sms' | 'call' | 'email' | 'note';
  dateRange: '7days' | '30days' | '90days' | 'all';
  search: string;
}

// PC Bulk Messaging Recipient
export interface PCBulkMessagingRecipient {
  projectId: string;
  recordId: number;
  customerName: string;
  customerPhone: string;
  projectStage: string;
  lenderName: string;
  salesRepName: string;
  coordinatorEmail: string;
}

// PC Bulk Messaging Payload
export interface PCBulkMessagingPayload {
  templateType: SmsTemplateType; // SmsTemplateType from templates.ts
  recipients: PCBulkMessagingRecipient[];
  variables: Record<string, string>; // template variables
  scheduledTime: string | null; // for future scheduling
}

// PC Bulk Messaging Result
export interface PCBulkMessagingResult {
  success: boolean;
  sent: number;
  failed: number;
  errors: Array<{ projectId: string; error: string }>;
  message: string;
}

// PC Call Queue Item
export interface PCCallQueueItem {
  projectId: string;
  recordId: number;
  customerName: string;
  customerPhone: string;
  projectStage: string;
  priority: number;
  callStatus: 'queued' | 'calling' | 'completed' | 'failed' | null;
  callSid: string | null;
  queuedAt: string;
}

// PC Inbound Queue Data
export interface PCInboundQueueData {
  requests: PCSalesAidRequest[];
  count: number;
  criticalCount: number;
  highCount: number;
}

// PC Communications Data
export interface PCCommunicationsData {
  conversations: PCConversationItem[];
  count: number;
  needsResponseCount: number;
}

// =============================================================================
// PROJECT DETAIL MODAL TYPES
// =============================================================================

// PC Project Detail (comprehensive project data for modal)
export interface PCProjectDetail {
  project: any; // QuickbaseProject (full project data from getProjectById)
  communications: PCConversationItem[]; // communication history
  documents: PCProjectDocument[]; // document metadata
  teamMembers: PCTeamMember[]; // team member details
}

// PC Team Member (team member details for project)
export interface PCTeamMember {
  role: 'closer' | 'setter' | 'coordinator' | 'install_crew';
  name: string;
  email: string;
  phone: string;
  userId: string;
}

// PC Project Document (document metadata for project)
export interface PCProjectDocument {
  recordId: number;
  documentType: 'contract' | 'permit' | 'photo' | 'other';
  fileName: string;
  uploadDate: string;
  uploadedBy: string;
  fileUrl: string | null;
  fileSize: number | null;
}

// PC Project Action (quick action types)
export type PCProjectAction = 
  | 'call' 
  | 'sms' 
  | 'email' 
  | 'add_note' 
  | 'assign_task' 
  | 'escalate';

// PC Project Action Payload (action request data)
export interface PCProjectActionPayload {
  action: PCProjectAction;
  projectId: string;
  recordId: number;
  data?: Record<string, any>; // action-specific data
}

// PC Project Action Result (action response data)
export interface PCProjectActionResult {
  success: boolean;
  result: TwilioCallResult | TwilioSmsResult;
  message: string;
}

// =============================================================================
// PC ↔ REP COLLABORATION TYPES
// =============================================================================

// PC Task Types
export type PCTaskType = 
  | 'callback_customer'     // PC requests rep to call customer back
  | 'collect_documents'      // PC needs rep to collect missing documents
  | 'clarify_pricing'        // PC needs rep to clarify pricing details with customer
  | 'handle_objection'       // PC needs rep to address customer objection
  | 'schedule_site_visit'    // PC needs rep to schedule site visit
  | 'resolve_hoa_issue';     // PC needs rep to resolve HOA-related issue

// PC Task (task assigned by PC to sales rep)
export interface PCTask {
  recordId: number; // TASK_FIELDS.RECORD_ID
  dateCreated: string; // TASK_FIELDS.DATE_CREATED
  dateModified: string; // TASK_FIELDS.DATE_MODIFIED
  taskGroup: number; // TASK_FIELDS.TASK_GROUP
  status: 'Not Started' | 'In Progress' | 'Completed' | 'Blocked'; // TASK_FIELDS.STATUS
  name: string; // TASK_FIELDS.NAME
  description: string; // TASK_FIELDS.DESCRIPTION
  taskType: PCTaskType; // derived from TASK_FIELDS.TASK_CATEGORY
  assignedTo: string; // sales rep email
  assignedBy: string; // PC email
  projectId: string;
  customerName: string;
  dueDate: string | null;
  priority: 'high' | 'normal' | 'low';
  submissions: PCTaskSubmission[]; // from Task Submissions table
}

// PC Task Submission (rep's response to task)
export interface PCTaskSubmission {
  recordId: number;
  dateCreated: string;
  relatedTask: number;
  submissionStatus: string;
  submissionNote: string;
  fileAttachments: string[]; // URLs from FILE_ATTACHMENT_1/2/3
  submittedBy: string;
}

// PC Message (message in PC ↔ Rep conversation)
export interface PCMessage {
  recordId: number; // INSTALL_COMMUNICATION_FIELDS.RECORD_ID
  date: string; // INSTALL_COMMUNICATION_FIELDS.DATE
  sentBy: string; // INSTALL_COMMUNICATION_FIELDS.NOTE_BY
  sentByRole: 'pc' | 'rep';
  projectId: string; // INSTALL_COMMUNICATION_FIELDS.RELATED_PROJECT
  content: string; // INSTALL_COMMUNICATION_FIELDS.COMMUNICATION_NOTE
  relatedTaskId: number | null; // extracted from content metadata
  mentions: string[]; // extracted @username mentions
  isRead: boolean; // calculated from metadata
  parentMessageId: number | null; // for threading
}

// PC Message Thread (conversation thread for project/task)
export interface PCMessageThread {
  projectId: string;
  taskId: number | null; // if thread is task-specific
  messages: PCMessage[];
  participants: Array<{ name: string; email: string; role: 'pc' | 'rep' }>;
  unreadCount: number;
}

// PC Task Assignment Payload (data for creating new task)
export interface PCTaskAssignmentPayload {
  projectId: string;
  recordId: number; // QuickBase project record ID
  taskType: PCTaskType;
  assignedTo: string; // rep email
  name: string;
  description: string;
  dueDate: string | null;
  priority: 'high' | 'normal' | 'low';
}

// PC Task Filters (filtering options for task list)
export interface PCTaskFilters {
  status: 'all' | 'active' | 'completed' | 'blocked';
  taskType: PCTaskType | 'all';
  assignedTo: string | 'all';
  priority: 'all' | 'high' | 'normal' | 'low';
}

// Escalation Management Types

// Escalation category types
export type PCEscalationCategory = 
  | 'mmu_required'      // Main Panel Upgrade required
  | 'rep_promises'       // Rep needs to follow up on promises/commitments
  | 'hoa_issues'         // HOA approval or compliance issues
  | 'financing_issues'   // Credit, loan, or financing problems
  | 'customer_complaints'; // Customer dissatisfaction or complaints

// Escalation interface (extends PCSalesAidRequest with additional fields)
export interface PCEscalation extends PCSalesAidRequest {
  category: PCEscalationCategory; // Derived from salesAidReason
  gracePeriodEnd: string; // Same as rep72HourDeadline
  gracePeriodExtended: boolean; // Calculated if deadline was modified
  resolutionNote: string | null; // From Install Communications
  history: PCEscalationHistoryItem[]; // Timeline of status changes
}

// Escalation history item
export interface PCEscalationHistoryItem {
  timestamp: string;
  action: 'created' | 'assigned' | 'grace_extended' | 'status_changed' | 'resolved';
  performedBy: string; // User email
  details: string; // Description of action
  oldValue: string | null;
  newValue: string | null;
}

// Escalation filters
export interface PCEscalationFilters {
  category: PCEscalationCategory | 'all';
  status: PCSalesAidStatus | 'all';
  urgency: 'critical' | 'high' | 'normal' | 'all';
  assignedTo: string | 'all';
  search: string;
}

// Escalation action types
export type PCEscalationAction = 
  | 'assign'           // Assign to sales aid rep
  | 'extend_grace'     // Extend grace period deadline
  | 'resolve'          // Mark escalation as resolved
  | 'notify_customer'; // Send notification to customer

// Escalation action payload
export interface PCEscalationActionPayload {
  action: PCEscalationAction;
  escalationId: number;
  data?: Record<string, any>; // Action-specific data
}

// Create escalation payload
export interface PCCreateEscalationPayload {
  projectId: string;
  recordId: number;
  reason: string; // Maps to SALES_AID_REASON
  category: PCEscalationCategory;
  description: string;
  priority: 'high' | 'normal';
  assignedTo?: string; // Optional sales rep email for assignment
}

// Escalation statistics
export interface PCEscalationStats {
  total: number;
  byCategory: Record<PCEscalationCategory, number>;
  byUrgency: Record<'critical' | 'high' | 'normal', number>;
  avgResolutionTime: number; // Hours
}

// PC Analytics Types
export interface PCPersonalMetrics {
  dailyOutreach: number;
  weeklyOutreach: number;
  monthlyOutreach: number;
  responseRate: number;
  avgTimeToContact: number;
  escalationRate: number;
  slaCompliance: number;
  totalProjects: number;
  completedOutreach: number;
  pendingOutreach: number;
  unresponsiveCount: number;
  activeEscalations: number;
}

export interface PCOutreachTrendData {
  date: string; // YYYY-MM-DD format
  outreachCount: number;
  successfulCount: number;
  responseRate: number;
}

export interface PCStageDistribution {
  stageName: string; // Intake, Survey, Design, Permit, NEM, Install, PTO
  projectCount: number;
  avgDaysInStage: number;
  percentage: number;
}

export interface PCResponseBreakdown {
  status: string;
  count: number;
  percentage: number;
}

export interface PCCoordinatorPerformance {
  coordinatorName: string;
  coordinatorEmail: string;
  totalProjects: number;
  dailyOutreach: number;
  responseRate: number;
  avgTimeToContact: number;
  escalationRate: number;
  slaCompliance: number;
  projectsCompleted: number; // projects that reached PTO
  avgProjectVelocity: number; // avg days from sale to PTO
}

export interface PCProjectVelocity {
  milestoneName: string;
  avgDays: number; // average days to complete this milestone
  medianDays: number; // median days
  projectCount: number; // projects that completed this milestone
}

export interface PCBottleneck {
  stageName: string;
  projectCount: number; // projects stuck in this stage
  avgDaysInStage: number;
  longestProject: { projectId: string; customerName: string; daysInStage: number };
  severity: 'critical' | 'high' | 'normal'; // based on avg days and count
}

export interface PCHoldAnalysis {
  category: string; // hold reason category
  count: number; // projects on hold for this reason
  percentage: number; // percentage of total holds
  avgResolutionDays: number | null; // average days to resolve
  resolvedCount: number; // holds resolved
}

export interface PCTeamMetrics {
  coordinators: PCCoordinatorPerformance[]; // performance per PC
  projectVelocity: PCProjectVelocity[]; // velocity per milestone
  bottlenecks: PCBottleneck[]; // identified bottlenecks
  holdAnalysis: PCHoldAnalysis[]; // hold breakdown
  teamAverages: {
    responseRate: number;
    avgTimeToContact: number;
    slaCompliance: number;
  };
}

export interface PCAnalyticsData {
  personalMetrics: PCPersonalMetrics;
  outreachTrend: PCOutreachTrendData[]; // last 30 days
  stageDistribution: PCStageDistribution[];
  responseBreakdown: PCResponseBreakdown[];
  teamMetrics?: PCTeamMetrics; // only for operations_manager
}

export interface PCAnalyticsFilters {
  timeRange: '7days' | '30days' | '90days' | 'all';
  coordinatorEmail: string | 'all'; // for team view
}

// PC Calendar Types
export interface PCCalendarEvent {
  id: string; // unique event identifier, typically recordId
  title: string; // event title, e.g., "Install - John Smith"
  start: Date; // event start date/time
  end: Date; // event end date/time
  allDay: boolean; // whether event is all-day
  type: 'install' | 'survey' | 'outreach' | 'appointment' | 'site_visit';
  projectId: string; // related project ID
  recordId: number; // QuickBase record ID
  customerName: string;
  customerPhone: string;
  status: 'scheduled' | 'overdue' | 'completed' | 'cancelled';
  priority: 'high' | 'normal' | 'low';
  location?: string; // optional location for site visits
  notes?: string; // optional event notes
  coordinatorEmail: string; // PC assigned to this event
  metadata?: Record<string, any>; // additional event data
}

export type PCCalendarView = 'month' | 'week' | 'day' | 'agenda';

export interface PCCalendarFilters {
  eventTypes: PCCalendarEvent['type'][]; // filter by event types
  status: PCCalendarEvent['status'] | 'all';
  dateRange: { start: Date; end: Date };
}

// =============================================================================
// PC INSPECTIONS TYPES
// =============================================================================

// Inspection status type
export type PCInspectionStatus =
  | 'waiting_for_inspection'   // Install complete, no inspection scheduled
  | 'inspection_scheduled'     // Inspection date is scheduled
  | 'inspection_failed'        // Inspection failed, needs remediation
  | 'inspection_passed'        // Inspection passed, waiting for PTO
  | 'pto_milestone';           // PTO stage - includes ready for PTO, PTO in progress, and failed

// Inspection failure category type
export type PCInspectionFailureCategory =
  | 'electrical'
  | 'structural'
  | 'code_violation'
  | 'documentation'
  | 'other';

// Inspection blocker type
export type PCInspectionBlocker =
  | 'as_builts_pending'
  | 'permit_pending'
  | 'on_hold'
  | 'blocked'
  | 'ready';

// PTO sub-status type (for categorizing within PTO milestone tab)
export type PCPTOSubStatus =
  | 'ready_for_pto'           // Inspection passed, ready to submit PTO
  | 'pto_in_progress'         // PTO submitted, awaiting utility approval
  | 'inspection_failed_pto';  // Inspection failed, needs reinspection before PTO

// PC Inspection Project
export interface PCInspectionProject {
  recordId: number;
  projectId: string;
  customerName: string;
  customerPhone: string;
  salesOffice: string;
  inspectionStatus: PCInspectionStatus;
  installCompletedDate: string | null;
  inspectionScheduledDate: string | null;
  inspectionFailedDate: string | null;
  inspectionPassedDate: string | null;
  daysInStatus: number; // days since entering current status
  failureReason: string | null; // from NOTE or BLOCK_REASON field
  failureCategory: PCInspectionFailureCategory | null; // categorized failure type
  asBuiltSubmitted: boolean; // whether as-built documents are submitted
  permitStatus: string | null; // current permit status
  permitApproved: string | null; // permit approval date
  permitSubmitted: string | null; // permit submission date
  permitRejected: string | null; // permit rejected/needs revisions
  permitResubmitted: string | null; // permit resubmitted date
  recentNoteCategory: string | null; // category of most recent note
  holdReason: string | null; // reason project is on hold
  blockReason: string | null; // reason project is blocked
  blockers: PCInspectionBlocker[]; // active blockers for inspection scheduling
  coordinatorEmail: string;
  salesRepName: string;
  salesRepEmail: string;
  lenderName: string;
  // PTO-specific fields (only populated when status is 'pto_milestone')
  ptoStatus?: string | null; // Current PTO status
  ptoSubmitted?: string | null; // PTO submission date
  ptoSubStatus?: PCPTOSubStatus | null; // PTO sub-category
  daysInPTO?: number | null; // Days since PTO submitted
  nemStatus?: string | null; // NEM/Interconnection status
  lenderPTOGreenlight?: string | null; // Lender PTO approval date
  lenderFundingReceived?: string | null; // Lender funding received date
  utilityApprovalDate?: string | null; // Utility approval date
}

// PC Inspection Data (returned by API)
export interface PCInspectionData {
  waitingForInspection: PCInspectionProject[];
  inspectionScheduled: PCInspectionProject[];
  inspectionFailed: PCInspectionProject[];
  inspectionPassed: PCInspectionProject[];
  ptoReadyForSubmission?: PCInspectionProject[]; // Ready for PTO (optional, only for PTO tab)
  ptoInProgress?: PCInspectionProject[]; // PTO submitted, pending approval (optional)
  ptoInspectionFailed?: PCInspectionProject[]; // Failed inspections in PTO view (optional)
  counts: {
    waitingForInspection: number;
    inspectionScheduled: number;
    inspectionFailed: number;
    inspectionPassed: number;
    ptoReadyForSubmission?: number; // Optional PTO counts
    ptoInProgress?: number;
    ptoInspectionFailed?: number;
    ptoTotal?: number; // Total PTO milestone count
  };
}

// PC Inspection Filters
export interface PCInspectionFilters {
  status: PCInspectionStatus | 'all';
  office: string | 'all';
  salesRep: string | 'all';
  search: string;
  dateRange: '7days' | '30days' | '90days' | 'custom' | 'all';
  customStartDate?: string; // ISO date string for custom range
  customEndDate?: string; // ISO date string for custom range
  state?: string | 'all'; // State filtering
}

// =============================================================================
// MILESTONE DASHBOARD TYPES
// =============================================================================

// Operations milestone type (project lifecycle stages) - 7 milestones
export type OperationsMilestone =
  | 'intake'       // Initial customer onboarding
  | 'survey'       // Site survey scheduling and completion
  | 'design'       // Design creation and approval
  | 'permitting'   // AHJ, NEM, and HOA permit processing
  | 'install'      // Installation scheduling and completion
  | 'inspections'  // Inspection scheduling and completion (plural form)
  | 'pto';         // Permission to Operate submission and approval

// Milestone-specific status types (mapped by milestone)
export type MilestoneStatus = {
  intake: 'deposit_received' | 'pending_documents' | 'documents_received' | 'intake_complete' | 'on_hold';
  survey: 'survey_requested' | 'survey_scheduled' | 'survey_completed' | 'pending_review' | 'blocked';
  design: 'design_in_progress' | 'pending_approval' | 'approved' | 'revisions_needed' | 'completed';
  permitting: 'ahj_pending' | 'ahj_approved' | 'ahj_rejected' | 'nem_pending' | 'nem_approved' | 'hoa_pending' | 'hoa_approved' | 'all_approved';
  install: 'install_scheduled' | 'install_in_progress' | 'install_completed' | 'pending_punch_list' | 'completed';
  inspection: 'waiting_for_inspection' | 'inspection_scheduled' | 'inspection_passed' | 'inspection_failed' | 'reinspection_scheduled';
  pto: 'ready_for_pto' | 'pto_submitted' | 'pto_approved' | 'pto_rejected' | 'inspection_failed_pto';
};

// Milestone project interface (project data for milestone dashboards)
export interface MilestoneProject {
  recordId: number;
  projectId: string;
  customerName: string;
  customerPhone: string;
  salesOffice: string;
  salesRepName: string;
  salesRepEmail: string;
  coordinatorEmail: string;
  lenderName: string;
  projectStatus: string; // Overall project status
  currentMilestone: OperationsMilestone; // Which milestone the project is in
  milestoneStatus: string; // Status within current milestone
  daysInMilestone: number; // Days in current milestone
  daysInStatus: number; // Days in current status
  isBlocked: boolean; // Whether project is blocked
  blockReason: string | null; // Reason for block
  isOnHold: boolean; // Whether project is on hold
  holdReason: string | null; // Reason for hold

  // Milestone-specific fields
  // Intake
  depositDate?: string | null;
  documentsReceivedDate?: string | null;

  // Survey
  surveyScheduledDate?: string | null;
  surveyCompletedDate?: string | null;

  // Design
  designStartedDate?: string | null;
  designApprovedDate?: string | null;

  // Permitting
  ahjStatus?: string | null;
  ahjSubmitted?: string | null;
  ahjApproved?: string | null;
  nemStatus?: string | null;
  nemSubmitted?: string | null;
  nemApproved?: string | null;
  hoaStatus?: string | null;
  hoaSubmitted?: string | null;
  hoaApproved?: string | null;

  // Install
  installScheduledDate?: string | null;
  installCompletedDate?: string | null;

  // Inspection
  inspectionScheduledDate?: string | null;
  inspectionPassedDate?: string | null;
  inspectionFailedDate?: string | null;
  failureReason?: string | null;

  // PTO
  ptoSubmitted?: string | null;
  ptoApproved?: string | null;
  utilityApprovalDate?: string | null;
}

// Milestone dashboard metrics
export interface MilestoneDashboardMetrics {
  total: number; // Total projects in milestone
  byStatus: Record<string, number>; // Count by status
  avgDaysInMilestone: number; // Average days in milestone
  avgDaysInStatus: number; // Average days in current status
  oldestProject: {
    projectId: string;
    customerName: string;
    daysInMilestone: number;
  } | null;
  newestProject: {
    projectId: string;
    customerName: string;
    daysInMilestone: number;
  } | null;
  blockedCount: number; // Projects blocked in this milestone
  onHoldCount: number; // Projects on hold in this milestone
}

// Milestone dashboard data (returned by API)
export interface MilestoneDashboardData {
  milestone: OperationsMilestone;
  projects: MilestoneProject[]; // All projects in this milestone
  projectsByStatus: Record<string, MilestoneProject[]>; // Projects grouped by status
  metrics: MilestoneDashboardMetrics;
  availableStatuses: string[]; // Valid statuses for this milestone
}

// Milestone dashboard filters
export interface MilestoneDashboardFilters {
  milestone: OperationsMilestone;
  status: string | 'all'; // Status within milestone
  office: string | 'all';
  salesRep: string | 'all';
  coordinator: string | 'all';
  search: string;
  dateRange: '7days' | '30days' | '90days' | 'custom' | 'all';
  customStartDate?: string;
  customEndDate?: string;
  showBlocked: boolean; // Include blocked projects
  showOnHold: boolean; // Include on-hold projects
}

// =============================================================================
// FIELD TRACKING TYPES (Arrivy Integration)
// =============================================================================

// Field tracking task types
export type FieldTrackingTaskType = 'survey' | 'install' | 'inspection' | 'service' | 'other';
export type FieldTrackingTaskStatus = 
  | 'NOT_STARTED'
  | 'ENROUTE'
  | 'STARTED'
  | 'COMPLETE'
  | 'CANCELLED'
  | 'EXCEPTION'
  | 'LATE'
  | 'NOSHOW';

// Field tracking task with Arrivy data for dashboard display
export interface FieldTrackingTask {
  id: number;
  arrivy_task_id: number;
  url_safe_id: string;
  quickbase_project_id: string | null;
  quickbase_record_id: number | null;
  customer_name: string | null;
  customer_phone: string | null;
  customer_email: string | null;
  customer_address: string | null;
  task_type: FieldTrackingTaskType | null;
  scheduled_start: Date | null;
  scheduled_end: Date | null;
  assigned_entity_ids: number[] | null;
  current_status: FieldTrackingTaskStatus | null;
  tracker_url: string | null;
  business_tracker_url: string | null;
  template_id: string | null;
  latest_status?: string | null;
  latest_status_time?: Date | null;
  entity_names?: string[] | null;
  hasQuickBaseLink?: boolean;
  created_at: Date;
  updated_at: Date;
  synced_at: Date | null;
}

// Crew member with location and status
export interface FieldTrackingEntity {
  id: number;
  arrivy_entity_id: number;
  name: string;
  email: string | null;
  phone: string | null;
  entity_type: string | null;
  quickbase_user_id: string | null;
  current_location?: FieldTrackingLocation | null;
  last_update?: Date | null;
  assigned_tasks_count?: number;
  status?: 'active' | 'inactive' | 'offline';
  created_at: Date;
  updated_at: Date;
}

// Event timeline item for activity feed
export interface FieldTrackingEvent {
  id: number;
  event_id: number;
  event_type: string;
  event_sub_type: string | null;
  timestamp: Date;
  reporter_id: number | null;
  reporter_name: string | null;
  message: string | null;
  title: string | null;
  arrivy_task_id: number | null;
  quickbase_project_id?: string | null;
  object_fields?: Record<string, any> | null;
  extra_fields?: Record<string, any> | null;
  is_transient: boolean;
  created_at: Date;
}

/**
 * Activity feed filter state
 */
export interface ActivityFeedFilters {
  eventType: string | 'all'; // Filter by event type (e.g., TASK_CREATED, LATE, NOSHOW)
  dateRange: { start: Date; end: Date } | null; // Date range filter (null = no date filtering)
  crewMember: string | 'all'; // Filter by crew member name (reporter_name)
  taskType: string | 'all'; // Filter by task type (survey, install, inspection, service)
  search: string; // Search query for customer name or task ID
}

/**
 * Activity feed API response structure
 */
export interface ActivityFeedResponse {
  events: FieldTrackingEvent[]; // Array of matching events
  total: number; // Total count of events matching filters
  hasMore: boolean; // Whether more events are available for pagination
  offset: number; // Current pagination offset
  limit: number; // Events per page
}

/**
 * Event type counts for filter dropdowns (optional)
 */
export type EventTypeCounts = Record<string, number>;

// Task status with details
export interface FieldTrackingStatus {
  id: number;
  arrivy_task_id: number;
  status_type: string;
  reported_at: Date;
  reporter_id: number | null;
  reporter_name: string | null;
  notes: string | null;
  has_attachments: boolean;
  visible_to_customer: boolean;
  source: string | null;
  created_at: Date;
}

/**
 * Task attachment metadata from Arrivy status updates
 * Files are stored in Arrivy and accessed via their web interface
 */
export interface TaskAttachment {
  file_id: number;
  file_path: string;
  filename: string;
  status_id: number;
  uploaded_by: string | null;
  uploaded_at: Date;
}

/**
 * Customer rating data from TASK_RATING events
 * Extracted from arrivy_events table extra_fields and object_fields
 */
export interface TaskRating {
  event_id: number;
  rating: number;
  rating_type: string;
  feedback: string | null;
  customer_name: string | null;
  rated_at: Date;
}

/**
 * Customer note data from note-type events
 * Includes any customer-submitted notes or comments beyond ratings
 */
export interface CustomerNote {
  event_id: number;
  note: string;
  customer_name: string | null;
  created_at: Date;
  event_type: string;
}

/**
 * Full crew member contact information
 * Extended from basic entity data with phone and email
 */
export interface CrewContact {
  entity_id: number;
  name: string;
  email: string | null;
  phone: string | null;
  entity_type: string | null;
}

/**
 * Calculated duration metrics for task completion analysis
 * Compares scheduled vs actual times and identifies delays
 */
export interface TaskDurationMetrics {
  scheduled_duration_minutes: number | null;
  actual_duration_minutes: number | null;
  time_to_start_minutes: number | null;
  is_completed: boolean;
  is_delayed: boolean;
  started_at: Date | null;
  completed_at: Date | null;
}

/**
 * Enhanced task details response
 * Combines task data with enriched operational information
 */
export interface EnhancedTaskDetails {
  task: FieldTrackingTask;
  statusHistory: FieldTrackingStatus[];
  events: FieldTrackingEvent[];
  attachments: TaskAttachment[];
  ratings: TaskRating[];
  customerNotes: CustomerNote[];
  crewContacts: CrewContact[];
  exceptions: TaskException[];
  durationMetrics: TaskDurationMetrics;
}

// Task exceptions (imported from database layer for convenience)
export interface TaskException {
  event_id: number;
  exception_type: 'EXCEPTION' | 'LATE' | 'NOSHOW';
  occurred_at: Date;
  reporter_name: string | null;
  description: string | null;
  exception_details?: {
    type?: string;
    notes?: string;
    reason?: string;
  } | null;
}

// Location data
export interface FieldTrackingLocation {
  lat: number;
  lng: number;
  timestamp: Date;
  address?: string | null;
  city?: string | null;
  state?: string | null;
  accuracy?: number;
  heading?: number;
  speed?: number;
}

// Main dashboard data structure
export interface FieldTrackingDashboardData {
  tasks: FieldTrackingTask[];
  entities: FieldTrackingEntity[];
  events: FieldTrackingEvent[];
  metrics: FieldTrackingMetrics;
}

// Dashboard metrics
export interface FieldTrackingMetrics {
  total_tasks: number;
  in_progress: number;
  completed_today: number;
  delayed: number;
  crews_active: number;
  avg_completion_time?: number; // in minutes
}

// Filter options for field tracking dashboard
export interface FieldTrackingFilters {
  task_type?: FieldTrackingTaskType | 'all';
  status?: FieldTrackingTaskStatus | 'all';
  date_range?: 'today' | 'this_week' | 'this_month' | 'custom';
  start_date?: string;
  end_date?: string;
  crew?: string; // entity email or ID
  search?: string; // customer name or project ID
}

// Arrivy sync result
export interface ArrivySyncResult {
  success: boolean;
  tasks_synced: number;
  entities_synced: number;
  errors: Array<{
    project_id: string;
    error: string;
  }>;
}

// Arrivy task mapping between QuickBase and Arrivy
// QuickBase fields are null for tasks originating in Arrivy
export interface ArrivyTaskMapping {
  quickbase_project_id: string | null;
  quickbase_record_id: number | null;
  arrivy_task_id: number;
  url_safe_id: string;
  synced_at: Date;
  tracker_url: string;
}

// ============================================================================
// CREW PERFORMANCE ANALYTICS TYPES
// ============================================================================

/**
 * Comprehensive performance metrics for a single crew member
 * Aggregated from Arrivy task completion, duration, ratings, and status data
 */
export interface CrewPerformanceMetrics {
  entity_id: number;
  crew_name: string;
  crew_email: string | null;
  crew_phone: string | null;
  tasks_completed_today: number;
  tasks_completed_week: number;
  tasks_completed_month: number;
  tasks_completed_total: number;
  avg_completion_time_minutes: number | null;
  on_time_percentage: number;
  customer_rating_avg: number | null;
  customer_rating_count: number;
  tasks_currently_assigned: number;
  total_tasks_assigned: number;
  delayed_tasks_count: number;
  exception_count: number;
  noshow_count: number;
}

/**
 * Comparison data for a specific crew member against team averages
 * Used for generating comparison charts and performance indicators
 */
export interface CrewComparisonData {
  crew_name: string;
  metric_value: number;
  team_average: number;
  delta: number;
  rank: number;
}

/**
 * Daily performance trend data for crew members
 * Used for historical trend charts showing performance over time
 */
export interface CrewPerformanceTrend {
  date: string; // YYYY-MM-DD format
  crew_name: string;
  tasks_completed: number;
  avg_completion_time: number;
  on_time_percentage: number;
}

/**
 * Leaderboard data identifying top performers and crew members needing support
 */
export interface CrewLeaderboard {
  top_performers: Array<{
    crew_name: string;
    metric_value: number;
    metric_name: string;
  }>;
  needs_support: Array<{
    crew_name: string;
    metric_value: number;
    issue: string;
  }>;
}

/**
 * Filter options for crew performance analytics
 */
export interface CrewPerformanceFilters {
  timeRange: '7days' | '30days' | '90days' | 'all';
  crewMember: string | 'all';
  taskType: string | 'all';
  metric: 'tasks_completed' | 'completion_time' | 'on_time' | 'ratings';
}

/**
 * Complete crew performance dashboard data structure
 * Returned by the /api/operations/crew-performance endpoint
 */
export interface CrewPerformanceDashboardData {
  crewMetrics: CrewPerformanceMetrics[];
  teamAverages: Omit<CrewPerformanceMetrics, 'entity_id' | 'crew_name' | 'crew_email' | 'crew_phone'>;
  leaderboard: CrewLeaderboard;
  trends?: CrewPerformanceTrend[];
}
