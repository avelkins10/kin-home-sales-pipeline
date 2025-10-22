// QuickbaseField type inline definition
type QuickbaseField<T = any> = {
  value: T | null;
};

// Raw QuickBase response format for Task Groups table (bu36gem4p)
export interface QuickbaseTaskGroup {
  [fieldId: number]: QuickbaseField<any>;
}

// Typed interface for Task Groups
export interface TaskGroup {
  recordId: number; // field 3
  relatedProject: number; // field 10
  totalTasks: number; // field 31
  unapprovedTasks: number; // field 32
  tasks: Task[]; // populated from child query
}

// Raw QuickBase response format for Tasks table (bu36ggiht)
export interface QuickbaseTask {
  [fieldId: number]: QuickbaseField<any>;
}

// Typed interface for Tasks
export interface Task {
  recordId: number; // field 3
  taskGroup: number; // field 6
  status: TaskStatus; // field 9
  name: string; // field 10
  category: string | null; // field 31
  maxSubmissionStatus: string | null; // field 13
  submissions: TaskSubmission[]; // populated from child query
}

// Raw QuickBase response format for Task Submissions table (bu36g8j99)
export interface QuickbaseTaskSubmission {
  [fieldId: number]: QuickbaseField<any>;
}

// Typed interface for Task Submissions
export interface TaskSubmission {
  recordId: number; // field 3
  dateCreated: string; // field 1
  relatedTask: number; // field 6
  submissionStatus: SubmissionStatus; // field 7
  opsDisposition: OpsDisposition | null; // field 8
  fileAttachment1: any; // field 9 - primary file
  isMaxSubmission?: boolean; // field 13 - is this the latest submission?
  submissionNote?: string | null; // field 24 - rep's notes
  fileAttachment2?: any; // field 25 - additional file 1
  fileAttachment3?: any; // field 26 - additional file 2
  opsDispositionNote?: string | null; // field 36 - ops feedback/review notes
  opsReviewCompletedBy?: string | null; // field 37 - who reviewed
  opsReviewCompletedAt?: string | null; // field 38 - when reviewed
  // Deprecated - keeping for backwards compatibility
  fileAttachments?: any; // alias for fileAttachment1
  opsFeedback?: string | null; // alias for opsDispositionNote
}

// Union types for status fields
export type TaskStatus = 'Not Started' | 'In Progress' | 'Complete' | 'Approved' | 'Closed by Ops';
export type SubmissionStatus = 'Pending Approval' | 'Reviewed' | 'Approved';
export type OpsDisposition = 'Approved' | 'Needs Revision';
