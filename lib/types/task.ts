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
  dateCreated: string | null; // field 1 - when task was assigned
  dateModified: string | null; // field 2 - last update
  taskGroup: number; // field 6
  status: TaskStatus; // field 9
  name: string; // field 10
  description: string | null; // field 11 - detailed instructions
  maxSubmissionStatus: string | null; // field 13
  taskTemplate: number | null; // field 16 - links to template
  category: string | null; // field 30 - Finance Approved, Utility Bill, etc.
  missingItem: string | null; // field 31 - Title Verification, Income Verification, etc.
  reviewedByOps: string | null; // field 40 - task-level review timestamp
  reviewedByOpsUser: string | null; // field 41 - task-level reviewer
  opsReviewNote: string | null; // field 42 - task-level ops feedback
  submissions: TaskSubmission[]; // populated from child query
}

// Raw QuickBase response format for Task Templates table (bu36jyuf9)
export interface QuickbaseTaskTemplate {
  [fieldId: number]: QuickbaseField<any>;
}

// Typed interface for Task Templates
export interface TaskTemplate {
  recordId: number; // field 3
  taskName: string; // field 6
  category: string | null; // field 9 - Intake Task: Category
  missingItem: string | null; // field 10 - Intake Task: Missing Item
  description: string | null; // field 18 - Detailed instructions
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
