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
  fileAttachments: any; // field 9
  opsFeedback?: string | null; // field for ops feedback
}

// Union types for status fields
export type TaskStatus = 'Not Started' | 'In Progress' | 'Complete';
export type SubmissionStatus = 'Pending Approval' | 'Reviewed';
export type OpsDisposition = 'Approved' | 'Needs Revision';
