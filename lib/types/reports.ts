/**
 * Types for reporting features
 */

/**
 * Weekly intake quality report for a single closer
 */
export interface WeeklyIntakeCloserReport {
  /** Closer's name */
  closerName: string;
  /** Closer's email */
  closerEmail: string;
  /** Closer's QuickBase user ID */
  closerId?: string;
  /** Office name */
  officeName: string | null;
  /** Total projects submitted (sales) in date range */
  totalSubmitted: number;
  /** Projects approved on first pass (never rejected) */
  neverRejected: number;
  /** Total projects rejected (currently or ever) */
  totalRejections: number;
  /** Rejected projects that were resubmitted and approved */
  totalFixed: number;
  /** Rejected projects not yet fixed (still rejected) */
  stillRejected: number;
  /** Projects that passed intake (have INTAKE_COMPLETED_DATE) */
  activeApproved: number;
  /** First-time pass rate as percentage (never rejected / total submitted) */
  firstTimePassRate: number;
  /** Rejection rate as percentage (total rejections / total submitted) */
  rejectionRate: number;
  /** Top rejection reasons for this closer in date range */
  topRejectionReasons: Array<{
    reason: string;
    count: number;
  }>;
  /** Average days to resolve rejections (from task creation to approval) */
  avgResolutionDays: number | null;
}

/**
 * Aggregated weekly intake report for all closers
 */
export interface WeeklyIntakeReport {
  /** Start date of report range */
  startDate: string;
  /** End date of report range */
  endDate: string;
  /** Total projects across all closers */
  totalProjects: number;
  /** Overall first-time pass rate */
  overallPassRate: number;
  /** Overall rejection rate */
  overallRejectionRate: number;
  /** Most common rejection reason across all closers */
  topRejectionReason: string | null;
  /** Per-closer breakdown */
  closers: WeeklyIntakeCloserReport[];
  /** Best performer (highest first-time pass rate) */
  topPerformer: {
    name: string;
    rate: number;
  } | null;
  /** Needs attention (lowest first-time pass rate) */
  needsAttention: {
    name: string;
    rate: number;
  } | null;
}
