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
  /** Projects approved on first intake attempt */
  firstTimeApproved: number;
  /** Projects rejected on first intake attempt */
  firstTimeRejected: number;
  /** Projects pending first review (intake not yet completed) */
  pendingReview: number;
  /** Projects that were rejected then resubmitted and approved */
  resubmittedAndApproved: number;
  /** First-time pass rate as percentage */
  firstTimePassRate: number;
  /** Rejection rate as percentage */
  rejectionRate: number;
  /** Top rejection reasons for this closer in date range */
  topRejectionReasons: Array<{
    reason: string;
    count: number;
  }>;
  /** Average days to resolve rejections */
  avgResolutionTime: number | null;
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
