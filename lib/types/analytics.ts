// lib/types/analytics.ts
// Type definitions for analytics and reason parsing functionality

/**
 * Standardized cancel reason categories for project cancellations
 * Used to categorize cancellation reasons from various Quickbase fields
 */
export type CancelReasonCategory = 
  | 'Finance - Failed Loan'
  | 'Customer - Unresponsive'
  | 'Customer - New Move-In'
  | 'Customer - Changed Mind'
  | 'Documentation - Rejected'
  | 'Property Issues - Roof Work Required'
  | 'Permit Delays'
  | 'Unknown / Not Specified';

/**
 * Standardized hold reason categories for project holds
 * Used to categorize hold reasons from various Quickbase fields
 */
export type HoldReasonCategory = 
  | 'Finance - Failed Loan'
  | 'Customer - Unresponsive'
  | 'Customer - New Move-In'
  | 'Customer - Changed Mind'
  | 'Documentation - Rejected'
  | 'Property Issues - Roof Work Required'
  | 'Permit Delays'
  | 'Unknown / Not Specified';

/**
 * Result of parsing reason data from Quickbase fields
 * Contains the categorized reason, original value, and source field
 */
export interface ParsedReason {
  /** Standardized category for the reason */
  category: CancelReasonCategory | HoldReasonCategory;
  /** Original raw value from the source field */
  rawValue: string | null;
  /** Which field the reason was extracted from */
  source: 'audit_log' | 'cancel_reason' | 'note_category' | 'intake_missing_items' | 'unknown';
}

/**
 * Input structure for reason parsing functions
 * Contains all the fields that may contain reason information
 */
export interface ReasonParserInput {
  /** Audit log information about project status before cancellation */
  auditLogPreCancelStatus?: string | null;
  /** Direct cancellation reason from cancel reason field */
  cancelReason?: string | null;
  /** Category of the most recent note */
  recentNoteCategory?: string | null;
  /** Missing items from intake process */
  intakeMissingItems?: string | null;
}

/**
 * Office-level aggregated metrics for analytics dashboard
 * Used by /api/analytics/office-metrics endpoint
 */
export interface OfficeMetrics {
  /** Name of the office */
  officeName: string;
  /** Quickbase record ID of the office */
  officeId: number;
  /** Total number of projects in the time range */
  totalProjects: number;
  /** Average system size in kW across all projects */
  avgSystemSize: number;
  /** Average gross PPW across all projects */
  avgGrossPpw: number;
  /** Average net PPW across all projects */
  avgNetPpw: number;
  /** Average commissionable PPW across all projects */
  avgCommissionablePpw: number;
  /** Average cycle time in days for completed projects */
  avgCycleTime: number | null;
  /** Percentage of projects with approved intake status */
  intakeApprovalRate: number;
  /** Percentage of projects approved on first intake attempt (clean deals) */
  firstTimePassRate: number;
  /** Percentage of projects rejected on first intake attempt */
  rejectionRate: number;
  /** Of rejected projects, percentage that were eventually approved after resubmit */
  resubmitSuccessRate: number;
  /** Average days to resolve intake rejections (from first rejection to approval) */
  avgResolutionTime: number | null;
  /** Most common intake rejection reasons */
  topRejectionReasons: Array<{ reason: string; count: number }>;
  /** Number of active projects */
  activeProjects: number;
  /** Number of cancelled projects */
  cancelledProjects: number;
  /** Number of projects on hold */
  onHoldProjects: number;
  /** Number of projects submitted for approval */
  projectsSubmitted: number;
  /** Number of projects approved */
  projectsApproved: number;
  /** Number of projects rejected */
  projectsRejected: number;
  /** Number of completed installs */
  installs: number;
  /** Number of holds */
  holds: number;
  /** Monthly install data for sparkline visualization */
  monthlyInstalls: Array<{ month: string; installs: number }>;
  /** Number of projects pending KCA */
  pendingKcaProjects: number;
  /** Number of projects on finance hold */
  financeHoldProjects: number;
  /** Number of projects pending cancellation */
  pendingCancelProjects: number;
  /** Number of projects on roof hold */
  roofHoldProjects: number;
}

/**
 * Per-rep performance metrics for analytics dashboard
 * Used by /api/analytics/rep-performance endpoint
 */
export interface RepPerformance {
  /** Optional rep ID for identification (uses email if not available) */
  repId?: string | null;
  /** Name of the sales rep */
  repName: string;
  /** Email address of the sales rep */
  repEmail: string | null;
  /** Role of the rep (closer or setter) */
  role: 'closer' | 'setter';
  /** Office ID where the rep works */
  officeId: number | null;
  /** Name of the office where the rep works */
  officeName: string | null;
  /** Total number of projects in the time range */
  totalProjects: number;
  /** Average system size in kW across all projects */
  avgSystemSize: number;
  /** Average gross PPW across all projects */
  avgGrossPpw: number;
  /** Average net PPW across all projects */
  avgNetPpw: number;
  /** Average commissionable PPW across all projects */
  avgCommissionablePpw: number;
  /** Average cycle time in days for completed projects */
  avgCycleTime: number | null;
  /** Percentage of projects with approved intake status */
  intakeApprovalRate: number;
  /** Percentage of projects approved on first intake attempt (clean deals) */
  firstTimePassRate: number;
  /** Percentage of projects rejected on first intake attempt */
  rejectionRate: number;
  /** Of rejected projects, percentage that were eventually approved after resubmit */
  resubmitSuccessRate: number;
  /** Average days to resolve intake rejections (from first rejection to approval) */
  avgResolutionTime: number | null;
  /** Most common intake rejection reasons */
  topRejectionReasons: Array<{ reason: string; count: number }>;
  /** Number of active projects */
  activeProjects: number;
  /** Number of cancelled projects */
  cancelledProjects: number;
  /** Number of projects on hold */
  onHoldProjects: number;
  /** Number of projects submitted for approval */
  projectsSubmitted: number;
  /** Number of projects approved */
  projectsApproved: number;
  /** Number of projects rejected */
  projectsRejected: number;
  /** Number of completed installs */
  installs: number;
  /** Number of holds */
  holds: number;
  /** Cancellation rate as a percentage */
  cancellationRate?: number;
  /** Hold rate as a percentage */
  holdRate?: number;
}

/**
 * Detailed rep metrics including project list
 * Used by /api/analytics/rep-details endpoint
 */
export interface RepDetailMetrics {
  /** Rep performance metrics */
  rep: RepPerformance;
  /** List of projects for this rep */
  projects: Array<{
    recordId: number;
    projectId: string;
    customerName: string;
    status: string;
    systemSize: number;
    grossPpw: number;
    netPpw: number;
    commissionablePpw: number;
    salesDate: string | null;
    installDate: string | null;
    cycleTime: number | null;
    officeName: string | null;
  }>;
  /** Monthly performance trends */
  monthlyTrends: Array<{
    month: string;
    projects: number;
    installs: number;
    cancellations: number;
    holds: number;
    avgSystemSize: number;
    avgNetPpw: number;
  }>;
}

/**
 * Pipeline forecast data for install forecasting
 * Used by /api/analytics/pipeline-forecast endpoint
 */
export interface PipelineForecast {
  /** Number of projects forecasted to install in next 30 days */
  next30Days: number;
  /** Number of projects forecasted to install in next 60 days */
  next60Days: number;
  /** Number of projects forecasted to install in next 90 days */
  next90Days: number;
  /** Detailed project list (only included if includeDetails=true) */
  projects?: Array<{
    /** Quickbase record ID */
    recordId: number;
    /** Project ID string */
    projectId: string;
    /** Customer name */
    customerName: string;
    /** System size in kW */
    systemSize: number;
    /** Forecast date (derived from scheduled/estimated/tentative) */
    forecastDate: string;
    /** Original estimated install date (if available) */
    estimatedInstallDate: string | null;
    /** Original scheduled install date (if available) */
    scheduledInstallDate: string | null;
    /** Source of the forecast date */
    forecastSource: 'scheduled' | 'estimated' | 'tentative';
  }>;
}

/**
 * Individual milestone timing statistics
 * Used for milestone timing analysis
 */
export interface MilestoneTiming {
  /** Name of the milestone */
  milestoneName: string;
  /** Average duration in days */
  avgDays: number | null;
  /** Median duration in days */
  medianDays: number | null;
  /** Minimum duration in days */
  minDays: number | null;
  /** Maximum duration in days */
  maxDays: number | null;
  /** Number of projects that completed this milestone */
  projectCount: number;
  /** Completion rate as percentage */
  completionRate: number;
}

/**
 * Complete milestone timing analysis
 * Used by /api/analytics/milestone-timings endpoint
 */
export interface MilestoneTimings {
  /** Overall cycle time from sale to PTO */
  overallCycleTime: MilestoneTiming;
  /** Individual milestone timings */
  milestones: MilestoneTiming[];
}

/**
 * Cancellation analysis breakdown
 * Used for cancellation reason analysis
 */
export interface CancellationAnalysis {
  /** Category of cancellation reason */
  category: CancelReasonCategory;
  /** Number of projects cancelled for this reason */
  count: number;
  /** Percentage of total cancellations */
  percentage: number;
  /** Dominant source field for this category */
  dominantSource: string;
}

/**
 * Hold analysis breakdown
 * Used for hold reason analysis
 */
export interface HoldAnalysis {
  /** Category of hold reason */
  category: HoldReasonCategory;
  /** Number of projects on hold for this reason */
  count: number;
  /** Percentage of total holds */
  percentage: number;
  /** Average resolution time in days */
  avgResolutionDays: number | null;
  /** Number of resolved holds for this category */
  resolvedCount: number;
  /** Dominant source field for this category */
  dominantSource: string;
}

/**
 * Monthly install data for sparkline visualization
 * Used in office comparison table to show trend over time
 */
export interface OfficeSparklineData {
  /** Month in YYYY-MM format */
  month: string;
  /** Number of installs in that month */
  installs: number;
}

/**
 * Office comparison table row extending OfficeMetrics with additional properties
 * Used for displaying office metrics in sortable comparison table
 */
export interface OfficeComparisonRow extends OfficeMetrics {
  /** Rank based on current sort column (1 = best) */
  rank: number;
  /** Monthly install data for sparkline visualization */
  sparklineData: OfficeSparklineData[];
  /** Whether this office is in top 20% for current metric */
  isOutlierHigh: boolean;
  /** Whether this office is in bottom 20% for current metric */
  isOutlierLow: boolean;
}

/**
 * Sortable columns for office comparison table
 * Defines which columns can be sorted in the comparison view
 */
export type SortColumn = 
  | 'officeName' 
  | 'totalProjects' 
  | 'avgSystemSize' 
  | 'avgGrossPpw' 
  | 'avgNetPpw' 
  | 'avgCommissionablePpw' 
  | 'avgCycleTime' 
  | 'activeProjects' 
  | 'cancelledProjects' 
  | 'onHoldProjects';

/**
 * Sort direction for table columns
 * Used in conjunction with SortColumn for table sorting
 * Matches HTML aria-sort attribute values
 */
export type SortDirection = 'ascending' | 'descending';

/**
 * Office detail page metrics combining office and rep data
 * Used for individual office detail pages with comprehensive metrics
 */
export interface OfficeDetailMetrics {
  /** Office-level aggregated metrics */
  office: OfficeMetrics;
  /** Performance data for all reps in this office */
  reps: RepPerformance[];
  /** Recent projects in this office (placeholder for future enhancement) */
  recentProjects: Array<{
    recordId: number;
    projectId: string;
    customerName: string;
    status: string;
    systemSize: number;
    grossPpw: number;
  }>;
  /** Monthly trends for this office (placeholder for future enhancement) */
  monthlyTrends: Array<{
    month: string;
    projects: number;
    installs: number;
    cancellations: number;
    holds: number;
  }>;
}
