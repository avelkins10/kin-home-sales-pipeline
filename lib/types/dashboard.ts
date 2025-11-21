// lib/types/dashboard.ts
import type { Notification } from './notification';

export type TimeRange = 'lifetime' | 'ytd' | 'month' | 'last_month' | 'week' | 'today' | 'custom' | 'last_30' | 'last_90' | 'last_12_months';
export type MetricsScope = 'personal' | 'team';

export interface CustomDateRange {
  startDate: string; // ISO date string
  endDate: string;   // ISO date string
}

export interface ProjectBuckets {
  installs: number;
  rejected: number;
  onHold: number;
  repAttention: number;
  pendingCancel: number;
  readyForInstall: number;
}

export interface CommissionBreakdown {
  earned: number;
  lost: number;
  onHold: number;
  pending: number;
  salesAid: number;
}

export interface TeamMemberCommission {
  memberName: string;        // Display name (closer or setter name)
  memberEmail: string | null; // Email for identification (may be null)
  role: 'closer' | 'setter';  // Role in projects
  earnedCommission: number;
  lostCommission: number;
  onHoldCommission: number;
  pendingCommission: number;
  projectCount: number;       // Number of projects for this member
}

export interface TeamMemberBuckets {
  memberName: string;        // Display name (closer or setter name)
  memberEmail: string | null; // Email for identification (may be null)
  role: 'closer' | 'setter';  // Role in projects
  installs: number;           // Projects completed but not yet PTO'd
  rejected: number;           // Rejected projects
  onHold: number;             // Projects currently on hold
  repAttention: number;       // Projects needing action (>90 days or hold >7 days)
  pendingCancel: number;      // Projects in cancellation process
  readyForInstall: number;    // Projects ready to schedule
  totalProjects: number;      // Total projects across all buckets
}

export interface RevenueMetrics {
  grossRevenue: number;
  installedRevenue: number;
  soldAccountsCount: number;
  installCount: number;
}

export interface EnhancedDashboardMetrics {
  // Basic metrics (existing)
  installsThisWeek: number;
  activeProjects: number;
  onHold: number;
  holdBreakdown: string;
  installsThisMonth: number;
  
  // Revenue metrics
  soldAccounts: number;
  grossRevenue: number;
  installCount: number;
  installedRevenue: number;
  
  // Performance metrics
  retentionRate: number; // Percentage
  
  // Commission breakdown
  earnedCommission: number;
  lostCommission: number;
  onHoldCommission: number;
  pendingCommission: number;
  salesAidCommission: number;
  
  // Team member breakdown (only populated for managers with team scope)
  commissionByMember?: TeamMemberCommission[];
  
  // Project buckets
  buckets: ProjectBuckets;
  
  // Team member bucket breakdown (only populated for managers with team scope)
  bucketsByMember?: TeamMemberBuckets[];
  
  // Metadata
  timeRange: TimeRange;
  customDateRange?: CustomDateRange;
  scope: MetricsScope; // Indicates whether metrics are personal or team-wide
}

export interface NotificationsSummary {
  notifications: Notification[];
  totalUnread: number;
  criticalCount: number;
  hasMore: boolean;
}

export type TeamActivityType = 
  | 'install_completed'
  | 'pto_approved'
  | 'placed_on_hold'
  | 'cancelled';

export interface TeamActivityItem {
  recordId: number;              // Project record ID for linking
  projectId: string;             // Human-readable project ID
  customerName: string;          // Customer name for display
  activityType: TeamActivityType; // Type of activity
  activityDescription: string;   // Human-readable description (e.g., "Install completed")
  teamMemberName: string;        // Closer or setter name
  teamMemberRole: 'closer' | 'setter'; // Role of team member
  timestamp: string;             // ISO date string of when activity occurred
  daysAgo: number;               // Calculated days since activity
}

export interface TeamActivityFeed {
  activities: TeamActivityItem[];
  totalCount: number;            // Total activities in time window
  hasMore: boolean;              // True if more than limit
}

// All types are already exported above as interfaces
