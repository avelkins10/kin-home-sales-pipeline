// lib/types/dashboard.ts
import type { Notification } from './notification';

export type TimeRange = 'lifetime' | 'month' | 'week';

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
  
  // Project buckets
  buckets: ProjectBuckets;
  
  // Metadata
  timeRange: TimeRange;
}

export interface NotificationsSummary {
  notifications: Notification[];
  totalUnread: number;
  criticalCount: number;
  hasMore: boolean;
}

// All types are already exported above as interfaces
