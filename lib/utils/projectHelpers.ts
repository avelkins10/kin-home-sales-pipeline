// lib/utils/projectHelpers.ts
import { QuickbaseProject } from '@/lib/types/project';
import { PROJECT_FIELDS } from '@/lib/constants/fieldIds';

/**
 * Check if a project is currently on hold
 */
export function isProjectOnHold(project: QuickbaseProject): boolean {
  return project[PROJECT_FIELDS.ON_HOLD]?.value === true;
}

/**
 * Get the duration a project has been on hold in days
 */
export function getHoldDuration(project: QuickbaseProject): number {
  const dateOnHold = project[PROJECT_FIELDS.DATE_ON_HOLD]?.value;
  if (!dateOnHold) return 0;
  
  const holdDate = new Date(dateOnHold);
  const now = new Date();
  const diffTime = Math.abs(now.getTime() - holdDate.getTime());
  return Math.ceil(diffTime / (1000 * 60 * 60 * 24));
}

/**
 * Get the age of a project in days
 */
export function getProjectAge(project: QuickbaseProject): number {
  const salesDate = project[PROJECT_FIELDS.SALES_DATE]?.value;
  if (!salesDate) return 0;
  
  const projectDate = new Date(salesDate);
  const now = new Date();
  const diffTime = Math.abs(now.getTime() - projectDate.getTime());
  return Math.ceil(diffTime / (1000 * 60 * 60 * 24));
}

/**
 * Parse hold reason to extract category
 */
export function parseHoldReason(rawReason: any): string {
  if (!rawReason) return 'Unknown';
  const reasonStr = typeof rawReason === 'string' ? rawReason : String(rawReason);
  if (!reasonStr || reasonStr === 'undefined' || reasonStr === 'null') return 'Unknown';
  
  const holdTypes = [
    'Finance Hold', 'Roof Hold', 'HOA Hold', 'Customer Hold', 'Site Survey Hold',
    'Engineering Hold', 'Permitting Hold', 'Intake Hold', 'Welcome Call Hold'
  ];
  
  for (const type of holdTypes) {
    if (reasonStr.toLowerCase().includes(type.toLowerCase())) {
      return type;
    }
  }
  
  let cleaned = reasonStr
    .replace(/Placeholder:\/\/[^\s]+/gi, '')
    .replace(/https?:\/\/[^\s]+/gi, '')
    .replace(/Related Project:?\s*/gi, '')
    .replace(/\bby:[\s\S]*$/gi, '')
    .replace(/\s*-\s*\d+\s*$/g, '')
    .replace(/^\d+\s*[-:]\s*/g, '')
    .split('\n')[0]
    .trim();
  
  const projectOnMatch = cleaned.match(/^Project on\s+(.+?)(?:\s+Hold)?$/i);
  if (projectOnMatch) {
    return `${projectOnMatch[1]} Hold`;
  }
  
  if (/^\d+$/.test(cleaned)) {
    return 'Project on hold';
  }
  
  if (!cleaned) {
    return 'Project on hold';
  }
  
  return cleaned;
}

/**
 * Get the current milestone name for a project
 */
export function getCurrentMilestoneName(project: QuickbaseProject, milestoneConfig: any): string {
  // This would need to be implemented based on your milestone logic
  // For now, return a placeholder
  return 'Unknown Milestone';
}

/**
 * Get project status with additional context
 */
export function getProjectStatus(project: QuickbaseProject): {
  status: string;
  isOnHold: boolean;
  holdDuration: number;
  age: number;
  priority: 'high' | 'medium' | 'low';
} {
  const status = project[PROJECT_FIELDS.PROJECT_STATUS]?.value || 'Unknown';
  const isOnHold = isProjectOnHold(project);
  const holdDuration = getHoldDuration(project);
  const age = getProjectAge(project);
  
  // Determine priority based on various factors
  let priority: 'high' | 'medium' | 'low' = 'low';
  if (isOnHold && holdDuration > 14) {
    priority = 'high';
  } else if (isOnHold && holdDuration > 7) {
    priority = 'medium';
  } else if (age > 90) {
    priority = 'medium';
  }
  
  return {
    status,
    isOnHold,
    holdDuration,
    age,
    priority
  };
}

/**
 * Get project financial summary
 */
export function getProjectFinancialSummary(project: QuickbaseProject): {
  systemPrice: number;
  commission: number;
  revenueAtRisk: number;
  monthlyRevenue: number;
} {
  const systemPrice = parseFloat(project[PROJECT_FIELDS.SYSTEM_PRICE]?.value || '0');
  const commission = systemPrice * 0.03; // 3% commission
  const revenueAtRisk = systemPrice;
  const monthlyRevenue = systemPrice / (20 * 12); // Assuming 20-year loan
  
  return {
    systemPrice,
    commission,
    revenueAtRisk,
    monthlyRevenue
  };
}

/**
 * Check if project is high value (over $100K)
 */
export function isHighValueProject(project: QuickbaseProject): boolean {
  const systemPrice = parseFloat(project[PROJECT_FIELDS.SYSTEM_PRICE]?.value || '0');
  return systemPrice >= 100000;
}

/**
 * Check if project is at risk of cancellation
 */
export function isProjectAtRisk(project: QuickbaseProject): boolean {
  const isOnHold = isProjectOnHold(project);
  const holdDuration = getHoldDuration(project);
  const age = getProjectAge(project);
  
  // High risk if on hold for more than 30 days or project is older than 120 days
  return (isOnHold && holdDuration > 30) || age > 120;
}

/**
 * Get project urgency score (0-100)
 */
export function getProjectUrgencyScore(project: QuickbaseProject): number {
  let score = 0;
  
  const isOnHold = isProjectOnHold(project);
  const holdDuration = getHoldDuration(project);
  const age = getProjectAge(project);
  const isHighValue = isHighValueProject(project);
  
  // Base score from hold duration
  if (isOnHold) {
    score += Math.min(holdDuration * 2, 50); // Max 50 points for hold duration
  }
  
  // Age factor
  if (age > 90) {
    score += Math.min((age - 90) * 0.5, 30); // Max 30 points for age
  }
  
  // High value factor
  if (isHighValue) {
    score += 20; // 20 points for high value
  }
  
  return Math.min(score, 100);
}
