// lib/analytics/basicHoldStats.ts
import { QuickbaseProject, Project } from '@/lib/types/project';
import { PROJECT_FIELDS } from '@/lib/constants/fieldIds';
import { parseHoldReason, getHoldDuration } from '@/lib/utils/projectHelpers';
import { getProjectsForUser } from '@/lib/quickbase/queries';
import { calculateFinancialImpact } from '@/lib/utils/financialCalculations';

export interface HoldCategoryStats {
  category: string;
  count: number;
  avgDurationDays: number;
  totalRevenueAtRisk: number;
  totalCommissionImpact: number;
  sampleSize: number;
  comparisonToAverage?: {
    companyAvgDuration: number;
    companyAvgRevenueAtRisk: number;
  };
}

export interface BasicHoldStats {
  totalProjectsOnHold: number;
  totalRevenueAtRisk: number;
  totalCommissionImpact: number;
  averageHoldDurationDays: number;
  holdCategories: HoldCategoryStats[];
  overallComparisonToCompany?: {
    companyAvgDuration: number;
    companyAvgRevenueAtRisk: number;
  };
}

/**
 * Aggregates basic hold statistics for a user or team.
 * This function does NOT use AI, it's a quick win for immediate value.
 */
export async function getBasicHoldStatistics(
  userId: string,
  role: string,
  timeRange: string, // e.g., 'ytd', 'last_month', 'custom'
  customDateRange?: { startDate: string; endDate: string }
): Promise<BasicHoldStats> {
  // In a real app, you'd filter projects by timeRange and customDateRange
  // For simplicity, we'll fetch all projects on hold for the user/role for now.
  const allProjects = await getProjectsForUser(userId, role);
  const projectsOnHold = allProjects.filter(p => p[PROJECT_FIELDS.ON_HOLD]?.value === true);

  let totalProjectsOnHold = 0;
  let totalRevenueAtRisk = 0;
  let totalCommissionImpact = 0;
  let totalHoldDurationDays = 0;
  const categoryMap = new Map<string, { count: number; totalDuration: number; totalRevenue: number; totalCommission: number; projects: QuickbaseProject[] }>();

  for (const project of projectsOnHold) {
    totalProjectsOnHold++;
    const holdReason = project[PROJECT_FIELDS.HOLD_REASON]?.value || 'Unknown';
    const category = parseHoldReason(holdReason);
    const holdDuration = getHoldDuration(project);
    const systemPrice = parseFloat(project[PROJECT_FIELDS.SYSTEM_PRICE]?.value || '0');

    const financial = calculateFinancialImpact(systemPrice);

    totalHoldDurationDays += holdDuration;
    totalRevenueAtRisk += financial.revenueAtRisk;
    totalCommissionImpact += financial.commissionImpact;

    if (!categoryMap.has(category)) {
      categoryMap.set(category, { count: 0, totalDuration: 0, totalRevenue: 0, totalCommission: 0, projects: [] });
    }
    const entry = categoryMap.get(category)!;
    entry.count++;
    entry.totalDuration += holdDuration;
    entry.totalRevenue += financial.revenueAtRisk;
    entry.totalCommission += financial.commissionImpact;
    entry.projects.push(project);
  }

  const holdCategories: HoldCategoryStats[] = Array.from(categoryMap.entries()).map(([category, data]) => ({
    category,
    count: data.count,
    avgDurationDays: data.count > 0 ? data.totalDuration / data.count : 0,
    totalRevenueAtRisk: data.totalRevenue,
    totalCommissionImpact: data.totalCommission,
    sampleSize: data.count,
    // Add comparison to company average here if available
  }));

  const averageHoldDurationDays = totalProjectsOnHold > 0 ? totalHoldDurationDays / totalProjectsOnHold : 0;

  // Placeholder for company-wide averages (would come from a separate aggregation)
  const companyAvgDuration = 10; // Example
  const companyAvgRevenueAtRisk = 500000; // Example

  return {
    totalProjectsOnHold,
    totalRevenueAtRisk,
    totalCommissionImpact,
    averageHoldDurationDays,
    holdCategories: holdCategories.sort((a, b) => b.totalRevenueAtRisk - a.totalRevenueAtRisk), // Sort by highest revenue at risk
    overallComparisonToCompany: {
      companyAvgDuration,
      companyAvgRevenueAtRisk,
    },
  };
}