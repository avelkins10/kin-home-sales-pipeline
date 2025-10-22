// lib/utils/financialCalculations.ts
export interface FinancialImpact {
  revenueAtRisk: number;
  commissionImpact: number;
  monthlyRevenue: number;
  annualRevenue: number;
}

/**
 * Calculate financial impact for a project
 */
export function calculateFinancialImpact(systemPrice: number): FinancialImpact {
  // Commission is typically 3% of system price
  const commissionRate = 0.03;
  const commissionImpact = systemPrice * commissionRate;
  
  // Revenue at risk is the full system price
  const revenueAtRisk = systemPrice;
  
  // Monthly revenue (assuming 20-year loan)
  const monthlyRevenue = systemPrice / (20 * 12);
  
  // Annual revenue
  const annualRevenue = systemPrice / 20;
  
  return {
    revenueAtRisk,
    commissionImpact,
    monthlyRevenue,
    annualRevenue
  };
}

/**
 * Calculate total financial impact for multiple projects
 */
export function calculateTotalFinancialImpact(projects: Array<{ systemPrice: number }>): FinancialImpact {
  let totalRevenueAtRisk = 0;
  let totalCommissionImpact = 0;
  let totalMonthlyRevenue = 0;
  let totalAnnualRevenue = 0;
  
  for (const project of projects) {
    const impact = calculateFinancialImpact(project.systemPrice);
    totalRevenueAtRisk += impact.revenueAtRisk;
    totalCommissionImpact += impact.commissionImpact;
    totalMonthlyRevenue += impact.monthlyRevenue;
    totalAnnualRevenue += impact.annualRevenue;
  }
  
  return {
    revenueAtRisk: totalRevenueAtRisk,
    commissionImpact: totalCommissionImpact,
    monthlyRevenue: totalMonthlyRevenue,
    annualRevenue: totalAnnualRevenue
  };
}

/**
 * Calculate ROI for an improvement action
 */
export function calculateROI(
  investment: number,
  expectedReturn: number,
  timeframe: number // in months
): {
  roi: number;
  monthlyROI: number;
  paybackPeriod: number; // in months
} {
  const roi = ((expectedReturn - investment) / investment) * 100;
  const monthlyROI = roi / timeframe;
  const paybackPeriod = investment / (expectedReturn / timeframe);
  
  return {
    roi,
    monthlyROI,
    paybackPeriod
  };
}

/**
 * Calculate commission impact for different scenarios
 */
export function calculateCommissionScenarios(
  systemPrice: number,
  scenarios: Array<{ name: string; probability: number; impact: number }>
): Array<{ name: string; probability: number; impact: number; expectedValue: number }> {
  const baseCommission = systemPrice * 0.03;
  
  return scenarios.map(scenario => ({
    ...scenario,
    expectedValue: baseCommission * scenario.impact * scenario.probability
  }));
}
