// lib/quickbase/queries.ts
import 'server-only'
// Server-only module. Do not import from client components.
export const __isServerOnly = true as const

// This module must not be imported by client components
import { qbClient } from './client';
import { logError } from '@/lib/logging/logger';
import { PROJECT_FIELDS } from '@/lib/constants/fieldIds';

// Quickbase table IDs
const QB_TABLE_PROJECTS = process.env.QUICKBASE_TABLE_PROJECTS || 'br9kwm8na';

// Shared role scoping helper
export function buildRoleClause(userId: string, role: string): string {
  console.log('[buildRoleClause] Building clause for:', { userId, role });

  const userIds = userId.split(',').map(id => id.trim());

  const buildClause = (fieldId: number, ids: string[]) => {
    if (ids.length === 1) {
      return `{${fieldId}.EX.'${ids[0]}'}`;
    }
    // Multiple IDs joined with OR and proper spacing
    return ids.map(id => `{${fieldId}.EX.'${id}'}`).join(' OR ');
  };

  let clause: string;
  switch (role) {
    case 'super_admin':
    case 'regional':
    case 'office_leader':
      // These roles see all projects, no user filter
      clause = '{3.GT.0}'; // Record ID > 0 (matches all records)
      console.log('[buildRoleClause] Admin role detected, returning all-projects clause:', clause);
      break;
    case 'closer':
      clause = buildClause(PROJECT_FIELDS.CLOSER_ID, userIds);
      console.log('[buildRoleClause] Closer role, filtering by closer ID:', clause);
      break;
    case 'setter':
      clause = buildClause(PROJECT_FIELDS.SETTER_ID, userIds);
      console.log('[buildRoleClause] Setter role, filtering by setter ID:', clause);
      break;
    case 'coordinator':
      clause = buildClause(PROJECT_FIELDS.PROJECT_COORDINATOR_ID, userIds);
      console.log('[buildRoleClause] Coordinator role, filtering by coordinator ID:', clause);
      break;
    default:
      clause = buildClause(PROJECT_FIELDS.CLOSER_ID, userIds);
      console.log('[buildRoleClause] Unknown role, defaulting to closer filter:', clause);
      break;
  }

  return clause;
}

export async function getProjectsForUser(userId: string, role: string, view?: string, search?: string) {
  console.log('[getProjectsForUser] START - userId:', userId, 'role:', role, 'view:', view, 'search:', search);

  // Build role-based where clause using shared helper
  const roleClause = buildRoleClause(userId, role);

  // Build view-based filter
  const viewFilter = buildViewFilter(view);

  // Build search filter
  const searchFilter = buildSearchFilter(search);

  // Combine all filters with proper spacing
  let whereClause = roleClause;
  if (viewFilter) {
    whereClause = `(${whereClause}) AND ${viewFilter}`;
  }
  if (searchFilter) {
    whereClause = `(${whereClause}) AND ${searchFilter}`;
  }

  console.log('[getProjectsForUser] Final WHERE clause:', whereClause);

  // Determine sort order based on view
  const sortBy = getSortOrder(view);

  try {
    console.log('[getProjectsForUser] Querying QuickBase table:', QB_TABLE_PROJECTS);
    const result = await qbClient.queryRecords({
      from: QB_TABLE_PROJECTS,
      select: [
        PROJECT_FIELDS.RECORD_ID,
        PROJECT_FIELDS.PROJECT_ID,
        PROJECT_FIELDS.CUSTOMER_NAME,
        PROJECT_FIELDS.CUSTOMER_ADDRESS,
        PROJECT_FIELDS.CUSTOMER_PHONE,
        PROJECT_FIELDS.PROJECT_STATUS,
        PROJECT_FIELDS.ON_HOLD,
        PROJECT_FIELDS.HOLD_REASON,
        PROJECT_FIELDS.BLOCK_REASON,
        PROJECT_FIELDS.DATE_ON_HOLD,
        PROJECT_FIELDS.PROJECT_PRIORITY,
        PROJECT_FIELDS.SALES_OFFICE,
        PROJECT_FIELDS.SALES_DATE,
        PROJECT_FIELDS.PROJECT_AGE,
        PROJECT_FIELDS.SYSTEM_SIZE_KW,
        PROJECT_FIELDS.SYSTEM_PRICE,
        // PPW fields
        2292, // soldGross PPW
        2480, // commissionable PPW
        // Milestone dates for current stage calculation
        PROJECT_FIELDS.INTAKE_INSTALL_DATE_TENTATIVE,
        PROJECT_FIELDS.SURVEY_SUBMITTED,
        PROJECT_FIELDS.SURVEY_APPROVED,
        PROJECT_FIELDS.PREDESIGN_APPROVED,
        PROJECT_FIELDS.DESIGN_COMPLETED,
        PROJECT_FIELDS.CAD_DESIGN_APPROVED,
        PROJECT_FIELDS.PERMIT_SUBMITTED,
        PROJECT_FIELDS.PERMIT_APPROVED,
        PROJECT_FIELDS.HOA_APPLICATION_APPROVED,
        PROJECT_FIELDS.NEM_SUBMITTED,
        PROJECT_FIELDS.NEM_APPROVED,
        PROJECT_FIELDS.INSTALL_SCHEDULED_DATE_CAPTURE,
        PROJECT_FIELDS.INSTALL_SCHEDULED_START_DATE,
        PROJECT_FIELDS.ESTIMATED_INSTALL_DATE,
        PROJECT_FIELDS.INSTALL_STARTED_DATE,
        PROJECT_FIELDS.INSTALLATION_COMPLETED_AT,
        PROJECT_FIELDS.READY_FOR_COMMISSION,
        PROJECT_FIELDS.INSTALL_COMPLETED_DATE,
        PROJECT_FIELDS.INSPECTION_SCHEDULED_DATE,
        PROJECT_FIELDS.PASSING_INSPECTION_COMPLETED,
        PROJECT_FIELDS.PTO_SUBMITTED,
        PROJECT_FIELDS.PTO_APPROVED,
      ],
      where: whereClause,
      sortBy,
    });

    console.log('[getProjectsForUser] QuickBase response:', {
      totalRecords: result.data?.length || 0,
      metadata: result.metadata
    });

    if (!result.data || result.data.length === 0) {
      console.warn('[getProjectsForUser] WARNING: No projects returned from QuickBase');
    }

    return result.data;
  } catch (error) {
    console.error('[getProjectsForUser] ERROR:', error);
    logError('Error fetching projects', error as Error);
    return [];
  }
}

// Helper function to build view-based filter
function buildViewFilter(view?: string): string {
  if (!view || view === 'all') return '';
  
  switch (view) {
    case 'active':
      return `{${PROJECT_FIELDS.PROJECT_STATUS}.CT.'Active'} AND {${PROJECT_FIELDS.ON_HOLD}.EX.'No'}`;
    
    case 'on-hold':
      return `{${PROJECT_FIELDS.ON_HOLD}.EX.'Yes'}`;
    
    case 'install-ready':
      return `{${PROJECT_FIELDS.NEM_APPROVED}.EX.'Yes'} AND {${PROJECT_FIELDS.PERMIT_APPROVED}.EX.'Yes'} AND {${PROJECT_FIELDS.INSTALL_SCHEDULED_DATE_CAPTURE}.EX.''}`;
    
    case 'install-scheduled':
      return `{${PROJECT_FIELDS.INSTALL_SCHEDULED_DATE_CAPTURE}.XEX.''} AND {${PROJECT_FIELDS.INSTALL_COMPLETED_DATE}.EX.''}`;
    
    case 'install-completed':
      return `{${PROJECT_FIELDS.INSTALL_COMPLETED_DATE}.XEX.''} AND {${PROJECT_FIELDS.PTO_APPROVED}.EX.''}`;
    
    case 'pending-cancel':
      return `{${PROJECT_FIELDS.PROJECT_STATUS}.CT.'Pending Cancel'}`;
    
    case 'cancelled':
      return `{${PROJECT_FIELDS.PROJECT_STATUS}.CT.'Cancel'} AND {${PROJECT_FIELDS.PROJECT_STATUS}.XCT.'Pending'}`;
    
    case 'needs-attention':
      // Projects older than 90 days OR on hold for more than 7 days
      const sevenDaysAgo = new Date(Date.now() - 7 * 24 * 60 * 60 * 1000).toISOString().split('T')[0];
      return `({${PROJECT_FIELDS.PROJECT_AGE}.GT.90} OR ({${PROJECT_FIELDS.ON_HOLD}.EX.'Yes'} AND {${PROJECT_FIELDS.DATE_ON_HOLD}.BF.'${sevenDaysAgo}'}))`;
    
    default:
      return '';
  }
}

// Helper function to build search filter
export function sanitizeQbLiteral(input?: string): string {
  if (!input) return '';
  let s = input.trim();
  if (s.length > 100) s = s.slice(0, 100);
  s = s.replace(/'/g, "''");
  s = s.replace(/[\n\r%]/g, ' ');
  return s;
}

export function buildSearchFilter(search?: string): string {
  if (!search) return '';
  const searchTerm = sanitizeQbLiteral(search);
  if (!searchTerm) return '';
  return `({${PROJECT_FIELDS.CUSTOMER_NAME}.CT.'${searchTerm}'} OR {${PROJECT_FIELDS.PROJECT_ID}.CT.'${searchTerm}'})`;
}

// Helper function to get sort order based on view
function getSortOrder(view?: string): { fieldId: number; order: 'ASC' | 'DESC' }[] {
  switch (view) {
    case 'on-hold':
      return [{ fieldId: PROJECT_FIELDS.DATE_ON_HOLD, order: 'ASC' as const }]; // Oldest first
    case 'install-scheduled':
      return [{ fieldId: PROJECT_FIELDS.INSTALL_SCHEDULED_DATE_CAPTURE, order: 'ASC' as const }];
    case 'install-completed':
      return [{ fieldId: PROJECT_FIELDS.INSTALL_COMPLETED_DATE, order: 'DESC' as const }];
    case 'needs-attention':
      return [{ fieldId: PROJECT_FIELDS.PROJECT_AGE, order: 'DESC' as const }]; // Oldest first
    default:
      return [
        { fieldId: PROJECT_FIELDS.ON_HOLD, order: 'DESC' as const }, // Holds first
        { fieldId: PROJECT_FIELDS.PROJECT_PRIORITY, order: 'ASC' as const }, // Then by priority
        { fieldId: PROJECT_FIELDS.SALES_DATE, order: 'DESC' as const }, // Then newest
      ];
  }
}

export async function getDashboardMetrics(userId: string, role: string) {
  const projects = await getProjectsForUser(userId, role);
  
  const now = new Date();
  const oneWeekAgo = new Date(now.getTime() - 7 * 24 * 60 * 60 * 1000);
  const currentMonth = now.getMonth();
  const currentYear = now.getFullYear();
  
  // Calculate installs this week
  const installsThisWeek = projects.filter((project: any) => {
    const installDate = project[PROJECT_FIELDS.INSTALL_COMPLETED_DATE]?.value;
    if (!installDate) return false;
    const install = new Date(installDate);
    return install >= oneWeekAgo && install <= now;
  }).length;
  
  // Calculate active projects (not on hold)
  const activeProjects = projects.filter((project: any) => {
    const status = project[PROJECT_FIELDS.PROJECT_STATUS]?.value || '';
    const onHold = project[PROJECT_FIELDS.ON_HOLD]?.value;
    return status.includes('Active') && onHold !== 'Yes';
  }).length;
  
  // Calculate on hold projects
  const onHold = projects.filter((project: any) => {
    return project[PROJECT_FIELDS.ON_HOLD]?.value === 'Yes';
  }).length;
  
  // Calculate installs this month
  const installsThisMonth = projects.filter((project: any) => {
    const installDate = project[PROJECT_FIELDS.INSTALL_COMPLETED_DATE]?.value;
    if (!installDate) return false;
    const install = new Date(installDate);
    return install.getMonth() === currentMonth && install.getFullYear() === currentYear;
  }).length;
  
  // Calculate hold breakdown
  const holdReasons = projects
    .filter((project: any) => project[PROJECT_FIELDS.ON_HOLD]?.value === 'Yes')
    .map((project: any) => project[PROJECT_FIELDS.HOLD_REASON]?.value || '')
    .filter((reason: string) => reason);
  
  const holdBreakdownMap = new Map<string, number>();
  holdReasons.forEach((reason: string) => {
    // Extract type from reason (Finance, Roof, Customer, Permit, HOA, etc.)
    const type = reason.split(' ')[0]; // Take first word as type
    holdBreakdownMap.set(type, (holdBreakdownMap.get(type) || 0) + 1);
  });
  
  const holdBreakdown = Array.from(holdBreakdownMap.entries())
    .map(([type, count]) => `${count} ${type}`)
    .join(', ');
  
  return {
    installsThisWeek,
    activeProjects,
    onHold,
    holdBreakdown,
    installsThisMonth,
  };
}

export async function getDashboardMetricsOptimized(userId: string, role: string) {
  console.log('[getDashboardMetricsOptimized] START - userId:', userId, 'role:', role);
  const startTime = Date.now();

  // Use shared role scoping helper for consistency
  const whereClause = buildRoleClause(userId, role);
  console.log('[getDashboardMetricsOptimized] WHERE clause:', whereClause);

  const userProjects = await qbClient.queryRecords({
    from: QB_TABLE_PROJECTS,
    select: [
      PROJECT_FIELDS.PROJECT_STATUS,
      PROJECT_FIELDS.ON_HOLD,
      PROJECT_FIELDS.HOLD_REASON,
      PROJECT_FIELDS.INSTALL_COMPLETED_DATE,
      PROJECT_FIELDS.SALES_DATE,
    ],
    where: whereClause,
  });

  const data = userProjects.data || [];
  const now = new Date();
  const oneWeekAgo = new Date(now.getTime() - 7 * 24 * 60 * 60 * 1000);
  const currentMonth = now.getMonth();
  const currentYear = now.getFullYear();

  const installsThisWeek = data.filter((p: any) => {
    const installDate = p[PROJECT_FIELDS.INSTALL_COMPLETED_DATE]?.value;
    if (!installDate) return false;
    const d = new Date(installDate);
    return d >= oneWeekAgo && d <= now;
  }).length;

  const activeProjects = data.filter((p: any) => {
    const status = p[PROJECT_FIELDS.PROJECT_STATUS]?.value || '';
    const onHold = p[PROJECT_FIELDS.ON_HOLD]?.value;
    return status.includes('Active') && onHold !== 'Yes';
  }).length;

  const onHold = data.filter((p: any) => p[PROJECT_FIELDS.ON_HOLD]?.value === 'Yes').length;

  const installsThisMonth = data.filter((p: any) => {
    const installDate = p[PROJECT_FIELDS.INSTALL_COMPLETED_DATE]?.value;
    if (!installDate) return false;
    const d = new Date(installDate);
    return d.getMonth() === currentMonth && d.getFullYear() === currentYear;
  }).length;

  const holdReasons = data
    .filter((p: any) => p[PROJECT_FIELDS.ON_HOLD]?.value === 'Yes')
    .map((p: any) => p[PROJECT_FIELDS.HOLD_REASON]?.value || '')
    .filter((r: string) => r);
  const holdBreakdownMap = new Map<string, number>();
  holdReasons.forEach((reason: string) => {
    const type = reason.split(' ')[0];
    holdBreakdownMap.set(type, (holdBreakdownMap.get(type) || 0) + 1);
  });
  const holdBreakdown = Array.from(holdBreakdownMap.entries())
    .map(([type, count]) => `${count} ${type}`)
    .join(', ');

  // Log performance metrics
  const duration = Date.now() - startTime;
  if (process.env.NODE_ENV === 'development') {
    console.log(`[METRICS] Optimized query completed in ${duration}ms for ${data.length} records`);
  }

  return { installsThisWeek, activeProjects, onHold, installsThisMonth, holdBreakdown };
}

export const __test__ = {
  sanitizeQbLiteral,
  buildSearchFilter,
};

export async function getUrgentProjects(userId: string, role: string) {
  console.log('[getUrgentProjects] START - userId:', userId, 'role:', role);
  const holds = await getProjectsOnHold(userId, role);
  console.log('[getUrgentProjects] Retrieved', holds?.length || 0, 'projects on hold');
  const sevenDaysAgo = new Date(Date.now() - 7 * 24 * 60 * 60 * 1000);
  
  const urgentProjects = holds
    .filter((project: any) => {
      const holdDate = project[PROJECT_FIELDS.DATE_ON_HOLD]?.value;
      if (!holdDate) return false;
      return new Date(holdDate) < sevenDaysAgo;
    })
    .map((project: any) => {
      const holdDate = new Date(project[PROJECT_FIELDS.DATE_ON_HOLD]?.value);
      const daysOnHold = Math.floor((Date.now() - holdDate.getTime()) / (1000 * 60 * 60 * 24));
      
      return {
        recordId: project[PROJECT_FIELDS.RECORD_ID]?.value,
        customerName: project[PROJECT_FIELDS.CUSTOMER_NAME]?.value,
        daysOnHold,
        holdReason: project[PROJECT_FIELDS.HOLD_REASON]?.value,
      };
    })
    .sort((a: any, b: any) => b.daysOnHold - a.daysOnHold); // Most urgent first
  
  return urgentProjects;
}

export async function getRecentProjects(userId: string, role: string) {
  const projects = await getProjectsForUser(userId, role);
  
  const recentProjects = projects
    .filter((project: any) => {
      const status = project[PROJECT_FIELDS.PROJECT_STATUS]?.value || '';
      const onHold = project[PROJECT_FIELDS.ON_HOLD]?.value;
      return status.includes('Active') && onHold !== 'Yes';
    })
    .sort((a: any, b: any) => {
      const dateA = new Date(a[PROJECT_FIELDS.SALES_DATE]?.value || 0);
      const dateB = new Date(b[PROJECT_FIELDS.SALES_DATE]?.value || 0);
      return dateB.getTime() - dateA.getTime(); // Newest first
    })
    .slice(0, 5); // Take first 5
  
  return recentProjects;
}

export async function getProjectById(recordId: number) {
  const result = await qbClient.queryRecords({
    from: QB_TABLE_PROJECTS,
    select: Object.values(PROJECT_FIELDS), // All 92 fields
    where: `{${PROJECT_FIELDS.RECORD_ID}.EX.${recordId}}`,
  });

  return result.data[0] || null;
}

export async function getProjectsOnHold(userId: string, role: string) {
  // Use shared role scoping helper and consistent ON_HOLD value
  const roleClause = buildRoleClause(userId, role);
  const whereClause = `(${roleClause}) AND {${PROJECT_FIELDS.ON_HOLD}.EX.'Yes'}`;

  const result = await qbClient.queryRecords({
    from: QB_TABLE_PROJECTS,
    select: [
      PROJECT_FIELDS.RECORD_ID,
      PROJECT_FIELDS.PROJECT_ID,
      PROJECT_FIELDS.CUSTOMER_NAME,
      PROJECT_FIELDS.CUSTOMER_PHONE,
      PROJECT_FIELDS.ON_HOLD,
      PROJECT_FIELDS.HOLD_REASON,
      PROJECT_FIELDS.BLOCK_REASON,
      PROJECT_FIELDS.DATE_ON_HOLD,
      PROJECT_FIELDS.USER_PLACED_ON_HOLD,
      PROJECT_FIELDS.PROJECT_PRIORITY,
    ],
    where: whereClause,
    sortBy: [
      { fieldId: PROJECT_FIELDS.DATE_ON_HOLD, order: 'ASC' }, // Oldest holds first
    ],
  });

  return result.data;
}

export async function updateProject(recordId: number, updates: any) {
  return qbClient.updateRecord({
    to: QB_TABLE_PROJECTS,
    data: [
      {
        [PROJECT_FIELDS.RECORD_ID]: { value: recordId },
        ...updates,
      },
    ],
  });
}
