// lib/quickbase/queries.ts
/**
 * ⚠️ QUICKBASE QUERIES MODULE
 *
 * This module handles all QuickBase data fetching.
 * Authorization logic is centralized in lib/auth/projectAuthorization.ts
 *
 * DO NOT add authorization logic here - import from projectAuthorization instead.
 */
import 'server-only'
// Server-only module. Do not import from client components.
export const __isServerOnly = true as const

// This module must not be imported by client components
import { qbClient } from './client';
import { logError, logInfo, logWarn } from '@/lib/logging/logger';
import { PROJECT_FIELDS } from '@/lib/constants/fieldIds';
import { ADDER_FIELDS } from '@/lib/constants/adderFieldIds';
import { sql } from '@/lib/db/client';
import { buildProjectAccessClause } from '@/lib/auth/projectAuthorization';
import type { TeamActivityItem, TeamActivityType } from '@/lib/types/dashboard';
import { isManagerRole } from '@/lib/utils/role-helpers';
import type { MetricsScope, TeamMemberCommission, TeamMemberBuckets } from '@/lib/types/dashboard';
import { parseQuickbaseDate } from '@/lib/utils/date-helpers';

// In-process cache for commission/team bucket calculations
interface CacheEntry<T> {
  data: T;
  timestamp: number;
  ttl: number;
}

class InProcessCache {
  private cache = new Map<string, CacheEntry<any>>();
  private readonly DEFAULT_TTL = 30000; // 30 seconds

  set<T>(key: string, data: T, ttl: number = this.DEFAULT_TTL): void {
    this.cache.set(key, {
      data,
      timestamp: Date.now(),
      ttl,
    });
  }

  get<T>(key: string): T | null {
    const entry = this.cache.get(key);
    if (!entry) return null;

    const now = Date.now();
    if (now - entry.timestamp > entry.ttl) {
      this.cache.delete(key);
      return null;
    }

    return entry.data;
  }

  clear(): void {
    this.cache.clear();
  }

  // Clean up expired entries
  cleanup(): void {
    const now = Date.now();
    const entries = Array.from(this.cache.entries());
    for (const [key, entry] of entries) {
      if (now - entry.timestamp > entry.ttl) {
        this.cache.delete(key);
      }
    }
  }
}

const commissionCache = new InProcessCache();

// Quickbase table IDs
const QB_TABLE_PROJECTS = process.env.QUICKBASE_TABLE_PROJECTS || 'br9kwm8na';
const QB_TABLE_ADDERS = 'bsaycczmf';

// Helper function to get managed user IDs for team leads
async function getManagedUserIds(managerId: string): Promise<string[]> {
  console.log('[getManagedUserIds] Getting managed users for team lead:', managerId);

  try {
    const result = await sql`
      SELECT u.quickbase_user_id
      FROM user_hierarchies uh
      JOIN users u ON uh.user_id = u.id
      WHERE uh.manager_id = ${managerId}
      AND u.quickbase_user_id IS NOT NULL
    `;

    const managedUserIds = result.rows
      .map(row => row.quickbase_user_id)
      .filter(Boolean)
      .flatMap(id => id.split(',').map((subId: string) => subId.trim())); // Handle comma-separated IDs

    console.log('[getManagedUserIds] Found managed users:', managedUserIds);
    return managedUserIds;
  } catch (error) {
    console.error('[getManagedUserIds] Error:', error);
    logError('Failed to get managed user IDs', error as Error);
    return [];
  }
}

// Helper function to get user email from dashboard database
async function getUserEmail(userId: string): Promise<string | null> {
  // Input validation
  if (!userId || typeof userId !== 'string' || userId.trim() === '') {
    console.warn('[getUserEmail] Invalid userId provided:', typeof userId, userId);
    return null;
  }

  if (process.env.NODE_ENV !== 'production') {
    console.log('[getUserEmail] Getting email for user:', userId);
  }

  try {
    const result = await sql`
      SELECT email
      FROM users
      WHERE id = ${userId}
      AND email IS NOT NULL
    `;

    if (result.rows.length === 0) {
      if (process.env.NODE_ENV !== 'production') {
        console.warn('[getUserEmail] No email found for user:', userId);
      }
      return null;
    }

    const email = result.rows[0].email;
    if (process.env.NODE_ENV !== 'production') {
      console.log('[getUserEmail] Found email for user:', userId);
    }
    return email;
  } catch (error) {
    console.error('[getUserEmail] Error:', error);
    logError('Failed to get user email', error as Error);
    return null;
  }
}

// Helper function to get managed user emails for team leads
async function getManagedUserEmails(managerId: string): Promise<string[]> {
  // Input validation
  if (!managerId || typeof managerId !== 'string' || managerId.trim() === '') {
    console.warn('[getManagedUserEmails] Invalid managerId provided:', typeof managerId, managerId);
    return [];
  }

  if (process.env.NODE_ENV !== 'production') {
    console.log('[getManagedUserEmails] Getting managed user emails for team lead:', managerId);
  }

  try {
    const result = await sql`
      SELECT u.email
      FROM user_hierarchies uh
      JOIN users u ON uh.user_id = u.id
      WHERE uh.manager_id = ${managerId}
      AND u.email IS NOT NULL
    `;

    const managedEmails = result.rows
      .map(row => row.email)
      .filter(Boolean);

    if (process.env.NODE_ENV !== 'production') {
      console.log('[getManagedUserEmails] Found', managedEmails.length, 'managed user emails for team lead:', managerId);
    }
    return managedEmails;
  } catch (error) {
    console.error('[getManagedUserEmails] Error:', error);
    logError('Failed to get managed user emails', error as Error);
    return [];
  }
}

// Helper function to get assigned office IDs for managers
// Returns QuickBase Record IDs (Field 810) for stable filtering that survives office name changes
async function getAssignedOffices(userId: string): Promise<number[]> {
  // Input validation
  if (!userId || typeof userId !== 'string' || userId.trim() === '') {
    console.warn('[getAssignedOffices] Invalid userId provided:', typeof userId, userId);
    return [];
  }

  if (process.env.NODE_ENV !== 'production') {
    console.log('[getAssignedOffices] Getting assigned offices for user:', userId);
  }

  try {
    const result = await sql`
      SELECT o.quickbase_office_id, o.name as office_name
      FROM office_assignments oa
      JOIN offices o ON oa.office_name = o.name
      WHERE oa.user_id = ${userId}
        AND o.quickbase_office_id IS NOT NULL
    `;

    const assignedOfficeIds = result.rows.map(row => row.quickbase_office_id);
    if (process.env.NODE_ENV !== 'production') {
      logInfo('[OFFICE_ASSIGNMENT_RESOLUTION] Resolved offices', {
        userId,
        officeCount: assignedOfficeIds.length,
        officeIds: assignedOfficeIds,
        officeNames: result.rows.map(r => r.office_name)
      });
    } else {
      logInfo('[OFFICE_ASSIGNMENT_RESOLUTION] Resolved offices', { userId, officeCount: assignedOfficeIds.length });
    }
    return assignedOfficeIds;
  } catch (error) {
    console.error('[getAssignedOffices] Error:', error);
    logError('Failed to get assigned offices', error as Error, { userId });
    return [];
  }
}

/**
 * Wrapper function for backward compatibility
 * Delegates to centralized buildProjectAccessClause from projectAuthorization module
 * @deprecated Use buildProjectAccessClause from @/lib/auth/projectAuthorization directly
 * This wrapper will be removed in a future release after one release cycle.
 */
export function buildRoleClause(
  userEmail: string | null,
  role: string,
  officeIds?: number[],
  managedEmails?: string[]
): string {
  // Log deprecation warning once per process start
  if (!(global as any).__buildRoleClauseDeprecationWarned) {
    console.warn(
      '[DEPRECATION WARNING] buildRoleClause() is deprecated and will be removed in a future release. ' +
      'Please use buildProjectAccessClause() from @/lib/auth/projectAuthorization directly.'
    );
    (global as any).__buildRoleClauseDeprecationWarned = true;
  }

  // Delegate to centralized authorization module
  return buildProjectAccessClause(userEmail, role, officeIds, managedEmails);
}

/**
 * Lean selector for list view - only essential fields for performance
 * @param officeIds - Office IDs to filter by (QuickBase Record IDs from Field 810). If not provided, will be fetched from office_assignments table for office-based roles.
 */
export async function getProjectsForUserList(userId: string, role: string, view?: string, search?: string, sort?: string, officeIds?: number[], memberEmail?: string, ownership?: string, officeFilter?: string, setterFilter?: string, closerFilter?: string, reqId?: string) {
  console.log('[getProjectsForUserList] START - userId:', userId, 'role:', role, 'view:', view, 'search:', search, 'sort:', sort, 'ownership:', ownership, 'officeFilter:', officeFilter, 'setterFilter:', setterFilter, 'closerFilter:', closerFilter, 'officeIds:', officeIds, 'reqId:', reqId);

  // Get user email for email-based filtering (needed for ownership filter)
  let userEmail: string | null = null;
  if (['closer', 'setter', 'coordinator'].includes(role) || isManagerRole(role)) {
    userEmail = await getUserEmail(userId);
  }

  // Get managed user emails for team leads
  let managedEmails: string[] | undefined;
  if (role === 'team_lead') {
    managedEmails = await getManagedUserEmails(userId);
  }

  // Get assigned office IDs for office-based roles if not provided
  let effectiveOfficeIds = officeIds;
  if (!effectiveOfficeIds && ['office_leader', 'area_director', 'divisional'].includes(role)) {
    effectiveOfficeIds = await getAssignedOffices(userId);
    logInfo('[PROJECT_QUERY] Fetched offices from database', { userId, role, officeCount: effectiveOfficeIds?.length || 0 });
  } else if (effectiveOfficeIds) {
    logInfo('[PROJECT_QUERY] Using provided offices (not fetching from DB)', { userId, role, officeCount: effectiveOfficeIds?.length || 0 });
  }

  // Check for empty office assignments for office-based roles
  if (['office_leader', 'area_director', 'divisional'].includes(role) && (!effectiveOfficeIds || effectiveOfficeIds.length === 0)) {
    logWarn('[OFFICE_ASSIGNMENT_RESOLUTION] No offices assigned to office-based role', { userId, role });
  }

  // Build role-based where clause using shared helper (now ID-based)
  const roleClause = buildProjectAccessClause(userEmail, role, effectiveOfficeIds, managedEmails, reqId);

  // Add member email filter if provided (for team member drill-down)
  let memberEmailFilter: string | undefined;
  if (memberEmail) {
    const sanitizedEmail = sanitizeQbLiteral(memberEmail);
    memberEmailFilter = `({${PROJECT_FIELDS.CLOSER_EMAIL}.EX.'${sanitizedEmail}'} OR {${PROJECT_FIELDS.SETTER_EMAIL}.EX.'${sanitizedEmail}'})`;
    if (process.env.NODE_ENV !== 'production') {
      console.log('[getProjectsForUserList] Filtering by team member email:', memberEmail);
    } else {
      console.log('[getProjectsForUserList] Filtering by team member email');
    }
  }

  // Add office filter if provided (for office-based filtering)
  let officeFilterClause: string | undefined;
  if (officeFilter) {
    const sanitizedOffice = sanitizeQbLiteral(officeFilter);
    officeFilterClause = `{${PROJECT_FIELDS.SALES_OFFICE}.EX.'${sanitizedOffice}'}`;
    if (process.env.NODE_ENV !== 'production') {
      console.log('[getProjectsForUserList] Filtering by office:', officeFilter);
    } else {
      console.log('[getProjectsForUserList] Filtering by office');
    }
  }

  // Add setter filter if provided
  let setterFilterClause: string | undefined;
  if (setterFilter) {
    const sanitizedSetter = sanitizeQbLiteral(setterFilter);
    setterFilterClause = `{${PROJECT_FIELDS.SETTER_EMAIL}.EX.'${sanitizedSetter}'}`;
    if (process.env.NODE_ENV !== 'production') {
      console.log('[getProjectsForUserList] Filtering by setter:', setterFilter);
    } else {
      console.log('[getProjectsForUserList] Filtering by setter');
    }
  }

  // Add closer filter if provided
  let closerFilterClause: string | undefined;
  if (closerFilter) {
    const sanitizedCloser = sanitizeQbLiteral(closerFilter);
    closerFilterClause = `{${PROJECT_FIELDS.CLOSER_EMAIL}.EX.'${sanitizedCloser}'}`;
    if (process.env.NODE_ENV !== 'production') {
      console.log('[getProjectsForUserList] Filtering by closer:', closerFilter);
    } else {
      console.log('[getProjectsForUserList] Filtering by closer');
    }
  }

  // Build view-based filter
  const viewFilter = buildViewFilter(view);

  // Build search filter
  const searchFilter = buildSearchFilter(search);

  // Build ownership filter
  const ownershipFilter = buildOwnershipFilter(userEmail, ownership);
  if (ownershipFilter) {
    console.log('[getProjectsForUserList] Applying ownership filter:', ownership);
  }

  // Combine all filters with proper parentheses for precedence
  let whereClause = roleClause;
  if (ownershipFilter) {
    whereClause = `(${whereClause}) AND (${ownershipFilter})`;
  }
  if (memberEmailFilter) {
    whereClause = `(${whereClause}) AND (${memberEmailFilter})`;
  }
  if (officeFilterClause) {
    whereClause = `(${whereClause}) AND (${officeFilterClause})`;
  }
  if (setterFilterClause) {
    whereClause = `(${whereClause}) AND (${setterFilterClause})`;
  }
  if (closerFilterClause) {
    whereClause = `(${whereClause}) AND (${closerFilterClause})`;
  }
  if (viewFilter) {
    whereClause = `(${whereClause}) AND (${viewFilter})`;
  }
  if (searchFilter) {
    whereClause = `(${whereClause}) AND (${searchFilter})`;
  }

  console.log('[getProjectsForUserList] Final WHERE clause:', whereClause);

  // Determine sort order based on sort param or view
  const sortBy = getSortOrder(view, sort);

  try {
    console.log('[getProjectsForUserList] Querying QuickBase table:', QB_TABLE_PROJECTS);
    const result = await qbClient.queryRecords({
      from: QB_TABLE_PROJECTS,
      select: [
        // Essential fields for list view only
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
        PROJECT_FIELDS.INTAKE_MISSING_ITEMS_COMBINED,
        PROJECT_FIELDS.PROJECT_PRIORITY,
        PROJECT_FIELDS.SALES_OFFICE,
        PROJECT_FIELDS.SALES_DATE,
        PROJECT_FIELDS.PROJECT_AGE,
        PROJECT_FIELDS.SYSTEM_SIZE_KW,
        PROJECT_FIELDS.SYSTEM_PRICE,
        PROJECT_FIELDS.CLOSER_NAME,
        PROJECT_FIELDS.SETTER_NAME,
        PROJECT_FIELDS.CLOSER_EMAIL,
        PROJECT_FIELDS.SETTER_EMAIL,
        // PPW fields
        PROJECT_FIELDS.SOLD_GROSS_PPW, // soldGross PPW
        PROJECT_FIELDS.COMMISSIONABLE_PPW, // commissionable PPW
        // Intake approval fields for filter counts
        PROJECT_FIELDS.FINANCE_INTAKE_APPROVED,
        PROJECT_FIELDS.WEBHOOK_INTAKE_COMPLETE,
        // All milestone status and completion fields for accurate traffic lights
        // Intake
        PROJECT_FIELDS.INTAKE_STATUS, // Field 347 - Primary status indicator for intake
        PROJECT_FIELDS.INTAKE_COMPLETED_DATE,
        // Survey
        PROJECT_FIELDS.SURVEY_STATUS, // Field 162 - Primary status indicator for survey
        PROJECT_FIELDS.INTAKE_INSTALL_DATE_TENTATIVE,
        PROJECT_FIELDS.SURVEY_SUBMITTED,
        PROJECT_FIELDS.SURVEY_APPROVED,
        PROJECT_FIELDS.SITE_SURVEY_ARRIVY_SCHEDULED, // Survey scheduled date from Arrivy (for calendar)
        // Design
        PROJECT_FIELDS.DESIGN_STATUS, // Field 317 - Primary status indicator for design
        PROJECT_FIELDS.DESIGN_COMPLETED,
        PROJECT_FIELDS.CAD_DESIGN_APPROVED,
        PROJECT_FIELDS.ENGINEERING_COMPLETED,
        PROJECT_FIELDS.DESIGN_SLA_DEADLINE, // Field 2459 - SLA tracking for design urgency
        // Permitting
        PROJECT_FIELDS.PERMIT_STATUS, // Field 2095 - Primary status indicator for permitting
        PROJECT_FIELDS.NEM_SIGNATURES_SENT,
        PROJECT_FIELDS.NEM_SUBMITTED,
        PROJECT_FIELDS.NEM_APPROVED,
        PROJECT_FIELDS.PERMIT_SUBMITTED,
        PROJECT_FIELDS.PERMIT_APPROVED,
        PROJECT_FIELDS.HOA_APPLICATION_SUBMITTED,
        PROJECT_FIELDS.HOA_APPLICATION_APPROVED,
        // Installation
        PROJECT_FIELDS.INSTALL_SCHEDULED_START_DATE,
        PROJECT_FIELDS.INSTALL_SCHEDULED_DATE_CAPTURE,
        PROJECT_FIELDS.INSTALL_STARTED_DATE,
        PROJECT_FIELDS.INSTALL_COMPLETED_DATE,
        // Inspection
        PROJECT_FIELDS.INSPECTION_SCHEDULED_DATE,
        PROJECT_FIELDS.PASSING_INSPECTION_COMPLETED,
        // PTO
        PROJECT_FIELDS.PTO_STATUS, // Field 556 - Primary status indicator for PTO
        PROJECT_FIELDS.PTO_SUBMITTED,
        PROJECT_FIELDS.PTO_APPROVED,
      ],
      where: whereClause,
      sortBy,
      options: {
        top: 250 // Limit to first 250 projects for faster loading
      }
    });

    console.log('[getProjectsForUserList] QuickBase response:', {
      totalRecords: result.data?.length || 0,
      metadata: result.metadata
    });

    if (!result.data || result.data.length === 0) {
      console.warn('[getProjectsForUserList] WARNING: No projects returned from QuickBase');
    }

    return result.data;
  } catch (error) {
    console.error('[getProjectsForUserList] ERROR:', error);
    logError('Error fetching projects list', error as Error);
    return [];
  }
}

export async function getProjectsForUser(userId: string, role: string, view?: string, search?: string, sort?: string, officeIds?: number[]) {
  console.log('[getProjectsForUser] START - userId:', userId, 'role:', role, 'view:', view, 'search:', search, 'sort:', sort, 'officeIds:', officeIds);

  // Get user email for email-based filtering
  let userEmail: string | null = null;
  if (['closer', 'setter', 'coordinator'].includes(role)) {
    userEmail = await getUserEmail(userId);
  }

  // Get managed user emails for team leads
  let managedEmails: string[] | undefined;
  if (role === 'team_lead') {
    managedEmails = await getManagedUserEmails(userId);
  }

  // Get assigned office IDs for office-based roles if not provided
  let effectiveOfficeIds = officeIds;
  if (!effectiveOfficeIds && ['office_leader', 'area_director', 'divisional'].includes(role)) {
    effectiveOfficeIds = await getAssignedOffices(userId);
  }

  // Build role-based where clause using shared helper (now ID-based)
  const roleClause = buildProjectAccessClause(userEmail, role, effectiveOfficeIds, managedEmails);

  // Build view-based filter
  const viewFilter = buildViewFilter(view);

  // Build search filter
  const searchFilter = buildSearchFilter(search);

  // Combine all filters with proper parentheses for precedence
  let whereClause = roleClause;
  if (viewFilter) {
    whereClause = `(${whereClause}) AND (${viewFilter})`;
  }
  if (searchFilter) {
    whereClause = `(${whereClause}) AND (${searchFilter})`;
  }

  console.log('[getProjectsForUser] Final WHERE clause:', whereClause);

  // Determine sort order based on sort param or view
  const sortBy = getSortOrder(view, sort);

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
        PROJECT_FIELDS.INTAKE_MISSING_ITEMS_COMBINED,
        PROJECT_FIELDS.PROJECT_PRIORITY,
        PROJECT_FIELDS.SALES_OFFICE,
        PROJECT_FIELDS.SALES_DATE,
        PROJECT_FIELDS.PROJECT_AGE,
        PROJECT_FIELDS.SYSTEM_SIZE_KW,
        PROJECT_FIELDS.SYSTEM_PRICE,
        // PPW fields
        PROJECT_FIELDS.SOLD_GROSS_PPW, // soldGross PPW
        PROJECT_FIELDS.COMMISSIONABLE_PPW, // commissionable PPW
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
      // Active projects (status contains "Active")
      // Excludes: "Active - On Hold", "Active - Installed", "Active - PTO"
      return `{${PROJECT_FIELDS.PROJECT_STATUS}.CT.'Active'} AND {${PROJECT_FIELDS.PROJECT_STATUS}.XCT.'On Hold'} AND {${PROJECT_FIELDS.PROJECT_STATUS}.XCT.'Installed'} AND {${PROJECT_FIELDS.PROJECT_STATUS}.XCT.'PTO'}`;

    case 'on-hold':
      // PROJECT_STATUS contains "On Hold"
      return `{${PROJECT_FIELDS.PROJECT_STATUS}.CT.'On Hold'}`;

    case 'install-completed':
      // PROJECT_STATUS = "Active - Installed" OR has install date but not PTO
      return `{${PROJECT_FIELDS.PROJECT_STATUS}.CT.'Installed'} OR ({${PROJECT_FIELDS.INSTALL_COMPLETED_DATE}.XEX.''} AND {${PROJECT_FIELDS.PTO_APPROVED}.EX.''})`;

    case 'pending-cancel':
      // PROJECT_STATUS contains "Pending Cancel"
      return `{${PROJECT_FIELDS.PROJECT_STATUS}.CT.'Pending Cancel'}`;

    case 'cancelled':
      // PROJECT_STATUS = "Cancelled" (exact match or contains Cancel but not Pending)
      return `({${PROJECT_FIELDS.PROJECT_STATUS}.EX.'Cancelled'} OR ({${PROJECT_FIELDS.PROJECT_STATUS}.CT.'Cancel'} AND {${PROJECT_FIELDS.PROJECT_STATUS}.XCT.'Pending'}))`;

    case 'needs-attention':
      // Projects >90 days old OR on hold >7 days
      const sevenDaysAgo = new Date(Date.now() - 7 * 24 * 60 * 60 * 1000).toISOString().split('T')[0];
      return `({${PROJECT_FIELDS.PROJECT_AGE}.GT.90} OR ({${PROJECT_FIELDS.PROJECT_STATUS}.CT.'On Hold'} AND {${PROJECT_FIELDS.DATE_ON_HOLD}.BF.'${sevenDaysAgo}'}))`;

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

/**
 * Build ownership filter based on user email and ownership preference
 * @param userEmail - Current user's email for comparison
 * @param ownership - Ownership filter value ('all' | 'my-projects' | 'team-projects')
 * @returns QuickBase WHERE clause string or empty string
 */
function buildOwnershipFilter(userEmail: string | null, ownership?: string): string {
  if (!ownership || ownership === 'all' || !userEmail || !userEmail.trim()) return '';

  const sanitizedEmail = sanitizeQbLiteral(userEmail);

  if (ownership === 'my-projects') {
    // Filter to projects where user is closer OR setter
    return `({${PROJECT_FIELDS.CLOSER_EMAIL}.EX.'${sanitizedEmail}'} OR {${PROJECT_FIELDS.SETTER_EMAIL}.EX.'${sanitizedEmail}'})`;
  }

  if (ownership === 'team-projects') {
    // Filter to projects where user is NOT closer AND NOT setter
    // Include projects with no closer/setter (unassigned)
    return `(({${PROJECT_FIELDS.CLOSER_EMAIL}.XEX.'${sanitizedEmail}'} OR {${PROJECT_FIELDS.CLOSER_EMAIL}.EX.''}) AND ({${PROJECT_FIELDS.SETTER_EMAIL}.XEX.'${sanitizedEmail}'} OR {${PROJECT_FIELDS.SETTER_EMAIL}.EX.''}))`;
  }

  return '';
}

// Helper function to get sort order based on view and sort parameter
function getSortOrder(view?: string, sort?: string): { fieldId: number; order: 'ASC' | 'DESC' }[] {
  // If explicit sort is provided, use it (overrides view-based sorting)
  if (sort && sort !== 'default') {
    switch (sort) {
      case 'newest':
        return [{ fieldId: PROJECT_FIELDS.SALES_DATE, order: 'DESC' as const }];
      case 'oldest':
        return [{ fieldId: PROJECT_FIELDS.SALES_DATE, order: 'ASC' as const }];
      case 'age-desc':
        return [{ fieldId: PROJECT_FIELDS.PROJECT_AGE, order: 'DESC' as const }];
      case 'customer-asc':
        return [{ fieldId: PROJECT_FIELDS.CUSTOMER_NAME, order: 'ASC' as const }];
      case 'customer-desc':
        return [{ fieldId: PROJECT_FIELDS.CUSTOMER_NAME, order: 'DESC' as const }];
    }
  }

  // Fall back to view-based sorting
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

export async function getDashboardMetricsOptimized(userId: string, role: string, officeIds?: number[]) {
  console.log('[getDashboardMetricsOptimized] START - userId:', userId, 'role:', role, 'officeIds:', officeIds);
  const startTime = Date.now();

  // Get user email for email-based filtering
  let userEmail: string | null = null;
  if (['closer', 'setter', 'coordinator'].includes(role)) {
    userEmail = await getUserEmail(userId);
  }

  // Get managed user emails for team leads
  let managedEmails: string[] | undefined;
  if (role === 'team_lead') {
    managedEmails = await getManagedUserEmails(userId);
  }

  // Get assigned office IDs for office-based roles if no office IDs provided
  let effectiveOfficeIds = officeIds;
  if (!effectiveOfficeIds && ['office_leader', 'area_director', 'divisional'].includes(role)) {
    effectiveOfficeIds = await getAssignedOffices(userId);
  }

  // Use shared role scoping helper for consistency (now email-based)
  const whereClause = buildProjectAccessClause(userEmail, role, effectiveOfficeIds, managedEmails);
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
    // Use timezone-aware date parsing to prevent off-by-one errors
    const d = parseQuickbaseDate(installDate);
    if (!d) return false;
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
    // Use timezone-aware date parsing to prevent off-by-one errors
    const d = parseQuickbaseDate(installDate);
    if (!d) return false;
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

// Time range filtering helper
function buildTimeRangeFilter(timeRange: 'lifetime' | 'month' | 'week'): string {
  const now = new Date();
  
  switch (timeRange) {
    case 'month':
      const currentMonth = now.getMonth();
      const currentYear = now.getFullYear();
      const monthStart = new Date(currentYear, currentMonth, 1);
      const monthEnd = new Date(currentYear, currentMonth + 1, 0);
      // Use AF for start date (after or equal) and BF for end date (before or equal)
      return `{${PROJECT_FIELDS.SALES_DATE}.AF.'${monthStart.toISOString().split('T')[0]}'} AND {${PROJECT_FIELDS.SALES_DATE}.BF.'${monthEnd.toISOString().split('T')[0]}'}`;
    
    case 'week':
      const oneWeekAgo = new Date(now.getTime() - 7 * 24 * 60 * 60 * 1000);
      // Use AF for start date (after or equal to one week ago)
      return `{${PROJECT_FIELDS.SALES_DATE}.AF.'${oneWeekAgo.toISOString().split('T')[0]}'}`;
    
    case 'lifetime':
    default:
      return ''; // No date filter for lifetime
  }
}

// Enhanced dashboard metrics with revenue, commission, and bucket calculations
export async function getEnhancedDashboardMetrics(
  userId: string,
  role: string,
  timeRange: 'lifetime' | 'ytd' | 'month' | 'week' | 'custom' = 'lifetime',
  officeIds?: number[],
  customDateRange?: { startDate: string; endDate: string },
  scope: 'personal' | 'team' = 'team', // NEW PARAMETER
  reqId?: string
) {
  console.log('[getEnhancedDashboardMetrics] START - userId:', userId, 'role:', role, 'timeRange:', timeRange, 'scope:', scope, 'officeIds:', officeIds, 'customDateRange:', customDateRange, 'reqId:', reqId);
  const startTime = Date.now();

  // Get user email for email-based filtering
  let userEmail: string | null = null;
  if (['closer', 'setter', 'coordinator'].includes(role)) {
    userEmail = await getUserEmail(userId);
  }

  // Get managed user emails for team leads
  let managedEmails: string[] | undefined;
  if (role === 'team_lead') {
    managedEmails = await getManagedUserEmails(userId);
  }

  // Get assigned office IDs (QuickBase Record IDs from Field 810) for office-based roles if no office IDs provided
  let effectiveOfficeIds = officeIds;
  if (!effectiveOfficeIds && ['office_leader', 'area_director', 'divisional'].includes(role)) {
    effectiveOfficeIds = await getAssignedOffices(userId);
  }

  // Handle personal scope for managers
  // When scope='personal', managers see only their own projects (like reps)
  if (scope === 'personal' && isManagerRole(role)) {
    // Fetch manager's personal email
    userEmail = await getUserEmail(userId);

    // Personal scope fallback when manager email is missing
    if (!userEmail) {
      logWarn('[ENHANCED_METRICS] Personal scope fallback: manager has no email, reverting to team scope', { userId, role });
      // revert flags
      scope = 'team';
      effectiveOfficeIds = officeIds ?? effectiveOfficeIds;
      managedEmails = role === 'team_lead' ? (await getManagedUserEmails(userId)) : managedEmails;
    } else {
      // Override role-based filtering - treat manager as a rep
      // This means we'll filter by their email instead of office IDs/managed users
      effectiveOfficeIds = undefined;
      managedEmails = undefined;
    }

    logInfo('[ENHANCED_METRICS] Personal scope requested for manager', {
      userId,
      role,
      hasEmail: !!userEmail
    });
  }

  // Build role-based where clause (no time filtering here, now email-based)
  // For personal scope managers, use effective role to trigger email-based filtering
  const effectiveRole = (scope === 'personal' && isManagerRole(role)) ? 'closer' : role;
  const roleClause = buildProjectAccessClause(userEmail, effectiveRole, effectiveOfficeIds, managedEmails, reqId);
  
  if (process.env.NODE_ENV !== 'production') {
    console.log('[getEnhancedDashboardMetrics] Role-based WHERE clause:', roleClause);
  }

  // Fetch all necessary fields for comprehensive metrics (role-scoped only)
  // Use pagination to handle large datasets efficiently
  const MAX_RECORDS_PER_QUERY = 5000;
  const userProjects = await qbClient.queryRecords({
    from: QB_TABLE_PROJECTS,
    select: [
      // Basic fields
      PROJECT_FIELDS.RECORD_ID,
      PROJECT_FIELDS.PROJECT_STATUS,
      PROJECT_FIELDS.ON_HOLD,
      PROJECT_FIELDS.HOLD_REASON,
      PROJECT_FIELDS.DATE_ON_HOLD,
      PROJECT_FIELDS.SALES_DATE,
      PROJECT_FIELDS.PROJECT_AGE,
      
      // Revenue fields
      PROJECT_FIELDS.SYSTEM_PRICE,
      PROJECT_FIELDS.SYSTEM_SIZE_KW,
      PROJECT_FIELDS.COMMISSIONABLE_PPW,
      
      // Team member fields (for commission breakdown by person)
      PROJECT_FIELDS.CLOSER_NAME,
      PROJECT_FIELDS.SETTER_NAME,
      PROJECT_FIELDS.CLOSER_EMAIL,
      PROJECT_FIELDS.SETTER_EMAIL,
      
      // Milestone dates
      PROJECT_FIELDS.INSTALL_COMPLETED_DATE,
      PROJECT_FIELDS.PTO_APPROVED,
      PROJECT_FIELDS.NEM_APPROVED,
      PROJECT_FIELDS.PERMIT_APPROVED,
      PROJECT_FIELDS.INSTALL_SCHEDULED_DATE_CAPTURE,
      
      // Funding fields
      PROJECT_FIELDS.LENDER_FUNDING_RECEIVED_DATE,
      PROJECT_FIELDS.INVOICE_DATE,
      PROJECT_FIELDS.FUNDING_DASHBOARD_M1_STATUS,
      PROJECT_FIELDS.FUNDING_DASHBOARD_M2_STATUS,
      PROJECT_FIELDS.FUNDING_DASHBOARD_M3_STATUS,
    ],
    where: roleClause,
    options: { top: MAX_RECORDS_PER_QUERY }
  });

  const allData = userProjects.data || [];
  const totalRecords = userProjects.metadata?.totalRecords || allData.length;
  
  console.log(`[getEnhancedDashboardMetrics] Retrieved ${allData.length} projects (role-scoped)`);
  
  // Log warning if we hit the pagination limit
  if (totalRecords > MAX_RECORDS_PER_QUERY) {
    logWarn('[ENHANCED_METRICS] Large dataset detected - using pagination', { 
      totalRecords, 
      maxRecords: MAX_RECORDS_PER_QUERY,
      message: 'Metrics calculated from first 5000 records only. Consider using date range filters for more accurate results.'
    });
  }

  // Create three period-specific arrays for per-metric time filtering
  const now = new Date();
  const currentMonth = now.getMonth();
  const currentYear = now.getFullYear();
  const monthStart = new Date(currentYear, currentMonth, 1);
  const monthEnd = new Date(currentYear, currentMonth + 1, 0);
  const oneWeekAgo = new Date(now.getTime() - 7 * 24 * 60 * 60 * 1000);

  // Year-to-date range: January 1st of current year to today
  const ytdStart = new Date(currentYear, 0, 1); // January 1st
  const ytdEnd = new Date(); // Today

  // Custom date range support
  let customStart: Date | undefined;
  let customEnd: Date | undefined;
  if (timeRange === 'custom' && customDateRange) {
    customStart = parseQuickbaseDate(customDateRange.startDate) || new Date(customDateRange.startDate);
    customEnd = parseQuickbaseDate(customDateRange.endDate) || new Date(customDateRange.endDate);
    // Set to end of day for endDate to include the entire day
    customEnd.setHours(23, 59, 59, 999);
  }

  // soldInPeriod = filter by SALES_DATE within timeRange
  const soldInPeriod = allData.filter((project: any) => {
    const salesDate = project[PROJECT_FIELDS.SALES_DATE]?.value;
    if (!salesDate) return false;

    // Use timezone-aware date parsing to prevent off-by-one errors
    const projectDate = parseQuickbaseDate(salesDate);
    if (!projectDate) return false;

    switch (timeRange) {
      case 'custom':
        return customStart && customEnd && projectDate >= customStart && projectDate <= customEnd;
      case 'ytd':
        return projectDate >= ytdStart && projectDate <= ytdEnd;
      case 'month':
        return projectDate >= monthStart && projectDate <= monthEnd;
      case 'week':
        return projectDate >= oneWeekAgo;
      case 'lifetime':
      default:
        return true;
    }
  });

  // installedInPeriod = filter by INSTALL_COMPLETED_DATE within timeRange
  const installedInPeriod = allData.filter((project: any) => {
    const installDate = project[PROJECT_FIELDS.INSTALL_COMPLETED_DATE]?.value;
    if (!installDate) return false;

    // Use timezone-aware date parsing to prevent off-by-one errors
    const projectDate = parseQuickbaseDate(installDate);
    if (!projectDate) return false;

    switch (timeRange) {
      case 'custom':
        return customStart && customEnd && projectDate >= customStart && projectDate <= customEnd;
      case 'ytd':
        return projectDate >= ytdStart && projectDate <= ytdEnd;
      case 'month':
        return projectDate >= monthStart && projectDate <= monthEnd;
      case 'week':
        return projectDate >= oneWeekAgo;
      case 'lifetime':
      default:
        return true;
    }
  });

  // fundedInPeriod = filter by LENDER_FUNDING_RECEIVED_DATE within timeRange
  const fundedInPeriod = allData.filter((project: any) => {
    const fundingDate = project[PROJECT_FIELDS.LENDER_FUNDING_RECEIVED_DATE]?.value;
    if (!fundingDate) return false;

    // Use timezone-aware date parsing to prevent off-by-one errors
    const projectDate = parseQuickbaseDate(fundingDate);
    if (!projectDate) return false;

    switch (timeRange) {
      case 'custom':
        return customStart && customEnd && projectDate >= customStart && projectDate <= customEnd;
      case 'ytd':
        return projectDate >= ytdStart && projectDate <= ytdEnd;
      case 'month':
        return projectDate >= monthStart && projectDate <= monthEnd;
      case 'week':
        return projectDate >= oneWeekAgo;
      case 'lifetime':
      default:
        return true;
    }
  });

  console.log(`[getEnhancedDashboardMetrics] Period arrays - sold: ${soldInPeriod.length}, installed: ${installedInPeriod.length}, funded: ${fundedInPeriod.length}`);

  // Calculate all metrics using appropriate period arrays
  // Pass allData for current state metrics (active projects, on hold) and installedInPeriod for period-filtered metrics
  const basicMetrics = calculateBasicMetrics(allData, installedInPeriod, timeRange);
  
  // Console validation for install metrics fix
  console.log(`[getEnhancedDashboardMetrics] Install metrics validation - installsThisWeek: ${basicMetrics.installsThisWeek}, installsThisMonth: ${basicMetrics.installsThisMonth} (from installedInPeriod: ${installedInPeriod.length} projects)`);
  const revenueMetrics = calculateRevenueMetrics(soldInPeriod, installedInPeriod);
  const commissionBreakdown = calculateCommissionBreakdown(allData, fundedInPeriod);
  const projectBuckets = getProjectBucketCounts(allData); // Use allData for buckets (no time filtering)
  const retentionRates = calculateRetentionRate(allData, timeRange); // Pass timeRange for period-specific calculation

  // Calculate team member breakdown for managers with team scope (with caching)
  let commissionByMember: TeamMemberCommission[] | undefined;
  if (scope === 'team' && isManagerRole(role)) {
    const commissionCacheKey = `${userId}:${role}:${timeRange}:${scope}:${customDateRange?.startDate || ''}:${customDateRange?.endDate || ''}:commission`;
    const cachedCommission = commissionCache.get<TeamMemberCommission[]>(commissionCacheKey);
    
    if (cachedCommission) {
      commissionByMember = cachedCommission;
      console.log(`[getEnhancedDashboardMetrics] Using cached commission data for ${commissionByMember.length} team members`);
    } else {
      commissionByMember = calculateCommissionByTeamMember(allData, fundedInPeriod);
      commissionCache.set(commissionCacheKey, commissionByMember, 60000); // 60 second TTL
      console.log(`[getEnhancedDashboardMetrics] Calculated and cached commission for ${commissionByMember.length} team members`);
    }
  }

  // Calculate team member bucket breakdown for managers with team scope (with caching)
  let bucketsByMember: TeamMemberBuckets[] | undefined;
  if (scope === 'team' && isManagerRole(role)) {
    const bucketsCacheKey = `${userId}:${role}:${timeRange}:${scope}:${customDateRange?.startDate || ''}:${customDateRange?.endDate || ''}:buckets`;
    const cachedBuckets = commissionCache.get<TeamMemberBuckets[]>(bucketsCacheKey);
    
    if (cachedBuckets) {
      bucketsByMember = cachedBuckets;
      console.log(`[getEnhancedDashboardMetrics] Using cached bucket data for ${bucketsByMember.length} team members`);
    } else {
      bucketsByMember = calculateProjectBucketsByTeamMember(allData);
      commissionCache.set(bucketsCacheKey, bucketsByMember, 60000); // 60 second TTL
      console.log(`[getEnhancedDashboardMetrics] Calculated and cached buckets for ${bucketsByMember.length} team members`);
    }
  }

  // Log performance metrics
  const duration = Date.now() - startTime;
  if (process.env.NODE_ENV === 'development') {
    console.log(`[ENHANCED_METRICS] Query completed in ${duration}ms for ${allData.length} total records, sold: ${soldInPeriod.length}, installed: ${installedInPeriod.length}, funded: ${fundedInPeriod.length}`);
  }

  return {
    ...basicMetrics,
    ...revenueMetrics,
    ...commissionBreakdown,
    buckets: projectBuckets,
    retentionRate: timeRange !== 'lifetime' ? retentionRates.period : retentionRates.lifetime,
    retentionRateLifetime: retentionRates.lifetime,
    retentionRatePeriod: retentionRates.period,
    timeRange,
    scope: scope as MetricsScope, // Include scope in response (always present)
    commissionByMember, // Add team member breakdown (undefined for non-managers or personal scope)
    bucketsByMember, // Add team member bucket breakdown
  };
}

// Calculate basic metrics (existing functionality)
// NOTE: This function receives ALL data (not filtered by period) to calculate absolute metrics
// like "Active Projects" and "On Hold" which should always show current state
function calculateBasicMetrics(allData: any[], installedInPeriod: any[], timeRange: 'lifetime' | 'ytd' | 'month' | 'week' | 'custom') {
  const now = new Date();
  const oneWeekAgo = new Date(now.getTime() - 7 * 24 * 60 * 60 * 1000);
  const currentMonth = now.getMonth();
  const currentYear = now.getFullYear();

  // For "Installs This Week" - use timezone-aware parsing and always show current week
  // This is independent of the time range filter (shows actual installs this week)
  const installsThisWeek = allData.filter((p: any) => {
    const installDate = p[PROJECT_FIELDS.INSTALL_COMPLETED_DATE]?.value;
    if (!installDate) return false;
    // Use timezone-aware date parsing to prevent off-by-one errors
    const d = parseQuickbaseDate(installDate);
    if (!d) return false;
    return d >= oneWeekAgo && d <= now;
  }).length;

  // Active Projects - always shows current state (not affected by time range filter)
  const activeProjects = allData.filter((p: any) => {
    const status = p[PROJECT_FIELDS.PROJECT_STATUS]?.value || '';
    const onHold = p[PROJECT_FIELDS.ON_HOLD]?.value;
    return status.includes('Active') && onHold !== 'Yes';
  }).length;

  // On Hold - always shows current state (not affected by time range filter)
  const onHold = allData.filter((p: any) => p[PROJECT_FIELDS.ON_HOLD]?.value === 'Yes').length;

  // For "Monthly Installs" - use timezone-aware parsing and always show current month
  // This is independent of the time range filter (shows actual installs this month)
  const installsThisMonth = allData.filter((p: any) => {
    const installDate = p[PROJECT_FIELDS.INSTALL_COMPLETED_DATE]?.value;
    if (!installDate) return false;
    // Use timezone-aware date parsing to prevent off-by-one errors
    const d = parseQuickbaseDate(installDate);
    if (!d) return false;
    return d.getMonth() === currentMonth && d.getFullYear() === currentYear;
  }).length;

  // Hold reasons - always current state
  const holdReasons = allData
    .filter((p: any) => p[PROJECT_FIELDS.ON_HOLD]?.value === 'Yes')
    .map((p: any) => p[PROJECT_FIELDS.HOLD_REASON]?.value || '')
    .filter((r: string) => r);

  // Refined hold breakdown with controlled categories
  const holdBreakdownMap = new Map<string, number>();
  holdReasons.forEach((reason: string) => {
    const category = categorizeHoldReason(reason);
    holdBreakdownMap.set(category, (holdBreakdownMap.get(category) || 0) + 1);
  });

  const holdBreakdown = Array.from(holdBreakdownMap.entries())
    .map(([category, count]) => `${count} ${category}`)
    .join(', ');

  return { installsThisWeek, activeProjects, onHold, installsThisMonth, holdBreakdown };
}

// Calculate revenue metrics
function calculateRevenueMetrics(soldInPeriod: any[], installedInPeriod: any[]) {
  const soldAccounts = soldInPeriod.length;
  const grossRevenue = soldInPeriod.reduce((sum, p: any) => {
    const price = parseFloat(p[PROJECT_FIELDS.SYSTEM_PRICE]?.value || '0');
    return sum + price;
  }, 0);

  const installCount = installedInPeriod.length;
  const installedRevenue = installedInPeriod.reduce((sum, p: any) => {
    const price = parseFloat(p[PROJECT_FIELDS.SYSTEM_PRICE]?.value || '0');
    return sum + price;
  }, 0);

  return { soldAccounts, grossRevenue, installCount, installedRevenue };
}

// Calculate commission breakdown
function calculateCommissionBreakdown(allData: any[], fundedInPeriod: any[]) {
  const calculateCommission = (projects: any[]) => {
    return projects.reduce((sum, p: any) => {
      const ppw = parseFloat(p[PROJECT_FIELDS.COMMISSIONABLE_PPW]?.value || '0');
      const sizeKw = parseFloat(p[PROJECT_FIELDS.SYSTEM_SIZE_KW]?.value || '0');
      
      // Guard against NaN and negative values
      if (isNaN(ppw) || isNaN(sizeKw) || ppw < 0 || sizeKw < 0) {
        return sum;
      }
      
      // Convert kW to watts: PPW × kW × 1000
      const watts = sizeKw * 1000;
      return sum + (ppw * watts);
    }, 0);
  };

  // Earned: Projects with lender funding received in period (M3 funded)
  const earnedCommission = calculateCommission(fundedInPeriod);

  // Lost: Cancelled projects (use allData for lifetime view)
  const lostProjects = allData.filter((p: any) => {
    const status = p[PROJECT_FIELDS.PROJECT_STATUS]?.value || '';
    return status.includes('Cancel') && !status.includes('Pending');
  });
  const lostCommission = calculateCommission(lostProjects);

  // On Hold: Projects currently on hold (use allData for lifetime view)
  const onHoldProjects = allData.filter((p: any) => p[PROJECT_FIELDS.ON_HOLD]?.value === 'Yes');
  const onHoldCommission = calculateCommission(onHoldProjects);

  // Pending: Active projects not yet installed (use allData for lifetime view)
  const pendingProjects = allData.filter((p: any) => {
    const status = p[PROJECT_FIELDS.PROJECT_STATUS]?.value || '';
    const onHold = p[PROJECT_FIELDS.ON_HOLD]?.value;
    const installed = p[PROJECT_FIELDS.INSTALL_COMPLETED_DATE]?.value;
    return status.includes('Active') && onHold !== 'Yes' && !installed;
  });
  const pendingCommission = calculateCommission(pendingProjects);

  // Sales Aid Commission (placeholder - needs business definition)
  const salesAidCommission = 0;

  return {
    earnedCommission,
    lostCommission,
    onHoldCommission,
    pendingCommission,
    salesAidCommission,
  };
}

/**
 * Calculate commission breakdown by team member
 * Groups projects by closer and setter, calculates commission for each person
 * @param allData - All projects in scope (role-filtered)
 * @param fundedInPeriod - Projects funded in the time period
 * @returns Array of team member commission objects
 */
export function calculateCommissionByTeamMember(
  allData: any[],
  fundedInPeriod: any[]
): TeamMemberCommission[] {
  // COMMISSION ATTRIBUTION STRATEGY:
  // When both closer and setter exist, split commission 50/50 to avoid per-member sums exceeding aggregate totals
  // TODO: Business confirmation needed for split ratio (currently 50/50)
  const COMMISSION_SPLIT_RATIO = 0.5; // 50% to each role when both exist

  // Helper to calculate commission for a single project
  const calculateProjectCommission = (project: any): number => {
    const ppw = parseFloat(project[PROJECT_FIELDS.COMMISSIONABLE_PPW]?.value || '0');
    const sizeKw = parseFloat(project[PROJECT_FIELDS.SYSTEM_SIZE_KW]?.value || '0');
    
    if (isNaN(ppw) || isNaN(sizeKw) || ppw < 0 || sizeKw < 0) {
      return 0;
    }
    
    const watts = sizeKw * 1000;
    return ppw * watts;
  };

  // Map to store commission data by member (key: email or name)
  const memberMap = new Map<string, TeamMemberCommission>();

  // Helper to get or create member entry
  const getMemberEntry = (
    name: string,
    email: string | null,
    role: 'closer' | 'setter'
  ): TeamMemberCommission => {
    // Use email as key if available, otherwise use name
    const key = email || name || 'Unassigned';
    
    if (!memberMap.has(key)) {
      memberMap.set(key, {
        memberName: name || 'Unassigned',
        memberEmail: email,
        role,
        earnedCommission: 0,
        lostCommission: 0,
        onHoldCommission: 0,
        pendingCommission: 0,
        projectCount: 0,
      });
    }
    
    return memberMap.get(key)!;
  };

  // Create Set of funded project IDs for O(1) lookup performance
  const fundedIds = new Set(fundedInPeriod.map(p => p[PROJECT_FIELDS.RECORD_ID]?.value));

  // Process each project and attribute commission to closer and setter
  allData.forEach((project: any) => {
    const fullCommission = calculateProjectCommission(project);
    const status = project[PROJECT_FIELDS.PROJECT_STATUS]?.value || '';
    const onHold = project[PROJECT_FIELDS.ON_HOLD]?.value === 'Yes';
    const installed = project[PROJECT_FIELDS.INSTALL_COMPLETED_DATE]?.value;
    const funded = fundedIds.has(project[PROJECT_FIELDS.RECORD_ID]?.value);

    // Determine commission category
    let category: 'earned' | 'lost' | 'onHold' | 'pending';
    if (funded) {
      category = 'earned';
    } else if (status.includes('Cancel') && !status.includes('Pending')) {
      category = 'lost';
    } else if (onHold) {
      category = 'onHold';
    } else if (status.includes('Active') && !installed) {
      category = 'pending';
    } else {
      return; // Skip projects that don't fit any category
    }

    // Check if both closer and setter exist
    const closerName = project[PROJECT_FIELDS.CLOSER_NAME]?.value;
    const closerEmail = project[PROJECT_FIELDS.CLOSER_EMAIL]?.value;
    const setterName = project[PROJECT_FIELDS.SETTER_NAME]?.value;
    const setterEmail = project[PROJECT_FIELDS.SETTER_EMAIL]?.value;
    
    const hasCloser = closerName || closerEmail;
    const hasSetter = (setterName || setterEmail) && setterEmail !== closerEmail;
    const hasBoth = hasCloser && hasSetter;

    // Calculate commission amount based on attribution strategy
    const closerCommission = hasBoth ? fullCommission * COMMISSION_SPLIT_RATIO : fullCommission;
    const setterCommission = hasBoth ? fullCommission * COMMISSION_SPLIT_RATIO : fullCommission;

    // Attribute to closer
    if (hasCloser) {
      const closerEntry = getMemberEntry(closerName, closerEmail, 'closer');
      closerEntry.projectCount++;
      
      switch (category) {
        case 'earned':
          closerEntry.earnedCommission += closerCommission;
          break;
        case 'lost':
          closerEntry.lostCommission += closerCommission;
          break;
        case 'onHold':
          closerEntry.onHoldCommission += closerCommission;
          break;
        case 'pending':
          closerEntry.pendingCommission += closerCommission;
          break;
      }
    }

    // Attribute to setter (if different from closer)
    if (hasSetter) {
      const setterEntry = getMemberEntry(setterName, setterEmail, 'setter');
      setterEntry.projectCount++;
      
      switch (category) {
        case 'earned':
          setterEntry.earnedCommission += setterCommission;
          break;
        case 'lost':
          setterEntry.lostCommission += setterCommission;
          break;
        case 'onHold':
          setterEntry.onHoldCommission += setterCommission;
          break;
        case 'pending':
          setterEntry.pendingCommission += setterCommission;
          break;
      }
    }
  });

  // Convert map to array and sort by total commission (earned + pending) descending
  const result = Array.from(memberMap.values()).sort((a, b) => {
    const totalA = a.earnedCommission + a.pendingCommission;
    const totalB = b.earnedCommission + b.pendingCommission;
    return totalB - totalA;
  });

  return result;
}

/**
 * Groups projects by closer and setter, calculates bucket counts for each person
 * @param allData - All projects in scope (role-filtered)
 * @returns Array of team member bucket objects
 */
export function calculateProjectBucketsByTeamMember(
  allData: any[]
): TeamMemberBuckets[] {
  const now = new Date();
  const sevenDaysAgo = new Date(now.getTime() - 7 * 24 * 60 * 60 * 1000);

  // Helper to determine which bucket(s) a project belongs to
  const getProjectBuckets = (project: any): string[] => {
    const buckets: string[] = [];
    
    // Installs: Projects completed but not yet PTO'd
    const installed = project[PROJECT_FIELDS.INSTALL_COMPLETED_DATE]?.value;
    const pto = project[PROJECT_FIELDS.PTO_APPROVED]?.value;
    if (installed && !pto) {
      buckets.push('installs');
    }

    // Rejected: Projects with status containing 'Reject'
    const status = project[PROJECT_FIELDS.PROJECT_STATUS]?.value || '';
    if (status.includes('Reject')) {
      buckets.push('rejected');
    }

    // On Hold: Projects currently on hold
    if (project[PROJECT_FIELDS.ON_HOLD]?.value === 'Yes') {
      buckets.push('onHold');
    }

    // Rep Attention: Projects >90 days old OR on hold >7 days
    const age = parseInt(project[PROJECT_FIELDS.PROJECT_AGE]?.value || '0');
    const onHold = project[PROJECT_FIELDS.ON_HOLD]?.value === 'Yes';
    const holdDate = project[PROJECT_FIELDS.DATE_ON_HOLD]?.value;
    
    if (age > 90) {
      buckets.push('repAttention');
    } else if (onHold && holdDate) {
      const hold = new Date(holdDate);
      if (hold < sevenDaysAgo) {
        buckets.push('repAttention');
      }
    }

    // Pending Cancel: Projects with status containing 'Pending Cancel'
    if (status.includes('Pending Cancel')) {
      buckets.push('pendingCancel');
    }

    // Ready for Install: Projects with permits/NEM but no install date
    const nem = project[PROJECT_FIELDS.NEM_APPROVED]?.value;
    const permit = project[PROJECT_FIELDS.PERMIT_APPROVED]?.value;
    const scheduled = project[PROJECT_FIELDS.INSTALL_SCHEDULED_DATE_CAPTURE]?.value;
    if (nem === 'Yes' && permit === 'Yes' && !scheduled) {
      buckets.push('readyForInstall');
    }

    return buckets;
  };

  // Map to store bucket data by member (key: email or name)
  const memberMap = new Map<string, TeamMemberBuckets>();

  // Helper to get or create member entry
  const getMemberEntry = (
    name: string,
    email: string | null,
    role: 'closer' | 'setter'
  ): TeamMemberBuckets => {
    // Use email as key if available, otherwise use name
    const key = email || name || 'Unassigned';
    
    if (!memberMap.has(key)) {
      memberMap.set(key, {
        memberName: name || 'Unassigned',
        memberEmail: email,
        role,
        installs: 0,
        rejected: 0,
        onHold: 0,
        repAttention: 0,
        pendingCancel: 0,
        readyForInstall: 0,
        totalProjects: 0,
      });
    }
    
    return memberMap.get(key)!;
  };

  // Track unique projects per member to avoid double-counting totalProjects
  const memberProjects = new Map<string, Set<string>>();

  // Process each project and attribute bucket counts to closer and setter
  allData.forEach((project: any) => {
    const projectId = project[PROJECT_FIELDS.RECORD_ID]?.value;
    const projectBuckets = getProjectBuckets(project);
    
    // Skip projects that don't fit any bucket
    if (projectBuckets.length === 0) {
      return;
    }

    // Check if both closer and setter exist
    const closerName = project[PROJECT_FIELDS.CLOSER_NAME]?.value;
    const closerEmail = project[PROJECT_FIELDS.CLOSER_EMAIL]?.value;
    const setterName = project[PROJECT_FIELDS.SETTER_NAME]?.value;
    const setterEmail = project[PROJECT_FIELDS.SETTER_EMAIL]?.value;
    
    const hasCloser = closerName || closerEmail;
    const namesMatch = (setterName && closerName) && setterName.trim().toLowerCase() === closerName.trim().toLowerCase();
    const emailsMatch = !!setterEmail && !!closerEmail && setterEmail.trim().toLowerCase() === closerEmail.trim().toLowerCase();
    const hasSetter = (setterName || setterEmail) && !(emailsMatch || namesMatch);

    // Attribute to closer
    if (hasCloser) {
      const closerEntry = getMemberEntry(closerName, closerEmail, 'closer');
      const closerKey = closerEmail || closerName || 'Unassigned';
      
      // Track unique projects for total count
      if (!memberProjects.has(closerKey)) {
        memberProjects.set(closerKey, new Set());
      }
      memberProjects.get(closerKey)!.add(projectId);
      
      // Increment bucket counts
      projectBuckets.forEach(bucket => {
        (closerEntry as any)[bucket]++;
      });
    }

    // Attribute to setter (if different from closer)
    if (hasSetter) {
      const setterEntry = getMemberEntry(setterName, setterEmail, 'setter');
      const setterKey = setterEmail || setterName || 'Unassigned';
      
      // Track unique projects for total count
      if (!memberProjects.has(setterKey)) {
        memberProjects.set(setterKey, new Set());
      }
      memberProjects.get(setterKey)!.add(projectId);
      
      // Increment bucket counts
      projectBuckets.forEach(bucket => {
        (setterEntry as any)[bucket]++;
      });
    }
  });

  // Set totalProjects for each member
  memberMap.forEach((member, key) => {
    member.totalProjects = memberProjects.get(key)?.size || 0;
  });

  // Convert map to array and sort by total projects descending
  const result = Array.from(memberMap.values()).sort((a, b) => {
    return b.totalProjects - a.totalProjects;
  });

  return result;
}

// Calculate project bucket counts
function getProjectBucketCounts(data: any[]) {
  const now = new Date();
  const sevenDaysAgo = new Date(now.getTime() - 7 * 24 * 60 * 60 * 1000);

  // Installs: Projects completed but not yet PTO'd
  const installs = data.filter((p: any) => {
    const installed = p[PROJECT_FIELDS.INSTALL_COMPLETED_DATE]?.value;
    const pto = p[PROJECT_FIELDS.PTO_APPROVED]?.value;
    return installed && !pto;
  }).length;

  // Rejected: Projects with status containing 'Reject'
  const rejected = data.filter((p: any) => {
    const status = p[PROJECT_FIELDS.PROJECT_STATUS]?.value || '';
    return status.includes('Reject');
  }).length;

  // On Hold: Projects currently on hold
  const onHold = data.filter((p: any) => p[PROJECT_FIELDS.ON_HOLD]?.value === 'Yes').length;

  // Rep Attention: Projects >90 days old OR on hold >7 days
  const repAttention = data.filter((p: any) => {
    const age = parseInt(p[PROJECT_FIELDS.PROJECT_AGE]?.value || '0');
    const onHold = p[PROJECT_FIELDS.ON_HOLD]?.value === 'Yes';
    const holdDate = p[PROJECT_FIELDS.DATE_ON_HOLD]?.value;
    
    if (age > 90) return true;
    if (onHold && holdDate) {
      const hold = new Date(holdDate);
      return hold < sevenDaysAgo;
    }
    return false;
  }).length;

  // Pending Cancel: Projects with status containing 'Pending Cancel'
  const pendingCancel = data.filter((p: any) => {
    const status = p[PROJECT_FIELDS.PROJECT_STATUS]?.value || '';
    return status.includes('Pending Cancel');
  }).length;

  // Ready for Install: Projects with permits/NEM but no install date
  const readyForInstall = data.filter((p: any) => {
    const nem = p[PROJECT_FIELDS.NEM_APPROVED]?.value;
    const permit = p[PROJECT_FIELDS.PERMIT_APPROVED]?.value;
    const scheduled = p[PROJECT_FIELDS.INSTALL_SCHEDULED_DATE_CAPTURE]?.value;
    return nem === 'Yes' && permit === 'Yes' && !scheduled;
  }).length;

  return { installs, rejected, onHold, repAttention, pendingCancel, readyForInstall };
}

// Calculate retention rate with business-approved statuses
function calculateRetentionRate(data: any[], timeRange: 'lifetime' | 'ytd' | 'month' | 'week' | 'custom' = 'lifetime'): { lifetime: number; period: number } {
  // Business-approved status categories
  const retainedStatuses = ['Active', 'Completed', 'Installed', 'PTO Approved'];
  const lostStatuses = ['Cancelled', 'Canceled', 'Rejected', 'Pending Cancel'];
  
  // Calculate lifetime retention (all data)
  const active = data.filter((p: any) => {
    const status = p[PROJECT_FIELDS.PROJECT_STATUS]?.value || '';
    return retainedStatuses.some(retainedStatus => status.includes(retainedStatus));
  }).length;

  const completed = data.filter((p: any) => {
    const status = p[PROJECT_FIELDS.PROJECT_STATUS]?.value || '';
    return status.includes('Completed') || status.includes('PTO Approved');
  }).length;

  const cancelled = data.filter((p: any) => {
    const status = p[PROJECT_FIELDS.PROJECT_STATUS]?.value || '';
    return lostStatuses.some(lostStatus => status.includes(lostStatus));
  }).length;

  const total = active + completed + cancelled;
  if (total === 0) return { lifetime: 0, period: 0 };
  
  const lifetimeRetention = Math.round(((active + completed) / total) * 100 * 10) / 10; // Round to 1 decimal place
  
  // Calculate period-specific retention based on time range
  let periodRetention = lifetimeRetention; // Default to lifetime
  
  if (timeRange !== 'lifetime') {
    const now = new Date();
    let periodStart: Date;
    
    switch (timeRange) {
      case 'month':
        const currentMonth = now.getMonth();
        const currentYear = now.getFullYear();
        periodStart = new Date(currentYear, currentMonth, 1);
        break;
      case 'week':
        periodStart = new Date(now.getTime() - 7 * 24 * 60 * 60 * 1000);
        break;
      default:
        periodStart = new Date(0); // All time
    }
    
    // Filter data to period-specific projects (based on sales date)
    const periodData = data.filter((p: any) => {
      const salesDate = p[PROJECT_FIELDS.SALES_DATE]?.value;
      if (!salesDate) return false;
      const projectDate = new Date(salesDate);
      return projectDate >= periodStart;
    });
    
    if (periodData.length > 0) {
      const periodActive = periodData.filter((p: any) => {
        const status = p[PROJECT_FIELDS.PROJECT_STATUS]?.value || '';
        return retainedStatuses.some(retainedStatus => status.includes(retainedStatus));
      }).length;

      const periodCompleted = periodData.filter((p: any) => {
        const status = p[PROJECT_FIELDS.PROJECT_STATUS]?.value || '';
        return status.includes('Completed') || status.includes('PTO Approved');
      }).length;

      const periodCancelled = periodData.filter((p: any) => {
        const status = p[PROJECT_FIELDS.PROJECT_STATUS]?.value || '';
        return lostStatuses.some(lostStatus => status.includes(lostStatus));
      }).length;

      const periodTotal = periodActive + periodCompleted + periodCancelled;
      if (periodTotal > 0) {
        periodRetention = Math.round(((periodActive + periodCompleted) / periodTotal) * 100 * 10) / 10;
      }
    }
  }
  
  return { 
    lifetime: lifetimeRetention, 
    period: periodRetention 
  };
}

// Test function for time range filter validation
export function testTimeRangeFilter() {
  const now = new Date();
  const currentMonth = now.getMonth();
  const currentYear = now.getFullYear();
  const monthStart = new Date(currentYear, currentMonth, 1);
  const monthEnd = new Date(currentYear, currentMonth + 1, 0);
  const oneWeekAgo = new Date(now.getTime() - 7 * 24 * 60 * 60 * 1000);

  console.log('Time Range Filter Tests:');
  console.log('Current date:', now.toISOString().split('T')[0]);
  console.log('Month start:', monthStart.toISOString().split('T')[0]);
  console.log('Month end:', monthEnd.toISOString().split('T')[0]);
  console.log('One week ago:', oneWeekAgo.toISOString().split('T')[0]);
  
  const monthFilter = buildTimeRangeFilter('month');
  const weekFilter = buildTimeRangeFilter('week');
  const lifetimeFilter = buildTimeRangeFilter('lifetime');
  
  console.log('Month filter:', monthFilter);
  console.log('Week filter:', weekFilter);
  console.log('Lifetime filter:', lifetimeFilter);
  
  return { monthFilter, weekFilter, lifetimeFilter };
}

// Test function for per-metric period array validation
export function testPerMetricPeriodArrays() {
  console.log('Per-Metric Period Array Tests:');
  
  // Mock project data for testing
  const mockProjects = [
    {
      [PROJECT_FIELDS.SALES_DATE]: { value: '2024-01-15' },
      [PROJECT_FIELDS.INSTALL_COMPLETED_DATE]: { value: '2024-02-15' },
      [PROJECT_FIELDS.LENDER_FUNDING_RECEIVED_DATE]: { value: '2024-03-15' },
    },
    {
      [PROJECT_FIELDS.SALES_DATE]: { value: '2024-01-20' },
      [PROJECT_FIELDS.INSTALL_COMPLETED_DATE]: { value: '2024-01-25' },
      [PROJECT_FIELDS.LENDER_FUNDING_RECEIVED_DATE]: { value: '2024-02-01' },
    },
    {
      [PROJECT_FIELDS.SALES_DATE]: { value: '2023-12-01' },
      [PROJECT_FIELDS.INSTALL_COMPLETED_DATE]: { value: '2024-01-10' },
      [PROJECT_FIELDS.LENDER_FUNDING_RECEIVED_DATE]: { value: '2024-01-20' },
    },
  ];

  const now = new Date();
  const currentMonth = now.getMonth();
  const currentYear = now.getFullYear();
  const monthStart = new Date(currentYear, currentMonth, 1);
  const monthEnd = new Date(currentYear, currentMonth + 1, 0);
  const oneWeekAgo = new Date(now.getTime() - 7 * 24 * 60 * 60 * 1000);

  // Test soldInPeriod (SALES_DATE filtering)
  const soldInPeriod = mockProjects.filter((project: any) => {
    const salesDate = project[PROJECT_FIELDS.SALES_DATE]?.value;
    if (!salesDate) return false;
    const projectDate = new Date(salesDate);
    return projectDate >= monthStart && projectDate <= monthEnd;
  });
  console.log(`soldInPeriod (SALES_DATE): ${soldInPeriod.length} projects`);

  // Test installedInPeriod (INSTALL_COMPLETED_DATE filtering)
  const installedInPeriod = mockProjects.filter((project: any) => {
    const installDate = project[PROJECT_FIELDS.INSTALL_COMPLETED_DATE]?.value;
    if (!installDate) return false;
    const projectDate = new Date(installDate);
    return projectDate >= monthStart && projectDate <= monthEnd;
  });
  console.log(`installedInPeriod (INSTALL_COMPLETED_DATE): ${installedInPeriod.length} projects`);

  // Test fundedInPeriod (LENDER_FUNDING_RECEIVED_DATE filtering)
  const fundedInPeriod = mockProjects.filter((project: any) => {
    const fundingDate = project[PROJECT_FIELDS.LENDER_FUNDING_RECEIVED_DATE]?.value;
    if (!fundingDate) return false;
    const projectDate = new Date(fundingDate);
    return projectDate >= monthStart && projectDate <= monthEnd;
  });
  console.log(`fundedInPeriod (LENDER_FUNDING_RECEIVED_DATE): ${fundedInPeriod.length} projects`);

  return { soldInPeriod, installedInPeriod, fundedInPeriod };
}

// Categorize hold reasons into controlled categories
function categorizeHoldReason(reason: string): string {
  if (!reason || typeof reason !== 'string') return 'Other';
  
  const normalizedReason = reason.toLowerCase().trim();
  
  // Split on common delimiters first
  const parts = normalizedReason.split(/[-:]/).map(part => part.trim());
  const firstPart = parts[0];
  
  // Predefined mapping for common hold reasons
  const categoryMap: Record<string, string> = {
    'finance': 'Finance',
    'financial': 'Finance',
    'funding': 'Finance',
    'credit': 'Finance',
    'loan': 'Finance',
    'roof': 'Roof',
    'roofing': 'Roof',
    'structural': 'Roof',
    'customer': 'Customer',
    'homeowner': 'Customer',
    'owner': 'Customer',
    'permit': 'Permit',
    'permits': 'Permit',
    'permitting': 'Permit',
    'hoa': 'HOA',
    'homeowner association': 'HOA',
    'utility': 'Utility',
    'utilities': 'Utility',
    'inspection': 'Inspection',
    'inspections': 'Inspection',
    'design': 'Design',
    'engineering': 'Design',
    'survey': 'Survey',
    'surveys': 'Survey',
    'equipment': 'Equipment',
    'material': 'Equipment',
    'materials': 'Equipment',
    'install': 'Installation',
    'installation': 'Installation',
    'installer': 'Installation',
    'weather': 'Weather',
    'scheduling': 'Scheduling',
    'schedule': 'Scheduling',
  };
  
  // Check for exact matches first
  if (categoryMap[firstPart]) {
    return categoryMap[firstPart];
  }
  
  // Check for partial matches
  for (const [key, category] of Object.entries(categoryMap)) {
    if (firstPart.includes(key) || key.includes(firstPart)) {
      return category;
    }
  }
  
  // If no match found, return the first word capitalized
  return firstPart.charAt(0).toUpperCase() + firstPart.slice(1) || 'Other';
}

// Test function for commission calculation validation
export function testCommissionCalculation() {
  console.log('Commission Calculation Tests:');
  
  // Test cases with sample data
  const testCases = [
    { ppw: 0.50, sizeKw: 5.0, expected: 2500 }, // 0.50 * 5 * 1000 = 2500
    { ppw: 0.75, sizeKw: 8.5, expected: 6375 }, // 0.75 * 8.5 * 1000 = 6375
    { ppw: 1.00, sizeKw: 10.0, expected: 10000 }, // 1.00 * 10 * 1000 = 10000
    { ppw: 0, sizeKw: 5.0, expected: 0 }, // Zero PPW
    { ppw: 0.50, sizeKw: 0, expected: 0 }, // Zero size
    { ppw: -0.50, sizeKw: 5.0, expected: 0 }, // Negative PPW (should be ignored)
    { ppw: 0.50, sizeKw: -5.0, expected: 0 }, // Negative size (should be ignored)
  ];
  
  testCases.forEach((testCase, index) => {
    const ppw = testCase.ppw;
    const sizeKw = testCase.sizeKw;
    
    // Guard against NaN and negative values
    if (isNaN(ppw) || isNaN(sizeKw) || ppw < 0 || sizeKw < 0) {
      console.log(`Test ${index + 1}: PPW=${ppw}, Size=${sizeKw}kW -> 0 (guarded)`);
      return;
    }
    
    // Convert kW to watts: PPW × kW × 1000
    const watts = sizeKw * 1000;
    const result = ppw * watts;
    
    console.log(`Test ${index + 1}: PPW=${ppw}, Size=${sizeKw}kW -> ${result} (expected: ${testCase.expected})`);
    
    if (result === testCase.expected) {
      console.log(`  ✓ PASS`);
    } else {
      console.log(`  ✗ FAIL`);
    }
  });
  
  return testCases;
}

// Test function for hold breakdown categorization
export function testHoldBreakdownCategorization() {
  console.log('Hold Breakdown Categorization Tests:');
  
  const testCases = [
    'Finance - Credit Check',
    'Roof: Structural Issues',
    'Customer Decision',
    'Permit Approval',
    'HOA Approval',
    'Utility Connection',
    'Inspection Failed',
    'Design Changes',
    'Survey Required',
    'Equipment Delay',
    'Installation Scheduling',
    'Weather Delay',
    'Unknown Reason',
    'Multi-word reason with spaces',
    '',
    null,
    undefined,
  ];
  
  testCases.forEach((reason, index) => {
    const category = categorizeHoldReason(reason as string);
    console.log(`Test ${index + 1}: "${reason}" -> "${category}"`);
  });
  
  return testCases.map(reason => ({
    input: reason,
    output: categorizeHoldReason(reason as string)
  }));
}

export const __test__ = {
  sanitizeQbLiteral,
  buildSearchFilter,
  buildOwnershipFilter,
  testTimeRangeFilter,
  testPerMetricPeriodArrays,
  testCommissionCalculation,
  testHoldBreakdownCategorization,
  // Test-only exports for SQL helper functions
  getAssignedOffices,
  getManagedUserEmails,
  // Cache testing utilities
  commissionCache,
};

export async function getUrgentProjects(userId: string, role: string, officeIds?: number[]) {
  console.log('[getUrgentProjects] START - userId:', userId, 'role:', role, 'officeIds:', officeIds);
  const holds = await getProjectsOnHold(userId, role, officeIds);
  console.log('[getUrgentProjects] Retrieved', holds?.length || 0, 'projects on hold');
  const sevenDaysAgo = new Date(Date.now() - 7 * 24 * 60 * 60 * 1000);
  
  const urgentProjects = holds
    .filter((project: any) => {
      const holdDate = project[PROJECT_FIELDS.DATE_ON_HOLD]?.value;
      if (!holdDate) return false;
      // Use timezone-aware date parsing to prevent off-by-one errors
      const parsedDate = parseQuickbaseDate(holdDate);
      if (!parsedDate) return false;
      return parsedDate < sevenDaysAgo;
    })
    .map((project: any) => {
      // Use timezone-aware date parsing to prevent off-by-one errors
      const holdDate = parseQuickbaseDate(project[PROJECT_FIELDS.DATE_ON_HOLD]?.value);
      const daysOnHold = holdDate ? Math.floor((Date.now() - holdDate.getTime()) / (1000 * 60 * 60 * 24)) : 0;
      
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

export async function getRecentProjects(userId: string, role: string, officeIds?: number[]) {
  const projects = await getProjectsForUser(userId, role, undefined, undefined, undefined, officeIds);
  
  const recentProjects = projects
    .filter((project: any) => {
      const status = project[PROJECT_FIELDS.PROJECT_STATUS]?.value || '';
      const onHold = project[PROJECT_FIELDS.ON_HOLD]?.value;
      return status.includes('Active') && onHold !== 'Yes';
    })
    .sort((a: any, b: any) => {
      // Use timezone-aware date parsing to prevent off-by-one errors
      const dateA = parseQuickbaseDate(a[PROJECT_FIELDS.SALES_DATE]?.value);
      const dateB = parseQuickbaseDate(b[PROJECT_FIELDS.SALES_DATE]?.value);
      const timeA = dateA?.getTime() || 0;
      const timeB = dateB?.getTime() || 0;
      return timeB - timeA; // Newest first
    })
    .slice(0, 5); // Take first 5
  
  return recentProjects;
}

export async function getTeamActivityFeed(
  userId: string,
  role: string,
  officeIds?: number[],
  limit: number = 10,
  offset: number = 0
): Promise<{ activities: TeamActivityItem[]; totalCount: number; hasMore: boolean }> {
  // Early return for non-managers
  if (!isManagerRole(role)) {
    return { activities: [], totalCount: 0, hasMore: false };
  }

  // Get role-based filtering data
  let offices: number[] = [];
  let managedEmails: string[] = [];

  if (role === 'office_leader') {
    offices = officeIds || await getAssignedOffices(userId);
  } else if (['area_director', 'divisional', 'regional'].includes(role)) {
    offices = officeIds || await getAssignedOffices(userId);
  } else if (role === 'team_lead') {
    managedEmails = await getManagedUserEmails(userId);
  }

  // Get user email for email-based filtering
  const userEmail = await getUserEmail(userId);
  if (!userEmail) {
    return { activities: [], totalCount: 0, hasMore: false };
  }

  // Build role-based access clause
  const accessClause = buildProjectAccessClause(userEmail, role, offices, managedEmails);

  // Calculate date 7 days ago for filtering recent activity
  const sevenDaysAgo = new Date();
  sevenDaysAgo.setDate(sevenDaysAgo.getDate() - 7);
  const sevenDaysAgoStr = sevenDaysAgo.toISOString().split('T')[0]; // YYYY-MM-DD format

  // Build date filter for last 7 days - only include records where the date field is populated AND recent
  // Use AF (after) operator with actual date
  const dateFilter = `(({${PROJECT_FIELDS.DATE_ON_HOLD}.XEX.''} AND {${PROJECT_FIELDS.DATE_ON_HOLD}.OAF.'${sevenDaysAgoStr}'}) OR ({${PROJECT_FIELDS.INSTALL_COMPLETED_DATE}.XEX.''} AND {${PROJECT_FIELDS.INSTALL_COMPLETED_DATE}.OAF.'${sevenDaysAgoStr}'}) OR ({${PROJECT_FIELDS.PTO_APPROVED}.XEX.''} AND {${PROJECT_FIELDS.PTO_APPROVED}.OAF.'${sevenDaysAgoStr}'}))`;

  // Combine clauses
  const whereClause = `(${accessClause}) AND (${dateFilter})`;

  // Query QuickBase with minimal fields and pagination
  const result = await qbClient.queryRecords({
    from: QB_TABLE_PROJECTS,
    select: [
      PROJECT_FIELDS.RECORD_ID,
      PROJECT_FIELDS.PROJECT_ID,
      PROJECT_FIELDS.CUSTOMER_NAME,
      PROJECT_FIELDS.PROJECT_STATUS,
      PROJECT_FIELDS.CLOSER_NAME,
      PROJECT_FIELDS.SETTER_NAME,
      PROJECT_FIELDS.DATE_ON_HOLD,
      PROJECT_FIELDS.INSTALL_COMPLETED_DATE,
      PROJECT_FIELDS.PTO_APPROVED,
    ],
    where: whereClause,
    options: {
      skip: offset,
      top: limit * 2 // Get more records to account for filtering
    }
  });

  // Transform projects to activity items
  const activities: TeamActivityItem[] = [];
  
  for (const project of result.data) {
    const activity = determineActivity(project);
    if (activity) {
      activities.push(activity);
    }
  }

  // Sort by timestamp descending (most recent first)
  activities.sort((a, b) => new Date(b.timestamp).getTime() - new Date(a.timestamp).getTime());
  
  // Apply pagination to final results
  const paginatedActivities = activities.slice(0, limit);
  const totalCount = result.metadata?.totalRecords || activities.length;
  const hasMore = (offset + limit) < totalCount;
  
  return {
    activities: paginatedActivities,
    totalCount,
    hasMore
  };
}

// Helper function to determine activity from project data
function determineActivity(project: any): TeamActivityItem | null {
  const recordId = project[PROJECT_FIELDS.RECORD_ID]?.value;
  const projectId = project[PROJECT_FIELDS.PROJECT_ID]?.value || 'N/A';
  const customerName = project[PROJECT_FIELDS.CUSTOMER_NAME]?.value || 'Unknown Customer';
  const projectStatus = project[PROJECT_FIELDS.PROJECT_STATUS]?.value;
  const closerName = project[PROJECT_FIELDS.CLOSER_NAME]?.value;
  const setterName = project[PROJECT_FIELDS.SETTER_NAME]?.value;
  const dateOnHold = project[PROJECT_FIELDS.DATE_ON_HOLD]?.value;
  const installCompletedDate = project[PROJECT_FIELDS.INSTALL_COMPLETED_DATE]?.value;
  const ptoApproved = project[PROJECT_FIELDS.PTO_APPROVED]?.value;

  // Determine team member
  const teamMemberName = closerName || setterName || 'Unassigned';
  const teamMemberRole: 'closer' | 'setter' = closerName ? 'closer' : 'setter';

  // Determine most recent activity (priority: PTO > Install > Hold > Cancel)
  let activityType: TeamActivityType;
  let activityDescription: string;
  let timestamp: string;

  if (ptoApproved) {
    activityType = 'pto_approved';
    activityDescription = 'PTO approved';
    timestamp = ptoApproved;
  } else if (installCompletedDate) {
    activityType = 'install_completed';
    activityDescription = 'Install completed';
    timestamp = installCompletedDate;
  } else if (dateOnHold) {
    activityType = 'placed_on_hold';
    activityDescription = 'Placed on hold';
    timestamp = dateOnHold;
  } else {
    return null; // No relevant activity
  }

  // Calculate days ago using timezone-aware date parsing
  const timestampDate = parseQuickbaseDate(timestamp);
  const daysAgo = timestampDate
    ? Math.floor((Date.now() - timestampDate.getTime()) / (1000 * 60 * 60 * 24))
    : 0;

  return {
    recordId,
    projectId,
    customerName,
    activityType,
    activityDescription,
    teamMemberName,
    teamMemberRole,
    timestamp,
    daysAgo,
  };
}

export async function getProjectById(recordId: number) {
  const result = await qbClient.queryRecords({
    from: QB_TABLE_PROJECTS,
    select: Object.values(PROJECT_FIELDS), // All 92 fields
    where: `{${PROJECT_FIELDS.RECORD_ID}.EX.${recordId}}`,
  });

  return result.data[0] || null;
}

export async function getProjectsOnHold(userId: string, role: string, officeIds?: number[]) {
  // Get user email for email-based filtering
  let userEmail: string | null = null;
  if (['closer', 'setter', 'coordinator'].includes(role)) {
    userEmail = await getUserEmail(userId);
  }

  // Get managed user emails for team leads
  let managedEmails: string[] | undefined;
  if (role === 'team_lead') {
    managedEmails = await getManagedUserEmails(userId);
  }

  // Get assigned office IDs (QuickBase Record IDs from Field 810) for office-based roles if no office IDs provided
  let effectiveOfficeIds = officeIds;
  if (!effectiveOfficeIds && ['office_leader', 'area_director', 'divisional'].includes(role)) {
    effectiveOfficeIds = await getAssignedOffices(userId);
  }

  // Use shared role scoping helper and consistent ON_HOLD value (now email-based)
  const roleClause = buildProjectAccessClause(userEmail, role, effectiveOfficeIds, managedEmails);
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

/**
 * Get adders for a specific project
 */
export async function getAddersForProject(projectRecordId: string | number) {
  console.log('[getAddersForProject] Fetching adders for project:', projectRecordId);

  try {
    const response = await qbClient.queryRecords({
      from: QB_TABLE_ADDERS,
      where: `{${ADDER_FIELDS.RELATED_PROJECT}.EX.${projectRecordId}}`,
      select: [
        ADDER_FIELDS.RECORD_ID,
        ADDER_FIELDS.PRODUCT_NAME,
        ADDER_FIELDS.TOTAL_COST,
        ADDER_FIELDS.QTY,
        ADDER_FIELDS.STATUS,
        ADDER_FIELDS.WHOS_PAYING,
        ADDER_FIELDS.ADDER_NAME_COST,
      ],
    });

    console.log('[getAddersForProject] Found adders:', response.data?.length || 0);
    return response.data || [];
  } catch (error) {
    logError('Failed to fetch adders for project', error as Error, { projectRecordId });
    return [];
  }
}

/**
 * Get rep-visible notes for a specific project
 */
export async function getNotesForProject(projectRecordId: string | number) {
  const { NOTE_FIELDS } = await import('@/lib/constants/noteFieldIds');
  const QB_TABLE_NOTES = 'bsb6bqt3b';

  console.log('[getNotesForProject] Fetching notes for project:', projectRecordId);

  try {
    const response = await qbClient.queryRecords({
      from: QB_TABLE_NOTES,
      where: `{${NOTE_FIELDS.RELATED_PROJECT}.EX.${projectRecordId}} AND {${NOTE_FIELDS.REP_VISIBLE}.EX.'Rep Visible'}`,
      select: [
        NOTE_FIELDS.RECORD_ID,
        NOTE_FIELDS.NOTE_CONTENT,
        NOTE_FIELDS.CATEGORY,
        NOTE_FIELDS.CREATED_BY,
        NOTE_FIELDS.DATE_CREATED,
        NOTE_FIELDS.REP_VISIBLE,
      ],
      sortBy: [{ fieldId: NOTE_FIELDS.DATE_CREATED, order: 'DESC' }],
    });

    console.log('[getNotesForProject] Found notes:', response.data?.length || 0);

    // Debug: Log first note's structure to understand data format
    if (response.data && response.data.length > 0) {
      const firstNote = response.data[0];
      console.log('[getNotesForProject] First note structure:', {
        keys: Object.keys(firstNote),
        field_8_raw: firstNote[8],
        field_9_raw: firstNote[9],
        field_8_value: firstNote[8]?.value,
        field_9_value: firstNote[9]?.value,
      });
    }

    return response.data || [];
  } catch (error) {
    logError('Failed to fetch notes for project', error as Error, { projectRecordId });
    return [];
  }
}

/**
 * Create a new note for a project (auto-tagged as Sales/Rep Visible)
 */
export async function createNoteForProject(projectRecordId: string | number, noteContent: string, createdBy: string) {
  const { NOTE_FIELDS, NOTE_CATEGORIES, REP_VISIBLE_FLAG } = await import('@/lib/constants/noteFieldIds');
  const QB_TABLE_NOTES = 'bsb6bqt3b';

  console.log('[createNoteForProject] Creating note for project:', projectRecordId);

  try {
    const response = await qbClient.updateRecord({
      to: QB_TABLE_NOTES,
      data: [
        {
          [NOTE_FIELDS.RELATED_PROJECT]: { value: Number(projectRecordId) },
          [NOTE_FIELDS.NOTE_CONTENT]: { value: noteContent },
          [NOTE_FIELDS.CATEGORY]: { value: NOTE_CATEGORIES.SALES },
          [NOTE_FIELDS.REP_VISIBLE]: { value: REP_VISIBLE_FLAG },
          [NOTE_FIELDS.SALES_TEAM_FLAG]: { value: 1 },
        },
      ],
    });

    console.log('[createNoteForProject] Note created successfully');
    return response;
  } catch (error) {
    logError('Failed to create note for project', error as Error, { projectRecordId, createdBy });
    throw error;
  }
}
