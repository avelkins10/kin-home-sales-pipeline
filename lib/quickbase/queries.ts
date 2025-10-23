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
import {
  PROJECT_FIELDS,
  TASK_FIELDS,
  TASK_GROUP_FIELDS,
  TASK_SUBMISSION_FIELDS,
  TASK_TEMPLATE_FIELDS,
  QB_TABLE_TASKS,
  QB_TABLE_TASK_GROUPS,
  QB_TABLE_TASK_SUBMISSIONS,
  QB_TABLE_TASK_TEMPLATES
} from '@/lib/constants/fieldIds';
import { ADDER_FIELDS } from '@/lib/constants/adderFieldIds';
import { sql } from '@/lib/db/client';
import { buildProjectAccessClause } from '@/lib/auth/projectAuthorization';
import type { TeamActivityItem, TeamActivityType } from '@/lib/types/dashboard';
import { isManagerRole } from '@/lib/utils/role-helpers';
import type { MetricsScope, TeamMemberCommission, TeamMemberBuckets } from '@/lib/types/dashboard';
import { parseQuickbaseDate } from '@/lib/utils/date-helpers';
import type { OfficeMetrics, RepPerformance, PipelineForecast, MilestoneTimings } from '@/lib/types/analytics';
import type { Task, TaskSubmission, TaskStatus, TaskTemplate } from '@/lib/types/task';

/**
 * Helper function to normalize QuickBase boolean values
 * Treats 1, '1', true, 'true' as true, everything else as false
 */
function isTrueQB(value: any): boolean {
  if (value === 1 || value === '1' || value === true || value === 'true') {
    return true;
  }
  return false;
}

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
const QB_TABLE_PROJECTS = (process.env.QUICKBASE_TABLE_PROJECTS || 'br9kwm8na').trim();
const QB_TABLE_ADDERS = (process.env.QUICKBASE_TABLE_ADDERS || 'bsaycczmf').trim();

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
export async function getUserEmail(userId: string): Promise<string | null> {
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
export async function getManagedUserEmails(managerId: string): Promise<string[]> {
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
export async function getAssignedOffices(userId: string): Promise<number[]> {
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
export async function getProjectsForUserList(userId: string, role: string, view?: string, search?: string, sort?: string, officeIds?: number[], memberEmail?: string, ownership?: string, officeFilter?: string, setterFilter?: string, closerFilter?: string, reqId?: string, withTasks?: boolean, dateFilter?: string, startDate?: string, endDate?: string) {
  console.log('[getProjectsForUserList] START - userId:', userId, 'role:', role, 'view:', view, 'search:', search, 'sort:', sort, 'ownership:', ownership, 'officeFilter:', officeFilter, 'setterFilter:', setterFilter, 'closerFilter:', closerFilter, 'officeIds:', officeIds, 'withTasks:', withTasks, 'dateFilter:', dateFilter, 'startDate:', startDate, 'endDate:', endDate, 'reqId:', reqId);

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
  if (!effectiveOfficeIds && ['office_leader', 'area_director', 'divisional', 'regional'].includes(role)) {
    effectiveOfficeIds = await getAssignedOffices(userId);
    logInfo('[PROJECT_QUERY] Fetched offices from database', { userId, role, officeCount: effectiveOfficeIds?.length || 0 });
  } else if (effectiveOfficeIds) {
    logInfo('[PROJECT_QUERY] Using provided offices (not fetching from DB)', { userId, role, officeCount: effectiveOfficeIds?.length || 0 });
  }

  // Check for empty office assignments for office-based roles
  if (['office_leader', 'area_director', 'divisional', 'regional'].includes(role) && (!effectiveOfficeIds || effectiveOfficeIds.length === 0)) {
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

  // Add date filter if provided
  let dateFilterClause: string | undefined;
  if (dateFilter) {
    let filterStartDate: string;
    let filterEndDate: string;

    if (dateFilter === 'custom') {
      // Use provided dates for custom range
      filterStartDate = startDate!;
      filterEndDate = endDate!;
    } else {
      // Calculate date ranges for preset options
      const today = new Date();
      const currentDay = today.getDay(); // 0 = Sunday, 1 = Monday, etc.

      if (dateFilter === 'this-week') {
        // Get Monday of this week
        const mondayOffset = currentDay === 0 ? -6 : 1 - currentDay;
        const monday = new Date(today);
        monday.setDate(today.getDate() + mondayOffset);
        filterStartDate = monday.toISOString().split('T')[0];
        filterEndDate = today.toISOString().split('T')[0];
      } else if (dateFilter === 'last-week') {
        // Get Monday of last week
        const mondayOffset = currentDay === 0 ? -6 : 1 - currentDay;
        const thisMonday = new Date(today);
        thisMonday.setDate(today.getDate() + mondayOffset);
        const lastMonday = new Date(thisMonday);
        lastMonday.setDate(thisMonday.getDate() - 7);
        const lastSunday = new Date(lastMonday);
        lastSunday.setDate(lastMonday.getDate() + 6);
        filterStartDate = lastMonday.toISOString().split('T')[0];
        filterEndDate = lastSunday.toISOString().split('T')[0];
      } else if (dateFilter === 'this-month') {
        // Get first day of this month
        const firstDay = new Date(today.getFullYear(), today.getMonth(), 1);
        filterStartDate = firstDay.toISOString().split('T')[0];
        filterEndDate = today.toISOString().split('T')[0];
      } else if (dateFilter === 'last-month') {
        // Get first and last day of last month
        const firstDay = new Date(today.getFullYear(), today.getMonth() - 1, 1);
        const lastDay = new Date(today.getFullYear(), today.getMonth(), 0);
        filterStartDate = firstDay.toISOString().split('T')[0];
        filterEndDate = lastDay.toISOString().split('T')[0];
      } else {
        // Default to no filter if unknown type
        filterStartDate = '';
        filterEndDate = '';
      }
    }

    // Build the QuickBase date filter clause
    if (filterStartDate && filterEndDate) {
      // QuickBase date format: YYYY-MM-DD
      dateFilterClause = `{${PROJECT_FIELDS.SALES_DATE}.OAF.'${filterStartDate}'} AND {${PROJECT_FIELDS.SALES_DATE}.OBF.'${filterEndDate}'}`;
      console.log('[getProjectsForUserList] Filtering by date range:', filterStartDate, 'to', filterEndDate);
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

  // Build tasks filter - query Task Groups table to find projects with unapproved tasks
  let tasksFilter: string | undefined;
  if (withTasks) {
    console.log('[getProjectsForUserList] Fetching projects with unapproved tasks from Task Groups table');
    try {
      // Query Task Groups for projects with unapproved tasks
      const taskGroupsResponse = await qbClient.queryRecords({
        from: QB_TABLE_TASK_GROUPS,
        select: [TASK_GROUP_FIELDS.RELATED_PROJECT, TASK_GROUP_FIELDS.UNAPPROVED_TASKS],
        where: `{${TASK_GROUP_FIELDS.UNAPPROVED_TASKS}.GT.0}`
      });

      // Extract unique project IDs
      const projectIdsWithTasks = new Set<number>();
      taskGroupsResponse.data?.forEach((record: any) => {
        const projectId = parseInt(record[TASK_GROUP_FIELDS.RELATED_PROJECT]?.value || '0');
        if (projectId > 0) {
          projectIdsWithTasks.add(projectId);
        }
      });

      if (projectIdsWithTasks.size === 0) {
        console.log('[getProjectsForUserList] No projects with unapproved tasks found');
        // Return empty early - no projects have tasks
        return [];
      }

      // Build filter for these project IDs
      const projectIds = Array.from(projectIdsWithTasks);
      tasksFilter = projectIds.map(id => `{${PROJECT_FIELDS.RECORD_ID}.EX.${id}}`).join(' OR ');
      console.log('[getProjectsForUserList] Found', projectIds.length, 'projects with unapproved tasks');
    } catch (error) {
      logError('Failed to fetch projects with tasks', error as Error, { userId, role });
      // On error, continue without task filter
      tasksFilter = undefined;
    }
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
  if (dateFilterClause) {
    whereClause = `(${whereClause}) AND (${dateFilterClause})`;
  }
  if (viewFilter) {
    whereClause = `(${whereClause}) AND (${viewFilter})`;
  }
  if (searchFilter) {
    whereClause = `(${whereClause}) AND (${searchFilter})`;
  }
  if (tasksFilter) {
    whereClause = `(${whereClause}) AND (${tasksFilter})`;
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
        PROJECT_FIELDS.INTAKE_FIRST_PASS_FINANCE_APPROVED, // Field 1831 - First pass result: "Approve" or "Reject"
        PROJECT_FIELDS.PRIOR_STATUS_WAS_REJECTED_BINARY, // Field 1930 - Ever rejected flag
        PROJECT_FIELDS.INTAKE_MISSING_ITEMS_COMBINED, // Field 1871 - Rejection reasons array
        PROJECT_FIELDS.INTAKE_FIRST_PASS_COMPLETE, // Field 1951 - First review timestamp
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
        // Task fields for filtering
        PROJECT_FIELDS.TOTAL_TASKS,
        PROJECT_FIELDS.UNAPPROVED_TASKS,
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
  if (!effectiveOfficeIds && ['office_leader', 'area_director', 'divisional', 'regional'].includes(role)) {
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

    case 'rejected':
      // Check BOTH INTAKE_STATUS and PROJECT_STATUS for "Rejected" (KCA Intake rejection)
      return `({${PROJECT_FIELDS.INTAKE_STATUS}.CT.'Rejected'} OR {${PROJECT_FIELDS.PROJECT_STATUS}.CT.'Rejected'})`;

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

  // Default to newest first by sales date for all views
  return [{ fieldId: PROJECT_FIELDS.SALES_DATE, order: 'DESC' as const }];
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
    return status.includes('Active') && !isTrueQB(onHold);
  }).length;
  
  // Calculate on hold projects
  const onHold = projects.filter((project: any) => {
    return isTrueQB(project[PROJECT_FIELDS.ON_HOLD]?.value);
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
    .filter((project: any) => isTrueQB(project[PROJECT_FIELDS.ON_HOLD]?.value))
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
  if (!effectiveOfficeIds && ['office_leader', 'area_director', 'divisional', 'regional'].includes(role)) {
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
    return status.includes('Active') && !isTrueQB(onHold);
  }).length;

  const onHold = data.filter((p: any) => isTrueQB(p[PROJECT_FIELDS.ON_HOLD]?.value)).length;

  const installsThisMonth = data.filter((p: any) => {
    const installDate = p[PROJECT_FIELDS.INSTALL_COMPLETED_DATE]?.value;
    if (!installDate) return false;
    // Use timezone-aware date parsing to prevent off-by-one errors
    const d = parseQuickbaseDate(installDate);
    if (!d) return false;
    return d.getMonth() === currentMonth && d.getFullYear() === currentYear;
  }).length;

  const holdReasons = data
    .filter((p: any) => isTrueQB(p[PROJECT_FIELDS.ON_HOLD]?.value))
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
  timeRange: 'lifetime' | 'ytd' | 'month' | 'week' | 'custom' | 'last_30' | 'last_90' | 'last_12_months' = 'lifetime',
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
  if (!effectiveOfficeIds && ['office_leader', 'area_director', 'divisional', 'regional'].includes(role)) {
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
function calculateBasicMetrics(allData: any[], installedInPeriod: any[], timeRange: 'lifetime' | 'ytd' | 'month' | 'week' | 'custom' | 'last_30' | 'last_90' | 'last_12_months') {
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
    return status.includes('Active') && !isTrueQB(onHold);
  }).length;

  // On Hold - always shows current state (not affected by time range filter)
  const onHold = allData.filter((p: any) => isTrueQB(p[PROJECT_FIELDS.ON_HOLD]?.value)).length;

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
    .filter((p: any) => isTrueQB(p[PROJECT_FIELDS.ON_HOLD]?.value))
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
  const onHoldProjects = allData.filter((p: any) => isTrueQB(p[PROJECT_FIELDS.ON_HOLD]?.value));
  const onHoldCommission = calculateCommission(onHoldProjects);

  // Pending: Active projects not yet installed (use allData for lifetime view)
  const pendingProjects = allData.filter((p: any) => {
    const status = p[PROJECT_FIELDS.PROJECT_STATUS]?.value || '';
    const onHold = p[PROJECT_FIELDS.ON_HOLD]?.value;
    const installed = p[PROJECT_FIELDS.INSTALL_COMPLETED_DATE]?.value;
    return status.includes('Active') && !isTrueQB(onHold) && !installed;
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
    const onHold = isTrueQB(project[PROJECT_FIELDS.ON_HOLD]?.value);
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
    if (isTrueQB(project[PROJECT_FIELDS.ON_HOLD]?.value)) {
      buckets.push('onHold');
    }

    // Rep Attention: Projects >90 days old OR on hold >7 days
    const age = parseInt(project[PROJECT_FIELDS.PROJECT_AGE]?.value || '0');
    const onHold = isTrueQB(project[PROJECT_FIELDS.ON_HOLD]?.value);
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
  const onHold = data.filter((p: any) => isTrueQB(p[PROJECT_FIELDS.ON_HOLD]?.value)).length;

  // Rep Attention: Projects >90 days old OR on hold >7 days
  const repAttention = data.filter((p: any) => {
    const age = parseInt(p[PROJECT_FIELDS.PROJECT_AGE]?.value || '0');
    const onHold = isTrueQB(p[PROJECT_FIELDS.ON_HOLD]?.value);
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
function calculateRetentionRate(data: any[], timeRange: 'lifetime' | 'ytd' | 'month' | 'week' | 'custom' | 'last_30' | 'last_90' | 'last_12_months' = 'lifetime'): { lifetime: number; period: number } {
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

  if (['office_leader', 'area_director', 'divisional', 'regional'].includes(role)) {
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

/**
 * Generic function to fetch projects with custom where clause
 * Used for reporting and analytics that need flexible filtering
 */
export async function fetchProjects(params: {
  userId: string;
  role: string;
  where?: string;
  select?: number[];
}) {
  const { userId, role, where, select } = params;

  // Get user access clause for authorization
  const userEmail = await getUserEmail(userId);
  let managedEmails: string[] | undefined;
  if (role === 'team_lead') {
    managedEmails = await getManagedUserEmails(userId);
  }

  // Get assigned office IDs for office-based roles
  let officeIds: number[] | undefined;
  if (['office_leader', 'area_director', 'divisional', 'regional'].includes(role)) {
    officeIds = await getAssignedOffices(userId);
  }

  // Build authorization clause
  const accessClause = buildProjectAccessClause(userEmail, role, officeIds, managedEmails);

  // Combine access clause with custom where clause
  const whereClause = where ? `(${accessClause}) AND (${where})` : accessClause;

  // Default to all fields if no select is provided
  const selectFields = select || Object.values(PROJECT_FIELDS);

  const result = await qbClient.queryRecords({
    from: QB_TABLE_PROJECTS,
    select: selectFields,
    where: whereClause,
  });

  return result.data;
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
  if (!effectiveOfficeIds && ['office_leader', 'area_director', 'divisional', 'regional'].includes(role)) {
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
  const QB_TABLE_NOTES = (process.env.QUICKBASE_TABLE_NOTES || 'bsb6bqt3b').trim();

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
  const QB_TABLE_NOTES = (process.env.QUICKBASE_TABLE_NOTES || 'bsb6bqt3b').trim();

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

// ============================================================================
// ANALYTICS QUERY FUNCTIONS
// ============================================================================

/**
 * Calculate monthly install counts for the last 6 months
 * Used for sparkline visualization in office comparison table
 */
function calculateMonthlyInstalls(completedProjects: any[]): Array<{ month: string; installs: number }> {
  const now = new Date();
  const monthlyData: Array<{ month: string; installs: number }> = [];
  
  // Generate last 6 months
  for (let i = 5; i >= 0; i--) {
    const date = new Date(now.getFullYear(), now.getMonth() - i, 1);
    const month = date.toISOString().slice(0, 7); // YYYY-MM format
    
    // Count installs in this month
    const installsInMonth = completedProjects.filter(project => {
      const installDate = project[PROJECT_FIELDS.INSTALL_COMPLETED_DATE]?.value;
      if (!installDate) return false;
      
      const install = parseQuickbaseDate(installDate);
      if (!install) return false;
      
      return install.getFullYear() === date.getFullYear() && 
             install.getMonth() === date.getMonth();
    }).length;
    
    monthlyData.push({
      month,
      installs: installsInMonth
    });
  }
  
  return monthlyData;
}

/**
 * Fetch and aggregate office-level metrics for analytics dashboard
 * Groups projects by office and calculates comprehensive metrics
 */
export async function getOfficeMetrics(
  userId: string,
  role: string,
  timeRange: 'lifetime' | 'ytd' | 'month' | 'week' | 'custom' | 'last_30' | 'last_90' | 'last_12_months' = 'ytd',
  officeIds?: number[],
  customDateRange?: { startDate: string; endDate: string },
  reqId?: string,
  timezone: string = 'America/New_York'
) {
  console.log('[getOfficeMetrics] === START ===');
  console.log('[getOfficeMetrics] userId:', userId);
  console.log('[getOfficeMetrics] role:', role);
  console.log('[getOfficeMetrics] timeRange:', timeRange);
  console.log('[getOfficeMetrics] officeIds:', officeIds);
  console.log('[getOfficeMetrics] customDateRange:', customDateRange);
  console.log('[getOfficeMetrics] reqId:', reqId);
  const startTime = Date.now();

  try {
    // Get assigned office IDs for office-based roles if no office IDs provided
    let effectiveOfficeIds = officeIds;
    if (!effectiveOfficeIds && ['office_leader', 'area_director', 'divisional', 'regional'].includes(role)) {
      console.log('[getOfficeMetrics] Fetching assigned offices for role:', role);
      effectiveOfficeIds = await getAssignedOffices(userId);
      console.log('[getOfficeMetrics] Assigned office IDs:', effectiveOfficeIds);
    }

    // Get user email for role-based filtering
    console.log('[getOfficeMetrics] Getting user email...');
    const userEmail = await getUserEmail(userId);
    console.log('[getOfficeMetrics] User email:', userEmail);

    // Build role-based access clause
    console.log('[getOfficeMetrics] Building access clause...');
    const accessClause = buildProjectAccessClause(userEmail, role, effectiveOfficeIds);
    console.log('[getOfficeMetrics] Access clause:', accessClause);

    // Build timezone-aware time range filter
    let timeFilter = '';
    if (timeRange === 'custom' && customDateRange) {
      timeFilter = `AND {${PROJECT_FIELDS.SALES_DATE}.OAF.'${customDateRange.startDate}'} AND {${PROJECT_FIELDS.SALES_DATE}.OBF.'${customDateRange.endDate}'}`;
    } else if (timeRange !== 'lifetime') {
      // Get current date/time in the target timezone (not UTC)
      const nowInTimezone = new Date(new Date().toLocaleString('en-US', { timeZone: timezone }));
      const currentYear = nowInTimezone.getFullYear();
      const currentMonth = nowInTimezone.getMonth();
      const currentDay = nowInTimezone.getDate();

      console.log('[getOfficeMetrics] Server current time:', {
        utc: new Date().toISOString(),
        timezone: timezone,
        dateInTimezone: nowInTimezone.toISOString(),
        year: currentYear,
        month: currentMonth + 1,
        day: currentDay
      });

      let startDate: string;
      let endDate: string;

      switch (timeRange) {
        case 'ytd':
          startDate = `${currentYear}-01-01`;
          endDate = `${currentYear}-${String(currentMonth + 1).padStart(2, '0')}-${String(currentDay).padStart(2, '0')}`;
          break;
        case 'month':
          startDate = `${currentYear}-${String(currentMonth + 1).padStart(2, '0')}-01`;
          endDate = `${currentYear}-${String(currentMonth + 1).padStart(2, '0')}-${String(currentDay).padStart(2, '0')}`;
          break;
        case 'week':
          const weekAgo = new Date(nowInTimezone);
          weekAgo.setDate(weekAgo.getDate() - 7);
          startDate = `${weekAgo.getFullYear()}-${String(weekAgo.getMonth() + 1).padStart(2, '0')}-${String(weekAgo.getDate()).padStart(2, '0')}`;
          endDate = `${currentYear}-${String(currentMonth + 1).padStart(2, '0')}-${String(currentDay).padStart(2, '0')}`;
          break;
        case 'last_30':
          const last30 = new Date(nowInTimezone);
          last30.setDate(last30.getDate() - 30);
          startDate = `${last30.getFullYear()}-${String(last30.getMonth() + 1).padStart(2, '0')}-${String(last30.getDate()).padStart(2, '0')}`;
          endDate = `${currentYear}-${String(currentMonth + 1).padStart(2, '0')}-${String(currentDay).padStart(2, '0')}`;
          break;
        case 'last_90':
          const last90 = new Date(nowInTimezone);
          last90.setDate(last90.getDate() - 90);
          startDate = `${last90.getFullYear()}-${String(last90.getMonth() + 1).padStart(2, '0')}-${String(last90.getDate()).padStart(2, '0')}`;
          endDate = `${currentYear}-${String(currentMonth + 1).padStart(2, '0')}-${String(currentDay).padStart(2, '0')}`;
          break;
        case 'last_12_months':
          const last12 = new Date(nowInTimezone);
          last12.setDate(last12.getDate() - 365);
          startDate = `${last12.getFullYear()}-${String(last12.getMonth() + 1).padStart(2, '0')}-${String(last12.getDate()).padStart(2, '0')}`;
          endDate = `${currentYear}-${String(currentMonth + 1).padStart(2, '0')}-${String(currentDay).padStart(2, '0')}`;
          break;
        default:
          startDate = '';
          endDate = '';
      }
      
      if (startDate && endDate) {
        timeFilter = `AND {${PROJECT_FIELDS.SALES_DATE}.OAF.'${startDate}'} AND {${PROJECT_FIELDS.SALES_DATE}.OBF.'${endDate}'}`;
        console.log('[getOfficeMetrics] Date filter:', {
          timeRange,
          timezone,
          startDate,
          endDate,
          timeFilter
        });
      }
    }

    const whereClause = `${accessClause} ${timeFilter}`.trim();
    console.log('[getOfficeMetrics] WHERE clause:', whereClause);
    console.log('[getOfficeMetrics] Querying QuickBase...');

    // Query projects with office and metrics fields
    const response = await qbClient.queryRecords({
      from: QB_TABLE_PROJECTS, // Projects table
      where: whereClause,
      select: [
        PROJECT_FIELDS.RECORD_ID,
        PROJECT_FIELDS.PROJECT_STATUS,
        PROJECT_FIELDS.SALES_DATE,
        PROJECT_FIELDS.SYSTEM_SIZE_KW,
        PROJECT_FIELDS.GROSS_PPW,
        PROJECT_FIELDS.NET_PPW,
        PROJECT_FIELDS.COMMISSIONABLE_PPW,
        PROJECT_FIELDS.PROJECT_AGE,
        PROJECT_FIELDS.INTAKE_STATUS,
        PROJECT_FIELDS.ON_HOLD,
        PROJECT_FIELDS.INSTALL_COMPLETED_DATE,
        PROJECT_FIELDS.OFFICE_RECORD_ID,
        PROJECT_FIELDS.SALES_OFFICE,
        // Intake quality tracking fields (for closer scorecards)
        PROJECT_FIELDS.INTAKE_COMPLETED_DATE,                    // Field 461
        PROJECT_FIELDS.INTAKE_FIRST_PASS_FINANCE_APPROVED,       // Field 1831 - "Approve" or "Reject"
        PROJECT_FIELDS.INTAKE_FIRST_PASS_COMPLETE,               // Field 1951 - First review timestamp
        PROJECT_FIELDS.INTAKE_MISSING_ITEMS_COMBINED,            // Field 1871 - Rejection reasons
      ],
      options: {
        top: 5000, // Handle large datasets
      },
    });

    console.log('[getOfficeMetrics] QB response received, data length:', response.data?.length || 0);

    const projects = response.data || [];
    console.log('[getOfficeMetrics] Fetched projects:', projects.length);

    // Debug: Check date range of fetched projects
    if (projects.length > 0 && timeRange === 'month') {
      const salesDates = projects
        .map((p: Record<string, any>) => p[PROJECT_FIELDS.SALES_DATE]?.value)
        .filter(Boolean)
        .sort();
      console.log('[getOfficeMetrics] Project date range:', {
        earliest: salesDates[0],
        latest: salesDates[salesDates.length - 1],
        totalWithDates: salesDates.length
      });

      // Check Stevens office specifically
      const stevensProjects = projects.filter((p: Record<string, any>) =>
        p[PROJECT_FIELDS.SALES_OFFICE]?.value?.includes('Stevens')
      );
      const stevensDates = stevensProjects
        .map((p: Record<string, any>) => p[PROJECT_FIELDS.SALES_DATE]?.value)
        .filter(Boolean)
        .sort();
      console.log('[getOfficeMetrics] Stevens office projects:', {
        total: stevensProjects.length,
        earliest: stevensDates[0],
        latest: stevensDates[stevensDates.length - 1]
      });
    }

    // Group projects by office
    const officeGroups = new Map<number, any[]>();
    const officeNames = new Map<number, string>();
    let projectsWithoutOfficeId = 0;

    for (const project of projects) {
      const officeId = project[PROJECT_FIELDS.OFFICE_RECORD_ID]?.value;
      const officeName = project[PROJECT_FIELDS.SALES_OFFICE]?.value;

      if (officeId) {
        if (!officeGroups.has(officeId)) {
          officeGroups.set(officeId, []);
        }
        officeGroups.get(officeId)!.push(project);
        officeNames.set(officeId, officeName || `Office ${officeId}`);
      } else {
        projectsWithoutOfficeId++;
        console.warn('[getOfficeMetrics] Project missing OFFICE_RECORD_ID:', {
          projectId: project[PROJECT_FIELDS.PROJECT_ID]?.value,
          officeName,
          hasOfficeId: !!officeId
        });
      }
    }

    console.log('[getOfficeMetrics] Office groups created:', {
      totalOfficeGroups: officeGroups.size,
      projectsWithoutOfficeId,
      totalProjects: projects.length
    });

    if (projectsWithoutOfficeId > 0) {
      console.warn(`[getOfficeMetrics] ${projectsWithoutOfficeId} projects missing OFFICE_RECORD_ID out of ${projects.length} total projects`);
    }

    // Calculate metrics for each office
    const officeMetrics = Array.from(officeGroups.entries()).map(([officeId, officeProjects]) => {
      const totalProjects = officeProjects.length;
      
      // Calculate averages
      const systemSizes = officeProjects
        .map(p => p[PROJECT_FIELDS.SYSTEM_SIZE_KW]?.value)
        .filter(size => size != null);
      const avgSystemSize = systemSizes.length > 0 ? systemSizes.reduce((sum, size) => sum + size, 0) / systemSizes.length : 0;

      const grossPpws = officeProjects
        .map(p => p[PROJECT_FIELDS.GROSS_PPW]?.value)
        .filter(ppw => ppw != null);
      const avgGrossPpw = grossPpws.length > 0 ? grossPpws.reduce((sum, ppw) => sum + ppw, 0) / grossPpws.length : 0;

      const netPpws = officeProjects
        .map(p => p[PROJECT_FIELDS.NET_PPW]?.value)
        .filter(ppw => ppw != null);
      const avgNetPpw = netPpws.length > 0 ? netPpws.reduce((sum, ppw) => sum + ppw, 0) / netPpws.length : 0;

      const commissionablePpws = officeProjects
        .map(p => p[PROJECT_FIELDS.COMMISSIONABLE_PPW]?.value)
        .filter(ppw => ppw != null);
      const avgCommissionablePpw = commissionablePpws.length > 0 ? commissionablePpws.reduce((sum, ppw) => sum + ppw, 0) / commissionablePpws.length : 0;

      // Calculate cycle time for completed projects (days from sale to install completion)
      const completedProjects = officeProjects.filter(p => p[PROJECT_FIELDS.INSTALL_COMPLETED_DATE]?.value);
      const cycleTimes = completedProjects
        .map(p => {
          const installDate = p[PROJECT_FIELDS.INSTALL_COMPLETED_DATE]?.value;
          const salesDate = p[PROJECT_FIELDS.SALES_DATE]?.value;
          if (!installDate || !salesDate) return null;
          const cycleTime = (new Date(installDate).getTime() - new Date(salesDate).getTime()) / (1000 * 60 * 60 * 24);
          return Math.round(cycleTime);
        })
        .filter(time => time != null && time > 0) as number[];
      const avgCycleTime = cycleTimes.length > 0 ? cycleTimes.reduce((sum, time) => sum + time, 0) / cycleTimes.length : null;

      // Calculate intake approval rate (projects with completed intake)
      const approvedIntakes = officeProjects.filter(p =>
        p[PROJECT_FIELDS.INTAKE_COMPLETED_DATE]?.value
      ).length;
      const intakeApprovalRate = totalProjects > 0 ? (approvedIntakes / totalProjects) * 100 : 0;

      // Calculate intake quality metrics for closer scorecard
      const projectsWithFirstPass = officeProjects.filter(p =>
        p[PROJECT_FIELDS.INTAKE_FIRST_PASS_FINANCE_APPROVED]?.value
      );

      // First-time pass rate (KEY METRIC - clean deals approved on first attempt)
      const firstTimePass = projectsWithFirstPass.filter(p =>
        p[PROJECT_FIELDS.INTAKE_FIRST_PASS_FINANCE_APPROVED]?.value === 'Approve'
      ).length;
      const firstTimePassRate = projectsWithFirstPass.length > 0
        ? (firstTimePass / projectsWithFirstPass.length) * 100
        : 0;

      // Rejection rate (projects rejected on first attempt)
      const rejections = projectsWithFirstPass.filter(p =>
        p[PROJECT_FIELDS.INTAKE_FIRST_PASS_FINANCE_APPROVED]?.value === 'Reject'
      ).length;
      const rejectionRate = projectsWithFirstPass.length > 0
        ? (rejections / projectsWithFirstPass.length) * 100
        : 0;

      // Resubmit success rate (of rejected projects, how many eventually got approved)
      const rejectedProjects = officeProjects.filter(p =>
        p[PROJECT_FIELDS.INTAKE_FIRST_PASS_FINANCE_APPROVED]?.value === 'Reject'
      );
      const eventuallyApproved = rejectedProjects.filter(p =>
        p[PROJECT_FIELDS.INTAKE_COMPLETED_DATE]?.value
      ).length;
      const resubmitSuccessRate = rejectedProjects.length > 0
        ? (eventuallyApproved / rejectedProjects.length) * 100
        : 100;

      // Average resolution time (only for rejected projects that were eventually approved)
      const resolutionTimes = rejectedProjects
        .map(p => {
          const firstReview = p[PROJECT_FIELDS.INTAKE_FIRST_PASS_COMPLETE]?.value;
          const finalApproval = p[PROJECT_FIELDS.INTAKE_COMPLETED_DATE]?.value;
          if (!firstReview || !finalApproval) return null;

          const days = (new Date(finalApproval).getTime() - new Date(firstReview).getTime())
            / (1000 * 60 * 60 * 24);
          return Math.round(days);
        })
        .filter(time => time !== null && time > 0) as number[];

      const avgResolutionTime = resolutionTimes.length > 0
        ? resolutionTimes.reduce((sum, time) => sum + time, 0) / resolutionTimes.length
        : null;

      // Top rejection reasons (most common issues across all projects)
      const reasonCounts: Record<string, number> = {};
      officeProjects.forEach(p => {
        const reasons = p[PROJECT_FIELDS.INTAKE_MISSING_ITEMS_COMBINED]?.value;
        if (Array.isArray(reasons)) {
          reasons.forEach(reason => {
            reasonCounts[reason] = (reasonCounts[reason] || 0) + 1;
          });
        }
      });

      const topRejectionReasons = Object.entries(reasonCounts)
        .sort((a, b) => b[1] - a[1])
        .slice(0, 5)
        .map(([reason, count]) => ({ reason, count }));

      // Count by status
      const statusCounts = officeProjects.reduce((acc, project) => {
        const status = project[PROJECT_FIELDS.PROJECT_STATUS]?.value || 'Unknown';
        acc[status] = (acc[status] || 0) + 1;
        return acc;
      }, {} as Record<string, number>);

      const activeProjects = statusCounts['Active'] || 0;
      const projectsSubmitted = statusCounts['Submitted'] || 0;
      const projectsApproved = statusCounts['Approved'] || 0;
      // Count projects currently rejected and awaiting resubmit (not yet fixed/approved)
      const projectsRejected = officeProjects.filter(p =>
        p[PROJECT_FIELDS.INTAKE_FIRST_PASS_FINANCE_APPROVED]?.value === 'Reject' &&
        !p[PROJECT_FIELDS.INTAKE_COMPLETED_DATE]?.value
      ).length;
      const installs = completedProjects.length;

      // All hold types from PROJECT_STATUS field (detailed breakdown)
      const pendingKcaProjects = statusCounts['Pending KCA'] || 0;
      const financeHoldProjects = statusCounts['Finance Hold'] || 0;
      const pendingCancelProjects = statusCounts['Pending Cancel'] || 0;
      const roofHoldProjects = statusCounts['Roof Hold'] || 0;
      const generalOnHoldProjects = statusCounts['On Hold'] || 0;
      const customerHoldProjects = statusCounts['Customer Hold'] || 0;

      // Total holds: sum of all hold types
      const onHoldProjects = generalOnHoldProjects + financeHoldProjects + roofHoldProjects + customerHoldProjects;
      const holds = onHoldProjects;

      // Total cancellations: includes both 'Cancelled' and 'Pending Cancel'
      const cancelledProjects = (statusCounts['Cancelled'] || 0) + pendingCancelProjects;

      // Calculate monthly install data for sparklines (last 6 months)
      const monthlyInstalls = calculateMonthlyInstalls(completedProjects);

      return {
        officeName: officeNames.get(officeId) || `Office ${officeId}`,
        officeId,
        totalProjects,
        avgSystemSize: Math.round(avgSystemSize * 100) / 100,
        avgGrossPpw: Math.round(avgGrossPpw * 100) / 100,
        avgNetPpw: Math.round(avgNetPpw * 100) / 100,
        avgCommissionablePpw: Math.round(avgCommissionablePpw * 100) / 100,
        avgCycleTime: avgCycleTime ? Math.round(avgCycleTime) : null,
        intakeApprovalRate: Math.round(intakeApprovalRate * 100) / 100,
        firstTimePassRate: Math.round(firstTimePassRate * 100) / 100,
        rejectionRate: Math.round(rejectionRate * 100) / 100,
        resubmitSuccessRate: Math.round(resubmitSuccessRate * 100) / 100,
        avgResolutionTime: avgResolutionTime ? Math.round(avgResolutionTime) : null,
        topRejectionReasons,
        activeProjects,
        cancelledProjects,
        onHoldProjects,
        projectsSubmitted,
        projectsApproved,
        projectsRejected,
        installs,
        holds,
        monthlyInstalls,
        pendingKcaProjects,
        financeHoldProjects,
        pendingCancelProjects,
        roofHoldProjects,
      };
    });

    // Sort by total projects descending
    officeMetrics.sort((a, b) => b.totalProjects - a.totalProjects);

    const duration = Date.now() - startTime;
    console.log('[getOfficeMetrics] COMPLETED - duration:', duration, 'ms, offices:', officeMetrics.length);

    if (officeMetrics.length === 0) {
      console.warn('[getOfficeMetrics] Returning 0 offices - check time range filter');
    }

    return officeMetrics;
  } catch (error) {
    logError('Failed to fetch office metrics', error as Error, { userId, role, timeRange, officeIds, reqId });
    throw error;
  }
}

/**
 * Fetch and aggregate per-rep performance metrics for analytics dashboard
 * Groups projects by closer and setter, handling cases where same person is both
 */
export async function getRepPerformance(
  userId: string,
  role: string,
  timeRange: 'lifetime' | 'ytd' | 'month' | 'week' | 'custom' | 'last_30' | 'last_90' | 'last_12_months' = 'ytd',
  officeIds?: number[],
  customDateRange?: { startDate: string; endDate: string },
  reqId?: string,
  timezone: string = 'America/New_York'
) {
  console.log('[getRepPerformance] START - userId:', userId, 'role:', role, 'timeRange:', timeRange, 'officeIds:', officeIds, 'reqId:', reqId);
  const startTime = Date.now();

  try {
    // Get assigned office IDs for office-based roles if no office IDs provided
    let effectiveOfficeIds = officeIds;
    if (!effectiveOfficeIds && ['office_leader', 'area_director', 'divisional', 'regional'].includes(role)) {
      effectiveOfficeIds = await getAssignedOffices(userId);
    }

    // Get user email for role-based filtering
    const userEmail = await getUserEmail(userId);

    // Build role-based access clause
    const accessClause = buildProjectAccessClause(userEmail, role, effectiveOfficeIds);
    console.log('[getRepPerformance] Access clause:', accessClause);

    // Build timezone-aware time range filter
    let timeFilter = '';
    if (timeRange === 'custom' && customDateRange) {
      timeFilter = `AND {${PROJECT_FIELDS.SALES_DATE}.OAF.'${customDateRange.startDate}'} AND {${PROJECT_FIELDS.SALES_DATE}.OBF.'${customDateRange.endDate}'}`;
    } else if (timeRange !== 'lifetime') {
      const now = new Date();
      const currentYear = now.getFullYear();
      const currentMonth = now.getMonth();
      
      let startDate: string;
      let endDate: string;
      
      switch (timeRange) {
        case 'ytd':
          startDate = `${currentYear}-01-01`;
          endDate = new Intl.DateTimeFormat('en-CA', {
            timeZone: timezone,
            year: 'numeric',
            month: '2-digit',
            day: '2-digit',
          }).format(now);
          break;
        case 'month':
          const monthStart = new Date(currentYear, currentMonth, 1);
          endDate = new Intl.DateTimeFormat('en-CA', {
            timeZone: timezone,
            year: 'numeric',
            month: '2-digit',
            day: '2-digit',
          }).format(now);
          startDate = new Intl.DateTimeFormat('en-CA', {
            timeZone: timezone,
            year: 'numeric',
            month: '2-digit',
            day: '2-digit',
          }).format(monthStart);
          break;
        case 'week':
          const oneWeekAgo = new Date(now.getTime() - 7 * 24 * 60 * 60 * 1000);
          startDate = new Intl.DateTimeFormat('en-CA', {
            timeZone: timezone,
            year: 'numeric',
            month: '2-digit',
            day: '2-digit',
          }).format(oneWeekAgo);
          endDate = new Intl.DateTimeFormat('en-CA', {
            timeZone: timezone,
            year: 'numeric',
            month: '2-digit',
            day: '2-digit',
          }).format(now);
          break;
        case 'last_30':
          const last30Start = new Date(now);
          last30Start.setDate(last30Start.getDate() - 30);
          startDate = new Intl.DateTimeFormat('en-CA', {
            timeZone: timezone,
            year: 'numeric',
            month: '2-digit',
            day: '2-digit',
          }).format(last30Start);
          endDate = new Intl.DateTimeFormat('en-CA', {
            timeZone: timezone,
            year: 'numeric',
            month: '2-digit',
            day: '2-digit',
          }).format(now);
          break;
        case 'last_90':
          const last90Start = new Date(now);
          last90Start.setDate(last90Start.getDate() - 90);
          startDate = new Intl.DateTimeFormat('en-CA', {
            timeZone: timezone,
            year: 'numeric',
            month: '2-digit',
            day: '2-digit',
          }).format(last90Start);
          endDate = new Intl.DateTimeFormat('en-CA', {
            timeZone: timezone,
            year: 'numeric',
            month: '2-digit',
            day: '2-digit',
          }).format(now);
          break;
        case 'last_12_months':
          const last12Start = new Date(now);
          last12Start.setDate(last12Start.getDate() - 365);
          startDate = new Intl.DateTimeFormat('en-CA', {
            timeZone: timezone,
            year: 'numeric',
            month: '2-digit',
            day: '2-digit',
          }).format(last12Start);
          endDate = new Intl.DateTimeFormat('en-CA', {
            timeZone: timezone,
            year: 'numeric',
            month: '2-digit',
            day: '2-digit',
          }).format(now);
          break;
        default:
          startDate = '';
          endDate = '';
      }
      
      if (startDate && endDate) {
        timeFilter = `AND {${PROJECT_FIELDS.SALES_DATE}.OAF.'${startDate}'} AND {${PROJECT_FIELDS.SALES_DATE}.OBF.'${endDate}'}`;
      }
    }

    const whereClause = `${accessClause} ${timeFilter}`.trim();

    // Query projects with rep and metrics fields
    const response = await qbClient.queryRecords({
      from: QB_TABLE_PROJECTS, // Projects table
      where: whereClause,
      select: [
        PROJECT_FIELDS.RECORD_ID,
        PROJECT_FIELDS.PROJECT_STATUS,
        PROJECT_FIELDS.SALES_DATE,
        PROJECT_FIELDS.SYSTEM_SIZE_KW,
        PROJECT_FIELDS.GROSS_PPW,
        PROJECT_FIELDS.NET_PPW,
        PROJECT_FIELDS.COMMISSIONABLE_PPW,
        PROJECT_FIELDS.PROJECT_AGE,
        PROJECT_FIELDS.INTAKE_STATUS,
        PROJECT_FIELDS.ON_HOLD,
        PROJECT_FIELDS.INSTALL_COMPLETED_DATE,
        PROJECT_FIELDS.OFFICE_RECORD_ID,
        PROJECT_FIELDS.SALES_OFFICE,
        PROJECT_FIELDS.CLOSER_NAME,
        PROJECT_FIELDS.CLOSER_EMAIL,
        PROJECT_FIELDS.SETTER_NAME,
        PROJECT_FIELDS.SETTER_EMAIL,
        // Intake quality tracking fields (for closer scorecards)
        PROJECT_FIELDS.INTAKE_COMPLETED_DATE,                    // Field 461
        PROJECT_FIELDS.INTAKE_FIRST_PASS_FINANCE_APPROVED,       // Field 1831 - "Approve" or "Reject"
        PROJECT_FIELDS.INTAKE_FIRST_PASS_COMPLETE,               // Field 1951 - First review timestamp
        PROJECT_FIELDS.INTAKE_MISSING_ITEMS_COMBINED,            // Field 1871 - Rejection reasons
      ],
      options: {
        top: 5000, // Handle large datasets
      },
    });

    const projects = response.data || [];
    console.log('[getRepPerformance] Fetched projects:', projects.length);

    // Group projects by rep (closer and setter) - initially by email
    const repGroupsByEmail = new Map<string, { projects: any[], role: 'closer' | 'setter', repName: string, repEmail: string }>();

    for (const project of projects) {
      // Process closer
      const closerEmail = project[PROJECT_FIELDS.CLOSER_EMAIL]?.value;
      const closerName = project[PROJECT_FIELDS.CLOSER_NAME]?.value;
      if (closerEmail && closerName) {
        const key = `closer:${closerEmail}`;
        if (!repGroupsByEmail.has(key)) {
          repGroupsByEmail.set(key, {
            projects: [],
            role: 'closer',
            repName: closerName,
            repEmail: closerEmail,
          });
        }
        repGroupsByEmail.get(key)!.projects.push(project);
      }

      // Process setter
      const setterEmail = project[PROJECT_FIELDS.SETTER_EMAIL]?.value;
      const setterName = project[PROJECT_FIELDS.SETTER_NAME]?.value;
      if (setterEmail && setterName) {
        const key = `setter:${setterEmail}`;
        if (!repGroupsByEmail.has(key)) {
          repGroupsByEmail.set(key, {
            projects: [],
            role: 'setter',
            repName: setterName,
            repEmail: setterEmail,
          });
        }
        repGroupsByEmail.get(key)!.projects.push(project);
      }
    }

    // Merge duplicate reps (same name/role but different emails)
    const repGroups = new Map<string, { projects: any[], role: 'closer' | 'setter', repName: string, repEmail: string }>();

    for (const [emailKey, repData] of Array.from(repGroupsByEmail.entries())) {
      const nameKey = `${repData.role}:${repData.repName.toLowerCase().trim()}`;

      if (repGroups.has(nameKey)) {
        // Merge projects with existing entry (same person, different email)
        const existing = repGroups.get(nameKey)!;
        existing.projects.push(...repData.projects);

        // Use the email from the most recent project
        const mostRecentProject = [...existing.projects].sort((a, b) => {
          const dateA = a[PROJECT_FIELDS.SALES_DATE]?.value || '';
          const dateB = b[PROJECT_FIELDS.SALES_DATE]?.value || '';
          return dateB.localeCompare(dateA);
        })[0];

        if (mostRecentProject) {
          const isCloser = mostRecentProject[PROJECT_FIELDS.CLOSER_NAME]?.value?.toLowerCase().trim() === repData.repName.toLowerCase().trim();
          existing.repEmail = isCloser
            ? (mostRecentProject[PROJECT_FIELDS.CLOSER_EMAIL]?.value || existing.repEmail)
            : (mostRecentProject[PROJECT_FIELDS.SETTER_EMAIL]?.value || existing.repEmail);
        }
      } else {
        // New rep entry
        repGroups.set(nameKey, { ...repData });
      }
    }

    // Calculate metrics for each rep
    const repMetrics = Array.from(repGroups.entries()).map(([key, repData]) => {
      const { projects: repProjects, role, repName, repEmail } = repData;
      const totalProjects = repProjects.length;

      // Determine office from most recent project (by sales date)
      const sortedProjects = [...repProjects].sort((a, b) => {
        const dateA = a[PROJECT_FIELDS.SALES_DATE]?.value || '';
        const dateB = b[PROJECT_FIELDS.SALES_DATE]?.value || '';
        return dateB.localeCompare(dateA); // descending - most recent first
      });
      const mostRecentProject = sortedProjects[0];
      const officeId = mostRecentProject?.[PROJECT_FIELDS.OFFICE_RECORD_ID]?.value || null;
      const officeName = mostRecentProject?.[PROJECT_FIELDS.SALES_OFFICE]?.value || null;
      
      // Calculate averages (same logic as office metrics)
      const systemSizes = repProjects
        .map(p => p[PROJECT_FIELDS.SYSTEM_SIZE_KW]?.value)
        .filter(size => size != null);
      const avgSystemSize = systemSizes.length > 0 ? systemSizes.reduce((sum, size) => sum + size, 0) / systemSizes.length : 0;

      const grossPpws = repProjects
        .map(p => p[PROJECT_FIELDS.GROSS_PPW]?.value)
        .filter(ppw => ppw != null);
      const avgGrossPpw = grossPpws.length > 0 ? grossPpws.reduce((sum, ppw) => sum + ppw, 0) / grossPpws.length : 0;

      const netPpws = repProjects
        .map(p => p[PROJECT_FIELDS.NET_PPW]?.value)
        .filter(ppw => ppw != null);
      const avgNetPpw = netPpws.length > 0 ? netPpws.reduce((sum, ppw) => sum + ppw, 0) / netPpws.length : 0;

      const commissionablePpws = repProjects
        .map(p => p[PROJECT_FIELDS.COMMISSIONABLE_PPW]?.value)
        .filter(ppw => ppw != null);
      const avgCommissionablePpw = commissionablePpws.length > 0 ? commissionablePpws.reduce((sum, ppw) => sum + ppw, 0) / commissionablePpws.length : 0;

      // Calculate cycle time for completed projects (days from sale to install completion)
      const completedProjects = repProjects.filter(p => p[PROJECT_FIELDS.INSTALL_COMPLETED_DATE]?.value);
      const cycleTimes = completedProjects
        .map(p => {
          const installDate = p[PROJECT_FIELDS.INSTALL_COMPLETED_DATE]?.value;
          const salesDate = p[PROJECT_FIELDS.SALES_DATE]?.value;
          if (!installDate || !salesDate) return null;
          const cycleTime = (new Date(installDate).getTime() - new Date(salesDate).getTime()) / (1000 * 60 * 60 * 24);
          return Math.round(cycleTime);
        })
        .filter(time => time != null && time > 0) as number[];
      const avgCycleTime = cycleTimes.length > 0 ? cycleTimes.reduce((sum, time) => sum + time, 0) / cycleTimes.length : null;

      // Calculate intake approval rate (projects with completed intake)
      const approvedIntakes = repProjects.filter(p =>
        p[PROJECT_FIELDS.INTAKE_COMPLETED_DATE]?.value
      ).length;
      const intakeApprovalRate = totalProjects > 0 ? (approvedIntakes / totalProjects) * 100 : 0;

      // Calculate intake quality metrics for closer scorecard
      const projectsWithFirstPass = repProjects.filter(p =>
        p[PROJECT_FIELDS.INTAKE_FIRST_PASS_FINANCE_APPROVED]?.value
      );

      // First-time pass rate (KEY METRIC - clean deals approved on first attempt)
      const firstTimePass = projectsWithFirstPass.filter(p =>
        p[PROJECT_FIELDS.INTAKE_FIRST_PASS_FINANCE_APPROVED]?.value === 'Approve'
      ).length;
      const firstTimePassRate = projectsWithFirstPass.length > 0
        ? (firstTimePass / projectsWithFirstPass.length) * 100
        : 0;

      // Rejection rate (projects rejected on first attempt)
      const rejections = projectsWithFirstPass.filter(p =>
        p[PROJECT_FIELDS.INTAKE_FIRST_PASS_FINANCE_APPROVED]?.value === 'Reject'
      ).length;
      const rejectionRate = projectsWithFirstPass.length > 0
        ? (rejections / projectsWithFirstPass.length) * 100
        : 0;

      // Resubmit success rate (of rejected projects, how many eventually got approved)
      const rejectedProjects = repProjects.filter(p =>
        p[PROJECT_FIELDS.INTAKE_FIRST_PASS_FINANCE_APPROVED]?.value === 'Reject'
      );
      const eventuallyApproved = rejectedProjects.filter(p =>
        p[PROJECT_FIELDS.INTAKE_COMPLETED_DATE]?.value
      ).length;
      const resubmitSuccessRate = rejectedProjects.length > 0
        ? (eventuallyApproved / rejectedProjects.length) * 100
        : 100;

      // Average resolution time (only for rejected projects that were eventually approved)
      const resolutionTimes = rejectedProjects
        .map(p => {
          const firstReview = p[PROJECT_FIELDS.INTAKE_FIRST_PASS_COMPLETE]?.value;
          const finalApproval = p[PROJECT_FIELDS.INTAKE_COMPLETED_DATE]?.value;
          if (!firstReview || !finalApproval) return null;

          const days = (new Date(finalApproval).getTime() - new Date(firstReview).getTime())
            / (1000 * 60 * 60 * 24);
          return Math.round(days);
        })
        .filter(time => time !== null && time > 0) as number[];

      const avgResolutionTime = resolutionTimes.length > 0
        ? resolutionTimes.reduce((sum, time) => sum + time, 0) / resolutionTimes.length
        : null;

      // Top rejection reasons (most common issues for this rep)
      const reasonCounts: Record<string, number> = {};
      repProjects.forEach(p => {
        const reasons = p[PROJECT_FIELDS.INTAKE_MISSING_ITEMS_COMBINED]?.value;
        if (Array.isArray(reasons)) {
          reasons.forEach(reason => {
            reasonCounts[reason] = (reasonCounts[reason] || 0) + 1;
          });
        }
      });

      const topRejectionReasons = Object.entries(reasonCounts)
        .sort((a, b) => b[1] - a[1])
        .slice(0, 5)
        .map(([reason, count]) => ({ reason, count }));

      // Count by status
      const statusCounts = repProjects.reduce((acc, project) => {
        const status = project[PROJECT_FIELDS.PROJECT_STATUS]?.value || 'Unknown';
        acc[status] = (acc[status] || 0) + 1;
        return acc;
      }, {} as Record<string, number>);

      const activeProjects = statusCounts['Active'] || 0;

      // Cancellations: Include both 'Cancelled' and 'Pending Cancel' statuses
      const cancelledProjects = (statusCounts['Cancelled'] || 0) + (statusCounts['Pending Cancel'] || 0);

      // Holds: Include all hold types from PROJECT_STATUS field
      const onHoldProjects = (statusCounts['On Hold'] || 0)
        + (statusCounts['Finance Hold'] || 0)
        + (statusCounts['Roof Hold'] || 0)
        + (statusCounts['Customer Hold'] || 0);

      const projectsSubmitted = statusCounts['Submitted'] || 0;
      const projectsApproved = statusCounts['Approved'] || 0;
      // Count projects currently rejected and awaiting resubmit (not yet fixed/approved)
      const projectsRejected = repProjects.filter(p =>
        p[PROJECT_FIELDS.INTAKE_FIRST_PASS_FINANCE_APPROVED]?.value === 'Reject' &&
        !p[PROJECT_FIELDS.INTAKE_COMPLETED_DATE]?.value
      ).length;
      const installs = completedProjects.length;
      const holds = onHoldProjects;

      // Calculate cancellation and hold rates as percentages
      const cancellationRate = totalProjects > 0
        ? (cancelledProjects / totalProjects) * 100
        : 0;
      const holdRate = totalProjects > 0
        ? (onHoldProjects / totalProjects) * 100
        : 0;

      return {
        repId: repEmail, // Use email as unique identifier
        repName,
        repEmail,
        role,
        officeId,
        officeName,
        totalProjects,
        avgSystemSize: Math.round(avgSystemSize * 100) / 100,
        avgGrossPpw: Math.round(avgGrossPpw * 100) / 100,
        avgNetPpw: Math.round(avgNetPpw * 100) / 100,
        avgCommissionablePpw: Math.round(avgCommissionablePpw * 100) / 100,
        avgCycleTime: avgCycleTime ? Math.round(avgCycleTime) : null,
        intakeApprovalRate: Math.round(intakeApprovalRate * 100) / 100,
        firstTimePassRate: Math.round(firstTimePassRate * 100) / 100,
        rejectionRate: Math.round(rejectionRate * 100) / 100,
        resubmitSuccessRate: Math.round(resubmitSuccessRate * 100) / 100,
        avgResolutionTime: avgResolutionTime ? Math.round(avgResolutionTime) : null,
        topRejectionReasons,
        activeProjects,
        cancelledProjects,
        onHoldProjects,
        projectsSubmitted,
        projectsApproved,
        projectsRejected,
        installs,
        holds,
        cancellationRate: Math.round(cancellationRate * 100) / 100,
        holdRate: Math.round(holdRate * 100) / 100,
      };
    });

    // Sort by total projects descending
    repMetrics.sort((a, b) => b.totalProjects - a.totalProjects);

    const duration = Date.now() - startTime;
    console.log('[getRepPerformance] COMPLETED - duration:', duration, 'ms, reps:', repMetrics.length);

    return repMetrics;
  } catch (error) {
    logError('Failed to fetch rep performance metrics', error as Error, { userId, role, timeRange, officeIds, reqId });
    throw error;
  }
}

/**
 * Get install tracker data for current 3-week window (last week, this week, next week)
 * Independent of page date filters - always shows current install activity
 * Respects office and role-based access controls
 */
export async function getPipelineForecast(
  userId: string,
  role: string,
  officeIds?: number[],
  includeDetails: boolean = true,
  reqId?: string
) {
  console.log('[getPipelineForecast] START - userId:', userId, 'role:', role, 'officeIds:', officeIds, 'reqId:', reqId);
  const startTime = Date.now();

  try {
    // Get assigned office IDs for office-based roles if no office IDs provided
    let effectiveOfficeIds = officeIds;
    if (!effectiveOfficeIds && ['office_leader', 'area_director', 'divisional', 'regional'].includes(role)) {
      effectiveOfficeIds = await getAssignedOffices(userId);
    }

    // Get user email for role-based filtering
    const userEmail = await getUserEmail(userId);

    // Build role-based access clause
    const accessClause = buildProjectAccessClause(userEmail, role, effectiveOfficeIds);
    console.log('[getPipelineForecast] Access clause:', accessClause);

    // Query all projects with access - status doesn't matter, only install dates
    const whereClause = accessClause.trim();

    // Query projects with actual install date fields (not estimates)
    const response = await qbClient.queryRecords({
      from: QB_TABLE_PROJECTS, // Projects table
      where: whereClause,
      select: [
        PROJECT_FIELDS.RECORD_ID,
        PROJECT_FIELDS.PROJECT_ID,
        PROJECT_FIELDS.CUSTOMER_NAME,
        PROJECT_FIELDS.SYSTEM_SIZE_KW,
        PROJECT_FIELDS.PROJECT_STATUS,
        PROJECT_FIELDS.INSTALL_COMPLETED_DATE, // Field 534 - Actual completion date
        PROJECT_FIELDS.INSTALL_SCHEDULED_START_DATE, // Field 178 - Primary scheduled date (49.2%)
        PROJECT_FIELDS.INSTALL_SCHEDULED_DATE_CAPTURE, // Field 710 - Backup scheduled date (54.4%)
      ],
      options: {
        top: 5000, // Handle large datasets
      },
    });

    const projects = response.data || [];
    console.log('[getPipelineForecast] Fetched active projects:', projects.length);

    // Calculate week boundaries in America/New_York timezone
    const now = new Date();
    const timezone = 'America/New_York';
    const nowInTimezone = new Date(now.toLocaleString('en-US', { timeZone: timezone }));

    // Get start of this week (Sunday at 00:00)
    const thisWeekStart = new Date(nowInTimezone);
    thisWeekStart.setDate(thisWeekStart.getDate() - thisWeekStart.getDay());
    thisWeekStart.setHours(0, 0, 0, 0);

    // Get end of this week (Saturday at 23:59:59)
    const thisWeekEnd = new Date(thisWeekStart);
    thisWeekEnd.setDate(thisWeekEnd.getDate() + 6);
    thisWeekEnd.setHours(23, 59, 59, 999);

    // Get start/end of last week
    const lastWeekStart = new Date(thisWeekStart);
    lastWeekStart.setDate(lastWeekStart.getDate() - 7);
    const lastWeekEnd = new Date(thisWeekStart);
    lastWeekEnd.setMilliseconds(lastWeekEnd.getMilliseconds() - 1);

    // Get start/end of next week
    const nextWeekStart = new Date(thisWeekEnd);
    nextWeekStart.setMilliseconds(nextWeekStart.getMilliseconds() + 1);
    const nextWeekEnd = new Date(nextWeekStart);
    nextWeekEnd.setDate(nextWeekEnd.getDate() + 6);

    let lastWeekCount = 0;
    let thisWeekCount = 0;
    let nextWeekCount = 0;
    let anyForecastCount = 0;
    const forecastProjects: any[] = includeDetails ? [] : [];

    for (const project of projects) {
      const recordId = project[PROJECT_FIELDS.RECORD_ID]?.value;
      const projectId = project[PROJECT_FIELDS.PROJECT_ID]?.value;
      const customerName = project[PROJECT_FIELDS.CUSTOMER_NAME]?.value;
      const systemSize = project[PROJECT_FIELDS.SYSTEM_SIZE_KW]?.value || 0;

      // For "Last Week": Use actual COMPLETED date
      const completedDateValue = project[PROJECT_FIELDS.INSTALL_COMPLETED_DATE]?.value;
      const completedDate = completedDateValue ? parseQuickbaseDate(completedDateValue) : null;

      // For "This Week" and "Next Week": Use SCHEDULED dates (not estimates)
      let scheduledDate: Date | null = null;
      let scheduledSource: 'primary' | 'capture' | null = null;

      // Try INSTALL_SCHEDULED_START_DATE first (Field 178 - matches milestone tracker)
      const scheduledStartValue = project[PROJECT_FIELDS.INSTALL_SCHEDULED_START_DATE]?.value;
      const scheduledCaptureValue = project[PROJECT_FIELDS.INSTALL_SCHEDULED_DATE_CAPTURE]?.value;

      if (scheduledStartValue) {
        scheduledDate = parseQuickbaseDate(scheduledStartValue);
        scheduledSource = 'primary';
      }
      // Fall back to INSTALL_SCHEDULED_DATE_CAPTURE (Field 710)
      else if (scheduledCaptureValue) {
        scheduledDate = parseQuickbaseDate(scheduledCaptureValue);
        scheduledSource = 'capture';
      }

      // Determine which week this project falls into
      let weekBucket: 'lastWeek' | 'thisWeek' | 'nextWeek' | null = null;
      let dateUsed: Date | null = null;
      let dateSource: 'completed' | 'scheduled-primary' | 'scheduled-capture' | null = null;

      // Last Week: Check if install was COMPLETED last week
      if (completedDate && completedDate >= lastWeekStart && completedDate <= lastWeekEnd) {
        lastWeekCount++;
        weekBucket = 'lastWeek';
        dateUsed = completedDate;
        dateSource = 'completed';
        anyForecastCount++;
      }
      // This Week: Check if install is SCHEDULED for this week
      else if (scheduledDate && scheduledDate >= thisWeekStart && scheduledDate <= thisWeekEnd) {
        thisWeekCount++;
        weekBucket = 'thisWeek';
        dateUsed = scheduledDate;
        dateSource = scheduledSource === 'primary' ? 'scheduled-primary' : 'scheduled-capture';
        anyForecastCount++;
      }
      // Next Week: Check if install is SCHEDULED for next week
      else if (scheduledDate && scheduledDate >= nextWeekStart && scheduledDate <= nextWeekEnd) {
        nextWeekCount++;
        weekBucket = 'nextWeek';
        dateUsed = scheduledDate;
        dateSource = scheduledSource === 'primary' ? 'scheduled-primary' : 'scheduled-capture';
        anyForecastCount++;
      }

      if (includeDetails && weekBucket && dateUsed) {
        const projectDetails = {
          recordId,
          projectId,
          customerName,
          systemSize,
          installDate: dateUsed.toISOString().split('T')[0],
          completedDate: completedDateValue || null,
          scheduledStartDate: scheduledStartValue || null,
          scheduledCaptureDate: scheduledCaptureValue || null,
          dateSource,
          weekBucket,
        };
        forecastProjects.push(projectDetails);
      }
    }

    // Calculate metadata
    const totalActiveProjects = projects.length;
    const projectsWithForecast = anyForecastCount;
    const projectsWithinWeeks = lastWeekCount + thisWeekCount + nextWeekCount;

    const duration = Date.now() - startTime;
    console.log('[getPipelineForecast] COMPLETED - duration:', duration, 'ms, forecast projects:', includeDetails ? forecastProjects.length : 'N/A (details not requested)');
    console.log('[getPipelineForecast] Weekly breakdown:', {
      lastWeek: lastWeekCount,
      thisWeek: thisWeekCount,
      nextWeek: nextWeekCount
    });

    const result = {
      lastWeek: lastWeekCount,
      thisWeek: thisWeekCount,
      nextWeek: nextWeekCount,
      totalActiveProjects,
      projectsWithForecast,
      projectsWithinWeeks,
      ...(includeDetails && { projects: forecastProjects }),
    };

    return result;
  } catch (error) {
    logError('Failed to fetch pipeline forecast', error as Error, { userId, role, officeIds, reqId });
    throw error;
  }
}

/**
 * Calculate average time per milestone for process optimization
 * Analyzes duration between milestone dates to identify bottlenecks
 */
export async function getMilestoneTimings(
  userId: string,
  role: string,
  timeRange: 'lifetime' | 'ytd' | 'month' | 'week' | 'custom' | 'last_30' | 'last_90' | 'last_12_months' = 'ytd',
  officeIds?: number[],
  customDateRange?: { startDate: string; endDate: string },
  reqId?: string,
  timezone: string = 'America/New_York'
) {
  console.log('[getMilestoneTimings] START - userId:', userId, 'role:', role, 'timeRange:', timeRange, 'officeIds:', officeIds, 'reqId:', reqId);
  const startTime = Date.now();

  try {
    // Get assigned office IDs for office-based roles if no office IDs provided
    let effectiveOfficeIds = officeIds;
    if (!effectiveOfficeIds && ['office_leader', 'area_director', 'divisional', 'regional'].includes(role)) {
      effectiveOfficeIds = await getAssignedOffices(userId);
    }

    // Get user email for role-based filtering
    const userEmail = await getUserEmail(userId);

    // Build role-based access clause
    const accessClause = buildProjectAccessClause(userEmail, role, effectiveOfficeIds);
    console.log('[getMilestoneTimings] Access clause:', accessClause);

    // Build timezone-aware time range filter
    let timeFilter = '';
    if (timeRange === 'custom' && customDateRange) {
      timeFilter = `AND {${PROJECT_FIELDS.SALES_DATE}.OAF.'${customDateRange.startDate}'} AND {${PROJECT_FIELDS.SALES_DATE}.OBF.'${customDateRange.endDate}'}`;
    } else if (timeRange !== 'lifetime') {
      const now = new Date();
      const currentYear = now.getFullYear();
      const currentMonth = now.getMonth();
      
      let startDate: string;
      let endDate: string;
      
      switch (timeRange) {
        case 'ytd':
          startDate = `${currentYear}-01-01`;
          endDate = new Intl.DateTimeFormat('en-CA', {
            timeZone: timezone,
            year: 'numeric',
            month: '2-digit',
            day: '2-digit',
          }).format(now);
          break;
        case 'month':
          const monthStart = new Date(currentYear, currentMonth, 1);
          endDate = new Intl.DateTimeFormat('en-CA', {
            timeZone: timezone,
            year: 'numeric',
            month: '2-digit',
            day: '2-digit',
          }).format(now);
          startDate = new Intl.DateTimeFormat('en-CA', {
            timeZone: timezone,
            year: 'numeric',
            month: '2-digit',
            day: '2-digit',
          }).format(monthStart);
          break;
        case 'week':
          const oneWeekAgo = new Date(now.getTime() - 7 * 24 * 60 * 60 * 1000);
          startDate = new Intl.DateTimeFormat('en-CA', {
            timeZone: timezone,
            year: 'numeric',
            month: '2-digit',
            day: '2-digit',
          }).format(oneWeekAgo);
          endDate = new Intl.DateTimeFormat('en-CA', {
            timeZone: timezone,
            year: 'numeric',
            month: '2-digit',
            day: '2-digit',
          }).format(now);
          break;
        case 'last_30':
          const last30Start = new Date(now);
          last30Start.setDate(last30Start.getDate() - 30);
          startDate = new Intl.DateTimeFormat('en-CA', {
            timeZone: timezone,
            year: 'numeric',
            month: '2-digit',
            day: '2-digit',
          }).format(last30Start);
          endDate = new Intl.DateTimeFormat('en-CA', {
            timeZone: timezone,
            year: 'numeric',
            month: '2-digit',
            day: '2-digit',
          }).format(now);
          break;
        case 'last_90':
          const last90Start = new Date(now);
          last90Start.setDate(last90Start.getDate() - 90);
          startDate = new Intl.DateTimeFormat('en-CA', {
            timeZone: timezone,
            year: 'numeric',
            month: '2-digit',
            day: '2-digit',
          }).format(last90Start);
          endDate = new Intl.DateTimeFormat('en-CA', {
            timeZone: timezone,
            year: 'numeric',
            month: '2-digit',
            day: '2-digit',
          }).format(now);
          break;
        case 'last_12_months':
          const last12Start = new Date(now);
          last12Start.setDate(last12Start.getDate() - 365);
          startDate = new Intl.DateTimeFormat('en-CA', {
            timeZone: timezone,
            year: 'numeric',
            month: '2-digit',
            day: '2-digit',
          }).format(last12Start);
          endDate = new Intl.DateTimeFormat('en-CA', {
            timeZone: timezone,
            year: 'numeric',
            month: '2-digit',
            day: '2-digit',
          }).format(now);
          break;
        default:
          startDate = '';
          endDate = '';
      }
      
      if (startDate && endDate) {
        timeFilter = `AND {${PROJECT_FIELDS.SALES_DATE}.OAF.'${startDate}'} AND {${PROJECT_FIELDS.SALES_DATE}.OBF.'${endDate}'}`;
      }
    }

    const whereClause = `${accessClause} ${timeFilter}`.trim();

    // Query projects with milestone date fields
    const response = await qbClient.queryRecords({
      from: QB_TABLE_PROJECTS, // Projects table
      where: whereClause,
      select: [
        PROJECT_FIELDS.RECORD_ID,
        PROJECT_FIELDS.SALES_DATE,
        PROJECT_FIELDS.INTAKE_FIRST_PASS_COMPLETE, // Field 1951
        PROJECT_FIELDS.SURVEY_APPROVED, // Field 165
        PROJECT_FIELDS.DESIGN_COMPLETED, // Field 315
        PROJECT_FIELDS.PERMIT_APPROVED, // Field 208
        PROJECT_FIELDS.INSTALL_COMPLETED_DATE, // Field 534
        PROJECT_FIELDS.PTO_APPROVED, // Field 538
      ],
      options: {
        top: 5000, // Handle large datasets
      },
    });

    const projects = response.data || [];
    console.log('[getMilestoneTimings] Fetched projects:', projects.length);

    // Helper function to calculate duration between dates
    const calculateMilestoneDuration = (startDate: string | null, endDate: string | null): number | null => {
      if (!startDate || !endDate) return null;
      
      const start = parseQuickbaseDate(startDate);
      const end = parseQuickbaseDate(endDate);
      
      if (!start || !end) return null;
      
      const diffTime = end.getTime() - start.getTime();
      const diffDays = Math.ceil(diffTime / (1000 * 60 * 60 * 24));
      
      // Filter out outliers (>365 days)
      return diffDays > 365 ? null : diffDays;
    };

    // Calculate durations for each milestone
    const milestoneData = {
      intake: [] as number[],
      survey: [] as number[],
      design: [] as number[],
      permit: [] as number[],
      install: [] as number[],
      pto: [] as number[],
      overall: [] as number[],
    };

    let totalProjects = projects.length;
    let completedProjects = 0;

    for (const project of projects) {
      const salesDate = project[PROJECT_FIELDS.SALES_DATE]?.value;
      const intakeComplete = project[PROJECT_FIELDS.INTAKE_FIRST_PASS_COMPLETE]?.value;
      const surveyApproved = project[PROJECT_FIELDS.SURVEY_APPROVED]?.value;
      const designCompleted = project[PROJECT_FIELDS.DESIGN_COMPLETED]?.value;
      const permitApproved = project[PROJECT_FIELDS.PERMIT_APPROVED]?.value;
      const installCompleted = project[PROJECT_FIELDS.INSTALL_COMPLETED_DATE]?.value;
      const ptoApproved = project[PROJECT_FIELDS.PTO_APPROVED]?.value;

      // Calculate milestone durations
      const intakeDuration = calculateMilestoneDuration(salesDate, intakeComplete);
      if (intakeDuration !== null) milestoneData.intake.push(intakeDuration);

      const surveyDuration = calculateMilestoneDuration(intakeComplete, surveyApproved);
      if (surveyDuration !== null) milestoneData.survey.push(surveyDuration);

      const designDuration = calculateMilestoneDuration(surveyApproved, designCompleted);
      if (designDuration !== null) milestoneData.design.push(designDuration);

      const permitDuration = calculateMilestoneDuration(designCompleted, permitApproved);
      if (permitDuration !== null) milestoneData.permit.push(permitDuration);

      const installDuration = calculateMilestoneDuration(permitApproved, installCompleted);
      if (installDuration !== null) milestoneData.install.push(installDuration);

      const ptoDuration = calculateMilestoneDuration(installCompleted, ptoApproved);
      if (ptoDuration !== null) milestoneData.pto.push(ptoDuration);

      // Calculate overall cycle time
      const overallDuration = calculateMilestoneDuration(salesDate, ptoApproved);
      if (overallDuration !== null) {
        milestoneData.overall.push(overallDuration);
        completedProjects++;
      }
    }

    // Helper function to calculate statistics
    const calculateStats = (durations: number[], milestoneName: string) => {
      if (durations.length === 0) {
        return {
          milestoneName,
          avgDays: null,
          medianDays: null,
          minDays: null,
          maxDays: null,
          projectCount: durations.length,
          completionRate: totalProjects > 0 ? (durations.length / totalProjects) * 100 : 0,
        };
      }

      const sorted = [...durations].sort((a, b) => a - b);
      const avg = durations.reduce((sum, days) => sum + days, 0) / durations.length;
      const median = sorted.length % 2 === 0 
        ? (sorted[sorted.length / 2 - 1] + sorted[sorted.length / 2]) / 2
        : sorted[Math.floor(sorted.length / 2)];

      return {
        milestoneName,
        avgDays: Math.round(avg),
        medianDays: Math.round(median),
        minDays: Math.min(...durations),
        maxDays: Math.max(...durations),
        projectCount: durations.length,
        completionRate: totalProjects > 0 ? (durations.length / totalProjects) * 100 : 0,
      };
    };

    // Calculate statistics for each milestone
    const overallCycleTime = calculateStats(milestoneData.overall, 'Overall Cycle Time');
    const milestones = [
      calculateStats(milestoneData.intake, 'Intake'),
      calculateStats(milestoneData.survey, 'Survey'),
      calculateStats(milestoneData.design, 'Design'),
      calculateStats(milestoneData.permit, 'Permit'),
      calculateStats(milestoneData.install, 'Install'),
      calculateStats(milestoneData.pto, 'PTO'),
    ];

    // Compute bottleneck - milestone with highest avgDays (skip nulls)
    const bottleneck = milestones
      .filter(m => m.avgDays !== null)
      .reduce((max, current) => 
        (current.avgDays || 0) > (max.avgDays || 0) ? current : max, 
        milestones.find(m => m.avgDays !== null) || milestones[0]
      );

    const duration = Date.now() - startTime;
    console.log('[getMilestoneTimings] COMPLETED - duration:', duration, 'ms, total projects:', totalProjects, 'completed:', completedProjects);

    return {
      overallCycleTime,
      milestones,
      totalProjects,
      completedProjects,
      bottleneck: bottleneck?.milestoneName || null,
    };
  } catch (error) {
    logError('Failed to fetch milestone timings', error as Error, { userId, role, timeRange, officeIds, reqId });
    throw error;
  }
}

// ============================================================================
// TASK MANAGEMENT FUNCTIONS
// ============================================================================

export async function getProjectTasks(projectId: number): Promise<Task[]> {
  try {
    console.log('[getProjectTasks] Fetching tasks for project:', projectId);

    // Step 1: Get task groups for this project
    const taskGroupsResponse = await qbClient.queryRecords({
      from: QB_TABLE_TASK_GROUPS,
      select: [TASK_GROUP_FIELDS.RECORD_ID],
      where: `{${TASK_GROUP_FIELDS.RELATED_PROJECT}.EX.${projectId}}`
    });

    // Extract task group IDs
    const taskGroupIds = taskGroupsResponse.data
      .map((record: any) => record[TASK_GROUP_FIELDS.RECORD_ID]?.value)
      .filter((id: any) => id);

    if (taskGroupIds.length === 0) {
      console.log('[getProjectTasks] No task groups found for project', projectId);
      return [];
    }

    console.log('[getProjectTasks] Found', taskGroupIds.length, 'task groups:', taskGroupIds);

    // Step 2: Get tasks for these task groups
    // Build OR clause: {6.EX.123}OR{6.EX.456}OR{6.EX.789}
    const whereClause = taskGroupIds.map((id: number) => `{${TASK_FIELDS.TASK_GROUP}.EX.${id}}`).join('OR');

    const response = await qbClient.queryRecords({
      from: QB_TABLE_TASKS,
      select: [
        TASK_FIELDS.RECORD_ID,
        TASK_FIELDS.DATE_CREATED,
        TASK_FIELDS.DATE_MODIFIED,
        TASK_FIELDS.TASK_GROUP,
        TASK_FIELDS.STATUS,
        TASK_FIELDS.NAME,
        TASK_FIELDS.DESCRIPTION,
        TASK_FIELDS.MAX_SUBMISSION_STATUS,
        TASK_FIELDS.TASK_TEMPLATE,
        TASK_FIELDS.TASK_CATEGORY,
        TASK_FIELDS.TASK_MISSING_ITEM,
        TASK_FIELDS.REVIEWED_BY_OPS,
        TASK_FIELDS.REVIEWED_BY_OPS_USER,
        TASK_FIELDS.OPS_REVIEW_NOTE
      ],
      where: whereClause
    });

    const tasks: Task[] = response.data.map((record: any) => ({
      recordId: record[TASK_FIELDS.RECORD_ID]?.value || 0,
      dateCreated: record[TASK_FIELDS.DATE_CREATED]?.value || null,
      dateModified: record[TASK_FIELDS.DATE_MODIFIED]?.value || null,
      taskGroup: record[TASK_FIELDS.TASK_GROUP]?.value || 0,
      status: record[TASK_FIELDS.STATUS]?.value || 'Not Started',
      name: record[TASK_FIELDS.NAME]?.value || '',
      description: record[TASK_FIELDS.DESCRIPTION]?.value || null,
      maxSubmissionStatus: record[TASK_FIELDS.MAX_SUBMISSION_STATUS]?.value || null,
      taskTemplate: record[TASK_FIELDS.TASK_TEMPLATE]?.value || null,
      category: record[TASK_FIELDS.TASK_CATEGORY]?.value || null,
      missingItem: record[TASK_FIELDS.TASK_MISSING_ITEM]?.value || null,
      reviewedByOps: record[TASK_FIELDS.REVIEWED_BY_OPS]?.value || null,
      reviewedByOpsUser: record[TASK_FIELDS.REVIEWED_BY_OPS_USER]?.value || null,
      opsReviewNote: record[TASK_FIELDS.OPS_REVIEW_NOTE]?.value || null,
      submissions: [] // Will be populated separately if needed
    }));

    console.log('[getProjectTasks] Found', tasks.length, 'tasks for project', projectId);
    return tasks;
  } catch (error) {
    logError('Failed to fetch project tasks', error as Error, { projectId });
    throw error;
  }
}

export async function getTaskById(taskId: number): Promise<Task | null> {
  try {
    console.log('[getTaskById] Fetching task:', taskId);

    const response = await qbClient.queryRecords({
      from: QB_TABLE_TASKS,
      select: [
        TASK_FIELDS.RECORD_ID,
        TASK_FIELDS.DATE_CREATED,
        TASK_FIELDS.DATE_MODIFIED,
        TASK_FIELDS.TASK_GROUP,
        TASK_FIELDS.STATUS,
        TASK_FIELDS.NAME,
        TASK_FIELDS.DESCRIPTION,
        TASK_FIELDS.MAX_SUBMISSION_STATUS,
        TASK_FIELDS.TASK_TEMPLATE,
        TASK_FIELDS.TASK_CATEGORY,
        TASK_FIELDS.TASK_MISSING_ITEM,
        TASK_FIELDS.REVIEWED_BY_OPS,
        TASK_FIELDS.REVIEWED_BY_OPS_USER,
        TASK_FIELDS.OPS_REVIEW_NOTE
      ],
      where: `{3.EX.${taskId}}` // Filter by record ID (field 3) = task ID
    });

    if (response.data.length === 0) {
      console.log('[getTaskById] Task not found:', taskId);
      return null;
    }

    const record = response.data[0];
    const task: Task = {
      recordId: record[TASK_FIELDS.RECORD_ID]?.value || 0,
      dateCreated: record[TASK_FIELDS.DATE_CREATED]?.value || null,
      dateModified: record[TASK_FIELDS.DATE_MODIFIED]?.value || null,
      taskGroup: record[TASK_FIELDS.TASK_GROUP]?.value || 0,
      status: record[TASK_FIELDS.STATUS]?.value || 'Not Started',
      name: record[TASK_FIELDS.NAME]?.value || '',
      description: record[TASK_FIELDS.DESCRIPTION]?.value || null,
      maxSubmissionStatus: record[TASK_FIELDS.MAX_SUBMISSION_STATUS]?.value || null,
      taskTemplate: record[TASK_FIELDS.TASK_TEMPLATE]?.value || null,
      category: record[TASK_FIELDS.TASK_CATEGORY]?.value || null,
      missingItem: record[TASK_FIELDS.TASK_MISSING_ITEM]?.value || null,
      reviewedByOps: record[TASK_FIELDS.REVIEWED_BY_OPS]?.value || null,
      reviewedByOpsUser: record[TASK_FIELDS.REVIEWED_BY_OPS_USER]?.value || null,
      opsReviewNote: record[TASK_FIELDS.OPS_REVIEW_NOTE]?.value || null,
      submissions: [] // Will be populated separately if needed
    };

    console.log('[getTaskById] Found task:', task.name);
    return task;
  } catch (error) {
    logError('Failed to fetch task by ID', error as Error, { taskId });
    throw error;
  }
}

// In-memory cache for task templates (38 templates, rarely change)
let templateCache: Map<number, TaskTemplate> | null = null;
let templateCacheTimestamp: number = 0;
const TEMPLATE_CACHE_TTL = 1000 * 60 * 60; // 1 hour

/**
 * Get a single task template by ID
 */
export async function getTaskTemplate(templateId: number): Promise<TaskTemplate | null> {
  try {
    console.log('[getTaskTemplate] Fetching template:', templateId);

    // Check cache first
    if (templateCache && templateCache.has(templateId)) {
      const cachedTemplate = templateCache.get(templateId);
      if (cachedTemplate && (Date.now() - templateCacheTimestamp) < TEMPLATE_CACHE_TTL) {
        console.log('[getTaskTemplate] Returning cached template');
        return cachedTemplate;
      }
    }

    const response = await qbClient.queryRecords({
      from: QB_TABLE_TASK_TEMPLATES,
      select: [
        TASK_TEMPLATE_FIELDS.RECORD_ID,
        TASK_TEMPLATE_FIELDS.TASK_NAME,
        TASK_TEMPLATE_FIELDS.INTAKE_TASK_CATEGORY,
        TASK_TEMPLATE_FIELDS.INTAKE_TASK_MISSING_ITEM,
        TASK_TEMPLATE_FIELDS.TASK_DESCRIPTION
      ],
      where: `{3.EX.${templateId}}`
    });

    if (response.data.length === 0) {
      console.log('[getTaskTemplate] Template not found:', templateId);
      return null;
    }

    const record = response.data[0];
    const template: TaskTemplate = {
      recordId: record[TASK_TEMPLATE_FIELDS.RECORD_ID]?.value || 0,
      taskName: record[TASK_TEMPLATE_FIELDS.TASK_NAME]?.value || '',
      category: record[TASK_TEMPLATE_FIELDS.INTAKE_TASK_CATEGORY]?.value || null,
      missingItem: record[TASK_TEMPLATE_FIELDS.INTAKE_TASK_MISSING_ITEM]?.value || null,
      description: record[TASK_TEMPLATE_FIELDS.TASK_DESCRIPTION]?.value || null
    };

    // Update cache
    if (!templateCache) {
      templateCache = new Map();
      templateCacheTimestamp = Date.now();
    }
    templateCache.set(templateId, template);

    console.log('[getTaskTemplate] Found template:', template.taskName);
    return template;
  } catch (error) {
    logError('Failed to fetch task template', error as Error, { templateId });
    return null;
  }
}

/**
 * Get all task templates (for caching and lookup)
 * Call this on app initialization to populate the cache
 */
export async function getAllTaskTemplates(): Promise<TaskTemplate[]> {
  try {
    console.log('[getAllTaskTemplates] Fetching all templates');

    const response = await qbClient.queryRecords({
      from: QB_TABLE_TASK_TEMPLATES,
      select: [
        TASK_TEMPLATE_FIELDS.RECORD_ID,
        TASK_TEMPLATE_FIELDS.TASK_NAME,
        TASK_TEMPLATE_FIELDS.INTAKE_TASK_CATEGORY,
        TASK_TEMPLATE_FIELDS.INTAKE_TASK_MISSING_ITEM,
        TASK_TEMPLATE_FIELDS.TASK_DESCRIPTION
      ]
    });

    const templates: TaskTemplate[] = response.data.map((record: any) => ({
      recordId: record[TASK_TEMPLATE_FIELDS.RECORD_ID]?.value || 0,
      taskName: record[TASK_TEMPLATE_FIELDS.TASK_NAME]?.value || '',
      category: record[TASK_TEMPLATE_FIELDS.INTAKE_TASK_CATEGORY]?.value || null,
      missingItem: record[TASK_TEMPLATE_FIELDS.INTAKE_TASK_MISSING_ITEM]?.value || null,
      description: record[TASK_TEMPLATE_FIELDS.TASK_DESCRIPTION]?.value || null
    }));

    // Populate cache
    templateCache = new Map(templates.map(t => [t.recordId, t]));
    templateCacheTimestamp = Date.now();

    console.log('[getAllTaskTemplates] Cached', templates.length, 'templates');
    return templates;
  } catch (error) {
    logError('Failed to fetch all task templates', error as Error);
    return [];
  }
}

export async function getTaskSubmissions(taskId: number): Promise<TaskSubmission[]> {
  try {
    console.log('[getTaskSubmissions] Fetching submissions for task:', taskId);

    const response = await qbClient.queryRecords({
      from: QB_TABLE_TASK_SUBMISSIONS,
      select: [
        TASK_SUBMISSION_FIELDS.RECORD_ID,
        TASK_SUBMISSION_FIELDS.DATE_CREATED,
        TASK_SUBMISSION_FIELDS.RELATED_TASK,
        TASK_SUBMISSION_FIELDS.SUBMISSION_STATUS,
        TASK_SUBMISSION_FIELDS.OPS_DISPOSITION,
        TASK_SUBMISSION_FIELDS.FILE_ATTACHMENT_1,
        TASK_SUBMISSION_FIELDS.IS_MAX_SUBMISSION,
        TASK_SUBMISSION_FIELDS.SUBMISSION_NOTE,
        TASK_SUBMISSION_FIELDS.FILE_ATTACHMENT_2,
        TASK_SUBMISSION_FIELDS.FILE_ATTACHMENT_3,
        TASK_SUBMISSION_FIELDS.OPS_DISPOSITION_NOTE,
        TASK_SUBMISSION_FIELDS.OPS_REVIEW_COMPLETED_BY,
        TASK_SUBMISSION_FIELDS.OPS_REVIEW_COMPLETED_AT
      ],
      where: `{6.EX.${taskId}}` // Filter by related task (field 6) = task ID
    });

    const submissions: TaskSubmission[] = response.data.map((record: any) => ({
      recordId: record[TASK_SUBMISSION_FIELDS.RECORD_ID]?.value || 0,
      dateCreated: record[TASK_SUBMISSION_FIELDS.DATE_CREATED]?.value || '',
      relatedTask: record[TASK_SUBMISSION_FIELDS.RELATED_TASK]?.value || 0,
      submissionStatus: record[TASK_SUBMISSION_FIELDS.SUBMISSION_STATUS]?.value || 'Pending Approval',
      opsDisposition: record[TASK_SUBMISSION_FIELDS.OPS_DISPOSITION]?.value || null,
      fileAttachment1: record[TASK_SUBMISSION_FIELDS.FILE_ATTACHMENT_1]?.value || null,
      isMaxSubmission: record[TASK_SUBMISSION_FIELDS.IS_MAX_SUBMISSION]?.value || false,
      submissionNote: record[TASK_SUBMISSION_FIELDS.SUBMISSION_NOTE]?.value || null,
      fileAttachment2: record[TASK_SUBMISSION_FIELDS.FILE_ATTACHMENT_2]?.value || null,
      fileAttachment3: record[TASK_SUBMISSION_FIELDS.FILE_ATTACHMENT_3]?.value || null,
      opsDispositionNote: record[TASK_SUBMISSION_FIELDS.OPS_DISPOSITION_NOTE]?.value || null,
      opsReviewCompletedBy: record[TASK_SUBMISSION_FIELDS.OPS_REVIEW_COMPLETED_BY]?.value || null,
      opsReviewCompletedAt: record[TASK_SUBMISSION_FIELDS.OPS_REVIEW_COMPLETED_AT]?.value || null,
      // Backwards compatibility aliases
      fileAttachments: record[TASK_SUBMISSION_FIELDS.FILE_ATTACHMENT_1]?.value || null,
      opsFeedback: record[TASK_SUBMISSION_FIELDS.OPS_DISPOSITION_NOTE]?.value || null
    }));

    console.log('[getTaskSubmissions] Found', submissions.length, 'submissions for task', taskId);
    return submissions;
  } catch (error) {
    logError('Failed to fetch task submissions', error as Error, { taskId });
    throw error;
  }
}

export async function createTaskSubmission(taskId: number, userId: string, submissionNote?: string): Promise<number> {
  try {
    console.log('[createTaskSubmission] Creating submission for task:', taskId);

    const data: any = {
      [TASK_SUBMISSION_FIELDS.RELATED_TASK]: { value: taskId },
      [TASK_SUBMISSION_FIELDS.SUBMISSION_STATUS]: { value: 'Pending Approval' },
      [TASK_SUBMISSION_FIELDS.OPS_DISPOSITION]: { value: '' } // Leave empty for ops to fill
    };

    // Add submission note if provided
    if (submissionNote) {
      data[TASK_SUBMISSION_FIELDS.SUBMISSION_NOTE] = { value: submissionNote };
    }

    const response = await qbClient.updateRecord({
      to: QB_TABLE_TASK_SUBMISSIONS,
      data: [data]
    });

    const submissionId = response.metadata?.createdRecordIds?.[0] || response.data?.[0]?.[3]?.value;
    console.log('[createTaskSubmission] Created submission with ID:', submissionId);
    return submissionId;
  } catch (error) {
    logError('Failed to create task submission', error as Error, { taskId, userId });
    throw error;
  }
}

export async function updateTaskStatus(taskId: number, status: TaskStatus): Promise<void> {
  try {
    console.log('[updateTaskStatus] Updating task status:', { taskId, status });

    await qbClient.updateRecord({
      to: QB_TABLE_TASKS,
      data: [
        {
          [TASK_FIELDS.RECORD_ID]: { value: taskId },
          [TASK_FIELDS.STATUS]: { value: status }
        }
      ]
    });

    console.log('[updateTaskStatus] Task status updated successfully');
  } catch (error) {
    logError('Failed to update task status', error as Error, { taskId, status });
    throw error;
  }
}

export async function uploadFileToSubmission(
  submissionId: number,
  fileName: string,
  fileBuffer: Buffer,
  fieldId: number = TASK_SUBMISSION_FIELDS.FILE_ATTACHMENT_1
): Promise<void> {
  try {
    console.log('[uploadFileToSubmission] Uploading file:', { submissionId, fileName, fieldId, size: fileBuffer.length });

    await qbClient.uploadFileToRecord({
      tableId: QB_TABLE_TASK_SUBMISSIONS,
      recordId: submissionId,
      fieldId: fieldId,
      fileName: fileName,
      fileData: fileBuffer
    });

    console.log('[uploadFileToSubmission] File uploaded successfully to field', fieldId, ', size:', fileBuffer.length);
  } catch (error) {
    logError('Failed to upload file to submission', error as Error, { submissionId, fileName, fieldId, size: fileBuffer.length });
    throw error;
  }
}

/**
 * Get task counts for multiple projects from Task Groups table
 * @param projectIds Array of project IDs to get task counts for
 * @returns Map of project ID to task counts
 */
export async function getTaskCountsForProjects(projectIds: number[]): Promise<Map<number, { totalTasks: number; unapprovedTasks: number }>> {
  if (projectIds.length === 0) {
    return new Map();
  }

  try {
    console.log('[getTaskCountsForProjects] Fetching task counts for', projectIds.length, 'projects');

    // Build WHERE clause for multiple project IDs
    // For large arrays, we might need to batch queries to avoid URL length limits
    const batchSize = 50;
    const batches = [];
    for (let i = 0; i < projectIds.length; i += batchSize) {
      batches.push(projectIds.slice(i, i + batchSize));
    }

    const taskCountsMap = new Map<number, { totalTasks: number; unapprovedTasks: number }>();

    // Build array of promises for parallel execution
    const promises = batches.map(batch => {
      // Build OR clause for this batch
      const whereClause = batch.map(id => `{${TASK_GROUP_FIELDS.RELATED_PROJECT}.EX.${id}}`).join(' OR ');

      return qbClient.queryRecords({
        from: QB_TABLE_TASK_GROUPS,
        select: [
          TASK_GROUP_FIELDS.RECORD_ID,
          TASK_GROUP_FIELDS.RELATED_PROJECT,
          TASK_GROUP_FIELDS.TOTAL_TASKS,
          TASK_GROUP_FIELDS.UNAPPROVED_TASKS
        ],
        where: whereClause
      });
    });

    // Execute all queries in parallel
    const results = await Promise.all(promises);

    // Process all results and build map
    results.forEach(response => {
      response.data?.forEach((record: any) => {
        const projectId = parseInt(record[TASK_GROUP_FIELDS.RELATED_PROJECT]?.value || '0');
        const totalTasks = parseInt(record[TASK_GROUP_FIELDS.TOTAL_TASKS]?.value || '0');
        const unapprovedTasks = parseInt(record[TASK_GROUP_FIELDS.UNAPPROVED_TASKS]?.value || '0');

        if (projectId > 0) {
          // If project has multiple task groups, sum the counts
          const existing = taskCountsMap.get(projectId);
          if (existing) {
            existing.totalTasks += totalTasks;
            existing.unapprovedTasks += unapprovedTasks;
          } else {
            taskCountsMap.set(projectId, { totalTasks, unapprovedTasks });
          }
        }
      });
    });

    console.log('[getTaskCountsForProjects] Found task groups for', taskCountsMap.size, 'projects');
    return taskCountsMap;
  } catch (error) {
    logError('Failed to fetch task counts for projects', error as Error, { projectCount: projectIds.length });
    throw error;
  }
}

/**
 * Create a notification for a task event
 * Helper function to standardize task notification creation
 */
export async function createTaskNotification(params: {
  userId: string;
  projectId: number;
  taskId: number;
  taskName: string;
  taskCategory?: string;
  type: 'task_submitted' | 'task_approved' | 'task_revision_needed' | 'all_tasks_complete';
  submissionId?: number;
  opsDisposition?: 'Approved' | 'Needs Revision';
  opsFeedback?: string;
  totalTasks?: number;
  approvedTasks?: number;
}): Promise<void> {
  try {
    const { createNotification } = await import('@/lib/db/notifications');

    // Build notification title and message based on type
    let title: string;
    let message: string;
    let priority: 'critical' | 'normal' | 'info';
    let icon: string;
    let color: string;

    switch (params.type) {
      case 'task_submitted':
        title = `Task Submitted: ${params.taskName}`;
        message = 'Your task submission is pending review by operations.';
        priority = 'info';
        icon = 'file-check';
        color = 'blue';
        break;

      case 'task_approved':
        title = `Task Approved: ${params.taskName}`;
        message = 'Your task submission has been approved!';
        priority = 'normal';
        icon = 'check-circle';
        color = 'green';
        break;

      case 'task_revision_needed':
        title = `Revision Needed: ${params.taskName}`;
        message = params.opsFeedback || 'Operations has requested revisions on your submission.';
        priority = 'critical';
        icon = 'alert-triangle';
        color = 'orange';
        break;

      case 'all_tasks_complete':
        title = 'All Tasks Complete!';
        const total = params.totalTasks ?? params.approvedTasks ?? null;
        message = total !== null
          ? `All ${total} tasks have been approved. Your project is ready for reactivation.`
          : 'All tasks have been approved. Your project is ready for reactivation.';
        priority = 'normal';
        icon = 'clipboard-check';
        color = 'green';
        break;
    }

    // Build metadata
    const metadata: any = {
      task_id: params.taskId,
      task_name: params.taskName,
    };

    if (params.taskCategory) metadata.task_category = params.taskCategory;
    if (params.submissionId) metadata.submission_id = params.submissionId;
    if (params.opsDisposition) metadata.ops_disposition = params.opsDisposition;
    if (params.opsFeedback) metadata.ops_feedback = params.opsFeedback;
    if (params.totalTasks !== undefined) metadata.total_tasks = params.totalTasks;
    if (params.approvedTasks !== undefined) metadata.approved_tasks = params.approvedTasks;

    // Create notification
    await createNotification({
      user_id: params.userId,
      project_id: params.projectId,
      type: params.type,
      priority,
      source: 'system',
      title,
      message,
      metadata,
      icon,
      color,
      action_url: `/projects/${params.projectId}#tasks`,
    });

    console.log('[createTaskNotification] Created notification:', {
      type: params.type,
      userId: params.userId,
      projectId: params.projectId,
      taskId: params.taskId,
    });
  } catch (error) {
    logError('Failed to create task notification', error as Error, {
      userId: params.userId,
      projectId: params.projectId,
      taskId: params.taskId,
      type: params.type,
    });
    // Don't throw - notification failure shouldn't break task submission
  }
}

/**
 * Check if all tasks for a project are complete
 * @param projectId Project record ID
 * @returns Completion status with task counts
 */
export async function checkAllTasksComplete(projectId: number): Promise<{
  allComplete: boolean;
  totalTasks: number;
  approvedTasks: number;
  unapprovedTasks: number;
}> {
  try {
    console.log('[checkAllTasksComplete] Checking completion for project:', projectId);

    // Get task groups for this project
    const taskGroupsResponse = await qbClient.queryRecords({
      from: QB_TABLE_TASK_GROUPS,
      select: [
        TASK_GROUP_FIELDS.RECORD_ID,
        TASK_GROUP_FIELDS.TOTAL_TASKS,
        TASK_GROUP_FIELDS.UNAPPROVED_TASKS
      ],
      where: `{${TASK_GROUP_FIELDS.RELATED_PROJECT}.EX.${projectId}}`
    });

    // Sum up totals across all task groups
    let totalTasks = 0;
    let unapprovedTasks = 0;

    taskGroupsResponse.data?.forEach((group: any) => {
      totalTasks += parseInt(group[TASK_GROUP_FIELDS.TOTAL_TASKS]?.value || '0');
      unapprovedTasks += parseInt(group[TASK_GROUP_FIELDS.UNAPPROVED_TASKS]?.value || '0');
    });

    const approvedTasks = totalTasks - unapprovedTasks;
    const allComplete = totalTasks > 0 && unapprovedTasks === 0;

    console.log('[checkAllTasksComplete] Result:', {
      allComplete,
      totalTasks,
      approvedTasks,
      unapprovedTasks
    });

    return {
      allComplete,
      totalTasks,
      approvedTasks,
      unapprovedTasks
    };
  } catch (error) {
    logError('Failed to check task completion', error as Error, { projectId });
    throw error;
  }
}

/**
 * Mark project as having all tasks complete
 * Updates project status to indicate ready for re-intake
 * @param projectId Project record ID
 */
export async function markProjectTasksComplete(projectId: number): Promise<void> {
  try {
    console.log('[markProjectTasksComplete] Marking project as tasks complete:', projectId);

    // Update project record
    // Note: You may need to adjust field IDs based on your project table structure
    // This sets a timestamp to indicate when tasks were completed
    await qbClient.updateRecord({
      to: QB_TABLE_PROJECTS, // Projects table
      data: [
        {
          [PROJECT_FIELDS.RECORD_ID]: { value: projectId },
          // Add timestamp field for task completion (adjust field ID as needed)
          // Example: [PROJECT_FIELDS.TASKS_COMPLETED_DATE]: { value: new Date().toISOString() }
          // You may also want to update a status field
          // Example: [PROJECT_FIELDS.TASK_STATUS]: { value: 'Tasks Complete' }
        }
      ]
    });

    console.log('[markProjectTasksComplete] Project marked as tasks complete');
  } catch (error) {
    logError('Failed to mark project tasks complete', error as Error, { projectId });
    throw error;
  }
}

/**
 * Calculate resolution time for a rejected project using task data
 * Resolution time = time from earliest task creation to latest task approval
 *
 * @param projectId Project record ID
 * @returns Resolution time in days, or null if cannot be calculated
 */
export async function calculateTaskBasedResolutionTime(projectId: number): Promise<number | null> {
  try {
    console.log('[calculateTaskBasedResolutionTime] Calculating resolution time for project:', projectId);

    // Step 1: Query all tasks for this project
    const tasksResponse = await qbClient.queryRecords({
      from: QB_TABLE_TASKS,
      where: `{6}.EX.${projectId}`, // Field 6 = TASK_GROUP (which links to project)
      select: [
        TASK_FIELDS.RECORD_ID,
        TASK_FIELDS.DATE_CREATED,
      ]
    });

    if (!tasksResponse.data || tasksResponse.data.length === 0) {
      console.log('[calculateTaskBasedResolutionTime] No tasks found for project');
      return null;
    }

    // Step 2: Find earliest task creation date (when project was rejected)
    const taskDates = tasksResponse.data
      .map(task => task[TASK_FIELDS.DATE_CREATED]?.value)
      .filter(Boolean)
      .map(dateStr => new Date(dateStr));

    if (taskDates.length === 0) {
      console.log('[calculateTaskBasedResolutionTime] No task creation dates found');
      return null;
    }

    const earliestTaskDate = new Date(Math.min(...taskDates.map(d => d.getTime())));

    // Step 3: Query all submissions for these tasks
    const taskIds = tasksResponse.data.map(task => task[TASK_FIELDS.RECORD_ID]?.value).filter(Boolean);

    if (taskIds.length === 0) {
      console.log('[calculateTaskBasedResolutionTime] No valid task IDs found');
      return null;
    }

    const submissionsWhere = taskIds.map(id => `{${TASK_SUBMISSION_FIELDS.RELATED_TASK}.EX.${id}}`).join(' OR ');

    const submissionsResponse = await qbClient.queryRecords({
      from: QB_TABLE_TASK_SUBMISSIONS,
      where: submissionsWhere,
      select: [
        TASK_SUBMISSION_FIELDS.OPS_REVIEW_COMPLETED_AT,
      ]
    });

    // Step 4: Find latest ops review completion date (when last item was approved)
    if (!submissionsResponse.data || submissionsResponse.data.length === 0) {
      console.log('[calculateTaskBasedResolutionTime] No submissions found for tasks');
      return null;
    }

    const reviewDates = submissionsResponse.data
      .map(sub => sub[TASK_SUBMISSION_FIELDS.OPS_REVIEW_COMPLETED_AT]?.value)
      .filter(Boolean)
      .map(dateStr => new Date(dateStr));

    if (reviewDates.length === 0) {
      console.log('[calculateTaskBasedResolutionTime] No review completion dates found');
      return null;
    }

    const latestReviewDate = new Date(Math.max(...reviewDates.map(d => d.getTime())));

    // Step 5: Calculate difference in days
    const resolutionDays = (latestReviewDate.getTime() - earliestTaskDate.getTime()) / (1000 * 60 * 60 * 24);
    const roundedDays = Math.round(resolutionDays * 10) / 10; // Round to 1 decimal

    console.log('[calculateTaskBasedResolutionTime] Resolution time:', roundedDays, 'days');
    return roundedDays > 0 ? roundedDays : null;

  } catch (error) {
    logError('Failed to calculate task-based resolution time', error as Error, { projectId });
    return null;
  }
}
