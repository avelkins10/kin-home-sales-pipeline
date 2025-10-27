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
import { logError, logInfo, logWarn, logQuickbaseRequest, logQuickbaseResponse, logQuickbaseError } from '@/lib/logging/logger';
import { markMessagesAsRead, getMessagesReadStatus } from '@/lib/db/messageReadReceipts';
import { getUserRoleByEmail } from '@/lib/users/enrich-user';
import {
  PROJECT_FIELDS,
  TASK_FIELDS,
  TASK_GROUP_FIELDS,
  INSTALL_COMMUNICATION_FIELDS,
  TASK_SUBMISSION_FIELDS,
  TASK_TEMPLATE_FIELDS,
  NOTIFICATION_AUDIT_FIELDS,
  OUTREACH_RECORD_FIELDS,
  SALES_AID_FIELDS,
  QB_TABLE_PROJECTS,
  QB_TABLE_TASKS,
  QB_TABLE_TASK_GROUPS,
  QB_TABLE_TASK_SUBMISSIONS,
  QB_TABLE_TASK_TEMPLATES,
  QB_TABLE_NOTIFICATION_AUDIT,
  QB_TABLE_OUTREACH_RECORDS,
  QB_TABLE_INSTALL_COMMUNICATIONS,
  QB_TABLE_SALES_AID_REQUESTS
} from '@/lib/constants/fieldIds';
import { ADDER_FIELDS } from '@/lib/constants/adderFieldIds';
import { sql } from '@/lib/db/client';
import { buildProjectAccessClause } from '@/lib/auth/projectAuthorization';
import { enrichUsersFromProjects } from '@/lib/users/enrich-users-from-projects';
import type { TeamActivityItem, TeamActivityType } from '@/lib/types/dashboard';
import { isManagerRole } from '@/lib/utils/role-helpers';
import { calculateSalesAidUrgency } from '@/lib/utils/escalation-helpers';
import type { MetricsScope, TeamMemberCommission, TeamMemberBuckets } from '@/lib/types/dashboard';
import { parseQuickbaseDate } from '@/lib/utils/date-helpers';
import type { OfficeMetrics, RepPerformance, PipelineForecast, MilestoneTimings } from '@/lib/types/analytics';
import type { Task, TaskSubmission, TaskStatus, TaskTemplate } from '@/lib/types/task';
import type {
  PCDashboardMetrics,
  PCPriorityQueueItem,
  PCProjectPipelineStage,
  PCActivityFeedItem,
  PCOutreachRecord,
  PCOutreachTabData,
  PCOutreachBulkAction,
  PCOutreachBulkActionResult,
  PCOutreachStatus,
  PCBulkMessagingRecipient,
  PCConversationItem,
  PCProjectDocument,
  PCTeamMember,
  PCTask,
  PCTaskSubmission,
  PCMessage,
  PCTaskAssignmentPayload,
  PCTaskType,
  PCSalesAidRequest
} from '@/lib/types/operations';

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

/**
 * Sanitize QuickBase query values to prevent injection
 * Escapes single quotes and special characters
 */
function sanitizeQuickbaseValue(value: string): string {
  if (!value) return '';
  return value.replace(/'/g, "\\'");
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

// Quickbase table IDs (QB_TABLE_PROJECTS is imported from fieldIds.ts)
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

    // Enrich users from project data (async, non-blocking)
    enrichUsersFromProjects(result.data, 'getProjectsForUserList');

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

// Helper function to safely extract numeric values from QuickBase wrapped objects
function extractNumericValue(field: any): number {
  if (typeof field === 'object' && field?.value !== undefined) {
    return Number(field.value) || 0;
  }
  return Number(field) || 0;
}

// Helper function to safely parse date strings to timestamps
export function toTimestamp(dateString: string): number {
  const date = new Date(dateString);
  return isNaN(date.getTime()) ? 0 : date.getTime();
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

  // Enrich users from project data (async, non-blocking)
  enrichUsersFromProjects(result.data, 'fetchProjects');

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
        PROJECT_FIELDS.CLOSER_ID,                                // Field 516 - RepCard user ID
        // Intake quality tracking fields (for closer scorecards)
        PROJECT_FIELDS.INTAKE_COMPLETED_DATE,                    // Field 461
        PROJECT_FIELDS.INTAKE_FIRST_PASS_FINANCE_APPROVED,       // Field 1831 - "Approve" or "Reject"
        PROJECT_FIELDS.INTAKE_FIRST_PASS_COMPLETE,               // Field 1951 - First review timestamp
        PROJECT_FIELDS.INTAKE_MISSING_ITEMS_COMBINED,            // Field 1871 - Rejection reasons
        PROJECT_FIELDS.PRIOR_STATUS_WAS_REJECTED_BINARY,         // Field 1930 - Ever rejected flag (hybrid detection)
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

      // Calculate intake quality metrics using hybrid detection (more accurate)
      // Helper function to check if project was ever rejected
      // Uses BOTH status strings (real-time) AND binary field (historical) for accuracy
      const wasEverRejected = (p: Record<string, any>): boolean => {
        // Check current status (real-time - catches newly rejected projects)
        const intakeStatus = (p[PROJECT_FIELDS.INTAKE_STATUS]?.value || '').toString().toLowerCase();
        const projectStatus = (p[PROJECT_FIELDS.PROJECT_STATUS]?.value || '').toString().toLowerCase();
        const currentlyRejected = intakeStatus.includes('rejected') || projectStatus.includes('rejected');

        // Check binary field (historical - catches projects that were rejected then fixed)
        const priorStatusRejected = p[PROJECT_FIELDS.PRIOR_STATUS_WAS_REJECTED_BINARY]?.value;
        const historicallyRejected = priorStatusRejected === 1 || priorStatusRejected === '1' || priorStatusRejected === true;

        // Return true if either indicates rejection
        return currentlyRejected || historicallyRejected;
      };

      // Helper function to check if project is currently rejected
      const isCurrentlyRejected = (p: Record<string, any>): boolean => {
        const intakeStatus = (p[PROJECT_FIELDS.INTAKE_STATUS]?.value || '').toString().toLowerCase();
        const projectStatus = (p[PROJECT_FIELDS.PROJECT_STATUS]?.value || '').toString().toLowerCase();
        return intakeStatus.includes('rejected') || projectStatus.includes('rejected');
      };

      // Never Rejected (first pass approved) - has completion date AND never rejected
      const neverRejected = officeProjects.filter(p =>
        p[PROJECT_FIELDS.INTAKE_COMPLETED_DATE]?.value &&
        !wasEverRejected(p)
      ).length;

      // First-time pass rate (KEY METRIC - clean deals approved on first attempt)
      const firstTimePassRate = totalProjects > 0
        ? (neverRejected / totalProjects) * 100
        : 0;

      // Total Rejections (ever rejected - using hybrid detection)
      const rejectedProjects = officeProjects.filter(wasEverRejected);
      const rejections = rejectedProjects.length;
      const rejectionRate = totalProjects > 0
        ? (rejections / totalProjects) * 100
        : 0;

      // Resubmit success rate (of rejected projects, how many eventually got approved)
      const eventuallyApproved = rejectedProjects.filter(p =>
        p[PROJECT_FIELDS.INTAKE_COMPLETED_DATE]?.value &&
        !isCurrentlyRejected(p)
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

      // Top rejection reasons (only from rejected projects)
      const reasonCounts: Record<string, number> = {};
      rejectedProjects.forEach(p => {
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
        isCurrentlyRejected(p)
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
        PROJECT_FIELDS.CLOSER_ID,                                // Field 516 - RepCard user ID
        PROJECT_FIELDS.CLOSER_NAME,
        PROJECT_FIELDS.CLOSER_EMAIL,
        PROJECT_FIELDS.SETTER_NAME,
        PROJECT_FIELDS.SETTER_EMAIL,
        // Intake quality tracking fields (for closer scorecards)
        PROJECT_FIELDS.INTAKE_COMPLETED_DATE,                    // Field 461
        PROJECT_FIELDS.INTAKE_FIRST_PASS_FINANCE_APPROVED,       // Field 1831 - "Approve" or "Reject"
        PROJECT_FIELDS.INTAKE_FIRST_PASS_COMPLETE,               // Field 1951 - First review timestamp
        PROJECT_FIELDS.INTAKE_MISSING_ITEMS_COMBINED,            // Field 1871 - Rejection reasons
        PROJECT_FIELDS.PRIOR_STATUS_WAS_REJECTED_BINARY,         // Field 1930 - Ever rejected flag (hybrid detection)
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

      // Calculate intake quality metrics using hybrid detection (more accurate)
      // Helper function to check if project was ever rejected
      // Uses BOTH status strings (real-time) AND binary field (historical) for accuracy
      const wasEverRejected = (p: Record<string, any>): boolean => {
        // Check current status (real-time - catches newly rejected projects)
        const intakeStatus = (p[PROJECT_FIELDS.INTAKE_STATUS]?.value || '').toString().toLowerCase();
        const projectStatus = (p[PROJECT_FIELDS.PROJECT_STATUS]?.value || '').toString().toLowerCase();
        const currentlyRejected = intakeStatus.includes('rejected') || projectStatus.includes('rejected');

        // Check binary field (historical - catches projects that were rejected then fixed)
        const priorStatusRejected = p[PROJECT_FIELDS.PRIOR_STATUS_WAS_REJECTED_BINARY]?.value;
        const historicallyRejected = priorStatusRejected === 1 || priorStatusRejected === '1' || priorStatusRejected === true;

        // Return true if either indicates rejection
        return currentlyRejected || historicallyRejected;
      };

      // Helper function to check if project is currently rejected
      const isCurrentlyRejected = (p: Record<string, any>): boolean => {
        const intakeStatus = (p[PROJECT_FIELDS.INTAKE_STATUS]?.value || '').toString().toLowerCase();
        const projectStatus = (p[PROJECT_FIELDS.PROJECT_STATUS]?.value || '').toString().toLowerCase();
        return intakeStatus.includes('rejected') || projectStatus.includes('rejected');
      };

      // Never Rejected (first pass approved) - has completion date AND never rejected
      const neverRejected = repProjects.filter(p =>
        p[PROJECT_FIELDS.INTAKE_COMPLETED_DATE]?.value &&
        !wasEverRejected(p)
      ).length;

      // First-time pass rate (KEY METRIC - clean deals approved on first attempt)
      const firstTimePassRate = totalProjects > 0
        ? (neverRejected / totalProjects) * 100
        : 0;

      // Total Rejections (ever rejected - using hybrid detection)
      const rejectedProjects = repProjects.filter(wasEverRejected);
      const rejections = rejectedProjects.length;
      const rejectionRate = totalProjects > 0
        ? (rejections / totalProjects) * 100
        : 0;

      // Resubmit success rate (of rejected projects, how many eventually got approved)
      const eventuallyApproved = rejectedProjects.filter(p =>
        p[PROJECT_FIELDS.INTAKE_COMPLETED_DATE]?.value &&
        !isCurrentlyRejected(p)
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

      // Top rejection reasons (only from rejected projects)
      const reasonCounts: Record<string, number> = {};
      rejectedProjects.forEach(p => {
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
        isCurrentlyRejected(p)
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
      where: `{3}.EX.${taskId}}` // Filter by record ID (field 3) = task ID
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
      where: `{3}.EX.${templateId}}`
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
      where: `{6}.EX.${taskId}}` // Filter by related task (field 6) = task ID
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

    // Send email notification (async, don't await to avoid blocking)
    const { sendTaskNotificationEmail } = await import('@/lib/utils/task-email-sender');
    sendTaskNotificationEmail({
      type: params.type,
      userId: params.userId,
      projectId: params.projectId,
      taskName: params.taskName,
      taskCategory: params.taskCategory,
      submissionId: params.submissionId,
      opsDisposition: params.opsDisposition,
      opsFeedback: params.opsFeedback,
      totalTasks: params.totalTasks,
      approvedTasks: params.approvedTasks
    }).catch(emailError => {
      // Log email errors but don't throw - email failures shouldn't break notifications
      console.error('[createTaskNotification] Email sending failed:', emailError);
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
      .map((task: any) => task[TASK_FIELDS.DATE_CREATED]?.value)
      .filter(Boolean)
      .map(dateStr => new Date(dateStr));

    if (taskDates.length === 0) {
      console.log('[calculateTaskBasedResolutionTime] No task creation dates found');
      return null;
    }

    const earliestTaskDate = new Date(Math.min(...taskDates.map(d => d.getTime())));

    // Step 3: Query all submissions for these tasks
    const taskIds = tasksResponse.data.map((task: any) => task[TASK_FIELDS.RECORD_ID]?.value).filter(Boolean);

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

// ============================================================
// PC (PROJECT COORDINATOR) DASHBOARD QUERIES
// ============================================================

/**
 * Get PC Projects Data (Consolidated)
 * Fetches all project data needed for the dashboard in a single query
 * This optimized function reduces API calls from 3 to 1 for better performance
 */
export async function getPCProjectsData(
  pcEmail: string,
  pcName: string,
  role: string,
  reqId: string
): Promise<any[]> {
  try {
    logQuickbaseRequest('getPCProjectsData', { pcEmail, pcName, role, reqId });

    // Build WHERE clause based on role
    const sanitizedEmail = sanitizeQbLiteral(pcEmail);
    const sanitizedName = sanitizeQbLiteral(pcName);

    let whereClause: string;
    const { hasOperationsUnrestrictedAccess, isOperationsManager } = await import('@/lib/utils/role-helpers');

    if (hasOperationsUnrestrictedAccess(role)) {
      // Super admin, regional, office leaders see ALL projects
      whereClause = `{${PROJECT_FIELDS.RECORD_ID}.GT.0}`;
    } else if (isOperationsManager(role)) {
      // Operations managers see all operations projects (any project with a PC)
      whereClause = `{${PROJECT_FIELDS.PROJECT_COORDINATOR}.XEX.''}`;
    } else {
      // Operations coordinators see only their assigned projects
      whereClause = `({${PROJECT_FIELDS.PROJECT_COORDINATOR_EMAIL}.EX.'${sanitizedEmail}'})OR({${PROJECT_FIELDS.PROJECT_COORDINATOR}.EX.'${sanitizedName}'})`;
    }

    // Fetch all fields needed by metrics, priority queue, and pipeline
    const query = {
      from: 'br9kwm8na', // Projects table
      where: whereClause,
      select: [
        PROJECT_FIELDS.RECORD_ID,
        PROJECT_FIELDS.PROJECT_ID,
        PROJECT_FIELDS.CUSTOMER_NAME,
        PROJECT_FIELDS.CUSTOMER_PHONE,
        PROJECT_FIELDS.PROJECT_STATUS,
        // Metrics fields
        PROJECT_FIELDS.PC_OUTREACH_DUE,
        PROJECT_FIELDS.PC_UNRESPONSIVE_COUNT,
        PROJECT_FIELDS.PC_IS_UNRESPONSIVE,
        PROJECT_FIELDS.PC_ESCALATIONS,
        PROJECT_FIELDS.INSTALL_COMPLETED_DATE,
        PROJECT_FIELDS.DESIGN_SLA_BREACH,
        PROJECT_FIELDS.INTAKE_COMPLETED_DATE,
        PROJECT_FIELDS.SURVEY_APPROVED,
        PROJECT_FIELDS.DESIGN_COMPLETED,
        PROJECT_FIELDS.PERMIT_APPROVED,
        PROJECT_FIELDS.NEM_SUBMITTED,
        PROJECT_FIELDS.PASSING_INSPECTION_COMPLETED,
        PROJECT_FIELDS.PTO_APPROVED,
        // Priority queue fields
        PROJECT_FIELDS.PC_DAYS_SINCE_CONTACT_INTAKE,
        PROJECT_FIELDS.PC_DAYS_SINCE_CONTACT_NEM,
        PROJECT_FIELDS.PC_DAYS_SINCE_CONTACT_PTO,
        PROJECT_FIELDS.PC_DAYS_SINCE_CONTACT_INSTALL,
        PROJECT_FIELDS.PC_CONTACT_ATTEMPTS_INTAKE,
        PROJECT_FIELDS.PC_CONTACT_ATTEMPTS_NEM,
        PROJECT_FIELDS.PC_CONTACT_ATTEMPTS_PTO,
        PROJECT_FIELDS.PC_CONTACT_ATTEMPTS_INSTALL,
        PROJECT_FIELDS.PC_OUTREACH_PREFERRED_METHOD,
        PROJECT_FIELDS.PC_MAX_OUTREACH_COMPLETED,
        PROJECT_FIELDS.CLOSER_NAME,
        PROJECT_FIELDS.CLOSER_EMAIL,
        // Pipeline fields
        PROJECT_FIELDS.INTAKE_STATUS,
        PROJECT_FIELDS.SURVEY_STATUS,
        PROJECT_FIELDS.DESIGN_STATUS,
        PROJECT_FIELDS.PERMIT_STATUS,
        PROJECT_FIELDS.NEM_INTERCONNECTION_STATUS,
        PROJECT_FIELDS.PTO_STATUS,
        PROJECT_FIELDS.PROJECT_AGE
      ],
      sortBy: [{ field: PROJECT_FIELDS.DATE_CREATED, order: 'DESC' }],
      options: { top: 5000 }
    };

    const response = await qbClient.queryRecords(query);
    logQuickbaseResponse('getPCProjectsData', response);

    return response.data || [];

  } catch (error) {
    logQuickbaseError('getPCProjectsData', error as Error, { pcEmail, reqId });
    throw error;
  }
}

/**
 * Get PC Dashboard Metrics
 * Fetches key metrics for PC dashboard including total projects, pending outreach, etc.
 * @param projectsData - Optional pre-fetched project data for performance optimization
 */
export async function getPCDashboardMetrics(
  pcEmail: string,
  pcName: string,
  role: string,
  reqId: string,
  projectsData?: any[]
): Promise<PCDashboardMetrics> {
  try {
    logQuickbaseRequest('getPCDashboardMetrics', { pcEmail, pcName, role, reqId, usingCache: !!projectsData });

    let records: any[];

    // Use pre-fetched data if available (optimized path)
    if (projectsData) {
      records = projectsData;
    } else {
      // Fall back to individual query (backward compatibility)
      const sanitizedEmail = sanitizeQbLiteral(pcEmail);
      const sanitizedName = sanitizeQbLiteral(pcName);

      let whereClause: string;
      const { hasOperationsUnrestrictedAccess, isOperationsManager } = await import('@/lib/utils/role-helpers');

      if (hasOperationsUnrestrictedAccess(role)) {
        // Super admin, regional, office leaders see ALL projects
        whereClause = `{${PROJECT_FIELDS.RECORD_ID}.GT.0}`;
      } else if (isOperationsManager(role)) {
        // Operations managers see all operations projects (any project with a PC)
        whereClause = `{${PROJECT_FIELDS.PROJECT_COORDINATOR}.XEX.''}`;
      } else {
        // Operations coordinators see only their assigned projects
        whereClause = `({${PROJECT_FIELDS.PROJECT_COORDINATOR_EMAIL}.EX.'${sanitizedEmail}'})OR({${PROJECT_FIELDS.PROJECT_COORDINATOR}.EX.'${sanitizedName}'})`;
      }

      const query = {
        from: 'br9kwm8na', // Projects table
        where: whereClause,
        select: [
          PROJECT_FIELDS.RECORD_ID,
          PROJECT_FIELDS.PC_OUTREACH_DUE,
          PROJECT_FIELDS.PC_UNRESPONSIVE_COUNT,
          PROJECT_FIELDS.PC_IS_UNRESPONSIVE,
          PROJECT_FIELDS.PC_ESCALATIONS,
          PROJECT_FIELDS.INSTALL_COMPLETED_DATE,
          PROJECT_FIELDS.DESIGN_SLA_BREACH,
          // Add milestone completion dates for comprehensive SLA calculation
          PROJECT_FIELDS.INTAKE_COMPLETED_DATE,
          PROJECT_FIELDS.SURVEY_APPROVED,
          PROJECT_FIELDS.DESIGN_COMPLETED,
          PROJECT_FIELDS.PERMIT_APPROVED,
          PROJECT_FIELDS.NEM_SUBMITTED,
          PROJECT_FIELDS.INSTALL_COMPLETED_DATE,
          PROJECT_FIELDS.PASSING_INSPECTION_COMPLETED,
          PROJECT_FIELDS.PTO_APPROVED
        ],
        sortBy: [{ field: PROJECT_FIELDS.DATE_CREATED, order: 'DESC' }],
        options: { top: 5000 }
      };

      const response = await qbClient.queryRecords(query);
      logQuickbaseResponse('getPCDashboardMetrics', response);

      records = response.data || [];
    }
    const today = new Date().toISOString().split('T')[0];

    // Calculate metrics
    const totalProjects = records.length;
    const pendingOutreach = records.filter(r => extractNumericValue(r[PROJECT_FIELDS.PC_OUTREACH_DUE]) > 0).length;
    const unresponsiveCustomers = records.filter(r => {
      const isUnresponsive = r[PROJECT_FIELDS.PC_IS_UNRESPONSIVE];
      const isUnresponsiveValue = typeof isUnresponsive === 'object' ? isUnresponsive?.value : isUnresponsive;
      return isUnresponsiveValue === 'Yes' || extractNumericValue(r[PROJECT_FIELDS.PC_UNRESPONSIVE_COUNT]) > 0;
    }).length;
    const activeEscalations = records.filter(r => extractNumericValue(r[PROJECT_FIELDS.PC_ESCALATIONS]) > 0).length;
    const todaysInstalls = records.filter(r => {
      const installDate = r[PROJECT_FIELDS.INSTALL_COMPLETED_DATE];
      if (!installDate) return false;
      // Extract value from wrapped object if needed
      const dateValue = typeof installDate === 'object' && installDate?.value ? installDate.value : installDate;
      const dateStr = String(dateValue);
      return dateStr.startsWith(today);
    }).length;
    
    // Calculate comprehensive SLA compliance across all relevant milestones
    const slaCompliance = calculateComprehensiveSLACompliance(records);

    const metrics: PCDashboardMetrics = {
      totalProjects,
      pendingOutreach,
      unresponsiveCustomers,
      activeEscalations,
      todaysInstalls,
      slaCompliance
    };

    logQuickbaseRequest('getPCDashboardMetrics', { metrics, reqId });
    return metrics;

  } catch (error) {
    logQuickbaseError('getPCDashboardMetrics', error as Error, { pcEmail, reqId });
    throw error;
  }
}

/**
 * Get PC Priority Queue
 * Fetches projects with priority scoring algorithm for PC outreach recommendations
 * @param projectsData - Optional pre-fetched project data for performance optimization
 */
export async function getPCPriorityQueue(
  pcEmail: string,
  pcName: string,
  role: string,
  limit: number = 10,
  reqId: string,
  projectsData?: any[]
): Promise<PCPriorityQueueItem[]> {
  try {
    logQuickbaseRequest('getPCPriorityQueue', { pcEmail, pcName, role, limit, reqId, usingCache: !!projectsData });

    let records: any[];

    // Use pre-fetched data if available (optimized path)
    if (projectsData) {
      // Filter for active projects only
      records = projectsData.filter(r => {
        const status = r[PROJECT_FIELDS.PROJECT_STATUS];
        const statusStr = typeof status === 'object' ? status?.value : status;
        return statusStr && String(statusStr).toLowerCase().includes('active');
      });
    } else {
      // Fall back to individual query (backward compatibility)
      const sanitizedEmail = sanitizeQbLiteral(pcEmail);
      const sanitizedName = sanitizeQbLiteral(pcName);

      let pcFilter: string;
      const { hasOperationsUnrestrictedAccess, isOperationsManager } = await import('@/lib/utils/role-helpers');

      if (hasOperationsUnrestrictedAccess(role)) {
        // Super admin, regional, office leaders see ALL projects
        pcFilter = `{${PROJECT_FIELDS.RECORD_ID}.GT.0}`;
      } else if (isOperationsManager(role)) {
        // Operations managers see all operations projects (any project with a PC)
        pcFilter = `{${PROJECT_FIELDS.PROJECT_COORDINATOR}.XEX.''}`;
      } else {
        // Operations coordinators see only their assigned projects
        pcFilter = `({${PROJECT_FIELDS.PROJECT_COORDINATOR_EMAIL}.EX.'${sanitizedEmail}'})OR({${PROJECT_FIELDS.PROJECT_COORDINATOR}.EX.'${sanitizedName}'})`;
      }

      const whereClause = `(${pcFilter})AND{${PROJECT_FIELDS.PROJECT_STATUS}.CT.'Active'}`;

      const query = {
        from: 'br9kwm8na', // Projects table
        where: whereClause,
        select: [
          PROJECT_FIELDS.RECORD_ID,
          PROJECT_FIELDS.PROJECT_ID,
          PROJECT_FIELDS.CUSTOMER_NAME,
          PROJECT_FIELDS.CUSTOMER_PHONE,
          PROJECT_FIELDS.PROJECT_STATUS,
          PROJECT_FIELDS.PC_DAYS_SINCE_CONTACT_INTAKE,
          PROJECT_FIELDS.PC_DAYS_SINCE_CONTACT_NEM,
          PROJECT_FIELDS.PC_DAYS_SINCE_CONTACT_PTO,
          PROJECT_FIELDS.PC_DAYS_SINCE_CONTACT_INSTALL,
          PROJECT_FIELDS.PC_CONTACT_ATTEMPTS_INTAKE,
          PROJECT_FIELDS.PC_CONTACT_ATTEMPTS_NEM,
          PROJECT_FIELDS.PC_CONTACT_ATTEMPTS_PTO,
          PROJECT_FIELDS.PC_CONTACT_ATTEMPTS_INSTALL,
          PROJECT_FIELDS.PC_OUTREACH_DUE,
          PROJECT_FIELDS.PC_ESCALATIONS,
          PROJECT_FIELDS.PC_IS_UNRESPONSIVE,
          PROJECT_FIELDS.PC_OUTREACH_PREFERRED_METHOD,
          PROJECT_FIELDS.PC_MAX_OUTREACH_COMPLETED,
          PROJECT_FIELDS.CLOSER_NAME,
          PROJECT_FIELDS.CLOSER_EMAIL,
          PROJECT_FIELDS.INTAKE_STATUS,
          PROJECT_FIELDS.SURVEY_STATUS,
          PROJECT_FIELDS.DESIGN_STATUS,
          PROJECT_FIELDS.PERMIT_STATUS,
          PROJECT_FIELDS.NEM_INTERCONNECTION_STATUS,
          PROJECT_FIELDS.PTO_STATUS
        ],
        sortBy: [{ field: PROJECT_FIELDS.DATE_CREATED, order: 'DESC' }],
        options: { top: 5000 }
      };

      const response = await qbClient.queryRecords(query);
      logQuickbaseResponse('getPCPriorityQueue', response);

      records = response.data || [];
    }
    
    // Apply priority scoring algorithm
    const priorityItems: PCPriorityQueueItem[] = records.map(record => {
      const currentStage = determineCurrentStage(record);
      const contactData = getContactDataForStage(record, currentStage);
      const { score, reason } = calculatePriorityScore(record, currentStage, contactData);
      
      return {
        recordId: extractNumericValue(record[PROJECT_FIELDS.RECORD_ID]),
        projectId: record[PROJECT_FIELDS.PROJECT_ID] || '',
        customerName: record[PROJECT_FIELDS.CUSTOMER_NAME] || '',
        customerPhone: record[PROJECT_FIELDS.CUSTOMER_PHONE] || '',
        currentStage,
        daysInStage: contactData.daysInStage,
        priorityScore: score,
        priorityReason: reason,
        daysSinceContact: contactData.daysSinceContact,
        contactAttempts: contactData.contactAttempts,
        dueMilestones: extractNumericValue(record[PROJECT_FIELDS.PC_OUTREACH_DUE]),
        isUnresponsive: record[PROJECT_FIELDS.PC_IS_UNRESPONSIVE] === 'Yes',
        escalationLevel: extractNumericValue(record[PROJECT_FIELDS.PC_ESCALATIONS]),
        preferredContactMethod: (typeof record[PROJECT_FIELDS.PC_OUTREACH_PREFERRED_METHOD] === 'object'
          ? record[PROJECT_FIELDS.PC_OUTREACH_PREFERRED_METHOD]?.value
          : record[PROJECT_FIELDS.PC_OUTREACH_PREFERRED_METHOD]) || 'Call',
        lastContactDate: record[PROJECT_FIELDS.PC_MAX_OUTREACH_COMPLETED] || null,
        coordinatorEmail: pcEmail,
        salesRepName: record[PROJECT_FIELDS.CLOSER_NAME] || '',
        salesRepEmail: record[PROJECT_FIELDS.CLOSER_EMAIL] || ''
      };
    });

    // Sort by priority score descending and return top N
    const sortedItems = priorityItems.sort((a, b) => b.priorityScore - a.priorityScore);
    const topItems = sortedItems.slice(0, limit);

    logQuickbaseRequest('getPCPriorityQueue', { itemCount: topItems.length, reqId });
    return topItems;

  } catch (error) {
    logQuickbaseError('getPCPriorityQueue', error as Error, { pcEmail, limit, reqId });
    throw error;
  }
}

/**
 * Get PC Project Pipeline
 * Fetches projects grouped by current milestone stage
 * @param projectsData - Optional pre-fetched project data for performance optimization
 */
export async function getPCProjectPipeline(
  pcEmail: string,
  pcName: string,
  role: string,
  reqId: string,
  projectsData?: any[]
): Promise<PCProjectPipelineStage[]> {
  try {
    logQuickbaseRequest('getPCProjectPipeline', { pcEmail, pcName, role, reqId, usingCache: !!projectsData });

    let records: any[];

    // Use pre-fetched data if available (optimized path)
    if (projectsData) {
      // Filter for active projects only
      records = projectsData.filter(r => {
        const status = r[PROJECT_FIELDS.PROJECT_STATUS];
        const statusStr = typeof status === 'object' ? status?.value : status;
        return statusStr && String(statusStr).toLowerCase().includes('active');
      });
    } else {
      // Fall back to individual query (backward compatibility)
      const sanitizedEmail = sanitizeQbLiteral(pcEmail);
      const sanitizedName = sanitizeQbLiteral(pcName);

      let pcFilter: string;
      const { hasOperationsUnrestrictedAccess, isOperationsManager } = await import('@/lib/utils/role-helpers');

      if (hasOperationsUnrestrictedAccess(role)) {
        pcFilter = `{${PROJECT_FIELDS.RECORD_ID}.GT.0}`;
      } else if (isOperationsManager(role)) {
        pcFilter = `{${PROJECT_FIELDS.PROJECT_COORDINATOR}.XEX.''}`;
      } else {
        pcFilter = `({${PROJECT_FIELDS.PROJECT_COORDINATOR_EMAIL}.EX.'${sanitizedEmail}'})OR({${PROJECT_FIELDS.PROJECT_COORDINATOR}.EX.'${sanitizedName}'})`;
      }

      const whereClause = `(${pcFilter})AND{${PROJECT_FIELDS.PROJECT_STATUS}.CT.'Active'}`;

      const query = {
        from: 'br9kwm8na', // Projects table
        where: whereClause,
        select: [
          PROJECT_FIELDS.RECORD_ID,
          PROJECT_FIELDS.PROJECT_ID,
          PROJECT_FIELDS.CUSTOMER_NAME,
          PROJECT_FIELDS.INTAKE_STATUS,
          PROJECT_FIELDS.SURVEY_STATUS,
          PROJECT_FIELDS.DESIGN_STATUS,
          PROJECT_FIELDS.PERMIT_STATUS,
          PROJECT_FIELDS.NEM_INTERCONNECTION_STATUS,
          PROJECT_FIELDS.PTO_STATUS,
          PROJECT_FIELDS.PROJECT_AGE
        ],
        sortBy: [{ field: PROJECT_FIELDS.DATE_CREATED, order: 'DESC' }],
        options: { top: 5000 }
      };

      const response = await qbClient.queryRecords(query);
      logQuickbaseResponse('getPCProjectPipeline', response);

      records = response.data || [];
    }
    
    // Group projects by current stage
    const stages: { [key: string]: any[] } = {
      'Intake': [],
      'Survey': [],
      'Design': [],
      'Permit': [],
      'NEM': [],
      'Install': [],
      'PTO': []
    };

    records.forEach(record => {
      const currentStage = determineCurrentStage(record);
      const projectInfo = {
        projectId: record[PROJECT_FIELDS.PROJECT_ID] || '',
        customerName: record[PROJECT_FIELDS.CUSTOMER_NAME] || '',
        daysInStage: calculateDaysInCurrentStage(record, currentStage)
      };
      
      if (stages[currentStage]) {
        stages[currentStage].push(projectInfo);
      }
    });

    // Convert to PCProjectPipelineStage format
    const pipelineStages: PCProjectPipelineStage[] = Object.entries(stages).map(([stageName, projects]) => ({
      stageName,
      projectCount: projects.length,
      projects: projects.slice(0, 5) // Show first 5 projects per stage
    }));

    logQuickbaseRequest('getPCProjectPipeline', { stageCount: pipelineStages.length, reqId });
    return pipelineStages;

  } catch (error) {
    logQuickbaseError('getPCProjectPipeline', error as Error, { pcEmail, reqId });
    throw error;
  }
}

/**
 * Get PC Activity Feed
 * Fetches recent activity from Install Communications table
 */
export async function getPCActivityFeed(
  pcEmail: string,
  pcName: string,
  role: string,
  limit: number = 20,
  reqId: string
): Promise<PCActivityFeedItem[]> {
  try {
    logQuickbaseRequest('getPCActivityFeed', { pcEmail, pcName, role, limit, reqId });

    // Import role helpers
    const { hasOperationsUnrestrictedAccess, isOperationsManager } = await import('@/lib/utils/role-helpers');

    // Get projects based on role
    const sanitizedEmail = sanitizeQbLiteral(pcEmail);
    const sanitizedName = sanitizeQbLiteral(pcName);

    let projectsWhere: string;
    if (hasOperationsUnrestrictedAccess(role)) {
      // Super admin, regional, office leaders see ALL projects
      projectsWhere = `{${PROJECT_FIELDS.RECORD_ID}.GT.0}`;
    } else if (isOperationsManager(role)) {
      // Operations managers see all operations projects (any project with a PC)
      projectsWhere = `{${PROJECT_FIELDS.PROJECT_COORDINATOR}.XEX.''}`;
    } else {
      // Operations coordinators see only their assigned projects
      projectsWhere = `({${PROJECT_FIELDS.PROJECT_COORDINATOR_EMAIL}.EX.'${sanitizedEmail}'})OR({${PROJECT_FIELDS.PROJECT_COORDINATOR}.EX.'${sanitizedName}'})`;
    }

    const projectsQuery = {
      from: 'br9kwm8na', // Projects table
      where: projectsWhere,
      select: [PROJECT_FIELDS.RECORD_ID, PROJECT_FIELDS.PROJECT_ID, PROJECT_FIELDS.CUSTOMER_NAME],
      sortBy: [{ field: PROJECT_FIELDS.DATE_CREATED, order: 'DESC' }],
      options: { top: 500 }
    };

    const projectsResponse = await qbClient.queryRecords(projectsQuery);
    const projectRecords = projectsResponse.data || [];
    const projectIds = projectRecords.map(p => {
      const recordId = p[PROJECT_FIELDS.RECORD_ID];
      return typeof recordId === 'object' && recordId?.value ? recordId.value : recordId;
    });

    if (projectIds.length === 0) {
      return [];
    }

    // Query Install Communications table with proper date range and project linking
    // Use XIN operator (in list) instead of OR chain for better performance
    const dateRange = getDateRange(7);
    const projectIdsStr = projectIds.join(',');
    const whereClause = `{${INSTALL_COMMUNICATION_FIELDS.DATE}.AF.'${dateRange}'}AND{${INSTALL_COMMUNICATION_FIELDS.RELATED_PROJECT}.XIN.'${projectIdsStr}'}`;
    
    const query = {
      from: 'bsb6bqt3b', // Install Communications table
      where: whereClause,
      select: [
        INSTALL_COMMUNICATION_FIELDS.RECORD_ID,
        INSTALL_COMMUNICATION_FIELDS.DATE,
        INSTALL_COMMUNICATION_FIELDS.NOTE_BY,
        INSTALL_COMMUNICATION_FIELDS.RELATED_PROJECT,
        INSTALL_COMMUNICATION_FIELDS.COMMUNICATION_NOTE,
        INSTALL_COMMUNICATION_FIELDS.NEM_BLOCKER_OUTREACH
      ],
      sortBy: [{ field: INSTALL_COMMUNICATION_FIELDS.DATE, order: 'DESC' }],
      options: { top: 5000 }
    };

    const response = await qbClient.queryRecords(query);
    logQuickbaseResponse('getPCActivityFeed', response);

    const records = response.data || [];
    
    // Create project lookup map
    const projectMap = new Map();
    projectRecords.forEach(project => {
      projectMap.set(project[PROJECT_FIELDS.RECORD_ID], {
        projectId: project[PROJECT_FIELDS.PROJECT_ID],
        customerName: project[PROJECT_FIELDS.CUSTOMER_NAME]
      });
    });

    // Convert to PCActivityFeedItem format
    const activityItems: PCActivityFeedItem[] = records
      .map(record => {
        const projectInfo = projectMap.get(record[INSTALL_COMMUNICATION_FIELDS.RELATED_PROJECT]);
        return {
          recordId: record[INSTALL_COMMUNICATION_FIELDS.RECORD_ID],
          date: record[INSTALL_COMMUNICATION_FIELDS.DATE] || new Date().toISOString(),
          noteBy: record[INSTALL_COMMUNICATION_FIELDS.NOTE_BY] || '',
          projectId: projectInfo?.projectId || '',
          customerName: projectInfo?.customerName || '',
          note: record[INSTALL_COMMUNICATION_FIELDS.COMMUNICATION_NOTE] || '',
          isNemBlocker: record[INSTALL_COMMUNICATION_FIELDS.NEM_BLOCKER_OUTREACH] === true
        };
      })
      .sort((a, b) => toTimestamp(b.date) - toTimestamp(a.date))
      .slice(0, limit);

    logQuickbaseRequest('getPCActivityFeed', { itemCount: activityItems.length, reqId });
    return activityItems;

  } catch (error) {
    logQuickbaseError('getPCActivityFeed', error as Error, { pcEmail, limit, reqId });
    throw error;
  }
}

// ============================================================
// PC HELPER FUNCTIONS
// ============================================================

/**
 * Determine current project stage based on milestone statuses
 */
function determineCurrentStage(record: any): string {
  // Helper to extract string values from QuickBase wrapped objects
  const extractStringValue = (field: any): string => {
    if (typeof field === 'object' && field?.value !== undefined) {
      return String(field.value);
    }
    return String(field || '');
  };

  const intakeStatus = extractStringValue(record[PROJECT_FIELDS.INTAKE_STATUS]);
  const surveyStatus = extractStringValue(record[PROJECT_FIELDS.SURVEY_STATUS]);
  const designStatus = extractStringValue(record[PROJECT_FIELDS.DESIGN_STATUS]);
  const permitStatus = extractStringValue(record[PROJECT_FIELDS.PERMIT_STATUS]);
  const nemStatus = extractStringValue(record[PROJECT_FIELDS.NEM_INTERCONNECTION_STATUS]);
  const ptoStatus = extractStringValue(record[PROJECT_FIELDS.PTO_STATUS]);

  if (ptoStatus === 'Approved') return 'PTO';
  if (nemStatus === 'Approved') return 'Install';
  if (permitStatus === 'Approved') return 'NEM';
  if (designStatus === 'Approved') return 'Permit';
  if (surveyStatus === 'Approved') return 'Design';
  if (intakeStatus === 'Approved') return 'Survey';
  return 'Intake';
}

/**
 * Get contact data for current stage
 */
function getContactDataForStage(record: any, currentStage: string): {
  daysSinceContact: number;
  contactAttempts: number;
  daysInStage: number;
} {
  let daysSinceContact = 0;
  let contactAttempts = 0;

  switch (currentStage) {
    case 'Intake':
      daysSinceContact = extractNumericValue(record[PROJECT_FIELDS.PC_DAYS_SINCE_CONTACT_INTAKE]);
      contactAttempts = extractNumericValue(record[PROJECT_FIELDS.PC_CONTACT_ATTEMPTS_INTAKE]);
      break;
    case 'Install':
      daysSinceContact = extractNumericValue(record[PROJECT_FIELDS.PC_DAYS_SINCE_CONTACT_INSTALL]);
      contactAttempts = extractNumericValue(record[PROJECT_FIELDS.PC_CONTACT_ATTEMPTS_INSTALL]);
      break;
    case 'NEM':
      daysSinceContact = extractNumericValue(record[PROJECT_FIELDS.PC_DAYS_SINCE_CONTACT_NEM]);
      contactAttempts = extractNumericValue(record[PROJECT_FIELDS.PC_CONTACT_ATTEMPTS_NEM]);
      break;
    case 'PTO':
      daysSinceContact = extractNumericValue(record[PROJECT_FIELDS.PC_DAYS_SINCE_CONTACT_PTO]);
      contactAttempts = extractNumericValue(record[PROJECT_FIELDS.PC_CONTACT_ATTEMPTS_PTO]);
      break;
    default:
      daysSinceContact = extractNumericValue(record[PROJECT_FIELDS.PC_DAYS_SINCE_CONTACT_INTAKE]);
      contactAttempts = extractNumericValue(record[PROJECT_FIELDS.PC_CONTACT_ATTEMPTS_INTAKE]);
  }

  // Calculate days in current stage using milestone transition dates
  const daysInStage = calculateDaysInCurrentStage(record, currentStage);

  return {
    daysSinceContact,
    contactAttempts,
    daysInStage
  };
}

/**
 * Calculate comprehensive SLA compliance across all relevant milestones
 */
function calculateComprehensiveSLACompliance(records: any[]): number {
  if (records.length === 0) return 100;

  // Helper to extract date value from wrapped object
  const extractDateValue = (field: any): string | null => {
    if (!field) return null;
    if (typeof field === 'object' && field?.value !== undefined) {
      return String(field.value);
    }
    return String(field);
  };

  let totalSLAs = 0;
  let metSLAs = 0;

  records.forEach(record => {
    // Define SLA deadlines for each milestone (in days from sales date)
    const slaDeadlines = {
      intake: 7,      // 7 days for intake completion
      survey: 14,    // 14 days for survey approval
      design: 21,    // 21 days for design completion
      permit: 28,    // 28 days for permit approval
      nem: 35,       // 35 days for NEM submission
      install: 42,   // 42 days for install completion
      inspection: 49, // 49 days for inspection completion
      pto: 56        // 56 days for PTO approval
    };

    const salesDateValue = extractDateValue(record[PROJECT_FIELDS.SALES_DATE]);
    if (!salesDateValue) return; // Skip if no sales date

    const salesDateObj = new Date(salesDateValue);
    const today = new Date();

    // Check each milestone SLA
    Object.entries(slaDeadlines).forEach(([milestone, deadlineDays]) => {
      const deadlineDate = new Date(salesDateObj);
      deadlineDate.setDate(deadlineDate.getDate() + deadlineDays);

      // Only count SLA if project is old enough to have reached this milestone
      if (today >= deadlineDate) {
        totalSLAs++;
        
        let milestoneCompleted = false;
        let completionDate: Date | null = null;

        // Check if milestone was completed on time
        switch (milestone) {
          case 'intake':
            const intakeCompletedValue = extractDateValue(record[PROJECT_FIELDS.INTAKE_COMPLETED_DATE]);
            if (intakeCompletedValue) {
              completionDate = new Date(intakeCompletedValue);
              milestoneCompleted = completionDate <= deadlineDate;
            }
            break;

          case 'survey':
            const surveyApprovedValue = extractDateValue(record[PROJECT_FIELDS.SURVEY_APPROVED]);
            if (surveyApprovedValue) {
              completionDate = new Date(surveyApprovedValue);
              milestoneCompleted = completionDate <= deadlineDate;
            }
            break;

          case 'design':
            const designCompletedValue = extractDateValue(record[PROJECT_FIELDS.DESIGN_COMPLETED]);
            if (designCompletedValue) {
              completionDate = new Date(designCompletedValue);
              milestoneCompleted = completionDate <= deadlineDate;
            }
            break;

          case 'permit':
            const permitApprovedValue = extractDateValue(record[PROJECT_FIELDS.PERMIT_APPROVED]);
            if (permitApprovedValue) {
              completionDate = new Date(permitApprovedValue);
              milestoneCompleted = completionDate <= deadlineDate;
            }
            break;

          case 'nem':
            const nemSubmittedValue = extractDateValue(record[PROJECT_FIELDS.NEM_SUBMITTED]);
            if (nemSubmittedValue) {
              completionDate = new Date(nemSubmittedValue);
              milestoneCompleted = completionDate <= deadlineDate;
            }
            break;

          case 'install':
            const installCompletedValue = extractDateValue(record[PROJECT_FIELDS.INSTALL_COMPLETED_DATE]);
            if (installCompletedValue) {
              completionDate = new Date(installCompletedValue);
              milestoneCompleted = completionDate <= deadlineDate;
            }
            break;

          case 'inspection':
            const inspectionCompletedValue = extractDateValue(record[PROJECT_FIELDS.PASSING_INSPECTION_COMPLETED]);
            if (inspectionCompletedValue) {
              completionDate = new Date(inspectionCompletedValue);
              milestoneCompleted = completionDate <= deadlineDate;
            }
            break;

          case 'pto':
            const ptoApprovedValue = extractDateValue(record[PROJECT_FIELDS.PTO_APPROVED]);
            if (ptoApprovedValue) {
              completionDate = new Date(ptoApprovedValue);
              milestoneCompleted = completionDate <= deadlineDate;
            }
            break;
        }

        if (milestoneCompleted) {
          metSLAs++;
        }
      }
    });
  });

  return totalSLAs > 0 ? Math.round((metSLAs / totalSLAs) * 100) : 100;
}

/**
 * Calculate days in current stage using milestone transition dates
 */
function calculateDaysInCurrentStage(record: any, currentStage: string): number {
  const today = new Date();
  let stageStartDate: Date | null = null;

  // Helper to extract date value from wrapped object
  const extractDateValue = (field: any): string | null => {
    if (!field) return null;
    if (typeof field === 'object' && field?.value !== undefined) {
      return String(field.value);
    }
    return String(field);
  };

  // Determine when the current stage began based on milestone completion dates
  switch (currentStage) {
    case 'Intake':
      // Intake stage starts at sales date
      const salesDateValue = extractDateValue(record[PROJECT_FIELDS.SALES_DATE]);
      if (salesDateValue) {
        stageStartDate = new Date(salesDateValue);
      }
      break;
    
    case 'Survey':
      // Survey stage starts when intake is completed
      const intakeCompletedValue = extractDateValue(record[PROJECT_FIELDS.INTAKE_COMPLETED_DATE]);
      if (intakeCompletedValue) {
        stageStartDate = new Date(intakeCompletedValue);
      } else {
        // Fallback to sales date if intake completion not available
        const salesDateValue2 = extractDateValue(record[PROJECT_FIELDS.SALES_DATE]);
        if (salesDateValue2) {
          stageStartDate = new Date(salesDateValue2);
        }
      }
      break;
    
    case 'Design':
      // Design stage starts when survey is approved
      const surveyApprovedValue = extractDateValue(record[PROJECT_FIELDS.SURVEY_APPROVED]);
      if (surveyApprovedValue) {
        stageStartDate = new Date(surveyApprovedValue);
      } else {
        // Fallback to survey submitted if approved not available
        const surveySubmittedValue = extractDateValue(record[PROJECT_FIELDS.SURVEY_SUBMITTED]);
        if (surveySubmittedValue) {
          stageStartDate = new Date(surveySubmittedValue);
        }
      }
      break;

    case 'NEM':
      // NEM stage starts when design is completed
      const designCompletedValue = extractDateValue(record[PROJECT_FIELDS.DESIGN_COMPLETED]);
      if (designCompletedValue) {
        stageStartDate = new Date(designCompletedValue);
      }
      break;

    case 'Permit':
      // Permit stage starts when NEM is submitted
      const nemSubmittedValue = extractDateValue(record[PROJECT_FIELDS.NEM_SUBMITTED]);
      if (nemSubmittedValue) {
        stageStartDate = new Date(nemSubmittedValue);
      }
      break;

    case 'Install':
      // Install stage starts when permit is approved
      const permitApprovedValue = extractDateValue(record[PROJECT_FIELDS.PERMIT_APPROVED]);
      if (permitApprovedValue) {
        stageStartDate = new Date(permitApprovedValue);
      }
      break;

    case 'Inspection':
      // Inspection stage starts when install is completed
      const installCompletedValue = extractDateValue(record[PROJECT_FIELDS.INSTALL_COMPLETED_DATE]);
      if (installCompletedValue) {
        stageStartDate = new Date(installCompletedValue);
      }
      break;

    case 'PTO':
      // PTO stage starts when inspection is completed
      const inspectionCompletedValue = extractDateValue(record[PROJECT_FIELDS.PASSING_INSPECTION_COMPLETED]);
      if (inspectionCompletedValue) {
        stageStartDate = new Date(inspectionCompletedValue);
      }
      break;
  }

  // If no stage start date found, fall back to project age
  if (!stageStartDate || isNaN(stageStartDate.getTime())) {
    return extractNumericValue(record[PROJECT_FIELDS.PROJECT_AGE]) || 0;
  }

  // Calculate days between stage start and today
  const diffTime = today.getTime() - stageStartDate.getTime();
  const diffDays = Math.floor(diffTime / (1000 * 60 * 60 * 24));
  
  return Math.max(0, diffDays);
}

/**
 * Calculate priority score for PC outreach
 */
function calculatePriorityScore(
  record: any,
  currentStage: string,
  contactData: { daysSinceContact: number; contactAttempts: number; daysInStage: number }
): { score: number; reason: string } {
  const { daysSinceContact, contactAttempts } = contactData;
  const dueMilestones = extractNumericValue(record[PROJECT_FIELDS.PC_OUTREACH_DUE]);
  const escalations = extractNumericValue(record[PROJECT_FIELDS.PC_ESCALATIONS]);
  const isUnresponsive = record[PROJECT_FIELDS.PC_IS_UNRESPONSIVE] === 'Yes';

  // Scoring algorithm: (Days Since Contact × 10) + (Attempts × 5) + (Due Milestones × 20) + (Escalation Level) + (Unresponsive Flag × 30)
  let score = (daysSinceContact * 10) + (contactAttempts * 5) + (dueMilestones * 20) + escalations;
  if (isUnresponsive) score += 30;

  // Generate reason string
  const reasons = [];
  if (daysSinceContact > 0) reasons.push(`${daysSinceContact} days since contact`);
  if (contactAttempts > 0) reasons.push(`${contactAttempts} attempts`);
  if (dueMilestones > 0) reasons.push(`${dueMilestones} due milestones`);
  if (escalations > 0) reasons.push(`${escalations} escalations`);
  if (isUnresponsive) reasons.push('unresponsive');

  const reason = reasons.length > 0 ? reasons.join(', ') : 'No recent activity';

  return { score, reason };
}

/**
 * Get date range string for QuickBase queries
 */
function getDateRange(days: number): string {
  const endDate = new Date();
  const startDate = new Date();
  startDate.setDate(startDate.getDate() - days);
  
  return `${startDate.toISOString().split('T')[0]}...${endDate.toISOString().split('T')[0]}`;
}

/**
 * Get projects for milestone monitoring checks
 * Optimized query for PC milestone monitoring job
 */
export async function getProjectsForMilestoneCheck(
  pcEmail: string | null,
  reqId: string
): Promise<any[]> {
  try {
    logInfo('Fetching projects for milestone check', { pcEmail, reqId });

    // Build where clause for active projects
    let whereClause = `{${PROJECT_FIELDS.PROJECT_STATUS}.NE.'Cancelled'}AND{${PROJECT_FIELDS.PROJECT_STATUS}.NE.'Completed'}`;
    
    // Add PC filter if specified
    if (pcEmail) {
      whereClause += `AND{${PROJECT_FIELDS.PROJECT_COORDINATOR_EMAIL}.EX.${pcEmail}}`;
    }

    // Select fields needed for milestone checks using numeric FIDs
    const selectFields = [
      PROJECT_FIELDS.RECORD_ID,
      PROJECT_FIELDS.PROJECT_ID, 
      PROJECT_FIELDS.CUSTOMER_NAME,
      PROJECT_FIELDS.PROJECT_COORDINATOR_EMAIL,
      PROJECT_FIELDS.PROJECT_STATUS,
      PROJECT_FIELDS.PROJECT_AGE,
      // Survey fields
      PROJECT_FIELDS.SURVEY_SCHEDULED_DATE,
      PROJECT_FIELDS.SURVEY_SUBMITTED,
      // Install fields  
      PROJECT_FIELDS.INSTALL_SCHEDULED_DATE_CAPTURE,
      PROJECT_FIELDS.INSTALL_COMPLETED_DATE,
      // NEM fields
      PROJECT_FIELDS.NEM_INTERCONNECTION_STATUS,
      PROJECT_FIELDS.NEM_SUBMITTED,
      // PTO fields
      PROJECT_FIELDS.PTO_STATUS,
      PROJECT_FIELDS.PTO_APPROVED,
      // Contact tracking fields
      PROJECT_FIELDS.PC_IS_UNRESPONSIVE,
      PROJECT_FIELDS.PC_DAYS_SINCE_CONTACT_INTAKE,
      PROJECT_FIELDS.PC_DAYS_SINCE_CONTACT_INSTALL, 
      PROJECT_FIELDS.PC_DAYS_SINCE_CONTACT_NEM,
      PROJECT_FIELDS.PC_DAYS_SINCE_CONTACT_PTO,
      PROJECT_FIELDS.PC_CONTACT_ATTEMPTS_INTAKE,
      PROJECT_FIELDS.PC_CONTACT_ATTEMPTS_INSTALL,
      PROJECT_FIELDS.PC_CONTACT_ATTEMPTS_NEM,
      PROJECT_FIELDS.PC_CONTACT_ATTEMPTS_PTO,
      // Milestone status fields
      PROJECT_FIELDS.INTAKE_STATUS,
      PROJECT_FIELDS.SURVEY_STATUS, 
      PROJECT_FIELDS.DESIGN_STATUS,
      PROJECT_FIELDS.PERMIT_STATUS
    ];

    const response = await qbClient.queryRecords({
      from: QB_TABLE_PROJECTS,
      where: whereClause,
      select: selectFields,
      options: { top: 1000 }
    });

    // Normalize the response - extract data from QuickBase response format
    const records = response.data || [];
    
    // Normalize each record to use FID-based access
    const normalizedRecords = records.map((record: any) => {
      const normalized: any = {};
      
      // Map each field ID to its value
      for (const [fid, value] of Object.entries(record)) {
        const fieldId = parseInt(fid);
        if (!isNaN(fieldId)) {
          // Extract the actual value from QuickBase cell format
          normalized[fieldId] = value?.value || value;
        }
      }
      
      return normalized;
    });

    logInfo('Successfully fetched projects for milestone check', {
      pcEmail,
      reqId,
      recordCount: normalizedRecords.length
    });

    return normalizedRecords;

  } catch (error) {
    logError('Failed to fetch projects for milestone check', error, {
      pcEmail,
      reqId
    });
    throw error;
  }
}

/**
 * Log notification to QuickBase audit trail
 * Creates a record in the appropriate audit/log table
 */
export async function logNotificationToQuickBase(
  notificationId: number,
  type: string,
  projectId: string,
  userId: string,
  metadata: any
): Promise<void> {
  try {
    logInfo('Logging notification to QuickBase audit trail', {
      notificationId,
      type,
      projectId,
      userId
    });

    // Create audit record in QuickBase
    const auditData = [{
      [NOTIFICATION_AUDIT_FIELDS.NOTIFICATION_ID]: { value: notificationId },
      [NOTIFICATION_AUDIT_FIELDS.NOTIFICATION_TYPE]: { value: type },
      [NOTIFICATION_AUDIT_FIELDS.PROJECT_ID]: { value: projectId },
      [NOTIFICATION_AUDIT_FIELDS.USER_ID]: { value: userId },
      [NOTIFICATION_AUDIT_FIELDS.TIMESTAMP]: { value: new Date().toISOString() },
      [NOTIFICATION_AUDIT_FIELDS.METADATA]: { value: JSON.stringify(metadata) },
      [NOTIFICATION_AUDIT_FIELDS.STATUS]: { value: 'sent' }
    }];

    const response = await qbClient.updateRecord({
      to: QB_TABLE_NOTIFICATION_AUDIT,
      data: auditData
    });

    logInfo('Successfully logged notification to QuickBase audit trail', {
      notificationId,
      type,
      projectId,
      userId,
      auditRecordId: response.metadata?.totalNumberOfRecordsProcessed
    });
  } catch (error) {
    // Log error but don't fail the notification creation
    logError('Failed to log notification to QuickBase audit trail', error, {
      notificationId,
      type,
      projectId,
      userId
    });
  }
}

// ============================================================
// PC OUTREACH MANAGEMENT QUERIES
// ============================================================

/**
 * Get initial outreach records for PC
 * Fetches outreach records where due date <= today AND status != 'Complete'
 */
export async function getPCOutreachInitial(
  pcEmail: string,
  pcName: string,
  role: string,
  reqId: string
): Promise<PCOutreachRecord[]> {
  try {
    logQuickbaseRequest('getPCOutreachInitial', 'btvik5kwi', { pcEmail, pcName, role, reqId });

    // Import role helpers
    const { hasOperationsUnrestrictedAccess, isOperationsManager } = await import('@/lib/utils/role-helpers');

    const sanitizedEmail = sanitizeQbLiteral(pcEmail);
    const sanitizedName = sanitizeQbLiteral(pcName);

    // Build WHERE clause based on role - optimize to skip project fetch for unrestricted roles
    let whereClause: string;
    if (hasOperationsUnrestrictedAccess(role)) {
      // Super admin, regional, office leaders see ALL outreach records - no project filter needed
      whereClause = `{${OUTREACH_RECORD_FIELDS.REPORTING_DUE_DATE}.OBF.TODAY} AND {${OUTREACH_RECORD_FIELDS.OUTREACH_STATUS}.XEX.'Complete'}`;
    } else {
      // For restricted roles, we need to fetch accessible projects first
      let projectsWhere: string;
      if (isOperationsManager(role)) {
        projectsWhere = `{${PROJECT_FIELDS.PROJECT_COORDINATOR}.XEX.''}`;
      } else {
        projectsWhere = `({${PROJECT_FIELDS.PROJECT_COORDINATOR_EMAIL}.EX.'${sanitizedEmail}'})OR({${PROJECT_FIELDS.PROJECT_COORDINATOR}.EX.'${sanitizedName}'})`;
      }

      const projectsQuery = {
        from: QB_TABLE_PROJECTS,
        where: projectsWhere,
        select: [PROJECT_FIELDS.RECORD_ID]
      };

      const projectsResponse = await qbClient.queryRecords(projectsQuery);
      const projectIds = (projectsResponse.data || []).map((p: any) => extractNumericValue(p[PROJECT_FIELDS.RECORD_ID]));

      if (projectIds.length === 0) {
        return [];
      }

      // Query Outreach Records table for initial outreach, filtered by accessible projects
      const projectIdsStr = projectIds.join(',');
      whereClause = `{${OUTREACH_RECORD_FIELDS.RELATED_PROJECT}.XIN.${projectIdsStr}} AND {${OUTREACH_RECORD_FIELDS.REPORTING_DUE_DATE}.OBF.TODAY} AND {${OUTREACH_RECORD_FIELDS.OUTREACH_STATUS}.XEX.'Complete'}`;
    }

    const selectFields = [
      OUTREACH_RECORD_FIELDS.RECORD_ID,
      OUTREACH_RECORD_FIELDS.DATE_CREATED,
      OUTREACH_RECORD_FIELDS.RELATED_PROJECT,
      OUTREACH_RECORD_FIELDS.OUTREACH_COMPLETED_DATE,
      OUTREACH_RECORD_FIELDS.OUTREACH_STATUS,
      OUTREACH_RECORD_FIELDS.NUM_ATTEMPTS,
      OUTREACH_RECORD_FIELDS.REPORTING_DUE_DATE,
      OUTREACH_RECORD_FIELDS.NEXT_OUTREACH_DUE_DATE,
      OUTREACH_RECORD_FIELDS.NOTE,
      OUTREACH_RECORD_FIELDS.ATTEMPT_NOTE,
      OUTREACH_RECORD_FIELDS.PROJECT_COORDINATOR,
      OUTREACH_RECORD_FIELDS.PROJECT_STAGE_LOOKUP,
      OUTREACH_RECORD_FIELDS.PROJECT_STATUS_LOOKUP
    ];

    const response = await qbClient.queryRecords({
      from: QB_TABLE_OUTREACH_RECORDS,
      where: whereClause,
      select: selectFields,
      sortBy: [{ field: OUTREACH_RECORD_FIELDS.DATE_CREATED, order: 'DESC' }],
      options: { top: 5000 }
    });

    const outreachRecords = response.data || [];

    // Get project data for each outreach record
    const relatedProjectIds = outreachRecords.map((record: any) => record[OUTREACH_RECORD_FIELDS.RELATED_PROJECT]?.value || record[OUTREACH_RECORD_FIELDS.RELATED_PROJECT]);
    const uniqueProjectIds = [...new Set(relatedProjectIds.filter(Boolean))];

    let projectData: any[] = [];
    if (uniqueProjectIds.length > 0) {
      const projectWhereClause = `{${PROJECT_FIELDS.RECORD_ID}.XIN.${uniqueProjectIds.join(',')}}`;
      const projectSelectFields = [
        PROJECT_FIELDS.RECORD_ID,
        PROJECT_FIELDS.PROJECT_ID,
        PROJECT_FIELDS.CUSTOMER_NAME,
        PROJECT_FIELDS.CUSTOMER_PHONE,
        PROJECT_FIELDS.CUSTOMER_EMAIL,
        PROJECT_FIELDS.PC_OUTREACH_PREFERRED_METHOD,
        PROJECT_FIELDS.LENDER_NAME,
        PROJECT_FIELDS.CLOSER_NAME,
        PROJECT_FIELDS.CLOSER_EMAIL,
        PROJECT_FIELDS.PC_IS_UNRESPONSIVE,
        PROJECT_FIELDS.PROJECT_STATUS,
        PROJECT_FIELDS.PROJECT_STAGE
      ];

      const projectResponse = await qbClient.queryRecords({
        from: QB_TABLE_PROJECTS,
        where: projectWhereClause,
        select: projectSelectFields,
        options: { top: 1000 }
      });

      projectData = projectResponse.data || [];
    }

    // Transform to PCOutreachRecord format
    const outreachRecordsFormatted: PCOutreachRecord[] = outreachRecords.map((record: any) => {
      const projectRecord = projectData.find(p => 
        p[PROJECT_FIELDS.RECORD_ID]?.value === record[OUTREACH_RECORD_FIELDS.RELATED_PROJECT]?.value ||
        p[PROJECT_FIELDS.RECORD_ID] === record[OUTREACH_RECORD_FIELDS.RELATED_PROJECT]
      );

      const dueDate = record[OUTREACH_RECORD_FIELDS.REPORTING_DUE_DATE]?.value || record[OUTREACH_RECORD_FIELDS.REPORTING_DUE_DATE];
      const today = new Date();
      const dueDateObj = dueDate ? new Date(dueDate) : today;
      const daysOverdue = Math.max(0, Math.floor((today.getTime() - dueDateObj.getTime()) / (1000 * 60 * 60 * 24)));

      return {
        recordId: record[OUTREACH_RECORD_FIELDS.RECORD_ID]?.value || record[OUTREACH_RECORD_FIELDS.RECORD_ID],
        dateCreated: record[OUTREACH_RECORD_FIELDS.DATE_CREATED]?.value || record[OUTREACH_RECORD_FIELDS.DATE_CREATED],
        relatedProject: record[OUTREACH_RECORD_FIELDS.RELATED_PROJECT]?.value || record[OUTREACH_RECORD_FIELDS.RELATED_PROJECT],
        projectId: projectRecord?.[PROJECT_FIELDS.PROJECT_ID]?.value || projectRecord?.[PROJECT_FIELDS.PROJECT_ID] || '',
        customerName: projectRecord?.[PROJECT_FIELDS.CUSTOMER_NAME]?.value || projectRecord?.[PROJECT_FIELDS.CUSTOMER_NAME] || '',
        customerPhone: projectRecord?.[PROJECT_FIELDS.CUSTOMER_PHONE]?.value || projectRecord?.[PROJECT_FIELDS.CUSTOMER_PHONE] || '',
        outreachCompletedDate: record[OUTREACH_RECORD_FIELDS.OUTREACH_COMPLETED_DATE]?.value || record[OUTREACH_RECORD_FIELDS.OUTREACH_COMPLETED_DATE] || null,
        outreachStatus: record[OUTREACH_RECORD_FIELDS.OUTREACH_STATUS]?.value || record[OUTREACH_RECORD_FIELDS.OUTREACH_STATUS] || 'No Answer Left Message',
        numAttempts: record[OUTREACH_RECORD_FIELDS.NUM_ATTEMPTS]?.value || record[OUTREACH_RECORD_FIELDS.NUM_ATTEMPTS] || 0,
        reportingDueDate: dueDate,
        nextOutreachDueDate: record[OUTREACH_RECORD_FIELDS.NEXT_OUTREACH_DUE_DATE]?.value || record[OUTREACH_RECORD_FIELDS.NEXT_OUTREACH_DUE_DATE] || null,
        note: record[OUTREACH_RECORD_FIELDS.NOTE]?.value || record[OUTREACH_RECORD_FIELDS.NOTE] || '',
        attemptNote: record[OUTREACH_RECORD_FIELDS.ATTEMPT_NOTE]?.value || record[OUTREACH_RECORD_FIELDS.ATTEMPT_NOTE] || '',
        projectCoordinator: record[OUTREACH_RECORD_FIELDS.PROJECT_COORDINATOR]?.value || record[OUTREACH_RECORD_FIELDS.PROJECT_COORDINATOR] || pcName,
        projectStage: record[OUTREACH_RECORD_FIELDS.PROJECT_STAGE_LOOKUP]?.value || record[OUTREACH_RECORD_FIELDS.PROJECT_STAGE_LOOKUP] || '',
        projectStatus: record[OUTREACH_RECORD_FIELDS.PROJECT_STATUS_LOOKUP]?.value || record[OUTREACH_RECORD_FIELDS.PROJECT_STATUS_LOOKUP] || '',
        preferredContactMethod: projectRecord?.[PROJECT_FIELDS.PC_OUTREACH_PREFERRED_METHOD]?.value || projectRecord?.[PROJECT_FIELDS.PC_OUTREACH_PREFERRED_METHOD] || '',
        lenderName: projectRecord?.[PROJECT_FIELDS.LENDER_NAME]?.value || projectRecord?.[PROJECT_FIELDS.LENDER_NAME] || '',
        salesRepName: projectRecord?.[PROJECT_FIELDS.CLOSER_NAME]?.value || projectRecord?.[PROJECT_FIELDS.CLOSER_NAME] || '',
        salesRepEmail: projectRecord?.[PROJECT_FIELDS.CLOSER_EMAIL]?.value || projectRecord?.[PROJECT_FIELDS.CLOSER_EMAIL] || '',
        daysOverdue,
        isUnresponsive: projectRecord?.[PROJECT_FIELDS.PC_IS_UNRESPONSIVE]?.value === 'Yes' || projectRecord?.[PROJECT_FIELDS.PC_IS_UNRESPONSIVE] === 'Yes' || false
      };
    });

    // Sort by days overdue descending (most overdue first)
    outreachRecordsFormatted.sort((a, b) => b.daysOverdue - a.daysOverdue);

    logQuickbaseResponse('getPCOutreachInitial', 'btvik5kwi', 0, outreachRecordsFormatted.length);

    return outreachRecordsFormatted;

  } catch (error) {
    logQuickbaseError('getPCOutreachInitial', 'btvik5kwi', error);
    throw error;
  }
}

/**
 * Get follow-up outreach records for PC (unresponsive customers)
 * Fetches projects where Is Unresponsive = 'Yes' OR Active Unresponsive Sales Aid Records > 0
 */
export async function getPCOutreachFollowups(
  pcEmail: string,
  pcName: string,
  role: string,
  reqId: string
): Promise<PCOutreachRecord[]> {
  try {
    logQuickbaseRequest('getPCOutreachFollowups', 'bqj8x8k8n', { pcEmail, pcName, role, reqId });

    // Import role helpers
    const { hasOperationsUnrestrictedAccess, isOperationsManager } = await import('@/lib/utils/role-helpers');

    const sanitizedEmail = sanitizeQbLiteral(pcEmail);
    const sanitizedName = sanitizeQbLiteral(pcName);

    // Build WHERE clause based on role
    let pcFilterClause: string;
    if (hasOperationsUnrestrictedAccess(role)) {
      pcFilterClause = `{${PROJECT_FIELDS.RECORD_ID}.GT.0}`;
    } else if (isOperationsManager(role)) {
      pcFilterClause = `{${PROJECT_FIELDS.PROJECT_COORDINATOR}.XEX.''}`;
    } else {
      pcFilterClause = `({${PROJECT_FIELDS.PROJECT_COORDINATOR_EMAIL}.EX.'${sanitizedEmail}'})OR({${PROJECT_FIELDS.PROJECT_COORDINATOR}.EX.'${sanitizedName}'})`;
    }

    // Query Projects table for unresponsive customers
    const whereClause = `(${pcFilterClause}) AND {${PROJECT_FIELDS.PROJECT_STATUS}.EX.'Active'} AND {${PROJECT_FIELDS.PC_IS_UNRESPONSIVE}.EX.'Yes'}`;
    const selectFields = [
      PROJECT_FIELDS.RECORD_ID,
      PROJECT_FIELDS.PROJECT_ID,
      PROJECT_FIELDS.CUSTOMER_NAME,
      PROJECT_FIELDS.CUSTOMER_PHONE,
      PROJECT_FIELDS.CUSTOMER_EMAIL,
      PROJECT_FIELDS.PC_OUTREACH_PREFERRED_METHOD,
      PROJECT_FIELDS.LENDER_NAME,
      PROJECT_FIELDS.CLOSER_NAME,
      PROJECT_FIELDS.CLOSER_EMAIL,
      PROJECT_FIELDS.PC_IS_UNRESPONSIVE,
      PROJECT_FIELDS.PC_UNRESPONSIVE_COUNT,
      PROJECT_FIELDS.PC_DAYS_SINCE_CONTACT_INTAKE,
      PROJECT_FIELDS.PC_DAYS_SINCE_CONTACT_INSTALL,
      PROJECT_FIELDS.PC_DAYS_SINCE_CONTACT_NEM,
      PROJECT_FIELDS.PC_DAYS_SINCE_CONTACT_PTO,
      PROJECT_FIELDS.PC_CONTACT_ATTEMPTS_INTAKE,
      PROJECT_FIELDS.PC_CONTACT_ATTEMPTS_INSTALL,
      PROJECT_FIELDS.PC_CONTACT_ATTEMPTS_NEM,
      PROJECT_FIELDS.PC_CONTACT_ATTEMPTS_PTO,
      PROJECT_FIELDS.PROJECT_STATUS,
      PROJECT_FIELDS.PROJECT_STAGE,
      PROJECT_FIELDS.PC_MAX_OUTREACH_COMPLETED
    ];

    const response = await qbClient.queryRecords({
      from: QB_TABLE_PROJECTS,
      where: whereClause,
      select: selectFields,
      sortBy: [{ field: PROJECT_FIELDS.DATE_CREATED, order: 'DESC' }],
      options: { top: 5000 }
    });

    const projectRecords = response.data || [];

    // Transform to PCOutreachRecord format
    const followupRecords: PCOutreachRecord[] = projectRecords.map((record: any) => {
      return transformProjectToOutreachRecord(record, 'followups');
    });

    // Sort by days since last contact descending
    followupRecords.sort((a, b) => b.daysOverdue - a.daysOverdue);

    logQuickbaseResponse('getPCOutreachFollowups', 'bqj8x8k8n', 0, followupRecords.length);

    return followupRecords;

  } catch (error) {
    logQuickbaseError('getPCOutreachFollowups', 'bqj8x8k8n', error);
    throw error;
  }
}

/**
 * Get welcome call outreach records for PC
 * Fetches projects where PC Outreach: Create Check-In = true
 */
export async function getPCOutreachWelcome(
  pcEmail: string,
  pcName: string,
  role: string,
  reqId: string
): Promise<PCOutreachRecord[]> {
  try {
    logQuickbaseRequest('getPCOutreachWelcome', 'bqj8x8k8n', { pcEmail, pcName, role, reqId });

    // Import role helpers
    const { hasOperationsUnrestrictedAccess, isOperationsManager } = await import('@/lib/utils/role-helpers');

    const sanitizedEmail = sanitizeQbLiteral(pcEmail);
    const sanitizedName = sanitizeQbLiteral(pcName);

    // Build WHERE clause based on role
    let pcFilterClause: string;
    if (hasOperationsUnrestrictedAccess(role)) {
      pcFilterClause = `{${PROJECT_FIELDS.RECORD_ID}.GT.0}`;
    } else if (isOperationsManager(role)) {
      pcFilterClause = `{${PROJECT_FIELDS.PROJECT_COORDINATOR}.XEX.''}`;
    } else {
      pcFilterClause = `({${PROJECT_FIELDS.PROJECT_COORDINATOR_EMAIL}.EX.'${sanitizedEmail}'})OR({${PROJECT_FIELDS.PROJECT_COORDINATOR}.EX.'${sanitizedName}'})`;
    }

    // Query Projects table for welcome call candidates
    const whereClause = `(${pcFilterClause}) AND {${PROJECT_FIELDS.PROJECT_STATUS}.EX.'Active'} AND {${PROJECT_FIELDS.PC_OUTREACH_CREATE_CHECKIN.EX.true}`;
    const selectFields = [
      PROJECT_FIELDS.RECORD_ID,
      PROJECT_FIELDS.PROJECT_ID,
      PROJECT_FIELDS.CUSTOMER_NAME,
      PROJECT_FIELDS.CUSTOMER_PHONE,
      PROJECT_FIELDS.CUSTOMER_EMAIL,
      PROJECT_FIELDS.PC_OUTREACH_PREFERRED_METHOD,
      PROJECT_FIELDS.LENDER_NAME,
      PROJECT_FIELDS.CLOSER_NAME,
      PROJECT_FIELDS.CLOSER_EMAIL,
      PROJECT_FIELDS.PC_IS_UNRESPONSIVE,
      PROJECT_FIELDS.PROJECT_STATUS,
      PROJECT_FIELDS.PROJECT_STAGE,
      PROJECT_FIELDS.PROJECT_AGE
    ];

    const response = await qbClient.queryRecords({
      from: QB_TABLE_PROJECTS,
      where: whereClause,
      select: selectFields,
      sortBy: [{ field: PROJECT_FIELDS.DATE_CREATED, order: 'DESC' }],
      options: { top: 5000 }
    });

    const projectRecords = response.data || [];

    // Transform to PCOutreachRecord format
    const welcomeRecords: PCOutreachRecord[] = projectRecords.map((record: any) => {
      return transformProjectToOutreachRecord(record, 'welcome');
    });

    // Sort by days overdue descending (most overdue first)
    welcomeRecords.sort((a, b) => b.daysOverdue - a.daysOverdue);

    logQuickbaseResponse('getPCOutreachWelcome', 'bqj8x8k8n', 0, welcomeRecords.length);

    return welcomeRecords;

  } catch (error) {
    logQuickbaseError('getPCOutreachWelcome', 'bqj8x8k8n', error);
    throw error;
  }
}

/**
 * Get all outreach tab data for PC
 * Fetches data for all three tabs in parallel
 */
export async function getPCOutreachTabData(
  pcEmail: string,
  pcName: string,
  role: string,
  reqId: string
): Promise<PCOutreachTabData> {
  const t0 = Date.now();

  try {
    logQuickbaseRequest('getPCOutreachTabData', 'aggregate', { pcEmail, pcName, role, reqId });

    // Fetch all tab data in parallel
    const [initial, followups, welcome] = await Promise.all([
      getPCOutreachInitial(pcEmail, pcName, role, reqId),
      getPCOutreachFollowups(pcEmail, pcName, role, reqId),
      getPCOutreachWelcome(pcEmail, pcName, role, reqId)
    ]);

    const tabData: PCOutreachTabData = {
      initial,
      followups,
      welcome,
      counts: {
        initial: initial.length,
        followups: followups.length,
        welcome: welcome.length
      }
    };

    const durationMs = Date.now() - t0;
    logQuickbaseResponse('getPCOutreachTabData', 'aggregate', durationMs, initial.length + followups.length + welcome.length);

    return tabData;

  } catch (error) {
    logQuickbaseError('getPCOutreachTabData', 'aggregate', error as Error);
    throw error;
  }
}

/**
 * Bulk update outreach records
 * Performs bulk actions on multiple outreach records
 */
export async function bulkUpdateOutreachRecords(
  action: PCOutreachBulkAction,
  recordIds: number[],
  data: Record<string, any>,
  reqId: string
): Promise<PCOutreachBulkActionResult> {
  try {
    logQuickbaseRequest('bulkUpdateOutreachRecords', 'btvik5kwi', { action, recordIds, reqId });

    const errors: Array<{ recordId: number; error: string }> = [];
    let processed = 0;

    if (action === 'assign_to_rep' || action === 'schedule_checkin') {
      // For these actions, we need to map outreach record IDs to project IDs
      // First, query the Outreach Records table to get the related project IDs
      const whereClause = `{${OUTREACH_RECORD_FIELDS.RECORD_ID}.XIN.${recordIds.join(',')}}`;
      const selectFields = [OUTREACH_RECORD_FIELDS.RECORD_ID, OUTREACH_RECORD_FIELDS.RELATED_PROJECT];
      
      const outreachQuery = await qbClient.queryRecords({
        from: QB_TABLE_OUTREACH_RECORDS,
        where: whereClause,
        select: selectFields
      });

      // Map outreach IDs to project IDs and deduplicate
      const projectIdMap = new Map<number, number>();
      const missingProjects: number[] = [];
      
      outreachQuery.data.forEach((record: any) => {
        const outreachId = record[OUTREACH_RECORD_FIELDS.RECORD_ID]?.value || record[OUTREACH_RECORD_FIELDS.RECORD_ID];
        const projectId = record[OUTREACH_RECORD_FIELDS.RELATED_PROJECT]?.value || record[OUTREACH_RECORD_FIELDS.RELATED_PROJECT];
        
        if (projectId) {
          projectIdMap.set(outreachId, projectId);
        } else {
          missingProjects.push(outreachId);
        }
      });

      // Report missing projects as errors
      missingProjects.forEach(outreachId => {
        errors.push({
          recordId: outreachId,
          error: 'No related project found for outreach record'
        });
      });

      // Build project updates with deduplicated project IDs
      const uniqueProjectIds = Array.from(new Set(projectIdMap.values()));
      const projectUpdates = uniqueProjectIds.map(projectId => {
        const baseRecord = { [PROJECT_FIELDS.RECORD_ID]: { value: projectId } };
        
        if (action === 'assign_to_rep') {
          return {
            ...baseRecord,
            [PROJECT_FIELDS.CLOSER_NAME]: { value: data.repName || 'Unknown' },
            [PROJECT_FIELDS.CLOSER_EMAIL]: { value: data.repEmail || '' }
          };
        } else if (action === 'schedule_checkin') {
          return {
            ...baseRecord,
            [PROJECT_FIELDS.PC_OUTREACH_CREATE_CHECKIN]: { value: true }
          };
        }
        return baseRecord;
      });

      if (projectUpdates.length > 0) {
        const response = await qbClient.updateRecord({
          to: QB_TABLE_PROJECTS,
          data: projectUpdates
        });
        processed = projectUpdates.length;
      }
    } else {
      // For other actions, update the Outreach Records table directly
      const updateData = recordIds.map(recordId => {
        const baseRecord: any = { [OUTREACH_RECORD_FIELDS.RECORD_ID]: { value: recordId } };

        switch (action) {
          case 'mark_contacted':
            return {
              ...baseRecord,
              [OUTREACH_RECORD_FIELDS.OUTREACH_STATUS]: { value: 'Complete' },
              [OUTREACH_RECORD_FIELDS.OUTREACH_COMPLETED_DATE]: { value: new Date().toISOString() }
            };

          case 'bulk_sms':
            // Placeholder for future Twilio integration
            return {
              ...baseRecord,
              [OUTREACH_RECORD_FIELDS.ATTEMPT_NOTE]: { value: 'SMS sent (placeholder)' }
            };

          default:
            throw new Error(`Unknown bulk action: ${action}`);
        }
      });

      const response = await qbClient.updateRecord({
        to: QB_TABLE_OUTREACH_RECORDS,
        data: updateData
      });

      processed = recordIds.length;
    }

    const result: PCOutreachBulkActionResult = {
      success: errors.length === 0,
      processed,
      failed: errors.length,
      errors,
      message: `Successfully processed ${processed} records`
    };

    logQuickbaseResponse('bulkUpdateOutreachRecords', 'btvik5kwi', 0, processed);

    return result;

  } catch (error) {
    logQuickbaseError('bulkUpdateOutreachRecords', 'btvik5kwi', error);

    return {
      success: false,
      processed: 0,
      failed: recordIds.length,
      errors: [{ recordId: 0, error: error instanceof Error ? error.message : 'Unknown error' }],
      message: 'Bulk update failed'
    };
  }
}

/**
 * Helper function to transform project record to outreach record format
 */
function transformProjectToOutreachRecord(record: any, source: 'followups' | 'welcome'): PCOutreachRecord {
  const recordId = record[PROJECT_FIELDS.RECORD_ID]?.value || record[PROJECT_FIELDS.RECORD_ID];
  const projectId = record[PROJECT_FIELDS.PROJECT_ID]?.value || record[PROJECT_FIELDS.PROJECT_ID];
  const customerName = record[PROJECT_FIELDS.CUSTOMER_NAME]?.value || record[PROJECT_FIELDS.CUSTOMER_NAME];
  const customerPhone = record[PROJECT_FIELDS.CUSTOMER_PHONE]?.value || record[PROJECT_FIELDS.CUSTOMER_PHONE];
  const preferredContactMethod = record[PROJECT_FIELDS.PC_OUTREACH_PREFERRED_METHOD]?.value || record[PROJECT_FIELDS.PC_OUTREACH_PREFERRED_METHOD];
  const lenderName = record[PROJECT_FIELDS.LENDER_NAME]?.value || record[PROJECT_FIELDS.LENDER_NAME];
  const salesRepName = record[PROJECT_FIELDS.CLOSER_NAME]?.value || record[PROJECT_FIELDS.CLOSER_NAME];
  const salesRepEmail = record[PROJECT_FIELDS.CLOSER_EMAIL]?.value || record[PROJECT_FIELDS.CLOSER_EMAIL];
  const isUnresponsive = record[PROJECT_FIELDS.PC_IS_UNRESPONSIVE]?.value === 'Yes' || record[PROJECT_FIELDS.PC_IS_UNRESPONSIVE] === 'Yes';

  // Calculate days overdue based on source type
  let daysOverdue = 0;
  if (source === 'followups') {
    // Use the maximum days since contact across all phases
    const daysSinceContactIntake = record[PROJECT_FIELDS.PC_DAYS_SINCE_CONTACT_INTAKE]?.value || record[PROJECT_FIELDS.PC_DAYS_SINCE_CONTACT_INTAKE] || 0;
    const daysSinceContactInstall = record[PROJECT_FIELDS.PC_DAYS_SINCE_CONTACT_INSTALL]?.value || record[PROJECT_FIELDS.PC_DAYS_SINCE_CONTACT_INSTALL] || 0;
    const daysSinceContactNEM = record[PROJECT_FIELDS.PC_DAYS_SINCE_CONTACT_NEM]?.value || record[PROJECT_FIELDS.PC_DAYS_SINCE_CONTACT_NEM] || 0;
    const daysSinceContactPTO = record[PROJECT_FIELDS.PC_DAYS_SINCE_CONTACT_PTO]?.value || record[PROJECT_FIELDS.PC_DAYS_SINCE_CONTACT_PTO] || 0;
    
    daysOverdue = Math.max(daysSinceContactIntake, daysSinceContactInstall, daysSinceContactNEM, daysSinceContactPTO);
  } else if (source === 'welcome') {
    // Use project age as days overdue for welcome calls
    daysOverdue = record[PROJECT_FIELDS.PROJECT_AGE]?.value || record[PROJECT_FIELDS.PROJECT_AGE] || 0;
  }

  // Calculate days in current stage (use project age as approximation)
  const daysInStage = record[PROJECT_FIELDS.PROJECT_AGE]?.value || record[PROJECT_FIELDS.PROJECT_AGE] || 0;

  return {
    recordId,
    dateCreated: new Date().toISOString(), // Use current date for transformed records
    relatedProject: recordId,
    projectId: projectId || '',
    customerName: customerName || '',
    customerPhone: customerPhone || '',
    outreachCompletedDate: null,
    outreachStatus: 'No Answer Left Message' as PCOutreachStatus,
    numAttempts: 0,
    reportingDueDate: new Date().toISOString(),
    nextOutreachDueDate: null,
    note: '',
    attemptNote: '',
    projectCoordinator: '', // Will be filled by caller
    projectStage: record[PROJECT_FIELDS.PROJECT_STAGE]?.value || record[PROJECT_FIELDS.PROJECT_STAGE] || '',
    projectStatus: record[PROJECT_FIELDS.PROJECT_STATUS]?.value || record[PROJECT_FIELDS.PROJECT_STATUS] || '',
    preferredContactMethod: preferredContactMethod || '',
    lenderName: lenderName || '',
    salesRepName: salesRepName || '',
    salesRepEmail: salesRepEmail || '',
    daysOverdue,
    daysInStage,
    isUnresponsive
  };
}

// =============================================================================
// COMMUNICATION HUB QUERY FUNCTIONS
// =============================================================================

/**
 * Get PC inbound sales rep queue (Sales Aid Requests)
 */
export async function getPCInboundRepQueue(
  pcEmail: string,
  pcName: string,
  role: string,
  reqId: string
): Promise<PCSalesAidRequest[]> {
  const startTime = Date.now();

  try {
    logQuickbaseRequest('getPCInboundRepQueue', { pcEmail, pcName, role }, reqId);

    // Import role helpers
    const { hasOperationsUnrestrictedAccess, isOperationsManager } = await import('@/lib/utils/role-helpers');

    const sanitizedEmail = sanitizeQbLiteral(pcEmail);
    const sanitizedName = sanitizeQuickbaseValue(pcName);

    // Query Sales Aid Requests table
    const salesAidQuery = {
      from: QB_TABLE_SALES_AID_REQUESTS,
      select: [
        SALES_AID_FIELDS.RECORD_ID,
        SALES_AID_FIELDS.DATE_CREATED,
        SALES_AID_FIELDS.RELATED_PROJECT,
        SALES_AID_FIELDS.SALES_AID_STATUS,
        SALES_AID_FIELDS.SALES_AID_REASON,
        SALES_AID_FIELDS.ESCALATE_TO_SALES_AID,
        SALES_AID_FIELDS.ESCALATED_DATETIME,
        SALES_AID_FIELDS.ASSIGNED_ESCALATION_REP,
        SALES_AID_FIELDS.REP_72_HOUR_DEADLINE,
        SALES_AID_FIELDS.COMPLETED_DATE
      ],
      where: `{${SALES_AID_FIELDS.SALES_AID_STATUS}.EX.'Waiting for Rep'} OR {${SALES_AID_FIELDS.SALES_AID_STATUS}.EX.'Working With Rep'}`,
      sortBy: [{ field: SALES_AID_FIELDS.DATE_CREATED, order: 'DESC' }],
      options: { top: 5000 }
    };

    const salesAidResponse = await qbClient.queryRecords(salesAidQuery);

    if (!salesAidResponse.data || salesAidResponse.data.length === 0) {
      logQuickbaseResponse('getPCInboundRepQueue', { count: 0 }, reqId, Date.now() - startTime);
      return [];
    }

    // Get project IDs for filtering
    const projectIds = salesAidResponse.data
      .map((record: any) => extractNumericValue(record[SALES_AID_FIELDS.RELATED_PROJECT]))
      .filter((id: any) => id && id > 0);

    if (projectIds.length === 0) {
      logQuickbaseResponse('getPCInboundRepQueue', { count: 0 }, reqId, Date.now() - startTime);
      return [];
    }

    // Build role-based filter
    let pcFilterClause: string;
    if (hasOperationsUnrestrictedAccess(role)) {
      pcFilterClause = `{${PROJECT_FIELDS.RECORD_ID}.GT.0}`;
    } else if (isOperationsManager(role)) {
      pcFilterClause = `{${PROJECT_FIELDS.PROJECT_COORDINATOR}.XEX.''}`;
    } else {
      pcFilterClause = `({${PROJECT_FIELDS.PROJECT_COORDINATOR_EMAIL}.EX.'${sanitizedEmail}'})OR({${PROJECT_FIELDS.PROJECT_COORDINATOR}.EX.'${sanitizedName}'})`;
    }

    // Query Projects table to get PC's projects and customer data
    const projectsQuery = {
      from: QB_TABLE_PROJECTS,
      select: [
        PROJECT_FIELDS.RECORD_ID,
        PROJECT_FIELDS.PROJECT_ID,
        PROJECT_FIELDS.CUSTOMER_NAME,
        PROJECT_FIELDS.CLOSER_NAME,
        PROJECT_FIELDS.CLOSER_EMAIL,
        PROJECT_FIELDS.PROJECT_COORDINATOR
      ],
      where: `{${PROJECT_FIELDS.RECORD_ID}.IN.${projectIds.join(',')}} AND (${pcFilterClause})`
    };

    const projectsResponse = await qbClient.queryRecords(projectsQuery);
    
    if (!projectsResponse.data || projectsResponse.data.length === 0) {
      logQuickbaseResponse('getPCInboundRepQueue', { count: 0 }, reqId, Date.now() - startTime);
      return [];
    }

    // Create project lookup map
    const projectMap = new Map();
    projectsResponse.data.forEach((project: any) => {
      projectMap.set(project[PROJECT_FIELDS.RECORD_ID], {
        projectId: project[PROJECT_FIELDS.PROJECT_ID]?.value || project[PROJECT_FIELDS.PROJECT_ID] || '',
        customerName: project[PROJECT_FIELDS.CUSTOMER_NAME]?.value || project[PROJECT_FIELDS.CUSTOMER_NAME] || '',
        salesRepName: project[PROJECT_FIELDS.CLOSER_NAME]?.value || project[PROJECT_FIELDS.CLOSER_NAME] || '',
        salesRepEmail: project[PROJECT_FIELDS.CLOSER_EMAIL]?.value || project[PROJECT_FIELDS.CLOSER_EMAIL] || ''
      });
    });

    // Transform sales aid requests
    const requests: PCSalesAidRequest[] = salesAidResponse.data
      .map((record: any) => {
        const projectData = projectMap.get(record[SALES_AID_FIELDS.RELATED_PROJECT]);
        if (!projectData) return null;

        const dateCreated = record[SALES_AID_FIELDS.DATE_CREATED];
        const deadline = record[SALES_AID_FIELDS.REP_72_HOUR_DEADLINE];
        
        // Calculate urgency and time waiting
        const urgency = calculateSalesAidUrgency(deadline);
        const timeWaiting = Math.floor((Date.now() - new Date(dateCreated).getTime()) / (1000 * 60 * 60 * 24));

        // Extract wrapped values from Sales Aid fields
        const salesAidReason = record[SALES_AID_FIELDS.SALES_AID_REASON]?.value || record[SALES_AID_FIELDS.SALES_AID_REASON] || '';
        const messagePreview = String(salesAidReason).substring(0, 100);

        return {
          recordId: record[SALES_AID_FIELDS.RECORD_ID],
          dateCreated,
          relatedProject: record[SALES_AID_FIELDS.RELATED_PROJECT],
          projectId: projectData.projectId,
          customerName: projectData.customerName,
          salesRepName: projectData.salesRepName,
          salesRepEmail: projectData.salesRepEmail,
          salesAidStatus: record[SALES_AID_FIELDS.SALES_AID_STATUS]?.value || record[SALES_AID_FIELDS.SALES_AID_STATUS] || '',
          salesAidReason,
          escalateToSalesAid: record[SALES_AID_FIELDS.ESCALATE_TO_SALES_AID] || false,
          escalatedDateTime: record[SALES_AID_FIELDS.ESCALATED_DATETIME],
          assignedEscalationRep: record[SALES_AID_FIELDS.ASSIGNED_ESCALATION_REP]?.value || record[SALES_AID_FIELDS.ASSIGNED_ESCALATION_REP] || '',
          rep72HourDeadline: deadline,
          completedDate: record[SALES_AID_FIELDS.COMPLETED_DATE],
          urgency,
          timeWaiting,
          messagePreview
        };
      })
      .filter((request: any) => request !== null)
      .sort((a: PCSalesAidRequest, b: PCSalesAidRequest) => {
        // Sort by urgency (critical > high > normal), then by time waiting
        const urgencyOrder = { critical: 0, high: 1, normal: 2 };
        const urgencyDiff = urgencyOrder[a.urgency] - urgencyOrder[b.urgency];
        if (urgencyDiff !== 0) return urgencyDiff;
        return b.timeWaiting - a.timeWaiting;
      });

    logQuickbaseResponse('getPCInboundRepQueue', { count: requests.length }, reqId, Date.now() - startTime);
    return requests;

  } catch (error) {
    logQuickbaseError('getPCInboundRepQueue', error, reqId);
    throw error;
  }
}

/**
 * Get PC conversation history (Install Communications)
 */
export async function getPCConversationHistory(
  pcEmail: string,
  pcName: string,
  role: string,
  filters: PCConversationFilters,
  reqId: string
): Promise<PCConversationItem[]> {
  const startTime = Date.now();

  try {
    logQuickbaseRequest('getPCConversationHistory', { pcEmail, pcName, role, filters }, reqId);

    // Import role helpers
    const { hasOperationsUnrestrictedAccess, isOperationsManager } = await import('@/lib/utils/role-helpers');

    const sanitizedEmail = sanitizeQbLiteral(pcEmail);
    const sanitizedName = sanitizeQuickbaseValue(pcName);

    // Build date range filter
    let dateFilter = '';
    const now = new Date();
    switch (filters.dateRange) {
      case '7days':
        const sevenDaysAgo = new Date(now.getTime() - 7 * 24 * 60 * 60 * 1000);
        dateFilter = `{${INSTALL_COMMUNICATION_FIELDS.DATE}.GTE.'${sevenDaysAgo.toISOString()}'}`;
        break;
      case '30days':
        const thirtyDaysAgo = new Date(now.getTime() - 30 * 24 * 60 * 60 * 1000);
        dateFilter = `{${INSTALL_COMMUNICATION_FIELDS.DATE}.GTE.'${thirtyDaysAgo.toISOString()}'}`;
        break;
      case '90days':
        const ninetyDaysAgo = new Date(now.getTime() - 90 * 24 * 60 * 60 * 1000);
        dateFilter = `{${INSTALL_COMMUNICATION_FIELDS.DATE}.GTE.'${ninetyDaysAgo.toISOString()}'}`;
        break;
    }

    // Build tab filter
    let tabFilter = '';
    switch (filters.tab) {
      case 'needs_response':
        tabFilter = `{${INSTALL_COMMUNICATION_FIELDS.NEM_BLOCKER_OUTREACH}.EX.'Yes'}`;
        break;
      case 'scheduled':
        tabFilter = `{${INSTALL_COMMUNICATION_FIELDS.DATE}.GT.'${now.toISOString()}'}`;
        break;
    }

    // Build search filter
    let searchFilter = '';
    if (filters.search) {
      const searchTerm = sanitizeQuickbaseValue(filters.search);
      searchFilter = `{${INSTALL_COMMUNICATION_FIELDS.COMMUNICATION_NOTE}.CT.'${searchTerm}'}`;
    }

    // Build project filter based on role - optimize to skip project fetch for unrestricted roles
    let projectFilter = '';
    if (!hasOperationsUnrestrictedAccess(role)) {
      // For restricted roles, we need to fetch accessible projects first
      let projectsWhere: string;
      if (isOperationsManager(role)) {
        projectsWhere = `{${PROJECT_FIELDS.PROJECT_COORDINATOR}.XEX.''}`;
      } else {
        projectsWhere = `({${PROJECT_FIELDS.PROJECT_COORDINATOR_EMAIL}.EX.'${sanitizedEmail}'})OR({${PROJECT_FIELDS.PROJECT_COORDINATOR}.EX.'${sanitizedName}'})`;
      }

      const projectsQuery = {
        from: QB_TABLE_PROJECTS,
        where: projectsWhere,
        select: [PROJECT_FIELDS.RECORD_ID]
      };

      const projectsResponse = await qbClient.queryRecords(projectsQuery);
      const projectIds = (projectsResponse.data || []).map((p: any) => extractNumericValue(p[PROJECT_FIELDS.RECORD_ID]));

      if (projectIds.length === 0) {
        return [];
      }

      const projectIdsStr = projectIds.join(',');
      projectFilter = `{${INSTALL_COMMUNICATION_FIELDS.RELATED_PROJECT}.XIN.${projectIdsStr}}`;
    }
    // else: For unrestricted roles, skip project filter entirely

    // Combine filters
    const whereConditions = [
      projectFilter,
      dateFilter,
      tabFilter,
      searchFilter
    ].filter(Boolean);

    // Query Install Communications with Projects join
    const communicationsQuery = {
      from: QB_TABLE_INSTALL_COMMUNICATIONS,
      select: [
        INSTALL_COMMUNICATION_FIELDS.RECORD_ID,
        INSTALL_COMMUNICATION_FIELDS.DATE,
        INSTALL_COMMUNICATION_FIELDS.NOTE_BY,
        INSTALL_COMMUNICATION_FIELDS.RELATED_PROJECT,
        INSTALL_COMMUNICATION_FIELDS.COMMUNICATION_NOTE,
        INSTALL_COMMUNICATION_FIELDS.NEM_BLOCKER_OUTREACH
      ],
      where: whereConditions.join(' AND '),
      sortBy: [{ field: INSTALL_COMMUNICATION_FIELDS.DATE, order: 'DESC' }],
      options: { top: 5000 }
    };

    const communicationsResponse = await qbClient.queryRecords(communicationsQuery);
    
    if (!communicationsResponse.data || communicationsResponse.data.length === 0) {
      logQuickbaseResponse('getPCConversationHistory', { count: 0 }, reqId, Date.now() - startTime);
      return [];
    }

    // Get project data for customer names
    const communicationProjectIds = communicationsResponse.data
      .map((record: any) => extractNumericValue(record[INSTALL_COMMUNICATION_FIELDS.RELATED_PROJECT]))
      .filter((id: any) => id && id > 0);

    const communicationProjectsQuery = {
      from: QB_TABLE_PROJECTS,
      select: [
        PROJECT_FIELDS.RECORD_ID,
        PROJECT_FIELDS.PROJECT_ID,
        PROJECT_FIELDS.CUSTOMER_NAME,
        PROJECT_FIELDS.CUSTOMER_PHONE,
        PROJECT_FIELDS.PROJECT_STAGE
      ],
      where: `{${PROJECT_FIELDS.RECORD_ID}.IN.${communicationProjectIds.join(',')}}`
    };

    const communicationProjectsResponse = await qbClient.queryRecords(communicationProjectsQuery);
    const projectMap = new Map();
    communicationProjectsResponse.data?.forEach((project: any) => {
      projectMap.set(project[PROJECT_FIELDS.RECORD_ID], {
        projectId: project[PROJECT_FIELDS.PROJECT_ID]?.value || project[PROJECT_FIELDS.PROJECT_ID] || '',
        customerName: project[PROJECT_FIELDS.CUSTOMER_NAME]?.value || project[PROJECT_FIELDS.CUSTOMER_NAME] || '',
        customerPhone: project[PROJECT_FIELDS.CUSTOMER_PHONE]?.value || project[PROJECT_FIELDS.CUSTOMER_PHONE] || '',
        projectStage: project[PROJECT_FIELDS.PROJECT_STAGE]?.value || project[PROJECT_FIELDS.PROJECT_STAGE] || ''
      });
    });

    // Transform communications
    const conversations: PCConversationItem[] = communicationsResponse.data
      .map((record: any) => {
        const projectData = projectMap.get(record[INSTALL_COMMUNICATION_FIELDS.RELATED_PROJECT]);
        if (!projectData) return null;

        // Extract wrapped values from communication fields
        const content = record[INSTALL_COMMUNICATION_FIELDS.COMMUNICATION_NOTE]?.value || record[INSTALL_COMMUNICATION_FIELDS.COMMUNICATION_NOTE] || '';
        const communicationType = detectCommunicationType(content);
        const twilioSid = extractTwilioSidFromNote(content);
        const isNemBlocker = record[INSTALL_COMMUNICATION_FIELDS.NEM_BLOCKER_OUTREACH] === 'Yes';

        return {
          recordId: record[INSTALL_COMMUNICATION_FIELDS.RECORD_ID],
          date: record[INSTALL_COMMUNICATION_FIELDS.DATE],
          noteBy: record[INSTALL_COMMUNICATION_FIELDS.NOTE_BY]?.value || record[INSTALL_COMMUNICATION_FIELDS.NOTE_BY] || '',
          projectId: projectData.projectId,
          customerName: projectData.customerName,
          customerPhone: projectData.customerPhone,
          communicationType,
          direction: 'outbound', // Most communications are outbound
          content,
          twilioSid,
          duration: null, // Will be populated from Twilio webhooks
          status: null, // Will be populated from Twilio webhooks
          isNemBlocker,
          needsResponse: isNemBlocker,
          projectStage: projectData.projectStage
        };
      })
      .filter((conversation: any) => conversation !== null);

    // Apply server-side filtering
    let filteredConversations = conversations;

    // Filter by communicationType
    if (filters.communicationType && filters.communicationType !== 'all') {
      filteredConversations = filteredConversations.filter(
        conversation => conversation.communicationType === filters.communicationType
      );
    }

    // Filter by projectStage
    if (filters.projectStage && filters.projectStage !== 'all') {
      filteredConversations = filteredConversations.filter(
        conversation => conversation.projectStage === filters.projectStage
      );
    }

    logQuickbaseResponse('getPCConversationHistory', { count: filteredConversations.length }, reqId, Date.now() - startTime);
    return filteredConversations;

  } catch (error) {
    logQuickbaseError('getPCConversationHistory', error, reqId);
    throw error;
  }
}

/**
 * Get PC bulk messaging recipients
 */
export async function getPCBulkMessagingRecipients(
  pcEmail: string,
  pcName: string,
  role: string,
  filters: { projectStage?: string; lender?: string; salesRep?: string },
  reqId: string
): Promise<PCBulkMessagingRecipient[]> {
  const startTime = Date.now();

  try {
    logQuickbaseRequest('getPCBulkMessagingRecipients', { pcEmail, pcName, role, filters }, reqId);

    // Import role helpers
    const { hasOperationsUnrestrictedAccess, isOperationsManager } = await import('@/lib/utils/role-helpers');

    const sanitizedEmail = sanitizeQbLiteral(pcEmail);
    const sanitizedName = sanitizeQuickbaseValue(pcName);

    // Build role-based filter
    let pcFilterClause: string;
    if (hasOperationsUnrestrictedAccess(role)) {
      pcFilterClause = `{${PROJECT_FIELDS.RECORD_ID}.GT.0}`;
    } else if (isOperationsManager(role)) {
      pcFilterClause = `{${PROJECT_FIELDS.PROJECT_COORDINATOR}.XEX.''}`;
    } else {
      pcFilterClause = `({${PROJECT_FIELDS.PROJECT_COORDINATOR_EMAIL}.EX.'${sanitizedEmail}'})OR({${PROJECT_FIELDS.PROJECT_COORDINATOR}.EX.'${sanitizedName}'})`;
    }

    // Build filter conditions
    const whereConditions = [
      `(${pcFilterClause})`,
      `{${PROJECT_FIELDS.PROJECT_STATUS}.EX.'Active'}`
    ];

    if (filters.projectStage && filters.projectStage !== 'all') {
      whereConditions.push(`{${PROJECT_FIELDS.PROJECT_STAGE}.EX.'${sanitizeQuickbaseValue(filters.projectStage)}'}`);
    }
    if (filters.lender && filters.lender !== 'all') {
      whereConditions.push(`{${PROJECT_FIELDS.LENDER_NAME}.EX.'${sanitizeQuickbaseValue(filters.lender)}'}`);
    }
    if (filters.salesRep && filters.salesRep !== 'all') {
      whereConditions.push(`{${PROJECT_FIELDS.CLOSER_NAME}.EX.'${sanitizeQuickbaseValue(filters.salesRep)}'}`);
    }

    // Query Projects table
    const projectsQuery = {
      from: QB_TABLE_PROJECTS,
      select: [
        PROJECT_FIELDS.RECORD_ID,
        PROJECT_FIELDS.PROJECT_ID,
        PROJECT_FIELDS.CUSTOMER_NAME,
        PROJECT_FIELDS.CUSTOMER_PHONE,
        PROJECT_FIELDS.PROJECT_STAGE,
        PROJECT_FIELDS.LENDER_NAME,
        PROJECT_FIELDS.CLOSER_NAME,
        PROJECT_FIELDS.PROJECT_COORDINATOR_EMAIL
      ],
      where: whereConditions.join(' AND '),
      sortBy: [{ field: PROJECT_FIELDS.DATE_CREATED, order: 'DESC' }],
      options: { top: 5000 }
    };

    const projectsResponse = await qbClient.queryRecords(projectsQuery);
    
    if (!projectsResponse.data || projectsResponse.data.length === 0) {
      logQuickbaseResponse('getPCBulkMessagingRecipients', { count: 0 }, reqId, Date.now() - startTime);
      return [];
    }

    // Transform to recipients, filtering out invalid phone numbers
    const recipients: PCBulkMessagingRecipient[] = projectsResponse.data
      .map((record: any) => {
        const phone = record[PROJECT_FIELDS.CUSTOMER_PHONE];
        if (!phone || phone.length < 10) return null; // Filter out invalid phones

        return {
          projectId: record[PROJECT_FIELDS.PROJECT_ID],
          recordId: record[PROJECT_FIELDS.RECORD_ID],
          customerName: record[PROJECT_FIELDS.CUSTOMER_NAME],
          customerPhone: phone,
          projectStage: record[PROJECT_FIELDS.PROJECT_STAGE],
          lenderName: record[PROJECT_FIELDS.LENDER_NAME],
          salesRepName: record[PROJECT_FIELDS.CLOSER_NAME],
          coordinatorEmail: record[PROJECT_FIELDS.PROJECT_COORDINATOR_EMAIL]
        };
      })
      .filter((recipient: any) => recipient !== null);

    logQuickbaseResponse('getPCBulkMessagingRecipients', { count: recipients.length }, reqId, Date.now() - startTime);
    return recipients;

  } catch (error) {
    logQuickbaseError('getPCBulkMessagingRecipients', error, reqId);
    throw error;
  }
}

/**
 * Update Sales Aid Request
 */
export async function updateSalesAidRequest(
  recordId: number,
  updates: {
    status?: PCSalesAidStatus;
    escalateToSalesAid?: boolean;
    assignedRep?: string;
    completedDate?: string;
    escalatedDateTime?: string;
  },
  reqId: string
): Promise<boolean> {
  const startTime = Date.now();
  
  try {
    logQuickbaseRequest('updateSalesAidRequest', { recordId, updates }, reqId);

    const updatePayload: any = {};

    if (updates.status) {
      updatePayload[SALES_AID_FIELDS.SALES_AID_STATUS] = updates.status;
    }
    if (updates.escalateToSalesAid !== undefined) {
      updatePayload[SALES_AID_FIELDS.ESCALATE_TO_SALES_AID] = updates.escalateToSalesAid;
    }
    if (updates.assignedRep) {
      updatePayload[SALES_AID_FIELDS.ASSIGNED_ESCALATION_REP] = updates.assignedRep;
    }
    if (updates.completedDate) {
      updatePayload[SALES_AID_FIELDS.COMPLETED_DATE] = updates.completedDate;
    }
    if (updates.escalatedDateTime) {
      updatePayload[SALES_AID_FIELDS.ESCALATED_DATETIME] = updates.escalatedDateTime;
    }

    // If status changes to 'Resolved by Rep', set completed date
    if (updates.status === 'Resolved by Rep' && !updates.completedDate) {
      updatePayload[SALES_AID_FIELDS.COMPLETED_DATE] = new Date().toISOString();
    }

    const response = await qbClient.updateRecord(QB_TABLE_SALES_AID_REQUESTS, recordId, updatePayload);
    
    logQuickbaseResponse('updateSalesAidRequest', { success: true }, reqId, Date.now() - startTime);
    return true;

  } catch (error) {
    logQuickbaseError('updateSalesAidRequest', error, reqId);
    throw error;
  }
}

// =============================================================================
// HELPER FUNCTIONS
// =============================================================================

/**
 * Calculate Sales Aid urgency based on deadline
 * Re-exported from escalation-helpers for backward compatibility
 */
export { calculateSalesAidUrgency } from '@/lib/utils/escalation-helpers';

/**
 * Extract Twilio SID from note content
 */
function extractTwilioSidFromNote(note: string): string | null {
  const sidMatch = note.match(/SID: ([A-Z]{2}[a-z0-9]{32})/);
  return sidMatch ? sidMatch[1] : null;
}

/**
 * Detect communication type from note content
 */
function detectCommunicationType(note: string): 'sms' | 'call' | 'email' | 'note' {
  const lowerNote = note.toLowerCase();
  
  if (lowerNote.includes('sms sent') || lowerNote.includes('text message')) return 'sms';
  if (lowerNote.includes('call initiated') || lowerNote.includes('phone call')) return 'call';
  if (lowerNote.includes('email sent') || lowerNote.includes('email sent')) return 'email';
  
  return 'note';
}

// =============================================================================
// PROJECT DETAIL MODAL QUERIES
// =============================================================================

/**
 * Get PC project communications from Install Communications table
 */
export async function getPCProjectCommunications(
  projectId: string,
  recordId: number,
  reqId: string
): Promise<PCConversationItem[]> {
  const startTime = Date.now();
  
  try {
    logQuickbaseRequest('getPCProjectCommunications', { projectId, recordId }, reqId);

    const whereClause = `{${INSTALL_COMMUNICATION_FIELDS.RELATED_PROJECT}.EX.'${projectId}'}OR{${INSTALL_COMMUNICATION_FIELDS.RELATED_PROJECT}.EX.${recordId}}`;

    const selectFields = [
      INSTALL_COMMUNICATION_FIELDS.RECORD_ID,
      INSTALL_COMMUNICATION_FIELDS.DATE,
      INSTALL_COMMUNICATION_FIELDS.NOTE_BY,
      INSTALL_COMMUNICATION_FIELDS.RELATED_PROJECT,
      INSTALL_COMMUNICATION_FIELDS.COMMUNICATION_NOTE,
      INSTALL_COMMUNICATION_FIELDS.NEM_BLOCKER_OUTREACH
    ];

    const response = await qbClient.queryRecords({
      from: QB_TABLE_INSTALL_COMMUNICATIONS,
      where: whereClause,
      select: selectFields,
      sortBy: [{ fieldId: INSTALL_COMMUNICATION_FIELDS.DATE, order: 'DESC' }]
    });

    const communications: PCConversationItem[] = response.data.map((record: any) => ({
      recordId: record[INSTALL_COMMUNICATION_FIELDS.RECORD_ID],
      date: record[INSTALL_COMMUNICATION_FIELDS.DATE],
      noteBy: record[INSTALL_COMMUNICATION_FIELDS.NOTE_BY],
      projectId: projectId,
      customerName: '', // Will be populated from project data
      customerPhone: '', // Will be populated from project data
      communicationType: detectCommunicationType(record[INSTALL_COMMUNICATION_FIELDS.COMMUNICATION_NOTE] || ''),
      direction: 'outbound', // Default for PC communications
      content: record[INSTALL_COMMUNICATION_FIELDS.COMMUNICATION_NOTE] || '',
      twilioSid: extractTwilioSidFromNote(record[INSTALL_COMMUNICATION_FIELDS.COMMUNICATION_NOTE] || ''),
      duration: null,
      status: null,
      isNemBlocker: isTrueQB(record[INSTALL_COMMUNICATION_FIELDS.NEM_BLOCKER_OUTREACH]),
      needsResponse: false,
      projectStage: '' // Will be populated from project data
    }));

    logQuickbaseResponse('getPCProjectCommunications', { count: communications.length }, reqId, Date.now() - startTime);
    return communications;

  } catch (error) {
    logQuickbaseError('getPCProjectCommunications', error, reqId);
    throw error;
  }
}

/**
 * Get PC project documents (placeholder implementation)
 * TODO: Identify document fields in Projects table or separate Documents table
 */
export async function getPCProjectDocuments(
  projectId: string,
  recordId: number,
  reqId: string
): Promise<PCProjectDocument[]> {
  const startTime = Date.now();
  
  try {
    logQuickbaseRequest('getPCProjectDocuments', { projectId, recordId }, reqId);

    // TODO: Implement document query once document fields are identified
    // For now, return empty array
    const documents: PCProjectDocument[] = [];

    logQuickbaseResponse('getPCProjectDocuments', { count: documents.length }, reqId, Date.now() - startTime);
    return documents;

  } catch (error) {
    logQuickbaseError('getPCProjectDocuments', error, reqId);
    throw error;
  }
}

/**
 * Create a project note in Install Communications table
 */
export async function createProjectNote(
  projectId: string,
  recordId: number,
  noteContent: string,
  authorEmail: string,
  reqId: string
): Promise<boolean> {
  const startTime = Date.now();
  
  try {
    logQuickbaseRequest('createProjectNote', { projectId, recordId, noteContent, authorEmail }, reqId);

    const noteData = {
      [INSTALL_COMMUNICATION_FIELDS.DATE]: new Date().toISOString(),
      [INSTALL_COMMUNICATION_FIELDS.NOTE_BY]: authorEmail,
      [INSTALL_COMMUNICATION_FIELDS.RELATED_PROJECT]: recordId,
      [INSTALL_COMMUNICATION_FIELDS.COMMUNICATION_NOTE]: noteContent,
      [INSTALL_COMMUNICATION_FIELDS.NEM_BLOCKER_OUTREACH]: false
    };

    const response = await qbClient.createRecord(QB_TABLE_INSTALL_COMMUNICATIONS, noteData);
    
    logQuickbaseResponse('createProjectNote', { success: true, recordId: response.data }, reqId, Date.now() - startTime);
    return true;

  } catch (error) {
    logQuickbaseError('createProjectNote', error, reqId);
    throw error;
  }
}

/**
 * Transform project data to team members array
 */
export function transformProjectToTeamMembers(project: any): PCTeamMember[] {
  const teamMembers: PCTeamMember[] = [];

  // Extract closer information
  if (project[PROJECT_FIELDS.CLOSER_NAME] && project[PROJECT_FIELDS.CLOSER_EMAIL]) {
    teamMembers.push({
      role: 'closer',
      name: project[PROJECT_FIELDS.CLOSER_NAME],
      email: project[PROJECT_FIELDS.CLOSER_EMAIL],
      phone: project[PROJECT_FIELDS.CLOSER_PHONE] || '',
      userId: project[PROJECT_FIELDS.CLOSER_ID] || ''
    });
  }

  // Extract setter information
  if (project[PROJECT_FIELDS.SETTER_NAME] && project[PROJECT_FIELDS.SETTER_EMAIL]) {
    teamMembers.push({
      role: 'setter',
      name: project[PROJECT_FIELDS.SETTER_NAME],
      email: project[PROJECT_FIELDS.SETTER_EMAIL],
      phone: project[PROJECT_FIELDS.SETTER_PHONE] || '',
      userId: project[PROJECT_FIELDS.SETTER_ID] || ''
    });
  }

  // Extract project coordinator information
  if (project[PROJECT_FIELDS.PROJECT_COORDINATOR_NAME] && project[PROJECT_FIELDS.PROJECT_COORDINATOR_EMAIL]) {
    teamMembers.push({
      role: 'coordinator',
      name: project[PROJECT_FIELDS.PROJECT_COORDINATOR_NAME],
      email: project[PROJECT_FIELDS.PROJECT_COORDINATOR_EMAIL],
      phone: project[PROJECT_FIELDS.PROJECT_COORDINATOR_PHONE] || '',
      userId: project[PROJECT_FIELDS.PROJECT_COORDINATOR_ID] || ''
    });
  }

  // Filter out members without names
  return teamMembers.filter(member => member.name && member.name.trim() !== '');
}

// =============================================================================
// PC ↔ REP COLLABORATION QUERIES
// =============================================================================

/**
 * Get PC tasks for a specific project
 */
export async function getPCTasksForProject(
  projectId: string,
  recordId: number,
  reqId: string
): Promise<PCTask[]> {
  const startTime = Date.now();
  
  try {
    logQuickbaseRequest('getPCTasksForProject', { projectId, recordId }, reqId);

    // First, get task groups for this project
    const taskGroupsResponse = await qbClient.queryRecords({
      from: QB_TABLE_TASK_GROUPS,
      where: `{${TASK_GROUP_FIELDS.RELATED_PROJECT}.EX.${recordId}}`,
      select: [TASK_GROUP_FIELDS.RECORD_ID]
    });

    if (taskGroupsResponse.data.length === 0) {
      logQuickbaseResponse('getPCTasksForProject', { count: 0 }, reqId, Date.now() - startTime);
      return [];
    }

    const taskGroupIds = taskGroupsResponse.data.map((group: any) => group[TASK_GROUP_FIELDS.RECORD_ID]);

    // Query tasks in these groups that are PC-assigned (TASK_CATEGORY starts with 'PC:')
    const tasksResponse = await qbClient.queryRecords({
      from: QB_TABLE_TASKS,
      where: `{${TASK_FIELDS.TASK_GROUP}.IN.${taskGroupIds.join(',')}}AND{${TASK_FIELDS.TASK_CATEGORY}.SW.'PC:'}`,
      select: [
        TASK_FIELDS.RECORD_ID,
        TASK_FIELDS.DATE_CREATED,
        TASK_FIELDS.DATE_MODIFIED,
        TASK_FIELDS.TASK_GROUP,
        TASK_FIELDS.STATUS,
        TASK_FIELDS.NAME,
        TASK_FIELDS.DESCRIPTION,
        TASK_FIELDS.TASK_CATEGORY,
        TASK_FIELDS.ASSIGNED_TO,
        TASK_FIELDS.ASSIGNED_BY
      ]
    });

    // Collect all task IDs for batch submission query
    const taskIds = tasksResponse.data.map((task: any) => task[TASK_FIELDS.RECORD_ID]);
    
    // Batch query all submissions for all tasks
    let allSubmissions: any[] = [];
    if (taskIds.length > 0) {
      const submissionsResponse = await qbClient.queryRecords({
        from: QB_TABLE_TASK_SUBMISSIONS,
        where: `{${TASK_SUBMISSION_FIELDS.RELATED_TASK}.IN.${taskIds.join(',')}}`,
        select: [
          TASK_SUBMISSION_FIELDS.RECORD_ID,
          TASK_SUBMISSION_FIELDS.DATE_CREATED,
          TASK_SUBMISSION_FIELDS.RELATED_TASK,
          TASK_SUBMISSION_FIELDS.SUBMISSION_STATUS,
          TASK_SUBMISSION_FIELDS.SUBMISSION_NOTE,
          TASK_SUBMISSION_FIELDS.FILE_ATTACHMENT_1,
          TASK_SUBMISSION_FIELDS.FILE_ATTACHMENT_2,
          TASK_SUBMISSION_FIELDS.FILE_ATTACHMENT_3,
          TASK_SUBMISSION_FIELDS.SUBMITTED_BY
        ]
      });
      allSubmissions = submissionsResponse.data;
    }

    // Group submissions by task ID
    const submissionsByTask: Record<number, any[]> = {};
    allSubmissions.forEach((sub: any) => {
      const taskId = sub[TASK_SUBMISSION_FIELDS.RELATED_TASK];
      if (!submissionsByTask[taskId]) {
        submissionsByTask[taskId] = [];
      }
      submissionsByTask[taskId].push(sub);
    });

    const tasks: PCTask[] = [];

    // Process each task
    for (const taskRecord of tasksResponse.data) {
      const taskType = parsePCTaskType(taskRecord[TASK_FIELDS.TASK_CATEGORY]);
      if (!taskType) continue; // Skip if not a valid PC task type

      // Get submissions for this task from the grouped data
      const taskSubmissions = submissionsByTask[taskRecord[TASK_FIELDS.RECORD_ID]] || [];
      const submissions: PCTaskSubmission[] = taskSubmissions.map((sub: any) => ({
        recordId: sub[TASK_SUBMISSION_FIELDS.RECORD_ID],
        dateCreated: sub[TASK_SUBMISSION_FIELDS.DATE_CREATED],
        relatedTask: sub[TASK_SUBMISSION_FIELDS.RELATED_TASK],
        submissionStatus: sub[TASK_SUBMISSION_FIELDS.SUBMISSION_STATUS] || '',
        submissionNote: sub[TASK_SUBMISSION_FIELDS.SUBMISSION_NOTE] || '',
        fileAttachments: [
          sub[TASK_SUBMISSION_FIELDS.FILE_ATTACHMENT_1],
          sub[TASK_SUBMISSION_FIELDS.FILE_ATTACHMENT_2],
          sub[TASK_SUBMISSION_FIELDS.FILE_ATTACHMENT_3]
        ].filter(Boolean),
        submittedBy: sub[TASK_SUBMISSION_FIELDS.SUBMITTED_BY] || ''
      }));

      // Use direct fields instead of parsing description
      const description = taskRecord[TASK_FIELDS.DESCRIPTION] || '';
      const assignedTo = taskRecord[TASK_FIELDS.ASSIGNED_TO] || '';
      const assignedBy = taskRecord[TASK_FIELDS.ASSIGNED_BY] || '';
      
      // Parse other details from description (dueDate, priority) as these aren't in separate fields yet
      const dueDateMatch = description.match(/Due date: ([^\n]+)/);
      const priorityMatch = description.match(/Priority: ([^\n]+)/);

      tasks.push({
        recordId: taskRecord[TASK_FIELDS.RECORD_ID],
        dateCreated: taskRecord[TASK_FIELDS.DATE_CREATED],
        dateModified: taskRecord[TASK_FIELDS.DATE_MODIFIED],
        taskGroup: taskRecord[TASK_FIELDS.TASK_GROUP],
        status: taskRecord[TASK_FIELDS.STATUS] as 'Not Started' | 'In Progress' | 'Completed' | 'Blocked',
        name: taskRecord[TASK_FIELDS.NAME],
        description: description,
        taskType: taskType,
        assignedTo: assignedTo,
        assignedBy: assignedBy,
        projectId: projectId,
        customerName: '', // Will be populated from project data
        dueDate: dueDateMatch ? dueDateMatch[1] : null,
        priority: (priorityMatch ? priorityMatch[1] : 'normal') as 'high' | 'normal' | 'low',
        submissions: submissions
      });
    }

    // Sort by priority (high > normal > low), then by dateCreated descending
    tasks.sort((a, b) => {
      const priorityOrder = { high: 3, normal: 2, low: 1 };
      const aPriority = priorityOrder[a.priority] || 2;
      const bPriority = priorityOrder[b.priority] || 2;
      
      if (aPriority !== bPriority) return bPriority - aPriority;
      return new Date(b.dateCreated).getTime() - new Date(a.dateCreated).getTime();
    });

    logQuickbaseResponse('getPCTasksForProject', { count: tasks.length }, reqId, Date.now() - startTime);
    return tasks;

  } catch (error) {
    logQuickbaseError('getPCTasksForProject', error, reqId);
    throw error;
  }
}

/**
 * Create a new PC task
 */
export async function createPCTask(
  payload: PCTaskAssignmentPayload,
  coordinatorEmail: string,
  reqId: string
): Promise<number> {
  const startTime = Date.now();
  
  try {
    logQuickbaseRequest('createPCTask', { payload, coordinatorEmail }, reqId);

    // Validate recordId is a finite number
    if (!Number.isFinite(payload.recordId) || payload.recordId <= 0) {
      throw new Error(`Invalid recordId: ${payload.recordId}. Must be a positive number.`);
    }

    // Get or create task group for the project
    const taskGroupId = await getOrCreateTaskGroup(payload.projectId, payload.recordId, reqId);

    // Create task name and description
    const taskName = `PC: ${formatPCTaskName(payload.taskType)}`;
    const taskDescription = `${payload.description}\n\nAssigned to: ${payload.assignedTo}\nAssigned by: ${coordinatorEmail}\nDue date: ${payload.dueDate || 'Not specified'}\nPriority: ${payload.priority}`;

    // Create the task
    const taskData = {
      [TASK_FIELDS.TASK_GROUP]: taskGroupId,
      [TASK_FIELDS.STATUS]: 'Not Started',
      [TASK_FIELDS.NAME]: taskName,
      [TASK_FIELDS.DESCRIPTION]: taskDescription,
      [TASK_FIELDS.TASK_CATEGORY]: `PC: ${payload.taskType}`,
      [TASK_FIELDS.ASSIGNED_TO]: payload.assignedTo,
      [TASK_FIELDS.ASSIGNED_BY]: coordinatorEmail,
      [TASK_FIELDS.DATE_CREATED]: new Date().toISOString()
    };

    const taskResponse = await qbClient.createRecord(QB_TABLE_TASKS, taskData);
    const taskId = taskResponse.data;

    // Create initial Install Communication to notify rep
    const notificationContent = `New task assigned: ${taskName}\n\n${payload.description}`;
    await qbClient.createRecord(QB_TABLE_INSTALL_COMMUNICATIONS, {
      [INSTALL_COMMUNICATION_FIELDS.DATE]: new Date().toISOString(),
      [INSTALL_COMMUNICATION_FIELDS.NOTE_BY]: coordinatorEmail,
      [INSTALL_COMMUNICATION_FIELDS.RELATED_PROJECT]: payload.recordId,
      [INSTALL_COMMUNICATION_FIELDS.COMMUNICATION_NOTE]: notificationContent
    });

    logQuickbaseResponse('createPCTask', { taskId }, reqId, Date.now() - startTime);
    return taskId;

  } catch (error) {
    logQuickbaseError('createPCTask', error, reqId);
    throw error;
  }
}

/**
 * Update PC task status
 */
export async function updatePCTaskStatus(
  taskId: number,
  status: string,
  updatedBy: string,
  reqId: string
): Promise<boolean> {
  const startTime = Date.now();
  
  try {
    logQuickbaseRequest('updatePCTaskStatus', { taskId, status, updatedBy }, reqId);

    // First, fetch the task to get its TASK_GROUP
    const taskResponse = await qbClient.queryRecords({
      from: QB_TABLE_TASKS,
      where: `{${TASK_FIELDS.RECORD_ID}.EX.${taskId}}`,
      select: [TASK_FIELDS.TASK_GROUP]
    });

    if (taskResponse.data.length === 0) {
      logQuickbaseError('updatePCTaskStatus', new Error('Task not found'), reqId);
      return false;
    }

    const taskGroupId = taskResponse.data[0][TASK_FIELDS.TASK_GROUP];

    // Then fetch the Task Group to get the RELATED_PROJECT
    const taskGroupResponse = await qbClient.queryRecords({
      from: QB_TABLE_TASK_GROUPS,
      where: `{${TASK_GROUP_FIELDS.RECORD_ID}.EX.${taskGroupId}}`,
      select: [TASK_GROUP_FIELDS.RELATED_PROJECT]
    });

    if (taskGroupResponse.data.length === 0) {
      logQuickbaseError('updatePCTaskStatus', new Error('Task group not found'), reqId);
      return false;
    }

    const relatedProjectId = taskGroupResponse.data[0][TASK_GROUP_FIELDS.RELATED_PROJECT];

    // Update the task status
    await qbClient.updateRecord({
      to: QB_TABLE_TASKS,
      data: [
        { 
          [3]: { value: taskId }, 
          [TASK_FIELDS.STATUS]: { value: status },
          [TASK_FIELDS.DATE_MODIFIED]: { value: new Date().toISOString() }
        }
      ]
    });

    // Create Install Communication to log status change
    const statusContent = `Task status updated to: ${status}`;
    await qbClient.createRecord(QB_TABLE_INSTALL_COMMUNICATIONS, {
      [INSTALL_COMMUNICATION_FIELDS.DATE]: new Date().toISOString(),
      [INSTALL_COMMUNICATION_FIELDS.NOTE_BY]: updatedBy,
      [INSTALL_COMMUNICATION_FIELDS.RELATED_PROJECT]: relatedProjectId,
      [INSTALL_COMMUNICATION_FIELDS.COMMUNICATION_NOTE]: statusContent
    });

    logQuickbaseResponse('updatePCTaskStatus', { success: true }, reqId, Date.now() - startTime);
    return true;

  } catch (error) {
    logQuickbaseError('updatePCTaskStatus', error, reqId);
    throw error;
  }
}

/**
 * Get PC messages for a project (optionally filtered by task)
 */
export async function getPCMessagesForProject(
  projectId: string,
  recordId: number,
  taskId: number | null,
  userEmail: string,
  reqId: string
): Promise<PCMessage[]> {
  const startTime = Date.now();
  
  try {
    logQuickbaseRequest('getPCMessagesForProject', { projectId, recordId, taskId }, reqId);

    // If taskId is provided, we'll filter by content metadata
    // This is a simple implementation - in production, you might want to store taskId in a separate field
    const response = await qbClient.queryRecords({
      from: QB_TABLE_INSTALL_COMMUNICATIONS,
      where: `{${INSTALL_COMMUNICATION_FIELDS.RELATED_PROJECT}.EX.${recordId}}`,
      select: [
        INSTALL_COMMUNICATION_FIELDS.RECORD_ID,
        INSTALL_COMMUNICATION_FIELDS.DATE,
        INSTALL_COMMUNICATION_FIELDS.NOTE_BY,
        INSTALL_COMMUNICATION_FIELDS.RELATED_PROJECT,
        INSTALL_COMMUNICATION_FIELDS.COMMUNICATION_NOTE
      ],
      sortBy: [{ fieldId: INSTALL_COMMUNICATION_FIELDS.DATE, order: 'DESC' }]
    });

    // Get all message IDs for read status lookup
    const messageIds = response.data.map((record: any) => record[INSTALL_COMMUNICATION_FIELDS.RECORD_ID]);
    
    // Get read status for all messages
    const readStatus = await getMessagesReadStatus(messageIds, userEmail);

    const messagesPromises = response.data.map(async (record: any) => {
      const content = record[INSTALL_COMMUNICATION_FIELDS.COMMUNICATION_NOTE] || '';

      // Extract mentions from content
      const mentions = extractMentions(content);

      // Determine role using authoritative user lookup
      const sentByEmail = record[INSTALL_COMMUNICATION_FIELDS.NOTE_BY];
      let sentByRole: 'pc' | 'rep' = 'rep'; // Default to rep

      if (sentByEmail) {
        try {
          const userRole = await getUserRoleByEmail(sentByEmail);
          if (userRole) {
            sentByRole = userRole;
          } else {
            // User not found in database - log for remediation
            console.warn(`[getPCMessagesForProject] User not found in database: ${sentByEmail}, defaulting to 'rep'`);
          }
        } catch (error) {
          // Role lookup failed - log error and use default
          console.error(`[getPCMessagesForProject] Error looking up role for ${sentByEmail}:`, error);
        }
      }

      // Extract task ID from content if present
      const taskIdMatch = content.match(/taskId: (\d+)/);
      const relatedTaskId = taskIdMatch ? parseInt(taskIdMatch[1]) : null;

      // If filtering by taskId, only include messages related to that task
      if (taskId && relatedTaskId !== taskId) return null;

      const messageId = record[INSTALL_COMMUNICATION_FIELDS.RECORD_ID];
      const isRead = readStatus[messageId] || false;

      return {
        recordId: messageId,
        date: record[INSTALL_COMMUNICATION_FIELDS.DATE],
        sentBy: record[INSTALL_COMMUNICATION_FIELDS.NOTE_BY],
        sentByRole: sentByRole,
        projectId: projectId,
        content: content,
        relatedTaskId: relatedTaskId,
        mentions: mentions,
        isRead: isRead,
        parentMessageId: null // TODO: Implement threading
      };
    });

    const messages: PCMessage[] = (await Promise.all(messagesPromises)).filter(Boolean) as PCMessage[];

    logQuickbaseResponse('getPCMessagesForProject', { count: messages.length }, reqId, Date.now() - startTime);
    return messages;

  } catch (error) {
    logQuickbaseError('getPCMessagesForProject', error, reqId);
    throw error;
  }
}

/**
 * Create a PC message
 */
export async function createPCMessage(
  projectId: string,
  recordId: number,
  content: string,
  sentBy: string,
  taskId: number | null,
  mentions: string[],
  reqId: string
): Promise<number> {
  const startTime = Date.now();
  
  try {
    logQuickbaseRequest('createPCMessage', { projectId, recordId, content, sentBy, taskId, mentions }, reqId);

    // Build message content with metadata
    let messageContent = content;
    if (taskId) {
      messageContent += `\n\ntaskId: ${taskId}`;
    }
    if (mentions.length > 0) {
      messageContent += `\n\nmentions: ${mentions.join(',')}`;
    }

    // Create the message
    const messageData = {
      [INSTALL_COMMUNICATION_FIELDS.DATE]: new Date().toISOString(),
      [INSTALL_COMMUNICATION_FIELDS.NOTE_BY]: sentBy,
      [INSTALL_COMMUNICATION_FIELDS.RELATED_PROJECT]: recordId,
      [INSTALL_COMMUNICATION_FIELDS.COMMUNICATION_NOTE]: messageContent
    };

    const response = await qbClient.createRecord(QB_TABLE_INSTALL_COMMUNICATIONS, messageData);
    const messageId = response.data;

    logQuickbaseResponse('createPCMessage', { messageId }, reqId, Date.now() - startTime);
    return messageId;

  } catch (error) {
    logQuickbaseError('createPCMessage', error, reqId);
    throw error;
  }
}

/**
 * Mark PC messages as read for a specific user
 */
export async function markPCMessagesRead(
  messageIds: number[],
  userEmail: string,
  reqId: string
): Promise<void> {
  const startTime = Date.now();
  
  try {
    logQuickbaseRequest('markPCMessagesRead', { messageIds, userEmail }, reqId);
    
    await markMessagesAsRead(messageIds, userEmail);
    
    logQuickbaseResponse('markPCMessagesRead', { messageIds: messageIds.length }, reqId, Date.now() - startTime);
    
  } catch (error) {
    logQuickbaseError('markPCMessagesRead', error, reqId);
    throw error;
  }
}

/**
 * Get or create task group for a project
 */
async function getOrCreateTaskGroup(
  projectId: string,
  recordId: number,
  reqId: string
): Promise<number> {
  try {
    // First, try to find existing task group
    const existingResponse = await qbClient.queryRecords({
      from: QB_TABLE_TASK_GROUPS,
      where: `{${TASK_GROUP_FIELDS.RELATED_PROJECT}.EX.${recordId}}`,
      select: [TASK_GROUP_FIELDS.RECORD_ID]
    });

    if (existingResponse.data.length > 0) {
      return existingResponse.data[0][TASK_GROUP_FIELDS.RECORD_ID];
    }

    // Create new task group
    const taskGroupData = {
      [TASK_GROUP_FIELDS.RELATED_PROJECT]: recordId
    };

    const response = await qbClient.createRecord(QB_TABLE_TASK_GROUPS, taskGroupData);
    return response.data;

  } catch (error) {
    logQuickbaseError('getOrCreateTaskGroup', error, reqId);
    throw error;
  }
}

/**
 * Parse PC task type from TASK_CATEGORY field
 */
function parsePCTaskType(taskCategory: string): PCTaskType | null {
  if (!taskCategory) return null;
  const categoryStr = typeof taskCategory === 'string' ? taskCategory : String(taskCategory);
  if (!categoryStr.startsWith('PC: ')) return null;

  const taskType = categoryStr.replace('PC: ', '') as PCTaskType;
  const validTypes: PCTaskType[] = [
    'callback_customer',
    'collect_documents',
    'clarify_pricing',
    'handle_objection',
    'schedule_site_visit',
    'resolve_hoa_issue'
  ];
  
  return validTypes.includes(taskType) ? taskType : null;
}

/**
 * Format PC task type to display name
 */
function formatPCTaskName(taskType: PCTaskType): string {
  const displayNames: Record<PCTaskType, string> = {
    callback_customer: 'Callback Customer',
    collect_documents: 'Collect Documents',
    clarify_pricing: 'Clarify Pricing',
    handle_objection: 'Handle Objection',
    schedule_site_visit: 'Schedule Site Visit',
    resolve_hoa_issue: 'Resolve HOA Issue'
  };
  
  return displayNames[taskType];
}

/**
 * Extract @mentions from message content
 */
function extractMentions(content: string): string[] {
  const mentionRegex = /@(\w+)/g;
  const mentions: string[] = [];
  let match;
  
  while ((match = mentionRegex.exec(content)) !== null) {
    mentions.push(match[1]);
  }
  
  return [...new Set(mentions)]; // Remove duplicates
}

// =============================================================================
// ESCALATION MANAGEMENT QUERIES
// =============================================================================

/**
 * Get PC escalations from Sales Aid Requests table
 */
export async function getPCEscalations(
  pcEmail: string,
  pcName: string,
  role: string,
  filters: PCEscalationFilters,
  reqId: string
): Promise<PCEscalation[]> {
  try {
    logQuickbaseRequest('getPCEscalations', { pcEmail, pcName, role, filters }, reqId);

    // Import role helpers
    const { hasOperationsUnrestrictedAccess, isOperationsManager } = await import('@/lib/utils/role-helpers');

    const sanitizedEmail = sanitizeQbLiteral(pcEmail);
    const sanitizedName = sanitizeQuickbaseValue(pcName);

    // Build WHERE clause for escalation status
    const escalationStatusClause = `{${SALES_AID_FIELDS.SALES_AID_STATUS}}.EX.'Escalated to Sales Aid' OR {${SALES_AID_FIELDS.ESCALATE_TO_SALES_AID}}.EX.'true' AND {${SALES_AID_FIELDS.SALES_AID_STATUS}}.NE.'Resolved by Rep' AND {${SALES_AID_FIELDS.SALES_AID_STATUS}}.NE.'Task Completed'`;

    let whereClause: string;

    // Optimize: For unrestricted roles, skip the project fetch entirely
    if (hasOperationsUnrestrictedAccess(role)) {
      // Super admin, regional, office leaders see ALL escalations - no project filter needed
      whereClause = escalationStatusClause;
    } else {
      // For restricted roles, we need to fetch accessible projects first
      let projectsWhere: string;
      if (isOperationsManager(role)) {
        // Operations managers see escalations for all operations projects (any project with a PC)
        projectsWhere = `{${PROJECT_FIELDS.PROJECT_COORDINATOR}.XEX.''}`;
      } else {
        // Operations coordinators see only escalations for their assigned projects
        projectsWhere = `({${PROJECT_FIELDS.PROJECT_COORDINATOR_EMAIL}.EX.'${sanitizedEmail}'})OR({${PROJECT_FIELDS.PROJECT_COORDINATOR}.EX.'${sanitizedName}'})`;
      }

      const projectsQuery = {
        from: QB_TABLE_PROJECTS,
        where: projectsWhere,
        select: [PROJECT_FIELDS.RECORD_ID]
      };

      const projectsResponse = await qbClient.queryRecords(projectsQuery);
      const projectIds = (projectsResponse.data || []).map((p: any) => extractNumericValue(p[PROJECT_FIELDS.RECORD_ID]));

      if (projectIds.length === 0) {
        return [];
      }

      // Filter by accessible projects
      const projectIdsStr = projectIds.join(',');
      whereClause = `(${escalationStatusClause})AND{${SALES_AID_FIELDS.RELATED_PROJECT}.IN.${projectIdsStr}}`;
    }

    const query = {
      from: QB_TABLE_SALES_AID_REQUESTS,
      where: whereClause,
      select: [
        SALES_AID_FIELDS.RECORD_ID,
        SALES_AID_FIELDS.DATE_CREATED,
        SALES_AID_FIELDS.RELATED_PROJECT,
        SALES_AID_FIELDS.SALES_AID_REASON,
        SALES_AID_FIELDS.SALES_AID_STATUS,
        SALES_AID_FIELDS.ESCALATE_TO_SALES_AID,
        SALES_AID_FIELDS.ESCALATED_DATETIME,
        SALES_AID_FIELDS.REP_72_HOUR_DEADLINE,
        SALES_AID_FIELDS.ASSIGNED_ESCALATION_REP,
        SALES_AID_FIELDS.COMPLETED_DATE,
        SALES_AID_FIELDS.TIME_WAITING,
        SALES_AID_FIELDS.URGENCY_LEVEL,
        SALES_AID_FIELDS.ESCALATION_NOTES
      ],
      sortBy: [
        { field: SALES_AID_FIELDS.URGENCY_LEVEL, order: 'DESC' },
        { field: SALES_AID_FIELDS.TIME_WAITING, order: 'DESC' }
      ],
      options: { top: 5000 }
    };

    const response = await qbClient.queryRecords(query);
    
    if (!response.data) {
      logQuickbaseResponse('getPCEscalations', { data: [] }, reqId);
      return [];
    }

    // Collect unique project IDs for lookup
    const escalationProjectIds = [...new Set(response.data.map((record: any) => extractNumericValue(record[SALES_AID_FIELDS.RELATED_PROJECT])))].filter(id => id && id > 0);

    // Fetch project details if we have project IDs
    let projectDetails: Map<number, any> = new Map();
    if (escalationProjectIds.length > 0) {
      try {
        const projectQuery = {
          from: QB_TABLE_PROJECTS,
          where: `{${PROJECT_FIELDS.RECORD_ID}.IN.${escalationProjectIds.join(',')}}`,
          select: [
            PROJECT_FIELDS.RECORD_ID,
            PROJECT_FIELDS.PROJECT_ID,
            PROJECT_FIELDS.CUSTOMER_NAME,
            PROJECT_FIELDS.CLOSER_NAME,
            PROJECT_FIELDS.CLOSER_EMAIL,
            PROJECT_FIELDS.SETTER_NAME,
            PROJECT_FIELDS.SETTER_EMAIL
          ]
        };
        
        const projectResponse = await qbClient.queryRecords(projectQuery);
        if (projectResponse.data) {
          projectResponse.data.forEach((project: any) => {
            projectDetails.set(project[PROJECT_FIELDS.RECORD_ID], project);
          });
        }
      } catch (projectError) {
        logQuickbaseError('getPCEscalations - project lookup', projectError, reqId);
        // Continue without project details rather than failing completely
      }
    }

    // Transform to PCEscalation format
    const escalations: PCEscalation[] = response.data.map((record: any) => {
      // Extract wrapped values from Sales Aid fields
      const salesAidReason = record[SALES_AID_FIELDS.SALES_AID_REASON]?.value || record[SALES_AID_FIELDS.SALES_AID_REASON] || '';
      const category = categorizeEscalationReason(salesAidReason);
      const urgency = calculateSalesAidUrgency(record[SALES_AID_FIELDS.REP_72_HOUR_DEADLINE]);

      // Get project details and extract wrapped values
      const projectDetail = projectDetails.get(record[SALES_AID_FIELDS.RELATED_PROJECT]);
      const projectId = projectDetail?.[PROJECT_FIELDS.PROJECT_ID]?.value || projectDetail?.[PROJECT_FIELDS.PROJECT_ID] || '';
      const customerName = projectDetail?.[PROJECT_FIELDS.CUSTOMER_NAME]?.value || projectDetail?.[PROJECT_FIELDS.CUSTOMER_NAME] || '';
      const salesRepName = projectDetail?.[PROJECT_FIELDS.CLOSER_NAME]?.value || projectDetail?.[PROJECT_FIELDS.CLOSER_NAME] || projectDetail?.[PROJECT_FIELDS.SETTER_NAME]?.value || projectDetail?.[PROJECT_FIELDS.SETTER_NAME] || '';
      const salesRepEmail = projectDetail?.[PROJECT_FIELDS.CLOSER_EMAIL]?.value || projectDetail?.[PROJECT_FIELDS.CLOSER_EMAIL] || projectDetail?.[PROJECT_FIELDS.SETTER_EMAIL]?.value || projectDetail?.[PROJECT_FIELDS.SETTER_EMAIL] || '';
      const messagePreview = String(salesAidReason).substring(0, 100);

      return {
        recordId: record[SALES_AID_FIELDS.RECORD_ID],
        dateCreated: record[SALES_AID_FIELDS.DATE_CREATED],
        relatedProject: record[SALES_AID_FIELDS.RELATED_PROJECT],
        projectId,
        customerName,
        salesRepName,
        salesRepEmail,
        salesAidStatus: record[SALES_AID_FIELDS.SALES_AID_STATUS]?.value || record[SALES_AID_FIELDS.SALES_AID_STATUS] || '',
        salesAidReason,
        escalateToSalesAid: record[SALES_AID_FIELDS.ESCALATE_TO_SALES_AID],
        escalatedDateTime: record[SALES_AID_FIELDS.ESCALATED_DATETIME],
        assignedEscalationRep: record[SALES_AID_FIELDS.ASSIGNED_ESCALATION_REP]?.value || record[SALES_AID_FIELDS.ASSIGNED_ESCALATION_REP] || '',
        rep72HourDeadline: record[SALES_AID_FIELDS.REP_72_HOUR_DEADLINE],
        completedDate: record[SALES_AID_FIELDS.COMPLETED_DATE],
        urgency,
        timeWaiting: record[SALES_AID_FIELDS.TIME_WAITING],
        messagePreview,
        category,
        gracePeriodEnd: record[SALES_AID_FIELDS.REP_72_HOUR_DEADLINE],
        gracePeriodExtended: false, // TODO: Calculate from history
        resolutionNote: null, // TODO: Get from Install Communications
        history: [] // TODO: Get from Install Communications
      };
    });

    // Apply filters
    let filteredEscalations = escalations;

    if (filters.category !== 'all') {
      filteredEscalations = filteredEscalations.filter(e => e.category === filters.category);
    }

    if (filters.status !== 'all') {
      filteredEscalations = filteredEscalations.filter(e => e.salesAidStatus === filters.status);
    }

    if (filters.urgency !== 'all') {
      filteredEscalations = filteredEscalations.filter(e => {
        const urgency = calculateSalesAidUrgency(e.rep72HourDeadline);
        return urgency === filters.urgency;
      });
    }

    if (filters.assignedTo !== 'all') {
      if (filters.assignedTo === 'unassigned') {
        filteredEscalations = filteredEscalations.filter(e => !e.assignedEscalationRep);
      } else {
        filteredEscalations = filteredEscalations.filter(e => e.assignedEscalationRep === filters.assignedTo);
      }
    }

    if (filters.search) {
      const searchLower = filters.search.toLowerCase();
      filteredEscalations = filteredEscalations.filter(e => 
        e.salesAidReason?.toLowerCase().includes(searchLower) ||
        e.relatedProject?.toString().includes(searchLower)
      );
    }

    logQuickbaseResponse('getPCEscalations', { count: filteredEscalations.length }, reqId);
    return filteredEscalations;

  } catch (error) {
    logQuickbaseError('getPCEscalations', error, reqId);
    throw error;
  }
}

/**
 * Create new escalation
 */
export async function createEscalation(
  payload: PCCreateEscalationPayload,
  coordinatorEmail: string,
  reqId: string
): Promise<number> {
  try {
    logQuickbaseRequest('createEscalation', { payload, coordinatorEmail }, reqId);

    // Calculate deadline (72 hours for normal, 48 hours for high priority)
    const deadlineHours = payload.priority === 'high' ? 48 : 72;
    const deadline = new Date(Date.now() + deadlineHours * 60 * 60 * 1000).toISOString();

    const data = [
      { field: SALES_AID_FIELDS.RELATED_PROJECT, value: payload.recordId },
      { field: SALES_AID_FIELDS.SALES_AID_REASON, value: payload.reason },
      { field: SALES_AID_FIELDS.SALES_AID_STATUS, value: 'Escalated to Sales Aid' },
      { field: SALES_AID_FIELDS.ESCALATE_TO_SALES_AID, value: true },
      { field: SALES_AID_FIELDS.ESCALATED_DATETIME, value: new Date().toISOString() },
      { field: SALES_AID_FIELDS.REP_72_HOUR_DEADLINE, value: deadline },
      { field: SALES_AID_FIELDS.ESCALATION_NOTES, value: payload.description }
    ];

    const response = await qbClient.updateRecord({
      to: QB_TABLE_SALES_AID_REQUESTS,
      data: data.map(item => ({ [item.field]: { value: item.value } }))
    });

    const escalationId = response.recordId;

    // Create Install Communication record to log escalation creation
    await createInstallCommunication({
      relatedProject: payload.recordId,
      communicationType: 'escalation_created',
      communicationNote: `Escalation created: ${payload.reason} - ${payload.description}`,
      performedBy: coordinatorEmail,
      relatedSalesAidRequest: escalationId
    }, reqId);

    // Create notification for assigned rep (if specified)
    if (payload.assignedTo) {
      await createNotification({
        userId: payload.assignedTo,
        type: 'escalation_assigned',
        title: 'New Escalation Assigned',
        message: `You have been assigned a new escalation: ${payload.reason}`,
        data: { escalationId, projectId: payload.projectId }
      }, reqId);
    }

    logQuickbaseResponse('createEscalation', { escalationId }, reqId);
    return escalationId;

  } catch (error) {
    logQuickbaseError('createEscalation', error, reqId);
    throw error;
  }
}

/**
 * Update escalation status
 */
export async function updateEscalationStatus(
  escalationId: number,
  status: PCSalesAidStatus,
  updatedBy: string,
  note: string | null,
  reqId: string
): Promise<boolean> {
  try {
    logQuickbaseRequest('updateEscalationStatus', { escalationId, status, updatedBy, note }, reqId);

    // Update Sales Aid Request status
    await updateSalesAidRequest(escalationId, status, updatedBy, reqId);

    // Set completion date if resolved
    if (status === 'Resolved by Rep' || status === 'Task Completed') {
      await qbClient.updateRecord({
        to: QB_TABLE_SALES_AID_REQUESTS,
        data: [
          { [3]: { value: escalationId }, [SALES_AID_FIELDS.COMPLETED_DATE]: { value: new Date().toISOString() } }
        ]
      });
    }

    // Create Install Communication record to log status change
    await createInstallCommunication({
      relatedSalesAidRequest: escalationId,
      communicationType: 'escalation_status_changed',
      communicationNote: `Status changed to: ${status}${note ? ` - ${note}` : ''}`,
      performedBy: updatedBy
    }, reqId);

    // Create notification for PC about status change
    await createNotification({
      userId: updatedBy,
      type: 'escalation_status_updated',
      title: 'Escalation Status Updated',
      message: `Escalation status changed to: ${status}`,
      data: { escalationId, status }
    }, reqId);

    logQuickbaseResponse('updateEscalationStatus', { success: true }, reqId);
    return true;

  } catch (error) {
    logQuickbaseError('updateEscalationStatus', error, reqId);
    throw error;
  }
}

/**
 * Extend escalation grace period
 */
export async function extendEscalationGracePeriod(
  escalationId: number,
  newDeadline: string,
  reason: string,
  extendedBy: string,
  reqId: string
): Promise<boolean> {
  try {
    logQuickbaseRequest('extendEscalationGracePeriod', { escalationId, newDeadline, reason, extendedBy }, reqId);

    // Update deadline
    await qbClient.updateRecord({
      to: QB_TABLE_SALES_AID_REQUESTS,
      data: [
        { [3]: { value: escalationId }, [SALES_AID_FIELDS.REP_72_HOUR_DEADLINE]: { value: newDeadline } }
      ]
    });

    // Create Install Communication record to log extension
    await createInstallCommunication({
      relatedSalesAidRequest: escalationId,
      communicationType: 'escalation_grace_extended',
      communicationNote: `Grace period extended to ${newDeadline}. Reason: ${reason}`,
      performedBy: extendedBy
    }, reqId);

    // Create notification for assigned rep about extension
    await createNotification({
      userId: extendedBy,
      type: 'escalation_grace_extended',
      title: 'Escalation Grace Period Extended',
      message: `Grace period extended to ${new Date(newDeadline).toLocaleDateString()}`,
      data: { escalationId, newDeadline }
    }, reqId);

    logQuickbaseResponse('extendEscalationGracePeriod', { success: true }, reqId);
    return true;

  } catch (error) {
    logQuickbaseError('extendEscalationGracePeriod', error, reqId);
    throw error;
  }
}

/**
 * Assign escalation to sales aid rep
 */
export async function assignEscalationToRep(
  escalationId: number,
  repEmail: string,
  assignedBy: string,
  reqId: string
): Promise<boolean> {
  try {
    logQuickbaseRequest('assignEscalationToRep', { escalationId, repEmail, assignedBy }, reqId);

    // Update assignment
    await qbClient.updateRecord({
      to: QB_TABLE_SALES_AID_REQUESTS,
      data: [
        { 
          [3]: { value: escalationId }, 
          [SALES_AID_FIELDS.ASSIGNED_ESCALATION_REP]: { value: repEmail },
          [SALES_AID_FIELDS.SALES_AID_STATUS]: { value: 'Escalated to Sales Aid' }
        }
      ]
    });

    // Create Install Communication record to log assignment
    await createInstallCommunication({
      relatedSalesAidRequest: escalationId,
      communicationType: 'escalation_assigned',
      communicationNote: `Escalation assigned to: ${repEmail}`,
      performedBy: assignedBy
    }, reqId);

    // Create notification for assigned rep
    await createNotification({
      userId: repEmail,
      type: 'escalation_assigned',
      title: 'Escalation Assigned to You',
      message: 'You have been assigned a new escalation to resolve',
      data: { escalationId }
    }, reqId);

    logQuickbaseResponse('assignEscalationToRep', { success: true }, reqId);
    return true;

  } catch (error) {
    logQuickbaseError('assignEscalationToRep', error, reqId);
    throw error;
  }
}

/**
 * Get escalation history
 */
export async function getEscalationHistory(
  escalationId: number,
  reqId: string
): Promise<PCEscalationHistoryItem[]> {
  try {
    logQuickbaseRequest('getEscalationHistory', { escalationId }, reqId);

    const query = {
      from: QB_TABLE_INSTALL_COMMUNICATIONS,
      where: `{${COMMUNICATION_FIELDS.RELATED_SALES_AID_REQUEST}.EX.${escalationId}}`,
      select: [
        COMMUNICATION_FIELDS.DATE,
        COMMUNICATION_FIELDS.COMMUNICATION_TYPE,
        COMMUNICATION_FIELDS.COMMUNICATION_NOTE,
        COMMUNICATION_FIELDS.PERFORMED_BY
      ],
      sortBy: [{ field: COMMUNICATION_FIELDS.DATE, order: 'DESC' }]
    };

    const response = await qbClient.queryRecords(query);
    
    if (!response.data) {
      logQuickbaseResponse('getEscalationHistory', { data: [] }, reqId);
      return [];
    }

    const history: PCEscalationHistoryItem[] = response.data.map((record: any) => ({
      timestamp: record[COMMUNICATION_FIELDS.DATE],
      action: mapCommunicationTypeToAction(record[COMMUNICATION_FIELDS.COMMUNICATION_TYPE]),
      performedBy: record[COMMUNICATION_FIELDS.PERFORMED_BY],
      details: record[COMMUNICATION_FIELDS.COMMUNICATION_NOTE],
      oldValue: null,
      newValue: null
    }));

    logQuickbaseResponse('getEscalationHistory', { count: history.length }, reqId);
    return history;

  } catch (error) {
    logQuickbaseError('getEscalationHistory', error, reqId);
    throw error;
  }
}

/**
 * Categorize escalation reason
 */
export function categorizeEscalationReason(reason: string): PCEscalationCategory {
  if (!reason) return 'customer_complaints';
  
  const lowerReason = reason.toLowerCase();
  
  if (lowerReason.includes('mpu required')) return 'mmu_required';
  if (lowerReason.includes("can't reach the customer") || 
      lowerReason.includes('system change') || 
      lowerReason.includes('needs new loan docs signed')) return 'rep_promises';
  if (lowerReason.includes('hoa')) return 'hoa_issues';
  if (lowerReason.includes('credit concerns') || 
      lowerReason.includes('needs new loan docs signed')) return 'financing_issues';
  if (lowerReason.includes('customer requested hold') || 
      lowerReason.includes('customer complaint')) return 'customer_complaints';
  
  return 'customer_complaints';
}

/**
 * Get category display name
 */
export function getCategoryDisplayName(category: PCEscalationCategory): string {
  const displayNames: Record<PCEscalationCategory, string> = {
    mmu_required: 'MMU Required',
    rep_promises: 'Rep Promises',
    hoa_issues: 'HOA Issues',
    financing_issues: 'Financing Issues',
    customer_complaints: 'Customer Complaints'
  };
  
  return displayNames[category];
}

/**
 * Map communication type to escalation action
 */
function mapCommunicationTypeToAction(communicationType: string): PCEscalationHistoryItem['action'] {
  switch (communicationType) {
    case 'escalation_created': return 'created';
    case 'escalation_assigned': return 'assigned';
    case 'escalation_grace_extended': return 'grace_extended';
    case 'escalation_status_changed': return 'status_changed';
    case 'escalation_resolved': return 'resolved';
    default: return 'status_changed';
  }
}

// =============================================================================
// PC ANALYTICS QUERIES
// =============================================================================

/**
 * Calculate date range based on timeRange parameter
 */
function calculateDateRange(timeRange: '7days' | '30days' | '90days' | 'all'): { startDate: string; endDate: string } {
  const now = new Date();
  const endDate = now.toISOString().split('T')[0];
  
  let startDate: string;
  switch (timeRange) {
    case '7days':
      startDate = new Date(now.getTime() - 7 * 24 * 60 * 60 * 1000).toISOString().split('T')[0];
      break;
    case '30days':
      startDate = new Date(now.getTime() - 30 * 24 * 60 * 60 * 1000).toISOString().split('T')[0];
      break;
    case '90days':
      startDate = new Date(now.getTime() - 90 * 24 * 60 * 60 * 1000).toISOString().split('T')[0];
      break;
    case 'all':
      startDate = '2020-01-01'; // Far back enough to capture all data
      break;
  }
  
  return { startDate, endDate };
}

/**
 * Calculate SLA compliance percentage
 */
function calculateSLACompliance(projects: any[]): number {
  if (projects.length === 0) return 0;
  
  let compliantCount = 0;
  
  for (const project of projects) {
    let isCompliant = true;
    
    // Check each milestone against SLA deadlines
    // Survey SLA: 7 days from sale
    const salesDate = project[PROJECT_FIELDS.SALES_DATE];
    const surveySubmitted = project[PROJECT_FIELDS.SURVEY_SUBMITTED];
    if (salesDate && surveySubmitted) {
      const salesDateObj = new Date(salesDate);
      const surveyDateObj = new Date(surveySubmitted);
      const daysToSurvey = (surveyDateObj.getTime() - salesDateObj.getTime()) / (1000 * 60 * 60 * 24);
      if (daysToSurvey > 7) isCompliant = false;
    }
    
    // Design SLA: 14 days from survey approval
    const surveyApproved = project[PROJECT_FIELDS.SURVEY_APPROVED];
    const designCompleted = project[PROJECT_FIELDS.DESIGN_COMPLETED];
    if (surveyApproved && designCompleted) {
      const surveyApprovedObj = new Date(surveyApproved);
      const designDateObj = new Date(designCompleted);
      const daysToDesign = (designDateObj.getTime() - surveyApprovedObj.getTime()) / (1000 * 60 * 60 * 24);
      if (daysToDesign > 14) isCompliant = false;
    }
    
    // Install SLA: 30 days from design completion
    const installCompleted = project[PROJECT_FIELDS.INSTALL_COMPLETED_DATE];
    if (designCompleted && installCompleted) {
      const designDateObj = new Date(designCompleted);
      const installDateObj = new Date(installCompleted);
      const daysToInstall = (installDateObj.getTime() - designDateObj.getTime()) / (1000 * 60 * 60 * 24);
      if (daysToInstall > 30) isCompliant = false;
    }
    
    // PTO SLA: 14 days from install completion
    const ptoCompleted = project[PROJECT_FIELDS.PTO_COMPLETED];
    if (installCompleted && ptoCompleted) {
      const installDateObj = new Date(installCompleted);
      const ptoDateObj = new Date(ptoCompleted);
      const daysToPTO = (ptoDateObj.getTime() - installDateObj.getTime()) / (1000 * 60 * 60 * 24);
      if (daysToPTO > 14) isCompliant = false;
    }
    
    if (isCompliant) compliantCount++;
  }
  
  return (compliantCount / projects.length) * 100;
}

/**
 * Get PC personal metrics for analytics dashboard
 */
export async function getPCPersonalMetrics(
  pcEmail: string,
  pcName: string,
  timeRange: '7days' | '30days' | '90days' | 'all',
  reqId: string
): Promise<PCPersonalMetrics> {
  const startTime = Date.now();
  
  try {
    logQuickbaseRequest('getPCPersonalMetrics', { pcEmail, pcName, timeRange }, reqId);

    const { startDate, endDate } = calculateDateRange(timeRange);

    // Query Outreach Records for PC's outreach data
    const outreachWhere = `{${OUTREACH_RECORD_FIELDS.PC_EMAIL}}.EX.'${pcEmail}' AND {${OUTREACH_RECORD_FIELDS.DATE_CREATED}}.GTE.'${startDate}' AND {${OUTREACH_RECORD_FIELDS.DATE_CREATED}}.LTE.'${endDate}'`;

    const outreachResponse = await qbClient.queryRecords({
      from: QB_TABLE_OUTREACH_RECORDS,
      where: outreachWhere,
      select: [
        OUTREACH_RECORD_FIELDS.DATE_CREATED,
        OUTREACH_RECORD_FIELDS.OUTREACH_STATUS,
        OUTREACH_RECORD_FIELDS.RECORD_ID
      ]
    });

    const outreachRecords = outreachResponse.data || [];
    
    // Calculate outreach metrics
    const today = new Date().toISOString().split('T')[0];
    const weekStart = new Date(Date.now() - 7 * 24 * 60 * 60 * 1000).toISOString().split('T')[0];
    const monthStart = new Date(Date.now() - 30 * 24 * 60 * 60 * 1000).toISOString().split('T')[0];
    
    const dailyOutreach = outreachRecords.filter(r => {
      const dateCreated = r[OUTREACH_RECORD_FIELDS.DATE_CREATED];
      if (!dateCreated) return false;
      const dateStr = typeof dateCreated === 'string' ? dateCreated : String(dateCreated);
      return dateStr.startsWith(today);
    }).length;
    const weeklyOutreach = outreachRecords.filter(r => r[OUTREACH_RECORD_FIELDS.DATE_CREATED] >= weekStart).length;
    const monthlyOutreach = outreachRecords.filter(r => r[OUTREACH_RECORD_FIELDS.DATE_CREATED] >= monthStart).length;
    
    const completedOutreach = outreachRecords.filter(r => r[OUTREACH_RECORD_FIELDS.OUTREACH_STATUS] === 'Complete').length;
    const responseRate = outreachRecords.length > 0 ? (completedOutreach / outreachRecords.length) * 100 : 0;
    
    // Query Projects for PC's active projects
    const projectsWhere = `{${PROJECT_FIELDS.PROJECT_COORDINATOR_EMAIL}}.EX.'${pcEmail}' AND {${PROJECT_FIELDS.PROJECT_STATUS}}.EX.'Active'`;

    const projectsResponse = await qbClient.queryRecords({
      from: QB_TABLE_PROJECTS,
      where: projectsWhere,
      select: [
        PROJECT_FIELDS.RECORD_ID,
        PROJECT_FIELDS.PC_DAYS_SINCE_CONTACT_1,
        PROJECT_FIELDS.PC_DAYS_SINCE_CONTACT_2,
        PROJECT_FIELDS.PC_DAYS_SINCE_CONTACT_3,
        PROJECT_FIELDS.PC_DAYS_SINCE_CONTACT_4,
        PROJECT_FIELDS.PROJECT_AGE,
        PROJECT_FIELDS.ON_HOLD
      ]
    });

    const projects = projectsResponse.data || [];
    
    // Calculate average time to contact
    const contactDays = projects
      .map(p => [
        p[PROJECT_FIELDS.PC_DAYS_SINCE_CONTACT_1],
        p[PROJECT_FIELDS.PC_DAYS_SINCE_CONTACT_2],
        p[PROJECT_FIELDS.PC_DAYS_SINCE_CONTACT_3],
        p[PROJECT_FIELDS.PC_DAYS_SINCE_CONTACT_4]
      ])
      .flat()
      .filter(d => d && !isNaN(Number(d)))
      .map(d => Number(d));
    
    const avgTimeToContact = contactDays.length > 0 
      ? contactDays.reduce((sum, days) => sum + days, 0) / contactDays.length 
      : 0;

    // Query Sales Aid Requests for escalations
    const escalationsWhere = `{${SALES_AID_REQUEST_FIELDS.REQUESTED_BY_EMAIL}}.EX.'${pcEmail}' AND {${SALES_AID_REQUEST_FIELDS.DATE_CREATED}}.GTE.'${startDate}' AND {${SALES_AID_REQUEST_FIELDS.DATE_CREATED}}.LTE.'${endDate}'`;

    const escalationsResponse = await qbClient.queryRecords({
      from: QB_TABLE_SALES_AID_REQUESTS,
      where: escalationsWhere,
      select: [SALES_AID_REQUEST_FIELDS.RECORD_ID]
    });

    const escalations = escalationsResponse.data || [];
    const escalationRate = projects.length > 0 ? (escalations.length / projects.length) * 100 : 0;

    // Calculate SLA compliance
    const slaCompliance = calculateSLACompliance(projects);

    // Calculate additional metrics
    const pendingOutreach = outreachRecords.filter(r => r[OUTREACH_RECORD_FIELDS.OUTREACH_STATUS] === 'Pending').length;
    const unresponsiveCount = outreachRecords.filter(r => r[OUTREACH_RECORD_FIELDS.OUTREACH_STATUS] === 'No Answer Left Message').length;
    const activeEscalations = escalations.length;

    const metrics: PCPersonalMetrics = {
      dailyOutreach,
      weeklyOutreach,
      monthlyOutreach,
      responseRate,
      avgTimeToContact,
      escalationRate,
      slaCompliance,
      totalProjects: projects.length,
      completedOutreach,
      pendingOutreach,
      unresponsiveCount,
      activeEscalations
    };

    logQuickbaseResponse('getPCPersonalMetrics', metrics, Date.now() - startTime, reqId);
    return metrics;

  } catch (error) {
    logQuickbaseError('getPCPersonalMetrics', error, reqId);
    throw error;
  }
}

/**
 * Get PC outreach trend data for last N days
 */
export async function getPCOutreachTrend(
  pcEmail: string,
  pcName: string,
  days: number = 30,
  reqId: string
): Promise<PCOutreachTrendData[]> {
  const startTime = Date.now();
  
  try {
    logQuickbaseRequest('getPCOutreachTrend', { pcEmail, pcName, days }, reqId);

    const endDate = new Date().toISOString().split('T')[0];
    const startDate = new Date(Date.now() - days * 24 * 60 * 60 * 1000).toISOString().split('T')[0];

    const outreachWhere = `{${OUTREACH_RECORD_FIELDS.PC_EMAIL}}.EX.'${pcEmail}' AND {${OUTREACH_RECORD_FIELDS.DATE_CREATED}}.GTE.'${startDate}' AND {${OUTREACH_RECORD_FIELDS.DATE_CREATED}}.LTE.'${endDate}'`;

    const outreachResponse = await qbClient.queryRecords({
      from: QB_TABLE_OUTREACH_RECORDS,
      where: outreachWhere,
      select: [
        OUTREACH_RECORD_FIELDS.DATE_CREATED,
        OUTREACH_RECORD_FIELDS.OUTREACH_STATUS
      ]
    });

    const outreachRecords = outreachResponse.data || [];
    
    // Group by date
    const dateGroups: Record<string, { total: number; successful: number }> = {};
    
    for (const record of outreachRecords) {
      const date = record[OUTREACH_RECORD_FIELDS.DATE_CREATED]?.split('T')[0];
      if (!date) continue;
      
      if (!dateGroups[date]) {
        dateGroups[date] = { total: 0, successful: 0 };
      }
      
      dateGroups[date].total++;
      if (record[OUTREACH_RECORD_FIELDS.OUTREACH_STATUS] === 'Complete') {
        dateGroups[date].successful++;
      }
    }
    
    // Fill missing dates with zero values
    const trendData: PCOutreachTrendData[] = [];
    for (let i = 0; i < days; i++) {
      const date = new Date(Date.now() - i * 24 * 60 * 60 * 1000).toISOString().split('T')[0];
      const dayData = dateGroups[date] || { total: 0, successful: 0 };
      const responseRate = dayData.total > 0 ? (dayData.successful / dayData.total) * 100 : 0;
      
      trendData.unshift({
        date,
        outreachCount: dayData.total,
        successfulCount: dayData.successful,
        responseRate
      });
    }

    logQuickbaseResponse('getPCOutreachTrend', { recordCount: trendData.length }, Date.now() - startTime, reqId);
    return trendData;

  } catch (error) {
    logQuickbaseError('getPCOutreachTrend', error, reqId);
    throw error;
  }
}

/**
 * Get PC stage distribution data
 */
export async function getPCStageDistribution(
  pcEmail: string,
  pcName: string,
  reqId: string
): Promise<PCStageDistribution[]> {
  const startTime = Date.now();
  
  try {
    logQuickbaseRequest('getPCStageDistribution', { pcEmail, pcName }, reqId);

    const projectsWhere = `{${PROJECT_FIELDS.PROJECT_COORDINATOR_EMAIL}.EX.'${pcEmail}'}AND{${PROJECT_FIELDS.PROJECT_STATUS}.EX.'Active'}`;

    const projectsResponse = await qbClient.queryRecords({
      from: QB_TABLE_PROJECTS,
      where: projectsWhere,
      select: [
        PROJECT_FIELDS.RECORD_ID,
        PROJECT_FIELDS.PROJECT_AGE,
        PROJECT_FIELDS.SURVEY_DATE,
        PROJECT_FIELDS.DESIGN_DATE,
        PROJECT_FIELDS.PERMIT_DATE,
        PROJECT_FIELDS.NEM_DATE,
        PROJECT_FIELDS.INSTALL_DATE,
        PROJECT_FIELDS.PTO_DATE
      ]
    });

    const projects = projectsResponse.data || [];
    
    // Determine current stage for each project
    const stageCounts: Record<string, { count: number; totalDays: number }> = {};
    
    for (const project of projects) {
      let currentStage = 'Intake';
      const projectAge = Number(project[PROJECT_FIELDS.PROJECT_AGE]) || 0;
      
      // Determine stage based on milestone dates
      if (project[PROJECT_FIELDS.PTO_DATE]) {
        currentStage = 'PTO';
      } else if (project[PROJECT_FIELDS.INSTALL_DATE]) {
        currentStage = 'Install';
      } else if (project[PROJECT_FIELDS.NEM_DATE]) {
        currentStage = 'NEM';
      } else if (project[PROJECT_FIELDS.PERMIT_DATE]) {
        currentStage = 'Permit';
      } else if (project[PROJECT_FIELDS.DESIGN_DATE]) {
        currentStage = 'Design';
      } else if (project[PROJECT_FIELDS.SURVEY_DATE]) {
        currentStage = 'Survey';
      }
      
      if (!stageCounts[currentStage]) {
        stageCounts[currentStage] = { count: 0, totalDays: 0 };
      }
      
      stageCounts[currentStage].count++;
      stageCounts[currentStage].totalDays += projectAge;
    }
    
    const totalProjects = projects.length;
    const distribution: PCStageDistribution[] = [];
    
    for (const [stageName, data] of Object.entries(stageCounts)) {
      const avgDaysInStage = data.count > 0 ? data.totalDays / data.count : 0;
      const percentage = totalProjects > 0 ? (data.count / totalProjects) * 100 : 0;
      
      distribution.push({
        stageName,
        projectCount: data.count,
        avgDaysInStage,
        percentage
      });
    }
    
    // Sort by typical stage order
    const stageOrder = ['Intake', 'Survey', 'Design', 'Permit', 'NEM', 'Install', 'PTO'];
    distribution.sort((a, b) => stageOrder.indexOf(a.stageName) - stageOrder.indexOf(b.stageName));

    logQuickbaseResponse('getPCStageDistribution', { stageCount: distribution.length }, Date.now() - startTime, reqId);
    return distribution;

  } catch (error) {
    logQuickbaseError('getPCStageDistribution', error, reqId);
    throw error;
  }
}

/**
 * Get PC response breakdown data
 */
export async function getPCResponseBreakdown(
  pcEmail: string,
  pcName: string,
  timeRange: '7days' | '30days' | '90days' | 'all',
  reqId: string
): Promise<PCResponseBreakdown[]> {
  const startTime = Date.now();
  
  try {
    logQuickbaseRequest('getPCResponseBreakdown', { pcEmail, pcName, timeRange }, reqId);

    const { startDate, endDate } = calculateDateRange(timeRange);

    const outreachWhere = `{${OUTREACH_RECORD_FIELDS.PC_EMAIL}}.EX.'${pcEmail}' AND {${OUTREACH_RECORD_FIELDS.DATE_CREATED}}.GTE.'${startDate}' AND {${OUTREACH_RECORD_FIELDS.DATE_CREATED}}.LTE.'${endDate}'`;

    const outreachResponse = await qbClient.queryRecords({
      from: QB_TABLE_OUTREACH_RECORDS,
      where: outreachWhere,
      select: [
        OUTREACH_RECORD_FIELDS.OUTREACH_STATUS,
        OUTREACH_RECORD_FIELDS.RECORD_ID
      ]
    });

    const outreachRecords = outreachResponse.data || [];
    
    // Group by status
    const statusCounts: Record<string, number> = {};
    for (const record of outreachRecords) {
      const status = record[OUTREACH_RECORD_FIELDS.OUTREACH_STATUS] || 'Unknown';
      statusCounts[status] = (statusCounts[status] || 0) + 1;
    }
    
    const totalRecords = outreachRecords.length;
    const breakdown: PCResponseBreakdown[] = [];
    
    for (const [status, count] of Object.entries(statusCounts)) {
      const percentage = totalRecords > 0 ? (count / totalRecords) * 100 : 0;
      breakdown.push({
        status,
        count,
        percentage
      });
    }
    
    // Sort by count descending
    breakdown.sort((a, b) => b.count - a.count);

    logQuickbaseResponse('getPCResponseBreakdown', { statusCount: breakdown.length }, Date.now() - startTime, reqId);
    return breakdown;

  } catch (error) {
    logQuickbaseError('getPCResponseBreakdown', error, reqId);
    throw error;
  }
}

/**
 * Get PC project velocity metrics
 */
async function getPCProjectVelocity(reqId: string): Promise<PCProjectVelocity[]> {
  try {
    // Query all active projects for milestone durations
    const projectsResponse = await qbClient.queryRecords({
      from: QB_TABLE_PROJECTS,
      where: `{${PROJECT_FIELDS.PROJECT_STATUS}.EX.'Active'}`,
      select: [
        PROJECT_FIELDS.SALES_DATE,
        PROJECT_FIELDS.SURVEY_SUBMITTED,
        PROJECT_FIELDS.SURVEY_APPROVED,
        PROJECT_FIELDS.DESIGN_COMPLETED,
        PROJECT_FIELDS.INSTALL_COMPLETED_DATE,
        PROJECT_FIELDS.PTO_COMPLETED
      ]
    });

    const projects = projectsResponse.data || [];
    const milestoneDurations: Record<string, number[]> = {
      'Survey': [],
      'Design': [],
      'Install': [],
      'PTO': []
    };

    for (const project of projects) {
      const salesDate = project[PROJECT_FIELDS.SALES_DATE];
      const surveySubmitted = project[PROJECT_FIELDS.SURVEY_SUBMITTED];
      const surveyApproved = project[PROJECT_FIELDS.SURVEY_APPROVED];
      const designCompleted = project[PROJECT_FIELDS.DESIGN_COMPLETED];
      const installCompleted = project[PROJECT_FIELDS.INSTALL_COMPLETED_DATE];
      const ptoCompleted = project[PROJECT_FIELDS.PTO_COMPLETED];

      // Calculate milestone durations
      if (salesDate && surveySubmitted) {
        const duration = (new Date(surveySubmitted).getTime() - new Date(salesDate).getTime()) / (1000 * 60 * 60 * 24);
        milestoneDurations['Survey'].push(duration);
      }

      if (surveyApproved && designCompleted) {
        const duration = (new Date(designCompleted).getTime() - new Date(surveyApproved).getTime()) / (1000 * 60 * 60 * 24);
        milestoneDurations['Design'].push(duration);
      }

      if (designCompleted && installCompleted) {
        const duration = (new Date(installCompleted).getTime() - new Date(designCompleted).getTime()) / (1000 * 60 * 60 * 24);
        milestoneDurations['Install'].push(duration);
      }

      if (installCompleted && ptoCompleted) {
        const duration = (new Date(ptoCompleted).getTime() - new Date(installCompleted).getTime()) / (1000 * 60 * 60 * 24);
        milestoneDurations['PTO'].push(duration);
      }
    }

    // Calculate averages and medians
    const velocity: PCProjectVelocity[] = [];
    for (const [milestone, durations] of Object.entries(milestoneDurations)) {
      if (durations.length > 0) {
        const avgDays = durations.reduce((sum, d) => sum + d, 0) / durations.length;
        const sortedDurations = [...durations].sort((a, b) => a - b);
        const medianDays = sortedDurations[Math.floor(sortedDurations.length / 2)];
        
        velocity.push({
          milestoneName: milestone,
          avgDays,
          medianDays,
          projectCount: durations.length
        });
      }
    }

    return velocity;
  } catch (error) {
    logQuickbaseError('getPCProjectVelocity', error, reqId);
    return [];
  }
}

/**
 * Identify PC bottlenecks
 */
async function identifyPCBottlenecks(reqId: string): Promise<PCBottleneck[]> {
  try {
    // Query projects by current stage and days in stage
    const projectsResponse = await qbClient.queryRecords({
      from: QB_TABLE_PROJECTS,
      where: `{${PROJECT_FIELDS.PROJECT_STATUS}}.EX.'Active'`,
      select: [
        PROJECT_FIELDS.PROJECT_ID,
        PROJECT_FIELDS.CUSTOMER_NAME,
        PROJECT_FIELDS.PROJECT_STAGE,
        PROJECT_FIELDS.SALES_DATE,
        PROJECT_FIELDS.SURVEY_SUBMITTED,
        PROJECT_FIELDS.SURVEY_APPROVED,
        PROJECT_FIELDS.DESIGN_COMPLETED,
        PROJECT_FIELDS.INSTALL_COMPLETED_DATE
      ]
    });

    const projects = projectsResponse.data || [];
    const stageGroups: Record<string, any[]> = {};

    // Group projects by stage and calculate days in stage
    for (const project of projects) {
      const stage = project[PROJECT_FIELDS.PROJECT_STAGE] || 'Unknown';
      const salesDate = project[PROJECT_FIELDS.SALES_DATE];
      
      if (!stageGroups[stage]) {
        stageGroups[stage] = [];
      }

      // Calculate days in current stage (simplified)
      let daysInStage = 0;
      if (salesDate) {
        const now = new Date();
        const salesDateObj = new Date(salesDate);
        daysInStage = (now.getTime() - salesDateObj.getTime()) / (1000 * 60 * 60 * 24);
      }

      stageGroups[stage].push({
        ...project,
        daysInStage
      });
    }

    // Identify bottlenecks
    const bottlenecks: PCBottleneck[] = [];
    for (const [stage, stageProjects] of Object.entries(stageGroups)) {
      if (stageProjects.length > 0) {
        const avgDaysInStage = stageProjects.reduce((sum, p) => sum + p.daysInStage, 0) / stageProjects.length;
        const longestProject = stageProjects.reduce((longest, current) => 
          current.daysInStage > longest.daysInStage ? current : longest
        );

        let severity: 'critical' | 'high' | 'normal' = 'normal';
        if (avgDaysInStage > 60 || stageProjects.length > 20) severity = 'critical';
        else if (avgDaysInStage > 30 || stageProjects.length > 10) severity = 'high';

        bottlenecks.push({
          stageName: stage,
          projectCount: stageProjects.length,
          avgDaysInStage,
          longestProject: {
            projectId: longestProject[PROJECT_FIELDS.PROJECT_ID],
            customerName: longestProject[PROJECT_FIELDS.CUSTOMER_NAME],
            daysInStage: longestProject.daysInStage
          },
          severity
        });
      }
    }

    return bottlenecks.sort((a, b) => b.avgDaysInStage - a.avgDaysInStage);
  } catch (error) {
    logQuickbaseError('identifyPCBottlenecks', error, reqId);
    return [];
  }
}

/**
 * Get PC hold analysis
 */
async function getPCHoldAnalysis(reqId: string): Promise<PCHoldAnalysis[]> {
  try {
    // Query projects on hold
    const projectsResponse = await qbClient.queryRecords({
      from: QB_TABLE_PROJECTS,
      where: `{${PROJECT_FIELDS.PROJECT_STATUS}}.EX.'On Hold'`,
      select: [
        PROJECT_FIELDS.PROJECT_ID,
        PROJECT_FIELDS.HOLD_REASON,
        PROJECT_FIELDS.HOLD_DATE,
        PROJECT_FIELDS.HOLD_RESOLVED_DATE
      ]
    });

    const projects = projectsResponse.data || [];
    const holdCategories: Record<string, any[]> = {};

    // Group by hold reason
    for (const project of projects) {
      const reason = project[PROJECT_FIELDS.HOLD_REASON] || 'Unknown';
      if (!holdCategories[reason]) {
        holdCategories[reason] = [];
      }
      holdCategories[reason].push(project);
    }

    const totalHolds = projects.length;
    const analysis: PCHoldAnalysis[] = [];

    for (const [category, categoryProjects] of Object.entries(holdCategories)) {
      const resolvedProjects = categoryProjects.filter(p => p[PROJECT_FIELDS.HOLD_RESOLVED_DATE]);
      const resolvedCount = resolvedProjects.length;
      
      // Calculate average resolution days
      let avgResolutionDays: number | null = null;
      if (resolvedProjects.length > 0) {
        const resolutionDays = resolvedProjects.map(p => {
          const holdDate = new Date(p[PROJECT_FIELDS.HOLD_DATE]);
          const resolvedDate = new Date(p[PROJECT_FIELDS.HOLD_RESOLVED_DATE]);
          return (resolvedDate.getTime() - holdDate.getTime()) / (1000 * 60 * 60 * 24);
        });
        avgResolutionDays = resolutionDays.reduce((sum, days) => sum + days, 0) / resolutionDays.length;
      }

      analysis.push({
        category,
        count: categoryProjects.length,
        percentage: totalHolds > 0 ? (categoryProjects.length / totalHolds) * 100 : 0,
        avgResolutionDays,
        resolvedCount
      });
    }

    return analysis.sort((a, b) => b.count - a.count);
  } catch (error) {
    logQuickbaseError('getPCHoldAnalysis', error, reqId);
    return [];
  }
}

/**
 * Get PC team metrics (operations_manager only)
 */
export async function getPCTeamMetrics(
  timeRange: '7days' | '30days' | '90days' | 'all',
  reqId: string
): Promise<PCTeamMetrics> {
  const startTime = Date.now();
  
  try {
    logQuickbaseRequest('getPCTeamMetrics', { timeRange }, reqId);

    // Get all PCs from Projects table
    const projectsResponse = await qbClient.queryRecords({
      from: QB_TABLE_PROJECTS,
      where: `{${PROJECT_FIELDS.PROJECT_COORDINATOR_EMAIL}}.XEX.''`,
      select: [
        PROJECT_FIELDS.PROJECT_COORDINATOR_EMAIL,
        PROJECT_FIELDS.PROJECT_COORDINATOR_NAME
      ]
    });

    const projects = projectsResponse.data || [];
    const uniquePCs = new Map<string, string>();
    
    for (const project of projects) {
      const email = project[PROJECT_FIELDS.PROJECT_COORDINATOR_EMAIL];
      const name = project[PROJECT_FIELDS.PROJECT_COORDINATOR_NAME];
      if (email && name) {
        uniquePCs.set(email, name);
      }
    }
    
    // Get performance metrics for each PC
    const coordinators: PCCoordinatorPerformance[] = [];
    let totalResponseRate = 0;
    let totalTimeToContact = 0;
    let totalSLACompliance = 0;
    let validPCs = 0;
    
    for (const [email, name] of uniquePCs) {
      try {
        const personalMetrics = await getPCPersonalMetrics(email, name, timeRange, reqId);
        
        coordinators.push({
          coordinatorName: name,
          coordinatorEmail: email,
          totalProjects: personalMetrics.totalProjects,
          dailyOutreach: personalMetrics.dailyOutreach,
          responseRate: personalMetrics.responseRate,
          avgTimeToContact: personalMetrics.avgTimeToContact,
          escalationRate: personalMetrics.escalationRate,
          slaCompliance: personalMetrics.slaCompliance,
          projectsCompleted: 0, // Would need additional query to calculate
          avgProjectVelocity: 0 // Would need additional query to calculate
        });
        
        totalResponseRate += personalMetrics.responseRate;
        totalTimeToContact += personalMetrics.avgTimeToContact;
        totalSLACompliance += personalMetrics.slaCompliance;
        validPCs++;
      } catch (error) {
        // Skip PCs with errors
        continue;
      }
    }
    
    // Calculate team averages
    const teamAverages = {
      responseRate: validPCs > 0 ? totalResponseRate / validPCs : 0,
      avgTimeToContact: validPCs > 0 ? totalTimeToContact / validPCs : 0,
      slaCompliance: validPCs > 0 ? totalSLACompliance / validPCs : 0
    };
    
    // Get project velocity, bottlenecks, and hold analysis
    const projectVelocity = await getPCProjectVelocity(reqId);
    const bottlenecks = await identifyPCBottlenecks(reqId);
    const holdAnalysis = await getPCHoldAnalysis(reqId);

    const teamMetrics: PCTeamMetrics = {
      coordinators,
      projectVelocity,
      bottlenecks,
      holdAnalysis,
      teamAverages
    };

    logQuickbaseResponse('getPCTeamMetrics', { coordinatorCount: coordinators.length }, Date.now() - startTime, reqId);
    return teamMetrics;

  } catch (error) {
    logQuickbaseError('getPCTeamMetrics', error, reqId);
    throw error;
  }
}

// =============================================================================
// PC CALENDAR QUERIES
// =============================================================================

/**
 * Get PC calendar events for a date range
 * Fetches scheduled installs, surveys, and outreach from Projects and Outreach Records tables
 */
export async function getPCCalendarEvents(
  pcEmail: string,
  pcName: string,
  startDate: string,
  endDate: string,
  reqId: string
): Promise<PCCalendarEvent[]> {
  try {
    logQuickbaseRequest('getPCCalendarEvents', { pcEmail, pcName, startDate, endDate }, reqId);

    // Sanitize input parameters
    const sanitizedEmail = sanitizeQbLiteral(pcEmail);
    const sanitizedName = sanitizeQbLiteral(pcName);

    // Validate date range
    const start = new Date(startDate);
    const end = new Date(endDate);
    const daysDiff = Math.ceil((end.getTime() - start.getTime()) / (1000 * 60 * 60 * 24));
    
    if (daysDiff > 90) {
      throw new Error('Date range cannot exceed 90 days');
    }

    if (start >= end) {
      throw new Error('startDate must be before endDate');
    }

    const events: PCCalendarEvent[] = [];
    const now = new Date();

    // Query Projects table for scheduled installs and surveys
    const projectsQuery = {
      from: QB_TABLE_PROJECTS,
      select: [
        FIELD_IDS.PROJECTS.RECORD_ID,
        FIELD_IDS.PROJECTS.PROJECT_ID,
        FIELD_IDS.PROJECTS.CUSTOMER_NAME,
        FIELD_IDS.PROJECTS.CUSTOMER_PHONE,
        FIELD_IDS.PROJECTS.INSTALL_SCHEDULED_DATE,
        FIELD_IDS.PROJECTS.SURVEY_SCHEDULED_DATE,
        FIELD_IDS.PROJECTS.PROJECT_STATUS,
        FIELD_IDS.PROJECTS.PROJECT_COORDINATOR_EMAIL
      ],
      where: `{${FIELD_IDS.PROJECTS.PROJECT_COORDINATOR_EMAIL}.EX.'${sanitizedEmail}'}AND({${FIELD_IDS.PROJECTS.INSTALL_SCHEDULED_DATE}.AF.'${startDate}'}OR{${FIELD_IDS.PROJECTS.SURVEY_SCHEDULED_DATE}.AF.'${startDate}'})AND({${FIELD_IDS.PROJECTS.INSTALL_SCHEDULED_DATE}.BF.'${endDate}'}OR{${FIELD_IDS.PROJECTS.SURVEY_SCHEDULED_DATE}.BF.'${endDate}'})`
    };

    const projectsResponse = await qbClient.queryRecords(projectsQuery);
    const projects = projectsResponse.data || [];

    // Transform project events
    for (const project of projects) {
      const recordId = project[FIELD_IDS.PROJECTS.RECORD_ID];
      const projectId = project[FIELD_IDS.PROJECTS.PROJECT_ID];
      const customerName = project[FIELD_IDS.PROJECTS.CUSTOMER_NAME];
      const customerPhone = project[FIELD_IDS.PROJECTS.CUSTOMER_PHONE];
      const projectStatus = project[FIELD_IDS.PROJECTS.PROJECT_STATUS];
      const installDate = project[FIELD_IDS.PROJECTS.INSTALL_SCHEDULED_DATE];
      const surveyDate = project[FIELD_IDS.PROJECTS.SURVEY_SCHEDULED_DATE];

      // Create install event if scheduled
      if (installDate) {
        const start = new Date(installDate);
        const end = new Date(start);
        end.setHours(end.getHours() + 4); // 4-hour install window

        const status = projectStatus === 'Completed' ? 'completed' : 
                     start < now ? 'overdue' : 'scheduled';

        events.push({
          id: `install-${recordId}`,
          title: `Install - ${customerName}`,
          start,
          end,
          allDay: false,
          type: 'install',
          projectId,
          recordId,
          customerName,
          customerPhone,
          status,
          priority: 'high',
          coordinatorEmail: pcEmail,
          metadata: { projectStatus }
        });
      }

      // Create survey event if scheduled
      if (surveyDate) {
        const start = new Date(surveyDate);
        const end = new Date(start);
        end.setHours(end.getHours() + 2); // 2-hour survey window

        const status = projectStatus === 'Completed' ? 'completed' : 
                     start < now ? 'overdue' : 'scheduled';

        events.push({
          id: `survey-${recordId}`,
          title: `Survey - ${customerName}`,
          start,
          end,
          allDay: false,
          type: 'survey',
          projectId,
          recordId,
          customerName,
          customerPhone,
          status,
          priority: 'normal',
          coordinatorEmail: pcEmail,
          metadata: { projectStatus }
        });
      }
    }

    // Query Outreach Records for upcoming outreach
    const outreachQuery = {
      from: QB_TABLE_OUTREACH_RECORDS,
      select: [
        FIELD_IDS.OUTREACH.RECORD_ID,
        FIELD_IDS.OUTREACH.RELATED_PROJECT,
        FIELD_IDS.OUTREACH.NEXT_OUTREACH_DUE_DATE,
        FIELD_IDS.OUTREACH.OUTREACH_STATUS
      ],
      where: `{${FIELD_IDS.OUTREACH.PC_NAME}.EX.'${sanitizedName}'}AND{${FIELD_IDS.OUTREACH.NEXT_OUTREACH_DUE_DATE}.AF.'${startDate}'}AND{${FIELD_IDS.OUTREACH.NEXT_OUTREACH_DUE_DATE}.BF.'${endDate}'}`
    };

    const outreachResponse = await qbClient.queryRecords(outreachQuery);
    const outreachRecords = outreachResponse.data || [];

    // Transform outreach events
    for (const record of outreachRecords) {
      const recordId = record[FIELD_IDS.OUTREACH.RECORD_ID];
      const projectId = record[FIELD_IDS.OUTREACH.RELATED_PROJECT];
      const dueDate = record[FIELD_IDS.OUTREACH.NEXT_OUTREACH_DUE_DATE];
      const outreachStatus = record[FIELD_IDS.OUTREACH.OUTREACH_STATUS];

      if (dueDate) {
        const start = new Date(dueDate);
        const end = new Date(start);
        end.setHours(end.getHours() + 1); // 1-hour outreach window

        const status = outreachStatus === 'Complete' ? 'completed' : 
                     start < now ? 'overdue' : 'scheduled';

        events.push({
          id: `outreach-${recordId}`,
          title: `Outreach Due`,
          start,
          end,
          allDay: true,
          type: 'outreach',
          projectId,
          recordId,
          customerName: 'Customer',
          customerPhone: '',
          status,
          priority: 'normal',
          coordinatorEmail: pcEmail,
          metadata: { outreachStatus }
        });
      }
    }

    // Sort events by start date
    events.sort((a, b) => a.start.getTime() - b.start.getTime());

    logQuickbaseResponse('getPCCalendarEvents', { eventCount: events.length }, reqId);
    return events;

  } catch (error) {
    logQuickbaseError('getPCCalendarEvents', error, reqId);
    throw error;
  }
}
