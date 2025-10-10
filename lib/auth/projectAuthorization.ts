/**
 * ⚠️ CENTRALIZED PROJECT AUTHORIZATION MODULE
 *
 * This module defines HOW users are authorized to access projects.
 *
 * IMPORTANT: All project access checks must use these functions.
 * If you need to change authorization logic, update this file ONLY.
 *
 * Current Strategy: EMAIL-BASED authorization
 * - Projects are matched to users by email comparison
 * - Field 518 = CLOSER_EMAIL
 * - Field 331 = SETTER_EMAIL
 * - Users see projects where their email matches closer OR setter email
 *
 * Authorization Hierarchy:
 * 1. super_admin & regional: See ALL projects
 * 2. office_leader/area_director/divisional: See all projects in their office(s)
 * 3. team_lead: See projects for their managed users (by email)
 * 4. closer/setter: See only projects where their email matches
 */

import type { Session } from 'next-auth';
import {
  getUserEmail,
  getUserRole,
  isAdmin,
  isOfficeLeader,
  isRep,
} from './userIdentity';
import { PROJECT_FIELDS } from '@/lib/constants/fieldIds';

/**
 * Check if a user can access a specific project based on email matching
 * @param session - User session
 * @param project - Project data from QuickBase
 * @returns True if user has access
 */
export function canUserAccessProject(
  session: Session,
  project: Record<string, any>
): boolean {
  // Admins see everything
  if (isAdmin(session)) {
    return true;
  }

  const role = getUserRole(session);

  // Office-based roles see everything (office filtering happens at query level)
  if (['office_leader', 'area_director', 'divisional'].includes(role)) {
    return true;
  }

  // Team leads see their managed users' projects (checked at query level)
  if (role === 'team_lead') {
    return true;
  }

  // Reps see only their projects (email match)
  if (isRep(session)) {
    const userEmail = getUserEmail(session);
    if (!userEmail) {
      return false; // No email = no access for reps
    }

    const closerEmail = project[PROJECT_FIELDS.CLOSER_EMAIL]?.value?.toLowerCase() || '';
    const setterEmail = project[PROJECT_FIELDS.SETTER_EMAIL]?.value?.toLowerCase() || '';

    return userEmail === closerEmail || userEmail === setterEmail;
  }

  // Default deny
  return false;
}

/**
 * Check if user should have access based on role alone (without project data)
 * Used for optimization - admins don't need project lookup
 * @param session - User session
 * @returns True if user has blanket access
 */
export function hasUnrestrictedAccess(session: Session): boolean {
  const role = getUserRole(session);
  return (
    isAdmin(session) ||
    ['office_leader', 'area_director', 'divisional', 'team_lead'].includes(role)
  );
}

/**
 * Build QuickBase WHERE clause for filtering projects by user access
 * This is the SINGLE SOURCE OF TRUTH for project authorization logic.
 *
 * @param userEmail - User's email (from database, not session directly)
 * @param role - User's role
 * @param salesOffice - Assigned offices for office-based roles
 * @param managedEmails - Managed user emails for team leads
 * @returns QuickBase WHERE clause string
 */
export function buildProjectAccessClause(
  userEmail: string | null,
  role: string,
  salesOffice?: string[],
  managedEmails?: string[]
): string {
  console.log('[buildProjectAccessClause] Building clause for:', {
    userEmail,
    role,
    salesOffice,
    managedEmails,
  });

  // Helper to build email-based clause for closer/setter fields
  const buildEmailClause = (fieldId: number, emails: string[]) => {
    if (emails.length === 0) {
      return '{3.EQ.0}'; // No email = no projects
    }
    if (emails.length === 1) {
      // Sanitize email for QB query (escape single quotes)
      const sanitizedEmail = emails[0].replace(/'/g, "''");
      return `{${fieldId}.EX.'${sanitizedEmail}'}`;
    }
    // Multiple emails joined with OR
    return emails
      .map((email) => {
        const sanitizedEmail = email.replace(/'/g, "''");
        return `{${fieldId}.EX.'${sanitizedEmail}'}`;
      })
      .join(' OR ');
  };

  const buildOfficeClause = (offices: string[]) => {
    if (offices.length === 1) {
      return `{${PROJECT_FIELDS.SALES_OFFICE}.EX.'${offices[0]}'}`;
    }
    // Multiple offices joined with OR
    return offices
      .map((office) => `{${PROJECT_FIELDS.SALES_OFFICE}.EX.'${office}'}`)
      .join(' OR ');
  };

  let clause: string;
  switch (role) {
    case 'super_admin':
    case 'regional':
      // These roles see all projects, no user filter
      clause = '{3.GT.0}'; // Record ID > 0 (matches all records)
      console.log('[buildProjectAccessClause] Admin role detected, returning all-projects clause:', clause);
      break;
    case 'office_leader':
    case 'area_director':
    case 'divisional':
      // Office-based roles see ALL projects in their assigned offices
      // This filters by project.sales_office, NOT by user.is_active
      // This means managers see projects even if the closer/setter doesn't have an active account
      if (salesOffice && salesOffice.length > 0) {
        clause = buildOfficeClause(salesOffice);
        console.log(
          `[buildProjectAccessClause] ${role} role, filtering by offices (office-based visibility):`,
          clause
        );
      } else {
        // Fallback to least-privilege: restrict to no projects if no office assigned
        clause = '{3.EQ.0}'; // Record ID = 0 (matches no records)
        console.log(
          `[buildProjectAccessClause] ${role} with no office assigned, returning no-projects clause:`,
          clause
        );
      }
      break;
    case 'team_lead':
      // Team leads see projects for their managed users (email-based visibility)
      if (managedEmails && managedEmails.length > 0) {
        // Build clause that matches projects where closer_email OR setter_email is in managed users
        const closerClause = buildEmailClause(PROJECT_FIELDS.CLOSER_EMAIL, managedEmails);
        const setterClause = buildEmailClause(PROJECT_FIELDS.SETTER_EMAIL, managedEmails);
        clause = `(${closerClause}) OR (${setterClause})`;
        console.log(
          '[buildProjectAccessClause] Team lead role, filtering by managed user emails (email-based visibility):',
          clause
        );
      } else {
        // No managed users, see no projects
        clause = '{3.EQ.0}'; // Record ID = 0 (matches no records)
        console.log(
          '[buildProjectAccessClause] Team lead with no managed users, returning no-projects clause:',
          clause
        );
      }
      break;
    case 'closer':
    case 'setter':
      // Show projects where user email matches CLOSER_EMAIL OR SETTER_EMAIL
      // This automatically includes ALL projects across all QB User IDs for this email
      if (!userEmail) {
        clause = '{3.EQ.0}'; // No email = no projects
        console.log('[buildProjectAccessClause] Rep role with no email, returning no-projects clause');
      } else {
        const emails = [userEmail];
        const closerClauseRep = buildEmailClause(PROJECT_FIELDS.CLOSER_EMAIL, emails);
        const setterClauseRep = buildEmailClause(PROJECT_FIELDS.SETTER_EMAIL, emails);
        clause = `(${closerClauseRep}) OR (${setterClauseRep})`;
        console.log(
          '[buildProjectAccessClause] Rep role (closer/setter), showing projects matching email (all QB User IDs):',
          clause
        );
      }
      break;
    case 'coordinator':
      // Coordinators still use ID-based filtering (no email field available)
      // Keep old behavior for coordinators until email field is added
      if (!userEmail) {
        clause = '{3.EQ.0}';
        console.log('[buildProjectAccessClause] Coordinator with no email, returning no-projects clause');
      } else {
        // For coordinators, we can't use email, so we need to get QB User ID
        // This is a limitation - coordinators won't benefit from email-based filtering yet
        console.warn('[buildProjectAccessClause] Coordinator role does not support email-based filtering yet');
        clause = '{3.EQ.0}'; // No projects for now
      }
      break;
    default:
      if (!userEmail) {
        clause = '{3.EQ.0}';
      } else {
        const emails = [userEmail];
        clause = buildEmailClause(PROJECT_FIELDS.CLOSER_EMAIL, emails);
      }
      console.log('[buildProjectAccessClause] Unknown role, defaulting to closer email filter:', clause);
      break;
  }

  return clause;
}

/**
 * Get list of emails that a user can see projects for
 * Used for database queries
 * @param session - User session
 * @returns Array of emails user can access
 */
export function getUserAuthorizedEmails(session: Session): string[] {
  // Reps can only see their own email
  if (isRep(session)) {
    const email = getUserEmail(session);
    return email ? [email] : [];
  }

  // Admins/office leaders can see all - return empty array to indicate "all"
  if (hasUnrestrictedAccess(session)) {
    return [];
  }

  return [];
}

/**
 * Check if user can perform administrative actions on a project
 * (e.g., delete, bulk edit, etc.)
 * @param session - User session
 * @returns True if user can perform admin actions
 */
export function canUserAdministerProjects(session: Session): boolean {
  return isAdmin(session);
}
