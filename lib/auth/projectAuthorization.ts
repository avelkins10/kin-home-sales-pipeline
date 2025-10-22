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
import { logInfo, logWarn } from '@/lib/logging/logger';

/**
 * Helper function to mask email addresses for logging
 * Masks the username part while preserving the domain
 * Used for GDPR/privacy compliance in production logs
 * @param email - Email address to mask
 * @returns Masked email (e.g., "a***@domain.com")
 */
function maskEmail(email: string | null | undefined): string {
  if (!email || typeof email !== 'string') {
    return 'null';
  }
  
  const [username, domain] = email.split('@');
  if (!username || !domain) {
    return 'invalid-email';
  }
  
  if (username.length <= 2) {
    return `${username[0]}***@${domain}`;
  }
  
  return `${username[0]}***@${domain}`;
}

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
  if (['office_leader', 'regional', 'area_director', 'divisional'].includes(role)) {
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
    ['office_leader', 'regional', 'area_director', 'divisional', 'team_lead'].includes(role)
  );
}

/**
 * Build QuickBase WHERE clause for filtering projects by user access
 * This is the SINGLE SOURCE OF TRUTH for project authorization logic.
 *
 * @param userEmail - User's email (from database, not session directly)
 * @param role - User's role
 * @param officeIds - Assigned office IDs (QuickBase Record IDs from Field 810) for office-based roles
 * @param managedEmails - Managed user emails for team leads
 * @returns QuickBase WHERE clause string
 */
export function buildProjectAccessClause(
  userEmail: string | null,
  role: string,
  officeIds?: number[],
  managedEmails?: string[],
  reqId?: string
): string {
  logInfo('[PROJECT_AUTHORIZATION] Building access clause', {
    role,
    officeCount: officeIds?.length || 0,
    officeIds: officeIds,
    managedEmailCount: managedEmails?.length || 0,
    reqId
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

  const buildOfficeClause = (officeIds: number[]) => {
    if (officeIds.length === 0) {
      return '{3.EQ.0}'; // No offices = no projects
    }
    if (officeIds.length === 1) {
      // Use Field 810 (OFFICE_RECORD_ID) for stable ID-based filtering
      return `{${PROJECT_FIELDS.OFFICE_RECORD_ID}.EX.${officeIds[0]}}`;
    }
    // Multiple office IDs joined with OR
    return officeIds
      .map((officeId) => {
        return `{${PROJECT_FIELDS.OFFICE_RECORD_ID}.EX.${officeId}}`;
      })
      .join(' OR ');
  };

  let clause: string;
  switch (role) {
    case 'super_admin':
      // Super admin WITH office filter sees those specific offices
      // Super admin WITHOUT office filter sees all projects
      if (officeIds && officeIds.length > 0) {
        clause = buildOfficeClause(officeIds);
        logInfo('[PROJECT_AUTHORIZATION] Super admin with office filter', {
          role,
          officeIds: officeIds,
          officeCount: officeIds.length,
          reqId
        });
      } else {
        // No office filter = see all projects
        clause = '{3.GT.0}'; // Record ID > 0 (matches all records)
        logInfo('[PROJECT_AUTHORIZATION] Super admin with no office filter, granting all-projects access', { role, reqId });
      }
      break;
    case 'regional':
      // Regional managers WITH office assignments see those specific offices
      // Regional managers WITHOUT office assignments see all projects (legacy/unrestricted)
      if (officeIds && officeIds.length > 0) {
        clause = buildOfficeClause(officeIds);
        logInfo('[PROJECT_AUTHORIZATION] Regional manager with office assignments', {
          role,
          officeIds: officeIds,
          officeCount: officeIds.length,
          reqId
        });
      } else {
        // No office assignments = see all projects (backward compatibility)
        clause = '{3.GT.0}';
        logInfo('[PROJECT_AUTHORIZATION] Regional manager with no office assignments, granting all-projects access', { role, reqId });
      }
      break;
    case 'office_leader':
    case 'area_director':
    case 'divisional':
      // Office-based roles see ALL projects in their assigned offices
      // This filters by Field 810 (OFFICE_RECORD_ID), NOT by user.is_active
      // This means managers see projects even if the closer/setter doesn't have an active account
      // Uses stable QuickBase Record IDs that don't change when office names are updated
      if (officeIds && officeIds.length > 0) {
        clause = buildOfficeClause(officeIds);
        logInfo('[PROJECT_AUTHORIZATION] Office-based role with assigned offices', {
          role,
          officeIds: officeIds,
          officeCount: officeIds.length,
          clauseLength: clause.length
        });
      } else {
        // Fallback to least-privilege: restrict to no projects if no office assigned
        clause = '{3.EQ.0}'; // Record ID = 0 (matches no records)
        logWarn('[PROJECT_AUTHORIZATION] Office-based role with NO assigned offices', { role, userId: 'redacted', reqId });
      }
      break;
    case 'team_lead':
      // Team leads see projects for their managed users (email-based visibility)
      if (managedEmails && managedEmails.length > 0) {
        // Build clause that matches projects where closer_email OR setter_email is in managed users
        const closerClause = buildEmailClause(PROJECT_FIELDS.CLOSER_EMAIL, managedEmails);
        const setterClause = buildEmailClause(PROJECT_FIELDS.SETTER_EMAIL, managedEmails);
        clause = `(${closerClause}) OR (${setterClause})`;
        logInfo('[PROJECT_AUTHORIZATION] Team lead with managed users', { managedUserCount: managedEmails.length });
      } else {
        // No managed users, see no projects
        clause = '{3.EQ.0}'; // Record ID = 0 (matches no records)
        logWarn('[PROJECT_AUTHORIZATION] Team lead with NO managed users', { role, reqId });
      }
      break;
    case 'closer':
    case 'setter':
      // Show projects where user email matches CLOSER_EMAIL OR SETTER_EMAIL
      // This automatically includes ALL projects across all QB User IDs for this email
      if (!userEmail || userEmail.trim() === '') {
        clause = '{3.EQ.0}'; // No email = no projects
        logWarn('[PROJECT_AUTHORIZATION] Rep role with no email, denying access', { role, reqId });
      } else {
        const emails = [userEmail];
        const closerClauseRep = buildEmailClause(PROJECT_FIELDS.CLOSER_EMAIL, emails);
        const setterClauseRep = buildEmailClause(PROJECT_FIELDS.SETTER_EMAIL, emails);
        clause = `(${closerClauseRep}) OR (${setterClauseRep})`;
        logInfo('[PROJECT_AUTHORIZATION] Rep role with email, filtering by email', { role });
      }
      break;
    case 'coordinator':
      // Coordinator role has no project visibility by design
      // This role is intended for administrative tasks, not project management
      // If project access is needed, user should be assigned a different role
      logWarn('[PROJECT_AUTHORIZATION] Coordinator role has no project visibility by design', { role, reqId });
      clause = '{3.EQ.0}'; // No projects - explicit denial
      break;
    default:
      if (!userEmail || userEmail.trim() === '') {
        clause = '{3.EQ.0}';
      } else {
        const emails = [userEmail];
        clause = buildEmailClause(PROJECT_FIELDS.CLOSER_EMAIL, emails);
      }
      logInfo('[PROJECT_AUTHORIZATION] Unknown role, defaulting to closer email filter', { role });
      break;
  }

  // Validate clause for potential issues
  if (!clause || clause.trim() === '') {
    logWarn('[PROJECT_AUTHORIZATION] Generated empty clause, defaulting to no access', { role, reqId });
    return '{3.EQ.0}';
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
