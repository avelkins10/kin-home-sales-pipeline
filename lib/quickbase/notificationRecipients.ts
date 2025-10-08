import 'server-only';
// Server-only module. Do not import from client components.
export const __isServerOnly = true as const;

import { qbClient } from './client';
import { sql } from '@vercel/postgres';
import { PROJECT_FIELDS } from '@/lib/constants/fieldIds';
import { logError } from '@/lib/logging/logger';

/**
 * Recipient information for notifications
 */
export interface NotificationRecipient {
  userId: string;       // User email or QB ID
  userName: string;     // Display name
  role: string;         // User role (closer, setter, coordinator, office_leader, admin)
  salesOffice?: string; // Office assignment (for office leaders)
}

/**
 * Get all users who should receive notifications for a project.
 *
 * Recipients include:
 * - Project closer (always)
 * - Project setter (if assigned)
 * - Project coordinator (if assigned)
 * - Office leaders (for the project's sales office)
 * - Admins (super_admin, regional roles)
 *
 * @param projectId - QuickBase project record ID
 * @returns Array of recipients with user info
 */
export async function getNotificationRecipientsForProject(
  projectId: number
): Promise<NotificationRecipient[]> {
  const recipients: NotificationRecipient[] = [];

  try {
    // 1. Fetch project details from QuickBase
    const QB_TABLE_PROJECTS = process.env.QUICKBASE_TABLE_PROJECTS || 'br9kwm8na';

    console.log('[notificationRecipients] Fetching project team for project:', projectId);

    const result = await qbClient.queryRecords({
      from: QB_TABLE_PROJECTS,
      select: [
        PROJECT_FIELDS.RECORD_ID,
        PROJECT_FIELDS.CLOSER_ID,
        PROJECT_FIELDS.CLOSER_NAME,
        PROJECT_FIELDS.SETTER_ID,
        PROJECT_FIELDS.SETTER_NAME,
        PROJECT_FIELDS.PROJECT_COORDINATOR_ID,
        PROJECT_FIELDS.PROJECT_COORDINATOR,
        PROJECT_FIELDS.SALES_OFFICE,
      ],
      where: `{${PROJECT_FIELDS.RECORD_ID}.EX.${projectId}}`,
    });

    if (!result.data || result.data.length === 0) {
      console.warn('[notificationRecipients] Project not found:', projectId);
      return recipients;
    }

    const project = result.data[0];
    const salesOffice = project[PROJECT_FIELDS.SALES_OFFICE]?.value;

    console.log('[notificationRecipients] Project team:', {
      closer: project[PROJECT_FIELDS.CLOSER_NAME]?.value,
      setter: project[PROJECT_FIELDS.SETTER_NAME]?.value,
      coordinator: project[PROJECT_FIELDS.PROJECT_COORDINATOR]?.value,
      office: salesOffice,
    });

    // 2. Add project closer (required)
    const closerId = project[PROJECT_FIELDS.CLOSER_ID]?.value;
    const closerName = project[PROJECT_FIELDS.CLOSER_NAME]?.value;

    if (closerId && closerId.email) {
      recipients.push({
        userId: closerId.email,
        userName: closerName || closerId.name || closerId.email,
        role: 'closer',
      });
    } else if (closerId && closerId.id) {
      recipients.push({
        userId: closerId.id,
        userName: closerName || closerId.name || closerId.id,
        role: 'closer',
      });
    }

    // 3. Add project setter (if assigned)
    const setterId = project[PROJECT_FIELDS.SETTER_ID]?.value;
    const setterName = project[PROJECT_FIELDS.SETTER_NAME]?.value;

    if (setterId && setterId.email) {
      recipients.push({
        userId: setterId.email,
        userName: setterName || setterId.name || setterId.email,
        role: 'setter',
      });
    } else if (setterId && setterId.id) {
      recipients.push({
        userId: setterId.id,
        userName: setterName || setterId.name || setterId.id,
        role: 'setter',
      });
    }

    // 4. Add project coordinator (if assigned)
    const coordinatorId = project[PROJECT_FIELDS.PROJECT_COORDINATOR_ID]?.value;
    const coordinatorName = project[PROJECT_FIELDS.PROJECT_COORDINATOR]?.value;

    if (coordinatorId && coordinatorId.email) {
      recipients.push({
        userId: coordinatorId.email,
        userName: coordinatorName || coordinatorId.name || coordinatorId.email,
        role: 'coordinator',
      });
    } else if (coordinatorId && coordinatorId.id) {
      recipients.push({
        userId: coordinatorId.id,
        userName: coordinatorName || coordinatorId.name || coordinatorId.id,
        role: 'coordinator',
      });
    }

    // 5. Add office leaders for this project's office
    if (salesOffice) {
      const officeLeaders = await getOfficeLeaders(salesOffice);
      recipients.push(...officeLeaders);
    }

    // 6. Add admins (super_admin and regional roles)
    const admins = await getAdmins();
    recipients.push(...admins);

    // 7. Deduplicate recipients by userId
    const uniqueRecipients = Array.from(
      new Map(recipients.map(r => [r.userId, r])).values()
    );

    console.log('[notificationRecipients] Final recipients:', {
      total: uniqueRecipients.length,
      roles: uniqueRecipients.map(r => r.role),
    });

    return uniqueRecipients;

  } catch (error) {
    logError('Failed to get notification recipients', error as Error, { projectId });
    return recipients; // Return empty array on error
  }
}

/**
 * Get office leaders for a specific sales office from the database
 */
async function getOfficeLeaders(salesOffice: string): Promise<NotificationRecipient[]> {
  try {
    const result = await sql<{
      email: string;
      name: string;
      sales_office: string[];
    }>`
      SELECT email, name, sales_office
      FROM users
      WHERE role = 'office_leader'
        AND ${salesOffice} = ANY(sales_office)
    `;

    return result.rows.map(user => ({
      userId: user.email,
      userName: user.name,
      role: 'office_leader',
      salesOffice: user.sales_office.join(', '),
    }));
  } catch (error) {
    logError('Failed to get office leaders', error as Error, { salesOffice });
    return [];
  }
}

/**
 * Get all admin users from the database
 */
async function getAdmins(): Promise<NotificationRecipient[]> {
  try {
    const result = await sql<{
      email: string;
      name: string;
      role: string;
    }>`
      SELECT email, name, role
      FROM users
      WHERE role IN ('super_admin', 'regional')
    `;

    return result.rows.map(user => ({
      userId: user.email,
      userName: user.name,
      role: user.role,
    }));
  } catch (error) {
    logError('Failed to get admins', error as Error);
    return [];
  }
}
