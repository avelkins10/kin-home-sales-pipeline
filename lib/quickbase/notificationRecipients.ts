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
        PROJECT_FIELDS.CLOSER_NAME,
        PROJECT_FIELDS.CLOSER_EMAIL,
        PROJECT_FIELDS.SETTER_NAME,
        PROJECT_FIELDS.SETTER_EMAIL,
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
      closerName: project[PROJECT_FIELDS.CLOSER_NAME]?.value,
      closerEmail: project[PROJECT_FIELDS.CLOSER_EMAIL]?.value,
      setterName: project[PROJECT_FIELDS.SETTER_NAME]?.value,
      setterEmail: project[PROJECT_FIELDS.SETTER_EMAIL]?.value,
      coordinator: project[PROJECT_FIELDS.PROJECT_COORDINATOR]?.value,
      office: salesOffice,
    });

    // 2. Add project closer (required) - use EMAIL field
    const closerEmail = project[PROJECT_FIELDS.CLOSER_EMAIL]?.value;
    const closerName = project[PROJECT_FIELDS.CLOSER_NAME]?.value;

    if (closerEmail && typeof closerEmail === 'string') {
      recipients.push({
        userId: closerEmail.toLowerCase(),
        userName: closerName || closerEmail,
        role: 'closer',
      });
    }

    // 3. Add project setter (if assigned) - use EMAIL field
    const setterEmail = project[PROJECT_FIELDS.SETTER_EMAIL]?.value;
    const setterName = project[PROJECT_FIELDS.SETTER_NAME]?.value;

    if (setterEmail && typeof setterEmail === 'string') {
      recipients.push({
        userId: setterEmail.toLowerCase(),
        userName: setterName || setterEmail,
        role: 'setter',
      });
    }

    // 4. Add project coordinator (if assigned)
    // Note: Coordinator email field not yet available in QuickBase, skip for now
    const coordinatorName = project[PROJECT_FIELDS.PROJECT_COORDINATOR]?.value;
    // TODO: Add coordinator email field once available in QuickBase

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
