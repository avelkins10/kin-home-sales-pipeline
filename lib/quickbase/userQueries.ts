// lib/quickbase/userQueries.ts
import 'server-only'
// Server-only module. Do not import from client components.
export const __isServerOnly = true as const

import { qbClient } from './client';
import { logError } from '@/lib/logging/logger';
import { PROJECT_FIELDS } from '@/lib/constants/fieldIds';
import { QuickBaseUserData } from '@/lib/types/user';

// Quickbase table IDs
const QB_TABLE_PROJECTS = process.env.QUICKBASE_TABLE_PROJECTS || 'br9kwm8na';

/**
 * Get active users from QuickBase based on recent project activity
 * This is the recommended approach to avoid creating thousands of inactive users
 * 
 * @param monthsBack - Number of months to look back for activity (default: 6)
 * @returns Array of active users with their data and project counts
 */
export async function getActiveUsersFromQuickbase(monthsBack: number = 6): Promise<QuickBaseUserData[]> {
  console.log('[getActiveUsersFromQuickbase] Fetching active users from last', monthsBack, 'months');
  
  try {
    // Calculate date threshold
    const thresholdDate = new Date();
    thresholdDate.setMonth(thresholdDate.getMonth() - monthsBack);
    const dateString = thresholdDate.toISOString().split('T')[0];
    
    // Query projects table for unique closers with projects in last N months
    const closerQuery = await qbClient.queryRecords({
      from: QB_TABLE_PROJECTS,
      select: [
        PROJECT_FIELDS.CLOSER_ID,
        PROJECT_FIELDS.CLOSER_NAME,
        PROJECT_FIELDS.CLOSER_EMAIL,
        PROJECT_FIELDS.CLOSER_PHONE,
        PROJECT_FIELDS.SALES_OFFICE,
        PROJECT_FIELDS.SALES_DATE,
      ],
      where: `{${PROJECT_FIELDS.SALES_DATE}.AF.'${dateString}'} AND {${PROJECT_FIELDS.CLOSER_ID}.XEX.''}`,
    });

    // Query projects table for unique setters with projects in last N months
    const setterQuery = await qbClient.queryRecords({
      from: QB_TABLE_PROJECTS,
      select: [
        PROJECT_FIELDS.SETTER_ID,
        PROJECT_FIELDS.SETTER_NAME,
        PROJECT_FIELDS.SETTER_EMAIL,
        PROJECT_FIELDS.SETTER_PHONE,
        PROJECT_FIELDS.SALES_OFFICE,
        PROJECT_FIELDS.SALES_DATE,
      ],
      where: `{${PROJECT_FIELDS.SALES_DATE}.AF.'${dateString}'} AND {${PROJECT_FIELDS.SETTER_ID}.XEX.''}`,
    });

    // Build map of unique users with their data and most recent project date
    const userMap = new Map<string, QuickBaseUserData>();
    
    // Process closer data
    closerQuery.data?.forEach((project: any) => {
      const closerId = project[PROJECT_FIELDS.CLOSER_ID]?.value;
      const closerName = project[PROJECT_FIELDS.CLOSER_NAME]?.value;
      const closerEmail = project[PROJECT_FIELDS.CLOSER_EMAIL]?.value;
      const closerPhone = project[PROJECT_FIELDS.CLOSER_PHONE]?.value;
      const office = project[PROJECT_FIELDS.SALES_OFFICE]?.value;
      const salesDate = project[PROJECT_FIELDS.SALES_DATE]?.value;
      
      if (closerId && closerName) {
        const existing = userMap.get(closerId);
        if (!existing || (salesDate && salesDate > (existing.lastProjectDate || ''))) {
          userMap.set(closerId, {
            quickbaseUserId: closerId,
            name: closerName,
            email: closerEmail,
            phone: closerPhone,
            role: 'closer',
            office,
            lastProjectDate: salesDate,
            projectCount: (existing?.projectCount || 0) + 1,
          });
        } else if (existing) {
          existing.projectCount = (existing.projectCount || 0) + 1;
        }
      }
    });

    // Process setter data
    setterQuery.data?.forEach((project: any) => {
      const setterId = project[PROJECT_FIELDS.SETTER_ID]?.value;
      const setterName = project[PROJECT_FIELDS.SETTER_NAME]?.value;
      const setterEmail = project[PROJECT_FIELDS.SETTER_EMAIL]?.value;
      const setterPhone = project[PROJECT_FIELDS.SETTER_PHONE]?.value;
      const office = project[PROJECT_FIELDS.SALES_OFFICE]?.value;
      const salesDate = project[PROJECT_FIELDS.SALES_DATE]?.value;
      
      if (setterId && setterName) {
        const existing = userMap.get(setterId);
        if (!existing || (salesDate && salesDate > (existing.lastProjectDate || ''))) {
          userMap.set(setterId, {
            quickbaseUserId: setterId,
            name: setterName,
            email: setterEmail,
            phone: setterPhone,
            role: 'setter',
            office,
            lastProjectDate: salesDate,
            projectCount: (existing?.projectCount || 0) + 1,
          });
        } else if (existing) {
          existing.projectCount = (existing.projectCount || 0) + 1;
        }
      }
    });

    const activeUsers = Array.from(userMap.values());
    console.log('[getActiveUsersFromQuickbase] Found', activeUsers.length, 'active users');
    
    return activeUsers;
  } catch (error) {
    console.error('[getActiveUsersFromQuickbase] Error:', error);
    logError('Failed to fetch active users from QuickBase', error as Error);
    return [];
  }
}

/**
 * Get user data by QuickBase ID
 * 
 * @param quickbaseUserId - QuickBase user ID to look up
 * @returns User data or null if not found
 */
export async function getUserByQuickbaseId(quickbaseUserId: string): Promise<QuickBaseUserData | null> {
  console.log('[getUserByQuickbaseId] Looking up user:', quickbaseUserId);
  
  try {
    // Query projects table for any project with this closer or setter ID
    const closerQuery = await qbClient.queryRecords({
      from: QB_TABLE_PROJECTS,
      select: [
        PROJECT_FIELDS.CLOSER_ID,
        PROJECT_FIELDS.CLOSER_NAME,
        PROJECT_FIELDS.CLOSER_EMAIL,
        PROJECT_FIELDS.CLOSER_PHONE,
        PROJECT_FIELDS.SALES_OFFICE,
        PROJECT_FIELDS.SALES_DATE,
      ],
      where: `{${PROJECT_FIELDS.CLOSER_ID}.CT.'${quickbaseUserId}'}`,
    });

    const setterQuery = await qbClient.queryRecords({
      from: QB_TABLE_PROJECTS,
      select: [
        PROJECT_FIELDS.SETTER_ID,
        PROJECT_FIELDS.SETTER_NAME,
        PROJECT_FIELDS.SETTER_EMAIL,
        PROJECT_FIELDS.SETTER_PHONE,
        PROJECT_FIELDS.SALES_OFFICE,
        PROJECT_FIELDS.SALES_DATE,
      ],
      where: `{${PROJECT_FIELDS.SETTER_ID}.CT.'${quickbaseUserId}'}`,
    });

    // Check if user is a closer
    if (closerQuery.data && closerQuery.data.length > 0) {
      const project = closerQuery.data[0];
      return {
        quickbaseUserId,
        name: project[PROJECT_FIELDS.CLOSER_NAME]?.value,
        email: project[PROJECT_FIELDS.CLOSER_EMAIL]?.value,
        phone: project[PROJECT_FIELDS.CLOSER_PHONE]?.value,
        role: 'closer',
        office: project[PROJECT_FIELDS.SALES_OFFICE]?.value,
        lastProjectDate: project[PROJECT_FIELDS.SALES_DATE]?.value,
        projectCount: closerQuery.data.length,
      };
    }

    // Check if user is a setter
    if (setterQuery.data && setterQuery.data.length > 0) {
      const project = setterQuery.data[0];
      return {
        quickbaseUserId,
        name: project[PROJECT_FIELDS.SETTER_NAME]?.value,
        email: project[PROJECT_FIELDS.SETTER_EMAIL]?.value,
        phone: project[PROJECT_FIELDS.SETTER_PHONE]?.value,
        role: 'setter',
        office: project[PROJECT_FIELDS.SALES_OFFICE]?.value,
        lastProjectDate: project[PROJECT_FIELDS.SALES_DATE]?.value,
        projectCount: setterQuery.data.length,
      };
    }

    console.log('[getUserByQuickbaseId] User not found:', quickbaseUserId);
    return null;
  } catch (error) {
    console.error('[getUserByQuickbaseId] Error:', error);
    logError('Failed to fetch user by QuickBase ID', error as Error);
    return null;
  }
}

/**
 * Search QuickBase users by name or email
 * 
 * @param searchTerm - Search term (name or email)
 * @returns Array of matching users
 */
export async function searchQuickbaseUsers(searchTerm: string): Promise<QuickBaseUserData[]> {
  console.log('[searchQuickbaseUsers] Searching for:', searchTerm);
  
  try {
    const sanitizedTerm = searchTerm.replace(/'/g, "''").substring(0, 100);
    
    // Search closers
    const closerQuery = await qbClient.queryRecords({
      from: QB_TABLE_PROJECTS,
      select: [
        PROJECT_FIELDS.CLOSER_ID,
        PROJECT_FIELDS.CLOSER_NAME,
        PROJECT_FIELDS.CLOSER_EMAIL,
        PROJECT_FIELDS.CLOSER_PHONE,
        PROJECT_FIELDS.SALES_OFFICE,
        PROJECT_FIELDS.SALES_DATE,
      ],
      where: `({${PROJECT_FIELDS.CLOSER_NAME}.CT.'${sanitizedTerm}'} OR {${PROJECT_FIELDS.CLOSER_EMAIL}.CT.'${sanitizedTerm}'}) AND {${PROJECT_FIELDS.CLOSER_ID}.XEX.''}`,
      options: { top: 25 }, // Limit results
    });

    // Search setters
    const setterQuery = await qbClient.queryRecords({
      from: QB_TABLE_PROJECTS,
      select: [
        PROJECT_FIELDS.SETTER_ID,
        PROJECT_FIELDS.SETTER_NAME,
        PROJECT_FIELDS.SETTER_EMAIL,
        PROJECT_FIELDS.SETTER_PHONE,
        PROJECT_FIELDS.SALES_OFFICE,
        PROJECT_FIELDS.SALES_DATE,
      ],
      where: `({${PROJECT_FIELDS.SETTER_NAME}.CT.'${sanitizedTerm}'} OR {${PROJECT_FIELDS.SETTER_EMAIL}.CT.'${sanitizedTerm}'}) AND {${PROJECT_FIELDS.SETTER_ID}.XEX.''}`,
      options: { top: 25 }, // Limit results
    });

    // Build unique user map
    const userMap = new Map<string, QuickBaseUserData>();
    
    // Process closer results
    closerQuery.data?.forEach((project: any) => {
      const closerId = project[PROJECT_FIELDS.CLOSER_ID]?.value;
      const closerName = project[PROJECT_FIELDS.CLOSER_NAME]?.value;
      const closerEmail = project[PROJECT_FIELDS.CLOSER_EMAIL]?.value;
      const closerPhone = project[PROJECT_FIELDS.CLOSER_PHONE]?.value;
      const office = project[PROJECT_FIELDS.SALES_OFFICE]?.value;
      const salesDate = project[PROJECT_FIELDS.SALES_DATE]?.value;
      
      if (closerId && closerName) {
        const existing = userMap.get(closerId);
        if (!existing || (salesDate && salesDate > (existing.lastProjectDate || ''))) {
          userMap.set(closerId, {
            quickbaseUserId: closerId,
            name: closerName,
            email: closerEmail,
            phone: closerPhone,
            role: 'closer',
            office,
            lastProjectDate: salesDate,
            projectCount: (existing?.projectCount || 0) + 1,
          });
        } else if (existing) {
          existing.projectCount = (existing.projectCount || 0) + 1;
        }
      }
    });

    // Process setter results
    setterQuery.data?.forEach((project: any) => {
      const setterId = project[PROJECT_FIELDS.SETTER_ID]?.value;
      const setterName = project[PROJECT_FIELDS.SETTER_NAME]?.value;
      const setterEmail = project[PROJECT_FIELDS.SETTER_EMAIL]?.value;
      const setterPhone = project[PROJECT_FIELDS.SETTER_PHONE]?.value;
      const office = project[PROJECT_FIELDS.SALES_OFFICE]?.value;
      const salesDate = project[PROJECT_FIELDS.SALES_DATE]?.value;
      
      if (setterId && setterName) {
        const existing = userMap.get(setterId);
        if (!existing || (salesDate && salesDate > (existing.lastProjectDate || ''))) {
          userMap.set(setterId, {
            quickbaseUserId: setterId,
            name: setterName,
            email: setterEmail,
            phone: setterPhone,
            role: 'setter',
            office,
            lastProjectDate: salesDate,
            projectCount: (existing?.projectCount || 0) + 1,
          });
        } else if (existing) {
          existing.projectCount = (existing.projectCount || 0) + 1;
        }
      }
    });

    const results = Array.from(userMap.values()).slice(0, 50); // Limit to 50 results
    console.log('[searchQuickbaseUsers] Found', results.length, 'matching users');
    
    return results;
  } catch (error) {
    console.error('[searchQuickbaseUsers] Error:', error);
    logError('Failed to search QuickBase users', error as Error);
    return [];
  }
}

/**
 * Get user project activity statistics
 * 
 * @param quickbaseUserId - QuickBase user ID
 * @returns Activity statistics
 */
export async function getUserProjectActivity(quickbaseUserId: string): Promise<{
  totalProjects: number;
  activeProjects: number;
  lastProjectDate?: string;
  offices: string[];
}> {
  console.log('[getUserProjectActivity] Getting activity for user:', quickbaseUserId);
  
  try {
    // Query all projects where user is closer
    const closerQuery = await qbClient.queryRecords({
      from: QB_TABLE_PROJECTS,
      select: [
        PROJECT_FIELDS.SALES_DATE,
        PROJECT_FIELDS.PROJECT_STATUS,
        PROJECT_FIELDS.SALES_OFFICE,
      ],
      where: `{${PROJECT_FIELDS.CLOSER_ID}.CT.'${quickbaseUserId}'}`,
    });

    // Query all projects where user is setter
    const setterQuery = await qbClient.queryRecords({
      from: QB_TABLE_PROJECTS,
      select: [
        PROJECT_FIELDS.SALES_DATE,
        PROJECT_FIELDS.PROJECT_STATUS,
        PROJECT_FIELDS.SALES_OFFICE,
      ],
      where: `{${PROJECT_FIELDS.SETTER_ID}.CT.'${quickbaseUserId}'}`,
    });

    const allProjects = [...(closerQuery.data || []), ...(setterQuery.data || [])];
    const totalProjects = allProjects.length;
    
    // Calculate active projects (last 6 months)
    const sixMonthsAgo = new Date();
    sixMonthsAgo.setMonth(sixMonthsAgo.getMonth() - 6);
    const dateString = sixMonthsAgo.toISOString().split('T')[0];
    
    const activeProjects = allProjects.filter((project: any) => {
      const salesDate = project[PROJECT_FIELDS.SALES_DATE]?.value;
      return salesDate && salesDate >= dateString;
    }).length;

    // Get most recent project date
    const lastProjectDate = allProjects
      .map((project: any) => project[PROJECT_FIELDS.SALES_DATE]?.value)
      .filter(Boolean)
      .sort()
      .pop();

    // Get unique offices
    const offices = Array.from(new Set(
      allProjects
        .map((project: any) => project[PROJECT_FIELDS.SALES_OFFICE]?.value)
        .filter(Boolean)
    ));

    console.log('[getUserProjectActivity] Activity stats:', {
      totalProjects,
      activeProjects,
      lastProjectDate,
      offices: offices.length,
    });

    return {
      totalProjects,
      activeProjects,
      lastProjectDate,
      offices,
    };
  } catch (error) {
    console.error('[getUserProjectActivity] Error:', error);
    logError('Failed to get user project activity', error as Error);
    return {
      totalProjects: 0,
      activeProjects: 0,
      offices: [],
    };
  }
}

/**
 * Helper function to build user data from project records
 * 
 * @param projects - Array of project records
 * @returns Array of unique users with aggregated data
 */
function buildUserDataFromProjects(projects: any[]): QuickBaseUserData[] {
  const userMap = new Map<string, QuickBaseUserData>();
  
  projects.forEach((project: any) => {
    // Process closer data
    const closerId = project[PROJECT_FIELDS.CLOSER_ID]?.value;
    const closerName = project[PROJECT_FIELDS.CLOSER_NAME]?.value;
    const closerEmail = project[PROJECT_FIELDS.CLOSER_EMAIL]?.value;
    const closerPhone = project[PROJECT_FIELDS.CLOSER_PHONE]?.value;
    const office = project[PROJECT_FIELDS.SALES_OFFICE]?.value;
    const salesDate = project[PROJECT_FIELDS.SALES_DATE]?.value;
    
    if (closerId && closerName) {
      const existing = userMap.get(closerId);
      if (!existing || (salesDate && salesDate > (existing.lastProjectDate || ''))) {
        userMap.set(closerId, {
          quickbaseUserId: closerId,
          name: closerName,
          email: closerEmail,
          phone: closerPhone,
          role: 'closer',
          office,
          lastProjectDate: salesDate,
          projectCount: (existing?.projectCount || 0) + 1,
        });
      } else if (existing) {
        existing.projectCount = (existing.projectCount || 0) + 1;
      }
    }

    // Process setter data
    const setterId = project[PROJECT_FIELDS.SETTER_ID]?.value;
    const setterName = project[PROJECT_FIELDS.SETTER_NAME]?.value;
    const setterEmail = project[PROJECT_FIELDS.SETTER_EMAIL]?.value;
    const setterPhone = project[PROJECT_FIELDS.SETTER_PHONE]?.value;
    
    if (setterId && setterName) {
      const existing = userMap.get(setterId);
      if (!existing || (salesDate && salesDate > (existing.lastProjectDate || ''))) {
        userMap.set(setterId, {
          quickbaseUserId: setterId,
          name: setterName,
          email: setterEmail,
          phone: setterPhone,
          role: 'setter',
          office,
          lastProjectDate: salesDate,
          projectCount: (existing?.projectCount || 0) + 1,
        });
      } else if (existing) {
        existing.projectCount = (existing.projectCount || 0) + 1;
      }
    }
  });

  return Array.from(userMap.values());
}
