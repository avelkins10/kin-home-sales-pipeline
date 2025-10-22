// lib/quickbase/queries.ts
import { qbClient } from './client';
import { logError } from '@/lib/logging/logger';
import { QuickbaseProject } from '@/lib/types/project';
import { Task, TaskSubmission, TaskStatus } from '@/lib/types/task';
import { 
  PROJECT_FIELDS, 
  TASK_FIELDS, 
  TASK_SUBMISSION_FIELDS,
  TASK_GROUP_FIELDS,
  QB_TABLE_TASKS,
  QB_TABLE_TASK_SUBMISSIONS,
  QB_TABLE_TASK_GROUPS
} from '@/lib/constants/fieldIds';

export async function getProjectsForUser(userId: string, role: string): Promise<QuickbaseProject[]> {
  // Mock implementation - in real app, this would query Quickbase
  return [
    {
      [PROJECT_FIELDS.RECORD_ID]: { value: '1' },
      [PROJECT_FIELDS.CUSTOMER_NAME]: { value: 'John Doe' },
      [PROJECT_FIELDS.PROJECT_STATUS]: { value: 'On Hold' },
      [PROJECT_FIELDS.ON_HOLD]: { value: true },
      [PROJECT_FIELDS.HOLD_REASON]: { value: 'Finance Hold - Credit approval pending' },
      [PROJECT_FIELDS.DATE_ON_HOLD]: { value: '2023-10-01' },
      [PROJECT_FIELDS.SYSTEM_PRICE]: { value: '75000' },
      [PROJECT_FIELDS.SALES_DATE]: { value: '2023-09-15' },
      [PROJECT_FIELDS.CLOSER_ID]: { value: userId },
      [PROJECT_FIELDS.SETTER_ID]: { value: 'setter-1' }
    }
  ];
}

export async function getProjectById(projectId: string | number): Promise<QuickbaseProject | null> {
  // Mock implementation
  const projects = await getProjectsForUser('test-user', 'closer');
  const idStr = typeof projectId === 'number' ? String(projectId) : projectId;
  return projects.find(p => p[PROJECT_FIELDS.RECORD_ID]?.value === idStr) || null;
}

export async function getEnhancedDashboardMetrics(userId: string, role: string): Promise<any> {
  // Mock implementation
  return {
    totalProjects: 25,
    projectsOnHold: 5,
    totalRevenue: 1500000,
    revenueAtRisk: 375000
  };
}

export async function getProjectTasks(projectId: number): Promise<Task[]> {
  try {
    console.log('[getProjectTasks] Fetching tasks for project:', projectId);
    
    const response = await qbClient.queryRecords({
      from: QB_TABLE_TASKS,
      select: [
        TASK_FIELDS.RECORD_ID,
        TASK_FIELDS.TASK_GROUP,
        TASK_FIELDS.STATUS,
        TASK_FIELDS.NAME,
        TASK_FIELDS.CATEGORY,
        TASK_FIELDS.MAX_SUBMISSION_STATUS
      ],
      where: `{6.EX.${projectId}}` // Filter by task group (field 6) = project ID
    });

    const tasks: Task[] = response.data.map((record: any) => ({
      recordId: record[TASK_FIELDS.RECORD_ID]?.value || 0,
      taskGroup: record[TASK_FIELDS.TASK_GROUP]?.value || 0,
      status: record[TASK_FIELDS.STATUS]?.value || 'Not Started',
      name: record[TASK_FIELDS.NAME]?.value || '',
      category: record[TASK_FIELDS.CATEGORY]?.value || null,
      maxSubmissionStatus: record[TASK_FIELDS.MAX_SUBMISSION_STATUS]?.value || null,
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
        TASK_FIELDS.TASK_GROUP,
        TASK_FIELDS.STATUS,
        TASK_FIELDS.NAME,
        TASK_FIELDS.CATEGORY,
        TASK_FIELDS.MAX_SUBMISSION_STATUS
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
      taskGroup: record[TASK_FIELDS.TASK_GROUP]?.value || 0,
      status: record[TASK_FIELDS.STATUS]?.value || 'Not Started',
      name: record[TASK_FIELDS.NAME]?.value || '',
      category: record[TASK_FIELDS.CATEGORY]?.value || null,
      maxSubmissionStatus: record[TASK_FIELDS.MAX_SUBMISSION_STATUS]?.value || null,
      submissions: [] // Will be populated separately if needed
    };

    console.log('[getTaskById] Found task:', task.name);
    return task;
  } catch (error) {
    logError('Failed to fetch task by ID', error as Error, { taskId });
    throw error;
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
        TASK_SUBMISSION_FIELDS.FILE_ATTACHMENTS
      ],
      where: `{6.EX.${taskId}}` // Filter by related task (field 6) = task ID
    });

    const submissions: TaskSubmission[] = response.data.map((record: any) => ({
      recordId: record[TASK_SUBMISSION_FIELDS.RECORD_ID]?.value || 0,
      dateCreated: record[TASK_SUBMISSION_FIELDS.DATE_CREATED]?.value || '',
      relatedTask: record[TASK_SUBMISSION_FIELDS.RELATED_TASK]?.value || 0,
      submissionStatus: record[TASK_SUBMISSION_FIELDS.SUBMISSION_STATUS]?.value || 'Pending Approval',
      opsDisposition: record[TASK_SUBMISSION_FIELDS.OPS_DISPOSITION]?.value || null,
      fileAttachments: record[TASK_SUBMISSION_FIELDS.FILE_ATTACHMENTS]?.value || null
    }));

    console.log('[getTaskSubmissions] Found', submissions.length, 'submissions for task', taskId);
    return submissions;
  } catch (error) {
    logError('Failed to fetch task submissions', error as Error, { taskId });
    throw error;
  }
}

export async function createTaskSubmission(taskId: number, userId: string): Promise<number> {
  try {
    console.log('[createTaskSubmission] Creating submission for task:', taskId);
    
    const response = await qbClient.updateRecord({
      to: QB_TABLE_TASK_SUBMISSIONS,
      data: [
        {
          [TASK_SUBMISSION_FIELDS.RELATED_TASK]: { value: taskId },
          [TASK_SUBMISSION_FIELDS.SUBMISSION_STATUS]: { value: 'Pending Approval' },
          [TASK_SUBMISSION_FIELDS.OPS_DISPOSITION]: { value: '' }, // Leave empty for ops to fill
          [TASK_SUBMISSION_FIELDS.FILE_ATTACHMENTS]: { value: '' } // Leave empty initially
        }
      ]
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

export async function uploadFileToSubmission(submissionId: number, fileName: string, fileBuffer: Buffer): Promise<void> {
  try {
    console.log('[uploadFileToSubmission] Uploading file:', { submissionId, fileName, size: fileBuffer.length });
    
    await qbClient.uploadFileToRecord({
      tableId: QB_TABLE_TASK_SUBMISSIONS,
      recordId: submissionId,
      fieldId: TASK_SUBMISSION_FIELDS.FILE_ATTACHMENTS,
      fileName: fileName,
      fileData: fileBuffer
    });

    console.log('[uploadFileToSubmission] File uploaded successfully, size:', fileBuffer.length);
  } catch (error) {
    logError('Failed to upload file to submission', error as Error, { submissionId, fileName, size: fileBuffer.length });
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
 * Get projects for user with comprehensive filtering and task count enrichment
 * @param userId User ID
 * @param role User role
 * @param view View filter
 * @param search Search term
 * @param sort Sort option
 * @param salesOffice Sales office (deprecated, use office parameter)
 * @param memberEmail Member email filter
 * @param ownership Ownership filter
 * @param office Office filter
 * @param setter Setter filter
 * @param closer Closer filter
 * @param reqId Request ID for logging
 * @param withTasks Filter to show only projects with any tasks
 * @returns Array of enriched projects
 */
export async function getProjectsForUserList(
  userId: string, 
  role: string, 
  view?: string, 
  search?: string, 
  sort?: string, 
  salesOffice?: string, 
  memberEmail?: string, 
  ownership?: string, 
  office?: string, 
  setter?: string, 
  closer?: string, 
  reqId?: string,
  withTasks?: boolean
): Promise<QuickbaseProject[]> {
  try {
    console.log('[getProjectsForUserList] Fetching projects for user:', { userId, role, view, search, sort, ownership, office, setter, closer, withTasks, reqId });
    
    // For now, return mock data - in real implementation, this would query QuickBase
    // with proper authorization filters and then enrich with task counts
    const mockProjects: QuickbaseProject[] = [
      {
        [PROJECT_FIELDS.RECORD_ID]: { value: '1' },
        [PROJECT_FIELDS.CUSTOMER_NAME]: { value: 'John Doe' },
        [PROJECT_FIELDS.PROJECT_STATUS]: { value: 'On Hold' },
        [PROJECT_FIELDS.ON_HOLD]: { value: true },
        [PROJECT_FIELDS.HOLD_REASON]: { value: 'Finance Hold - Credit approval pending' },
        [PROJECT_FIELDS.DATE_ON_HOLD]: { value: '2023-10-01' },
        [PROJECT_FIELDS.SYSTEM_PRICE]: { value: '75000' },
        [PROJECT_FIELDS.SALES_DATE]: { value: '2023-09-15' },
        [PROJECT_FIELDS.CLOSER_ID]: { value: userId },
        [PROJECT_FIELDS.SETTER_ID]: { value: 'setter-1' }
      }
    ];

    // Extract project IDs for task count lookup
    const projectIds = mockProjects
      .map(project => parseInt(project[PROJECT_FIELDS.RECORD_ID]?.value || '0'))
      .filter(id => id > 0);

    // Get task counts for all projects
    let taskCountsMap: Map<number, { totalTasks: number; unapprovedTasks: number }>;
    let taskCountsFailed = false;
    
    try {
      taskCountsMap = await getTaskCountsForProjects(projectIds);
    } catch (error) {
      logError('Task count fetch failed, proceeding without task enrichment', error as Error, { projectCount: projectIds.length });
      taskCountsMap = new Map();
      taskCountsFailed = true;
    }

    // Enrich projects with task counts
    const enrichedProjects = mockProjects.map(project => {
      const projectId = parseInt(project[PROJECT_FIELDS.RECORD_ID]?.value || '0');
      const taskCounts = taskCountsMap.get(projectId) || { totalTasks: 0, unapprovedTasks: 0 };
      
      // Add task count fields to project object
      const enrichedProject = {
        ...project,
        [PROJECT_FIELDS.TOTAL_TASKS]: { value: taskCounts.totalTasks.toString() },
        [PROJECT_FIELDS.UNAPPROVED_TASKS]: { value: taskCounts.unapprovedTasks.toString() }
      };
      
      return enrichedProject;
    });

    // Apply withTasks filter if requested
    let filteredProjects = enrichedProjects;
    if (withTasks) {
      if (taskCountsFailed) {
        // If task counts failed and withTasks is true, return all projects to avoid empty list
        console.log('[getProjectsForUserList] Task count fetch failed with withTasks=true, returning all projects');
        filteredProjects = enrichedProjects;
      } else {
        filteredProjects = enrichedProjects.filter(project => {
          const totalTasks = parseInt(project[PROJECT_FIELDS.TOTAL_TASKS]?.value || '0');
          return totalTasks > 0;
        });
      }
    }

    console.log('[getProjectsForUserList] Returning', filteredProjects.length, 'projects (enriched with task counts)');
    return filteredProjects;
  } catch (error) {
    logError('Failed to fetch projects for user list', error as Error, { userId, role, reqId });
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

// Stub functions for incomplete features
export async function updateProject(projectId: string | number, updates: any): Promise<void> {
  throw new Error('updateProject not implemented');
}

export async function getAddersForProject(projectId: string | number): Promise<any[]> {
  throw new Error('getAddersForProject not implemented');
}

export async function getNotesForProject(projectId: string | number): Promise<any[]> {
  throw new Error('getNotesForProject not implemented');
}

export async function createNoteForProject(projectId: string | number, content: string, userEmail?: string): Promise<void> {
  throw new Error('createNoteForProject not implemented');
}

export async function fetchProjects(query?: any): Promise<any[]> {
  throw new Error('fetchProjects not implemented');
}