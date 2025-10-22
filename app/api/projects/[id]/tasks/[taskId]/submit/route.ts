export const runtime = 'nodejs';

import { NextResponse } from 'next/server';
import { requireAuth, requireProjectAccessById } from '@/lib/auth/guards';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';
import { createTaskSubmission, uploadFileToSubmission, updateTaskStatus, getTaskSubmissions, createTaskNotification, getTaskById } from '@/lib/quickbase/queries';
import { getTaskRequirements } from '@/lib/utils/task-requirements';
import { TASK_SUBMISSION_FIELDS } from '@/lib/constants/fieldIds';

/**
 * POST /api/projects/[id]/tasks/[taskId]/submit
 * Submit a task with file upload
 */
export async function POST(req: Request, { params }: { params: { id: string; taskId: string } }) {
  const startedAt = Date.now();
  const reqId = req.headers.get('x-request-id') || Math.random().toString(36).slice(2, 10);
  logApiRequest('POST', '/api/projects/{id}/tasks/{taskId}/submit', undefined, reqId);

  try {
    // Authentication
    const auth = await requireAuth();
    if (!auth.authorized) {
      return auth.response;
    }

    // Parse and validate project ID and task ID
    const numericProjectId = parseInt(params.id, 10);
    const numericTaskId = parseInt(params.taskId, 10);
    
    if (isNaN(numericProjectId) || isNaN(numericTaskId)) {
      return NextResponse.json({ error: 'Invalid project ID or task ID' }, { status: 400 });
    }

    // Authorization - verify user can access this project
    const access = await requireProjectAccessById(numericProjectId);
    if (!access.authorized) {
      return access.response;
    }

    // Fetch task details to determine requirements
    let task;
    try {
      task = await getTaskById(numericTaskId);
      if (!task) {
        return NextResponse.json({ error: 'Task not found' }, { status: 404 });
      }
    } catch (taskFetchError) {
      console.error('[API] Failed to fetch task details:', taskFetchError);
      logError('Failed to fetch task for submission', taskFetchError as Error, {
        taskId: numericTaskId,
        projectId: numericProjectId
      });
      return NextResponse.json({
        error: 'Failed to load task details',
        details: (taskFetchError as Error).message
      }, { status: 500 });
    }

    // Calculate task requirements
    const taskRequirements = getTaskRequirements(task.name, task.category);
    console.log('[API] Task requirements:', {
      taskId: numericTaskId,
      taskName: task.name,
      requiresFile: taskRequirements.requiresFile,
      reason: taskRequirements.reason
    });

    // Parse multipart form-data
    const formData = await req.formData();
    const file1 = formData.get('file1') as File | null;
    const file2 = formData.get('file2') as File | null;
    const file3 = formData.get('file3') as File | null;
    const notes = formData.get('notes') as string | null;

    // Backwards compatibility - check for 'file' field
    const primaryFile = file1 || (formData.get('file') as File | null);

    // Validate file requirement
    if (taskRequirements.requiresFile && !primaryFile) {
      return NextResponse.json({
        error: 'File is required for this task',
        reason: taskRequirements.reason
      }, { status: 400 });
    }

    // Helper function to validate and convert file
    const validateAndConvertFile = async (file: File): Promise<Buffer> => {
      // Size validation (10 MB max, but warn if > 3 MB due to Base64 encoding)
      if (file.size > 10 * 1024 * 1024) {
        throw new Error(`File ${file.name} too large (max 10 MB)`);
      }

      // Base64-adjusted size limit check
      const base64AdjustedSize = file.size * 1.33;
      if (base64AdjustedSize > 10 * 1024 * 1024) {
        throw new Error(`File ${file.name} too large when Base64 encoded (max 10 MB)`);
      }

      if (file.size > 3 * 1024 * 1024) {
        console.warn('[API] Large file upload:', { fileName: file.name, size: file.size, 'Base64 size will be ~': Math.round(base64AdjustedSize) });
      }

      // Type validation
      const allowedMimeTypes = [
        'application/pdf',
        'image/jpeg',
        'image/png',
        'application/msword',
        'application/vnd.openxmlformats-officedocument.wordprocessingml.document'
      ];

      const allowedExtensions = ['.pdf', '.jpg', '.jpeg', '.png', '.doc', '.docx'];
      const fileExtension = file.name.toLowerCase().substring(file.name.lastIndexOf('.'));

      if (!allowedMimeTypes.includes(file.type) && !allowedExtensions.includes(fileExtension)) {
        throw new Error(`Invalid file type for ${file.name}. Allowed: PDF, JPG, PNG, DOC, DOCX`);
      }

      // Convert File to Buffer
      const arrayBuffer = await file.arrayBuffer();
      return Buffer.from(arrayBuffer);
    };

    // Validate and convert all provided files
    const files: Array<{ file: File; buffer: Buffer; fieldId: number }> = [];

    try {
      if (primaryFile) {
        files.push({
          file: primaryFile,
          buffer: await validateAndConvertFile(primaryFile),
          fieldId: TASK_SUBMISSION_FIELDS.FILE_ATTACHMENT_1
        });
      }
      if (file2) {
        files.push({
          file: file2,
          buffer: await validateAndConvertFile(file2),
          fieldId: TASK_SUBMISSION_FIELDS.FILE_ATTACHMENT_2
        });
      }
      if (file3) {
        files.push({
          file: file3,
          buffer: await validateAndConvertFile(file3),
          fieldId: TASK_SUBMISSION_FIELDS.FILE_ATTACHMENT_3
        });
      }
    } catch (validationError) {
      return NextResponse.json({
        error: (validationError as Error).message
      }, { status: 400 });
    }

    // Transaction flow
    try {
      // 1. Create submission record with notes
      const submissionId = await createTaskSubmission(
        numericTaskId,
        auth.session.user.email,
        notes || undefined
      );
      console.log('[API] Created submission:', submissionId);

      // Guard against missing submissionId
      if (!submissionId || typeof submissionId !== 'number' || submissionId <= 0) {
        throw new Error('Failed to create task submission - no valid submission ID returned');
      }

      // 2. Upload all files to their respective fields
      if (files.length > 0) {
        for (const { file, buffer, fieldId } of files) {
          await uploadFileToSubmission(submissionId, file.name, buffer, fieldId);
          console.log(`[API] Uploaded ${file.name} to field ${fieldId}`);
        }
        console.log(`[API] Successfully uploaded ${files.length} file(s)`);
      } else {
        console.log('[API] No files to upload (task does not require file)');
      }

      // 3. Update task status to "In Progress"
      await updateTaskStatus(numericTaskId, 'In Progress');
      console.log('[API] Task status updated to In Progress');

      // 4. Fetch updated submissions (optional, for immediate UI update)
      const updatedSubmissions = await getTaskSubmissions(numericTaskId);

      // 5. Create notification for task submission
      try {
        // Create notification (task already fetched earlier)
        await createTaskNotification({
          userId: auth.session.user.email,
          projectId: numericProjectId,
          taskId: numericTaskId,
          taskName: task.name,
          taskCategory: task.category || undefined,
          type: 'task_submitted',
          submissionId,
        });

        console.log('[API] Task submission notification created');
      } catch (notificationError) {
        // Log but don't fail the request if notification creation fails
        console.error('[API] Failed to create task notification:', notificationError);
      }

      // Response
      const duration = Date.now() - startedAt;
      logApiResponse('POST', '/api/projects/{id}/tasks/{taskId}/submit', duration, {
        submissionId,
        fileCount: files.length,
        fileNames: files.map(f => f.file.name).join(', ') || null,
        totalFileSize: files.reduce((sum, f) => sum + f.file.size, 0),
        hadNotes: !!notes,
        requiresFile: taskRequirements.requiresFile
      }, reqId);

      return NextResponse.json({
        success: true,
        submissionId,
        message: 'Task submission created successfully'
      }, { status: 201 });

    } catch (transactionError) {
      logError('Failed to submit task', transactionError as Error, { 
        projectId: params.id, 
        taskId: params.taskId 
      });

      // Return appropriate error response
      const errorMessage = (transactionError as Error).message;
      if (errorMessage.includes('Invalid file')) {
        return NextResponse.json({ error: 'Invalid file format' }, { status: 400 });
      }
      if (errorMessage.includes('Not found')) {
        return NextResponse.json({ error: 'Task not found' }, { status: 404 });
      }
      
      return NextResponse.json({ error: 'Internal Server Error' }, { status: 500 });
    }

  } catch (error) {
    logError('Failed to submit task', error as Error, { 
      projectId: params.id, 
      taskId: params.taskId 
    });
    return NextResponse.json({ error: 'Internal Server Error' }, { status: 500 });
  }
}
