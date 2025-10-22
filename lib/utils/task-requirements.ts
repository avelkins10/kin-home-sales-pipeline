/**
 * Task Requirements Utilities
 *
 * Determines whether a task requires file upload based on task name and category.
 * Uses pattern matching to infer requirements until explicit QuickBase field is added.
 */

export interface TaskRequirements {
  requiresFile: boolean;
  requirementType: 'required' | 'optional' | 'none';
  reason: string;
}

/**
 * Keywords that indicate a task REQUIRES file upload
 */
const FILE_REQUIRED_KEYWORDS = [
  'upload',
  'photo',
  'photos',
  'picture',
  'pictures',
  'image',
  'images',
  'document',
  'documents',
  'scan',
  'scanned',
  'file',
  'files',
  'pdf',
  'contract',
  'signed',
  'signature',
  'proof',
  'evidence',
  'attachment',
  'attachments',
  'submit file',
  'provide document',
  'send photo'
];

/**
 * Keywords that indicate a task does NOT require file upload (confirmation/acknowledgment tasks)
 */
const NO_FILE_KEYWORDS = [
  'confirm',
  'acknowledge',
  'verify status',
  'check status',
  'review only',
  'notification',
  'reminder'
];

/**
 * Determines if a task requires file upload based on its name and category
 *
 * @param taskName - The name of the task (field 10)
 * @param taskCategory - The category of the task (field 31), optional
 * @param explicitRequirement - Optional explicit requirement from QuickBase field (future enhancement)
 * @returns TaskRequirements object with requirement details
 */
export function getTaskRequirements(
  taskName: string,
  taskCategory?: string | null,
  explicitRequirement?: 'file_required' | 'file_optional' | 'text_only' | null
): TaskRequirements {
  // Priority 1: Use explicit requirement if provided (future QuickBase field)
  if (explicitRequirement) {
    switch (explicitRequirement) {
      case 'file_required':
        return {
          requiresFile: true,
          requirementType: 'required',
          reason: 'Explicitly configured as file required'
        };
      case 'file_optional':
        return {
          requiresFile: false,
          requirementType: 'optional',
          reason: 'File upload is optional'
        };
      case 'text_only':
        return {
          requiresFile: false,
          requirementType: 'none',
          reason: 'Text-only task (no file upload)'
        };
    }
  }

  // Priority 2: Infer from task name and category
  const searchText = `${taskName} ${taskCategory || ''}`.toLowerCase();

  // Check for file required keywords
  const hasFileKeyword = FILE_REQUIRED_KEYWORDS.some(keyword =>
    searchText.includes(keyword)
  );

  if (hasFileKeyword) {
    return {
      requiresFile: true,
      requirementType: 'required',
      reason: `Task name/category indicates file upload required`
    };
  }

  // Check for no-file keywords
  const hasNoFileKeyword = NO_FILE_KEYWORDS.some(keyword =>
    searchText.includes(keyword)
  );

  if (hasNoFileKeyword) {
    return {
      requiresFile: false,
      requirementType: 'none',
      reason: 'Confirmation/acknowledgment task (no file needed)'
    };
  }

  // Default: File optional (safer default - allows upload but doesn't require it)
  return {
    requiresFile: false,
    requirementType: 'optional',
    reason: 'File upload is optional for this task'
  };
}

/**
 * Helper function to check if file upload should be shown in UI
 * Even if file is not required, we may still want to show the upload option
 */
export function shouldShowFileUpload(requirements: TaskRequirements): boolean {
  // Show file upload for 'required' and 'optional' types
  // Hide for 'none' type (text-only tasks)
  return requirements.requirementType !== 'none';
}

/**
 * Helper function to get user-friendly label for file upload section
 */
export function getFileUploadLabel(requirements: TaskRequirements): string {
  switch (requirements.requirementType) {
    case 'required':
      return 'Upload File';
    case 'optional':
      return 'Upload File (Optional)';
    case 'none':
      return ''; // Should not be shown
  }
}
