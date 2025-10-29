// lib/integrations/arrivy/utils.ts
import type { ArrivyTask } from './types';

/**
 * Extract detailed task type from Arrivy task data using multi-signal detection
 *
 * Returns format: "Category - Subcategory" (e.g., "Surveys - Site Survey", "Installations - Full Install")
 *
 * Detection priority:
 * 1. extra_fields.task_type (explicit)
 * 2. Form detection (Site Survey Form, Install Verification Form, etc.)
 * 3. Group membership (e.g., "Site Survey - Location" groups)
 * 4. Template name with specific subcategories
 * 5. Title keyword inference
 * 6. Default to 'Service - General'
 */
export function extractTaskType(task: ArrivyTask): string {
  // 1. Check explicit task_type in extra_fields
  if (task.extra_fields?.task_type) {
    const explicitType = task.extra_fields.task_type.toLowerCase();
    // If it already has the category format, return as-is
    if (explicitType.includes(' - ')) {
      return task.extra_fields.task_type;
    }
    // Otherwise, try to enhance it
    return enhanceTaskType(explicitType);
  }

  // 2. Check for form indicators in extra_fields
  const formType = detectFromForms(task);
  if (formType) return formType;

  // 3. Check group membership
  const groupName = task.group?.name?.toLowerCase() || '';
  if (groupName.includes('site survey')) return 'Surveys - Site Survey';
  if (groupName.includes('survey')) return 'Surveys - General';
  if (groupName.includes('install')) return 'Installations - General';
  if (groupName.includes('inspection')) return 'Inspections - General';

  // 4. Check template name with specific subcategories
  const template = task.template?.toLowerCase() || '';

  // Surveys
  if (template.includes('site survey')) return 'Surveys - Site Survey';
  if (template.includes('survey')) return 'Surveys - General';

  // Installations (with specific subcategories)
  if (template.includes('solar install - full install')) return 'Installations - Full Install';
  if (template.includes('solar install - modules only')) return 'Installations - Modules Only';
  if (template.includes('solar install - tie-in only')) return 'Installations - Tie-In Only';
  if (template.includes('electrical upgrade') || template.includes('mpu')) return 'Installations - Electrical Upgrade (MPU)';
  if (template.includes('roof work')) return 'Installations - Roof Work';
  if (template.includes('install')) return 'Installations - General';

  // Inspections
  if (template.includes('electrical inspection')) return 'Inspections - Electrical';
  if (template.includes('final inspection')) return 'Inspections - Final';
  if (template.includes('inspection')) return 'Inspections - General';

  // Service/Maintenance
  if (template.includes('service') || template.includes('maintenance')) return 'Service - General';
  if (template.includes('activity')) return 'Service - Activity';

  // 5. Check title keywords
  const title = task.title?.toLowerCase() || '';

  if (title.includes('site survey')) return 'Surveys - Site Survey';
  if (title.includes('survey')) return 'Surveys - General';

  if (title.includes('full install')) return 'Installations - Full Install';
  if (title.includes('modules only')) return 'Installations - Modules Only';
  if (title.includes('tie-in')) return 'Installations - Tie-In Only';
  if (title.includes('mpu') || title.includes('electrical upgrade')) return 'Installations - Electrical Upgrade (MPU)';
  if (title.includes('roof work')) return 'Installations - Roof Work';
  if (title.includes('install')) return 'Installations - General';

  if (title.includes('electrical inspection')) return 'Inspections - Electrical';
  if (title.includes('final inspection')) return 'Inspections - Final';
  if (title.includes('inspection')) return 'Inspections - General';

  // 6. Default
  return 'Service - General';
}

/**
 * Detect task type from attached forms
 * Checks extra_fields for form-related data
 */
function detectFromForms(task: ArrivyTask): string | null {
  if (!task.extra_fields) return null;

  // Common form field names to check
  const formFields = [
    'form_name',
    'form_type',
    'forms',
    'attached_forms',
    'document_name',
    'template_form'
  ];

  for (const field of formFields) {
    const value = task.extra_fields[field];
    if (!value) continue;

    const formName = (typeof value === 'string' ? value : JSON.stringify(value)).toLowerCase();

    if (formName.includes('site survey form')) return 'Surveys - Site Survey';
    if (formName.includes('survey form')) return 'Surveys - General';
    if (formName.includes('install verification form')) return 'Installations - General';
    if (formName.includes('mpu verification form')) return 'Installations - Electrical Upgrade (MPU)';
    if (formName.includes('field check-in')) return 'Service - Field Check-In';
  }

  return null;
}

/**
 * Enhance a basic task type into detailed format
 */
function enhanceTaskType(basicType: string): string {
  const type = basicType.toLowerCase();

  if (type === 'survey') return 'Surveys - Site Survey';
  if (type === 'install' || type === 'installation') return 'Installations - General';
  if (type === 'inspection') return 'Inspections - General';
  if (type === 'service') return 'Service - General';

  // If already specific, capitalize properly
  return basicType.split(' ').map(word =>
    word.charAt(0).toUpperCase() + word.slice(1)
  ).join(' ');
}

/**
 * Format full address from Arrivy task data
 */
export function formatTaskAddress(task: ArrivyTask): string | undefined {
  const parts = [
    task.customer_address_line_1,
    task.customer_address_line_2,
    task.customer_city,
    task.customer_state,
    task.customer_zipcode
  ].filter(Boolean);
  return parts.length > 0 ? parts.join(', ') : undefined;
}
