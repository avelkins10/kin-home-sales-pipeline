// lib/integrations/arrivy/utils.ts
import type { ArrivyTask } from './types';

/**
 * Extract detailed task type from Arrivy task data using multi-signal detection
 *
 * Returns format: "Category - Subcategory" (e.g., "Surveys - Site Survey", "Installations - Full Install")
 *
 * Detection priority (OPTIMIZED):
 * 1. extra_fields.task_type (explicit manual override)
 * 2. Template name/ID (PRIMARY - Arrivy's explicit categorization mechanism)
 * 3. Form detection from extra_fields (indirect but reliable - form fields come from template)
 * 4. Group membership (e.g., "Site Survey - Location" groups)
 * 5. Title keyword inference (least reliable - often customer names)
 * 6. Default to 'Service - General'
 */
export function extractTaskType(task: ArrivyTask): string {
  // 1. Check explicit task_type in extra_fields (manual override - highest priority)
  if (task.extra_fields?.task_type) {
    const explicitType = task.extra_fields.task_type.toLowerCase();
    // If it already has the category format, return as-is
    if (explicitType.includes(' - ')) {
      return task.extra_fields.task_type;
    }
    // Otherwise, try to enhance it
    return enhanceTaskType(explicitType);
  }

  // 2. Check template name/ID (PRIMARY DETECTION - Arrivy's explicit categorization)
  // Templates are specifically designed to categorize tasks in Arrivy
  // Template can be string (name) or number (ID) - handle both
  if (task.template || task.template_id) {
    // If template is a string, it's the template name (most reliable)
    if (typeof task.template === 'string' && task.template.trim()) {
      const templateName = task.template.toLowerCase();
      const templateType = detectFromTemplateName(templateName);
      if (templateType) return templateType;
    }
    
    // If template is numeric ID, we can't parse it directly
    // But we can check if template_id matches known patterns
    // For now, fall through to form detection (form fields come from template)
  }

  // 3. Check for form indicators in extra_fields (indirect template detection)
  // Form fields like "Notes for Surveyor" exist because of the template
  // This is reliable because templates define which form fields are available
  const formType = detectFromForms(task);
  if (formType) return formType;

  // 4. Check details field for keywords (often contains task description)
  const details = task.details?.toLowerCase() || '';
  if (details.includes('site survey') || details.includes('property survey')) return 'Surveys - Site Survey';
  if (details.includes('survey')) return 'Surveys - General';
  if (details.includes('full install') || details.includes('complete install')) return 'Installations - Full Install';
  if (details.includes('modules only') || details.includes('panels only')) return 'Installations - Modules Only';
  if (details.includes('tie-in') || details.includes('tie in')) return 'Installations - Tie-In Only';
  if (details.includes('mpu') || details.includes('electrical upgrade') || details.includes('main panel')) return 'Installations - Electrical Upgrade (MPU)';
  if (details.includes('roof work') || details.includes('roofing')) return 'Installations - Roof Work';
  if (details.includes('install') || details.includes('installation')) return 'Installations - General';
  if (details.includes('electrical inspection')) return 'Inspections - Electrical';
  if (details.includes('final inspection') || details.includes('building inspection')) return 'Inspections - Final';
  if (details.includes('inspection')) return 'Inspections - General';

  // 5. Check group membership
  const groupName = task.group?.name?.toLowerCase() || '';
  if (groupName.includes('site survey')) return 'Surveys - Site Survey';
  if (groupName.includes('survey')) return 'Surveys - General';
  if (groupName.includes('install')) return 'Installations - General';
  if (groupName.includes('inspection')) return 'Inspections - General';

  // 6. Check title keywords (often contains task type)
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

  // 7. Check external_id pattern (QuickBase project IDs might contain type hints)
  // Format: projectId_taskId (e.g., "725_bvbqgs5yc")
  // This is less reliable but can help as last resort
  const externalId = task.external_id?.toLowerCase() || '';
  // Note: External IDs are usually opaque, but checking just in case

  // 8. Default fallback - ONLY if absolutely no signals found
  // Log a warning so we can debug why detection failed
  if (process.env.NODE_ENV === 'development') {
    console.warn('[extractTaskType] No type signals found, defaulting to Service - General', {
      task_id: task.id,
      template: task.template,
      template_id: task.template_id,
      title: task.title,
      has_extra_fields: !!task.extra_fields,
      extra_fields_keys: task.extra_fields ? Object.keys(task.extra_fields) : [],
      group_name: task.group?.name,
    });
  }
  
  return 'Service - General';
}

/**
 * Detect task type from template name
 * Templates are Arrivy's explicit categorization mechanism
 */
function detectFromTemplateName(templateName: string): string | null {
  if (!templateName || templateName.trim() === '') return null;

  const lower = templateName.toLowerCase();

  // Surveys (most specific first)
  if (lower.includes('site survey')) return 'Surveys - Site Survey';
  if (lower.includes('survey')) return 'Surveys - General';

  // Installations (most specific first)
  if (lower.includes('solar install - full install')) return 'Installations - Full Install';
  if (lower.includes('solar install - modules only')) return 'Installations - Modules Only';
  if (lower.includes('solar install - tie-in only')) return 'Installations - Tie-In Only';
  if (lower.includes('electrical upgrade') || lower.includes('mpu')) return 'Installations - Electrical Upgrade (MPU)';
  if (lower.includes('roof work')) return 'Installations - Roof Work';
  if (lower.includes('install')) return 'Installations - General';

  // Inspections (most specific first)
  if (lower.includes('electrical inspection')) return 'Inspections - Electrical';
  if (lower.includes('final inspection')) return 'Inspections - Final';
  if (lower.includes('inspection')) return 'Inspections - General';

  // Service/Maintenance
  if (lower.includes('service') || lower.includes('maintenance')) return 'Service - General';
  if (lower.includes('activity')) return 'Service - Activity';

  return null;
}

/**
 * Detect task type from attached forms
 * Checks extra_fields for form-related data and field indicators
 */
function detectFromForms(task: ArrivyTask): string | null {
  if (!task.extra_fields) return null;

  // MOST RELIABLE: Check for "Notes for [Role]" fields (template-specific form fields)
  const fieldKeys = Object.keys(task.extra_fields).join('|').toLowerCase();
  
  // Also check field values (not just keys) for additional signals
  const fieldValues = Object.values(task.extra_fields)
    .map(v => typeof v === 'string' ? v : JSON.stringify(v))
    .join('|')
    .toLowerCase();
  
  const allFields = `${fieldKeys}|${fieldValues}`;

  // Survey detection
  if (allFields.includes('notes for surveyor') || allFields.includes('surveyor notes')) return 'Surveys - Site Survey';
  if (allFields.includes('surveyor') || allFields.includes('site survey')) return 'Surveys - Site Survey';
  if (allFields.includes('survey')) return 'Surveys - General';

  // Installation detection
  if (allFields.includes('notes for installer') || allFields.includes('installer notes')) return 'Installations - General';
  if (allFields.includes('full install') || allFields.includes('complete install')) return 'Installations - Full Install';
  if (allFields.includes('modules only') || allFields.includes('panels only')) return 'Installations - Modules Only';
  if (allFields.includes('tie-in') || allFields.includes('tie in')) return 'Installations - Tie-In Only';
  if (allFields.includes('mpu') || allFields.includes('main panel upgrade')) return 'Installations - Electrical Upgrade (MPU)';
  if (allFields.includes('electrical upgrade')) return 'Installations - Electrical Upgrade (MPU)';
  if (allFields.includes('roof work') || allFields.includes('roofing')) return 'Installations - Roof Work';
  if (allFields.includes('installer') || allFields.includes('install') || allFields.includes('installation')) return 'Installations - General';

  // Inspection detection
  if (allFields.includes('notes for inspector') || allFields.includes('inspector notes')) return 'Inspections - General';
  if (allFields.includes('electrical inspection')) return 'Inspections - Electrical';
  if (allFields.includes('final inspection') || allFields.includes('building inspection')) return 'Inspections - Final';
  if (allFields.includes('inspector') || allFields.includes('inspection')) return 'Inspections - General';

  // Service detection (lower priority - only if explicitly service-related)
  if (allFields.includes('notes for service tech') || allFields.includes('service tech notes')) return 'Service - General';
  if (allFields.includes('service tech') && !allFields.includes('install') && !allFields.includes('survey')) return 'Service - General';

  // Check for Solar/PV specific indicators
  if (allFields.includes('solar') || allFields.includes('pv system') || allFields.includes('photovoltaic')) {
    if (allFields.includes('install')) return 'Installations - General';
    return 'Installations - Solar';
  }

  // Check form field values for form names
  const formFields = [
    'form_name',
    'form_type',
    'forms',
    'attached_forms',
    'document_name',
    'template_form',
    'form_title'
  ];

  for (const field of formFields) {
    const value = task.extra_fields[field];
    if (!value) continue;

    const formName = (typeof value === 'string' ? value : JSON.stringify(value)).toLowerCase();

    if (formName.includes('site survey form') || formName.includes('site survey')) return 'Surveys - Site Survey';
    if (formName.includes('survey form') || formName.includes('survey')) return 'Surveys - General';
    if (formName.includes('install verification form') || formName.includes('install form')) return 'Installations - General';
    if (formName.includes('mpu verification form') || formName.includes('mpu form')) return 'Installations - Electrical Upgrade (MPU)';
    if (formName.includes('inspection form') || formName.includes('inspection')) return 'Inspections - General';
    if (formName.includes('field check-in') || formName.includes('service form')) return 'Service - Field Check-In';
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
 * Calculate arrival window times from Arrivy task data
 * Arrivy uses time_window_start (minutes before scheduled_start) and duration
 */
export function calculateArrivalWindow(
  scheduledStart: Date | null,
  timeWindowStart?: number,
  duration?: number
): { start: Date | null; end: Date | null } {
  if (!scheduledStart || timeWindowStart === undefined) {
    return { start: null, end: null };
  }

  // time_window_start is minutes BEFORE scheduled_start
  const windowStart = new Date(scheduledStart.getTime() - (timeWindowStart * 60 * 1000));
  
  // Window end is scheduled_start + duration (if available)
  // Otherwise use scheduled_start as end
  const windowEnd = duration 
    ? new Date(scheduledStart.getTime() + (duration * 60 * 1000))
    : scheduledStart;

  return { start: windowStart, end: windowEnd };
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
