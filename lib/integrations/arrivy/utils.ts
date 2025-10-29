// lib/integrations/arrivy/utils.ts
import type { ArrivyTask } from './types';

/**
 * Extract task type from Arrivy task data
 * Priority:
 * 1. extra_fields.task_type (explicit)
 * 2. template name (e.g., "Site Survey", "Installation")
 * 3. title inference
 * 4. Default to 'service'
 */
export function extractTaskType(task: ArrivyTask): string {
  // 1. Check explicit task_type in extra_fields
  if (task.extra_fields?.task_type) {
    return task.extra_fields.task_type.toLowerCase();
  }

  // 2. Check template name
  const template = task.template?.toLowerCase() || '';
  if (template.includes('survey') || template.includes('site survey')) return 'survey';
  if (template.includes('install')) return 'install';
  if (template.includes('inspection')) return 'inspection';
  if (template.includes('service') || template.includes('maintenance')) return 'service';

  // 3. Check title
  const title = task.title?.toLowerCase() || '';
  if (title.includes('survey') || title.includes('site survey')) return 'survey';
  if (title.includes('install')) return 'install';
  if (title.includes('inspection')) return 'inspection';

  // 4. Default
  return 'service';
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
