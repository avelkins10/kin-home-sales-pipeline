// lib/utils/hold-detection.ts

/**
 * Detect if a project is on hold based on status string
 * @param status - Project status string
 * @returns true if project is on hold
 */
export function detectHoldStatus(status: string): boolean {
  const holdKeywords = ['On Hold', 'Finance Hold', 'Roof Hold', 'Customer Hold', 'Permit Hold', 'HOA Hold'];
  return holdKeywords.some(keyword => status.includes(keyword));
}

/**
 * Extract hold type from status string
 * @param status - Project status string
 * @returns hold type or 'generic' if not specific
 */
export function extractHoldType(status: string): 'finance' | 'roof' | 'customer' | 'permit' | 'hoa' | 'generic' {
  if (status.includes('Finance Hold')) {
    return 'finance';
  }
  
  if (status.includes('Roof Hold')) {
    return 'roof';
  }
  
  if (status.includes('Customer Hold')) {
    return 'customer';
  }
  
  if (status.includes('Permit Hold')) {
    return 'permit';
  }
  
  if (status.includes('HOA Hold')) {
    return 'hoa';
  }
  
  return 'generic';
}
