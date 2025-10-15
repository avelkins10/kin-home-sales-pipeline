// lib/utils/reason-parser.ts
// Priority-based parsing of cancellation and hold reasons from Quickbase fields

import type { 
  CancelReasonCategory, 
  HoldReasonCategory, 
  ParsedReason, 
  ReasonParserInput 
} from '@/lib/types/analytics';

/**
 * Parse cancellation reason from various Quickbase fields using priority-based logic
 * 
 * Priority order:
 * 1. auditLogPreCancelStatus - Check for keywords like 'Roof Hold', 'Rejected'
 * 2. cancelReason - Check for keywords like 'failedloan', 'unresponsive', etc.
 * 3. recentNoteCategory - Check for 'Retention', 'Permitting'
 * 4. intakeMissingItems - Return first 2 items as 'Documentation - [items]'
 * 5. Default - Return 'Unknown / Not Specified'
 * 
 * @param input - Object containing all potential reason fields
 * @returns ParsedReason with category, rawValue, and source
 */
export function parseCancelReason(input: ReasonParserInput): ParsedReason {
  const { auditLogPreCancelStatus, cancelReason, recentNoteCategory, intakeMissingItems } = input;

  // Priority 1: Check audit log for status keywords
  if (auditLogPreCancelStatus) {
    const auditKeywords: { keyword: string; category: CancelReasonCategory | HoldReasonCategory }[] = [
      { keyword: 'Roof Hold', category: 'Property Issues - Roof Work Required' },
      { keyword: 'Rejected', category: 'Documentation - Rejected' }
    ];
    
    const match = extractKeywords(auditLogPreCancelStatus, auditKeywords.map(k => k.keyword));
    if (match) {
      const keywordObj = auditKeywords.find(k => k.keyword.toLowerCase() === match.toLowerCase());
      if (keywordObj) {
        return {
          category: keywordObj.category,
          rawValue: auditLogPreCancelStatus,
          source: 'audit_log'
        };
      }
    }
  }

  // Priority 2: Check cancel reason field
  if (cancelReason) {
    const cancelKeywords: { keywords: string[]; category: CancelReasonCategory | HoldReasonCategory }[] = [
      { keywords: ['failedloan', 'loan'], category: 'Finance - Failed Loan' },
      { keywords: ['unresponsive'], category: 'Customer - Unresponsive' },
      { keywords: ['move in', 'bills'], category: 'Customer - New Move-In' },
      { keywords: ['commitment'], category: 'Customer - Changed Mind' }
    ];

    for (const { keywords, category } of cancelKeywords) {
      const match = extractKeywords(cancelReason, keywords);
      if (match) {
        return {
          category,
          rawValue: cancelReason,
          source: 'cancel_reason'
        };
      }
    }
  }

  // Priority 3: Check recent note category
  if (recentNoteCategory) {
    const noteKeywords: { keyword: string; category: CancelReasonCategory | HoldReasonCategory }[] = [
      { keyword: 'Retention', category: 'Customer - Unresponsive' },
      { keyword: 'Permitting', category: 'Permit Delays' }
    ];

    const match = extractKeywords(recentNoteCategory, noteKeywords.map(k => k.keyword));
    if (match) {
      const keywordObj = noteKeywords.find(k => k.keyword.toLowerCase() === match.toLowerCase());
      if (keywordObj) {
        return {
          category: keywordObj.category,
          rawValue: recentNoteCategory,
          source: 'note_category'
        };
      }
    }
  }

  // Priority 4: Check intake missing items
  if (intakeMissingItems) {
    // Parse multitext field and return first 2 items
    const items = intakeMissingItems.split(/[,;\n]/).map(item => item.trim()).filter(Boolean);
    if (items.length > 0) {
      const firstTwoItems = items.slice(0, 2).join(', ');
      return {
        category: 'Documentation - Rejected',
        rawValue: intakeMissingItems,
        source: 'intake_missing_items'
      };
    }
  }

  // Default: Unknown
  return {
    category: 'Unknown / Not Specified',
    rawValue: null,
    source: 'unknown'
  };
}

/**
 * Parse hold reason from various Quickbase fields using priority-based logic
 * Similar to parseCancelReason but optimized for hold scenarios
 * 
 * @param input - Object containing all potential reason fields
 * @returns ParsedReason with category, rawValue, and source
 */
export function parseHoldReason(input: ReasonParserInput): ParsedReason {
  const { auditLogPreCancelStatus, cancelReason, recentNoteCategory, intakeMissingItems } = input;

  // Priority 1: Check audit log for status keywords
  if (auditLogPreCancelStatus) {
    const auditKeywords: { keyword: string; category: CancelReasonCategory | HoldReasonCategory }[] = [
      { keyword: 'Roof Hold', category: 'Property Issues - Roof Work Required' },
      { keyword: 'Rejected', category: 'Documentation - Rejected' }
    ];
    
    const match = extractKeywords(auditLogPreCancelStatus, auditKeywords.map(k => k.keyword));
    if (match) {
      const keywordObj = auditKeywords.find(k => k.keyword.toLowerCase() === match.toLowerCase());
      if (keywordObj) {
        return {
          category: keywordObj.category,
          rawValue: auditLogPreCancelStatus,
          source: 'audit_log'
        };
      }
    }
  }

  // Priority 2: Check cancel reason field (same logic as cancel reasons)
  if (cancelReason) {
    const cancelKeywords: { keywords: string[]; category: CancelReasonCategory | HoldReasonCategory }[] = [
      { keywords: ['failedloan', 'loan'], category: 'Finance - Failed Loan' },
      { keywords: ['unresponsive'], category: 'Customer - Unresponsive' },
      { keywords: ['move in', 'bills'], category: 'Customer - New Move-In' },
      { keywords: ['commitment'], category: 'Customer - Changed Mind' }
    ];

    for (const { keywords, category } of cancelKeywords) {
      const match = extractKeywords(cancelReason, keywords);
      if (match) {
        return {
          category,
          rawValue: cancelReason,
          source: 'cancel_reason'
        };
      }
    }
  }

  // Priority 3: Check recent note category
  if (recentNoteCategory) {
    const noteKeywords: { keyword: string; category: CancelReasonCategory | HoldReasonCategory }[] = [
      { keyword: 'Retention', category: 'Customer - Unresponsive' },
      { keyword: 'Permitting', category: 'Permit Delays' }
    ];

    const match = extractKeywords(recentNoteCategory, noteKeywords.map(k => k.keyword));
    if (match) {
      const keywordObj = noteKeywords.find(k => k.keyword.toLowerCase() === match.toLowerCase());
      if (keywordObj) {
        return {
          category: keywordObj.category,
          rawValue: recentNoteCategory,
          source: 'note_category'
        };
      }
    }
  }

  // Priority 4: Check intake missing items
  if (intakeMissingItems) {
    const items = intakeMissingItems.split(/[,;\n]/).map(item => item.trim()).filter(Boolean);
    if (items.length > 0) {
      return {
        category: 'Documentation - Rejected',
        rawValue: intakeMissingItems,
        source: 'intake_missing_items'
      };
    }
  }

  // Default: Unknown
  return {
    category: 'Unknown / Not Specified',
    rawValue: null,
    source: 'unknown'
  };
}

/**
 * Helper function to extract keywords from text using case-insensitive matching
 * Returns the first keyword found in the text
 * 
 * @param text - Text to search in
 * @param keywords - Array of keywords to search for
 * @returns First matching keyword or null if none found
 */
function extractKeywords(text: string, keywords: string[]): string | null {
  if (!text || !keywords.length) {
    return null;
  }

  const lowerText = text.toLowerCase();
  
  for (const keyword of keywords) {
    if (lowerText.includes(keyword.toLowerCase())) {
      return keyword;
    }
  }
  
  return null;
}
