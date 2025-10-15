import { describe, it, expect } from 'vitest';
import { parseCancelReason, parseHoldReason } from '@/lib/utils/reason-parser';
import type { ReasonParserInput } from '@/lib/types/analytics';

describe('Reason Parser', () => {
  // Helper function to create mock input objects
  function createMockInput(overrides: Partial<ReasonParserInput> = {}): ReasonParserInput {
    return {
      auditLogPreCancelStatus: null,
      cancelReason: null,
      recentNoteCategory: null,
      intakeMissingItems: null,
      ...overrides
    };
  }

  describe('parseCancelReason', () => {
    describe('Priority 1: Audit Log', () => {
      it('returns Property Issues - Roof Work Required for Roof Hold in audit log', () => {
        const input = createMockInput({
          auditLogPreCancelStatus: 'Roof Hold - Customer needs roof work'
        });
        
        const result = parseCancelReason(input);
        
        expect(result.category).toBe('Property Issues - Roof Work Required');
        expect(result.rawValue).toBe('Roof Hold - Customer needs roof work');
        expect(result.source).toBe('audit_log');
      });

      it('returns Documentation - Rejected for Rejected in audit log', () => {
        const input = createMockInput({
          auditLogPreCancelStatus: 'Rejected - Missing documentation'
        });
        
        const result = parseCancelReason(input);
        
        expect(result.category).toBe('Documentation - Rejected');
        expect(result.rawValue).toBe('Rejected - Missing documentation');
        expect(result.source).toBe('audit_log');
      });

      it('is case-insensitive for audit log keywords', () => {
        const input = createMockInput({
          auditLogPreCancelStatus: 'roof hold - customer issue'
        });
        
        const result = parseCancelReason(input);
        
        expect(result.category).toBe('Property Issues - Roof Work Required');
        expect(result.source).toBe('audit_log');
      });
    });

    describe('Priority 2: Cancel Reason', () => {
      it('returns Finance - Failed Loan for failedloan keyword', () => {
        const input = createMockInput({
          cancelReason: 'Failedloan - Customer credit issues'
        });
        
        const result = parseCancelReason(input);
        
        expect(result.category).toBe('Finance - Failed Loan');
        expect(result.rawValue).toBe('Failedloan - Customer credit issues');
        expect(result.source).toBe('cancel_reason');
      });

      it('returns Finance - Failed Loan for loan keyword', () => {
        const input = createMockInput({
          cancelReason: 'Customer loan application denied'
        });
        
        const result = parseCancelReason(input);
        
        expect(result.category).toBe('Finance - Failed Loan');
        expect(result.source).toBe('cancel_reason');
      });

      it('returns Customer - Unresponsive for unresponsive keyword', () => {
        const input = createMockInput({
          cancelReason: 'unresponsive customer - no contact'
        });
        
        const result = parseCancelReason(input);
        
        expect(result.category).toBe('Customer - Unresponsive');
        expect(result.source).toBe('cancel_reason');
      });

      it('returns Customer - New Move-In for move in keyword', () => {
        const input = createMockInput({
          cancelReason: 'New move in - customer not ready'
        });
        
        const result = parseCancelReason(input);
        
        expect(result.category).toBe('Customer - New Move-In');
        expect(result.source).toBe('cancel_reason');
      });

      it('returns Customer - New Move-In for bills keyword', () => {
        const input = createMockInput({
          cancelReason: 'Customer has bills to pay first'
        });
        
        const result = parseCancelReason(input);
        
        expect(result.category).toBe('Customer - New Move-In');
        expect(result.source).toBe('cancel_reason');
      });

      it('returns Customer - Changed Mind for commitment keyword', () => {
        const input = createMockInput({
          cancelReason: 'Scared of long term commitment'
        });
        
        const result = parseCancelReason(input);
        
        expect(result.category).toBe('Customer - Changed Mind');
        expect(result.source).toBe('cancel_reason');
      });

      it('is case-insensitive for cancel reason keywords', () => {
        const input = createMockInput({
          cancelReason: 'FAILEDLOAN - credit issues'
        });
        
        const result = parseCancelReason(input);
        
        expect(result.category).toBe('Finance - Failed Loan');
        expect(result.source).toBe('cancel_reason');
      });
    });

    describe('Priority 3: Note Category', () => {
      it('returns Customer - Unresponsive for Retention note category', () => {
        const input = createMockInput({
          recentNoteCategory: 'Retention'
        });
        
        const result = parseCancelReason(input);
        
        expect(result.category).toBe('Customer - Unresponsive');
        expect(result.rawValue).toBe('Retention');
        expect(result.source).toBe('note_category');
      });

      it('returns Permit Delays for Permitting note category', () => {
        const input = createMockInput({
          recentNoteCategory: 'Permitting'
        });
        
        const result = parseCancelReason(input);
        
        expect(result.category).toBe('Permit Delays');
        expect(result.rawValue).toBe('Permitting');
        expect(result.source).toBe('note_category');
      });

      it('is case-insensitive for note category keywords', () => {
        const input = createMockInput({
          recentNoteCategory: 'retention'
        });
        
        const result = parseCancelReason(input);
        
        expect(result.category).toBe('Customer - Unresponsive');
        expect(result.source).toBe('note_category');
      });
    });

    describe('Priority 4: Intake Missing Items', () => {
      it('returns Documentation - Rejected for intake missing items', () => {
        const input = createMockInput({
          intakeMissingItems: 'Utility Bill, Property Deed'
        });
        
        const result = parseCancelReason(input);
        
        expect(result.category).toBe('Documentation - Rejected');
        expect(result.rawValue).toBe('Utility Bill, Property Deed');
        expect(result.source).toBe('intake_missing_items');
      });

      it('handles single missing item', () => {
        const input = createMockInput({
          intakeMissingItems: 'Utility Bill'
        });
        
        const result = parseCancelReason(input);
        
        expect(result.category).toBe('Documentation - Rejected');
        expect(result.source).toBe('intake_missing_items');
      });

      it('handles multiple missing items', () => {
        const input = createMockInput({
          intakeMissingItems: 'Utility Bill, Property Deed, Insurance Certificate'
        });
        
        const result = parseCancelReason(input);
        
        expect(result.category).toBe('Documentation - Rejected');
        expect(result.source).toBe('intake_missing_items');
      });

      it('handles empty intake missing items', () => {
        const input = createMockInput({
          intakeMissingItems: ''
        });
        
        const result = parseCancelReason(input);
        
        expect(result.category).toBe('Unknown / Not Specified');
        expect(result.source).toBe('unknown');
      });
    });

    describe('Priority Order', () => {
      it('prioritizes audit log over cancel reason', () => {
        const input = createMockInput({
          auditLogPreCancelStatus: 'Roof Hold',
          cancelReason: 'Failedloan'
        });
        
        const result = parseCancelReason(input);
        
        expect(result.category).toBe('Property Issues - Roof Work Required');
        expect(result.source).toBe('audit_log');
      });

      it('prioritizes cancel reason over note category', () => {
        const input = createMockInput({
          cancelReason: 'unresponsive customer',
          recentNoteCategory: 'Retention'
        });
        
        const result = parseCancelReason(input);
        
        expect(result.category).toBe('Customer - Unresponsive');
        expect(result.source).toBe('cancel_reason');
      });

      it('prioritizes note category over intake missing items', () => {
        const input = createMockInput({
          recentNoteCategory: 'Permitting',
          intakeMissingItems: 'Utility Bill'
        });
        
        const result = parseCancelReason(input);
        
        expect(result.category).toBe('Permit Delays');
        expect(result.source).toBe('note_category');
      });

      it('uses intake missing items when higher priorities are null', () => {
        const input = createMockInput({
          intakeMissingItems: 'Utility Bill, Property Deed'
        });
        
        const result = parseCancelReason(input);
        
        expect(result.category).toBe('Documentation - Rejected');
        expect(result.source).toBe('intake_missing_items');
      });
    });

    describe('Edge Cases', () => {
      it('returns Unknown / Not Specified when all fields are null', () => {
        const input = createMockInput();
        
        const result = parseCancelReason(input);
        
        expect(result.category).toBe('Unknown / Not Specified');
        expect(result.rawValue).toBe(null);
        expect(result.source).toBe('unknown');
      });

      it('handles empty strings as null', () => {
        const input = createMockInput({
          auditLogPreCancelStatus: '',
          cancelReason: '',
          recentNoteCategory: '',
          intakeMissingItems: ''
        });
        
        const result = parseCancelReason(input);
        
        expect(result.category).toBe('Unknown / Not Specified');
        expect(result.source).toBe('unknown');
      });

      it('handles partial keyword matches', () => {
        const input = createMockInput({
          cancelReason: 'Customer is unresponsive to calls'
        });
        
        const result = parseCancelReason(input);
        
        expect(result.category).toBe('Customer - Unresponsive');
        expect(result.source).toBe('cancel_reason');
      });

      it('handles multiple keywords in one field - returns first match', () => {
        const input = createMockInput({
          cancelReason: 'Failedloan and unresponsive customer'
        });
        
        const result = parseCancelReason(input);
        
        expect(result.category).toBe('Finance - Failed Loan');
        expect(result.source).toBe('cancel_reason');
      });

      it('handles whitespace in input', () => {
        const input = createMockInput({
          cancelReason: '  unresponsive customer  '
        });
        
        const result = parseCancelReason(input);
        
        expect(result.category).toBe('Customer - Unresponsive');
        expect(result.source).toBe('cancel_reason');
      });
    });
  });

  describe('parseHoldReason', () => {
    describe('Basic Functionality', () => {
      it('returns Property Issues - Roof Work Required for Roof Hold in audit log', () => {
        const input = createMockInput({
          auditLogPreCancelStatus: 'Roof Hold - Customer needs roof work'
        });
        
        const result = parseHoldReason(input);
        
        expect(result.category).toBe('Property Issues - Roof Work Required');
        expect(result.rawValue).toBe('Roof Hold - Customer needs roof work');
        expect(result.source).toBe('audit_log');
      });

      it('returns Finance - Failed Loan for failedloan keyword', () => {
        const input = createMockInput({
          cancelReason: 'Failedloan - Customer credit issues'
        });
        
        const result = parseHoldReason(input);
        
        expect(result.category).toBe('Finance - Failed Loan');
        expect(result.source).toBe('cancel_reason');
      });

      it('returns Permit Delays for Permitting note category', () => {
        const input = createMockInput({
          recentNoteCategory: 'Permitting'
        });
        
        const result = parseHoldReason(input);
        
        expect(result.category).toBe('Permit Delays');
        expect(result.source).toBe('note_category');
      });

      it('returns Documentation - Rejected for intake missing items', () => {
        const input = createMockInput({
          intakeMissingItems: 'Utility Bill, Property Deed'
        });
        
        const result = parseHoldReason(input);
        
        expect(result.category).toBe('Documentation - Rejected');
        expect(result.source).toBe('intake_missing_items');
      });
    });

    describe('Priority Order', () => {
      it('follows same priority order as parseCancelReason', () => {
        const input = createMockInput({
          auditLogPreCancelStatus: 'Roof Hold',
          cancelReason: 'Failedloan',
          recentNoteCategory: 'Retention',
          intakeMissingItems: 'Utility Bill'
        });
        
        const result = parseHoldReason(input);
        
        expect(result.category).toBe('Property Issues - Roof Work Required');
        expect(result.source).toBe('audit_log');
      });
    });

    describe('Edge Cases', () => {
      it('returns Unknown / Not Specified when all fields are null', () => {
        const input = createMockInput();
        
        const result = parseHoldReason(input);
        
        expect(result.category).toBe('Unknown / Not Specified');
        expect(result.rawValue).toBe(null);
        expect(result.source).toBe('unknown');
      });

      it('handles case-insensitive matching', () => {
        const input = createMockInput({
          cancelReason: 'FAILEDLOAN - credit issues'
        });
        
        const result = parseHoldReason(input);
        
        expect(result.category).toBe('Finance - Failed Loan');
        expect(result.source).toBe('cancel_reason');
      });
    });
  });

  describe('Complex Scenarios', () => {
    it('handles all fields populated - respects priority order', () => {
      const input = createMockInput({
        auditLogPreCancelStatus: 'Rejected - Missing docs',
        cancelReason: 'unresponsive customer',
        recentNoteCategory: 'Retention',
        intakeMissingItems: 'Utility Bill, Property Deed'
      });
      
      const result = parseCancelReason(input);
      
      expect(result.category).toBe('Documentation - Rejected');
      expect(result.source).toBe('audit_log');
    });

    it('handles malformed input gracefully', () => {
      const input = createMockInput({
        cancelReason: '!@#$%^&*()_+{}|:"<>?[]\\;\',./'
      });
      
      const result = parseCancelReason(input);
      
      expect(result.category).toBe('Unknown / Not Specified');
      expect(result.source).toBe('unknown');
    });

    it('handles very long strings', () => {
      const longString = 'unresponsive '.repeat(1000);
      const input = createMockInput({
        cancelReason: longString
      });
      
      const result = parseCancelReason(input);
      
      expect(result.category).toBe('Customer - Unresponsive');
      expect(result.source).toBe('cancel_reason');
    });
  });
});
