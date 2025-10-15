import { describe, it, expect } from 'vitest';
import { getCalendarDates } from '@/lib/utils/calendar-helpers';
import { PROJECT_FIELDS } from '@/lib/constants/fieldIds';
import { QuickbaseProject } from '@/lib/types/project';

// Helper function to create mock project
function createMockProject(fields: Record<string, any>): QuickbaseProject {
  const project: QuickbaseProject = {};
  
  Object.entries(fields).forEach(([key, value]) => {
    project[key] = { value: String(value) };
  });
  
  return project;
}

describe('Calendar Helpers', () => {
  describe('getCalendarDates', () => {
    describe('Core Functionality', () => {
      it('returns empty array for missing recordId', () => {
        const project = createMockProject({
          [PROJECT_FIELDS.PROJECT_ID]: 'KIN-001',
          [PROJECT_FIELDS.CUSTOMER_NAME]: 'John Smith',
        });

        const result = getCalendarDates(project);
        expect(result).toEqual([]);
      });

      it('returns empty array for missing projectId', () => {
        const project = createMockProject({
          [PROJECT_FIELDS.RECORD_ID]: 123,
          [PROJECT_FIELDS.CUSTOMER_NAME]: 'John Smith',
        });

        const result = getCalendarDates(project);
        expect(result).toEqual([]);
      });

      it('returns empty array for null project', () => {
        const result = getCalendarDates(null as any);
        expect(result).toEqual([]);
      });

      it('returns empty array for undefined project', () => {
        const result = getCalendarDates(undefined as any);
        expect(result).toEqual([]);
      });
    });

    describe('Survey Events', () => {
      it('creates survey scheduled event from SITE_SURVEY_ARRIVY_SCHEDULED', () => {
        const project = createMockProject({
          [PROJECT_FIELDS.RECORD_ID]: 123,
          [PROJECT_FIELDS.PROJECT_ID]: 'KIN-001',
          [PROJECT_FIELDS.CUSTOMER_NAME]: 'John Smith',
          [PROJECT_FIELDS.SITE_SURVEY_ARRIVY_SCHEDULED]: '2024-03-15',
        });

        const result = getCalendarDates(project);
        expect(result).toHaveLength(1);
        expect(result[0].id).toBe('123-survey-scheduled');
        expect(result[0].type).toBe('survey');
        expect(result[0].status).toBe('scheduled');
        expect(result[0].date).toBe('2024-03-15');
        expect(result[0].title).toContain('Survey Scheduled - John Smith');
      });

      it('creates survey approved event from SURVEY_APPROVED', () => {
        const project = createMockProject({
          [PROJECT_FIELDS.RECORD_ID]: 123,
          [PROJECT_FIELDS.PROJECT_ID]: 'KIN-001',
          [PROJECT_FIELDS.CUSTOMER_NAME]: 'John Smith',
          [PROJECT_FIELDS.SURVEY_APPROVED]: '2024-03-20',
        });

        const result = getCalendarDates(project);
        expect(result).toHaveLength(1);
        expect(result[0].id).toBe('123-survey-approved');
        expect(result[0].type).toBe('survey');
        expect(result[0].status).toBe('completed');
        expect(result[0].date).toBe('2024-03-20');
        expect(result[0].title).toContain('Survey Approved');
      });

      it('creates both survey events when both dates exist', () => {
        const project = createMockProject({
          [PROJECT_FIELDS.RECORD_ID]: 123,
          [PROJECT_FIELDS.PROJECT_ID]: 'KIN-001',
          [PROJECT_FIELDS.CUSTOMER_NAME]: 'John Smith',
          [PROJECT_FIELDS.SITE_SURVEY_ARRIVY_SCHEDULED]: '2024-03-15',
          [PROJECT_FIELDS.SURVEY_APPROVED]: '2024-03-20',
        });

        const result = getCalendarDates(project);
        expect(result).toHaveLength(2);
        
        const scheduledEvent = result.find(e => e.id === '123-survey-scheduled');
        const approvedEvent = result.find(e => e.id === '123-survey-approved');
        
        expect(scheduledEvent?.status).toBe('scheduled');
        expect(approvedEvent?.status).toBe('completed');
      });

      it('uses "Unknown Customer" when customerName is missing', () => {
        const project = createMockProject({
          [PROJECT_FIELDS.RECORD_ID]: 123,
          [PROJECT_FIELDS.PROJECT_ID]: 'KIN-001',
          [PROJECT_FIELDS.SITE_SURVEY_ARRIVY_SCHEDULED]: '2024-03-15',
        });

        const result = getCalendarDates(project);
        expect(result[0].title).toContain('Unknown Customer');
      });
    });

    describe('Install Events with Fallback Logic', () => {
      it('creates install scheduled from INSTALL_SCHEDULED_DATE_CAPTURE (primary)', () => {
        const project = createMockProject({
          [PROJECT_FIELDS.RECORD_ID]: 123,
          [PROJECT_FIELDS.PROJECT_ID]: 'KIN-001',
          [PROJECT_FIELDS.CUSTOMER_NAME]: 'John Smith',
          [PROJECT_FIELDS.INSTALL_SCHEDULED_DATE_CAPTURE]: '2024-04-01',
        });

        const result = getCalendarDates(project);
        expect(result).toHaveLength(1);
        expect(result[0].id).toBe('123-install-scheduled');
        expect(result[0].type).toBe('install');
        expect(result[0].status).toBe('scheduled');
        expect(result[0].date).toBe('2024-04-01');
      });

      it('falls back to INSTALL_SCHEDULED_START_DATE when primary missing', () => {
        const project = createMockProject({
          [PROJECT_FIELDS.RECORD_ID]: 123,
          [PROJECT_FIELDS.PROJECT_ID]: 'KIN-001',
          [PROJECT_FIELDS.CUSTOMER_NAME]: 'John Smith',
          [PROJECT_FIELDS.INSTALL_SCHEDULED_START_DATE]: '2024-04-05',
        });

        const result = getCalendarDates(project);
        expect(result).toHaveLength(1);
        expect(result[0].id).toBe('123-install-start');
        expect(result[0].type).toBe('install');
        expect(result[0].status).toBe('scheduled');
        expect(result[0].date).toBe('2024-04-05');
      });

      it('falls back to ESTIMATED_INSTALL_DATE when both primary and secondary missing', () => {
        const project = createMockProject({
          [PROJECT_FIELDS.RECORD_ID]: 123,
          [PROJECT_FIELDS.PROJECT_ID]: 'KIN-001',
          [PROJECT_FIELDS.CUSTOMER_NAME]: 'John Smith',
          [PROJECT_FIELDS.ESTIMATED_INSTALL_DATE]: '2024-04-10',
        });

        const result = getCalendarDates(project);
        expect(result).toHaveLength(1);
        expect(result[0].id).toBe('123-install-estimated');
        expect(result[0].type).toBe('install');
        expect(result[0].status).toBe('pending');
        expect(result[0].date).toBe('2024-04-10');
      });

      it('prefers primary over fallbacks when multiple exist', () => {
        const project = createMockProject({
          [PROJECT_FIELDS.RECORD_ID]: 123,
          [PROJECT_FIELDS.PROJECT_ID]: 'KIN-001',
          [PROJECT_FIELDS.CUSTOMER_NAME]: 'John Smith',
          [PROJECT_FIELDS.INSTALL_SCHEDULED_DATE_CAPTURE]: '2024-04-01',
          [PROJECT_FIELDS.INSTALL_SCHEDULED_START_DATE]: '2024-04-05',
          [PROJECT_FIELDS.ESTIMATED_INSTALL_DATE]: '2024-04-10',
        });

        const result = getCalendarDates(project);
        expect(result).toHaveLength(1);
        expect(result[0].id).toBe('123-install-scheduled');
        expect(result[0].date).toBe('2024-04-01');
      });

      it('creates install completed event from INSTALL_COMPLETED_DATE', () => {
        const project = createMockProject({
          [PROJECT_FIELDS.RECORD_ID]: 123,
          [PROJECT_FIELDS.PROJECT_ID]: 'KIN-001',
          [PROJECT_FIELDS.CUSTOMER_NAME]: 'John Smith',
          [PROJECT_FIELDS.INSTALL_COMPLETED_DATE]: '2024-04-15',
        });

        const result = getCalendarDates(project);
        expect(result).toHaveLength(1);
        expect(result[0].id).toBe('123-install-completed');
        expect(result[0].type).toBe('install');
        expect(result[0].status).toBe('completed');
        expect(result[0].date).toBe('2024-04-15');
      });

      it('creates both scheduled and completed install events', () => {
        const project = createMockProject({
          [PROJECT_FIELDS.RECORD_ID]: 123,
          [PROJECT_FIELDS.PROJECT_ID]: 'KIN-001',
          [PROJECT_FIELDS.CUSTOMER_NAME]: 'John Smith',
          [PROJECT_FIELDS.INSTALL_SCHEDULED_DATE_CAPTURE]: '2024-04-01',
          [PROJECT_FIELDS.INSTALL_COMPLETED_DATE]: '2024-04-15',
        });

        const result = getCalendarDates(project);
        expect(result).toHaveLength(2);
        
        const scheduledEvent = result.find(e => e.id === '123-install-scheduled');
        const completedEvent = result.find(e => e.id === '123-install-completed');
        
        expect(scheduledEvent?.status).toBe('scheduled');
        expect(completedEvent?.status).toBe('completed');
      });
    });

    describe('Inspection Events', () => {
      it('creates inspection scheduled event', () => {
        const project = createMockProject({
          [PROJECT_FIELDS.RECORD_ID]: 123,
          [PROJECT_FIELDS.PROJECT_ID]: 'KIN-001',
          [PROJECT_FIELDS.CUSTOMER_NAME]: 'John Smith',
          [PROJECT_FIELDS.INSPECTION_SCHEDULED_DATE]: '2024-05-01',
        });

        const result = getCalendarDates(project);
        expect(result).toHaveLength(1);
        expect(result[0].id).toBe('123-inspection-scheduled');
        expect(result[0].type).toBe('inspection');
        expect(result[0].status).toBe('scheduled');
        expect(result[0].date).toBe('2024-05-01');
      });

      it('creates inspection completed event', () => {
        const project = createMockProject({
          [PROJECT_FIELDS.RECORD_ID]: 123,
          [PROJECT_FIELDS.PROJECT_ID]: 'KIN-001',
          [PROJECT_FIELDS.CUSTOMER_NAME]: 'John Smith',
          [PROJECT_FIELDS.PASSING_INSPECTION_COMPLETED]: '2024-05-05',
        });

        const result = getCalendarDates(project);
        expect(result).toHaveLength(1);
        expect(result[0].id).toBe('123-inspection-completed');
        expect(result[0].type).toBe('inspection');
        expect(result[0].status).toBe('completed');
        expect(result[0].date).toBe('2024-05-05');
      });

      it('creates both inspection events when both exist', () => {
        const project = createMockProject({
          [PROJECT_FIELDS.RECORD_ID]: 123,
          [PROJECT_FIELDS.PROJECT_ID]: 'KIN-001',
          [PROJECT_FIELDS.CUSTOMER_NAME]: 'John Smith',
          [PROJECT_FIELDS.INSPECTION_SCHEDULED_DATE]: '2024-05-01',
          [PROJECT_FIELDS.PASSING_INSPECTION_COMPLETED]: '2024-05-05',
        });

        const result = getCalendarDates(project);
        expect(result).toHaveLength(2);
        
        const scheduledEvent = result.find(e => e.id === '123-inspection-scheduled');
        const completedEvent = result.find(e => e.id === '123-inspection-completed');
        
        expect(scheduledEvent?.status).toBe('scheduled');
        expect(completedEvent?.status).toBe('completed');
      });
    });

    describe('PTO Events', () => {
      it('creates PTO submitted event', () => {
        const project = createMockProject({
          [PROJECT_FIELDS.RECORD_ID]: 123,
          [PROJECT_FIELDS.PROJECT_ID]: 'KIN-001',
          [PROJECT_FIELDS.CUSTOMER_NAME]: 'John Smith',
          [PROJECT_FIELDS.PTO_SUBMITTED]: '2024-06-01',
        });

        const result = getCalendarDates(project);
        expect(result).toHaveLength(1);
        expect(result[0].id).toBe('123-pto-submitted');
        expect(result[0].type).toBe('pto');
        expect(result[0].status).toBe('completed');
        expect(result[0].date).toBe('2024-06-01');
      });

      it('creates PTO approved event', () => {
        const project = createMockProject({
          [PROJECT_FIELDS.RECORD_ID]: 123,
          [PROJECT_FIELDS.PROJECT_ID]: 'KIN-001',
          [PROJECT_FIELDS.CUSTOMER_NAME]: 'John Smith',
          [PROJECT_FIELDS.PTO_APPROVED]: '2024-06-10',
        });

        const result = getCalendarDates(project);
        expect(result).toHaveLength(1);
        expect(result[0].id).toBe('123-pto-approved');
        expect(result[0].type).toBe('pto');
        expect(result[0].status).toBe('completed');
        expect(result[0].date).toBe('2024-06-10');
      });

      it('creates both PTO events when both exist', () => {
        const project = createMockProject({
          [PROJECT_FIELDS.RECORD_ID]: 123,
          [PROJECT_FIELDS.PROJECT_ID]: 'KIN-001',
          [PROJECT_FIELDS.CUSTOMER_NAME]: 'John Smith',
          [PROJECT_FIELDS.PTO_SUBMITTED]: '2024-06-01',
          [PROJECT_FIELDS.PTO_APPROVED]: '2024-06-10',
        });

        const result = getCalendarDates(project);
        expect(result).toHaveLength(2);
        
        const submittedEvent = result.find(e => e.id === '123-pto-submitted');
        const approvedEvent = result.find(e => e.id === '123-pto-approved');
        
        expect(submittedEvent?.status).toBe('completed');
        expect(approvedEvent?.status).toBe('completed');
      });
    });

    describe('Multiple Events from Single Project', () => {
      it('creates all event types from fully populated project', () => {
        const project = createMockProject({
          [PROJECT_FIELDS.RECORD_ID]: 123,
          [PROJECT_FIELDS.PROJECT_ID]: 'KIN-001',
          [PROJECT_FIELDS.CUSTOMER_NAME]: 'John Smith',
          [PROJECT_FIELDS.SITE_SURVEY_ARRIVY_SCHEDULED]: '2024-03-15',
          [PROJECT_FIELDS.SURVEY_APPROVED]: '2024-03-20',
          [PROJECT_FIELDS.INSTALL_COMPLETED_DATE]: '2024-04-15',
          [PROJECT_FIELDS.ESTIMATED_INSTALL_DATE]: '2024-04-20',
          [PROJECT_FIELDS.INSPECTION_SCHEDULED_DATE]: '2024-05-01',
          [PROJECT_FIELDS.PASSING_INSPECTION_COMPLETED]: '2024-05-05',
          [PROJECT_FIELDS.PTO_SUBMITTED]: '2024-06-01',
          [PROJECT_FIELDS.PTO_APPROVED]: '2024-06-10',
        });

        const result = getCalendarDates(project);
        expect(result).toHaveLength(8);
        
        // Verify all event types are present
        const eventTypes = result.map(e => e.type);
        expect(eventTypes).toContain('survey');
        expect(eventTypes).toContain('install');
        expect(eventTypes).toContain('inspection');
        expect(eventTypes).toContain('pto');
        
        // Verify all statuses are present
        const statuses = result.map(e => e.status);
        expect(statuses).toContain('scheduled');
        expect(statuses).toContain('completed');
        expect(statuses).toContain('pending');
        
        // Verify all events have same project payload
        result.forEach(event => {
          expect(event.project.recordId).toBe(123);
          expect(event.project.projectId).toBe('KIN-001');
          expect(event.project.customerName).toBe('John Smith');
        });
      });
    });

    describe('Project Payload', () => {
      it('includes all project fields in event payload', () => {
        const project = createMockProject({
          [PROJECT_FIELDS.RECORD_ID]: 123,
          [PROJECT_FIELDS.PROJECT_ID]: 'KIN-001',
          [PROJECT_FIELDS.CUSTOMER_NAME]: 'John Smith',
          [PROJECT_FIELDS.CUSTOMER_ADDRESS]: '123 Main St, Phoenix, AZ',
          [PROJECT_FIELDS.CUSTOMER_PHONE]: '555-0101',
          [PROJECT_FIELDS.SALES_OFFICE]: 'Phoenix',
          [PROJECT_FIELDS.CLOSER_NAME]: 'Test Closer',
          [PROJECT_FIELDS.SETTER_NAME]: 'Test Setter',
          [PROJECT_FIELDS.SITE_SURVEY_ARRIVY_SCHEDULED]: '2024-03-15',
        });

        const result = getCalendarDates(project);
        const event = result[0];
        
        expect(event.project.recordId).toBe(123);
        expect(event.project.projectId).toBe('KIN-001');
        expect(event.project.customerName).toBe('John Smith');
        expect(event.project.customerAddress).toBe('123 Main St, Phoenix, AZ');
        expect(event.project.customerPhone).toBe('555-0101');
        expect(event.project.salesOffice).toBe('Phoenix');
        expect(event.project.closerName).toBe('Test Closer');
        expect(event.project.setterName).toBe('Test Setter');
      });

      it('handles missing optional fields gracefully', () => {
        const project = createMockProject({
          [PROJECT_FIELDS.RECORD_ID]: 123,
          [PROJECT_FIELDS.PROJECT_ID]: 'KIN-001',
          [PROJECT_FIELDS.SITE_SURVEY_ARRIVY_SCHEDULED]: '2024-03-15',
        });

        const result = getCalendarDates(project);
        const event = result[0];
        
        expect(event.project.recordId).toBe(123);
        expect(event.project.projectId).toBe('KIN-001');
        expect(event.project.customerName).toBeNull();
        expect(event.project.customerAddress).toBeNull();
        expect(event.project.customerPhone).toBeNull();
        expect(event.project.salesOffice).toBeNull();
        expect(event.project.closerName).toBeNull();
        expect(event.project.setterName).toBeNull();
      });
    });

    describe('Edge Cases', () => {
      it('handles empty string dates', () => {
        const project = createMockProject({
          [PROJECT_FIELDS.RECORD_ID]: 123,
          [PROJECT_FIELDS.PROJECT_ID]: 'KIN-001',
          [PROJECT_FIELDS.SITE_SURVEY_ARRIVY_SCHEDULED]: '',
          [PROJECT_FIELDS.SURVEY_APPROVED]: '',
          [PROJECT_FIELDS.INSTALL_SCHEDULED_DATE_CAPTURE]: '',
        });

        const result = getCalendarDates(project);
        expect(result).toEqual([]);
      });

      it('handles whitespace-only dates', () => {
        const project = createMockProject({
          [PROJECT_FIELDS.RECORD_ID]: 123,
          [PROJECT_FIELDS.PROJECT_ID]: 'KIN-001',
          [PROJECT_FIELDS.SITE_SURVEY_ARRIVY_SCHEDULED]: '   ',
          [PROJECT_FIELDS.SURVEY_APPROVED]: '   ',
          [PROJECT_FIELDS.INSTALL_SCHEDULED_DATE_CAPTURE]: '   ',
        });

        const result = getCalendarDates(project);
        expect(result).toEqual([]);
      });

      it('handles projects with only required fields', () => {
        const project = createMockProject({
          [PROJECT_FIELDS.RECORD_ID]: 123,
          [PROJECT_FIELDS.PROJECT_ID]: 'KIN-001',
        });

        const result = getCalendarDates(project);
        expect(result).toEqual([]);
      });

      it('handles very long customer names', () => {
        const longName = 'A'.repeat(200);
        const project = createMockProject({
          [PROJECT_FIELDS.RECORD_ID]: 123,
          [PROJECT_FIELDS.PROJECT_ID]: 'KIN-001',
          [PROJECT_FIELDS.CUSTOMER_NAME]: longName,
          [PROJECT_FIELDS.SITE_SURVEY_ARRIVY_SCHEDULED]: '2024-03-15',
        });

        const result = getCalendarDates(project);
        expect(result).toHaveLength(1);
        expect(result[0].title).toContain(longName);
      });
    });
  });
});
