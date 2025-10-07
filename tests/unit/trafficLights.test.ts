import { describe, it, expect } from 'vitest';
import { calculateMilestoneState, getCurrentMilestoneId, getMilestoneStatusText } from '@/lib/utils/traffic-lights';
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

describe('Traffic Lights Utilities', () => {
  describe('calculateMilestoneState - intake', () => {
    it('returns complete when survey approved', () => {
      const project = createMockProject({
        [PROJECT_FIELDS.SURVEY_APPROVED]: '2024-01-15',
        [PROJECT_FIELDS.PROJECT_AGE]: '5',
      });

      const result = calculateMilestoneState(project, 'intake');
      expect(result).toBe('complete');
    });

    it('returns overdue when project age > 7 days', () => {
      const project = createMockProject({
        [PROJECT_FIELDS.SURVEY_APPROVED]: '',
        [PROJECT_FIELDS.PROJECT_AGE]: '10',
      });

      const result = calculateMilestoneState(project, 'intake');
      expect(result).toBe('overdue');
    });

    it('returns in-progress when project age <= 7 days', () => {
      const project = createMockProject({
        [PROJECT_FIELDS.SURVEY_APPROVED]: '',
        [PROJECT_FIELDS.PROJECT_AGE]: '5',
      });

      const result = calculateMilestoneState(project, 'intake');
      expect(result).toBe('in-progress');
    });

    it('returns on-hold when project status contains hold keyword', () => {
      const project = createMockProject({
        [PROJECT_FIELDS.PROJECT_STATUS]: 'On Hold',
        [PROJECT_FIELDS.SURVEY_APPROVED]: '',
        [PROJECT_FIELDS.PROJECT_AGE]: '5',
      });

      const result = calculateMilestoneState(project, 'intake');
      expect(result).toBe('on-hold');
    });
  });

  describe('calculateMilestoneState - survey', () => {
    it('returns pending when survey not started', () => {
      const project = createMockProject({
        [PROJECT_FIELDS.SURVEY_APPROVED]: '',
        [PROJECT_FIELDS.SURVEY_SUBMITTED]: '',
      });

      const result = calculateMilestoneState(project, 'survey');
      expect(result).toBe('pending');
    });

    it('returns in-progress when submitted but not approved', () => {
      const project = createMockProject({
        [PROJECT_FIELDS.SURVEY_APPROVED]: '',
        [PROJECT_FIELDS.SURVEY_SUBMITTED]: '2024-01-15',
      });

      const result = calculateMilestoneState(project, 'survey');
      expect(result).toBe('in-progress');
    });

    it('returns complete when approved', () => {
      const project = createMockProject({
        [PROJECT_FIELDS.SURVEY_APPROVED]: '2024-01-15',
        [PROJECT_FIELDS.SURVEY_SUBMITTED]: '2024-01-10',
      });

      const result = calculateMilestoneState(project, 'survey');
      expect(result).toBe('complete');
    });
  });

  describe('calculateMilestoneState - design', () => {
    it('returns pending when survey not approved', () => {
      const project = createMockProject({
        [PROJECT_FIELDS.SURVEY_APPROVED]: '',
        [PROJECT_FIELDS.CAD_DESIGN_APPROVED]: '',
        [PROJECT_FIELDS.DESIGN_COMPLETED]: '',
        [PROJECT_FIELDS.PROJECT_AGE]: '5',
      });

      const result = calculateMilestoneState(project, 'design');
      expect(result).toBe('pending');
    });

    it('returns in-progress when survey approved but design not complete', () => {
      const project = createMockProject({
        [PROJECT_FIELDS.SURVEY_APPROVED]: '2024-01-15',
        [PROJECT_FIELDS.CAD_DESIGN_APPROVED]: '',
        [PROJECT_FIELDS.DESIGN_COMPLETED]: '',
        [PROJECT_FIELDS.PROJECT_AGE]: '5',
      });

      const result = calculateMilestoneState(project, 'design');
      expect(result).toBe('in-progress');
    });

    it('returns overdue when project age > 21 days', () => {
      const project = createMockProject({
        [PROJECT_FIELDS.SURVEY_APPROVED]: '2024-01-15',
        [PROJECT_FIELDS.CAD_DESIGN_APPROVED]: '',
        [PROJECT_FIELDS.DESIGN_COMPLETED]: '',
        [PROJECT_FIELDS.PROJECT_AGE]: '25',
      });

      const result = calculateMilestoneState(project, 'design');
      expect(result).toBe('overdue');
    });

    it('returns complete when CAD design approved', () => {
      const project = createMockProject({
        [PROJECT_FIELDS.SURVEY_APPROVED]: '2024-01-15',
        [PROJECT_FIELDS.CAD_DESIGN_APPROVED]: '2024-01-20',
        [PROJECT_FIELDS.DESIGN_COMPLETED]: '2024-01-18',
        [PROJECT_FIELDS.PROJECT_AGE]: '5',
      });

      const result = calculateMilestoneState(project, 'design');
      expect(result).toBe('complete');
    });
  });

  describe('calculateMilestoneState - NEM', () => {
    it('returns pending when design not approved', () => {
      const project = createMockProject({
        [PROJECT_FIELDS.CAD_DESIGN_APPROVED]: '',
        [PROJECT_FIELDS.NEM_APPROVED]: '',
        [PROJECT_FIELDS.NEM_SUBMITTED]: '',
      });

      const result = calculateMilestoneState(project, 'nem');
      expect(result).toBe('pending');
    });

    it('returns in-progress when design approved but NEM not submitted', () => {
      const project = createMockProject({
        [PROJECT_FIELDS.CAD_DESIGN_APPROVED]: '2024-01-15',
        [PROJECT_FIELDS.NEM_APPROVED]: '',
        [PROJECT_FIELDS.NEM_SUBMITTED]: '',
      });

      const result = calculateMilestoneState(project, 'nem');
      expect(result).toBe('in-progress');
    });

    it('returns in-progress when NEM submitted but not approved', () => {
      const project = createMockProject({
        [PROJECT_FIELDS.CAD_DESIGN_APPROVED]: '2024-01-15',
        [PROJECT_FIELDS.NEM_APPROVED]: '',
        [PROJECT_FIELDS.NEM_SUBMITTED]: '2024-01-20',
      });

      const result = calculateMilestoneState(project, 'nem');
      expect(result).toBe('in-progress');
    });

    it('returns complete when NEM approved', () => {
      const project = createMockProject({
        [PROJECT_FIELDS.CAD_DESIGN_APPROVED]: '2024-01-15',
        [PROJECT_FIELDS.NEM_APPROVED]: '2024-01-25',
        [PROJECT_FIELDS.NEM_SUBMITTED]: '2024-01-20',
      });

      const result = calculateMilestoneState(project, 'nem');
      expect(result).toBe('complete');
    });
  });

  describe('calculateMilestoneState - permit', () => {
    it('returns pending when design not approved', () => {
      const project = createMockProject({
        [PROJECT_FIELDS.CAD_DESIGN_APPROVED]: '',
        [PROJECT_FIELDS.PERMIT_APPROVED]: '',
        [PROJECT_FIELDS.PERMIT_SUBMITTED]: '',
      });

      const result = calculateMilestoneState(project, 'permit');
      expect(result).toBe('pending');
    });

    it('returns in-progress when design approved but permit not submitted', () => {
      const project = createMockProject({
        [PROJECT_FIELDS.CAD_DESIGN_APPROVED]: '2024-01-15',
        [PROJECT_FIELDS.NEM_APPROVED]: '2024-01-18',
        [PROJECT_FIELDS.PERMIT_APPROVED]: '',
        [PROJECT_FIELDS.PERMIT_SUBMITTED]: '',
      });

      const result = calculateMilestoneState(project, 'permit');
      expect(result).toBe('in-progress');
    });

    it('returns in-progress when permit submitted but not approved', () => {
      const project = createMockProject({
        [PROJECT_FIELDS.CAD_DESIGN_APPROVED]: '2024-01-15',
        [PROJECT_FIELDS.NEM_APPROVED]: '2024-01-18',
        [PROJECT_FIELDS.PERMIT_APPROVED]: '',
        [PROJECT_FIELDS.PERMIT_SUBMITTED]: '2024-01-20',
      });

      const result = calculateMilestoneState(project, 'permit');
      expect(result).toBe('in-progress');
    });

    it('returns complete when permit approved', () => {
      const project = createMockProject({
        [PROJECT_FIELDS.CAD_DESIGN_APPROVED]: '2024-01-15',
        [PROJECT_FIELDS.NEM_APPROVED]: '2024-01-18',
        [PROJECT_FIELDS.PERMIT_APPROVED]: '2024-01-25',
        [PROJECT_FIELDS.PERMIT_SUBMITTED]: '2024-01-20',
      });

      const result = calculateMilestoneState(project, 'permit');
      expect(result).toBe('complete');
    });
  });

  describe('calculateMilestoneState - install', () => {
    it('returns pending when NEM and permit not approved', () => {
      const project = createMockProject({
        [PROJECT_FIELDS.NEM_APPROVED]: '',
        [PROJECT_FIELDS.PERMIT_APPROVED]: '',
        [PROJECT_FIELDS.INSTALL_COMPLETED_DATE]: '',
        [PROJECT_FIELDS.INSTALLATION_COMPLETED_AT]: '',
        [PROJECT_FIELDS.INSTALL_SCHEDULED_DATE_CAPTURE]: '',
        [PROJECT_FIELDS.ESTIMATED_INSTALL_DATE]: '',
        [PROJECT_FIELDS.INSTALL_SCHEDULED_START_DATE]: '',
      });

      const result = calculateMilestoneState(project, 'install');
      expect(result).toBe('pending');
    });

    it('returns in-progress when scheduled', () => {
      const project = createMockProject({
        [PROJECT_FIELDS.NEM_APPROVED]: '2024-01-15',
        [PROJECT_FIELDS.PERMIT_APPROVED]: '2024-01-15',
        [PROJECT_FIELDS.INSTALL_COMPLETED_DATE]: '',
        [PROJECT_FIELDS.INSTALLATION_COMPLETED_AT]: '',
        [PROJECT_FIELDS.INSTALL_SCHEDULED_DATE_CAPTURE]: '2024-01-20',
        [PROJECT_FIELDS.ESTIMATED_INSTALL_DATE]: '',
        [PROJECT_FIELDS.INSTALL_SCHEDULED_START_DATE]: '',
      });

      const result = calculateMilestoneState(project, 'install');
      expect(result).toBe('in-progress');
    });

    it('returns complete when install completed', () => {
      const project = createMockProject({
        [PROJECT_FIELDS.NEM_APPROVED]: '2024-01-15',
        [PROJECT_FIELDS.PERMIT_APPROVED]: '2024-01-15',
        [PROJECT_FIELDS.INSTALL_COMPLETED_DATE]: '2024-01-20',
        [PROJECT_FIELDS.INSTALLATION_COMPLETED_AT]: '',
        [PROJECT_FIELDS.INSTALL_SCHEDULED_DATE_CAPTURE]: '',
        [PROJECT_FIELDS.ESTIMATED_INSTALL_DATE]: '',
        [PROJECT_FIELDS.INSTALL_SCHEDULED_START_DATE]: '',
      });

      const result = calculateMilestoneState(project, 'install');
      expect(result).toBe('complete');
    });

    it('handles multiple install date fields (INSTALL_COMPLETED_DATE, INSTALLATION_COMPLETED_AT)', () => {
      const project = createMockProject({
        [PROJECT_FIELDS.NEM_APPROVED]: '2024-01-15',
        [PROJECT_FIELDS.PERMIT_APPROVED]: '2024-01-15',
        [PROJECT_FIELDS.INSTALL_COMPLETED_DATE]: '',
        [PROJECT_FIELDS.INSTALLATION_COMPLETED_AT]: '2024-01-20',
        [PROJECT_FIELDS.INSTALL_SCHEDULED_DATE_CAPTURE]: '',
        [PROJECT_FIELDS.ESTIMATED_INSTALL_DATE]: '',
        [PROJECT_FIELDS.INSTALL_SCHEDULED_START_DATE]: '',
      });

      const result = calculateMilestoneState(project, 'install');
      expect(result).toBe('complete');
    });
  });

  describe('calculateMilestoneState - inspection', () => {
    it('returns pending when install not complete', () => {
      const project = createMockProject({
        [PROJECT_FIELDS.INSTALL_COMPLETED_DATE]: '',
        [PROJECT_FIELDS.INSTALLATION_COMPLETED_AT]: '',
        [PROJECT_FIELDS.PTO_APPROVED]: '',
        [PROJECT_FIELDS.PASSING_INSPECTION_COMPLETED]: '',
      });

      const result = calculateMilestoneState(project, 'inspection');
      expect(result).toBe('pending');
    });

    it('returns in-progress when install complete but inspection not passed', () => {
      const project = createMockProject({
        [PROJECT_FIELDS.INSTALL_COMPLETED_DATE]: '2024-01-20',
        [PROJECT_FIELDS.INSTALLATION_COMPLETED_AT]: '',
        [PROJECT_FIELDS.PTO_APPROVED]: '',
        [PROJECT_FIELDS.PASSING_INSPECTION_COMPLETED]: '',
      });

      const result = calculateMilestoneState(project, 'inspection');
      expect(result).toBe('in-progress');
    });

    it('returns complete when PTO approved', () => {
      const project = createMockProject({
        [PROJECT_FIELDS.INSTALL_COMPLETED_DATE]: '2024-01-20',
        [PROJECT_FIELDS.INSTALLATION_COMPLETED_AT]: '',
        [PROJECT_FIELDS.PTO_APPROVED]: '2024-01-25',
        [PROJECT_FIELDS.PASSING_INSPECTION_COMPLETED]: '2024-01-22',
      });

      const result = calculateMilestoneState(project, 'inspection');
      expect(result).toBe('complete');
    });
  });

  describe('getCurrentMilestoneId', () => {
    it('returns intake for new project', () => {
      const project = createMockProject({
        [PROJECT_FIELDS.SURVEY_APPROVED]: '',
        [PROJECT_FIELDS.CAD_DESIGN_APPROVED]: '',
        [PROJECT_FIELDS.NEM_APPROVED]: '',
        [PROJECT_FIELDS.PERMIT_APPROVED]: '',
        [PROJECT_FIELDS.INSTALL_COMPLETED_DATE]: '',
        [PROJECT_FIELDS.PTO_APPROVED]: '',
      });

      const result = getCurrentMilestoneId(project);
      expect(result).toBe('intake');
    });

    it('returns survey when intake complete', () => {
      const project = createMockProject({
        [PROJECT_FIELDS.SURVEY_APPROVED]: '',
        [PROJECT_FIELDS.SURVEY_SUBMITTED]: '2024-01-10',
        [PROJECT_FIELDS.CAD_DESIGN_APPROVED]: '',
        [PROJECT_FIELDS.NEM_APPROVED]: '',
        [PROJECT_FIELDS.PERMIT_APPROVED]: '',
        [PROJECT_FIELDS.INSTALL_COMPLETED_DATE]: '',
        [PROJECT_FIELDS.PTO_APPROVED]: '',
      });

      const result = getCurrentMilestoneId(project);
      expect(result).toBe('survey');
    });

    it('returns design when survey complete', () => {
      const project = createMockProject({
        [PROJECT_FIELDS.SURVEY_APPROVED]: '2024-01-15',
        [PROJECT_FIELDS.CAD_DESIGN_APPROVED]: '',
        [PROJECT_FIELDS.NEM_APPROVED]: '',
        [PROJECT_FIELDS.PERMIT_APPROVED]: '',
        [PROJECT_FIELDS.INSTALL_COMPLETED_DATE]: '',
        [PROJECT_FIELDS.PTO_APPROVED]: '',
      });

      const result = getCurrentMilestoneId(project);
      expect(result).toBe('design');
    });

    it('returns first in-progress milestone (checks in reverse order)', () => {
      const project = createMockProject({
        [PROJECT_FIELDS.SURVEY_APPROVED]: '2024-01-15',
        [PROJECT_FIELDS.CAD_DESIGN_APPROVED]: '2024-01-20',
        [PROJECT_FIELDS.NEM_APPROVED]: '2024-01-25',
        [PROJECT_FIELDS.PERMIT_APPROVED]: '2024-01-25',
        [PROJECT_FIELDS.INSTALL_COMPLETED_DATE]: '',
        [PROJECT_FIELDS.PTO_APPROVED]: '',
      });

      const result = getCurrentMilestoneId(project);
      expect(result).toBe('install');
    });
  });

  describe('getMilestoneStatusText', () => {
    it('returns intake text when in intake', () => {
      const project = createMockProject({
        [PROJECT_FIELDS.SURVEY_APPROVED]: '',
      });

      const result = getMilestoneStatusText(project);
      expect(result).toBe('Intake: Scheduling survey');
    });

    it('returns survey scheduled text with date', () => {
      const project = createMockProject({
        [PROJECT_FIELDS.SURVEY_APPROVED]: '',
        [PROJECT_FIELDS.SURVEY_SUBMITTED]: '2024-01-20',
      });

      const result = getMilestoneStatusText(project);
      expect(result).toBe('Survey: Scheduled for 1/20');
    });

    it('returns intake text when survey not yet scheduled', () => {
      const project = createMockProject({
        [PROJECT_FIELDS.SURVEY_APPROVED]: '',
        [PROJECT_FIELDS.SURVEY_SUBMITTED]: '',
        [PROJECT_FIELDS.PROJECT_AGE]: '5',
      });

      const result = getMilestoneStatusText(project);
      expect(result).toBe('Intake: Scheduling survey');
    });

    it('returns NEM submitted text with days waiting', () => {
      const project = createMockProject({
        [PROJECT_FIELDS.SURVEY_APPROVED]: '2024-01-15',
        [PROJECT_FIELDS.CAD_DESIGN_APPROVED]: '2024-01-20',
        [PROJECT_FIELDS.NEM_APPROVED]: '',
        [PROJECT_FIELDS.NEM_SUBMITTED]: '2024-01-25',
      });

      const result = getMilestoneStatusText(project);
      expect(result).toMatch(/NEM: Submitted 1\/25 • \d+d waiting/);
    });

    it('returns permit submitted text with days waiting', () => {
      const project = createMockProject({
        [PROJECT_FIELDS.SURVEY_APPROVED]: '2024-01-15',
        [PROJECT_FIELDS.CAD_DESIGN_APPROVED]: '2024-01-20',
        [PROJECT_FIELDS.NEM_APPROVED]: '2024-01-25',
        [PROJECT_FIELDS.PERMIT_APPROVED]: '',
        [PROJECT_FIELDS.PERMIT_SUBMITTED]: '2024-01-30',
      });

      const result = getMilestoneStatusText(project);
      expect(result).toMatch(/Permit: Submitted 1\/30 • \d+d waiting/);
    });

    it('returns install started text with date', () => {
      const project = createMockProject({
        [PROJECT_FIELDS.SURVEY_APPROVED]: '2024-01-15',
        [PROJECT_FIELDS.CAD_DESIGN_APPROVED]: '2024-01-20',
        [PROJECT_FIELDS.NEM_APPROVED]: '2024-01-25',
        [PROJECT_FIELDS.PERMIT_APPROVED]: '2024-01-25',
        [PROJECT_FIELDS.INSTALL_COMPLETED_DATE]: '',
        [PROJECT_FIELDS.INSTALL_STARTED_DATE]: '2024-02-01',
      });

      const result = getMilestoneStatusText(project);
      expect(result).toBe('Install: Started 2/1');
    });

    it('returns install scheduled text with date', () => {
      const project = createMockProject({
        [PROJECT_FIELDS.SURVEY_APPROVED]: '2024-01-15',
        [PROJECT_FIELDS.CAD_DESIGN_APPROVED]: '2024-01-20',
        [PROJECT_FIELDS.NEM_APPROVED]: '2024-01-25',
        [PROJECT_FIELDS.PERMIT_APPROVED]: '2024-01-25',
        [PROJECT_FIELDS.INSTALL_COMPLETED_DATE]: '',
        [PROJECT_FIELDS.INSTALL_STARTED_DATE]: '',
        [PROJECT_FIELDS.INSTALL_SCHEDULED_DATE_CAPTURE]: '2024-02-01',
      });

      const result = getMilestoneStatusText(project);
      expect(result).toBe('Install: Scheduled for 2/1');
    });

    it('returns inspection passed text with date', () => {
      const project = createMockProject({
        [PROJECT_FIELDS.SURVEY_APPROVED]: '2024-01-15',
        [PROJECT_FIELDS.CAD_DESIGN_APPROVED]: '2024-01-20',
        [PROJECT_FIELDS.NEM_APPROVED]: '2024-01-25',
        [PROJECT_FIELDS.PERMIT_APPROVED]: '2024-01-25',
        [PROJECT_FIELDS.INSTALL_COMPLETED_DATE]: '2024-02-01',
        [PROJECT_FIELDS.PTO_APPROVED]: '',
        [PROJECT_FIELDS.PASSING_INSPECTION_COMPLETED]: '2024-02-05',
      });

      const result = getMilestoneStatusText(project);
      expect(result).toBe('Inspection: Passed 2/5 • Awaiting PTO');
    });
  });
});
