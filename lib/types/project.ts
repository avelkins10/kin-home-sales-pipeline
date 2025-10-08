// lib/types/project.ts

// Generic type for Quickbase field response format per official REST API spec
export interface QuickbaseField<T = any> {
  value: T;
  type?: string;
}

// Type for raw project data from Quickbase API
export interface QuickbaseProject {
  [fieldId: number]: QuickbaseField<any>;
}

// Typed project object with named properties
export interface Project {
  // Core Identity
  recordId: number;
  projectId: string;
  projectStatus: string | null;
  salesDate: string | null;
  projectAge: number | null;

  // Customer Contact
  customerName: string | null;
  customerAddress: string | null;
  customerPhone: string | null;
  customerEmail: string | null;
  customerCity: string | null;
  customerState: string | null;
  customerZip: string | null;

  // System Specs
  systemSizeKw: number | null;
  systemPrice: number | null;
  numberOfPanels: number | null;
  module: string | null;
  inverter: string | null;

  // Battery
  batteryModel: string | null;
  batteryQuantity: number | null;

  // PPW (Price Per Watt)
  grossPpw: number | null;
  netPpw: number | null;
  commissionablePpw: number | null;
  totalAdderPpw: number | null;

  // Team Members
  closerId: string | null;
  closerName: string | null;
  setterId: string | null;
  setterName: string | null;
  projectCoordinatorId: string | null;
  projectCoordinator: string | null;

  // Office/Region
  salesOffice: string | null;
  ahj: string | null;

  // Holds & Blockers
  onHold: boolean | null;
  holdReason: string | null;
  blockReason: string | null;
  dateOnHold: string | null;
  userPlacedOnHold: string | null;

  // Adders
  totalAdders: number | null;
  totalAdderCost: number | null;
  salesFacingAdderList: string | null;
  numApprovedAdders: number | null;
  numNeedsReviewAdders: number | null;

  // MILESTONE 1: Intake
  intakeInstallDateTentative: string | null;

  // MILESTONE 2: Survey
  surveySubmitted: string | null;
  surveyApproved: string | null;
  maxSurveySubmitted: string | null;
  maxSurveyApproved: string | null;

  // MILESTONE 3: Design
  predesignApproved: string | null;
  designCompleted: string | null;
  cadDesignApproved: string | null;
  cadDesignSubmitted: string | null;
  engineeringCompleted: string | null;
  designSlaDeadline: string | null;

  // MILESTONE 4: HOA
  hoaApplicationSubmitted: string | null;
  hoaApplicationApproved: string | null;

  // MILESTONE 5: Permit
  permitSubmitted: string | null;
  permitApproved: string | null;
  asBuiltSubmittedToAhj: string | null;
  estimatedPermitReturnDate: string | null;
  permitSubmittedDateCapture: string | null;
  permitResubmitted: string | null;

  // MILESTONE 6: NEM
  nemSignaturesSent: string | null;
  nemSignatureReceived: string | null;
  nemSubmitted: string | null;
  nemApproved: string | null;
  nemSubmittedCaptured: string | null;
  nemApprovedCaptured: string | null;
  interconnectionSignaturesSent: string | null;

  // MILESTONE 7: Install
  installScheduledDateCapture: string | null;
  installCompletedDate: string | null;
  readyForCommission: string | null;
  installDateImport: string | null;
  estimatedInstallDate: string | null;
  installationCompletedAt: string | null;
  installScheduledStartDate: string | null;
  installStartedDate: string | null;
  installFundingSubmitted: string | null;
  installFundingReceived: string | null;

  // MILESTONE 8: Verification (calculated)

  // MILESTONE 9: Inspection
  passingInspectionCompleted: string | null;
  inspectionScheduledDate: string | null;
  firstInspectionScheduled: string | null;

  // MILESTONE 10: PTO
  ptoSubmitted: string | null;
  ptoApproved: string | null;
  ptoUploadedToLender: string | null;

  // Status/Visual
  projectPriority: string | null;
  statusBarHtml: string | null;
}

/**
 * User role hierarchy and visibility rules:
 * 
 * User-based visibility (sees only assigned projects):
 * - closer: Access to own projects as closer
 * - setter: Access to own projects as setter
 * - team_lead: Access to managed reps' projects (both closer and setter projects)
 * 
 * Office-based visibility (sees ALL projects in assigned offices):
 * - office_leader: Access to ALL projects in assigned offices (regardless of user active status)
 * - area_director: Access to ALL projects in assigned offices (same as office_leader, typically manages multiple offices)
 * - divisional: Access to ALL projects in assigned offices/regions (larger scope than area_director)
 * - regional: Access to ALL projects across all offices
 * 
 * Admin access:
 * - super_admin: Access to all projects and admin features
 * 
 * Important: Office-based roles (office_leader, area_director, divisional) filter by 
 * project.sales_office, NOT by whether the closer/setter has an active account. 
 * This means managers see ALL projects in their offices, including projects where 
 * the rep doesn't have an active account in the app.
 */
export type UserRole = 
  | 'closer' 
  | 'setter' 
  | 'team_lead' 
  | 'office_leader' 
  | 'area_director' 
  | 'divisional' 
  | 'regional' 
  | 'super_admin';

export type ProjectStatus = 'Active' | 'On Hold' | 'Completed' | 'Cancelled';

export type ProjectPriority = 'Insane' | 'Urgent' | 'Normal';

export type MilestoneStatus = 'complete' | 'in-progress' | 'pending' | 'blocked' | 'upcoming';
