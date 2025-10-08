// Quickbase Notes Table Field IDs
// Table: bsb6bqt3b

export const NOTE_FIELDS = {
  RECORD_ID: 3,
  NOTE_CONTENT: 6,
  CATEGORY: 7,
  CREATED_BY: 8,
  DATE_CREATED: 9,
  SALES_TEAM_FLAG: 10,
  RELATED_PROJECT: 13,
  NOTE_TYPE: 39,
  REP_VISIBLE: 141,
} as const;

export const NOTE_CATEGORIES = {
  SALES: 'Sales',
  ACCEPTANCE: 'Acceptance',
  SURVEY: 'Survey',
  DESIGN: 'Design',
  NEM: 'NEM',
  PERMITTING: 'Permitting',
  HOA: 'HOA',
  INSTALLATION: 'Installation',
  VERIFICATION: 'Verification',
  INSPECTION: 'Inspection',
  PTO: 'PTO',
  COMMISSIONING: 'Commissioning',
} as const;

export const REP_VISIBLE_FLAG = 'Rep Visible';
