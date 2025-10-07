// lib/constants/fieldIds.ts

export const PROJECT_FIELDS = {
  // Core Identity (5 fields)
  RECORD_ID: 3,                    // ✅ PRIMARY - Unique Quickbase record ID
  PROJECT_ID: 11,                  // ✅ PRIMARY - Human-readable project number
  PROJECT_STATUS: 255,             // ✅ PRIMARY - Active, Completed, Cancelled, On Hold
  SALES_DATE: 522,                 // ✅ PRIMARY - 100% - When project was sold
  PROJECT_AGE: 438,                // ✅ CALCULATED - Days since sales date

  // Customer Contact (7 fields)
  CUSTOMER_NAME: 145,              // ✅ PRIMARY - 100% - Full name
  CUSTOMER_ADDRESS: 146,           // ✅ PRIMARY - 100% - Street address
  CUSTOMER_PHONE: 148,             // ✅ PRIMARY - 100% - CRITICAL for call/text
  CUSTOMER_EMAIL: 147,             // ✅ PRIMARY - ~95% - CRITICAL for communication
  CUSTOMER_CITY: 149,              // ✅ PRIMARY - 100%
  CUSTOMER_STATE: 150,             // ✅ PRIMARY - 100%
  CUSTOMER_ZIP: 151,               // ✅ PRIMARY - 100%

  // System Specs (5 fields)
  SYSTEM_SIZE_KW: 13,              // ✅ PRIMARY - 100% - Total kW
  SYSTEM_PRICE: 133,               // ✅ PRIMARY - 100% - Total price
  NUMBER_OF_PANELS: 260,           // ✅ SECONDARY - Panel count
  MODULE: 127,                     // ✅ SECONDARY - Panel manufacturer/model
  INVERTER: 128,                   // ✅ SECONDARY - Inverter model

  // Battery (2 fields)
  BATTERY_MODEL: 1099,             // ✅ SECONDARY - Battery type
  BATTERY_QUANTITY: 1098,          // ✅ SECONDARY - Number of batteries

  // PPW (Price Per Watt) (6 fields)
  GROSS_PPW: 19,                   // ✅ PRIMARY - 100% - Before incentives
  NET_PPW: 543,                    // ✅ PRIMARY - 100% - After incentives
  COMMISSIONABLE_PPW: 2113,        // ✅ PRIMARY - 100% - Rep commission basis
  TOTAL_ADDER_PPW: 2114,           // ✅ SECONDARY - Adder impact on PPW
  SOLD_GROSS_PPW: 2292,            // ✅ PRIMARY - 100% - Sold PPW (from quickbase-config.json)
  SOLD_NET_PPW: 2293,              // ✅ PRIMARY - 100% - Sold Net PPW (from quickbase-config.json)

  // Team Members (6 fields)
  CLOSER_ID: 516,                  // ✅ PRIMARY - 100% - User object
  CLOSER_NAME: 517,                // ✅ PRIMARY - 100% - Display name
  SETTER_ID: 329,                  // ✅ SECONDARY - 82% - User object
  SETTER_NAME: 330,                // ✅ SECONDARY - 82% - Display name
  PROJECT_COORDINATOR_ID: 819,     // ✅ SECONDARY - 67% - User object
  PROJECT_COORDINATOR: 820,        // ✅ SECONDARY - 67% - Display name

  // Office/Region (2 fields)
  SALES_OFFICE: 2087,              // ✅ PRIMARY - 100% - For office filtering
  AHJ: 1123,                       // ✅ PRIMARY - 100% - Permitting jurisdiction

  // Holds & Blockers (5 fields)
  ON_HOLD: 231,                    // ✅ PRIMARY - 100% - Boolean checkbox
  HOLD_REASON: 232,                // ✅ PRIMARY - 21% - Why project stopped
  BLOCK_REASON: 233,               // ✅ SECONDARY - 15% - Specific blocker
  DATE_ON_HOLD: 235,               // ✅ PRIMARY - 21% - When hold started
  USER_PLACED_ON_HOLD: 234,        // ✅ SECONDARY - 21% - Who placed hold

  // Adders (5 fields)
  TOTAL_ADDERS: 252,               // ✅ PRIMARY - 100% - Count of adders
  TOTAL_ADDER_COST: 2115,          // ✅ PRIMARY - 92% - Total adder value
  SALES_FACING_ADDER_LIST: 2286,   // ✅ PRIMARY - 92% - Comma-separated list
  NUM_APPROVED_ADDERS: 1046,       // ✅ SECONDARY - 67% - Approved count
  NUM_NEEDS_REVIEW_ADDERS: 2282,   // ✅ PRIMARY - 67% - CRITICAL bottleneck!

  // MILESTONE 1: Intake (1 field)
  INTAKE_INSTALL_DATE_TENTATIVE: 902, // ✅ PRIMARY - 100% - Always filled at sale

  // MILESTONE 2: Survey (4 fields)
  SURVEY_SUBMITTED: 164,           // ✅ PRIMARY - 26.5% - Survey sent to ops
  SURVEY_APPROVED: 165,            // ✅ PRIMARY - 25.2% - Survey approved
  SURVEY_SCHEDULED_DATE: 164,      // ✅ PRIMARY - Used by traffic lights (same as SURVEY_SUBMITTED)
  SURVEY_COMPLETED: 165,           // ✅ PRIMARY - Used by traffic lights (same as SURVEY_APPROVED)
  MAX_SURVEY_SUBMITTED: 575,       // 🔄 BACKUP - 26.5% - Timestamp version
  MAX_SURVEY_APPROVED: 572,        // 🔄 BACKUP - 25.2% - Timestamp version

  // MILESTONE 3: Design (9 fields)
  PREDESIGN_APPROVED: 316,         // ✅ PRIMARY - 27.2% - HIGHEST design field!
  DESIGN_COMPLETED: 315,           // ✅ PRIMARY - 26.2% - Final design done
  CAD_DESIGN_APPROVED: 476,        // ✅ PRIMARY - 26.2% - CAD approved
  CAD_DESIGN_SUBMITTED: 475,       // 🔄 BACKUP - 27.5% - Timestamp version
  ENGINEERING_COMPLETED: 2458,     // ✅ SECONDARY - 27.0% - PE stamp complete
  DESIGN_SLA_DEADLINE: 2459,       // ⏰ SLA - 25.2% - Track design delays

  // MILESTONE 4: HOA (2 fields) - CONDITIONAL
  HOA_APPLICATION_SUBMITTED: 212,  // ✅ PRIMARY - 10.7% - Only if HOA req'd
  HOA_APPLICATION_APPROVED: 213,   // ✅ PRIMARY - 10.7% - Only if HOA req'd

  // MILESTONE 5: Permit (6 fields)
  PERMIT_SUBMITTED: 207,           // ✅ PRIMARY - 23.5%
  PERMIT_APPROVED: 208,            // ✅ PRIMARY - 21.8%
  PERMIT_SUBMITTED_DATE: 207,      // ✅ PRIMARY - Used by traffic lights (same as PERMIT_SUBMITTED)
  AS_BUILT_SUBMITTED_TO_AHJ: 614,  // ✅ PRIMARY - 25.8% - HIGHEST permit usage!
  ESTIMATED_PERMIT_RETURN_DATE: 1777, // ✅ PRIMARY - 23.5% - Timeline planning
  PERMIT_SUBMITTED_DATE_CAPTURE: 709, // 🔄 BACKUP - 23.2%
  PERMIT_RESUBMITTED: 205,         // ⚠️ EDGE - 6.7% - Track rejections

  // MILESTONE 6: NEM (8 fields)
  NEM_SIGNATURES_SENT: 1844,       // ✅ PRIMARY - 21.8% - DocuSign sent
  NEM_SIGNATURE_RECEIVED: 1845,    // ✅ SECONDARY - 12.8% - Customer signed
  NEM_SUBMITTED: 326,              // ✅ PRIMARY - 24.2% - Submitted to utility
  NEM_APPROVED: 327,               // ✅ PRIMARY - 22.8% - Utility approved
  NEM_SUBMITTED_DATE: 326,         // ✅ PRIMARY - Used by traffic lights (same as NEM_SUBMITTED)
  NEM_SUBMITTED_CAPTURED: 716,     // 🔄 BACKUP - 24.2%
  NEM_APPROVED_CAPTURED: 585,      // 🔄 BACKUP - 22.5%
  INTERCONNECTION_SIGNATURES_SENT: 2198, // ✅ SECONDARY - 16.1% - PTO signatures

  // MILESTONE 7: Install (10 fields)
  INSTALL_SCHEDULED_DATE_CAPTURE: 710, // ✅ PRIMARY - 54.4% - HIGHEST usage!
  INSTALL_COMPLETED_DATE: 534,     // ✅ PRIMARY - 52.3% - Install done
  READY_FOR_COMMISSION: 813,       // ✅ PRIMARY - 47.0% - System ready!
  INSTALL_DATE_IMPORT: 835,        // 🔄 BACKUP - 44.6% - Alternate date
  ESTIMATED_INSTALL_DATE: 1124,    // 🔄 BACKUP - 27.5% - Planning field
  INSTALLATION_COMPLETED_AT: 587,  // 🔄 BACKUP - 52.3% - Timestamp
  INSTALLATION_COMPLETED: 587,     // ✅ PRIMARY - Used by traffic lights (same as INSTALLATION_COMPLETED_AT)
  INSTALL_COMPLETE_IMPORT: 835,    // ✅ PRIMARY - Used by traffic lights (same as INSTALL_DATE_IMPORT)
  INSTALL_SCHEDULED_START_DATE: 178, // 🔄 BACKUP - 20.1% - Start date
  INSTALL_STARTED_DATE: 464,       // ✅ SECONDARY - 19.8% - Crew arrived
  INSTALL_FUNDING_SUBMITTED: 486,  // ✅ SECONDARY - 18.8% - M2 request
  INSTALL_FUNDING_RECEIVED: 487,   // ✅ SECONDARY - 13.8% - M2 funded

  // MILESTONE 8: Verification (NO DIRECT FIELD - CALCULATED)
  // Estimate 1-3 days after install complete

  // MILESTONE 9: Inspection (2 fields)
  PASSING_INSPECTION_COMPLETED: 491, // ✅ PRIMARY - 19.1% - Passed inspection
  INSPECTION_SCHEDULED_DATE: 226,  // ⚠️ EDGE - 9.7% - Scheduled only
  FIRST_INSPECTION_SCHEDULED: 1589, // 🔄 BACKUP - 9.7%

  // MILESTONE 10: PTO (4 fields)
  PTO_SUBMITTED: 537,              // ✅ PRIMARY - 21.1% - Submitted to utility
  PTO_APPROVED: 538,               // ✅ PRIMARY - 20.5% - FINAL MILESTONE!
  PTO_UPLOADED_TO_LENDER: 556,     // ✅ SECONDARY - 19.8% - M3 payment!

  // Status/Visual (2 fields)
  PROJECT_PRIORITY: 300,           // ✅ PRIMARY - 100% - Insane/Urgent/Normal
  STATUS_BAR_HTML: 301,            // ✅ SECONDARY - 100% - Visual timeline

  // Reschedule tracking (1 field)
  RESCHEDULE_COUNT: 9999,          // ⚠️ PLACEHOLDER - Field may not exist in Quickbase
} as const;

// Type helper
export type ProjectFieldId = typeof PROJECT_FIELDS[keyof typeof PROJECT_FIELDS];
