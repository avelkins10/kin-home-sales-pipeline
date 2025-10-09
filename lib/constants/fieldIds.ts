// lib/constants/fieldIds.ts
// Version 2.0 - Comprehensive milestone fields from COMPLETE-FIELD-MAPPING.md

export const PROJECT_FIELDS = {
  // ============================================================
  // CORE IDENTITY & STATUS
  // ============================================================

  // Core Identity (5 fields)
  RECORD_ID: 3,                    // ✅ PRIMARY - Unique Quickbase record ID
  PROJECT_ID: 11,                  // ✅ PRIMARY - Human-readable project number
  PROJECT_STATUS: 255,             // ✅ PRIMARY - 100% - Active, Completed, Cancelled, On Hold
  SALES_DATE: 522,                 // ✅ PRIMARY - 100% - When project was sold
  PROJECT_AGE: 438,                // ✅ CALCULATED - 100% - Days since sales date

  // Global Status & Priority (6 fields)
  PROJECT_STATUS_BAR: 301,         // ✅ PRIMARY - 100% - HTML visual status bar
  PROJECT_STATUS_COLOR: 299,       // ✅ PRIMARY - 100% - HTML formula for color coding
  PROJECT_PRIORITY: 300,           // ✅ PRIMARY - 100% - Insane/Urgent/Normal
  PROJECT_ESCALATION: 302,         // ⚠️ SECONDARY - High/Medium/ARC

  // Customer Contact (7 fields)
  CUSTOMER_NAME: 145,              // ✅ PRIMARY - 100% - Full name
  CUSTOMER_ADDRESS: 146,           // ✅ PRIMARY - 100% - Street address
  CUSTOMER_PHONE: 148,             // ✅ PRIMARY - 100% - CRITICAL for call/text
  CUSTOMER_EMAIL: 147,             // ✅ PRIMARY - ~95% - CRITICAL for communication
  CUSTOMER_CITY: 149,              // ✅ PRIMARY - 100%
  CUSTOMER_STATE: 150,             // ✅ PRIMARY - 100%
  CUSTOMER_ZIP: 151,               // ✅ PRIMARY - 100%

  // System Specs (8 fields)
  SYSTEM_SIZE_KW: 13,              // ✅ PRIMARY - 100% - Total kW
  SYSTEM_PRICE: 133,               // ✅ PRIMARY - 100% - Total price
  NUMBER_OF_PANELS: 16,            // ✅ PRIMARY - Panel count
  MODULE: 131,                     // ✅ SECONDARY - Panel manufacturer/model
  MODULE_BRAND: 368,               // ✅ SECONDARY - Panel brand (e.g., JA Solar)
  INVERTER: 17,                    // ✅ SECONDARY - Inverter model
  INVERTER_BRAND: 370,             // ✅ SECONDARY - Inverter brand (e.g., SolarEdge)
  INVERTER_COUNT: 331,             // ✅ SECONDARY - Number of inverters

  // Battery (2 fields)
  BATTERY_MODEL: 1099,             // ✅ SECONDARY - Battery type
  BATTERY_QUANTITY: 1098,          // ✅ SECONDARY - Number of batteries

  // PPW (Price Per Watt) (6 fields)
  GROSS_PPW: 19,                   // ✅ PRIMARY - 100% - Before incentives
  NET_PPW: 543,                    // ✅ PRIMARY - 100% - After incentives
  COMMISSIONABLE_PPW: 2480,        // ✅ PRIMARY - 94% - Rep commission basis (CORRECTED from 2113)
  TOTAL_ADDER_PPW: 2114,           // ✅ SECONDARY - Adder impact on PPW
  SOLD_GROSS_PPW: 2292,            // ✅ PRIMARY - 0% - Sold PPW
  SOLD_NET_PPW: 2293,              // ✅ PRIMARY - 3% - Sold Net PPW

  // Team Members (10 fields)
  CLOSER_ID: 516,                  // ✅ PRIMARY - 100% - User object
  CLOSER_NAME: 517,                // ✅ PRIMARY - 100% - Display name
  CLOSER_EMAIL: 518,               // ✅ PRIMARY - ~95% - Closer email address
  CLOSER_PHONE: 519,               // ✅ PRIMARY - ~90% - Closer phone number
  SETTER_ID: 329,                  // ✅ SECONDARY - 100% - User object
  SETTER_NAME: 330,                // ✅ SECONDARY - 100% - Display name
  SETTER_EMAIL: 331,               // ✅ SECONDARY - ~80% - Setter email address
  SETTER_PHONE: 332,               // ✅ SECONDARY - ~75% - Setter phone number
  PROJECT_COORDINATOR_ID: 819,     // ✅ SECONDARY - 100% - User object
  PROJECT_COORDINATOR: 820,        // ✅ SECONDARY - 100% - Display name

  // Office/Region (2 fields)
  SALES_OFFICE: 2087,              // ✅ PRIMARY - 100% - For office filtering
  AHJ: 1123,                       // ✅ PRIMARY - 100% - Permitting jurisdiction

  // ============================================================
  // HOLDS & BLOCKERS
  // ============================================================

  // General Hold Fields (6 fields)
  ON_HOLD: 231,                    // ✅ PRIMARY - Checkbox - Boolean hold status
  HOLD_REASON: 232,                // ✅ PRIMARY - 21% - Why project stopped
  BLOCK_REASON: 233,               // ✅ SECONDARY - 15% - Specific blocker
  HOLD_BLOCKERS: 1389,             // ✅ PRIMARY - Multi-line - Detailed description
  DATE_ON_HOLD: 235,               // ✅ PRIMARY - 21% - When hold started
  USER_PLACED_ON_HOLD: 234,        // ✅ SECONDARY - 21% - Who placed hold

  // Specific Hold Types (5 fields)
  INTAKE_HOLD: 1071,               // ⚠️ Checkbox - Intake hold
  WELCOME_CALL_HOLD: 1412,         // ⚠️ Checkbox - Welcome call hold
  SITE_SURVEY_HOLD: 1413,          // ⚠️ Checkbox - Site survey hold
  ENGINEERING_HOLD: 2099,          // ⚠️ Checkbox - Engineering hold
  PERMITTING_HOLD: 2100,           // ⚠️ Checkbox - Permitting hold

  // ============================================================
  // ADDERS
  // ============================================================

  // Adder Counts (7 fields)
  TOTAL_ADDERS: 252,               // ✅ PRIMARY - 100% - Count of adders
  NUM_APPROVED_ADDERS: 1046,       // ✅ SECONDARY - 100% - Approved count
  NUM_NEEDS_REVIEW_ADDERS: 2282,   // ✅ PRIMARY - 100% - CRITICAL bottleneck!
  NUM_CONTRACT_ADDERS: 2272,       // ✅ PRIMARY - 100% - Contract adders
  NUM_POST_SALE_ADDERS: 2273,      // ✅ PRIMARY - 100% - Post-sale adders
  NUM_ELECTRICAL_UPGRADE_ADDERS: 2240, // ✅ PRIMARY - 100% - Electrical upgrades

  // Adder Costs (4 fields)
  TOTAL_ADDER_COST: 2115,          // ✅ PRIMARY - 99.7% - Total adder value
  ADDERS_TOTAL_COST_CUSTOMER: 253, // ✅ PRIMARY - 67.8% - Customer-facing cost
  REP_TOTAL_ADDER_COST: 553,       // ✅ PRIMARY - 100% - Rep adder cost

  // Adder Lists (2 fields)
  SALES_FACING_ADDER_LIST: 2286,   // ✅ PRIMARY - 100% - CRITICAL - Comma-separated list
  NEEDS_REVIEW_ADDER_LIST: 2276,   // ✅ PRIMARY - 100% - Needs review list

  // ============================================================
  // MILESTONE 1: INTAKE
  // ============================================================

  // Intake Status (1 field - 100% USAGE!)
  INTAKE_STATUS: 347,              // ⭐ PRIMARY - 100% - Intake status field

  // Intake Completion (5 fields)
  INTAKE_COMPLETED_DATE: 461,      // ✅ PRIMARY - 5.7% - Intake completed timestamp
  INTAKE_COMPLETED_DATE_XX: 542,   // ✅ PRIMARY - 5.7% - xx.Intake Completed Date
  FINANCE_INTAKE_APPROVED: 1548,   // ✅ SECONDARY - Finance approved checkbox (OLD)
  FINANCE_INTAKE_APPROVED_DATE: 1549, // ⚠️ LOW - 0.3% - When intake was approved (BROKEN)
  INTAKE_COMPLETE: 554,            // ✅ SECONDARY - 100% - Intake complete URL
  WEBHOOK_INTAKE_COMPLETE: 603,    // ✅ SECONDARY - 100% - Intake complete checkbox

  // Intake Substeps (3 fields)
  INTAKE_FIRST_PASS_STARTED: 1965, // ✅ PRIMARY - 100% - First pass started processing
  INTAKE_FIRST_PASS_COMPLETE: 1951, // ✅ PRIMARY - 100% - First pass complete timestamp
  MINIMUM_INTAKE_APPROVAL_DATE: 2397, // ✅ PRIMARY - 87% - Minimum approval date

  // Intake Helper (1 field)
  INTAKE_INSTALL_DATE_TENTATIVE: 902, // ⭐ CRITICAL - 100% - Tentative install date set at intake

  // ============================================================
  // MILESTONE 2: SURVEY
  // ============================================================

  // Survey Status (1 field - 100% USAGE!)
  SURVEY_STATUS: 162,              // ⭐ PRIMARY - 100% - Survey status field

  // Survey Dates (6 fields)
  SURVEY_SUBMITTED: 164,           // ✅ PRIMARY - 26.5% - Survey sent to ops
  SURVEY_APPROVED: 165,            // ✅ PRIMARY - 25.2% - Survey approved
  MAX_SURVEY_SUBMITTED: 575,       // 🔄 BACKUP - 26.5% - Timestamp version
  MAX_DESIGN_SURVEY_APPROVED: 693, // 🔄 BACKUP - 25.2% - Maximum Design - Survey Approved

  // Survey Scheduled (LOW USAGE)
  SURVEY_SCHEDULED_DATE: 166,      // ⚠️ LOW - 0.7% - Survey scheduled (unreliable)
  MAX_SURVEY_SCHEDULED_DATE: 793,  // ⚠️ LOW - 0.7% - Maximum survey scheduled

  // Survey Arrivy Integration (1 field - 100% USAGE!)
  SITE_SURVEY_ARRIVY_SCHEDULED: 2526, // ⭐ CRITICAL - 100% - Arrivy scheduled date/time

  // Survey Helper (1 field)
  SALE_TO_SURVEY_COMPLETE_DURATION: 416, // ✅ PRIMARY - 100% - Duration field

  // ============================================================
  // MILESTONE 3: DESIGN
  // ============================================================

  // Design Status (1 field - 100% USAGE!)
  DESIGN_STATUS: 317,              // ⭐ PRIMARY - 100% - Design status field

  // Design Completion (3 fields)
  DESIGN_COMPLETED: 315,           // ✅ PRIMARY - 26.2% - Final design done
  MIN_INITIAL_DESIGN_COMPLETED: 1774, // 🔄 BACKUP - 24.2% - Minimum initial design completed
  DESIGN_AUDIT_COMPLETED: 2201,    // ✅ PRIMARY - 65% - Design audit completed timestamp

  // Design Substeps (7 fields)
  PREDESIGN_APPROVED: 316,         // ✅ PRIMARY - 27.2% - Predesign approved
  CAD_DESIGN_SUBMITTED: 475,       // ✅ PRIMARY - 27.5% - Maximum CAD design submitted
  CAD_DESIGN_APPROVED: 476,        // ✅ PRIMARY - 26.2% - CAD approved
  ENGINEERING_SUBMITTED: 477,      // ✅ PRIMARY - 27.5% - Maximum engineering submitted
  ENGINEERING_COMPLETED: 478,      // ✅ PRIMARY - 26.2% - Maximum engineering completed
  ENGINEERING_AUDIT: 2515,         // ✅ PRIMARY - 65% - Max initial design engineering audit

  // Design Min/Max Timestamps (8 fields)
  MIN_INITIAL_CAD_SUBMITTED: 699,  // 🔄 BACKUP - 68% - Min initial design CAD submitted
  MAX_INITIAL_CAD_SUBMITTED: 700,  // 🔄 BACKUP - 68% - Max initial CAD design submitted
  MIN_INITIAL_CAD_APPROVED: 701,   // 🔄 BACKUP - 68% - Min initial CAD design approved
  MAX_INITIAL_CAD_APPROVED: 702,   // 🔄 BACKUP - 68% - Max initial CAD design approved
  MAX_INITIAL_ENGINEERING_SUBMITTED: 704, // 🔄 BACKUP - 68% - Max initial engineering submitted
  MIN_INITIAL_ENGINEERING_SUBMITTED: 705, // 🔄 BACKUP - 68% - Min initial engineering submitted
  MAX_CAD_APPROVED_CAPTURE: 577,   // 🔄 BACKUP - 25.8% - Maximum CAD approved capture
  MAX_ENG_APPROVED_CAPTURE: 578,   // 🔄 BACKUP - 25.8% - Maximum ENG approved capture

  // Design SLA Tracking (4 fields)
  DESIGN_SLA_DEADLINE: 2459,       // ⏰ SLA - 25.2% - Track design delays
  DESIGN_SLA_WARNING: 2460,        // ⏰ SLA - Boolean flag
  DESIGN_SLA_BREACH: 2461,         // ⏰ SLA - Boolean flag if breached
  DESIGN_SLACK_ALERT_SENT: 2464,   // ⏰ SLA - 52% - Checkbox - alert already sent

  // ============================================================
  // MILESTONE 4: HOA (CONDITIONAL)
  // ============================================================

  // HOA Status (1 field - 45% USAGE!)
  HOA_STATUS: 2098,                // ⭐ PRIMARY - 45% - HOA status field

  // HOA Dates (2 fields)
  HOA_APPLICATION_SUBMITTED: 212,  // ✅ PRIMARY - 10.7% - Only if HOA req'd
  HOA_APPLICATION_APPROVED: 213,   // ✅ PRIMARY - 10.7% - Only if HOA req'd

  // HOA Backup Fields (3 fields)
  HOA_APPROVED_DATE_CAPTURE: 583,  // 🔄 BACKUP - 10.7% - HOA approved capture
  MAX_HOA_APPLICATION_SUBMITTED: 1795, // 🔄 BACKUP - 15% - Maximum HOA application submitted
  MAX_HOA_APPROVED: 1796,          // 🔄 BACKUP - 12% - Maximum HOA approved

  // HOA Helper (2 fields)
  TOTAL_DURATION_HOA_APPROVAL: 582, // ✅ DURATION - 25.8% - Formula field
  ARC_DATE_TIME: 891,              // ⚠️ LOW - 0.3% - Architectural Review Committee

  // ============================================================
  // MILESTONE 5: NEM / INTERCONNECTION
  // ============================================================

  // NEM Status (1 field - 100% USAGE!)
  NEM_INTERCONNECTION_STATUS: 1692, // ⭐ PRIMARY - 100% - NEM/Interconnection status field

  // NEM Completion (3 fields)
  INTERCONNECTION_APPROVED: 327,   // ✅ PRIMARY - 18% - Interconnection approved
  INTERCONNECTION_APPROVED_DATE: 328, // ✅ PRIMARY - 18% - Interconnection approved date
  NEM_APPROVED: 327,               // ✅ PRIMARY - 18% - Same as INTERCONNECTION_APPROVED (alias)
  NEM_APPROVED_CAPTURED: 585,      // 🔄 BACKUP - 22.5% - NEM approved captured
  MAX_NEM_INTERCONNECTION_APPROVED: 1845, // 🔄 BACKUP - 18% - Max NEM/Interconnection approved

  // NEM Submission (2 fields)
  NEM_SUBMITTED: 326,              // ✅ PRIMARY - 31% - NEM/Interconnection submitted
  NEM_SUBMITTED_CAPTURED: 716,     // 🔄 BACKUP - 52% - NEM submitted captured (CORRECTED - this is signatures sent)
  MAX_NEM_INTERCONNECTION_SUBMITTED: 1842, // 🔄 BACKUP - 31% - Max NEM/Interconnection submitted

  // NEM Signatures (3 fields)
  NEM_INTERCONNECTION_SIGNATURES_SENT: 716, // ⭐ PRIMARY - 52% - NEM/Interconnection signatures sent
  NEM_SIGNATURES_SENT: 716,        // 🔄 ALIAS - Same as NEM_INTERCONNECTION_SIGNATURES_SENT (backward compatibility)
  NEM_SIGNATURE_RECEIVED: 1845,    // ✅ SECONDARY - 12.8% - Customer signed
  INTERCONNECTION_SIGNATURES_SENT: 2198, // ✅ SECONDARY - 16.1% - PTO signatures (different from NEM)

  // NEM Helper (4 fields)
  NEM_INITIAL_PACKET_FILLED: 1842, // ⚠️ LOW - 0.3% - NEM initial packet filled out
  UTILITY_APPROVAL_DATE: 310,      // ⚠️ LOW - 2.3% - Alternative approval date
  TOTAL_DURATION_READY_FOR_NEM: 601, // ✅ DURATION - 25.5% - Duration field
  NEM_CHECKLIST_TIER_2_REQUESTED: 1701, // ⚠️ LOW - 0.3% - Tier 2 interconnection

  // ============================================================
  // MILESTONE 6: PERMIT
  // ============================================================

  // Permit Status (2 fields - BOTH 100% USAGE!)
  PERMIT_STATUS: 2095,             // ⭐ PRIMARY - 100% - Permit status field
  PERMIT_DASHBOARD_STATUS: 2501,   // ⭐ PRIMARY - 100% - Permit dashboard status

  // Permit Submission (3 fields)
  PERMIT_SUBMITTED: 207,           // ✅ PRIMARY - 23.5% - Permit submitted
  PERMIT_APPLICATION_SUBMITTED: 709, // 🔄 BACKUP - 23.2% - Permit application submitted
  MIN_PERMIT_APPLICATION_SUBMITTED: 1402, // 🔄 BACKUP - 23.2% - Minimum permit application submitted

  // Permit Approval (3 fields)
  PERMIT_APPROVED: 208,            // ✅ PRIMARY - 21.8% - Permit approved
  PERMIT_APPROVED_DATE_CAPTURE: 584, // 🔄 BACKUP - 21.5% - Permit approved date capture
  MIN_PERMIT_APPROVED_CAPTURE: 2059, // 🔄 BACKUP - 21% - Minimum permit approved capture

  // Permit As-Built (1 field)
  AS_BUILT_SUBMITTED_TO_AHJ: 614,  // ✅ PRIMARY - 25.8% - HIGHEST permit usage! Post-install

  // Permit Processing (3 fields)
  PERMIT_START_PROCESSING_DATE: 2508, // ✅ PRIMARY - 38% - Start processing date
  PERMIT_AUDIT_STATUS: 2514,       // ✅ PRIMARY - 55% - Audit status
  MAX_PERMIT_AUDIT_COMPLETE_DATE: 2519, // ✅ PRIMARY - 55% - Maximum permit audit complete

  // Permit Resubmission (4 fields)
  MAX_PERMIT_REJECTED_REVISIONS: 706, // ⚠️ EDGE - 4.0% - Maximum permit rejected/revisions needed
  MAX_PERMIT_RESUBMITTED: 707,     // ⚠️ EDGE - 6.7% - Maximum permit resubmitted
  MAX_PERMIT_RESUBMITTED_DATE: 2094, // ⚠️ EDGE - 6.7% - Maximum permit resubmitted date
  REVISION_RESUBMITTED_DATE: 2097, // ⚠️ EDGE - 5.0% - Revision resubmitted date

  // Permit Helper (6 fields)
  ESTIMATED_PERMIT_RETURN_DATE: 1777, // ✅ PRIMARY - 23.5% - Timeline planning
  PERMIT_SUBMITTED_PLUS_3_DAYS: 1402, // ✅ CALCULATED - 23.2% - Permit submitted +3 days
  PERMIT_SUBMITTED_PLUS_7_DAYS: 1403, // ✅ CALCULATED - 23.2% - Permit submitted +7 days
  TOTAL_DURATION_PERMIT_APPROVAL: 581, // ✅ DURATION - 20.8% - Duration field
  TOTAL_DURATION_PERMIT_SUBMITTED: 1121, // ✅ DURATION - 25.8% - Duration field
  AHJ_AVERAGE_DURATION_PERMIT_SUBMITTED: 1125, // ✅ BENCHMARK - 12.4% - AHJ average duration

  // ============================================================
  // MILESTONE 7: INSTALL (BEST DATA COVERAGE!)
  // ============================================================

  // Install Completion (3 fields - 52.3% USAGE!)
  INSTALL_COMPLETED_DATE: 534,     // ⭐ PRIMARY - 52.3% - Install done
  INSTALLATION_COMPLETED_AT: 587,  // ⭐ PRIMARY - 52.3% - Timestamp version (same data)
  INSTALL_COMPLETED_TEST: 2006,    // 🔄 BACKUP - 52.3% - Install completed test

  // Install Commissioning (2 fields)
  READY_FOR_COMMISSION: 813,       // ⭐ PRIMARY - 47.0% - System commissioned & quality checked
  READY_FOR_COMMISSION_1029: 841,  // 🔄 BACKUP - 40.3% - Alternative version (10/29)

  // Install Scheduled (1 field - 54.4% USAGE!)
  INSTALL_SCHEDULED_DATE_CAPTURE: 710, // ⭐ PRIMARY - 54.4% - HIGHEST reliability!

  // Install Estimated (2 fields)
  ESTIMATED_INSTALL_DATE: 1124,    // ✅ PRIMARY - 27.5% - Estimated/projected date
  ESTIMATED_INSTALL_DATE_SUNDAY: 1182, // ✅ CALCULATED - 27.5% - Sunday-aligned estimate

  // Install Start Dates (5 fields)
  INSTALL_SCHEDULED_START_DATE: 178, // ✅ PRIMARY - 20.1% - Start date
  INSTALL_SCHEDULED_START_DATE_2: 1356, // 🔄 BACKUP - 20.1% - Start date 2.0
  Z_INSTALLATION_SCHEDULED_START_DATE: 228, // 🔄 BACKUP - 20.1% - z.Installation scheduled start
  MIN_INSTALL_START_DATE: 2442,    // 🔄 BACKUP - 20.1% - Minimum install start date
  MIN_INSTALL_SCHEDULED_CREATED_AT: 2408, // 🔄 BACKUP - 22.5% - Minimum install scheduled created

  // Install End Dates (2 fields)
  INSTALL_SCHEDULED_END_DATE: 179, // ✅ PRIMARY - 20.1% - End date
  INSTALL_SCHEDULED_END_DATE_2: 1357, // 🔄 BACKUP - 20.1% - End date 2.0

  // Install Reschedule Tracking (1 field)
  RESCHEDULE_COUNT: 9999,          // ⚠️ PLACEHOLDER - Field ID unknown - needs configuration

  // Install Started (1 field)
  INSTALL_STARTED_DATE: 464,       // ✅ SECONDARY - 19.8% - Crew arrived on site

  // Install Multi-Touch (2 fields - 52.3% USAGE!)
  MULTI_TOUCH_MILESTONE_COMPLETED_START: 2182, // ✅ PRIMARY - 52.3% - Multi-day install start
  MULTI_TOUCH_MILESTONE_COMPLETED_END: 2183, // ✅ PRIMARY - 52.3% - Multi-day install end

  // Install Helper (6 fields)
  INSTALL_DATE_IMPORT: 835,        // 🔄 BACKUP - 44.6% - Legacy import field
  INSTALLATION_DATE_CALC: 1185,    // ✅ CALCULATED - 24.2% - Installation date calculated
  MAX_ANY_INSTALL_EVENT_SCHEDULED_START: 2449, // 🔄 BACKUP - 20.1% - Max any install event
  DAYS_SINCE_INSTALL_COMPLETED: 1387, // ✅ CALCULATED - 52.3% - Age since completion
  READY_FOR_PAYROLL: 769,          // ✅ SECONDARY - 37.9% - Payroll processing
  READY_FOR_PAYROLL_1029: 842,     // ✅ SECONDARY - 44.0% - Alternative version (10/29)

  // Install Funding M2 (3 fields)
  INSTALL_FUNDING_SUBMITTED: 486,  // ✅ SECONDARY - 18.8% - M2 request
  INSTALL_FUNDING_RECEIVED: 487,   // ✅ SECONDARY - 13.8% - M2 funded
  FUNDING_DASHBOARD_READY_FOR_M2: 1993, // ⭐ PRIMARY - 52.3% - Ready for M2 funding

  // ============================================================
  // MILESTONE 8: VERIFICATION (CALCULATED - NO DIRECT FIELD)
  // ============================================================
  // Estimate 1-3 days after install complete
  // Uses INSTALL_COMPLETED_DATE (534) + 2 days

  // ============================================================
  // MILESTONE 9: INSPECTION
  // ============================================================

  // Inspection Completion (3 fields)
  PASSING_INSPECTION_COMPLETED: 491, // ✅ PRIMARY - 19.1% - Passed inspection
  PASSED_INSPECTION_COMPLETE_CAPTURE: 593, // 🔄 BACKUP - 4.0% - Passed inspection capture
  MAX_FINAL_INSPECTION_COMPLETED: 1780, // 🔄 BACKUP - 29% - Maximum final inspection (HIGHER!)

  // Inspection Scheduled (3 fields)
  INSPECTION_SCHEDULED_DATE: 226,  // ⚠️ LOW - 9.7% - Scheduled only
  FIRST_INSPECTION_SCHEDULED: 1589, // ⚠️ LOW - 9.7% - First inspection scheduled date/time
  TASK_INSPECTION_SCHEDULED: 1815, // ⚠️ LOW - 9.7% - Task: Inspection scheduled date/time

  // Inspection Failed (1 field)
  INSPECTION_FAILED_DATE: 1469,    // ⚠️ EDGE - 2.3% - Track failed inspections

  // Inspection Helper (1 field)
  INSTALL_TO_INSPECTION_SCHEDULED_DURATION: 876, // ✅ DURATION - 9.7% - Duration field

  // ============================================================
  // MILESTONE 10: PTO (FINAL MILESTONE!)
  // ============================================================

  // PTO Status (1 field - 100% USAGE!)
  PTO_STATUS: 556,                 // ⭐ PRIMARY - 100% - PTO status field

  // PTO Dates (2 fields)
  PTO_SUBMITTED: 537,              // ✅ PRIMARY - 21.1% - Submitted to utility
  PTO_APPROVED: 538,               // ✅ PRIMARY - 20.5% - FINAL MILESTONE!

  // PTO Lender Upload (1 field - CELEBRATION!)
  PTO_UPLOADED_TO_LENDER: 556,     // 🎉 CELEBRATION - 19.8% - M3 payment! (same as PTO_STATUS)

  // PTO Lender Fields (5 fields)
  FIC_UPLOADED_TO_LENDER: 555,     // ✅ SECONDARY - 19.1% - FIC uploaded
  NTP_APPROVAL_DATE: 312,          // ✅ SECONDARY - 19.1% - NTP approval
  NTP_SUBMITTED_DATE: 479,         // ✅ SECONDARY - 18.8% - NTP submitted
  LENDER_FUNDING_RECEIVED_DATE: 2375, // ⭐ PRIMARY - 48% - When lender paid
  LENDER_PTO_GREENLIGHT_DATE: 2376, // ⭐ PRIMARY - 52% - Lender approval to proceed

  // PTO Final Funding (2 fields)
  FINAL_FUNDING_SUBMITTED: 488,    // ✅ SECONDARY - 11.1% - Final funding submitted
  FINAL_FUNDING_RECEIVED: 489,     // ✅ SECONDARY - 10.4% - Final funding received

  // PTO Activation (1 field)
  ACTIVATION_COMPLETED: 572,       // ⚠️ RARE - 0.3% - Customer education call

  // PTO Helper (1 field)
  INVOICE_DATE: 618,               // ✅ PRIMARY - 51.7% - When customer was invoiced

  // ============================================================
  // FUNDING DASHBOARD (M1/M2/M3) - ALL 100% USAGE!
  // ============================================================

  // Funding Status Fields (3 fields - ALL 100% USAGE!)
  FUNDING_DASHBOARD_M1_STATUS: 2049, // ⭐ PRIMARY - 100% - M1 status
  FUNDING_DASHBOARD_M2_STATUS: 2050, // ⭐ PRIMARY - 100% - M2 status
  FUNDING_DASHBOARD_M3_STATUS: 2051, // ⭐ PRIMARY - 100% - M3 status

  // M1 Funding (4 fields)
  M1_REQUEST_DATE: 442,            // ✅ SECONDARY - 19.5% - M1 request date
  EXPECTED_M1_RECEIVED_DATE: 444,  // ✅ SECONDARY - 19.5% - Expected M1 received
  FUNDING_DASHBOARD_OFFICIAL_M1_REQUESTED: 2038, // ✅ SECONDARY - 19.5% - Official M1 requested
  FUNDING_DASHBOARD_READY_FOR_M1: 1992, // ✅ SECONDARY - 20.5% - Ready for M1 funding

  // M2 Funding (5 fields)
  M2_REQUEST_DATE: 445,            // ✅ SECONDARY - 18.5% - M2 request date
  EXPECTED_M2_RECEIVED_DATE: 447,  // ✅ SECONDARY - 18.8% - Expected M2 received
  M2_CREDIT_DATE: 915,             // ✅ SECONDARY - 2.0% - M2 credit date
  FUNDING_DASHBOARD_OFFICIAL_M2_REQUESTED: 2039, // ✅ SECONDARY - 18.5% - Official M2 requested
  // FUNDING_DASHBOARD_READY_FOR_M2: 1993 (already defined above in Install section)

  // M3 Funding (6 fields)
  M3_REQUEST_DATE: 513,            // ✅ SECONDARY - 5.4% - M3 request date
  EXPECTED_M3_RECEIVED_DATE: 512,  // ✅ SECONDARY - 4.7% - Expected M3 received
  FUNDING_DASHBOARD_OFFICIAL_M3_REQUESTED: 2040, // ✅ SECONDARY - 5.7% - Official M3 requested
  FUNDING_DASHBOARD_READY_FOR_M3: 1994, // ✅ SECONDARY - 20.5% - Ready for M3 funding
  FUNDING_DASHBOARD_MAX_M3_EVENT_APPROVED: 2029, // ⚠️ RARE - 0.3% - Max M3 event approved
  FUNDING_DASHBOARD_MAX_M3_EVENT_REQUESTED: 2028, // ⚠️ RARE - 0.3% - Max M3 event requested

} as const;

// Type helper
export type ProjectFieldId = typeof PROJECT_FIELDS[keyof typeof PROJECT_FIELDS];
