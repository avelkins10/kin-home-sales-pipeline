# Complete Field Mapping - All Potentially Relevant Fields

## 📋 Instructions for Use

This document lists **every field that could be relevant** for the Rep Dashboard, organized by purpose with:
- ✅ **Primary fields** (highest usage/reliability)
- 🔄 **Backup/alternate fields** (use if primary is empty)
- 📊 **Usage %** (how often the field has data)
- 💡 **Notes** on when/why to use each field

### Field Selection Strategy:
1. **Try Primary First** - Highest usage, most reliable
2. **Fall Back to Alternates** - If primary is empty
3. **Show "Not Available"** - If all fields are empty
4. **Combine Multiple Fields** - Some data is split across fields

---

## 1️⃣ CORE PROJECT IDENTITY

### Record/Project IDs
```javascript
{
  recordId: 3,              // ✅ PRIMARY - Record ID# (100% usage)
  projectId: 11,            // ✅ PRIMARY - Project ID (100% usage)
  dateCreated: 1,           // Date Created (100% usage)
  dateModified: 2,          // Date Modified (100% usage)
  recordOwner: 4            // Record Owner (100% usage)
}
```

### Customer Information
```javascript
{
  // PRIMARY
  customerName: 145,        // ✅ Customer Name (100% usage)
  customerAddress: 146,     // ✅ Customer Address (100% usage)
  customerPhone: 148,       // Customer Phone (100% usage)

  // ADDITIONAL INFO
  customerEmail: 147,       // Customer Email (check usage)
  customerCity: 149,        // Customer City
  customerState: 150,       // Customer State
  customerZip: 151          // Customer Zip
}
```

**💡 Instructions:**
- Always show Customer Name (required)
- Display full address for install scheduling context
- Phone is critical for rep contact
- Email useful for communication tracking

---

## 2️⃣ PROJECT STATUS & PRIORITY

### Status Fields
```javascript
{
  // PRIMARY STATUS
  projectStatus: 255,       // ✅ PRIMARY - Project Status (100% usage)
                           // Values: Active, Active - On Hold, Completed,
                           // Cancelled, Active - PTO, Active - Installed

  // VISUAL INDICATORS
  projectStatusColor: 299,  // Project Status - Color (HTML formula)
  projectStatusBar: 301,    // ✅ CRITICAL - Project Status - Color (HTML table)
                           // Shows all 9 milestones with colored dots

  // PRIORITY
  projectPriority: 300,     // ✅ PRIMARY - Project Priority (100% usage)
                           // Values: Insane, Urgent, Normal

  // ESCALATION
  projectEscalation: 302,   // Project Escalation (check for High/Medium/ARC)

  // STATE
  state: 280               // State (Florida, Texas, etc.)
}
```

**💡 Instructions:**
- `projectStatus` (255) - Main status filter
- `projectStatusBar` (301) - Parse for milestone visualization
- `projectPriority` (300) - Sort "Insane" and "Urgent" to top
- Show red badge for "Active - On Hold"
- Escalation indicates urgent attention needed

---

## 3️⃣ SALES DATE (CRITICAL TIMELINE START)

### Primary Sales Date
```javascript
{
  salesDate: 522,           // ✅ PRIMARY - Sales Date (100% usage)

  // BACKUPS (all show sales date)
  saleDate: 695,            // Sale Date (100% usage) - SAME as 522?
  salesDateBackup: 948,     // Sales Date (Backup) (100% usage)
  salesDateTime: 1580,      // Sales Date_Time (100% usage)
  sales_date: 933           // sales_date (100% usage)
}
```

**💡 Instructions:**
- Use `salesDate` (522) as PRIMARY
- All others appear to be duplicates/backups
- If 522 is null (shouldn't happen), try 695, 948, 1580, 933 in order
- This is the **anchor date** for all timeline calculations

---

## 4️⃣ PROJECT AGE & TIMING

### Age Calculations
```javascript
{
  projectAge: 438,          // ✅ PRIMARY - Project Age (days since created) (100% usage)
  projectCreatedDate: 393,  // Project Created Date (93.6% usage)

  // SPECIFIC AGE CALCULATIONS
  projectAgeAtIntakeComplete: 730,        // Biz days (5.4% usage)
  projectAgeAtInstallComplete: 623,       // Days (52.3% usage)
  projectBizAgeAtInstallScheduled: 918,   // Biz days (20.1% usage)
  projectCalAgeAtInstallScheduled: 708,   // Cal days (20.1% usage)

  // DURATION FIELDS
  saleToSiteSurveyComplete: 416,          // Duration (100% usage)
  saleToSiteSurveyScheduledBiz: 794,      // Biz days (0.7% usage)
  saleToSiteSurveyScheduledCal: 1430      // Cal days (0.7% usage)
}
```

**💡 Instructions:**
- Use `projectAge` (438) for "Days in Pipeline"
- Calculate "Days Since Sale" from `salesDate` to today
- `projectAgeAtInstallComplete` shows how long projects took to complete
- Use for performance metrics and SLA tracking

---

## 5️⃣ INTAKE MILESTONE

### Intake Dates
```javascript
{
  // PRIMARY
  intakeCompletedDate: 461,               // ✅ Intake Completed Date (5.7% usage)
  xxIntakeCompletedDate: 542,             // xx.Intake Completed Date (5.7% usage)

  // ALTERNATE
  financeIntakeApprovedDateTime: 1549,    // Finance Intake Approved Date/Time (0.3%)

  // RELATED
  intakeInstallDateTentative: 902,        // ✅ intake_install_date_tentative (100% usage!)
                                          // This is an ESTIMATED install date set at intake
}
```

**💡 Instructions:**
- Intake rarely has dates (5.7% usage)
- `intakeInstallDateTentative` (902) - 100% usage! Use this for initial install estimate
- Show "Intake: Not Tracked" if dates missing
- This milestone often tracked via Tasks table instead

---

## 6️⃣ SURVEY MILESTONE

### Survey Dates
```javascript
{
  // SUBMITTED
  surveySubmittedDate: 164,               // ✅ PRIMARY - Survey Submitted Date (26.5% usage)
  maxSurveySubmittedDateTime: 575,        // 🔄 BACKUP - Maximum Survey Submitted Date/Time (26.5%)

  // APPROVED
  surveyApprovedDate: 165,                // ✅ PRIMARY - Survey Approved Date (25.2% usage)
  maxDesignSurveyApprovedDate: 693,       // 🔄 BACKUP - Maximum Design - Survey Approved Date (25.2%)

  // SCHEDULED (low usage)
  surveyScheduledDate: 166,               // Survey Scheduled Date (0.7% usage)
  maxSurveyScheduledDate: 793,            // Maximum Survey Scheduled Date (0.7%)
  siteSurveyScheduled: 969,               // Site Survey Scheduled (0.7%)
  surveyScheduledDateHistorical: 2209     // Survey Scheduled Date - Historical Data (0.7%)
}
```

**💡 Instructions:**
- Use `surveySubmittedDate` (164) as PRIMARY - 26.5% usage
- Fall back to `maxSurveySubmittedDateTime` (575)
- Use `surveyApprovedDate` (165) for completion
- IGNORE scheduled dates (0.7% usage - unreliable)
- Show "Survey: Completed [date]" or "Survey: In Progress" or "Survey: Not Started"

---

## 7️⃣ DESIGN MILESTONE (CAD + Engineering)

### Design Submitted
```javascript
{
  maxCADDesignSubmitted: 475,             // ✅ PRIMARY - Maximum CAD Design Submitted (27.5%)
  maxEngineeringSubmitted: 477,           // ✅ PRIMARY - Maximum Engineering Submitted (27.5%)

  // INITIAL DESIGN
  maxInitialCADDesignSubmitted: 700,      // Maximum Initial CAD Design Submitted (25.5%)
  maxInitialCADDesignSubmitted2: 703,     // Duplicate (25.5%)
  minInitialDesignCADSubmitted: 699,      // Minimum Initial Design CAD Design Submitted (25.5%)
  maxInitialEngineeringSubmitted: 704,    // Maximum Initial Engineering Submitted (25.5%)
  minInitialEngineeringSubmitted: 705     // Minimum Initial Engineering Submitted (25.5%)
}
```

### Design Approved/Completed
```javascript
{
  // COMPLETED
  designCompleted: 315,                   // ✅ PRIMARY - Design Completed (26.2% usage)
  minInitialDesignCompleted: 1774,        // Minimum Initial Design Completed (24.2%)

  // CAD APPROVED
  cadDesignApproved: 476,                 // ✅ CAD Design Approved (26.2%)
  maxCADApprovedCapture: 577,             // Maximum CAD Approved Capture (25.8%)
  maxInitialCADDesignApproved: 702,       // Maximum Initial CAD Design Approved (24.2%)
  minInitialCADDesignApproved: 701,       // Minimum Initial CAD Design Approved (24.2%)

  // ENGINEERING APPROVED
  maxEngineeringCompleted: 478,           // Maximum Engineering Completed (26.2%)
  maxENGApprovedCapture: 578,             // Maximum ENG Approved Capture (25.8%)

  // PRE-DESIGN
  predesignApproved: 316,                 // Predesign Approved (27.2% usage)

  // SLA TRACKING
  designSLADeadline: 2459,                // ✅ CRITICAL - Design SLA Deadline (25.2%)
  designSLAWarning: 2460,                 // Design SLA Warning (check usage)
  designSLABreach: 2461                   // Design SLA Breach (check usage)
}
```

**💡 Instructions:**
- **Submitted:** Use `maxCADDesignSubmitted` (475) + `maxEngineeringSubmitted` (477)
- **Completed:** Use `designCompleted` (315) as PRIMARY
- **SLA:** Show `designSLADeadline` (2459) with warning if approaching or overdue
- Design has MULTIPLE sub-steps (pre-design, CAD, engineering)
- Show latest/maximum date if multiple exist
- 25-27% usage means 3 out of 4 projects have this data

---

## 8️⃣ HOA MILESTONE

### HOA Dates
```javascript
{
  // SUBMITTED
  hoaApplicationSubmitted: 212,           // ✅ PRIMARY - HOA Application Submitted (10.7% usage)

  // APPROVED
  hoaApplicationApproved: 213,            // ✅ PRIMARY - HOA Application Approved (10.7%)
  hoaApprovedDateCapture: 583,            // 🔄 BACKUP - HOA Approved Date - Capture (10.7%)

  // DURATION
  totalDurationHOAApproval: 582,          // Total Duration - HOA Approval (25.8%)

  // ARC (Architectural Review Committee)
  arcDateTime: 891                        // ARC Date/Time (0.3% usage)
}
```

**💡 Instructions:**
- Only ~11% of projects have HOA
- Use `hoaApplicationSubmitted` (212) and `hoaApplicationApproved` (213)
- Show "HOA: Not Applicable" if no dates
- `totalDurationHOAApproval` might have data even if dates don't (formula field)

---

## 9️⃣ PERMIT MILESTONE

### Permit Submitted
```javascript
{
  // PRIMARY
  permitSubmitted: 207,                   // ✅ PRIMARY - Permit Submitted (23.5% usage)
  permitSubmittedDateCapture: 709,        // 🔄 BACKUP - Permit Submitted Date - Capture (23.2%)

  // CALCULATED/RELATED
  permitSubmittedPlus3Days: 1402,         // Permit Submitted (+3 Days) (23.2%)
  permitSubmittedPlus7Days: 1403,         // Permit Submitted (+7 Days) (23.2%)
  estimatedPermitReturnDate: 1777,        // Estimated Permit Return Date (23.5%)

  // RESUBMITTED (if rejected)
  maxPermitResubmitted: 707,              // Maximum Permit Resubmitted (6.7%)
  maxPermitResubmittedDate: 2094,         // Maximum Permit Resubmitted Date (6.7%)
  maxPermitRejectedRevisionsNeeded: 706,  // Maximum Permit Rejected/ Revisions Needed (4.0%)
  revisionResubmittedDate: 2097,          // Revision Resubmitted Date (5.0%)

  // RELATED
  asBuiltSubmittedToAHJ: 614              // As built Submitted to AHJ (25.8% - post-install)
}
```

### Permit Approved
```javascript
{
  // PRIMARY
  permitApproved: 208,                    // ✅ PRIMARY - Permit Approved (21.8% usage)
  permitApprovedDateCapture: 584,         // 🔄 BACKUP - Permit Approved Date - Capture (21.5%)

  // DURATION
  totalDurationPermitApproval: 581,       // Total Duration - Permit Approval (20.8%)
  totalDurationPermitSubmitted: 1121,     // Total Duration - Permit Submitted (25.8%)
  totalDurationPermitSubmittedActiveOnly: 1122, // Active Only (12.8%)

  // AHJ AVERAGE (for estimates)
  ahjAverageDurationPermitSubmitted: 1125 // AHJ - Average Duration - Permit Submitted (12.4%)
}
```

**💡 Instructions:**
- Use `permitSubmitted` (207) and `permitApproved` (208) as PRIMARY
- Show resubmission if `maxPermitResubmitted` has a date (6.7% have revisions)
- `estimatedPermitReturnDate` useful for forecasting
- AHJ average can show "typical time for this jurisdiction"
- 21-24% usage is decent coverage

---

## 🔟 NEM/INTERCONNECTION MILESTONE

### NEM Submitted
```javascript
{
  // PRIMARY
  nemSubmittedDate: 326,                  // ✅ PRIMARY - NEM Submitted Date (24.2% usage)
  nemSubmittedCaptured: 716,              // 🔄 BACKUP - NEM Submitted - Captured (24.2%)

  // SIGNATURES
  nemSignaturesSent: 1844,                // NEM Signatures Sent (21.8%)
  nemSignatureReceived: 1845,             // NEM Signature Received (12.8%)
  interconnectionSignaturesSent: 2198,    // Interconnection Signatures Sent (16.1%)

  // PACKET
  nemInitialPacketFilledOut: 1842,        // NEM Initial Packet Filled Out (0.3%)

  // TIER 2
  nemChecklistDateLoggerTier2Requested: 1701  // Nem Checklist Date Logger - Tier 2 Requested (0.3%)
}
```

### NEM Approved
```javascript
{
  // PRIMARY
  nemApprovedDate: 327,                   // ✅ PRIMARY - NEM Approved Date (22.8% usage)
  nemApprovedCaptured: 585,               // 🔄 BACKUP - NEM Approved- Captured (22.5%)

  // UTILITY
  utilityApprovalDate: 310,               // Utility - Approval Date (2.3%)

  // DURATION
  totalDurationReadyForNEM: 601           // Total Duration - Ready for NEM (25.5%)
}
```

**💡 Instructions:**
- NEM = Net Energy Metering / Interconnection Application
- Use `nemSubmittedDate` (326) and `nemApprovedDate` (327)
- Signature tracking shows sub-steps of process
- 22-24% usage means 1 in 4 projects have dates
- This step often runs parallel with install

---

## 1️⃣1️⃣ INSTALL MILESTONE (MOST RELIABLE DATA!)

### Install Scheduled (20-54% usage)
```javascript
{
  // HIGHLY RELIABLE
  installScheduledDateCapture: 710,       // ✅ PRIMARY - Install Scheduled Date Capture (54.4%!)

  // SCHEDULED START
  installScheduledStartDate: 178,         // ✅ Install Scheduled Start Date (20.1%)
  installScheduledStartDate2: 1356,       // Install Scheduled Start Date 2.0 (20.1%)
  zInstallationScheduledStartDate: 228,   // z.Installation Scheduled Start Date (20.1%)
  maxAnyInstallEventScheduledStart: 2449, // Max - Any Install Event Scheduled Start Date (20.1%)
  minInstallStartDate: 2442,              // Minimum Install Start Date (20.1%)
  minInstallScheduledCreatedAt: 2408,     // Minimum Install Scheduled Created At (22.5%)

  // SCHEDULED END
  installScheduledEndDate: 179,           // Install Scheduled End Date (20.1%)
  installScheduledEndDate2: 1357,         // Install Scheduled End Date 2.0 (20.1%)

  // ESTIMATED/CALCULATED
  estimatedInstallDate: 1124,             // Estimated Install Date (27.5%)
  estimatedInstallDateSunday: 1182,       // Estimated Install Date (Sunday) (27.5%)
  installationDateCalc: 1185,             // Installation Date (Calc) (24.2%)
  installDateImport: 835,                 // Install Date Import (44.6%)
  intakeInstallDateTentative: 902         // ✅ intake_install_date_tentative (100%! - early estimate)
}
```

### Install Started (19.8% usage)
```javascript
{
  installStartedDate: 464                 // Install Started Date (19.8% usage)
}
```

### Install Completed (50-52% usage - MOST RELIABLE!)
```javascript
{
  // PRIMARY (all ~52% usage)
  installCompletedDate: 534,              // ✅ PRIMARY - Install Completed Date (52.3%!)
  installationCompletedAt: 587,           // ✅ Installation - Completed At (52.3%!)
  installCompletedTest: 2006,             // Install Completed Test (52.3%)

  // MULTI-TOUCH TRACKING
  multiTouchMilestoneCompletedStartDate: 2182,  // Multi-Touch: Milestone Completed Start (52.3%)
  multiTouchMilestoneCompletedEndDate: 2183,    // Multi-Touch: Milestone Completed End (52.3%)
  multiTouchUncategorizedCompletedDate: 2256,   // Multi-Touch: Uncategorized Completed (52.3%)

  // COMMISSIONING (post-install)
  readyForCommissionDateTime: 813,        // Ready for Commission - Date/Time (47.0%)
  readyForCommissionDateTime1029: 841,    // Ready for Commission - Date/Time (10/29) (40.3%)
  csHandoffCommissioningCompleteDateTime: 1462, // CS Handoff - Commissioning Complete (0.3%)

  // PAYROLL (post-install)
  readyForPayrollDateTime: 769,           // Ready for Payroll - Date/Time (37.9%)
  readyForPayrollDateTime1029: 842,       // Ready for Payroll - Date/Time (10/29) (44.0%)

  // INSTALL AGE CALCULATIONS
  daysSinceInstallCompleted: 1387         // Days Since Install Completed (52.3%)
}
```

### Install Funding (milestone payments)
```javascript
{
  installFundingSubmitted: 486,           // Install Funding Submitted (18.8%)
  installFundingReceived: 487             // Install Funding Received (13.8%)
}
```

**💡 Instructions:**
- **Install has BEST data coverage!** 50%+ usage
- **Scheduled:** Use `installScheduledDateCapture` (710) - 54% usage!
- **Tentative/Estimated:** Use `intakeInstallDateTentative` (902) - 100% usage early in project
- **Actual Install:** Use `installCompletedDate` (534) or `installationCompletedAt` (587) - both 52%
- Multiple dates exist because install can span multiple days
- Show start→end date range if both available
- "Ready for Commission" indicates quality check complete

---

## 1️⃣2️⃣ INSPECTION MILESTONE

### Inspection Scheduled (9.7% usage)
```javascript
{
  inspectionScheduledDate: 226,           // Inspection Scheduled Date (9.7%)
  firstInspectionScheduledDateTime: 1589, // First Inspection Scheduled Date/Time (9.7%)
  taskInspectionScheduledDateTime: 1815   // Task: Inspection Scheduled Date/Time (9.7%)
}
```

### Inspection Completed
```javascript
{
  // PRIMARY
  passingInspectionCompletedDate: 491,    // ✅ Passing Inspection Completed Date (19.1%)
  passedInspectionCompleteCapture: 593,   // Passed Inspection Complete - Capture (4.0%)

  // FAILED
  inspectionFailedDate: 1469,             // Inspection Failed Date (2.3%)

  // DURATION
  installCompleteToMaxInspectionScheduled: 876  // Install Complete to Max Inspection Scheduled (9.7%)
}
```

**💡 Instructions:**
- Low usage (9-19%) but important when it exists
- Use `passingInspectionCompletedDate` (491) - 19.1% usage
- Show "Inspection Scheduled" if scheduled date exists
- Track failures with `inspectionFailedDate` (2.3%)
- This is often tracked in Tasks table instead

---

## 1️⃣3️⃣ PTO (PERMISSION TO OPERATE) MILESTONE

### PTO Submitted
```javascript
{
  ptoSubmittedDate: 537,                  // ✅ PRIMARY - PTO Submitted Date (21.1% usage)
  ptoUploadedToLender: 556                // PTO Uploaded to Lender (19.8%)
}
```

### PTO Approved/Activated
```javascript
{
  ptoApprovedDate: 538,                   // ✅ PRIMARY - PTO Approved Date (20.5% usage)
  activationCompleted: 572                // Activation Completed (0.3% usage - rare)
}
```

**💡 Instructions:**
- Use `ptoSubmittedDate` (537) and `ptoApprovedDate` (538)
- 20-21% usage is decent
- PTO = final step, permission from utility to turn system on
- `activationCompleted` is different (customer education call)

---

## 1️⃣4️⃣ TEAM ASSIGNMENTS

### Closer (Sales Person who closed deal)
```javascript
{
  closerId: 516,                          // ✅ Closer ID (100% usage)
  closerName: 517,                        // ✅ Closer Name (100% usage)
  closerPhoneOfficeHome: 518              // Closer - Office/ Home Phone (check usage)
}
```

### Setter (Lead Setter)
```javascript
{
  setterId: 329,                          // ✅ Setter ID (100% usage)
  setterName: 330,                        // ✅ Setter Name (100% usage)
  setterHomePhone: 2215                   // Setter Home Phone (check usage)
}
```

### Project Coordinator
```javascript
{
  projectCoordinatorId: 819,              // ✅ Project Coordinator ID (100% usage)
  projectCoordinator: 820                 // ✅ Project Coordinator (100% usage)
}
```

### Sales Office/Company
```javascript
{
  salesOffice: 339,                       // Sales Office (100% usage)
  salesCompany: 340,                      // Sales Company (check usage)
  epcName: 341                            // EPC Name (check usage)
}
```

**💡 Instructions:**
- **Filter projects by:** `{516.EX.'userId'}` for Closer, `{329.EX.'userId'}` for Setter
- User fields return objects: `{ id, email, name, userName }`
- Always have ID and Name fields (100% usage)
- Use ID for filtering, Name for display

---

## 1️⃣5️⃣ SYSTEM SPECIFICATIONS

### System Size & Specs
```javascript
{
  // SIZE
  systemSizeKW: 13,                       // ✅ System Size(kW) (100% usage)
  systemSizeWatts: 342,                   // system_size(watts) (check if needed)

  // PANELS
  module: 343,                            // Module (panel type)
  numberOfPanels: 344,                    // Number of Panels
  panelWattage: 345,                      // Panel Wattage

  // INVERTER
  inverter: 346,                          // Inverter (type)

  // BATTERY
  batteryModel: 347,                      // Battery Model (if applicable)
  batteryQuantity: 348                    // Battery Quantity
}
```

### Utility Information
```javascript
{
  utility: 1147,                          // ✅ Utility (100% usage)
  ahjName: 1123                           // ✅ AHJ Name (jurisdiction) (100% usage)
}
```

**💡 Instructions:**
- `systemSizeKW` (13) - ALWAYS show this (key metric)
- Utility and AHJ useful for rep context
- Module/Inverter/Battery shown in specs section

---

## 1️⃣6️⃣ FINANCIAL FIELDS

### System Price
```javascript
{
  systemPrice: 133,                       // ✅ PRIMARY - System Price (100% usage)
  totalSystemAddOnCost: 129,              // Total (System+Add-on Cost) (100%)

  // COST BREAKDOWN
  zzSystemCost: 349,                      // zz.System Cost (check usage)
  costAddersTotal: 384                    // Cost - Adders Total (94.0% usage)
}
```

### Price Per Watt (PPW)
```javascript
{
  // PRIMARY PPW
  grossPPW: 19,                           // ✅ Gross PPW (100% usage)
  netPPW: 543,                            // ✅ Net PPW (94.0% usage)

  // SOLD PPW
  soldGrossPPW: 2292,                     // Sold Gross PPW (0% usage in sample)
  soldNetPPW: 2293,                       // Sold Net PPW (3.0% usage)
  soldNetPPW2: 2216,                      // Sold Net PPW2 (check usage)

  // COMMISSIONABLE
  commissionablePPW: 2480,                // ✅ Commissionable PPW (94.0% usage)
  commissionablePPWRndDelta: 2482,        // Commissionable PPW Rnd Delta (94.0%)

  // DEALER FEE
  dealerFeePPW: 545,                      // Dealer Fee PPW (94.0% usage)

  // ADDER IMPACT
  repAdderPPW: 544,                       // Rep Adder PPW (100% usage)
  totalAddOnPPW: 2114                     // Total Add-on PPW (100% usage)
}
```

### Financing
```javascript
{
  financing: 645,                         // Financing (lender name)
  financingPlan: 649                      // Financing Plan (loan term/type)
}
```

### Funding/Payments
```javascript
{
  // M1 (Milestone 1 - usually at permit or design)
  m1RequestDate: 442,                     // M1 Request Date (19.5%)
  expectedM1ReceivedDate: 444,            // Expected M1 Received Date (19.5%)
  fundingDashboardOfficialM1Requested: 2038, // Funding Dashboard: Official M1 Requested (19.5%)
  fundingDashboardReadyForM1Funding: 1992,   // Funding Dashboard: Ready for M1 Funding (20.5%)

  // M2 (Milestone 2 - usually at install)
  m2RequestDate: 445,                     // M2 Request Date (18.5%)
  expectedM2ReceivedDate: 447,            // Expected M2 Received Date (18.8%)
  expectedM2ClawbackDate: 908,            // Expected M2 Clawback Date (2.0%)
  m2CreditDate: 915,                      // M2 Credit Date (2.0%)
  fundingDashboardOfficialM2Requested: 2039, // Official M2 Requested (18.5%)
  fundingDashboardReadyForM2Funding: 1993,   // Ready for M2 Funding (52.3%)

  // M3 (Milestone 3 - usually at PTO)
  m3RequestDate: 513,                     // M3 Request Date (5.4%)
  expectedM3ReceivedDate: 512,            // Expected M3 Received Date (4.7%)
  fundingDashboardOfficialM3Requested: 2040, // Official M3 Requested (5.7%)
  fundingDashboardReadyForM3Funding: 1994,   // Ready for M3 Funding (20.5%)
  fundingDashboardMaxM3EventApprovedDate: 2029,    // Max M3 Event - Approved (0.3%)
  fundingDashboardMaxM3EventRequestedDate: 2028,   // Max M3 Event - Requested (0.3%)

  // FINAL FUNDING
  finalFundingSubmitted: 488,             // Final Funding Submitted (11.1%)
  finalFundingReceived: 489,              // Final Funding Received (10.4%)

  // INVOICE
  invoiceDate: 618,                       // Invoice Date (51.7%)

  // LENDER UPLOADS
  ficUploadedToLender: 555,               // FIC Uploaded to Lender (19.1%)
  ntpApprovalDate: 312,                   // NTP - Approval Date (19.1%)
  ntpSubmittedDate: 479                   // NTP - Submitted Date (18.8%)
}
```

**💡 Instructions:**
- **Essential:** `systemPrice` (133), `grossPPW` (19), `netPPW` (543)
- **For Reps:** `commissionablePPW` (2480) - what they get paid on
- **Funding:** M1/M2/M3 are milestone payments from lender
- Show "Ready for M2 Funding" (52.3% usage) to indicate install payment ready
- Invoice date shows when customer was billed

---

## 1️⃣7️⃣ ADDER FIELDS (ADD-ONS)

### Adder Counts
```javascript
{
  totalAdders: 252,                       // ✅ Total # of Adders (100% usage)
  numAddersApproved: 1046,                // ✅ # of Adders (Approved) (100%)
  numAdders: 1047,                        // # of Adders (100% - same as 252?)

  // BY TYPE
  numContractAdders: 2272,                // # of Contract Adders (100%)
  numPostSaleAdders: 2273,                // # of Post Sale Adders (100%)
  numElectricalUpgradeAdders: 2240,       // # of Electrical Upgrade Adders (100%)
  numNeedsReviewAdders: 2282,             // ✅ # of Needs Review Adders (100%)
  numRejectedContractAdders: 2413,        // # of Rejected Contract Adders (check usage)
  numPostSaleAdders2: 2414,               // # of Post-Sale Adders (check usage)
  numSalesAidAdderRecords: 2421           // # of Sales Aid Adder Records (check usage)
}
```

### Adder Costs
```javascript
{
  // CUSTOMER COST
  addersTotalCostCustomer: 253,           // ✅ Adders Total Cost - Customer (67.8% usage)
  totalAddOnCost: 128,                    // Total Add-On Cost (100%)

  // REP COST
  addersTotalCostRep: 254,                // Adders Total Cost - Rep (0.7%)
  repTotalAdderCost: 553,                 // Rep Total Adder Cost (100%)

  // TOTAL
  totalAdderCost: 2115,                   // ✅ Total Adder Cost (99.7% usage)
  costAddersTotal: 384,                   // Cost - Adders Total (94.0%)

  // EXCLUDING CHARGES
  addersTotalCostExcludingCharges: 2458,  // Adders Total Cost (excluding Charges) (0.7%)
  adderTotalCostRepCharges: 2457,         // Adder Total Cost - Rep Charges (check)

  // POST-SALE
  postSaleAddersTotalCost: 2274,          // Post Sale Adders Total Cost (check usage)

  // PPW IMPACT
  repAdderPPW: 544,                       // ✅ Rep Adder PPW (100% usage)
  totalAddOnPPW: 2114                     // ✅ Total Add-on PPW (100% usage)
}
```

### Adder Lists & Details
```javascript
{
  salesFacingAdderList: 2286,             // ✅ CRITICAL - Sales Facing Adder List (100%)
                                          // Format: "Product - Cost, Product - Cost"
                                          // Example: "Encharge 10kW - 12500, PPB - 2250"

  needsReviewAdderList: 2276,             // Needs-Review Adder List (100%)
  confirmedAdderList: 350,                // Confirmed Adder List (check usage)

  // DBLINK TO CHILD TABLE
  adders: 140,                            // Adders (dblink - 0% usage, query child table instead)
  adderReview: 353                        // Adder Review (dblink)
}
```

### Adder Audit
```javascript
{
  triggerAdderAudit: 1164,                // Trigger - Adder Audit (100% - checkbox)
  triggerAdderAuditDate: 1163,            // Trigger - Adder Audit (Date) (3.7%)
  recordIdAdderAudit: 1080                // Record ID# - Adder Audit (check usage)
}
```

### Adder URLs (for adding)
```javascript
{
  addAdder: 141,                          // Add Adder (URL) (100%)
  addDesignAdder: 1839                    // Add Design Adder (URL) (100%)
}
```

**💡 Instructions:**
- **CRITICAL:** `salesFacingAdderList` (2286) - shows all adders with costs
- Parse this field: Split by comma, extract product name and cost
- `totalAdders` (252) and `numAddersApproved` (1046) - show approval rate
- `numNeedsReviewAdders` (2282) - highlight if > 0 (needs attention)
- Query child table (bsaycczmf) for full adder details
- 67.8% of projects have adders

---

## 1️⃣8️⃣ HOLDS & BLOCKERS

### Hold Status
```javascript
{
  onHold: 231,                            // ✅ On Hold? (checkbox) (check usage)
  onHold2: 1217,                          // On Hold (text) (check usage)

  // SPECIFIC HOLD TYPES
  intakeHold: 1071,                       // Intake Hold (checkbox)
  welcomeCallHold: 1412,                  // Welcome Call Hold (checkbox)
  siteSurveyHold: 1413,                   // Site Survey Hold (checkbox)
  engineeringHold: 2099,                  // Engineering Hold (checkbox)
  permittingHold: 2100                    // Permitting Hold (checkbox)
}
```

### Hold Details
```javascript
{
  holdReason: 232,                        // ✅ Hold Reason (text field)
  blockReason: 233,                       // ✅ Block Reason (text field)
  holdBlockers: 1389,                     // ✅ Hold/Blockers (text-multi-line - detailed description)

  userPlacedOnHold: 234,                  // ✅ User Placed On Hold (user type)
  dateProjectPlacedOnHold: 235            // ✅ Date Project Placed On Hold (date)
}
```

### Escalation
```javascript
{
  projectEscalation: 302,                 // Project Escalation (High/Medium/ARC)
  projectPriority: 300                    // Project Priority (Insane/Urgent/Normal)
}
```

**💡 Instructions:**
- **Check:** `onHold` (231) checkbox first
- **Show:** `holdReason` (232) and/or `blockReason` (233)
- **Display:** `holdBlockers` (1389) for full description
- **Track:** `userPlacedOnHold` (234) and `dateProjectPlacedOnHold` (235)
- **Alert:** If `projectPriority` = "Insane" or "Urgent"
- **Escalate:** If `projectEscalation` = "High" or "ARC"
- 21% of projects are on hold - MAJOR issue to surface

---

## 1️⃣9️⃣ NOTES & COMMUNICATIONS

### Recent Notes
```javascript
{
  recentNoteCategory: 1141,               // Recent Note - Category (check usage)
  recentNoteContent: 1142,                // Recent Note - Content (check usage)
  recentNoteDateCreated: 1143,            // Recent Note - Date Created (31.2% usage)
  recentNotesDateTime: 1602,              // Recent Notes Date/Time (31.2%)
  recentNoteDateCreatedCancelARC: 1596,   // Recent Note - Date Created (Cancel/ARC) (7.4%)

  maxPCNoteDate: 2304,                    // Max PC Note Date (0.3%)
  maxNoteModified: 713                    // Maximum Note Date Modified (31.2%)
}
```

### Note Field
```javascript
{
  note: 570                               // Note (general notes field)
}
```

### Related Records (DBLinks)
```javascript
{
  tasksLink: 1074,                        // ✅ Tasks Link (dblink to Tasks table)
  ticketsLink: 2150,                      // Tickets Link (dblink)
  communicationsLink: 2137,               // Communications Link (dblink)
  statusUpdatesLink: 919,                 // Status Updates Link (dblink)

  // ADD RECORD URLs
  addTaskUrl: 1075,                       // Add Task URL
  addTicketUrl: 745,                      // Add Ticket URL
  addCommunicationUrl: 2138               // Add Communication URL
}
```

**💡 Instructions:**
- Use `recentNoteDateCreated` (1143) to show "Last Updated: X days ago"
- `recentNoteContent` (1142) - show snippet in project card
- Query child tables via dblinks for full history
- 31% of projects have recent note data

---

## 2️⃣0️⃣ WELCOME CALL & INTAKE

### Welcome Call
```javascript
{
  welcomeCallScheduled: 967,              // Welcome Call Scheduled (0.7% usage)
  welcomeCallStarted: 559,                // Welcome Call Started (0.7%)
  welcomeCallCompleted: 558               // Welcome Call Completed (0.7%)
}
```

### Installation Agreement (IA)
```javascript
{
  iaApprovalDate: 303,                    // IA - Approval Date (1.7% usage)
  installationAgreementDateTime: 1011,    // Installation Agreement Date/Time (0.3%)
  ntpDateTime: 1028                       // NTP Date/Time (0.3%)
}
```

**💡 Instructions:**
- Low usage (0.7-1.7%) - not critical
- Show if available as early-stage milestone
- Welcome call = customer onboarding

---

## 2️⃣1️⃣ CANCELLATION

### Cancellation Fields
```javascript
{
  maxDateCancel: 873,                     // Maximum Date Cancel (13.8% usage)
  xxMaxCancellationRecordIdCancellationDate: 1755,  // xx.Maximum Cancellation Record ID# - Date (41.6%)
  cancellationInvoiceSentDate: 951        // Cancellation Invoice Sent Date (3.0%)
}
```

**💡 Instructions:**
- 14-42% usage indicates significant cancellations
- Show cancellation date if `projectStatus` = "Cancelled"
- Useful for tracking when/why projects failed

---

## 2️⃣2️⃣ ADDITIONAL TRACKING FIELDS

### Timestamps & Modified Dates
```javascript
{
  dateCreated: 1,                         // Date Created (100%)
  dateModified: 2,                        // Date Modified (100%)
  maxModifiedDateTime: 677,               // Maximum Modified Date Time (38.9%)
  todayDate: 1197,                        // Today (Date) (100% - formula)
  nowDateTime: 893                        // Now Date/Time (100% - formula)
}
```

### Credit & Expiration
```javascript
{
  creditExpirationDate: 321,              // credit_expiration_date (4.4%)
  estimatedClawbackDate: 1659             // Estimated Clawback Date (18.5%)
}
```

### Triggers
```javascript
{
  triggerDocumentReviewDate: 1159,        // Trigger - Document Review (Date) (3.7%)
  triggerFinanceAuditDate: 1162,          // Trigger - Finance Audit (Date) (3.7%)
  triggerAdderAuditDate: 1163             // Trigger - Adder Audit (Date) (3.7%)
}
```

**💡 Instructions:**
- `dateModified` (2) useful for "Last Activity" indicator
- Trigger fields indicate when audits/reviews initiated
- Credit expiration shows financing deadline

---

## 📊 USAGE SUMMARY BY MILESTONE

### Data Reliability Tiers:

**Tier 1: Excellent (50%+ usage)**
- ✅ Install dates (50-54%)
- ✅ Sales date (100%)
- ✅ Core project info (100%)
- ✅ Team assignments (100%)
- ✅ Adder summaries (99%+)

**Tier 2: Good (20-30% usage)**
- 🟢 Design dates (24-27%)
- 🟢 Survey dates (25-27%)
- 🟢 Permit dates (21-24%)
- 🟢 NEM dates (22-24%)
- 🟢 PTO dates (20-21%)

**Tier 3: Fair (10-20% usage)**
- 🟡 HOA dates (10-11%)
- 🟡 Inspection dates (9-19%)
- 🟡 Funding dates (10-20%)

**Tier 4: Poor (<10% usage)**
- 🔴 Intake dates (5-6%)
- 🔴 Scheduled dates (0.7%)
- 🔴 Welcome call (0.7%)

---

## 🎯 RECOMMENDED FIELD SELECTION STRATEGY

### Minimal Dashboard (10 fields)
```javascript
[3, 11, 145, 255, 522, 534, 231, 232, 252, 2286]
// ID, Project ID, Name, Status, Sales Date, Install Date, On Hold, Hold Reason, Adder Count, Adder List
```

### Standard Dashboard (25 fields)
```javascript
[
  3, 11, 145, 146, 255, 300, // Core + Priority
  516, 517, 329, 330,         // Team
  522, 438,                   // Sales date + age
  164, 207, 326, 534, 537,    // Key milestone dates
  710, 2459,                  // Install scheduled + Design SLA
  231, 232, 1389,             // Holds
  252, 2286, 1046, 2282       // Adders
]
```

### Complete Dashboard (50+ fields)
```javascript
[
  // Core (6)
  3, 11, 145, 146, 148, 255,

  // Status & Priority (3)
  300, 301, 302,

  // Team (6)
  516, 517, 329, 330, 819, 820,

  // Financial (6)
  133, 13, 19, 543, 2480, 2115,

  // Timeline (15)
  522, 438, 164, 165, 315, 476,
  207, 208, 326, 327, 710, 534, 537, 538, 2459,

  // Holds (5)
  231, 232, 233, 234, 235,

  // Adders (6)
  252, 1046, 2282, 2286, 2115, 544,

  // System (3)
  13, 1147, 1123,

  // Notes (2)
  1142, 1143
]
```

---

## 💾 Export This as JSON Config

See `quickbase-complete-data/quickbase-config.json` for a production-ready version of these mappings.

**Remember:**
- Start with PRIMARY fields
- Fall back to BACKUPS if null
- Show "Not Available" if all are null
- Combine multiple dates for complete picture
- Use HIGHEST usage % fields for reliability
