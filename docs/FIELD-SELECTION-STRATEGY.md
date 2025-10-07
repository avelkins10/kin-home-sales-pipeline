# Field Selection Strategy - Complete Guide

**Updated:** October 2, 2025

## Understanding Usage Percentages

### ⚠️ IMPORTANT: Don't Ignore "Low" Usage Fields!

**A field with 15-20% usage is NOT "rarely used" - it means 15-20% of projects have REACHED that milestone.**

Since our 500-record sample includes projects at all stages (41% Active, 21% On Hold, 19% Completed), usage percentages reflect **milestone progression**, not field reliability.

---

## Usage Tier Interpretation

### 🟢 **Excellent (50%+)** - Half of projects reach this milestone
- **Install Completed:** 52.3%
- **Install Scheduled Date Capture:** 54.4%
- **Intake Install Date Tentative:** 100% (always filled at sale)

**Interpretation:** These milestones are reached by most projects OR are filled early in pipeline.

### 🟡 **Good (20-50%)** - 1 in 4 to 1 in 2 projects reach this
- **Design Completed:** 26.2%
- **Survey Submitted:** 26.5%
- **Permit Submitted:** 23.5%
- **NEM Submitted:** 24.2%
- **Estimated Install Date:** 27.5%

**Interpretation:** These are mid-pipeline milestones. Only completed/active projects reach them.

### 🟠 **Fair (10-20%)** - 1 in 10 projects reach this (STILL IMPORTANT!)
- **Passing Inspection:** 19.1%
- **PTO Approved:** 20.5%
- **Install Funding Received:** 13.8%
- **HOA Application:** 10.7%
- **Inspection Scheduled:** 9.7%

**Interpretation:** Later-stage milestones. These ARE used, just fewer projects get there.

### 🔴 **Rare (<10%)** - Truly optional or edge cases
- **Survey Scheduled:** 0.7% (most skip straight to submitted)
- **Inspection Failed:** 2.3% (only failed inspections)
- **Permit Resubmitted:** 6.7% (only rejected permits)

**Interpretation:** These are conditionally filled (failures, special cases, or legacy fields).

---

## Recommended Field Selection

### Minimal Dashboard (15 core fields)
For basic project list with status:
```javascript
{
  recordId: 3,
  projectId: 11,
  customerName: 145,
  projectStatus: 255,
  salesDate: 522,
  closerId: 516,
  setterId: 329,
  onHold: 231,
  holdReason: 232,
  totalAdders: 252,
  installScheduledDateCapture: 710,    // 54% usage
  installCompletedDate: 534,           // 52% usage
  ptoApprovedDate: 538,                // 20% usage - INCLUDE!
  projectAge: 438,
  systemSize: 13
}
```

### Standard Dashboard (40 fields)
For project detail view with 9-milestone timeline:

**Include ALL fields ≥10% usage:**

```javascript
{
  // Core (5 fields)
  recordId: 3,
  projectId: 11,
  customerName: 145,
  customerAddress: 146,
  projectStatus: 255,

  // Team (6 fields)
  closerId: 516,
  closerName: 517,
  setterId: 329,
  setterName: 330,
  projectCoordinatorId: 819,
  projectCoordinator: 820,

  // Holds (5 fields)
  onHold: 231,
  holdReason: 232,
  blockReason: 233,
  dateOnHold: 235,
  userPlacedOnHold: 234,

  // Intake Milestone (1 field - 100% usage!)
  intakeInstallDateTentative: 902,     // 100% - always filled

  // Survey Milestone (2 fields - 25-26% usage)
  surveySubmitted: 164,                // 26.5%
  surveyApproved: 165,                 // 25.2%

  // Design Milestone (3 fields - 25-27% usage)
  designCompleted: 315,                // 26.2%
  cadDesignApproved: 476,              // 26.2%
  designSLADeadline: 2459,             // 25.2% - SLA tracking!

  // HOA Milestone (2 fields - 10.7% usage) ⭐ INCLUDE!
  hoaApplicationSubmitted: 212,        // 10.7%
  hoaApplicationApproved: 213,         // 10.7%

  // Permit Milestone (2 fields - 21-23% usage)
  permitSubmitted: 207,                // 23.5%
  permitApproved: 208,                 // 21.8%

  // NEM Milestone (3 fields - 20-24% usage)
  nemSubmitted: 326,                   // 24.2%
  nemApproved: 327,                    // 22.8%
  ptoSubmitted: 537,                   // 21.1%
  ptoApproved: 538,                    // 20.5% ⭐ INCLUDE!

  // Install Milestone (4 fields - 19-54% usage)
  installScheduledDateCapture: 710,    // 54.4%
  installCompletedDate: 534,           // 52.3%
  installStartedDate: 464,             // 19.8% ⭐ INCLUDE!
  installFundingSubmitted: 486,        // 18.8% ⭐ INCLUDE!

  // Inspection Milestone (2 fields - 9-19% usage)
  passingInspectionCompleted: 491,     // 19.1% ⭐ INCLUDE!
  inspectionScheduledDate: 226,        // 9.7% - edge case, optional

  // Adders (3 fields)
  totalAdders: 252,
  totalAdderCost: 2115,
  salesFacingAdderList: 2286,

  // Financials (2 fields)
  systemPrice: 133,
  systemSize: 13,

  // Calculated (1 field)
  projectAge: 438
}
```

**Total: 42 fields** (includes all ≥10% usage milestone dates)

### Complete Dashboard (65+ fields)
For analytics and full milestone tracking:

Add these **backup/alternate fields** for robustness:

```javascript
{
  // ... all Standard Dashboard fields PLUS:

  // Install Backups
  installScheduledStartDate: 178,      // 20.1% - backup for scheduled
  installDateImport: 835,              // 44.6% - backup for completed
  estimatedInstallDate: 1124,          // 27.5% - planning

  // Install Funding (13-18% usage) ⭐ INCLUDE!
  installFundingReceived: 487,         // 13.8%

  // Survey Backups
  maximumSurveySubmitted: 575,         // 26.5% - timestamp version

  // Design Backups
  maximumCADDesignSubmitted: 475,      // 27.5%
  predesignApproved: 316,              // 27.2%

  // Permit Backups
  asBuiltSubmittedToAHJ: 614,          // 25.8%
  estimatedPermitReturnDate: 1777,     // 23.5%
  permitSubmittedDateCapture: 709,     // 23.2%

  // NEM Backups
  nemSubmittedCaptured: 716,           // 24.2%
  nemApprovedCaptured: 585,            // 22.5%
  nemSignaturesSent: 1844,             // 21.8%
  ptoUploadedToLender: 556,            // 19.8% ⭐ INCLUDE!
  interconnectionSignaturesSent: 2198, // 16.1% ⭐ INCLUDE!
  nemSignatureReceived: 1845,          // 12.8% ⭐ INCLUDE!

  // Inspection Backups
  firstInspectionScheduled: 1589,      // 9.7%

  // Activation/Completion
  readyForCommission: 813,             // 47.0%
  readyForCommissionOld: 841,          // 40.3%

  // Additional status fields
  projectPriority: 300,
  statusBarHTML: 301,

  // Additional financials
  grossPPW: 19,
  netPPW: 543,
  totalAdderPPW: 2114,

  // Additional adder tracking
  numAddersApproved: 1046,
  numNeedsReviewAdders: 2282
}
```

---

## Critical Insights

### 1. **Don't Skip 10-20% Fields!**

**HOA fields (10.7%)** → Only 10% of homes need HOA approval, but for those that do, this is CRITICAL
**PTO Approved (20.5%)** → Only 20% of projects completed PTO in sample, but it's the FINAL milestone
**Inspection (19%)** → Only completed projects have this - it's essential for tracking

### 2. **Why Some Fields Have Low Usage**

| Field | Usage | Why Low? | Include? |
|-------|-------|----------|----------|
| Inspection Scheduled | 9.7% | Only recent/upcoming inspections | ❌ No - use Completed instead |
| Survey Scheduled | 0.7% | Skipped - most go straight to Submitted | ❌ No |
| Permit Resubmitted | 6.7% | Only rejected permits | ✅ Yes - for troubleshooting |
| Inspection Failed | 2.3% | Only failures | ✅ Yes - for alerts |
| PTO Approved | 20.5% | Only completed projects | ✅ YES - final milestone! |

### 3. **Milestone Timeline Logic**

For a 9-milestone progress bar, use these PRIMARY fields:

```javascript
const MILESTONE_DATES = {
  // 1. Intake (100% - always has data)
  intake: 902,                         // intake_install_date_tentative

  // 2. Survey (26% - 1 in 4 reach this)
  survey: 164,                         // Survey Submitted Date

  // 3. Design (26% - same stage as survey)
  design: 315,                         // Design Completed

  // 4. NEM (24% - pre-permit for some utilities)
  nem: 326,                            // NEM Submitted Date

  // 5. Permit (23% - critical bottleneck)
  permit: 207,                         // Permit Submitted

  // 6. Install (54% - most reliable milestone)
  install: 710,                        // Install Scheduled Date Capture
  installComplete: 534,                // Install Completed Date (52%)

  // 7. Verification (skip - no good field)
  // Use install complete + 1-3 days estimate

  // 8. Inspection (19% - post-install)
  inspection: 491,                     // Passing Inspection Completed

  // 9. PTO (20% - final milestone) ⭐ INCLUDE!
  pto: 538                             // PTO Approved Date
};
```

**For projects not yet at a milestone:** Show as "Not Started" or "Pending"

---

## Dashboard Display Strategy

### Project Cards (List View)
```javascript
// Show these on every card:
- Customer Name (145)
- Project Status (255)
- Days Since Sale (438)
- On Hold Badge if true (231)
- Current Milestone: Latest filled date field
- Next Milestone: First empty date field ≥10% usage
```

### Project Detail (Timeline View)
```javascript
// Show all 9 milestones:
Intake      [✓ 100% of projects have this]
Survey      [✓ if filled, ⏳ if empty] 26% have data
Design      [✓ if filled, ⏳ if empty] 26% have data
NEM         [✓ if filled, ⏳ if empty] 24% have data
Permit      [✓ if filled, ⏳ if empty] 23% have data
Install     [✓ if filled, ⏳ if empty] 54% have data ⭐
Inspection  [✓ if filled, ⏳ if empty] 19% have data
PTO         [✓ if filled, ⏳ if empty] 20% have data ⭐
```

### Graceful Handling of Missing Data
```javascript
// For a project that's only reached Design:
✓ Intake: 2024-01-15
✓ Survey: 2024-01-22
✓ Design: 2024-02-05
⏳ NEM: Not yet started
⏳ Permit: Not yet started
⏳ Install: Estimated 2024-05-15 (from field 1124)
⏳ Inspection: Pending
⏳ PTO: Pending
```

---

## API Query Optimization

### Query Strategy for Timeline
```javascript
// Single query with ALL milestone dates ≥10% usage
POST /v1/records/query
{
  "from": "br9kwm8na",
  "select": [
    3, 11, 145, 255, 522,      // Core
    902,                        // Intake (100%)
    164, 165,                   // Survey (25-26%)
    315, 476,                   // Design (26%)
    212, 213,                   // HOA (10.7%) ⭐
    207, 208,                   // Permit (21-23%)
    326, 327,                   // NEM (22-24%)
    710, 534, 464, 486,         // Install (18-54%)
    491,                        // Inspection (19%) ⭐
    537, 538                    // PTO (20-21%) ⭐
  ],
  "where": "{516.EX.USER_ID}",
  "options": { "top": 100 }
}
```

**Result:** Complete milestone data for dashboard in ONE API call

---

## Summary

### ✅ DO Include:
- All fields ≥10% usage
- HOA fields (10.7%) - required for some homes
- Inspection (19%) - post-install milestone
- PTO (20%) - final milestone
- Install Funding (13-18%) - payment tracking
- NEM intermediate steps (12-16%) - signature tracking

### ❌ DON'T Include:
- Survey Scheduled (0.7%) - skipped field
- Welcome Call (0.7%) - legacy
- Activation Completed (0.3%) - not used
- Edge case "failure" fields unless building error dashboard

### 🎯 Sweet Spot: **42 fields**
- Core: 5
- Team: 6
- Holds: 5
- Milestones: 20 (all ≥10% usage)
- Adders: 3
- Financials: 2
- Calculated: 1

This gives you **complete milestone coverage** while staying performant for API queries.
