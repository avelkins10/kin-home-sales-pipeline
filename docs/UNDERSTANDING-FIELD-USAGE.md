# Understanding Field Usage Percentages

**Critical Concept:** Usage percentages ≠ Field reliability

## What Usage Percentages Actually Mean

### ❌ WRONG Interpretation
"PTO Approved has 20% usage, so it's unreliable and rarely used - let's skip it."

### ✅ CORRECT Interpretation
"PTO Approved has 20% usage because only 20% of projects in our sample have REACHED the final PTO milestone. This is a CRITICAL field for tracking project completion."

---

## Why This Matters

Our 500-record sample includes projects at ALL stages:
- **41% Active** (early/mid pipeline)
- **21% On Hold** (stalled at various stages)
- **19% Completed** (finished all milestones)
- **14% Cancelled** (never finished)

**Result:** Later-stage milestone fields will have lower usage percentages, but they're still essential!

---

## Field Usage by Milestone Stage

### Intake (100% usage)
```javascript
intakeInstallDateTentative: 902  // 100% - ALWAYS filled at sale
```
**Why 100%?** Every sold project gets this field filled immediately.

### Survey (25-26% usage)
```javascript
surveySubmitted: 164             // 26.5%
surveyApproved: 165              // 25.2%
```
**Why 25%?** Only ~1 in 4 projects in sample have reached survey stage.
**Include?** ✅ YES - Essential mid-pipeline milestone

### Design (24-27% usage)
```javascript
designCompleted: 315             // 26.2%
cadDesignApproved: 476           // 26.2%
```
**Why 26%?** Same stage as survey - only ~1 in 4 reach this.
**Include?** ✅ YES - Critical for tracking design bottlenecks

### HOA (10.7% usage)
```javascript
hoaApplicationSubmitted: 212     // 10.7%
hoaApplicationApproved: 213      // 10.7%
```
**Why 10%?** Only ~10-15% of homes actually NEED HOA approval!
**Include?** ✅ YES - For the 10% that need it, this is CRITICAL

### Permit (21-23% usage)
```javascript
permitSubmitted: 207             // 23.5%
permitApproved: 208              // 21.8%
```
**Why 23%?** Only completed/near-complete projects reach permitting.
**Include?** ✅ YES - Essential milestone

### NEM/Interconnection (12-24% usage)
```javascript
nemSubmitted: 326                // 24.2%
nemApproved: 327                 // 22.8%
ptoSubmitted: 537                // 21.1%
ptoApproved: 538                 // 20.5% ⭐
interconnectionSignaturesSent: 2198  // 16.1%
nemSignatureReceived: 1845       // 12.8%
```
**Why 12-24%?** Later-stage milestone, only advanced projects reach this.
**Include?** ✅ YES - These track the approval workflow (Sales→Design→Ops)

### Install (18-54% usage)
```javascript
installScheduledDateCapture: 710 // 54.4% - Most reliable milestone!
installCompletedDate: 534        // 52.3%
installStartedDate: 464          // 19.8%
installFundingSubmitted: 486     // 18.8%
installFundingReceived: 487      // 13.8%
```
**Why 18-54%?** High usage because many projects reach install. Lower percentages for funding because payment timing varies.
**Include?** ✅ YES - ALL of these! Install is most important milestone.

### Inspection (9-19% usage)
```javascript
passingInspectionCompleted: 491  // 19.1%
inspectionScheduledDate: 226     // 9.7%
```
**Why 19%?** Only post-install projects have this.
**Include?** ✅ YES - Critical post-install milestone

### PTO (20% usage) ⭐ THE FINAL MILESTONE
```javascript
ptoApproved: 538                 // 20.5%
```
**Why 20%?** Only 20% of projects in sample are FULLY COMPLETE.
**Include?** ✅ ABSOLUTELY - This is the finish line!

---

## Decision Matrix: Include or Skip?

| Usage % | Interpretation | Include? | Example |
|---------|---------------|----------|---------|
| **50%+** | Very common milestone OR early-stage field | ✅ Always | Install Completed (52%) |
| **20-50%** | Mid-pipeline milestone | ✅ Always | Design Completed (26%) |
| **10-20%** | Later-stage OR conditional milestone | ✅ Yes! | PTO Approved (20%), HOA (10%) |
| **5-10%** | Edge cases OR resubmissions | ⚠️ Maybe | Permit Resubmitted (6.7%) |
| **<5%** | Rarely used, legacy, or test fields | ❌ Skip | Survey Scheduled (0.7%) |

---

## Real-World Examples

### Example 1: PTO Approved (20.5% usage)

**Sample:** 500 projects
- 100 completed projects (20%)
- 200 active projects (40%) - not yet to PTO
- 100 on-hold projects (20%) - stalled
- 100 cancelled (20%) - never finished

**Only the 100 completed projects would have PTO dates = 20% usage**

✅ **Conclusion:** This is a CRITICAL field! It marks project completion.

### Example 2: HOA Application (10.7% usage)

**Sample:** 500 projects
- ~50 projects in HOA-required neighborhoods (10%)
- ~450 projects without HOA requirements (90%)

**Only the 50 HOA-required projects would have these dates = 10% usage**

✅ **Conclusion:** For the 10% that need it, this field is ESSENTIAL. Don't skip!

### Example 3: Survey Scheduled (0.7% usage)

**Sample:** 500 projects
- 3-4 projects with "scheduled" date filled (0.7%)
- 130 projects with "submitted" date filled (26%)

**Why so low?** Most projects skip straight from "not started" to "submitted". The "scheduled" date is rarely filled.

❌ **Conclusion:** Skip this field - use "Survey Submitted" instead.

---

## Recommended Thresholds

### For Basic Dashboard (Project List)
**Minimum usage:** 20%
**Included fields:** ~15 fields
- Core info (status, customer, dates)
- High-usage milestones only (Install, Design, Permit)

### For Standard Dashboard (Project Detail)
**Minimum usage:** 10%
**Included fields:** ~40 fields
- All core fields
- All milestones ≥10% usage
- Team, holds, adders

### For Complete Dashboard (Analytics)
**Minimum usage:** 5-10% for edge cases
**Included fields:** ~65 fields
- All standard fields
- Backup/alternate fields
- Failure/resubmission tracking
- Payment milestones (M1/M2/M3)

---

## How to Present Missing Data in UI

### For 10-20% Usage Fields

```javascript
// GOOD - Shows field with graceful handling
if (project.ptoApproved) {
  return `PTO: ${formatDate(project.ptoApproved)}`;
} else {
  return `PTO: Pending`;  // Not "N/A" - implies it WILL happen
}
```

```javascript
// GOOD - Shows progress bar with future milestones
const milestones = [
  { name: 'Intake', date: project.intakeDate, status: '✓' },
  { name: 'Survey', date: project.surveySubmitted, status: project.surveySubmitted ? '✓' : '⏳' },
  { name: 'Design', date: project.designCompleted, status: project.designCompleted ? '✓' : '⏳' },
  { name: 'Permit', date: project.permitSubmitted, status: project.permitSubmitted ? '✓' : '⏳' },
  { name: 'Install', date: project.installCompleted, status: project.installCompleted ? '✓' : '⏳' },
  { name: 'Inspection', date: project.inspectionPassed, status: project.inspectionPassed ? '✓' : '⏳' },
  { name: 'PTO', date: project.ptoApproved, status: project.ptoApproved ? '✓' : '⏳' }
];
```

### For <5% Usage Fields (Skip These)

```javascript
// BAD - Don't waste API calls on these
surveyScheduled: 166,    // 0.7% - almost never filled
welcomeCallCompleted: 558, // 0.7% - legacy field
```

---

## Summary: The Golden Rule

**Don't judge a field by its usage percentage alone!**

Ask yourself:
1. **Is this a milestone?** → If yes, and usage ≥10%, INCLUDE IT
2. **Is this conditional?** (HOA, failures) → If yes, INCLUDE IT (it's rare but critical when needed)
3. **Is this a "scheduled" date?** → If usage <5%, SKIP IT (use submitted/completed instead)
4. **Is this a legacy/test field?** → If usage <1%, SKIP IT

**Bottom line:** Include ALL milestone dates ≥10% usage. These represent real workflow stages, not "unreliable" fields.
