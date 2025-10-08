# Milestone Mapping & Field Audit

**Purpose**: Comprehensive audit of all milestone logic across the 3 existing systems
**Date Created**: 2025-01-08
**For Review By**: Austin Elkins

---

## 🚨 Critical Issues Found

1. **Traffic Lights Missing 3 Milestones**: HOA, Verification, and PTO not shown at all
2. **Mislabeled Traffic Light**: Last light says "PTO" but actually shows Inspection state
3. **Design Conflict**: Different complete conditions between systems
4. **Permit As-Built**: Only tracked in milestoneStatus.ts, not traffic-lights.ts
5. **Intake Missing**: milestoneStatus.ts has no intake logic

---

## Milestone 1: Intake

### Currently Used Fields

| Field | Field ID | Population | Used In | Purpose |
|-------|----------|------------|---------|---------|
| `FINANCE_INTAKE_APPROVED` | 1548 | 100% | traffic-lights.ts:79 | Marks intake as complete |
| `WEBHOOK_INTAKE_COMPLETE` | 603 | 100% | traffic-lights.ts:79, Timeline.tsx:38 | Alternative complete marker |
| `FINANCE_INTAKE_APPROVED_DATE` | 1549 | 0.3% ⚠️ | Timeline.tsx:57 | Date when approved |
| `SALES_DATE` | 522 | 100% | traffic-lights.ts:70 | Fallback date, project age calc |
| `PROJECT_STATUS` | 255 | 100% | traffic-lights.ts:74 | Detect rejected/cancelled |

### Available But Unused

| Field | Field ID | Population | Recommendation |
|-------|----------|------------|----------------|
| `INTAKE_COMPLETE` | 554 | 100% | ⚠️ **URL field** - should we use this instead? |
| `INTAKE_INSTALL_DATE_TENTATIVE` | 902 | 100% | Could show estimated timeline |

### Current State Logic

**Complete**: `FINANCE_INTAKE_APPROVED` exists OR `WEBHOOK_INTAKE_COMPLETE` exists
**Rejected**: `PROJECT_STATUS` contains "rejected" or "cancel"
**Overdue**: Project age > 7 days AND not complete
**In-Progress**: Default state

**Date Shown**: `FINANCE_INTAKE_APPROVED_DATE` (fallback: `SALES_DATE`)

### Conflicts & Questions

- ⚠️ **MAJOR**: `milestoneStatus.ts` has NO intake logic - assumes always complete!
- ⚠️ **QUESTION**: `FINANCE_INTAKE_APPROVED_DATE` only 0.3% populated - is this the right field?
- ⚠️ **QUESTION**: Should we use `INTAKE_COMPLETE` URL field (100% populated)?

**Your Decision Needed**:
- [ ] Keep current logic
- [ ] Add `INTAKE_COMPLETE` check
- [ ] Fix `FINANCE_INTAKE_APPROVED_DATE` or use different date field

---

## Milestone 2: Survey

### Currently Used Fields

| Field | Field ID | Population | Used In | Purpose |
|-------|----------|------------|---------|---------|
| `SURVEY_APPROVED` | 165 | 25.2% | All 3 systems | Marks survey complete |
| `SURVEY_SUBMITTED` | 164 | 26.5% | All 3 systems | Survey scheduled/submitted |
| `MAX_SURVEY_SUBMITTED` | 575 | 26.5% | milestoneStatus.ts:25 | Backup timestamp |
| `MAX_SURVEY_APPROVED` | 572 | 25.2% | (unused) | Backup timestamp |

### Available But Unused

| Field | Field ID | Population | Recommendation |
|-------|----------|------------|----------------|
| `SURVEY_SCHEDULED_DATE` | 164 | 26.5% | ⚠️ **Same as SURVEY_SUBMITTED** - consolidate? |
| `SURVEY_COMPLETED` | 165 | 25.2% | ⚠️ **Same as SURVEY_APPROVED** - consolidate? |

### Current State Logic

**Complete**: `SURVEY_APPROVED` exists
**In-Progress**: `SURVEY_SUBMITTED` OR `MAX_SURVEY_SUBMITTED` exists
**Pending**: Neither exists

**Date Shown**: `SURVEY_APPROVED` (when complete), `SURVEY_SUBMITTED` (when in-progress)

**Substeps** (milestoneStatus.ts only):
1. Submitted → `SURVEY_SUBMITTED`
2. Approved → `SURVEY_APPROVED`

### Conflicts & Questions

- ✅ **CONSISTENT**: All 3 systems agree on logic
- ⚠️ **QUESTION**: Multiple identical fields - `SURVEY_SCHEDULED_DATE` vs `SURVEY_SUBMITTED`?
- ⚠️ **QUESTION**: Do we need both regular and "MAX" timestamp versions?

**Your Decision Needed**:
- [ ] Keep all fields
- [ ] Consolidate duplicate fields
- [ ] Use only one timestamp version

---

## Milestone 3: Design

### Currently Used Fields

| Field | Field ID | Population | Used In | Purpose |
|-------|----------|------------|---------|---------|
| `DESIGN_COMPLETED` | 315 | 26.2% | milestoneStatus.ts, milestones.ts | **Final design completion** |
| `CAD_DESIGN_APPROVED` | 476 | 26.2% | traffic-lights.ts, milestones.ts | **CAD approval** |
| `PREDESIGN_APPROVED` | 316 | 27.2% | milestoneStatus.ts:55 | First substep |
| `SURVEY_APPROVED` | 165 | 25.2% | traffic-lights.ts:115 | Dependency check |
| `DESIGN_SLA_DEADLINE` | 2459 | 25.2% | milestoneStatus.ts:58 | Urgency tracking |

### Available But Unused

| Field | Field ID | Population | Recommendation |
|-------|----------|------------|----------------|
| `CAD_DESIGN_SUBMITTED` | 475 | 27.5% | ⚠️ Track submission separately? |
| `ENGINEERING_COMPLETED` | 2458 | 27.0% | ⚠️ **PE stamp** - important milestone? |

### Current State Logic

**❌ MAJOR CONFLICT**:
- `traffic-lights.ts`: Complete when `CAD_DESIGN_APPROVED` exists
- `milestoneStatus.ts`: Complete when `DESIGN_COMPLETED` exists
- `milestones.ts`: Complete when `DESIGN_COMPLETED` OR `CAD_DESIGN_APPROVED` exists

**Overdue**: Project age > 21 days (traffic-lights.ts only)
**Urgent**: `DESIGN_SLA_DEADLINE` within 3 days (milestoneStatus.ts only)

**Substeps** (milestoneStatus.ts only):
1. Predesign → `PREDESIGN_APPROVED`
2. CAD Approved → `CAD_DESIGN_APPROVED`
3. Complete → `DESIGN_COMPLETED`

### Conflicts & Questions

- 🚨 **CRITICAL CONFLICT**: What truly marks design as complete?
  - Option A: `CAD_DESIGN_APPROVED` (traffic-lights.ts)
  - Option B: `DESIGN_COMPLETED` (milestoneStatus.ts)
  - Option C: Either one (milestones.ts)
- ⚠️ **QUESTION**: Should `ENGINEERING_COMPLETED` be a separate substep?
- ⚠️ **QUESTION**: Is SLA deadline logic important enough to show in traffic lights?

**Your Decision Needed** (IMPORTANT):
- [ ] Complete = `CAD_DESIGN_APPROVED` only
- [ ] Complete = `DESIGN_COMPLETED` only
- [ ] Complete = Either one (current milestones.ts logic)
- [ ] Add `ENGINEERING_COMPLETED` as substep
- [ ] Keep/remove SLA urgency logic

---

## Milestone 4: HOA (Conditional)

### Currently Used Fields

| Field | Field ID | Population | Used In | Purpose |
|-------|----------|------------|---------|---------|
| `HOA_APPLICATION_SUBMITTED` | 212 | 10.7% | milestoneStatus.ts, Timeline.tsx | Application sent |
| `HOA_APPLICATION_APPROVED` | 213 | 10.7% | milestoneStatus.ts, Timeline.tsx | Approval received |

### Available But Unused

_No additional HOA-specific fields found_

### Current State Logic

**Complete**: `HOA_APPLICATION_APPROVED` exists
**In-Progress**: `HOA_APPLICATION_SUBMITTED` exists (warning: 30-90 day wait)
**Not Required**: Neither field has value

**Date Shown**: `HOA_APPLICATION_APPROVED` or `HOA_APPLICATION_SUBMITTED`

**Substeps**:
1. Submitted → `HOA_APPLICATION_SUBMITTED`
2. Approved → `HOA_APPLICATION_APPROVED`

### Conflicts & Questions

- 🚨 **MISSING**: HOA milestone completely absent from `TrafficLightPipeline`!
- 🚨 **MISSING**: `milestones.ts` getCurrentMilestone() doesn't check HOA!
- ✅ **ONLY IN**: Timeline.tsx and milestoneStatus.ts show it
- ⚠️ **QUESTION**: Should HOA always be shown, or only when fields are populated?
- ⚠️ **QUESTION**: Where does HOA fit in the traffic light sequence?

**Your Decision Needed**:
- [ ] Add HOA to traffic lights (between which milestones?)
- [ ] Keep HOA only in Timeline (conditional display)
- [ ] How do we detect if project requires HOA? (just check if fields exist?)

---

## Milestone 5: NEM

### Currently Used Fields

| Field | Field ID | Population | Used In | Purpose |
|-------|----------|------------|---------|---------|
| `NEM_APPROVED` | 327 | 22.8% | All 3 systems | NEM approval received |
| `NEM_SUBMITTED` | 326 | 24.2% | All 3 systems | Submitted to utility |
| `NEM_SIGNATURES_SENT` | 1844 | 21.8% | milestoneStatus.ts:215 | DocuSign sent |
| `NEM_APPROVED_CAPTURED` | 585 | 22.5% | (unused backup) | Timestamp version |
| `NEM_SUBMITTED_CAPTURED` | 716 | 24.2% | (unused backup) | Timestamp version |

### Available But Unused

| Field | Field ID | Population | Recommendation |
|-------|----------|------------|----------------|
| `NEM_SIGNATURE_RECEIVED` | 1845 | 12.8% | ⚠️ Track customer signature separately? |
| `INTERCONNECTION_SIGNATURES_SENT` | 2198 | 16.1% | ⚠️ Is this part of NEM or separate? |

### Current State Logic

**Complete**: `NEM_APPROVED` exists
**In-Progress**: `NEM_SUBMITTED` OR `NEM_SIGNATURES_SENT` exists
**Blocked** (milestoneStatus.ts only): Signatures sent >7 days ago but not submitted
**Pending**: `CAD_DESIGN_APPROVED` doesn't exist

**Urgency Logic** (milestoneStatus.ts only):
- If signatures sent >7 days ago: "Customer signatures needed - follow up!"

**Substeps**:
1. Signatures Sent → `NEM_SIGNATURES_SENT`
2. Submitted → `NEM_SUBMITTED`
3. Approved → `NEM_APPROVED`

### Conflicts & Questions

- ⚠️ **CONFLICT**: traffic-lights.ts requires BOTH `CAD_DESIGN_APPROVED` AND `NEM_APPROVED` for dependencies
- ✅ **BLOCKED STATE**: milestoneStatus.ts has sophisticated >7 day waiting logic
- ⚠️ **QUESTION**: Should we track `NEM_SIGNATURE_RECEIVED` between sent and submitted?
- ⚠️ **QUESTION**: Is `INTERCONNECTION_SIGNATURES_SENT` part of NEM or separate PTO step?

**Your Decision Needed**:
- [ ] Add `NEM_SIGNATURE_RECEIVED` check
- [ ] Clarify `INTERCONNECTION_SIGNATURES_SENT` usage
- [ ] Keep/adjust 7-day blocked logic

---

## Milestone 6: Permit

### Currently Used Fields

| Field | Field ID | Population | Used In | Purpose |
|-------|----------|------------|---------|---------|
| `PERMIT_APPROVED` | 208 | 21.8% | All 3 systems | Permit approval |
| `PERMIT_SUBMITTED` | 207 | 23.5% | All 3 systems | Submitted to AHJ |
| `AS_BUILT_SUBMITTED_TO_AHJ` | 614 | 25.8% | **milestoneStatus.ts ONLY** | Final as-built plans |

### Available But Unused

| Field | Field ID | Population | Recommendation |
|-------|----------|------------|----------------|
| `ESTIMATED_PERMIT_RETURN_DATE` | 1777 | 23.5% | ⚠️ Show estimated timeline? |
| `PERMIT_SUBMITTED_DATE_CAPTURE` | 709 | 23.2% | ⚠️ Duplicate of PERMIT_SUBMITTED? |
| `PERMIT_RESUBMITTED` | 205 | 6.7% | ⚠️ Track rejections/resubmissions? |

### Current State Logic

**❌ MAJOR CONFLICT**:
- `traffic-lights.ts`: Complete when `PERMIT_APPROVED` exists
- `milestoneStatus.ts`: Complete when `AS_BUILT_SUBMITTED_TO_AHJ` OR `PERMIT_APPROVED` exists
- **Special case**: If install complete but no as-built, permit shows as in-progress with warning

**In-Progress**: `PERMIT_SUBMITTED` exists
**Warning**: If submitted >30 days ago (milestoneStatus.ts only)

**Substeps** (milestoneStatus.ts only):
1. Submitted → `PERMIT_SUBMITTED`
2. Approved → `PERMIT_APPROVED`
3. As-Built → `AS_BUILT_SUBMITTED_TO_AHJ`

### Conflicts & Questions

- 🚨 **MAJOR FEATURE**: milestoneStatus.ts tracks as-built as final permit step
- 🚨 **MISSING**: traffic-lights.ts doesn't check as-built at all
- ⚠️ **QUESTION**: Does permit truly complete at `PERMIT_APPROVED` or `AS_BUILT_SUBMITTED_TO_AHJ`?
- ⚠️ **QUESTION**: Should we track permit resubmissions?
- ⚠️ **QUESTION**: Show estimated return date?

**Your Decision Needed** (IMPORTANT):
- [ ] Complete = `PERMIT_APPROVED` only (traffic-lights logic)
- [ ] Complete = `AS_BUILT_SUBMITTED_TO_AHJ` required (milestoneStatus logic)
- [ ] Complete = Either one
- [ ] Add resubmission tracking

---

## Milestone 7: Install

### Currently Used Fields

| Field | Field ID | Population | Used In | Purpose |
|-------|----------|------------|---------|---------|
| `INSTALL_COMPLETED_DATE` | 534 | 52.3% | All 3 systems | Install completion |
| `INSTALLATION_COMPLETED_AT` | 587 | 52.3% | traffic-lights, milestones.ts | Timestamp version |
| `READY_FOR_COMMISSION` | 813 | 47.0% | **milestoneStatus.ts ONLY** | System commissioned |
| `INSTALL_SCHEDULED_DATE_CAPTURE` | 710 | 54.4% | All 3 systems | Scheduled date (highest usage!) |
| `ESTIMATED_INSTALL_DATE` | 1124 | 27.5% | traffic-lights.ts | Backup date |
| `INSTALL_SCHEDULED_START_DATE` | 178 | 20.1% | traffic-lights.ts | Another backup date |

### Available But Unused

| Field | Field ID | Population | Recommendation |
|-------|----------|------------|----------------|
| `INSTALL_STARTED_DATE` | 464 | 19.8% | ⚠️ Track when crew arrived? |
| `INSTALL_DATE_IMPORT` | 835 | 44.6% | ⚠️ Legacy import field? |
| `INSTALL_FUNDING_SUBMITTED` | 486 | 18.8% | ⚠️ **M2 milestone**? |
| `INSTALL_FUNDING_RECEIVED` | 487 | 13.8% | ⚠️ **M2 funding received**? |

### Current State Logic

**Complete**: `INSTALL_COMPLETED_DATE` OR `INSTALLATION_COMPLETED_AT` OR `READY_FOR_COMMISSION`
**Upcoming** (milestoneStatus.ts only): Scheduled within 7 days
**In-Progress**: Any of 3 scheduled date fields exist
**Pending**: `NEM_APPROVED` and `PERMIT_APPROVED` don't exist

**Substeps** (milestoneStatus.ts only):
1. Scheduled → `INSTALL_SCHEDULED_DATE_CAPTURE`
2. Completed → `INSTALL_COMPLETED_DATE`
3. Ready → `READY_FOR_COMMISSION`

### Conflicts & Questions

- ⚠️ **TOO MANY DATE FIELDS**: 3 different schedule date fields, 2 different completion fields
- ✅ **UPCOMING STATE**: milestoneStatus.ts has nice "upcoming" state for installs within 7 days
- ⚠️ **QUESTION**: Do we need M2 funding as a substep?
- ⚠️ **QUESTION**: Should we track `INSTALL_STARTED_DATE` separately?
- ⚠️ **QUESTION**: Which date fields should be primary vs fallback?

**Your Decision Needed**:
- [ ] Consolidate date fields (which ones are most accurate?)
- [ ] Add M2 funding substeps
- [ ] Keep "upcoming" state for traffic lights

---

## Milestone 8: Verification (Calculated)

### Currently Used Fields

| Field | Field ID | Population | Used In | Purpose |
|-------|----------|------------|---------|---------|
| `INSTALL_COMPLETED_DATE` | 534 | 52.3% | milestoneStatus.ts | Base for calculation |
| `PASSING_INSPECTION_COMPLETED` | 491 | 19.1% | milestoneStatus.ts | Override if inspection passed |

### Available But Unused

_No specific verification fields - this is a calculated milestone_

### Current State Logic

**Complete**:
- `PASSING_INSPECTION_COMPLETED` exists, OR
- 1+ days since `INSTALL_COMPLETED_DATE`

**In-Progress**: Same day as install completion (< 1 day)
**Pending**: Install not complete

**Calculated Date**: `INSTALL_COMPLETED_DATE` + 2 days

### Conflicts & Questions

- 🚨 **MISSING**: Verification completely absent from `TrafficLightPipeline`!
- 🚨 **MISSING**: Not checked in `milestones.ts` getCurrentMilestone()!
- ✅ **ONLY IN**: Timeline.tsx and milestoneStatus.ts
- ⚠️ **QUESTION**: Should this be shown in traffic lights? Or is 1-2 days too quick?
- ⚠️ **QUESTION**: Auto-complete after 1 day or wait for actual field?

**Your Decision Needed**:
- [ ] Add to traffic lights
- [ ] Keep only in Timeline (too fast for traffic lights)
- [ ] Require actual verification field instead of auto-calculating

---

## Milestone 9: Inspection

### Currently Used Fields

| Field | Field ID | Population | Used In | Purpose |
|-------|----------|------------|---------|---------|
| `PASSING_INSPECTION_COMPLETED` | 491 | 19.1% | All 3 systems | Inspection passed |
| `AS_BUILT_SUBMITTED_TO_AHJ` | 614 | 25.8% | milestoneStatus.ts | Prerequisite step |
| `INSTALL_COMPLETED_DATE` | 534 | 52.3% | traffic-lights.ts | Dependency check |

### Available But Unused

| Field | Field ID | Population | Recommendation |
|-------|----------|------------|----------------|
| `INSPECTION_SCHEDULED_DATE` | 226 | 9.7% | ⚠️ Show upcoming inspection? |
| `FIRST_INSPECTION_SCHEDULED` | 1589 | 9.7% | ⚠️ Same as above? |

### Current State Logic

**Complete**: `PASSING_INSPECTION_COMPLETED` exists
**In-Progress**: `AS_BUILT_SUBMITTED_TO_AHJ` exists (milestoneStatus.ts) OR install complete (traffic-lights.ts)
**Pending**: Install not complete

**Substeps** (milestoneStatus.ts only):
1. As-Built → `AS_BUILT_SUBMITTED_TO_AHJ`
2. Passed → `PASSING_INSPECTION_COMPLETED`

### Conflicts & Questions

- ✅ **CONSISTENT**: All systems use `PASSING_INSPECTION_COMPLETED` for completion
- ⚠️ **QUESTION**: Should we show inspection scheduled date?
- ⚠️ **QUESTION**: Track failed inspections separately?

**Your Decision Needed**:
- [ ] Add inspection scheduled date
- [ ] Keep current logic

---

## Milestone 10: PTO

### Currently Used Fields

| Field | Field ID | Population | Used In | Purpose |
|-------|----------|------------|---------|---------|
| `PTO_APPROVED` | 538 | 20.5% | milestoneStatus.ts, milestones.ts | PTO approved |
| `PTO_SUBMITTED` | 537 | 21.1% | milestoneStatus.ts | Submitted to utility |
| `PTO_UPLOADED_TO_LENDER` | 556 | 19.8% | **milestoneStatus.ts ONLY** | M3 funding! 🎉 |

### Available But Unused

_No additional PTO fields_

### Current State Logic

**Complete**: `PTO_APPROVED` OR `PTO_UPLOADED_TO_LENDER` exists
**In-Progress**: `PTO_SUBMITTED` exists
**Warning**: If submitted >42 days ago (milestoneStatus.ts only)

**Celebration**: `PTO_UPLOADED_TO_LENDER` triggers celebration state 🎉

**Substeps** (milestoneStatus.ts only):
1. Submitted → `PTO_SUBMITTED`
2. Approved → `PTO_APPROVED`
3. Uploaded → `PTO_UPLOADED_TO_LENDER` (M3 Funded!)

### Conflicts & Questions

- 🚨 **CRITICAL BUG**: `TrafficLightPipeline` line 30 says "PTO" but `milestoneId='inspection'`!
- 🚨 **COMPLETELY MISSING**: PTO milestone not in traffic lights at all!
- ✅ **M3 CELEBRATION**: milestoneStatus.ts has nice celebration state for lender upload
- ⚠️ **QUESTION**: Should PTO show as separate traffic light or combined with inspection?
- ⚠️ **QUESTION**: Is 42-day warning threshold correct?

**Your Decision Needed** (CRITICAL):
- [ ] Add PTO as separate traffic light (10 total lights)
- [ ] Combine PTO with Inspection (keep 7 lights)
- [ ] Keep M3 celebration logic
- [ ] Adjust 42-day warning

---

## Summary of Decisions Needed

### Critical (Must Decide)

1. **Design Complete**: CAD approved, Design completed, or either?
2. **Permit Complete**: Permit approved only, or require as-built?
3. **PTO Traffic Light**: Add as separate or combine with Inspection?
4. **HOA Placement**: Where in traffic light sequence?

### Important

5. **Intake Date**: Fix `FINANCE_INTAKE_APPROVED_DATE` (0.3% populated) or use different field?
6. **Verification**: Add to traffic lights or keep Timeline-only?
7. **M2 Funding**: Track install funding as substep?

### Nice to Have

8. Consolidate duplicate date fields
9. Add inspection scheduled date
10. Track permit resubmissions

---

## Next Steps

1. ✅ Review this document
2. ✅ Make decisions on critical questions above
3. ✅ Update `milestones.json` with your corrections
4. ✅ Review `FIELD_USAGE_AUDIT.md` (next file)
5. → Build unified engine with approved logic
