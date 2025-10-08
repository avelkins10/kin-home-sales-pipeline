# Field Usage Audit - Milestone Fields

**Purpose**: Complete inventory of all milestone-related fields and their current usage
**Date Created**: 2025-01-08
**For Review By**: Austin Elkins

---

## Legend

- ✅ = Used in all 3 systems (traffic-lights.ts, milestoneStatus.ts, milestones.ts)
- 🟡 = Used in 1-2 systems
- ❌ = Not used in any milestone logic
- ⚠️ = Low population in QuickBase (< 20%)
- 🎯 = High population, recommended for use

---

## Milestone 1: Intake

| Field Name | ID | Pop% | Status | Used In | Recommendation |
|------------|-----|------|--------|---------|----------------|
| `FINANCE_INTAKE_APPROVED` | 1548 | 100% 🎯 | ✅ | traffic-lights, Timeline | **KEEP** - Primary complete marker |
| `WEBHOOK_INTAKE_COMPLETE` | 603 | 100% 🎯 | 🟡 | traffic-lights, Timeline | **KEEP** - Alternative marker |
| `FINANCE_INTAKE_APPROVED_DATE` | 1549 | 0.3% ⚠️ | 🟡 | Timeline only | **FIX OR REPLACE** - Almost never populated! |
| `INTAKE_COMPLETE` | 554 | 100% 🎯 | ❌ | None | **CONSIDER** - URL field, 100% populated |
| `INTAKE_INSTALL_DATE_TENTATIVE` | 902 | 100% 🎯 | ❌ | None | **OPTIONAL** - Could show estimated date |
| `SALES_DATE` | 522 | 100% 🎯 | ✅ | All systems | **KEEP** - Fallback date, age calculation |
| `PROJECT_STATUS` | 255 | 100% 🎯 | ✅ | All systems | **KEEP** - Detect rejected/cancelled |

**Issues**:
- 🚨 `FINANCE_INTAKE_APPROVED_DATE` only 0.3% populated - likely wrong field!
- ⚠️ milestoneStatus.ts doesn't check intake completion at all

---

## Milestone 2: Survey

| Field Name | ID | Pop% | Status | Used In | Recommendation |
|------------|-----|------|--------|---------|----------------|
| `SURVEY_APPROVED` | 165 | 25.2% | ✅ | All 3 systems | **KEEP** - Primary complete marker |
| `SURVEY_SUBMITTED` | 164 | 26.5% | ✅ | All 3 systems | **KEEP** - In-progress marker |
| `MAX_SURVEY_SUBMITTED` | 575 | 26.5% | 🟡 | milestoneStatus only | **OPTIONAL** - Backup timestamp |
| `MAX_SURVEY_APPROVED` | 572 | 25.2% | ❌ | None | **CONSIDER** - Unused backup |
| `SURVEY_SCHEDULED_DATE` | 164 | 26.5% | ❌ | None | **DUPLICATE?** - Same ID as SURVEY_SUBMITTED |
| `SURVEY_COMPLETED` | 165 | 25.2% | ❌ | None | **DUPLICATE?** - Same ID as SURVEY_APPROVED |

**Issues**:
- ⚠️ `SURVEY_SCHEDULED_DATE` and `SURVEY_COMPLETED` appear to be duplicates
- Consider consolidating to reduce confusion

---

## Milestone 3: Design

| Field Name | ID | Pop% | Status | Used In | Recommendation |
|------------|-----|------|--------|---------|----------------|
| `DESIGN_COMPLETED` | 315 | 26.2% | ✅ | milestoneStatus, milestones | **KEEP** - One option for complete |
| `CAD_DESIGN_APPROVED` | 476 | 26.2% | ✅ | traffic-lights, milestones | **KEEP** - Other option for complete |
| `PREDESIGN_APPROVED` | 316 | 27.2% 🎯 | 🟡 | milestoneStatus only | **KEEP** - First substep |
| `DESIGN_SLA_DEADLINE` | 2459 | 25.2% | 🟡 | milestoneStatus only | **KEEP** - Urgency tracking |
| `CAD_DESIGN_SUBMITTED` | 475 | 27.5% 🎯 | ❌ | None | **CONSIDER** - Track submission separately? |
| `ENGINEERING_COMPLETED` | 2458 | 27.0% | ❌ | None | **CONSIDER** - PE stamp milestone? |

**Issues**:
- 🚨 **CRITICAL**: Systems disagree on what marks design "complete"
- Decision needed: CAD approved vs Design completed vs either

---

## Milestone 4: HOA (Conditional)

| Field Name | ID | Pop% | Status | Used In | Recommendation |
|------------|-----|------|--------|---------|----------------|
| `HOA_APPLICATION_SUBMITTED` | 212 | 10.7% ⚠️ | 🟡 | milestoneStatus, Timeline | **KEEP** - Required for HOA projects |
| `HOA_APPLICATION_APPROVED` | 213 | 10.7% ⚠️ | 🟡 | milestoneStatus, Timeline | **KEEP** - Completion marker |

**Issues**:
- 🚨 Completely missing from TrafficLightPipeline
- 🚨 Not checked in milestones.ts getCurrentMilestone()
- Low population (10.7%) because it's conditional - this is normal

---

## Milestone 5: NEM

| Field Name | ID | Pop% | Status | Used In | Recommendation |
|------------|-----|------|--------|---------|----------------|
| `NEM_APPROVED` | 327 | 22.8% | ✅ | All 3 systems | **KEEP** - Complete marker |
| `NEM_SUBMITTED` | 326 | 24.2% | ✅ | All 3 systems | **KEEP** - In-progress marker |
| `NEM_SIGNATURES_SENT` | 1844 | 21.8% | 🟡 | milestoneStatus only | **KEEP** - DocuSign tracking |
| `NEM_APPROVED_CAPTURED` | 585 | 22.5% | ❌ | None | **OPTIONAL** - Backup timestamp |
| `NEM_SUBMITTED_CAPTURED` | 716 | 24.2% | ❌ | None | **OPTIONAL** - Backup timestamp |
| `NEM_SIGNATURE_RECEIVED` | 1845 | 12.8% ⚠️ | ❌ | None | **CONSIDER** - Track customer signature? |
| `INTERCONNECTION_SIGNATURES_SENT` | 2198 | 16.1% ⚠️ | ❌ | None | **CLARIFY** - Part of NEM or separate PTO step? |

**Issues**:
- ⚠️ `INTERCONNECTION_SIGNATURES_SENT` - need to clarify if this is NEM or PTO related

---

## Milestone 6: Permit

| Field Name | ID | Pop% | Status | Used In | Recommendation |
|------------|-----|------|--------|---------|----------------|
| `PERMIT_APPROVED` | 208 | 21.8% | ✅ | All 3 systems | **KEEP** - Approval marker |
| `PERMIT_SUBMITTED` | 207 | 23.5% | ✅ | All 3 systems | **KEEP** - In-progress marker |
| `AS_BUILT_SUBMITTED_TO_AHJ` | 614 | 25.8% 🎯 | 🟡 | milestoneStatus only | **CRITICAL DECISION** - Final permit step? |
| `ESTIMATED_PERMIT_RETURN_DATE` | 1777 | 23.5% | ❌ | None | **CONSIDER** - Show timeline |
| `PERMIT_SUBMITTED_DATE_CAPTURE` | 709 | 23.2% | ❌ | None | **DUPLICATE?** - Same as PERMIT_SUBMITTED? |
| `PERMIT_RESUBMITTED` | 205 | 6.7% ⚠️ | ❌ | None | **CONSIDER** - Track rejections |

**Issues**:
- 🚨 **MAJOR**: milestoneStatus requires as-built, traffic-lights doesn't
- Decision needed: Is as-built required for permit completion?

---

## Milestone 7: Install

| Field Name | ID | Pop% | Status | Used In | Recommendation |
|------------|-----|------|--------|---------|----------------|
| `INSTALL_COMPLETED_DATE` | 534 | 52.3% 🎯 | ✅ | All 3 systems | **KEEP** - Primary complete |
| `INSTALLATION_COMPLETED_AT` | 587 | 52.3% 🎯 | 🟡 | traffic-lights, milestones | **DUPLICATE** - Timestamp version |
| `READY_FOR_COMMISSION` | 813 | 47.0% | 🟡 | milestoneStatus only | **CONSIDER** - Final install step? |
| `INSTALL_SCHEDULED_DATE_CAPTURE` | 710 | 54.4% 🎯 | ✅ | All 3 systems | **KEEP** - Primary schedule date (HIGHEST!) |
| `ESTIMATED_INSTALL_DATE` | 1124 | 27.5% | 🟡 | traffic-lights only | **KEEP** - Fallback date |
| `INSTALL_SCHEDULED_START_DATE` | 178 | 20.1% | 🟡 | traffic-lights only | **OPTIONAL** - 3rd fallback |
| `INSTALL_DATE_IMPORT` | 835 | 44.6% | ❌ | None | **CLARIFY** - Legacy import? |
| `INSTALL_STARTED_DATE` | 464 | 19.8% ⚠️ | ❌ | None | **CONSIDER** - Track crew arrival |
| `INSTALL_FUNDING_SUBMITTED` | 486 | 18.8% ⚠️ | ❌ | None | **CONSIDER** - M2 milestone? |
| `INSTALL_FUNDING_RECEIVED` | 487 | 13.8% ⚠️ | ❌ | None | **CONSIDER** - M2 funding received? |

**Issues**:
- ⚠️ **TOO MANY DATE FIELDS** - 3 scheduled dates, 2-3 completion dates
- Decision needed: Consolidate to primary + 1 fallback?
- Question: Should M2 funding be tracked as substep?

---

## Milestone 8: Verification (Calculated)

| Field Name | ID | Pop% | Status | Used In | Recommendation |
|------------|-----|------|--------|---------|----------------|
| `INSTALL_COMPLETED_DATE` | 534 | 52.3% 🎯 | 🟡 | milestoneStatus | **KEEP** - Base for calculation |
| `PASSING_INSPECTION_COMPLETED` | 491 | 19.1% ⚠️ | 🟡 | milestoneStatus | **KEEP** - Override marker |

**Issues**:
- 🚨 Verification missing from TrafficLightPipeline
- 🚨 Not checked in milestones.ts
- No dedicated verification field - auto-calculated
- Decision needed: Add to traffic lights or keep Timeline-only?

---

## Milestone 9: Inspection

| Field Name | ID | Pop% | Status | Used In | Recommendation |
|------------|-----|------|--------|---------|----------------|
| `PASSING_INSPECTION_COMPLETED` | 491 | 19.1% ⚠️ | ✅ | All 3 systems | **KEEP** - Complete marker |
| `AS_BUILT_SUBMITTED_TO_AHJ` | 614 | 25.8% 🎯 | 🟡 | milestoneStatus only | **KEEP** - Prerequisite |
| `INSPECTION_SCHEDULED_DATE` | 226 | 9.7% ⚠️ | ❌ | None | **CONSIDER** - Show upcoming |
| `FIRST_INSPECTION_SCHEDULED` | 1589 | 9.7% ⚠️ | ❌ | None | **DUPLICATE?** - Same as above? |

**Issues**:
- ⚠️ Low population (19.1%) for `PASSING_INSPECTION_COMPLETED`
- Consider showing scheduled dates

---

## Milestone 10: PTO

| Field Name | ID | Pop% | Status | Used In | Recommendation |
|------------|-----|------|--------|---------|----------------|
| `PTO_APPROVED` | 538 | 20.5% | 🟡 | milestoneStatus, milestones | **KEEP** - Approval marker |
| `PTO_SUBMITTED` | 537 | 21.1% | 🟡 | milestoneStatus only | **KEEP** - In-progress marker |
| `PTO_UPLOADED_TO_LENDER` | 556 | 19.8% ⚠️ | 🟡 | milestoneStatus only | **KEEP** - M3 celebration! 🎉 |

**Issues**:
- 🚨 **CRITICAL BUG**: PTO completely missing from TrafficLightPipeline!
- 🚨 Last traffic light says "PTO" but shows Inspection state
- Decision needed: Add PTO as separate light or combine with Inspection?

---

## Global Status Fields

| Field Name | ID | Pop% | Status | Used In | Recommendation |
|------------|-----|------|--------|---------|----------------|
| `PROJECT_STATUS` | 255 | 100% 🎯 | ✅ | All systems | **KEEP** - Hold detection, rejection |
| `ON_HOLD` | 231 | 100% 🎯 | 🟡 | Timeline, hold-detection | **KEEP** - Hold boolean |
| `HOLD_REASON` | 232 | 21% | ❌ | None (displayed only) | **KEEP** - Show reason |
| `DATE_ON_HOLD` | 235 | 21% | ❌ | None (displayed only) | **KEEP** - Hold duration |
| `PROJECT_AGE` | 438 | 100% 🎯 | ✅ | All systems | **KEEP** - Age calculations |

---

## Summary Statistics

### By Usage Status

- ✅ **Used in all 3 systems**: 7 fields (12%)
- 🟡 **Used in 1-2 systems**: 28 fields (48%)
- ❌ **Not used anywhere**: 23 fields (40%)

### By Population

- 🎯 **High population (>40%)**: 11 fields
- **Medium population (20-40%)**: 25 fields
- ⚠️ **Low population (<20%)**: 22 fields

### Critical Unused Fields (High Pop, Not Used)

1. `INTAKE_COMPLETE` (100% pop) - URL field for intake
2. `ENGINEERING_COMPLETED` (27% pop) - PE stamp milestone
3. `CAD_DESIGN_SUBMITTED` (27.5% pop) - Design submission tracking
4. `INSTALL_DATE_IMPORT` (44.6% pop) - Legacy? Or still used?

---

## Recommendations Priority

### 🚨 Critical (Fix Immediately)

1. **Fix traffic lights**: Add HOA, Verification, and PTO milestones
2. **Fix PTO mislabel**: Last light says "PTO" but shows Inspection
3. **Decide design complete**: CAD approved vs Design completed
4. **Decide permit complete**: Permit approved vs As-built required
5. **Fix intake date**: `FINANCE_INTAKE_APPROVED_DATE` only 0.3% populated

### ⚠️ Important (Resolve Soon)

6. **Consolidate install dates**: Too many overlapping date fields
7. **Add M2 funding**: Consider tracking install funding as substep
8. **Clarify interconnection**: Is `INTERCONNECTION_SIGNATURES_SENT` NEM or PTO?
9. **Track engineering**: Should `ENGINEERING_COMPLETED` be a substep?

### 💡 Nice to Have (Consider Later)

10. **Add inspection scheduling**: Show `INSPECTION_SCHEDULED_DATE`
11. **Track permit resubmissions**: Use `PERMIT_RESUBMITTED` field
12. **Show estimated dates**: Use `ESTIMATED_PERMIT_RETURN_DATE` for timeline
13. **Track install start**: Use `INSTALL_STARTED_DATE` for crew arrival

---

## Next Steps

1. ✅ Review this audit
2. ✅ Make decisions on critical fields
3. ✅ Update `milestones.json` with corrections
4. → I'll build the milestone engine based on your approved config
