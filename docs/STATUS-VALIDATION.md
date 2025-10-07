# Status Validation Guide

## How Milestone States Are Calculated

The app calculates milestone states based on QuickBase field values. Here's the logic for each milestone:

### 1. Intake
- **Complete**: When Survey Approved (field 165) has a value
- **Overdue**: When project age > 7 days and not complete
- **In Progress**: All other cases

### 2. Survey
- **Complete**: When Survey Approved (field 165) has a value
- **In Progress**: When Survey Submitted (field 164) has a value
- **Pending**: When no survey data exists

### 3. Design
- **Pending**: When Survey Approved is empty
- **Complete**: When CAD Design Approved (field 476) has a value
- **In Progress**: When Design Completed (field 315) has a value
- **Overdue**: When project age > 21 days and not complete
- **In Progress**: All other cases (after survey approved)

### 4. NEM
- **Pending**: When CAD Design Approved is empty
- **Complete**: When NEM Approved (field 327) has a value
- **In Progress**: When NEM Submitted (field 326) has a value OR after CAD approved

### 5. Permit
- **Pending**: When CAD Design Approved OR NEM Approved is empty
- **Complete**: When Permit Approved (field 208) has a value
- **In Progress**: When Permit Submitted (field 207) has a value OR after both dependencies met

### 6. Install
- **Pending**: When NEM Approved OR Permit Approved is empty
- **Complete**: When Install Completed Date (field 534) has a value
- **In Progress**: When install is scheduled (fields 118, 124, 125) OR after dependencies met

### 7. Inspection (PTO)
- **Pending**: When Install Completed Date is empty
- **Complete**: When PTO Approved (field 538) has a value
- **In Progress**: After install complete

### On Hold Override
**All milestones show "on-hold" state** when project status contains any of:
- "On Hold"
- "Finance Hold"
- "Roof Hold"
- "Customer Hold"
- "Permit Hold"
- "HOA Hold"

## Verifying Status Accuracy

### Method 1: Debug Page
1. Log in as Super Admin
2. Navigate to `/debug/status-validation`
3. Compare calculated states with raw field values
4. Check if logic matches actual project progress

### Method 2: QuickBase Comparison
1. Open project in dashboard
2. Click project to view details
3. Compare milestone colors with QuickBase:
   - Green = Complete (field has date value)
   - Orange = In Progress (field has value or dependencies met)
   - Gray = Pending (dependencies not met)
   - Red = On Hold or Overdue

### Method 3: Console Logging
The app logs warnings when:
- Project age > 10,000 days (indicates data issue)
- Milestone states conflict with field values

## Common Issues

### Issue: Milestone shows wrong state
**Check:**
1. Field ID is correct in `lib/constants/fieldIds.ts`
2. QuickBase field actually has data
3. Dependencies are met (previous milestones complete)
4. Project isn't on hold

### Issue: All milestones show "on-hold"
**Check:**
1. Project status field value
2. Contains any hold keywords
3. Should clear when status changes

### Issue: Dates don't match
**Check:**
1. Timezone differences between QB and app
2. Date format parsing in `formatDate()` function
3. Multiple date fields (submitted vs approved)

## Field Mapping Reference

| Milestone | Submitted Field | Approved Field |
|-----------|----------------|----------------|
| Survey | 164 | 165 |
| Design | 315 | 476 |
| NEM | 326 | 327 |
| Permit | 207 | 208 |
| Install | 118/124/125 | 534 |
| PTO | - | 538 |

## Testing Status Logic

```typescript
// Example test case
const project = {
  [165]: { value: '2025-01-01' }, // Survey Approved
  [476]: { value: null },         // CAD Design Approved
  [327]: { value: null },         // NEM Approved
  // ... other fields
}

// Expected results:
// Intake: complete (survey approved)
// Survey: complete (survey approved)
// Design: in-progress (survey done, CAD pending)
// NEM: pending (design not complete)
// Permit: pending (design + NEM not complete)
// Install: pending (permit not complete)
// Inspection: pending (install not complete)
```

## Updating Status Logic

To modify milestone calculation logic:

1. Edit `lib/utils/traffic-lights.ts`
2. Update specific milestone calculator function
3. Test with debug page
4. Verify against real QuickBase data
5. Update this documentation

## Contact

If statuses appear incorrect:
1. Check debug page first
2. Verify QuickBase field IDs
3. Review calculation logic
4. Check for QB data issues
