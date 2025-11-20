# RepCard Data We're NOT Currently Syncing

## Currently Syncing ✅
1. **Users** (minimal) - `/users/minimal`
2. **Offices** - `/offices`
3. **Customers** - `/customers`
4. **Appointments** - `/appointments`
5. **Status Logs** - `/customers/status-logs`
6. **Customer Attachments** - `/customers/attachments`
7. **Appointment Attachments** - `/appointments/attachments`
8. **Customer Notes** - `/customers/notes`
9. **Customer Status Definitions** - `/customers/status`
10. **Calendars** - `/calendar/lists`
11. **Custom Fields** - `/custom-fields/{entityType}`
12. **Leaderboards** - `/leaderboards`
13. **Teams** - (from offices)

## Missing Data We Could Sync 🔍

### 1. **User First Activity Dates** ⭐ HIGH VALUE
- **Endpoint**: `/users/minimal?firstVerifiedDoorKnock=1&firstAppointment=1`
- **What**: First verified door knock date and first appointment date per user
- **Why**: Critical for onboarding metrics and rep performance tracking
- **Impact**: Currently missing from `repcard_users` table
- **Effort**: Low - just add query params to existing sync

### 2. **Aurora Project Links** ⭐ MEDIUM VALUE
- **Endpoint**: `/customers?withRelations=auroraProjectLinks`
- **What**: Links between RepCard customers and Aurora projects
- **Why**: Could help link RepCard data to your Aurora/Quickbase projects
- **Impact**: Would enable cross-platform data correlation
- **Effort**: Medium - need to add relation parsing and new table

### 3. **User Details (Full)** ⭐ LOW VALUE (for now)
- **Endpoint**: `/users/{id}/details`
- **What**: Full user profile with potentially more fields
- **Why**: Might have `officeId` that `/users/minimal` doesn't return
- **Impact**: Could solve our `office_id` NULL issue, but requires 100+ API calls
- **Effort**: High - would need to call individually for each user (slow)

### 4. **Event Users** ⭐ LOW VALUE
- **Endpoint**: `/event-users/{id}`
- **What**: Users registered to specific events
- **Why**: Different from regular users, might have event-specific data
- **Impact**: Unclear if Kin Home uses events
- **Effort**: Medium - need to understand event structure first

### 5. **Calendar Details** ⭐ MEDIUM VALUE
- **Endpoint**: `/calendar/{id}?with=setters,closers,dispatchers`
- **What**: Full calendar with assigned users
- **Why**: We sync calendar lists but not details with assignments
- **Impact**: Could show which users are assigned to which calendars
- **Effort**: Medium - need to call for each calendar

### 6. **Customer Relations** ⭐ LOW VALUE (already have most)
- **Endpoint**: `/customers?withRelations=attachments,user,auroraProjectLinks,notes`
- **What**: We already sync attachments, notes separately
- **Why**: Could reduce API calls by getting relations in one call
- **Impact**: Performance optimization, but adds complexity
- **Effort**: High - would require refactoring sync logic

## Recommendations 🎯

### **Priority 1: Add First Activity Dates** (Quick Win)
```typescript
// In syncUsers function, add query params:
const response = await repcardClient.getUsersMinimal({
  companyId: KIN_HOME_COMPANY_ID,
  page,
  perPage: 100,
  firstVerifiedDoorKnock: 1,  // NEW
  firstAppointment: 1         // NEW
});
```

**Database Migration Needed:**
```sql
ALTER TABLE repcard_users 
ADD COLUMN first_verified_door_knock_date TIMESTAMP,
ADD COLUMN first_appointment_date TIMESTAMP;
```

### **Priority 2: Add Aurora Project Links** (If Needed)
Only if you need to link RepCard customers to Aurora/Quickbase projects. Requires:
- New table: `repcard_aurora_project_links`
- Parse `auroraProjectLinks` relation from customer API response
- Store project IDs and metadata

### **Priority 3: Calendar Details** (If Using Calendars)
Only if calendars are actively used. Would show:
- Which setters/closers/dispatchers are assigned to each calendar
- Calendar-specific settings and configurations

## What We're NOT Missing (Already Covered)
- ✅ Customer attachments (synced separately)
- ✅ Customer notes (synced separately)  
- ✅ Status logs (synced separately)
- ✅ Appointments (synced separately)
- ✅ Custom fields (synced separately)
- ✅ Leaderboards (synced separately)

## Next Steps
1. **Add first activity dates** - Easy win, high value
2. **Test Aurora project links** - Check if Kin Home uses Aurora integration
3. **Consider calendar details** - Only if calendars are actively used
4. **Skip event users** - Unless Kin Home uses RepCard events feature

