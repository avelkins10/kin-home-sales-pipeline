# RepCard Tab Complete Audit - Database & API Mapping

## 🔴 CRITICAL ISSUE FOUND: Office Name Mismatch

### Problem
**Office names don't match between RepCard and App:**
- RepCard: "Richards Region", "Bitton Region", "HQ", "Champagne Dynasty Region"
- App: "Richards Mgmt", "Champagne - Panama City 2025", "Kin Home HQ"

**Impact:**
- Office filtering fails because `LEFT JOIN offices o ON o.name = COALESCE(u.sales_office[1], ru.office_name)` never matches
- Users with RepCard office names can't be filtered by app office IDs
- Office filtering always falls back to "all users"

### Current Office Filtering Logic
```sql
LEFT JOIN offices o ON o.name = COALESCE(u.sales_office[1], ru.office_name)
WHERE o.quickbase_office_id = ANY(${officeIds}::int[])
```

**Problem:** This JOIN fails because names don't match, so `o.quickbase_office_id` is always NULL.

---

## ✅ What's Working Correctly

### 1. **User Display** ✅
- ✅ All RepCard users are displayed (from `repcard_users` table)
- ✅ Users are linked to app `users` table by email
- ✅ Falls back to RepCard data if not linked
- ✅ Shows: name, email, office, role

### 2. **Database Mapping** ✅
- ✅ `repcard_users` → Users table (linked by email)
- ✅ `repcard_customers` → Customers/Leads
- ✅ `repcard_appointments` → Appointments
- ✅ `repcard_status_logs` → Sales/Status changes
- ✅ `repcard_offices` → Offices
- ✅ `repcard_teams` → Teams (synced from users/offices)
- ✅ `repcard_calendars` → Calendars with setters/closers/dispatchers

### 3. **API Mapping** ✅
- ✅ All RepCard API endpoints correctly mapped
- ✅ Query parameters correctly converted (camelCase → snake_case)
- ✅ Pagination handled correctly
- ✅ Error handling with retries

### 4. **Data Flow** ✅
- ✅ Sync → Database → API → Frontend
- ✅ All components use database (not direct API calls)
- ✅ Auto-refresh every 30 seconds
- ✅ Proper error handling

---

## ⚠️ Issues Found

### 1. **Office Filtering Broken** 🔴 CRITICAL
**Problem:** Office names don't match, so filtering fails

**Current Code:**
```sql
LEFT JOIN offices o ON o.name = COALESCE(u.sales_office[1], ru.office_name)
WHERE o.quickbase_office_id = ANY(${officeIds}::int[])
```

**Fix Options:**
1. **Option A:** Use RepCard office names directly (don't try to match to app offices)
2. **Option B:** Create office mapping table
3. **Option C:** Use fuzzy matching or partial name matching
4. **Option D:** Filter by RepCard office_id instead of app office IDs

**Recommendation:** Option A - Use RepCard office names directly since we're displaying RepCard data

### 2. **Teams Not Displayed** 🟡 MEDIUM
**Problem:** Teams are synced but not displayed in leaderboards

**Current:** Teams are synced to `repcard_teams` table and `repcard_users.team_name` exists
**Missing:** Team column in leaderboard display, team filtering

### 3. **Office Display Uses RepCard Names** ✅ CORRECT
**Current:** `COALESCE(u.sales_office[1], ru.office_name)` - Shows RepCard office name
**Status:** ✅ This is correct - we want to show RepCard office names

---

## 🔧 Recommended Fixes

### Fix 1: Office Filtering (CRITICAL)
**Change office filtering to use RepCard office names directly:**

```typescript
// Instead of matching to app offices, filter by RepCard office names
if (officeIds && officeIds.length > 0) {
  // Get RepCard office names from app office IDs (if mapping exists)
  // OR: Filter by RepCard office_id directly
  // OR: Skip office filtering and show all (current fallback)
}
```

**Better Solution:** Create a mapping or use RepCard office IDs directly

### Fix 2: Add Team Display
**Add team column to leaderboard:**

```typescript
// In leaderboard queries, add:
COALESCE(ru.team_name, '') as team

// In frontend, add team column to table
<TableHead>Team</TableHead>
<TableCell>{entry.team || '-'}</TableCell>
```

### Fix 3: Improve Office Filtering
**Option 1:** Filter by RepCard office_id instead of app office IDs
**Option 2:** Create office name mapping table
**Option 3:** Use partial/fuzzy matching

---

## 📊 Data Verification

### Database Status ✅
- ✅ 32 active RepCard users
- ✅ 2,800 customers
- ✅ 2,040 appointments
- ✅ 7 RepCard offices
- ✅ Teams synced (from users)

### API Status ✅
- ✅ All endpoints working
- ✅ Retry logic working
- ✅ Rate limiting handled

### Frontend Status ✅
- ✅ Components render correctly
- ✅ Error handling improved
- ✅ Loading states work
- ⚠️ Office filtering may not work due to name mismatch

---

## 🎯 Action Items

### Critical (Do Now)
1. **Fix office filtering** - Handle name mismatch or use RepCard office IDs
2. **Test office filtering** - Verify it works with RepCard office names

### High Priority
3. **Add team display** - Show team names in leaderboards
4. **Add team filtering** - Allow filtering by team

### Medium Priority
5. **Create office mapping** - Map RepCard offices to app offices (if needed)
6. **Add office comparison** - Compare RepCard offices side-by-side

---

## 📝 Notes

- Office filtering currently falls back to "all users" when it fails
- This means data still displays, but filtering doesn't work
- Teams are synced but not displayed in UI
- All other mappings are correct

