# CONTACTS TABLE SYNC - QUICK REFERENCE

**Table ID:** `br9kwm8td` | **App ID:** `br9kwm8bk`

---

## 🎯 ONE-LINER: HOW TO GET SALES REPS

```javascript
// Query all sales reps from QuickBase Contacts table
where: '{37.EX.true}'  // Sales Rep checkbox = true
```

---

## 📋 ESSENTIAL FIELDS FOR SYNC

```typescript
export const CONTACT_FIELDS = {
  // Identity
  RECORD_ID: 3,           // Primary key
  FULL_NAME: 6,
  FIRST_NAME: 14,
  LAST_NAME: 15,
  EMAIL: 17,              // 100% populated
  MOBILE_PHONE: 18,       // 78% populated
  STATUS: 7,              // Active/Inactive

  // DISTINGUISHER (Rep vs Customer)
  SALES_REP: 37,          // ⭐ PRIMARY FILTER - checkbox

  // External IDs
  REPCARD_ID: 235,        // 66% populated - "67198"
  ENERFLO_USER_ID: 168,   // 80% populated - "6861"
  SEQUIFI_USER_ID: 243,   // 58% populated - "KNH0004"

  // RepCard Details (from RepCard system)
  REPCARD_FIRST_NAME: 250,
  REPCARD_LAST_NAME: 251,
  REPCARD_OFFICE_ID: 252,
  REPCARD_OFFICE: 253,
  REPCARD_TEAM_ID: 254,
  REPCARD_TEAM: 255,
  REPCARD_IS_ROOKIE: 256,
  REPCARD_IMAGE: 261,     // Profile photo URL

  // Office/Team (from QB)
  OFFICE_NAME: 113,
  TEAM_NAME: 60,

  // Role
  SETTER: 102,            // Setter checkbox
  NUM_PROJECTS_CLOSER: 114,
  NUM_PROJECTS_SETTER: 278,
};
```

---

## 🚀 COPY-PASTE SYNC QUERY

```javascript
const salesReps = await qbRequest('/v1/records/query', 'POST', {
  from: 'br9kwm8td',
  select: [
    3,   // Record ID
    6,   // Full Name
    14,  // First Name
    15,  // Last Name
    17,  // Email
    18,  // Mobile Phone
    7,   // Status
    235, // RepCard ID
    250, // RepCard First Name
    251, // RepCard Last Name
    252, // RepCard Office ID
    253, // RepCard Office
    254, // RepCard Team ID
    255, // RepCard Team
    256, // Is Rookie
    261, // RepCard Image
    168, // Enerflo User ID
    243, // Sequifi User ID
    113, // Office Name
    60,  // Team Name
    102, // Setter
    114, // # Projects (Closer)
    278, // # Projects (Setter)
  ],
  where: '{37.EX.true}',  // ⭐ Sales Rep = true
  sortBy: [{ fieldId: 15, order: 'ASC' }]  // Sort by Last Name
});

const reps = salesReps.data.data || [];
console.log(`Found ${reps.length} sales reps`);
```

---

## 📊 WHAT TO EXPECT

| Metric | Value | Notes |
|--------|-------|-------|
| **Total Sales Reps** | ~50+ | Filtered by field 37 |
| **With RepCard ID** | 66% | Field 235 |
| **With Enerflo ID** | 80% | Field 168 |
| **With Email** | 100% | Field 17 - always populated |
| **With Phone** | 78% | Field 18 |
| **Active Status** | 60% | Field 7 - rest are blank |

---

## ⚠️ GOTCHAS

### 1. Duplicates Exist
**Problem:** Same person has multiple QB records (e.g., Addison Richards has 3)
**Solution:** Deduplicate by email or RepCard ID after fetching

### 2. RepCard ID Not Always Present
**Problem:** 34% of reps don't have RepCard IDs
**Solution:** Use field 37 (Sales Rep checkbox) as filter, NOT RepCard ID

### 3. Office from Two Sources
**Problem:** Office in field 113 OR field 253 (RepCard)
**Solution:** Prefer RepCard Office (253), fallback to Office Name (113)

### 4. Rookie Name Prefix
**Problem:** Rookie first names have prefix: "❌ R - Alan"
**Solution:** Strip prefix when syncing: `name.replace(/^❌\s*R\s*-\s*/i, '')`

### 5. Status Sometimes Empty
**Problem:** Field 7 (Status) only populated 60% of time
**Solution:** Treat empty as "Active" for sales reps

---

## 🔥 QUICK QUERIES

### Get Only Active Reps
```javascript
where: '{37.EX.true}AND{7.EX."Active"}'
```

### Get Reps with RepCard Only
```javascript
where: '{235.XEX}'  // RepCard ID not empty
```

### Get Reps by Office
```javascript
where: '{37.EX.true}AND{113.EX."Tampa"}'
```

### Get Closers (with projects)
```javascript
where: '{37.EX.true}AND{114.GT.0}'  // Has closer projects
```

### Get Rookies
```javascript
where: '{37.EX.true}AND{256.EX.true}'  // Is Rookie = true
```

---

## 💾 LOCAL DATABASE SCHEMA

```typescript
interface SalesRep {
  id: string;                    // Your local ID
  qbRecordId: number;            // QB Record ID (field 3)
  email: string;                 // Unique identifier
  firstName: string;
  lastName: string;
  fullName: string;
  phone: string | null;
  status: 'Active' | 'Inactive';

  // External IDs
  repcardId: string | null;      // Field 235
  enerfloUserId: string | null;  // Field 168
  sequifiUserId: string | null;  // Field 243

  // Office/Team
  office: string | null;         // Field 113 or 253
  team: string | null;           // Field 60 or 255
  officeId: string | null;       // Field 252
  teamId: string | null;         // Field 254

  // Metadata
  isRookie: boolean;             // Field 256
  isSetter: boolean;             // Field 102
  numProjectsCloser: number;     // Field 114
  numProjectsSetter: number;     // Field 278
  profileImageUrl: string | null; // Field 261

  syncedAt: Date;
}
```

---

## 🔄 DEDUPLICATION LOGIC

```typescript
function deduplicateSalesReps(reps: any[]): any[] {
  const byEmail = new Map<string, any>();

  reps.forEach(rep => {
    const email = rep[17]?.value;
    if (!email) return;

    const existing = byEmail.get(email);
    if (!existing) {
      byEmail.set(email, rep);
    } else {
      // Keep record with highest Record ID (most recent)
      const existingId = existing[3]?.value;
      const currentId = rep[3]?.value;

      if (currentId > existingId) {
        byEmail.set(email, rep);
      }
    }
  });

  return Array.from(byEmail.values());
}
```

---

## 🎨 NAME CLEANING

```typescript
function cleanRepName(firstName: string, lastName: string): {
  firstName: string;
  lastName: string;
  fullName: string;
  isRookie: boolean;
} {
  // Remove rookie prefix from first name
  const cleaned = firstName
    .replace(/^❌\s*R\s*-\s*/i, '')
    .replace(/^R\s*-\s*/i, '')
    .trim();

  const isRookie = firstName.includes('❌') || firstName.match(/^R\s*-/i);

  return {
    firstName: cleaned,
    lastName: lastName.trim(),
    fullName: `${cleaned} ${lastName.trim()}`,
    isRookie: !!isRookie
  };
}
```

---

## 📝 COMPLETE SYNC FUNCTION

```typescript
async function syncSalesRepsFromQB() {
  // 1. Fetch from QuickBase
  const response = await qbRequest('/v1/records/query', 'POST', {
    from: 'br9kwm8td',
    select: [3, 6, 14, 15, 17, 18, 7, 235, 250, 251, 252, 253, 254, 255, 256, 261, 168, 243, 113, 60, 102, 114, 278],
    where: '{37.EX.true}',
    sortBy: [{ fieldId: 15, order: 'ASC' }]
  });

  const records = response.data.data || [];

  // 2. Deduplicate
  const uniqueRecords = deduplicateSalesReps(records);

  // 3. Map and clean
  const salesReps = uniqueRecords.map(record => {
    const firstName = record[14]?.value || record[250]?.value || '';
    const lastName = record[15]?.value || record[251]?.value || '';
    const cleaned = cleanRepName(firstName, lastName);

    return {
      qbRecordId: record[3]?.value,
      email: record[17]?.value,
      firstName: cleaned.firstName,
      lastName: cleaned.lastName,
      fullName: cleaned.fullName,
      phone: record[18]?.value || null,
      status: record[7]?.value || 'Active',
      repcardId: record[235]?.value || null,
      enerfloUserId: record[168]?.value || null,
      sequifiUserId: record[243]?.value || null,
      office: record[253]?.value || record[113]?.value || null,
      team: record[255]?.value || record[60]?.value || null,
      officeId: record[252]?.value || null,
      teamId: record[254]?.value || null,
      isRookie: cleaned.isRookie || record[256]?.value === true,
      isSetter: record[102]?.value === true,
      numProjectsCloser: record[114]?.value || 0,
      numProjectsSetter: record[278]?.value || 0,
      profileImageUrl: record[261]?.value || null,
    };
  });

  // 4. Upsert to local DB
  for (const rep of salesReps) {
    await db.upsertSalesRep({
      where: { email: rep.email },
      create: { ...rep, syncedAt: new Date() },
      update: { ...rep, syncedAt: new Date() }
    });
  }

  console.log(`✅ Synced ${salesReps.length} sales reps`);
  return salesReps;
}
```

---

## ✅ VALIDATION CHECKLIST

After implementing sync:

- [ ] All active sales reps synced (check Status = "Active")
- [ ] No duplicate emails in local database
- [ ] RepCard IDs populated where available
- [ ] Office/Team assignments correct
- [ ] Rookie status correctly identified
- [ ] Name prefixes stripped ("❌ R - " removed)
- [ ] Phone numbers formatted consistently
- [ ] External IDs (Enerflo, Sequifi) stored
- [ ] Sync runs successfully on schedule
- [ ] Error handling for API failures

---

## 🐛 TROUBLESHOOTING

### "No sales reps found"
**Check:** Are you using `{37.EX.true}` not `{37.EX."true"}`?

### "Duplicate reps in local DB"
**Fix:** Run deduplication function on results before inserting

### "Missing office/team data"
**Fix:** Check both sources: field 113/60 AND 253/255 (RepCard)

### "Can't identify closers"
**Fix:** Use field 114 (# Projects Closer) > 0, not a "Closer" checkbox

### "API returns 400"
**Fix:** Ensure all field IDs in select array are valid (test with fewer fields)

---

## 📞 SUPPORT

**QB API Docs:** https://developer.quickbase.com/
**Table ID:** br9kwm8td
**App ID:** br9kwm8bk
**Realm:** kin.quickbase.com

---

*Last Updated: 2025-10-24*
*Based on analysis of 50+ sales rep records*
