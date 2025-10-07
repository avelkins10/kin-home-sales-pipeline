# Quickbase User ID Mapping

## Problem Statement
The application filters projects by user using Quickbase field 516 (CLOSER_ID). The seed script contained placeholder IDs that didn't match actual Quickbase data, causing projects to not load for users.

## Investigation Process

### Step 1: Discover Actual IDs
Ran `sync-closers-from-quickbase.js` to fetch all unique closer IDs from Quickbase:

```bash
node scripts/sync-closers-from-quickbase.js
```

**Output:** Found 433 unique closers in Quickbase. Key findings for Addison Richards:

```json
[
  { "closerId": 118, "closerName": "Addison Richards", "closerEmail": "addison.r@goiconenergy.com" },
  { "closerId": 129, "closerName": "Addison Richards", "closerEmail": "addison.r@kinhome.com" },
  { "closerId": 186, "closerName": "Addison Richards", "closerEmail": "addison.r@kinhome.com" },
  { "closerId": 326, "closerName": "Addison Richards", "closerEmail": "addison.r@kinhome.com" },
  { "closerId": 452, "closerName": "Addison Richards", "closerEmail": "addison.r@kinhome.com" },
  { "closerId": 508, "closerName": "Addison Richards", "closerEmail": "addison.r@kinhome.com" },
  { "closerId": 650, "closerName": "Addison Richards", "closerEmail": "addison.r@kinhome.com" },
  { "closerId": 711, "closerName": "Addison Richards", "closerEmail": "addison.r@kinhome.com" },
  { "closerId": 870, "closerName": "Addison Richards", "closerEmail": "addison.r@kinhome.com" },
  { "closerId": 882, "closerName": "Addison Richards", "closerEmail": "addison.r@kinhome.com" },
  { "closerId": 910, "closerName": "Addison Richards", "closerEmail": "addison.r@kinhome.com" }
]
```

### Step 2: Map IDs to Users
Created mapping between Quickbase user reference IDs and application user emails:

| Email | Name | Quickbase User ID(s) | Notes |
|-------|------|---------------------|-------|
| addison.r@kinhome.com | Addison Richards | 118,129,186,326,452,508,650,711,870,882,910 | Multiple IDs due to account changes |
| closer@kinhome.com | Test Closer | N/A | Test user only |
| setter@kinhome.com | Test Setter | N/A | Test user only |

### Step 3: Verify IDs Work
Tested fetching projects with discovered IDs:

```bash
node scripts/fetch-addison-projects.js
```

**Result:** Found 600 projects for Addison Richards, confirming the IDs are correct.

## Resolution

### Updated Files
1. **seed-users.js** - Updated `quickbase_user_id` values with real IDs
2. **fetch-addison-projects.js** - Updated test query with real IDs
3. **update-addison-user.js** - Updated SQL with real ID

### Re-seeding Database
```bash
npm run setup:seed
```

## Understanding Quickbase User Fields

Quickbase User fields (like CLOSER_ID field 516) store user references as:
- **Type:** Text field that references a Quickbase user
- **Values:** Can be email, username, or hashed user ID
- **Querying:** Use `.EX.` operator with the stored reference value

**Key Insight:** The "user ID" stored in field 516 is NOT the same as the Quickbase account user ID. It's a reference value specific to how that field stores user data.

## Testing

After updating IDs and re-seeding:

1. **Login** as addison.r@kinhome.com
2. **Navigate** to Projects page
3. **Verify** projects load correctly
4. **Check** that filtering by view works
5. **Test** project detail pages load

## Future Maintenance

If new users are added:
1. Run `sync-closers-from-quickbase.js` to discover their Quickbase ID
2. Update `seed-users.js` with the correct ID
3. Re-run seeding or use a similar update script

## References
- Quickbase API: https://developer.quickbase.com/
- Field 516 (CLOSER_ID) definition in `lib/constants/fieldIds.ts`
- Query logic in `lib/quickbase/queries.ts` (lines 9-28)
