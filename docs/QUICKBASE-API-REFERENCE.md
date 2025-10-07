# Quickbase API Reference - Quick Guide

## Authentication

```javascript
const headers = {
  'QB-Realm-Hostname': 'kin.quickbase.com',
  'Authorization': 'QB-USER-TOKEN {your-user-token}',
  'Content-Type': 'application/json'
};
```

---

## Common API Calls

### 1. Get Projects for Current User (Closer)

```javascript
POST https://api.quickbase.com/v1/records/query

{
  "from": "br9kwm8na",
  "select": [
    3,    // Record ID#
    11,   // Project ID
    145,  // Customer Name
    146,  // Customer Address
    255,  // Project Status
    300,  // Project Priority
    522,  // Sales Date
    438,  // Project Age
    534,  // Install Completed Date
    231,  // On Hold?
    232,  // Hold Reason
    2286, // Sales Facing Adder List
    1046  // # of Adders (Approved)
  ],
  "where": "{516.EX.'USER_ID_HERE'}",  // closerId equals current user
  "sortBy": [
    { "fieldId": 522, "order": "DESC" }  // Sort by Sales Date, newest first
  ],
  "options": {
    "top": 100
  }
}
```

### 2. Get Single Project Details

```javascript
POST https://api.quickbase.com/v1/records/query

{
  "from": "br9kwm8na",
  "select": [
    // Core
    3, 11, 145, 146, 148, 255, 300,

    // Team
    516, 517, 329, 330, 819, 820,

    // Financials
    13, 133, 19, 543, 2480,

    // Milestone Dates (most reliable)
    522,  // Sales Date
    164,  // Survey Submitted
    165,  // Survey Approved
    315,  // Design Completed
    476,  // CAD Design Approved
    207,  // Permit Submitted
    208,  // Permit Approved
    326,  // NEM Submitted
    327,  // NEM Approved
    710,  // Install Scheduled Date Capture
    534,  // Install Completed Date
    537,  // PTO Submitted
    538,  // PTO Approved

    // Holds/Blockers
    231, 232, 233, 234, 235, 1389,

    // Adders
    252, 2115, 2286, 1046, 2282,

    // Status Bar
    301
  ],
  "where": "{3.EX.PROJECT_RECORD_ID}",  // Record ID# equals specific project
  "options": { "top": 1 }
}
```

### 3. Get Adders for a Project

```javascript
POST https://api.quickbase.com/v1/records/query

{
  "from": "bsaycczmf",  // Adders table
  "select": [
    3,    // Record ID#
    10,   // Related Project
    31,   // Customer Name
    56,   // Product Name
    8,    // Total Cost
    37,   // Quoted Amount
    13,   // Status (Active/Complete/Cancel)
    20,   // Adder Status (Approved/Pending Review)
    23,   // Operations Approval Status
    17,   // Product Category
    148,  // Design Confirmed?
    149,  // Needs Operations Review?
    152,  // Sales Notified?
    174,  // Required for Install?
    22    // Approval Date - Capture
  ],
  "where": "{10.EX.PROJECT_ID}",  // Related Project equals project ID
  "sortBy": [
    { "fieldId": 1, "order": "DESC" }  // Sort by Date Created
  ],
  "options": { "top": 50 }
}
```

### 4. Get Projects On Hold

```javascript
POST https://api.quickbase.com/v1/records/query

{
  "from": "br9kwm8na",
  "select": [3, 11, 145, 255, 231, 232, 233, 234, 235, 1389],
  "where": "{231.EX.true}",  // On Hold? equals true
  "sortBy": [
    { "fieldId": 235, "order": "DESC" }  // Sort by Date Placed On Hold
  ],
  "options": { "top": 100 }
}
```

### 5. Get Projects by Status

```javascript
POST https://api.quickbase.com/v1/records/query

{
  "from": "br9kwm8na",
  "select": [3, 11, 145, 255, 300, 522, 438],
  "where": "{255.EX.'Active'}",  // Project Status equals 'Active'
  "options": { "top": 100 }
}
```

**Common Status Values:**
- `Active`
- `Active - On Hold`
- `Completed`
- `Cancelled`
- `Active - PTO`
- `Active - Installed`

### 6. Get Projects with SLA Deadline Coming Up

```javascript
POST https://api.quickbase.com/v1/records/query

{
  "from": "br9kwm8na",
  "select": [3, 11, 145, 255, 2459, 2460, 2461],  // Includes Design SLA fields
  "where": "{2459.BF.TODAY}",  // Design SLA Deadline before today (overdue)
  "sortBy": [
    { "fieldId": 2459, "order": "ASC" }  // Soonest deadline first
  ],
  "options": { "top": 50 }
}
```

---

## Query Operators

| Operator | Meaning | Example |
|----------|---------|---------|
| `EX` | Equals | `{255.EX.'Active'}` |
| `CT` | Contains | `{255.CT.'Hold'}` |
| `BF` | Before | `{522.BF.'2025-01-01'}` |
| `AF` | After | `{522.AF.'2025-01-01'}` |
| `XEX` | Does not equal | `{255.XEX.'Cancelled'}` |
| `TV` | Has value | `{534.TV.}` |
| `XTV` | Is empty | `{534.XTV.}` |

### Combining Conditions (AND)

```javascript
"where": "{516.EX.'123456'}AND{255.EX.'Active'}"
```

### Combining Conditions (OR)

```javascript
"where": "{255.EX.'Active'}OR{255.EX.'Active - On Hold'}"
```

---

## Response Format

```javascript
{
  "data": [
    {
      "3": { "value": 61 },           // Record ID#
      "145": { "value": "John Doe" }, // Customer Name
      "255": { "value": "Active" },   // Project Status
      "522": { "value": "2025-01-15" } // Sales Date
    }
  ],
  "fields": [
    { "id": 3, "label": "Record ID#", "type": "recordid" },
    { "id": 145, "label": "Customer Name", "type": "text" }
  ],
  "metadata": {
    "numFields": 4,
    "numRecords": 1,
    "skip": 0,
    "top": 100,
    "totalRecords": 1
  }
}
```

---

## Field Type Handling

### User Fields (closerId, setterId, etc.)

```javascript
// Response format for user fields:
{
  "516": {
    "value": {
      "email": "rep@example.com",
      "id": "123456",
      "name": "John Rep",
      "userName": "jrep"
    }
  }
}

// To query by user ID:
"where": "{516.EX.'123456'}"

// To filter by current user (in Quickbase interface):
"where": "{516.EX.CurrentUser}"
```

### Date Fields

```javascript
// Response format:
{
  "522": { "value": "2025-01-15" }  // YYYY-MM-DD format
}

// Query today's date:
"where": "{522.EX.TODAY}"

// Query date range:
"where": "{522.BF.'2025-12-31'}AND{522.AF.'2025-01-01'}"
```

### Currency Fields

```javascript
// Response format:
{
  "133": { "value": 50000 }  // Returns as number (cents or dollars)
}
```

### HTML/Rich Text Fields (Status Bar)

```javascript
// Response format:
{
  "301": { "value": "<table>...</table>" }  // Full HTML string
}
```

---

## Error Handling

```javascript
// Common errors:

// 401 Unauthorized
{
  "message": "Invalid or expired token"
}

// 400 Bad Request
{
  "message": "Invalid query",
  "description": "Field ID 9999 does not exist"
}

// 403 Forbidden
{
  "message": "User does not have access to this table"
}
```

---

## Rate Limits

- **Standard:** 10 requests per second per user
- **Burst:** Up to 100 requests in quick succession
- **Daily:** No published limit, but excessive use may be throttled

**Best Practice:** Batch queries when possible, use pagination for large datasets

---

## Pagination

```javascript
// First page:
{
  "options": {
    "skip": 0,
    "top": 100
  }
}

// Second page:
{
  "options": {
    "skip": 100,
    "top": 100
  }
}

// Check metadata.totalRecords to know if more pages exist
```

---

## Special Field Values

### Checkbox Fields
- `true` or `false` (boolean)

### Multitext Fields (like Sales Facing Adder List)
- Returns as string with semicolons or commas as separators
- Example: `"Perfect Power Box - 2250,Encharge 10kW - 12500"`

### DBLink Fields (relationships)
- Returns count of related records
- To get actual related records, query the child table with `where` clause

---

## Performance Tips

1. **Select only needed fields** - Don't use `"select": []` (all fields)
2. **Use specific where clauses** - Indexed fields are faster (Record ID#, user fields)
3. **Limit results** - Use `"top"` parameter appropriately
4. **Cache frequently-accessed data** - Field definitions don't change often
5. **Batch requests** - Get multiple projects in one query vs separate queries

---

## Example: Complete Dashboard Query

```javascript
// Single query to get everything for dashboard
POST https://api.quickbase.com/v1/records/query

{
  "from": "br9kwm8na",
  "select": [
    // Identity
    3, 11, 145,

    // Status & Priority
    255, 300,

    // Key Dates
    522, 534, 438,

    // Holds
    231, 232,

    // Adders Summary
    252, 2286, 1046,

    // Team
    516, 517, 329, 330
  ],
  "where": "{516.EX.'USER_ID'}AND{255.XEX.'Cancelled'}",  // My active projects
  "sortBy": [
    { "fieldId": 231, "order": "DESC" },  // On Hold first
    { "fieldId": 300, "order": "ASC" },   // Then by priority
    { "fieldId": 522, "order": "DESC" }   // Then by sales date
  ],
  "options": {
    "top": 50
  }
}
```

---

## Useful Calculated Fields

Some fields are formulas that calculate automatically:

- `[438]` Project Age - Days since Date Created
- `[2114]` Total Add-on PPW - Calculated from adder cost / system watts
- `[301]` Project Status - Color - HTML visualization
- `[2459]` Design SLA Deadline - Calculated deadline based on rules

**Note:** These update automatically, don't need manual calculation in your app.

---

## Next Steps for Development

1. **Set up authentication** - Get user token or implement OAuth
2. **Test basic query** - Start with simple project list
3. **Handle user filtering** - Detect current user's role (closer/setter)
4. **Build data layer** - Abstract Quickbase API calls
5. **Add error handling** - Handle auth, network, and data errors
6. **Implement caching** - Reduce API calls for better performance
