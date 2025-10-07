# Adders/Add-ons Analysis

## Overview

The system tracks **66 adder-related fields** in the main Projects table and has a dedicated **Adders child table** (bsaycczmf) with **172 fields** for detailed tracking.

---

## ADDER PRODUCTS FOUND

From the sample data, the following adder products were identified:

| Product Name | Typical Cost | Usage |
|--------------|--------------|-------|
| **Perfect Power Box** | $2,250 | Most common |
| **Encharge 10kW** | $12,500 | Battery storage |
| **Insulation** | $5,280 | Electrical upgrade |
| **T2** | $1,250 | Unknown category |

**Note:** Sample data is limited. Production data likely contains many more adder types (electrical upgrades, battery systems, panel/inverter upgrades, roof work, trenching, etc.)

---

## PROJECT-LEVEL ADDER FIELDS

### Financial Tracking (43 fields)

**Count Fields:**
- `[1047]` # of Adders - 100% usage
- `[1046]` # of Adders (Approved) - 100% usage
- `[2272]` # of Contract Adders - 100% usage
- `[2240]` # of Electrical Upgrade Adders - 100% usage
- `[2282]` # of Needs Review Adders - 100% usage
- `[2273]` # of Post Sale Adders - 100% usage
- `[252]` Total # of Adders - 100% usage

**Cost Fields:**
- `[253]` **Adders Total Cost - Customer** - 67.8% usage | Sample: $21,280
- `[254]` **Adders Total Cost - Rep** - 0.7% usage | Sample: $2,000
- `[384]` **Cost - Adders Total** - 94.0% usage | Sample: $16,750
- `[2115]` **Total Adder Cost** - 99.7% usage
- `[128]` **Total Add-On Cost** - 100% usage
- `[129]` **Total (System+Add-on Cost)** - 100% usage

**PPW (Price Per Watt) Fields:**
- `[544]` **Rep Adder PPW** - 100% usage
- `[2114]` **Total Add-on PPW** - 100% usage | Sample: $0.19/watt

**Specialized Cost Fields:**
- `[2458]` Adders Total Cost (excluding Charges)
- `[2274]` Post Sale Adders Total Cost
- `[2457]` Adder Total Cost - Rep Charges
- `[672]` Inverter Adder Amount (formula-based)
- `[675]` Panel Adder Amount (formula-based)

### Approval/Audit Fields (2 fields)

- `[1164]` **Trigger - Adder Audit** - 100% usage (checkbox)
- `[1163]` **Trigger - Adder Audit (Date)** - 3.7% usage

### Description/Display Fields (1+ fields)

- `[654]` xx.Adder Name (formula extracts from Enerflo data)
- `[2286]` **Sales Facing Adder List** - 100% usage
  - Format: "Product Name - Cost, Product Name - Cost"
  - Example: "Encharge 10kW - 12500,Insulation - 5280,Perfect Power Box - 2250,T2 - 1250"
- `[2276]` **Needs-Review Adder List** - 100% usage (multitext)

### Operational Fields

- `[1839]` Add Design Adder (URL to add adder during design)
- `[141]` Add Adder (URL to add new adder)
- `[140]` Adders (dblink to child table) - 0% usage in sample
- `[353]` Adder Review (dblink)

---

## ADDERS CHILD TABLE (bsaycczmf)

### Structure: 172 fields organized into:

**Core (3 fields):**
- Record ID, Date Created, Date Modified

**Project Links (46 fields):**
- `[10]` Related Project (numeric ID)
- `[30]` Project ID #
- `[11]` Client - Project (text)
- Plus 40+ project lookup fields:
  - Project Status, Sales Date, Install Date
  - Closer, Setter, Project Coordinator
  - System Size, PPW values
  - Permit dates, Design dates
  - Utility, AHJ info

**Name/Description (8 fields):**
- `[31]` Customer Name
- `[56]` **Product Name** (the actual adder product)
- `[26]` Team Name
- `[66]` Adder Name - Cost
- `[35]` EPC Name
- `[55]` Product Name Text
- `[67]` Related Adder - Product Name

**Pricing (14 fields):**
- `[8]` **Total Cost** (primary cost field)
- `[37]` Quoted Amount
- `[36]` Invoiced Amount
- `[63]` Manual Adjustment: PPW
- `[64]` Manual Adjustment: Input Cost
- `[117]` Calculated Cost
- `[109]` Adder Price
- `[9]` Adder Total
- `[116]` Kin Fulfillment Cost
- `[188]` Total Amount Invoiced
- `[200-203]` Charge Amount fields

**Approval/Status (11 fields):**
- `[13]` **Status** (Active, Complete, Cancel)
- `[20]` **Adder Status** (Approved, Pending Review)
- `[23]` **Operations Approval Status** (Approved)
- `[19]` Reject Adder (URL)
- `[22]` Approval Date - Capture
- `[127]` CX Confirmed Date
- `[147]` Confirmed Adder List
- `[148]` Design Confirmed? (checkbox)
- `[159]` Adder Review Status (2025)
- `[161]` Adder Review: Manual Design Confirmation
- `[175]` Adder Review: Change Order Confirmed

**Category/Type (3 fields):**
- `[17]` **Product Category** (currently all "Adder")
- `[28]` Item - Type
- `[138]` Adder Source: Contract Adder? (checkbox)

**Sales/Design (7 fields):**
- `[60]` Sales Aid User
- `[125]` Design Adder Callout Note
- `[142]` Adder Review: Sales Notified Date
- `[152]` Adder Review: Sales Notified? (checkbox)
- `[163]` Related Adder - Require Design Review? (checkbox)
- `[166]` Best Related Design Completed Date
- `[225]` Sales SMS Copy

**Operational (4 fields):**
- `[48]` sale_to_install_biz_days
- `[54]` sale_to_install_cal_days
- `[149]` Needs Operations Review? (checkbox)
- `[174]` Adder Review: Required for Install?

**Dates (14 fields):**
- `[32]` Scheduled Start Date
- `[33]` Scheduled End Date
- `[34]` Completed Date
- `[47]` date_scheduled
- `[126]` Shutdown Date
- `[128]` Inspection Date
- `[140]` Ops Adder Plan Complete Date
- `[99]` Service Completed Date
- `[100]` Service Scheduled Date/Time
- `[164]` Adder Review: Ops Review Complete
- `[167]` Rep Notification SLA Start Date
- `[177]` Adder Review: Rep Charge Decision Date
- `[209]` Rep Notified Date - Call-out
- `[218]` Skip Rep Notification Date

---

## WORKFLOW & APPROVAL PROCESS

### From Sample Data Analysis (200 adder records):

**Status Distribution:**
- **Active:** 78 (39%)
- **Cancel:** 72 (36%)
- **Complete:** 50 (25%)

**Adder Status:**
- **Pending Review:** 133 (67%)
- **Approved:** 67 (33%)

**Operations Approval:**
- **Approved:** 67 records
- Only approved adders get ops approval status

### Approval Flow (Inferred):

1. **Sales Stage:**
   - Adder added during or after sale
   - Appears in "Sales Facing Adder List" on project
   - Status: "Pending Review"

2. **Review Stage:**
   - Design team reviews if `Require Design Review = true`
   - Operations reviews if `Needs Operations Review = true`
   - Sales notified of changes
   - Status changes to "Approved" or gets cancelled

3. **Operations Stage:**
   - `Operations Approval Status = Approved`
   - Ops Adder Plan Complete Date set
   - Required for Install flag determines timing

4. **Completion:**
   - Service scheduled and completed
   - Status: "Complete"
   - Invoiced

### Key Checkpoints:

**Sales → Design:**
- `[152]` Adder Review: Sales Notified?
- `[142]` Adder Review: Sales Notified Date
- `[148]` Design Confirmed?
- `[161]` Adder Review: Manual Design Confirmation

**Design → Operations:**
- `[149]` Needs Operations Review?
- `[23]` Operations Approval Status
- `[164]` Adder Review: Ops Review Complete
- `[174]` Adder Review: Required for Install?

**Operations → Install:**
- `[140]` Ops Adder Plan Complete Date
- `[100]` Service Scheduled Date/Time
- `[99]` Service Completed Date

---

## ADDER TYPES (Inferred from Field Names)

### 1. **Contract Adders**
- Added during initial sale
- Field: `[138]` Adder Source: Contract Adder?
- Counted in: `[2272]` # of Contract Adders

### 2. **Post-Sale Adders**
- Added after contract signing
- Counted in: `[2273]` # of Post Sale Adders
- Total cost in: `[2274]` Post Sale Adders Total Cost

### 3. **Electrical Upgrade Adders**
- MPU (Main Panel Upgrade)
- Trenching
- Conduit work
- Counted in: `[2240]` # of Electrical Upgrade Adders

### 4. **Design Adders**
- Added during design phase
- URL: `[1839]` Add Design Adder
- Require design review

### 5. **Needs Review Adders**
- Require approval before proceeding
- Listed in: `[2276]` Needs-Review Adder List
- Counted in: `[2282]` # of Needs Review Adders

---

## KEY INSIGHTS

### High-Level Summary:

1. **67.8% of projects** have customer-paid adders
2. **Average adder cost** varies widely ($1,250 - $12,500+)
3. **67% of adders** are pending review (high backlog)
4. **36% cancellation rate** for adders (approval/feasibility issues)

### Workflow Insights:

1. **Multi-Stage Approval:**
   - Sales review
   - Design confirmation (for technical adders)
   - Operations approval (for install requirements)

2. **Rep Communication:**
   - Sales reps notified when adders need review
   - SMS notifications available
   - SLA tracking for rep response

3. **Cost Tracking:**
   - Customer cost vs Rep cost (commissionable)
   - PPW impact calculated
   - Charge amounts can be overridden

4. **Integration Points:**
   - Enerflo integration (field `[568]` adders enerflo)
   - Invoice generation (field `[187]` Invoice records)
   - Service scheduling (field `[96]` Service Records)

### Potential Issues:

1. **High Pending Review Rate (67%):** Bottleneck in approval process
2. **High Cancellation Rate (36%):** Many adders don't make it through
3. **Low Rep Cost Usage (0.7%):** Most adders are customer-paid only
4. **Limited Product Variety in Sample:** May indicate data quality or limited scope

---

## RECOMMENDATIONS

### For Application Development:

1. **Adder Management Screen:**
   - Show pending adders requiring review
   - Filter by: Sales/Design/Ops approval stage
   - Quick approve/reject actions
   - Notification system for sales reps

2. **Project View:**
   - Display `Sales Facing Adder List` prominently
   - Show counts: Total, Approved, Pending, Cancelled
   - Calculate total adder cost impact on PPW

3. **Design Workflow:**
   - Flag adders requiring design review
   - Track design confirmation dates
   - Notify sales of design changes

4. **Operations Dashboard:**
   - Adders requiring ops approval
   - Install-required adders by scheduled date
   - Service scheduling for adder work

5. **Reporting:**
   - Adder approval velocity (time to approve)
   - Cancellation reasons
   - Most common adder types
   - Revenue impact (adder cost vs base system)

---

## FIELD MAPPING FOR APPLICATION

### Essential Fields:

**Project Level:**
```javascript
{
  totalAdders: 252,              // Total # of Adders
  totalAdderCost: 2115,          // Total Adder Cost
  addersCustomerCost: 253,       // Adders Total Cost - Customer
  addersPPW: 2114,               // Total Add-on PPW
  salesFacingList: 2286,         // Sales Facing Adder List
  needsReviewList: 2276,         // Needs-Review Adder List
  adderAuditTrigger: 1164,       // Trigger - Adder Audit
  numApproved: 1046,             // # of Adders (Approved)
  numPostSale: 2273,             // # of Post Sale Adders
  numNeedsReview: 2282           // # of Needs Review Adders
}
```

**Adder Record:**
```javascript
{
  id: 3,                         // Record ID#
  projectId: 10,                 // Related Project
  productName: 56,               // Product Name
  totalCost: 8,                  // Total Cost
  status: 13,                    // Status (Active/Complete/Cancel)
  adderStatus: 20,               // Adder Status (Approved/Pending)
  opsApprovalStatus: 23,         // Operations Approval Status
  productCategory: 17,           // Product Category
  designConfirmed: 148,          // Design Confirmed?
  needsOpsReview: 149,           // Needs Operations Review?
  requiredForInstall: 174,       // Required for Install?
  salesNotified: 152,            // Sales Notified?
  approvalDate: 22               // Approval Date - Capture
}
```
