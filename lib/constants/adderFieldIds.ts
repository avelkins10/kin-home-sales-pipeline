// lib/constants/adderFieldIds.ts
// QuickBase field IDs for Adders table (bsaycczmf)

export const ADDER_FIELDS = {
  RECORD_ID: 3,                    // Primary key
  RELATED_PROJECT: 10,             // Link to Projects table
  PRODUCT_NAME: 56,                // Adder name (e.g., "Small System < 4kW")
  TOTAL_COST: 8,                   // Total cost of adder
  QTY: 7,                          // Quantity
  STATUS: 13,                      // Status (e.g., "Pending Review", "Approved")
  WHOS_PAYING: 14,                 // Who's paying? (e.g., "Rep paying", "Customer paying")
  ADDER_NAME_COST: 66,             // Combined field "Adder Name - Cost"
} as const;
