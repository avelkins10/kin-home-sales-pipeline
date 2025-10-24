/**
 * QuickBase Contacts Table Field IDs
 *
 * Table ID: br9kwm8td
 *
 * This table contains both sales reps AND customers.
 * Use confidence scoring to distinguish between them.
 *
 * IMPORTANT: Sales Rep checkbox (field 37) is NOT reliably maintained!
 * Use project counts (fields 114/278) as primary indicator instead.
 */

export const CONTACT_FIELDS = {
  // ============================================================
  // CORE IDENTITY
  // ============================================================

  RECORD_ID: 3,                     // QuickBase Record ID - stable, never changes
  FULL_NAME: 6,                     // Full name (display)
  FIRST_NAME: 14,                   // First name
  LAST_NAME: 15,                    // Last name
  EMAIL: 17,                        // Email address
  PHONE: 18,                        // Phone number
  STATUS: 7,                        // Status (Active, Inactive, etc.)

  // ============================================================
  // SALES REP INDICATORS (Checkboxes - UNRELIABLE!)
  // ============================================================

  SALES_REP_CHECKBOX: 37,           // ⚠️ NOT MAINTAINED - Don't use alone!
  SETTER_CHECKBOX: 102,             // Setter checkbox
  CONTACT_TYPE: 54,                 // Contact Type (Sales, Customer, etc.)

  // ============================================================
  // EXTERNAL SYSTEM IDs (PRIMARY INTEGRATION POINTS)
  // ============================================================

  REPCARD_ID: 235,                  // ⭐ RepCard User ID - PRIMARY
  ENERFLO_USER_ID: 168,             // ⭐ Enerflo User ID - PRIMARY
  SEQUIFI_USER_ID: 243,             // Sequifi User ID

  // ============================================================
  // REPCARD EXTENDED FIELDS
  // ============================================================

  REPCARD_FIRST_NAME: 250,          // First name from RepCard
  REPCARD_LAST_NAME: 251,           // Last name from RepCard
  REPCARD_OFFICE_ID: 252,           // RepCard Office ID (numeric)
  REPCARD_OFFICE: 253,              // RepCard Office name
  REPCARD_TEAM_ID: 254,             // RepCard Team ID (numeric)
  REPCARD_TEAM: 255,                // RepCard Team name
  REPCARD_IS_ROOKIE: 256,           // Rookie status from RepCard
  REPCARD_PROFILE_IMAGE: 261,       // Profile image URL from RepCard

  // ============================================================
  // OFFICE/TEAM ASSIGNMENT
  // ============================================================

  OFFICE: 113,                      // Office name (local)
  TEAM: 60,                         // Team name (local)

  // ============================================================
  // PROJECT COUNTS (MOST RELIABLE INDICATOR!)
  // ============================================================

  NUM_CLOSER_PROJECTS: 114,         // ⭐⭐⭐ Total projects as closer - HIGHEST confidence
  NUM_SETTER_PROJECTS: 278,         // ⭐⭐⭐ Total projects as setter - HIGHEST confidence

} as const;

// QuickBase table ID
export const QB_TABLE_CONTACTS = 'br9kwm8td';

// Type helper
export type ContactFieldId = typeof CONTACT_FIELDS[keyof typeof CONTACT_FIELDS];

/**
 * Calculate confidence score for whether a contact is a sales rep
 *
 * Confidence Scoring:
 * - 80+: HIGH - Definitely a sales rep
 * - 50-79: MEDIUM - Very likely a sales rep
 * - 30-49: LOW - Possibly a sales rep (needs verification)
 * - <30: NOT_REP - Likely customer or office staff
 */
export function calculateSalesRepConfidence(contact: Record<number, any>): {
  confidence: number;
  reasons: string[];
  category: 'HIGH' | 'MEDIUM' | 'LOW' | 'NOT_REP';
} {
  let confidence = 0;
  const reasons: string[] = [];

  // STRONG INDICATORS (High confidence)
  const closerProjects = contact[CONTACT_FIELDS.NUM_CLOSER_PROJECTS]?.value || 0;
  const setterProjects = contact[CONTACT_FIELDS.NUM_SETTER_PROJECTS]?.value || 0;
  const totalProjects = closerProjects + setterProjects;

  if (totalProjects > 0) {
    confidence += 50;
    reasons.push(`Has ${totalProjects} projects`);
  }

  if (contact[CONTACT_FIELDS.SALES_REP_CHECKBOX]?.value === true) {
    confidence += 30;
    reasons.push('Sales Rep checkbox = true');
  }

  if (contact[CONTACT_FIELDS.REPCARD_ID]?.value) {
    confidence += 25;
    reasons.push('Has RepCard ID');
  }

  // MODERATE INDICATORS
  if (contact[CONTACT_FIELDS.ENERFLO_USER_ID]?.value) {
    confidence += 15;
    reasons.push('Has Enerflo ID');
  }

  if (contact[CONTACT_FIELDS.SEQUIFI_USER_ID]?.value) {
    confidence += 15;
    reasons.push('Has Sequifi ID');
  }

  if (contact[CONTACT_FIELDS.SETTER_CHECKBOX]?.value === true) {
    confidence += 10;
    reasons.push('Setter checkbox');
  }

  const contactType = contact[CONTACT_FIELDS.CONTACT_TYPE]?.value || '';
  if (contactType.toLowerCase().includes('sales')) {
    confidence += 10;
    reasons.push('Contact Type = Sales');
  }

  const status = contact[CONTACT_FIELDS.STATUS]?.value || '';
  if (status.toLowerCase() === 'active') {
    confidence += 5;
    reasons.push('Status = Active');
  }

  // NEGATIVE INDICATORS (Office staff, not sales)
  const email = contact[CONTACT_FIELDS.EMAIL]?.value || '';
  const hasInternalEmail = email.includes('@kinhome.com') || email.includes('@goiconenergy.com');
  const hasNoSalesIndicators = !contact[CONTACT_FIELDS.REPCARD_ID]?.value && totalProjects === 0;

  if (hasInternalEmail && hasNoSalesIndicators) {
    confidence -= 20;
    reasons.push('Internal email but no projects/RepCard');
  }

  // Determine category
  let category: 'HIGH' | 'MEDIUM' | 'LOW' | 'NOT_REP';
  if (confidence >= 80) category = 'HIGH';
  else if (confidence >= 50) category = 'MEDIUM';
  else if (confidence >= 30) category = 'LOW';
  else category = 'NOT_REP';

  return { confidence, reasons, category };
}
