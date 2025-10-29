/**
 * Protected User Fields
 *
 * These fields in the users table are managed by the application and should
 * NEVER be overwritten by external sync scripts (QuickBase, RepCard, etc.).
 *
 * Sync scripts should only update external IDs and metadata fields using
 * COALESCE to fill missing values, never overwriting existing data.
 */

export const PROTECTED_USER_FIELDS = [
  'name',           // User's display name - managed by admins in the app
  'role',           // User's role (closer, setter, office_leader, etc.) - managed by admins
  'sales_office',   // Array of offices for access control - managed by admins via office_assignments
  'password_hash',  // Security - never sync from external sources
  'email',          // Primary identifier - should not be changed after creation
] as const;

export const ADMIN_MANAGED_FIELDS = [
  ...PROTECTED_USER_FIELDS,
  'office_assignments', // Separate table - which offices user can access
] as const;

/**
 * Fields that CAN be synced, but should use COALESCE (only fill if NULL)
 */
export const FILLABLE_USER_FIELDS = [
  'quickbase_contact_id',
  'repcard_user_id',
  'enerflo_user_id',
  'sequifi_user_id',
  'office',              // Single office from RepCard (metadata, different from sales_office array)
  'team',                // RepCard team name
  'phone',
  'profile_image_url',
  'is_rookie',
  'is_setter',
] as const;

/**
 * Metadata fields that can be updated freely (counters, timestamps, etc.)
 */
export const METADATA_USER_FIELDS = [
  'num_closer_projects',
  'num_setter_projects',
  'last_synced_from_contacts_at',
  'last_synced_from_repcard_at',
  'last_synced_at',
  'sync_confidence',
  'updated_at',
] as const;

/**
 * Validates that an UPDATE query doesn't touch protected fields
 *
 * @param updateFields - Array of field names being updated
 * @throws Error if any protected fields are being updated
 */
export function validateSyncUpdate(updateFields: string[]): void {
  const protectedBeingUpdated = updateFields.filter(field =>
    PROTECTED_USER_FIELDS.includes(field as any)
  );

  if (protectedBeingUpdated.length > 0) {
    throw new Error(
      `SYNC VALIDATION ERROR: Attempted to update protected fields: ${protectedBeingUpdated.join(', ')}. ` +
      `These fields are managed by the application and must not be overwritten by sync scripts.`
    );
  }
}

/**
 * Important distinction between office fields:
 *
 * - users.sales_office (TEXT[]): Array of office names for authorization/access control
 *   → Managed by admins in the app UI
 *   → Used for determining which projects/data a user can see
 *   → PROTECTED - never sync from external sources
 *
 * - users.office (TEXT): Single office name from RepCard
 *   → Metadata/display field only
 *   → Can be synced using COALESCE (fill if missing)
 *   → NOT used for authorization
 *
 * - office_assignments table: Links users to offices with access levels
 *   → Managed by admins in the app UI
 *   → PROTECTED - never touched by sync scripts
 */
