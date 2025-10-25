/**
 * PC Notification Preferences Database Operations
 */

import { sql } from '@/lib/db/client';
import { logInfo, logError } from '@/lib/logging/logger';

export interface PCNotificationPreferences {
  id?: number;
  user_id: string;
  email_enabled: boolean;
  email_frequency: 'immediate' | 'daily' | 'weekly';
  notification_types: string[];
  quiet_hours_start?: string;
  quiet_hours_end?: string;
  created_at?: Date;
  updated_at?: Date;
}

/**
 * Get PC notification preferences for a user
 * Returns default preferences if none exist
 */
export async function getPCNotificationPreferences(
  pcEmail: string
): Promise<PCNotificationPreferences> {
  try {
    const result = await sql`
      SELECT * FROM pc_notification_preferences 
      WHERE user_id = ${pcEmail}
      LIMIT 1
    `;

    if (result.length === 0) {
      // Return default preferences
      return {
        user_id: pcEmail,
        email_enabled: false, // Default to disabled to avoid spam
        email_frequency: 'immediate',
        notification_types: [
          'milestone_survey_late',
          'milestone_install_late',
          'milestone_nem_overdue',
          'milestone_pto_overdue',
          'milestone_unresponsive_escalation'
        ]
      };
    }

    const prefs = result[0];
    return {
      id: prefs.id,
      user_id: prefs.user_id,
      email_enabled: prefs.email_enabled,
      email_frequency: prefs.email_frequency,
      notification_types: prefs.notification_types || [],
      quiet_hours_start: prefs.quiet_hours_start,
      quiet_hours_end: prefs.quiet_hours_end,
      created_at: prefs.created_at,
      updated_at: prefs.updated_at
    };
  } catch (error) {
    logError('Failed to get PC notification preferences', error, { pcEmail });
    
    // Return safe defaults on error
    return {
      user_id: pcEmail,
      email_enabled: false,
      email_frequency: 'immediate',
      notification_types: []
    };
  }
}

/**
 * Update PC notification preferences
 */
export async function updatePCNotificationPreferences(
  pcEmail: string,
  preferences: Partial<PCNotificationPreferences>
): Promise<PCNotificationPreferences> {
  try {
    const result = await sql`
      INSERT INTO pc_notification_preferences (
        user_id, 
        email_enabled, 
        email_frequency, 
        notification_types,
        quiet_hours_start,
        quiet_hours_end
      )
      VALUES (
        ${pcEmail},
        ${preferences.email_enabled ?? false},
        ${preferences.email_frequency ?? 'immediate'},
        ${JSON.stringify(preferences.notification_types ?? [])},
        ${preferences.quiet_hours_start ?? '22:00:00'},
        ${preferences.quiet_hours_end ?? '08:00:00'}
      )
      ON CONFLICT (user_id) 
      DO UPDATE SET
        email_enabled = EXCLUDED.email_enabled,
        email_frequency = EXCLUDED.email_frequency,
        notification_types = EXCLUDED.notification_types,
        quiet_hours_start = EXCLUDED.quiet_hours_start,
        quiet_hours_end = EXCLUDED.quiet_hours_end,
        updated_at = NOW()
      RETURNING *
    `;

    const updated = result[0];
    return {
      id: updated.id,
      user_id: updated.user_id,
      email_enabled: updated.email_enabled,
      email_frequency: updated.email_frequency,
      notification_types: updated.notification_types || [],
      quiet_hours_start: updated.quiet_hours_start,
      quiet_hours_end: updated.quiet_hours_end,
      created_at: updated.created_at,
      updated_at: updated.updated_at
    };
  } catch (error) {
    logError('Failed to update PC notification preferences', error, { pcEmail, preferences });
    throw error;
  }
}

/**
 * Check if email should be sent based on preferences and quiet hours
 */
export async function shouldSendEmailNotification(
  pcEmail: string,
  notificationType: string
): Promise<boolean> {
  try {
    const preferences = await getPCNotificationPreferences(pcEmail);
    
    // Check if email is enabled
    if (!preferences.email_enabled) {
      return false;
    }
    
    // Check if notification type is allowed
    if (!preferences.notification_types.includes(notificationType)) {
      return false;
    }
    
    // Check quiet hours if configured
    if (preferences.quiet_hours_start && preferences.quiet_hours_end) {
      const now = new Date();
      const currentTime = now.toTimeString().slice(0, 8); // HH:MM:SS format
      
      // Simple time comparison (assumes quiet hours don't cross midnight)
      if (currentTime >= preferences.quiet_hours_start && 
          currentTime <= preferences.quiet_hours_end) {
        return false;
      }
    }
    
    return true;
  } catch (error) {
    logError('Failed to check email notification preferences', error, { pcEmail, notificationType });
    return false; // Default to not sending on error
  }
}
