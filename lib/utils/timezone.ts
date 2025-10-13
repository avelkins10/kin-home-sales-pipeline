// lib/utils/timezone.ts
'use client';

/**
 * Detect the user's timezone using the browser's Intl API
 */
export function detectUserTimezone(): string {
  try {
    const timezone = Intl.DateTimeFormat().resolvedOptions().timeZone;
    return timezone || 'America/New_York';
  } catch (error) {
    console.error('Failed to detect timezone:', error);
    return 'America/New_York';
  }
}

/**
 * Update the user's timezone on the server
 */
export async function updateUserTimezone(timezone: string): Promise<boolean> {
  try {
    const response = await fetch('/api/user/timezone', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify({ timezone }),
      credentials: 'include',
    });

    if (!response.ok) {
      console.error('Failed to update timezone:', await response.text());
      return false;
    }

    return true;
  } catch (error) {
    console.error('Failed to update timezone:', error);
    return false;
  }
}

/**
 * Check if the user's stored timezone matches their browser timezone
 * If not, update it automatically
 */
export async function syncUserTimezone(storedTimezone?: string): Promise<void> {
  const detectedTimezone = detectUserTimezone();

  // Only update if different from stored value
  if (storedTimezone && storedTimezone === detectedTimezone) {
    return;
  }

  // Update timezone on server
  await updateUserTimezone(detectedTimezone);
}
