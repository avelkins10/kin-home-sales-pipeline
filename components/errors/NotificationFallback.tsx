'use client';

import { Bell } from 'lucide-react';

/**
 * Fallback UI when notification system fails
 * Shows a disabled bell icon to indicate notifications are unavailable
 */
export function NotificationFallback() {
  return (
    <button
      className="relative p-2 rounded-lg text-slate-400 cursor-not-allowed opacity-50"
      disabled
      title="Notifications temporarily unavailable"
      aria-label="Notifications unavailable"
    >
      <Bell className="h-5 w-5" />
    </button>
  );
}
