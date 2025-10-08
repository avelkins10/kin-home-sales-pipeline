'use client';

import { NotificationBell } from '@/components/notifications/NotificationBell';

export function DashboardHeader() {
  return (
    <div className="flex h-16 items-center justify-between px-4 border-b border-slate-200">
      <h1 className="text-lg font-bold text-slate-900">
        Kin Home Sales
      </h1>
      <NotificationBell />
    </div>
  );
}
