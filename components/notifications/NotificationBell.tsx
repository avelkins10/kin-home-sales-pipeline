'use client';

import { useState, useRef, useEffect } from 'react';
import { Bell } from 'lucide-react';
import { useUnreadCounts, useCriticalUnreadCount } from '@/lib/hooks/useNotifications';
import { UnreadBadge } from '@/components/ui/UnreadBadge';
import { NotificationCenter } from './NotificationCenter';

export function NotificationBell() {
  const [isOpen, setIsOpen] = useState(false);
  const dropdownRef = useRef<HTMLDivElement>(null);
  const buttonRef = useRef<HTMLButtonElement>(null);

  const { data: unreadCounts } = useUnreadCounts();
  const criticalCount = useCriticalUnreadCount();

  const totalUnread = unreadCounts?.total || 0;
  const hasCritical = criticalCount > 0;

  // Close dropdown when clicking outside
  useEffect(() => {
    function handleClickOutside(event: MouseEvent) {
      if (
        dropdownRef.current &&
        buttonRef.current &&
        !dropdownRef.current.contains(event.target as Node) &&
        !buttonRef.current.contains(event.target as Node)
      ) {
        setIsOpen(false);
      }
    }

    if (isOpen) {
      document.addEventListener('mousedown', handleClickOutside);
      return () => {
        document.removeEventListener('mousedown', handleClickOutside);
      };
    }
  }, [isOpen]);

  // Close on Escape key
  useEffect(() => {
    function handleEscape(event: KeyboardEvent) {
      if (event.key === 'Escape' && isOpen) {
        setIsOpen(false);
      }
    }

    document.addEventListener('keydown', handleEscape);
    return () => {
      document.removeEventListener('keydown', handleEscape);
    };
  }, [isOpen]);

  return (
    <div className="relative">
      {/* Bell Button */}
      <button
        ref={buttonRef}
        onClick={() => setIsOpen(!isOpen)}
        className="relative p-2 rounded-lg hover:bg-slate-100 transition-colors focus:outline-none focus:ring-2 focus:ring-indigo-500"
        aria-label={`Notifications ${totalUnread > 0 ? `(${totalUnread} unread)` : ''}`}
      >
        <Bell
          className={`h-5 w-5 ${
            hasCritical
              ? 'text-red-600 animate-pulse'
              : totalUnread > 0
              ? 'text-indigo-600'
              : 'text-slate-600'
          }`}
        />

        {/* Unread Badge */}
        {totalUnread > 0 && (
          <div className="absolute -top-1 -right-1">
            <UnreadBadge
              count={totalUnread}
              variant={hasCritical ? 'critical' : 'default'}
              size="small"
            />
          </div>
        )}
      </button>

      {/* Dropdown Panel */}
      {isOpen && (
        <div
          ref={dropdownRef}
          className="absolute right-0 top-full mt-2 z-50 animate-in fade-in slide-in-from-top-2 duration-200"
        >
          <NotificationCenter onClose={() => setIsOpen(false)} />
        </div>
      )}
    </div>
  );
}
