'use client';

import { useState } from 'react';
import { Loader2, Inbox, Filter } from 'lucide-react';
import { useNotifications } from '@/lib/hooks/useNotifications';
import { NotificationCard } from './NotificationCard';
import type { NotificationPriority } from '@/lib/types/notification';

interface NotificationCenterProps {
  onClose?: () => void;
}

export function NotificationCenter({ onClose }: NotificationCenterProps) {
  const [filter, setFilter] = useState<'all' | NotificationPriority | 'tasks'>('all');
  const [showUnreadOnly, setShowUnreadOnly] = useState(false);

  const { data, isLoading } = useNotifications({
    limit: 50,
    unreadOnly: showUnreadOnly,
  });

  const notifications = data?.notifications || [];

  // Filter by priority if selected
  const filteredNotifications = 
    filter === 'all'
      ? notifications
      : filter === 'tasks'
      ? notifications.filter((n) => 
          ['task_submitted', 'task_approved', 'task_revision_needed', 'all_tasks_complete'].includes(n.type)
        )
      : notifications.filter((n) => n.priority === filter);

  return (
    <div className="w-[420px] max-h-[600px] flex flex-col bg-white rounded-lg shadow-xl border border-slate-200">
      {/* Header */}
      <div className="flex items-center justify-between px-4 py-3 border-b border-slate-200">
        <h3 className="text-base font-semibold text-slate-900">Notifications</h3>
        <div className="flex items-center gap-2">
          {/* Unread only toggle */}
          <button
            onClick={() => setShowUnreadOnly(!showUnreadOnly)}
            className={`px-2 py-1 text-xs font-medium rounded-md transition-colors ${
              showUnreadOnly
                ? 'bg-indigo-100 text-indigo-700'
                : 'bg-slate-100 text-slate-700 hover:bg-slate-200'
            }`}
          >
            Unread
          </button>
        </div>
      </div>

      {/* Priority Filter Tabs */}
      <div className="flex items-center gap-1 px-4 py-2 border-b border-slate-100 bg-slate-50">
        <Filter className="h-3.5 w-3.5 text-slate-500 mr-1" />
        {(['all', 'critical', 'normal', 'info', 'tasks'] as const).map((priorityFilter) => {
          const isActive = filter === priorityFilter;
          
          // Calculate count based on filter type
          let count: number;
          if (priorityFilter === 'all') {
            count = notifications.length;
          } else if (priorityFilter === 'tasks') {
            count = notifications.filter((n) => 
              ['task_submitted', 'task_approved', 'task_revision_needed', 'all_tasks_complete'].includes(n.type)
            ).length;
          } else {
            count = notifications.filter((n) => n.priority === priorityFilter).length;
          }

          return (
            <button
              key={priorityFilter}
              onClick={() => setFilter(priorityFilter)}
              className={`px-3 py-1 text-xs font-medium rounded-md transition-colors capitalize ${
                isActive
                  ? 'bg-white text-slate-900 shadow-sm'
                  : 'text-slate-600 hover:text-slate-900 hover:bg-white/50'
              }`}
            >
              {priorityFilter}
              {count > 0 && (
                <span className="ml-1.5 text-[10px] opacity-60">({count})</span>
              )}
            </button>
          );
        })}
      </div>

      {/* Notifications List */}
      <div className="flex-1 overflow-y-auto p-3 space-y-2">
        {isLoading ? (
          <div className="flex items-center justify-center py-12">
            <Loader2 className="h-6 w-6 animate-spin text-indigo-600" />
          </div>
        ) : filteredNotifications.length === 0 ? (
          <div className="flex flex-col items-center justify-center py-12 text-center">
            <Inbox className="h-12 w-12 text-slate-300 mb-3" />
            <p className="text-sm font-medium text-slate-600">
              {showUnreadOnly ? 'No unread notifications' : 'No notifications'}
            </p>
            <p className="text-xs text-slate-500 mt-1">
              {showUnreadOnly
                ? "You're all caught up!"
                : 'New notifications will appear here'}
            </p>
          </div>
        ) : (
          filteredNotifications.map((notification) => (
            <NotificationCard
              key={notification.id}
              notification={notification}
              onClick={onClose}
            />
          ))
        )}
      </div>

      {/* Footer - View All Link */}
      {filteredNotifications.length > 0 && (
        <div className="px-4 py-3 border-t border-slate-200 bg-slate-50">
          <button
            onClick={onClose}
            className="w-full text-center text-sm font-medium text-indigo-600 hover:text-indigo-700 transition-colors"
          >
            View all notifications
          </button>
        </div>
      )}
    </div>
  );
}
