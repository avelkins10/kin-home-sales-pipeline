'use client';

import { useState } from 'react';
import { useRouter } from 'next/navigation';
import { MessageSquare, Users, Bell, ExternalLink } from 'lucide-react';
import type { Notification } from '@/lib/types/notification';
import {
  getCardClassName,
  getPriorityStyles,
  getNotificationTypeLabel,
} from '@/lib/constants/notificationStyles';
import { useMarkAsRead } from '@/lib/hooks/useNotifications';

interface NotificationCardProps {
  notification: Notification;
  onClick?: () => void;
}

export function NotificationCard({ notification, onClick }: NotificationCardProps) {
  const router = useRouter();
  const markAsRead = useMarkAsRead();
  const [isMarking, setIsMarking] = useState(false);

  const priorityStyles = getPriorityStyles(notification.priority);
  const cardClassName = getCardClassName(notification.priority, notification.is_read);

  // Select icon based on notification type
  const Icon = notification.type === 'quickbase_note'
    ? MessageSquare
    : notification.type === 'internal_message'
    ? Users
    : Bell;

  // Format date
  const formatDate = (date: Date) => {
    const now = new Date();
    const notifDate = new Date(date);
    const diffMs = now.getTime() - notifDate.getTime();
    const diffMins = Math.floor(diffMs / 60000);
    const diffHours = Math.floor(diffMs / 3600000);
    const diffDays = Math.floor(diffMs / 86400000);

    if (diffMins < 1) return 'Just now';
    if (diffMins < 60) return `${diffMins}m ago`;
    if (diffHours < 24) return `${diffHours}h ago`;
    if (diffDays < 7) return `${diffDays}d ago`;

    return notifDate.toLocaleDateString('en-US', {
      month: 'short',
      day: 'numeric',
      year: notifDate.getFullYear() !== now.getFullYear() ? 'numeric' : undefined,
    });
  };

  const handleClick = async () => {
    // Mark as read if unread
    if (!notification.is_read && !isMarking) {
      setIsMarking(true);
      try {
        await markAsRead.mutateAsync(notification.id);
      } catch (error) {
        console.error('Failed to mark as read:', error);
      } finally {
        setIsMarking(false);
      }
    }

    // Navigate to action URL if provided
    if (notification.action_url) {
      router.push(notification.action_url);
    }

    // Call custom onClick handler
    onClick?.();
  };

  return (
    <div
      className={cardClassName}
      onClick={handleClick}
      role="button"
      tabIndex={0}
      onKeyDown={(e) => {
        if (e.key === 'Enter' || e.key === ' ') {
          handleClick();
        }
      }}
    >
      {/* Header - Icon, Type, and Time */}
      <div className="flex items-start justify-between mb-2">
        <div className="flex items-center gap-2">
          <Icon className={`h-4 w-4 ${priorityStyles.iconColor}`} />
          <span className={`text-xs font-medium ${priorityStyles.accentColor}`}>
            {getNotificationTypeLabel(notification.type)}
          </span>
        </div>
        <div className="flex items-center gap-2">
          <span className="text-xs text-slate-500">
            {formatDate(notification.created_at)}
          </span>
          {!notification.is_read && (
            <span className="h-2 w-2 rounded-full bg-indigo-600" aria-label="Unread" />
          )}
        </div>
      </div>

      {/* Title */}
      <h4 className={`text-sm font-semibold text-slate-900 mb-1 ${!notification.is_read ? 'font-bold' : ''}`}>
        {notification.title}
      </h4>

      {/* Message */}
      {notification.message && (
        <p className="text-sm text-slate-700 mb-2 line-clamp-2">
          {notification.message}
        </p>
      )}

      {/* Footer - Sender info and action link */}
      <div className="flex items-center justify-between">
        {notification.sender_name && (
          <span className="text-xs text-slate-500">
            From: {notification.sender_name}
          </span>
        )}
        {notification.action_url && (
          <span className="flex items-center gap-1 text-xs text-indigo-600 font-medium">
            View
            <ExternalLink className="h-3 w-3" />
          </span>
        )}
      </div>
    </div>
  );
}
