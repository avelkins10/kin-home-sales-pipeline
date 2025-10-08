// Notification Style Constants
// Linear-inspired design with priority-based color coding

import type { NotificationPriority, NotificationType } from '@/lib/types/notification';

// Priority-based border and accent colors
export const PRIORITY_STYLES: Record<
  NotificationPriority,
  {
    borderColor: string;
    accentColor: string;
    bgColor: string;
    textColor: string;
    iconColor: string;
  }
> = {
  critical: {
    borderColor: 'border-l-red-500',
    accentColor: 'text-red-600',
    bgColor: 'bg-red-50',
    textColor: 'text-red-900',
    iconColor: 'text-red-500',
  },
  normal: {
    borderColor: 'border-l-blue-500',
    accentColor: 'text-blue-600',
    bgColor: 'bg-blue-50',
    textColor: 'text-blue-900',
    iconColor: 'text-blue-500',
  },
  info: {
    borderColor: 'border-l-slate-400',
    accentColor: 'text-slate-600',
    bgColor: 'bg-slate-50',
    textColor: 'text-slate-900',
    iconColor: 'text-slate-500',
  },
};

// Notification type labels and descriptions
export const NOTIFICATION_TYPE_LABELS: Record<NotificationType, string> = {
  quickbase_note: 'Operations Note',
  internal_message: 'Team Message',
  system_alert: 'System Alert',
};

// Default icons for notification types
export const NOTIFICATION_TYPE_ICONS: Record<NotificationType, string> = {
  quickbase_note: 'message-square',
  internal_message: 'users',
  system_alert: 'bell',
};

// Badge styles for unread counts
export const UNREAD_BADGE_STYLES = {
  default: 'bg-indigo-600 text-white',
  critical: 'bg-red-600 text-white',
  large: 'px-2 py-1 text-xs font-semibold rounded-full',
  small: 'px-1.5 py-0.5 text-[10px] font-bold rounded-full',
};

// Card animation classes
export const CARD_ANIMATIONS = {
  enter: 'animate-in fade-in slide-in-from-top-1 duration-200',
  exit: 'animate-out fade-out slide-out-to-right-1 duration-150',
  hover: 'transition-all hover:shadow-md hover:border-indigo-200',
};

// Read/Unread visual states
export const READ_STATE_STYLES = {
  unread: {
    bgColor: 'bg-white',
    borderColor: 'border-slate-200',
    textWeight: 'font-medium',
    opacity: 'opacity-100',
  },
  read: {
    bgColor: 'bg-slate-50',
    borderColor: 'border-slate-100',
    textWeight: 'font-normal',
    opacity: 'opacity-60',
  },
};

// Helper function to get priority styles
export function getPriorityStyles(priority: NotificationPriority) {
  return PRIORITY_STYLES[priority];
}

// Helper function to get notification type label
export function getNotificationTypeLabel(type: NotificationType): string {
  return NOTIFICATION_TYPE_LABELS[type];
}

// Helper function to get notification icon name
export function getNotificationIcon(type: NotificationType): string {
  return NOTIFICATION_TYPE_ICONS[type];
}

// Helper function to build card class names
export function getCardClassName(
  priority: NotificationPriority,
  isRead: boolean
): string {
  const priorityStyle = PRIORITY_STYLES[priority];
  const readState = isRead ? READ_STATE_STYLES.read : READ_STATE_STYLES.unread;

  return [
    'relative',
    'border-l-4',
    priorityStyle.borderColor,
    'border-t border-r border-b',
    readState.borderColor,
    readState.bgColor,
    readState.opacity,
    'rounded-lg',
    'p-4',
    'cursor-pointer',
    CARD_ANIMATIONS.hover,
  ].join(' ');
}
