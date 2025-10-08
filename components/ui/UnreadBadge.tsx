'use client';

import { cn } from '@/lib/utils/cn';
import { UNREAD_BADGE_STYLES } from '@/lib/constants/notificationStyles';

interface UnreadBadgeProps {
  count: number;
  variant?: 'default' | 'critical';
  size?: 'small' | 'large';
  className?: string;
}

export function UnreadBadge({
  count,
  variant = 'default',
  size = 'small',
  className,
}: UnreadBadgeProps) {
  // Don't render if count is 0
  if (count === 0) return null;

  // Format count display (99+ for large numbers)
  const displayCount = count > 99 ? '99+' : count.toString();

  const badgeStyles = variant === 'critical'
    ? UNREAD_BADGE_STYLES.critical
    : UNREAD_BADGE_STYLES.default;

  const sizeStyles = size === 'large'
    ? UNREAD_BADGE_STYLES.large
    : UNREAD_BADGE_STYLES.small;

  return (
    <span
      className={cn(
        badgeStyles,
        sizeStyles,
        'inline-flex items-center justify-center min-w-[1.25rem] leading-none',
        'shadow-sm animate-in fade-in zoom-in duration-200',
        className
      )}
      aria-label={`${count} unread notification${count === 1 ? '' : 's'}`}
    >
      {displayCount}
    </span>
  );
}
