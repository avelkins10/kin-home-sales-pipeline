'use client';

import { cn } from '@/lib/utils/cn';
import { AlertTriangle } from 'lucide-react';

interface TaskBadgeProps {
  count: number;
  variant?: 'default' | 'critical';
  size?: 'small' | 'large';
  className?: string;
}

export function TaskBadge({
  count,
  variant = 'default',
  size = 'small',
  className,
}: TaskBadgeProps) {
  // Don't render if count is 0
  if (count === 0) return null;

  // Format count display (99+ for large numbers)
  const displayCount = count > 99 ? '99+' : count.toString();

  const baseStyles = 'inline-flex items-center gap-1 rounded-full font-semibold shadow-sm animate-in fade-in zoom-in duration-200';
  
  const variantStyles = variant === 'critical'
    ? 'bg-red-100 text-red-800 border border-red-200'
    : 'bg-orange-100 text-orange-800 border border-orange-200';

  const sizeStyles = size === 'large'
    ? 'px-2.5 py-1 text-xs'
    : 'px-2 py-0.5 text-[10px]';

  const ariaLabel = `⚠️ ${count} task${count === 1 ? '' : 's'}`;
  const title = `⚠️ ${count} task${count === 1 ? '' : 's'}`;

  return (
    <span
      className={cn(
        baseStyles,
        variantStyles,
        sizeStyles,
        'min-w-[1.25rem] leading-none',
        className
      )}
      aria-label={ariaLabel}
      title={title}
    >
      <AlertTriangle className="h-3 w-3" />
      <span>{displayCount}</span>
      <span className="hidden sm:inline"> tasks</span>
    </span>
  );
}
