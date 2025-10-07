'use client';

import { Check, X, AlertCircle, Pause, Clock, CircleDot } from 'lucide-react';
import { cn } from '@/lib/utils/cn';

interface StatusBadgeProps {
  status: string;
  className?: string;
}

interface StatusConfig {
  label: string;
  icon: React.ElementType;
  bgColor: string;
  textColor: string;
  borderColor: string;
}

function getStatusConfig(status: string): StatusConfig {
  const normalizedStatus = status.toLowerCase();

  // Check for specific status keywords
  if (normalizedStatus.includes('active')) {
    return {
      label: 'Active',
      icon: CircleDot,
      bgColor: 'bg-emerald-50',
      textColor: 'text-emerald-700',
      borderColor: 'border-emerald-200',
    };
  }

  if (normalizedStatus.includes('completed') || normalizedStatus.includes('complete')) {
    return {
      label: 'Completed',
      icon: Check,
      bgColor: 'bg-blue-50',
      textColor: 'text-blue-700',
      borderColor: 'border-blue-200',
    };
  }

  if (normalizedStatus.includes('cancelled') || normalizedStatus.includes('cancel')) {
    // Check if it's pending cancel first
    if (normalizedStatus.includes('pending')) {
      return {
        label: 'Pending Cancel',
        icon: Clock,
        bgColor: 'bg-orange-50',
        textColor: 'text-orange-700',
        borderColor: 'border-orange-200',
      };
    }
    return {
      label: 'Cancelled',
      icon: X,
      bgColor: 'bg-slate-100',
      textColor: 'text-slate-700',
      borderColor: 'border-slate-300',
    };
  }

  if (normalizedStatus.includes('rejected') || normalizedStatus.includes('reject')) {
    return {
      label: 'Rejected',
      icon: AlertCircle,
      bgColor: 'bg-rose-50',
      textColor: 'text-rose-700',
      borderColor: 'border-rose-200',
    };
  }

  if (normalizedStatus.includes('hold')) {
    return {
      label: 'On Hold',
      icon: Pause,
      bgColor: 'bg-amber-50',
      textColor: 'text-amber-700',
      borderColor: 'border-amber-200',
    };
  }

  // Default/fallback for unknown statuses
  return {
    label: status || 'Unknown',
    icon: CircleDot,
    bgColor: 'bg-gray-50',
    textColor: 'text-gray-700',
    borderColor: 'border-gray-200',
  };
}

export function StatusBadge({ status, className }: StatusBadgeProps) {
  const config = getStatusConfig(status);
  const Icon = config.icon;

  return (
    <div
      className={cn(
        'inline-flex items-center gap-1.5 px-2.5 py-1 rounded-full border text-xs font-medium transition-colors',
        config.bgColor,
        config.textColor,
        config.borderColor,
        className
      )}
      title={`Project status: ${config.label}`}
    >
      <Icon className="w-3 h-3" strokeWidth={2.5} />
      <span className="hidden sm:inline">{config.label}</span>
    </div>
  );
}
