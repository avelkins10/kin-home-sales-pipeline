'use client';

import { User, Users } from 'lucide-react';
import { cn } from '@/lib/utils/cn';

interface OwnershipBadgeProps {
  status: 'mine' | 'team-closer' | 'team-setter' | 'unassigned';
  displayName: string | null;
  className?: string;
}

interface OwnershipConfig {
  label: string;
  icon: React.ElementType;
  bgColor: string;
  textColor: string;
  borderColor: string;
}

function getOwnershipConfig(status: string): OwnershipConfig {
  switch (status) {
    case 'mine':
      return {
        label: 'Mine',
        icon: User,
        bgColor: 'bg-blue-50',
        textColor: 'text-blue-700',
        borderColor: 'border-blue-200',
      };
    case 'team-closer':
    case 'team-setter':
      return {
        label: 'Team',
        icon: Users,
        bgColor: 'bg-purple-50',
        textColor: 'text-purple-700',
        borderColor: 'border-purple-200',
      };
    case 'unassigned':
      return {
        label: 'Unassigned',
        icon: Users,
        bgColor: 'bg-gray-50',
        textColor: 'text-gray-600',
        borderColor: 'border-gray-200',
      };
    default:
      return {
        label: 'Unknown',
        icon: User,
        bgColor: 'bg-gray-50',
        textColor: 'text-gray-600',
        borderColor: 'border-gray-200',
      };
  }
}

export function OwnershipBadge({ status, displayName, className }: OwnershipBadgeProps) {
  const config = getOwnershipConfig(status);
  const Icon = config.icon;

  // Determine what to display
  const label = status === 'mine' 
    ? config.label 
    : displayName || config.label;

  return (
    <div
      className={cn(
        'inline-flex items-center gap-1.5 px-2.5 py-1 rounded-full border text-xs font-medium transition-colors',
        config.bgColor,
        config.textColor,
        config.borderColor,
        className
      )}
      title={`Project owner: ${label}`}
    >
      <Icon className="w-3 h-3" strokeWidth={2.5} />
      <span className="hidden sm:inline">{label}</span>
    </div>
  );
}
