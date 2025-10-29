'use client';

import { Badge } from '@/components/ui/badge';
import {
  Circle,
  Navigation,
  Play,
  CheckCircle,
  XCircle,
  AlertTriangle,
  Clock,
  Ban
} from 'lucide-react';
import { cn } from '@/lib/utils';
import type { FieldTrackingTaskStatus } from '@/lib/types/operations';

interface FieldTrackingTaskStatusBadgeProps {
  status: FieldTrackingTaskStatus | null;
  className?: string;
  showIcon?: boolean;
  animate?: boolean;
}

export function FieldTrackingTaskStatusBadge({
  status,
  className,
  showIcon = true,
  animate = true,
}: FieldTrackingTaskStatusBadgeProps) {
  const getStatusConfig = (status: FieldTrackingTaskStatus | null) => {
    switch (status) {
      case 'NOT_STARTED':
        return {
          label: 'Not Started',
          color: 'bg-gray-100 text-gray-800 border-gray-300',
          icon: Circle,
          iconColor: 'text-gray-600',
          animation: '',
        };
      case 'ENROUTE':
        return {
          label: 'En Route',
          color: 'bg-blue-100 text-blue-800 border-blue-300',
          icon: Navigation,
          iconColor: 'text-blue-600',
          animation: 'animate-pulse',
        };
      case 'STARTED':
        return {
          label: 'In Progress',
          color: 'bg-yellow-100 text-yellow-800 border-yellow-300',
          icon: Play,
          iconColor: 'text-yellow-600',
          animation: 'animate-pulse',
        };
      case 'COMPLETE':
        return {
          label: 'Complete',
          color: 'bg-green-100 text-green-800 border-green-300',
          icon: CheckCircle,
          iconColor: 'text-green-600',
          animation: '',
        };
      case 'LATE':
        return {
          label: 'Late',
          color: 'bg-red-100 text-red-800 border-red-300',
          icon: Clock,
          iconColor: 'text-red-600',
          animation: 'animate-pulse',
        };
      case 'NOSHOW':
        return {
          label: 'No Show',
          color: 'bg-red-100 text-red-800 border-red-300',
          icon: XCircle,
          iconColor: 'text-red-600',
          animation: 'animate-bounce',
        };
      case 'EXCEPTION':
        return {
          label: 'Exception',
          color: 'bg-orange-100 text-orange-800 border-orange-300',
          icon: AlertTriangle,
          iconColor: 'text-orange-600',
          animation: 'animate-pulse',
        };
      case 'CANCELLED':
        return {
          label: 'Cancelled',
          color: 'bg-gray-100 text-gray-600 border-gray-300',
          icon: Ban,
          iconColor: 'text-gray-500',
          animation: '',
        };
      default:
        return {
          label: 'Unknown',
          color: 'bg-gray-100 text-gray-600 border-gray-300',
          icon: Circle,
          iconColor: 'text-gray-500',
          animation: '',
        };
    }
  };

  const config = getStatusConfig(status);
  const Icon = config.icon;

  return (
    <Badge
      variant="outline"
      className={cn(
        config.color,
        'font-medium',
        animate && config.animation,
        className
      )}
    >
      {showIcon && (
        <Icon className={cn('h-3 w-3 mr-1', config.iconColor)} />
      )}
      {config.label}
    </Badge>
  );
}
