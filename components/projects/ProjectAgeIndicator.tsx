'use client';

import { Calendar } from 'lucide-react';
import { cn } from '@/lib/utils/cn';

interface ProjectAgeIndicatorProps {
  age: number;
}

function getAgeColor(days: number): string {
  if (days > 120) {
    return 'text-red-500 font-bold';
  }
  if (days > 90) {
    return 'text-amber-500 font-semibold';
  }
  return 'text-gray-500';
}

function getAgeLabel(days: number): string {
  if (days > 120) {
    return 'CRITICAL';
  }
  if (days > 90) {
    return 'WARNING';
  }
  return '';
}

export function ProjectAgeIndicator({ age }: ProjectAgeIndicatorProps) {
  const ageColor = getAgeColor(age);
  const ageLabel = getAgeLabel(age);

  return (
    <div className="flex flex-col items-end gap-1">
      {/* Calendar icon + age text */}
      <div className={cn('flex items-center gap-1', ageColor)}>
        <Calendar className="h-3 w-3" />
        <span className="text-sm">{age}d</span>
      </div>
      
      {/* Warning label (conditional) */}
      {ageLabel && (
        <div className={cn('text-xs font-bold', ageColor)}>
          {ageLabel}
        </div>
      )}
    </div>
  );
}
