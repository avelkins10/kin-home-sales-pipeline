'use client';

import { Progress } from '@/components/ui/progress';
import { CheckCircle, Circle } from 'lucide-react';
import { calculatePasswordStrength, getPasswordRequirements } from '@/lib/utils/password-strength';
import { cn } from '@/lib/utils/cn';

interface PasswordStrengthIndicatorProps {
  password: string;
  showRequirements?: boolean;
  className?: string;
}

export function PasswordStrengthIndicator({ 
  password, 
  showRequirements = false, 
  className 
}: PasswordStrengthIndicatorProps) {
  if (!password) return null;

  const { score, label, color } = calculatePasswordStrength(password);
  const requirements = getPasswordRequirements(password);

  const getProgressColor = (color: string) => {
    switch (color) {
      case 'red':
        return 'bg-red-500';
      case 'orange':
        return 'bg-orange-500';
      case 'yellow':
        return 'bg-yellow-500';
      case 'green':
        return 'bg-green-500';
      default:
        return 'bg-gray-500';
    }
  };

  return (
    <div className={cn('space-y-2', className)}>
      {/* Progress bar and label */}
      <div className="flex items-center gap-2">
        <Progress 
          value={score} 
          className="flex-1 h-2"
        />
        <span className={cn(
          'text-sm font-medium min-w-[60px]',
          color === 'red' && 'text-red-600',
          color === 'orange' && 'text-orange-600',
          color === 'yellow' && 'text-yellow-600',
          color === 'green' && 'text-green-600'
        )}>
          {label}
        </span>
      </div>

      {/* Requirements checklist */}
      {showRequirements && (
        <div className="space-y-1">
          {requirements.map((requirement, index) => (
            <div key={index} className="flex items-center gap-2 text-sm">
              {requirement.met ? (
                <CheckCircle className="h-4 w-4 text-green-600" />
              ) : (
                <Circle className="h-4 w-4 text-gray-400" />
              )}
              <span className={cn(
                requirement.met ? 'text-green-700' : 'text-gray-600'
              )}>
                {requirement.text}
              </span>
            </div>
          ))}
        </div>
      )}
    </div>
  );
}
