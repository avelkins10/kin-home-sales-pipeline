'use client'

import { Button } from '@/components/ui/button';
import { User, Users } from 'lucide-react';
import type { MetricsScope } from '@/lib/types/dashboard';

interface DashboardScopeToggleProps {
  selectedScope: MetricsScope;
  onScopeChange: (scope: MetricsScope) => void;
}

export function DashboardScopeToggle({ selectedScope, onScopeChange }: DashboardScopeToggleProps) {
  const scopes: { value: MetricsScope; label: string; icon: typeof User }[] = [
    { value: 'personal', label: 'My Performance', icon: User },
    { value: 'team', label: 'Team Performance', icon: Users },
  ];

  return (
    <div 
      className="flex items-center space-x-1" 
      role="group" 
      aria-label="Toggle between personal and team metrics"
      data-testid="scope-toggle"
    >
      {scopes.map((scope, index) => {
        const isActive = selectedScope === scope.value;
        const Icon = scope.icon;
        const isFirst = index === 0;
        const isLast = index === scopes.length - 1;

        return (
          <Button
            key={scope.value}
            variant={isActive ? 'default' : 'outline'}
            size="sm"
            onClick={() => onScopeChange(scope.value)}
            className={`
              px-4 py-2 text-sm font-medium transition-colors
              ${isFirst ? 'rounded-l-lg' : ''}
              ${isLast ? 'rounded-r-lg' : 'rounded-none'}
              ${!isLast ? 'border-r-0' : ''}
              ${isActive
                ? 'bg-blue-600 text-white hover:bg-blue-700'
                : 'bg-white text-gray-700 hover:bg-gray-50 border-gray-300'
              }
            `}
            aria-pressed={isActive}
            data-testid={`scope-toggle-${scope.value}`}
          >
            <Icon className="h-4 w-4 mr-2" />
            {scope.label}
          </Button>
        );
      })}
    </div>
  );
}
