'use client';

import { Card } from '@/components/ui/card';
import { Skeleton } from '@/components/ui/skeleton';
import { LucideIcon, TrendingUp, TrendingDown, Minus } from 'lucide-react';
import { cn } from '@/lib/utils';

interface MetricCardProps {
  label: string;
  value: number | string;
  subtitle?: string;
  icon: LucideIcon;
  color: string;
  bgColor: string;
  trend?: {
    value: number;
    direction: 'up' | 'down' | 'neutral';
  };
  formatter?: (value: number) => string;
  onClick?: () => void;
}

export function MetricCard({
  label,
  value,
  subtitle,
  icon: Icon,
  color,
  bgColor,
  trend,
  formatter,
  onClick
}: MetricCardProps) {
  const displayValue = typeof value === 'number' && formatter ? formatter(value) : value;
  
  const TrendIcon = trend?.direction === 'up' ? TrendingUp : 
                   trend?.direction === 'down' ? TrendingDown : Minus;
  
  const trendColor = trend?.direction === 'up' ? 'text-green-600' :
                    trend?.direction === 'down' ? 'text-red-600' : 'text-gray-500';

  const handleKeyDown = (event: React.KeyboardEvent) => {
    if (onClick && (event.key === 'Enter' || event.key === ' ')) {
      event.preventDefault();
      onClick();
    }
  };

  const ariaLabel = onClick ? `${label}: ${displayValue}${subtitle ? `, ${subtitle}` : ''}` : undefined;

  return (
    <Card 
      className={cn(
        "p-3 mobile:p-4 ipad:p-6 transition-shadow hover:shadow-md",
        onClick && "cursor-pointer"
      )}
      onClick={onClick}
      role={onClick ? "button" : undefined}
      tabIndex={onClick ? 0 : undefined}
      aria-label={ariaLabel}
      onKeyDown={onClick ? handleKeyDown : undefined}
      aria-live={trend ? "polite" : undefined}
    >
      <div className="flex flex-col mobile:flex-row items-start mobile:items-center gap-2 mobile:gap-0">
        {/* Left section - Metric info */}
        <div className="flex-1 space-y-1">
          <p className="text-xs mobile:text-sm font-medium text-gray-600">
            {label}
          </p>
          <div className="flex items-center gap-2">
            <p className="text-xl mobile:text-2xl ipad:text-3xl font-bold text-gray-900">
              {displayValue}
            </p>
            {trend && (
              <div className={cn("flex items-center gap-1", trendColor)}>
                <TrendIcon className="h-3 w-3" />
                <span className="text-xs font-medium">
                  {trend.value > 0 ? '+' : ''}{trend.value}%
                </span>
              </div>
            )}
          </div>
          {subtitle && (
            <p className="text-xs text-gray-500">
              {subtitle}
            </p>
          )}
        </div>
        
        {/* Right section - Icon */}
        <div className={cn(
          "flex items-center justify-center rounded-full p-2 mobile:p-3",
          bgColor
        )}>
          <Icon className={cn("h-5 w-5 mobile:h-6 mobile:w-6 ipad:h-8 ipad:w-8", color)} />
        </div>
      </div>
    </Card>
  );
}

export function MetricCardSkeleton() {
  return (
    <Card className="p-3 mobile:p-4 ipad:p-6">
      <div className="flex flex-col mobile:flex-row items-start mobile:items-center gap-2 mobile:gap-0">
        {/* Left section - Metric info skeleton */}
        <div className="flex-1 space-y-1">
          <Skeleton className="h-3 w-20 mobile:h-4 mobile:w-24" />
          <Skeleton className="h-6 w-16 mobile:h-8 mobile:w-20 ipad:h-10 ipad:w-24" />
          <Skeleton className="h-3 w-16 mobile:h-4 mobile:w-20" />
        </div>
        
        {/* Right section - Icon skeleton */}
        <div className="flex items-center justify-center rounded-full p-2 mobile:p-3 bg-gray-100">
          <Skeleton className="h-5 w-5 mobile:h-6 mobile:w-6 ipad:h-8 ipad:w-8 rounded" />
        </div>
      </div>
    </Card>
  );
}
