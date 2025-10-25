'use client';

import { useState, useEffect } from 'react';
import { Badge } from '@/components/ui/badge';
import { Tooltip, TooltipContent, TooltipProvider, TooltipTrigger } from '@/components/ui/tooltip';
import { Clock, AlertCircle, AlertTriangle } from 'lucide-react';

interface EscalationTimerProps {
  deadline: string;
  urgency: 'critical' | 'high' | 'normal';
  compact?: boolean;
}

export function EscalationTimer({ deadline, urgency, compact = false }: EscalationTimerProps) {
  const [timeRemaining, setTimeRemaining] = useState<number>(0);
  const [isExpired, setIsExpired] = useState<boolean>(false);

  useEffect(() => {
    const updateTimer = () => {
      const now = new Date().getTime();
      const deadlineTime = new Date(deadline).getTime();
      const remaining = deadlineTime - now;
      
      setTimeRemaining(remaining);
      setIsExpired(remaining < 0);
    };

    // Update immediately
    updateTimer();

    // Update every second
    const interval = setInterval(updateTimer, 1000);

    return () => clearInterval(interval);
  }, [deadline]);

  const formatTimeRemaining = (ms: number): string => {
    const absMs = Math.abs(ms);
    const hours = Math.floor(absMs / (1000 * 60 * 60));
    const minutes = Math.floor((absMs % (1000 * 60 * 60)) / (1000 * 60));
    const days = Math.floor(hours / 24);

    if (days > 0) {
      return `${days}d ${hours % 24}h`;
    } else if (hours > 0) {
      return `${hours}h ${minutes}m`;
    } else {
      return `${minutes}m`;
    }
  };

  const getUrgencyColor = (urgency: string, isExpired: boolean): string => {
    if (isExpired) return 'destructive';
    if (urgency === 'critical') return 'destructive';
    if (urgency === 'high') return 'secondary';
    return 'default';
  };

  const getUrgencyIcon = (urgency: string, isExpired: boolean) => {
    if (isExpired) return AlertCircle;
    if (urgency === 'critical') return AlertTriangle;
    return Clock;
  };

  const getDisplayText = (): string => {
    if (isExpired) {
      return `Overdue by ${formatTimeRemaining(timeRemaining)}`;
    }
    
    const hoursUntilDeadline = timeRemaining / (1000 * 60 * 60);
    
    if (hoursUntilDeadline < 24) {
      return `${formatTimeRemaining(timeRemaining)} remaining`;
    } else if (hoursUntilDeadline < 48) {
      return `${formatTimeRemaining(timeRemaining)} remaining`;
    } else {
      return `${formatTimeRemaining(timeRemaining)} remaining`;
    }
  };

  const getBadgeVariant = (): "default" | "secondary" | "destructive" | "outline" => {
    if (isExpired) return 'destructive';
    if (urgency === 'critical') return 'destructive';
    if (urgency === 'high') return 'secondary';
    return 'default';
  };

  const Icon = getUrgencyIcon(urgency, isExpired);
  const displayText = getDisplayText();
  const badgeVariant = getBadgeVariant();

  if (compact) {
    return (
      <TooltipProvider>
        <Tooltip>
          <TooltipTrigger asChild>
            <Badge variant={badgeVariant} className="flex items-center gap-1">
              <Icon className="h-3 w-3" />
              <span className="text-xs">
                {isExpired ? 'Overdue' : formatTimeRemaining(timeRemaining)}
              </span>
            </Badge>
          </TooltipTrigger>
          <TooltipContent>
            <p>{displayText}</p>
            <p className="text-xs text-muted-foreground">
              Deadline: {new Date(deadline).toLocaleString()}
            </p>
          </TooltipContent>
        </Tooltip>
      </TooltipProvider>
    );
  }

  return (
    <TooltipProvider>
      <Tooltip>
        <TooltipTrigger asChild>
          <Badge 
            variant={badgeVariant} 
            className={`flex items-center gap-2 ${
              (isExpired || urgency === 'critical') ? 'animate-pulse' : ''
            }`}
          >
            <Icon className="h-4 w-4" />
            <span className="text-sm font-medium">
              {displayText}
            </span>
          </Badge>
        </TooltipTrigger>
        <TooltipContent>
          <p>{displayText}</p>
          <p className="text-xs text-muted-foreground">
            Deadline: {new Date(deadline).toLocaleString()}
          </p>
          <p className="text-xs text-muted-foreground">
            Urgency: {urgency}
          </p>
        </TooltipContent>
      </Tooltip>
    </TooltipProvider>
  );
}
