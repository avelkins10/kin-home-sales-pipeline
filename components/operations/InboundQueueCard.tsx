'use client';

import { useState } from 'react';
import { Card, CardContent } from '@/components/ui/card';
import { Button } from '@/components/ui/button';
import { Badge } from '@/components/ui/badge';
import { Skeleton } from '@/components/ui/skeleton';
import { ProjectDetailModal } from './ProjectDetailModal';
import { PCSalesAidRequest } from '@/lib/types/operations';
import { MessageSquare, AlertTriangle, CheckCircle } from 'lucide-react';

interface InboundQueueCardProps {
  item: PCSalesAidRequest;
  onAction: (recordId: number, action: 'respond' | 'escalate' | 'resolve') => void;
}

export function InboundQueueCard({ item, onAction }: InboundQueueCardProps) {
  const [modalOpen, setModalOpen] = useState(false);

  // Helper to extract value from QuickBase wrapped objects
  const extractValue = (field: any): string => {
    if (typeof field === 'object' && field?.value) {
      return String(field.value);
    }
    return String(field || '');
  };

  const getUrgencyColor = (urgency: string) => {
    switch (urgency) {
      case 'critical': return 'border-l-red-500';
      case 'high': return 'border-l-orange-500';
      default: return 'border-l-blue-500';
    }
  };

  const getUrgencyDotColor = (urgency: string) => {
    switch (urgency) {
      case 'critical': return 'bg-red-500';
      case 'high': return 'bg-orange-500';
      default: return 'bg-blue-500';
    }
  };

  const formatTimeWaiting = (days: number) => {
    if (days === 0) return 'Today';
    if (days === 1) return '1 day';
    return `${days} days`;
  };

  const formatDeadline = (deadline: string) => {
    if (!deadline) return null;
    
    const deadlineDate = new Date(deadline);
    const now = new Date();
    const hoursUntilDeadline = (deadlineDate.getTime() - now.getTime()) / (1000 * 60 * 60);
    
    if (hoursUntilDeadline < 0) {
      const hoursOverdue = Math.abs(hoursUntilDeadline);
      return `Overdue by ${Math.floor(hoursOverdue)}h`;
    } else if (hoursUntilDeadline < 24) {
      return `Deadline in ${Math.floor(hoursUntilDeadline)}h`;
    }
    
    return null;
  };

  return (
    <>
      <Card 
        className={`${getUrgencyColor(item.urgency)} hover:bg-gray-50 transition-colors cursor-pointer`}
        onClick={() => setModalOpen(true)}
      >
        <CardContent className="p-3 mobile:p-4">
        <div className="flex items-start gap-3">
          {/* Urgency indicator */}
          <div className={`w-3 h-3 rounded-full ${getUrgencyDotColor(item.urgency)} mt-1 flex-shrink-0`} />
          
          {/* Main content */}
          <div className="flex-1 min-w-0">
            <div className="flex items-start justify-between gap-2 mb-2">
              <div className="min-w-0 flex-1">
                <div className="flex items-center gap-2 mb-1">
                  <span className="font-semibold text-gray-900 truncate">
                    {extractValue(item.salesRepName)}
                  </span>
                  <Badge variant="outline" className="text-xs">
                    {item.projectId}
                  </Badge>
                </div>
                <p className="text-sm text-gray-600 truncate">
                  {extractValue(item.customerName)}
                </p>
              </div>
              
              {/* Time waiting badge */}
              <Badge variant="secondary" className="text-xs whitespace-nowrap">
                Waiting {formatTimeWaiting(item.timeWaiting)}
              </Badge>
            </div>
            
            {/* Message preview */}
            <p className="text-sm text-gray-700 mb-2 line-clamp-2">
              {extractValue(item.messagePreview)}
            </p>
            
            {/* Deadline countdown */}
            {formatDeadline(item.rep72HourDeadline) && (
              <div className="flex items-center gap-2 mb-2">
                <Badge 
                  variant={item.urgency === 'critical' ? 'destructive' : 'secondary'}
                  className="text-xs"
                >
                  {formatDeadline(item.rep72HourDeadline)}
                </Badge>
              </div>
            )}
          </div>
          
          {/* Action buttons */}
          <div className="flex flex-col gap-1 flex-shrink-0" onClick={(e) => e.stopPropagation()}>
            <Button
              size="sm"
              variant="default"
              onClick={() => onAction(item.recordId, 'respond')}
              className="h-8 px-2 text-xs"
            >
              <MessageSquare className="w-3 h-3 mr-1" />
              Respond
            </Button>
            <Button
              size="sm"
              variant="outline"
              onClick={() => onAction(item.recordId, 'escalate')}
              className="h-8 px-2 text-xs"
            >
              <AlertTriangle className="w-3 h-3 mr-1" />
              Escalate
            </Button>
            <Button
              size="sm"
              variant="outline"
              onClick={() => onAction(item.recordId, 'resolve')}
              className="h-8 px-2 text-xs"
            >
              <CheckCircle className="w-3 h-3 mr-1" />
              Resolve
            </Button>
          </div>
        </div>
      </CardContent>
    </Card>
    
    <ProjectDetailModal
      recordId={item.recordId}
      isOpen={modalOpen}
      onClose={() => setModalOpen(false)}
    />
  </>
  );
}

export function InboundQueueCardSkeleton() {
  return (
    <Card className="border-l-gray-300">
      <CardContent className="p-3 mobile:p-4">
        <div className="flex items-start gap-3">
          {/* Urgency indicator skeleton */}
          <Skeleton className="w-3 h-3 rounded-full mt-1 flex-shrink-0" />
          
          {/* Main content skeleton */}
          <div className="flex-1 min-w-0">
            <div className="flex items-start justify-between gap-2 mb-2">
              <div className="min-w-0 flex-1">
                <div className="flex items-center gap-2 mb-1">
                  <Skeleton className="h-4 w-24" />
                  <Skeleton className="h-5 w-16" />
                </div>
                <Skeleton className="h-4 w-32" />
              </div>
              <Skeleton className="h-5 w-20" />
            </div>
            
            <Skeleton className="h-4 w-full mb-1" />
            <Skeleton className="h-4 w-3/4 mb-2" />
            <Skeleton className="h-5 w-24" />
          </div>
          
          {/* Action buttons skeleton */}
          <div className="flex flex-col gap-1 flex-shrink-0">
            <Skeleton className="h-8 w-16" />
            <Skeleton className="h-8 w-16" />
            <Skeleton className="h-8 w-16" />
          </div>
        </div>
      </CardContent>
    </Card>
  );
}
