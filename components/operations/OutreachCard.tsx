'use client';

import { useState } from 'react';
import { Card } from '@/components/ui/card';
import { Checkbox } from '@/components/ui/checkbox';
import { Button } from '@/components/ui/button';
import { Badge } from '@/components/ui/badge';
import { Skeleton } from '@/components/ui/skeleton';
import { ProjectDetailModal } from './ProjectDetailModal';
import { 
  Phone, 
  Mail, 
  MessageSquare, 
  CheckCircle, 
  Calendar,
  User,
  Building
} from 'lucide-react';
import { PCOutreachRecord } from '@/lib/types/operations';
import { cn } from '@/lib/utils';

interface OutreachCardProps {
  item: PCOutreachRecord;
  selected: boolean;
  onSelect: (recordId: number, selected: boolean) => void;
  onAction: (recordId: number, action: string) => void;
}

export function OutreachCard({ item, selected, onSelect, onAction }: OutreachCardProps) {
  const [modalOpen, setModalOpen] = useState(false);

  // Helper to extract value from QuickBase wrapped objects
  const extractValue = (field: any): string => {
    if (typeof field === 'object' && field?.value) {
      return String(field.value);
    }
    return String(field || '');
  };

  const getDaysOverdueColor = (days: number) => {
    if (days >= 8) return 'text-red-600 bg-red-50 border-red-200';
    if (days >= 4) return 'text-orange-600 bg-orange-50 border-orange-200';
    if (days >= 1) return 'text-yellow-600 bg-yellow-50 border-yellow-200';
    return 'text-gray-600 bg-gray-50 border-gray-200';
  };

  const getContactMethodIcon = (method: any) => {
    const methodStr = extractValue(method).toLowerCase();
    switch (methodStr) {
      case 'phone':
      case 'call':
        return <Phone className="h-4 w-4" />;
      case 'email':
        return <Mail className="h-4 w-4" />;
      case 'text':
      case 'sms':
        return <MessageSquare className="h-4 w-4" />;
      default:
        return <MessageSquare className="h-4 w-4" />;
    }
  };

  const formatLastAttempt = (date: string | null, attempts: number) => {
    if (!date || attempts === 0) return 'No attempts yet';
    
    const attemptDate = new Date(date);
    const now = new Date();
    const diffTime = Math.abs(now.getTime() - attemptDate.getTime());
    const diffDays = Math.ceil(diffTime / (1000 * 60 * 60 * 24));
    
    return `${attempts} attempt${attempts > 1 ? 's' : ''}, last ${diffDays} day${diffDays > 1 ? 's' : ''} ago`;
  };

  const borderColor = item.daysOverdue >= 8 ? 'border-l-red-500' : 
                     item.daysOverdue >= 4 ? 'border-l-orange-500' : 
                     item.daysOverdue >= 1 ? 'border-l-yellow-500' : 'border-l-gray-300';

  return (
    <>
      <Card 
        className={cn(
          "p-3 mobile:p-4 hover:bg-gray-50 transition-colors border-l-4 cursor-pointer",
          borderColor,
          selected && "ring-2 ring-blue-500"
        )}
        onClick={() => setModalOpen(true)}
      >
        <div className="flex items-start gap-3">
        {/* Selection checkbox */}
        <div className="flex-shrink-0 pt-1" onClick={(e) => e.stopPropagation()}>
          <Checkbox
            checked={selected}
            onCheckedChange={(checked) => onSelect(item.recordId, !!checked)}
            className="h-4 w-4"
          />
        </div>

        {/* Main content */}
        <div className="flex-1 min-w-0">
          <div className="flex items-start justify-between">
            <div className="flex-1 min-w-0">
              {/* Customer info */}
              <div className="flex items-center gap-2 mb-2">
                <h3 className="font-semibold text-gray-900 truncate">
                  {extractValue(item.customerName)}
                </h3>
                <span className="text-sm text-gray-500">
                  {item.projectId}
                </span>
              </div>

              {/* Project details */}
              <div className="flex items-center gap-4 mb-2 text-sm text-gray-600">
                <span>{extractValue(item.projectStage)}</span>
                {item.daysInStage && (
                  <span>{item.daysInStage} days in stage</span>
                )}
              </div>

              {/* Days overdue badge */}
              <div className="flex items-center gap-2 mb-2">
                <Badge 
                  variant="outline" 
                  className={cn(
                    "text-xs font-medium",
                    getDaysOverdueColor(item.daysOverdue)
                  )}
                >
                  {item.daysOverdue} days overdue
                </Badge>
              </div>

              {/* Contact info */}
              <div className="flex items-center gap-4 text-sm text-gray-500 mb-2">
                <span>{formatLastAttempt(item.outreachCompletedDate, item.numAttempts)}</span>
                {item.preferredContactMethod && (
                  <div className="flex items-center gap-1">
                    {getContactMethodIcon(item.preferredContactMethod)}
                    <span className="capitalize">{extractValue(item.preferredContactMethod)}</span>
                  </div>
                )}
              </div>

              {/* Sales rep and lender */}
              <div className="flex items-center gap-4 text-xs text-gray-400">
                {item.salesRepName && (
                  <div className="flex items-center gap-1">
                    <User className="h-3 w-3" />
                    <span>{extractValue(item.salesRepName)}</span>
                  </div>
                )}
                {item.lenderName && (
                  <div className="flex items-center gap-1">
                    <Building className="h-3 w-3" />
                    <span>{extractValue(item.lenderName)}</span>
                  </div>
                )}
              </div>
            </div>

            {/* Action buttons */}
            <div className="flex-shrink-0 flex items-center gap-1 ml-4" onClick={(e) => e.stopPropagation()}>
              {/* Call button (disabled - Twilio integration placeholder) */}
              <Button
                size="sm"
                variant="outline"
                disabled
                className="h-8 w-8 p-0"
                title="Available after Twilio integration"
              >
                <Phone className="h-4 w-4" />
              </Button>

              {/* Text button (disabled - Twilio integration placeholder) */}
              <Button
                size="sm"
                variant="outline"
                disabled
                className="h-8 w-8 p-0"
                title="Available after Twilio integration"
              >
                <MessageSquare className="h-4 w-4" />
              </Button>

              {/* Email button (disabled - Twilio integration placeholder) */}
              <Button
                size="sm"
                variant="outline"
                disabled
                className="h-8 w-8 p-0"
                title="Available after Twilio integration"
              >
                <Mail className="h-4 w-4" />
              </Button>

              {/* Mark Contacted button (enabled) */}
              <Button
                size="sm"
                variant="default"
                className="h-8 px-2"
                onClick={() => onAction(item.recordId, 'mark_contacted')}
              >
                <CheckCircle className="h-4 w-4 mr-1" />
                Mark Contacted
              </Button>
            </div>
          </div>
        </div>
      </div>
    </Card>
    
    <ProjectDetailModal
      recordId={item.recordId}
      isOpen={modalOpen}
      onClose={() => setModalOpen(false)}
    />
  </>
  );
}

export function OutreachCardSkeleton() {
  return (
    <Card className="p-3 mobile:p-4">
      <div className="flex items-start gap-3">
        {/* Checkbox skeleton */}
        <div className="flex-shrink-0 pt-1">
          <Skeleton className="h-4 w-4" />
        </div>

        {/* Content skeleton */}
        <div className="flex-1 min-w-0">
          <div className="flex items-start justify-between">
            <div className="flex-1 min-w-0">
              {/* Customer name and project ID */}
              <div className="flex items-center gap-2 mb-2">
                <Skeleton className="h-5 w-32" />
                <Skeleton className="h-4 w-16" />
              </div>

              {/* Project stage */}
              <div className="flex items-center gap-4 mb-2">
                <Skeleton className="h-4 w-20" />
                <Skeleton className="h-4 w-24" />
              </div>

              {/* Days overdue badge */}
              <div className="mb-2">
                <Skeleton className="h-6 w-24" />
              </div>

              {/* Contact info */}
              <div className="flex items-center gap-4 mb-2">
                <Skeleton className="h-4 w-32" />
                <Skeleton className="h-4 w-20" />
              </div>

              {/* Sales rep and lender */}
              <div className="flex items-center gap-4">
                <Skeleton className="h-3 w-24" />
                <Skeleton className="h-3 w-20" />
              </div>
            </div>

            {/* Action buttons skeleton */}
            <div className="flex-shrink-0 flex items-center gap-1 ml-4">
              <Skeleton className="h-8 w-8" />
              <Skeleton className="h-8 w-8" />
              <Skeleton className="h-8 w-8" />
              <Skeleton className="h-8 w-24" />
            </div>
          </div>
        </div>
      </div>
    </Card>
  );
}
