'use client';

import { useState } from 'react';
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card';
import { Badge } from '@/components/ui/badge';
import { Button } from '@/components/ui/button';
import { Skeleton } from '@/components/ui/skeleton';
import { ProjectDetailModal } from './ProjectDetailModal';
import { 
  Phone, 
  Mail, 
  MessageSquare, 
  Clock, 
  AlertCircle,
  User,
  Calendar
} from 'lucide-react';
import type { PCPriorityQueueItem } from '@/lib/types/operations';

interface PCPriorityQueueProps {
  priorityQueue: PCPriorityQueueItem[];
}

function PCPriorityQueueSkeleton() {
  return (
    <Card>
      <CardHeader>
        <CardTitle>Priority Outreach Queue</CardTitle>
        <p className="text-sm text-gray-600">Recommended contacts based on urgency</p>
      </CardHeader>
      <CardContent>
        <div className="space-y-3">
          {Array.from({ length: 3 }).map((_, i) => (
            <div key={i} className="flex items-center justify-between p-3 border rounded-lg">
              <div className="flex items-center space-x-3">
                <Skeleton className="h-6 w-12 rounded-full" />
                <div className="space-y-1">
                  <Skeleton className="h-4 w-32" />
                  <Skeleton className="h-3 w-24" />
                </div>
              </div>
              <div className="flex space-x-2">
                <Skeleton className="h-8 w-16" />
                <Skeleton className="h-8 w-16" />
                <Skeleton className="h-8 w-16" />
              </div>
            </div>
          ))}
        </div>
      </CardContent>
    </Card>
  );
}

export function PCPriorityQueue({ priorityQueue }: PCPriorityQueueProps) {
  const [selectedRecordId, setSelectedRecordId] = useState<number | null>(null);
  const [modalOpen, setModalOpen] = useState(false);
  const getPriorityColor = (score: number) => {
    if (score > 80) return 'bg-red-100 text-red-800 border-red-200';
    if (score > 50) return 'bg-orange-100 text-orange-800 border-orange-200';
    return 'bg-yellow-100 text-yellow-800 border-yellow-200';
  };

  // Helper to extract value from QuickBase wrapped objects
  const extractValue = (field: any): string => {
    if (typeof field === 'object' && field?.value) {
      return String(field.value);
    }
    return String(field || '');
  };

  const getContactMethodIcon = (method: string | any) => {
    // Handle both string and wrapped object {value: "string"} from QuickBase
    const methodStr = extractValue(method).toLowerCase() || 'call';

    switch (methodStr) {
      case 'text':
        return MessageSquare;
      case 'email':
        return Mail;
      default:
        return Phone;
    }
  };

  const formatLastContact = (lastContactDate: string | null) => {
    if (!lastContactDate) return 'Never contacted';
    
    const date = new Date(lastContactDate);
    if (isNaN(date.getTime())) return 'Invalid date';
    
    const now = new Date();
    const diffTime = now.getTime() - date.getTime();
    const diffDays = Math.floor(diffTime / (1000 * 60 * 60 * 24));
    
    if (diffDays === 0) return 'Today';
    if (diffDays === 1) return 'Yesterday';
    return `${diffDays} days ago`;
  };

  if (priorityQueue.length === 0) {
    return (
      <Card>
        <CardHeader>
          <CardTitle>Priority Outreach Queue</CardTitle>
          <p className="text-sm text-gray-600">Recommended contacts based on urgency</p>
        </CardHeader>
        <CardContent>
          <div className="text-center py-8 text-gray-500">
            <AlertCircle className="h-12 w-12 mx-auto mb-4 text-gray-400" />
            <p>No pending outreach at this time</p>
          </div>
        </CardContent>
      </Card>
    );
  }

  return (
    <Card>
      <CardHeader>
        <CardTitle>Priority Outreach Queue</CardTitle>
        <p className="text-sm text-gray-600">Recommended contacts based on urgency</p>
      </CardHeader>
      <CardContent>
        <div className="max-h-96 overflow-y-auto space-y-3">
          {priorityQueue.map((item) => {
            const ContactIcon = getContactMethodIcon(item.preferredContactMethod);
            
            return (
              <div 
                key={item.recordId} 
                className="flex items-center justify-between p-3 border rounded-lg hover:bg-gray-50 transition-colors cursor-pointer"
                onClick={() => {
                  setSelectedRecordId(item.recordId);
                  setModalOpen(true);
                }}
              >
                <div className="flex items-center space-x-3 flex-1 min-w-0">
                  <Badge 
                    variant="outline" 
                    className={`${getPriorityColor(item.priorityScore)} font-semibold`}
                  >
                    {item.priorityScore}
                  </Badge>
                  
                  <div className="flex-1 min-w-0">
                    <div className="flex items-center space-x-2">
                      <p className="font-medium text-gray-900 truncate">
                        {extractValue(item.customerName)}
                      </p>
                      <span className="text-xs text-gray-500">
                        #{item.projectId}
                      </span>
                    </div>

                    <div className="flex items-center space-x-4 mt-1">
                      <div className="flex items-center space-x-1 text-xs text-gray-600">
                        <Calendar className="h-3 w-3" />
                        <span>{extractValue(item.currentStage)}</span>
                        <span>•</span>
                        <span>{item.daysInStage} days</span>
                      </div>

                      <div className="flex items-center space-x-1 text-xs text-gray-600">
                        <Clock className="h-3 w-3" />
                        <span>{formatLastContact(item.lastContactDate)}</span>
                      </div>
                    </div>

                    <p className="text-xs text-gray-500 mt-1 truncate">
                      {extractValue(item.priorityReason)}
                    </p>
                  </div>
                </div>
                
                <div className="flex items-center space-x-2 ml-4">
                  <div className="flex items-center space-x-1 text-xs text-gray-500">
                    <ContactIcon className="h-3 w-3" />
                    <span>{extractValue(item.preferredContactMethod)}</span>
                  </div>
                  
                  <div className="flex space-x-1">
                    <Button 
                      size="sm" 
                      variant="outline" 
                      className="h-8 px-2"
                      disabled
                      onClick={(e) => e.stopPropagation()}
                    >
                      <Phone className="h-3 w-3" />
                    </Button>
                    <Button 
                      size="sm" 
                      variant="outline" 
                      className="h-8 px-2"
                      disabled
                      onClick={(e) => e.stopPropagation()}
                    >
                      <MessageSquare className="h-3 w-3" />
                    </Button>
                    <Button 
                      size="sm" 
                      variant="outline" 
                      className="h-8 px-2"
                      disabled
                      onClick={(e) => e.stopPropagation()}
                    >
                      <Mail className="h-3 w-3" />
                    </Button>
                  </div>
                </div>
              </div>
            );
          })}
        </div>
      </CardContent>
      
      <ProjectDetailModal
        recordId={selectedRecordId}
        isOpen={modalOpen}
        onClose={() => {
          setModalOpen(false);
          setSelectedRecordId(null);
        }}
      />
    </Card>
  );
}

export { PCPriorityQueueSkeleton };
