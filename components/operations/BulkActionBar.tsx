'use client';

import { Card } from '@/components/ui/card';
import { Button } from '@/components/ui/button';
import { Tooltip, TooltipContent, TooltipProvider, TooltipTrigger } from '@/components/ui/tooltip';
import { 
  CheckCircle, 
  UserPlus, 
  MessageSquare, 
  Calendar, 
  X,
  Loader2
} from 'lucide-react';
import { PCOutreachBulkAction } from '@/lib/types/operations';
import { cn } from '@/lib/utils';

interface BulkActionBarProps {
  selectedCount: number;
  onAction: (action: PCOutreachBulkAction) => void;
  onClearSelection: () => void;
  isProcessing: boolean;
}

export function BulkActionBar({ 
  selectedCount, 
  onAction, 
  onClearSelection, 
  isProcessing 
}: BulkActionBarProps) {
  if (selectedCount === 0) return null;

  return (
    <div className="fixed bottom-0 left-0 right-0 z-50 animate-in slide-in-from-bottom-4">
      <div className="max-w-4xl mx-auto px-4 pb-4">
        <Card className="shadow-lg border-t bg-white">
          <div className="p-4">
            <div className="flex items-center justify-between">
              {/* Left section: Selection count and clear button */}
              <div className="flex items-center gap-3">
                <span className="text-sm font-medium text-gray-700">
                  {selectedCount} selected
                </span>
                <Button
                  variant="ghost"
                  size="sm"
                  onClick={onClearSelection}
                  disabled={isProcessing}
                  className="h-8 w-8 p-0"
                >
                  <X className="h-4 w-4" />
                </Button>
              </div>

              {/* Right section: Action buttons */}
              <div className="flex items-center gap-2">
                {/* Mark Contacted button */}
                <Button
                  variant="default"
                  size="sm"
                  onClick={() => onAction('mark_contacted')}
                  disabled={isProcessing}
                  className="h-8"
                >
                  {isProcessing ? (
                    <Loader2 className="h-4 w-4 mr-1 animate-spin" />
                  ) : (
                    <CheckCircle className="h-4 w-4 mr-1" />
                  )}
                  Mark Contacted
                </Button>

                {/* Assign to Rep button */}
                <Button
                  variant="outline"
                  size="sm"
                  onClick={() => onAction('assign_to_rep')}
                  disabled={isProcessing}
                  className="h-8"
                >
                  <UserPlus className="h-4 w-4 mr-1" />
                  Assign to Rep
                </Button>

                {/* Bulk SMS button (disabled with tooltip) */}
                <TooltipProvider>
                  <Tooltip>
                    <TooltipTrigger asChild>
                      <div>
                        <Button
                          variant="outline"
                          size="sm"
                          disabled
                          className="h-8"
                        >
                          <MessageSquare className="h-4 w-4 mr-1" />
                          Bulk SMS
                        </Button>
                      </div>
                    </TooltipTrigger>
                    <TooltipContent>
                      <p>SMS functionality will be available after Twilio integration</p>
                    </TooltipContent>
                  </Tooltip>
                </TooltipProvider>

                {/* Schedule Check-in button */}
                <Button
                  variant="outline"
                  size="sm"
                  onClick={() => onAction('schedule_checkin')}
                  disabled={isProcessing}
                  className="h-8"
                >
                  <Calendar className="h-4 w-4 mr-1" />
                  Schedule Check-in
                </Button>
              </div>
            </div>
          </div>
        </Card>
      </div>
    </div>
  );
}
