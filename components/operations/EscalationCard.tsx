'use client';

import { useState } from 'react';
import { Card } from '@/components/ui/card';
import { Button } from '@/components/ui/button';
import { Badge } from '@/components/ui/badge';
import { AlertDialog, AlertDialogContent, AlertDialogHeader, AlertDialogTitle, AlertDialogTrigger } from '@/components/ui/alert-dialog';
import { Dialog, DialogContent, DialogHeader, DialogTitle, DialogTrigger } from '@/components/ui/dialog';
import { Skeleton } from '@/components/ui/skeleton';
import {
  UserPlus,
  Clock,
  CheckCircle,
  MessageSquare,
  History,
  Zap,
  Home,
  DollarSign,
  AlertCircle
} from 'lucide-react';
import { PCEscalation, PCEscalationAction, PCEscalationCategory } from '@/lib/types/operations';
import { EscalationTimer } from './EscalationTimer';
import { calculateSalesAidUrgency } from '@/lib/utils/escalation-helpers';

interface EscalationCardProps {
  escalation: PCEscalation;
  onAction: (escalationId: number, action: PCEscalationAction, data?: any) => void;
  onViewDetails?: (escalationId: number) => void;
}

export function EscalationCard({ escalation, onAction, onViewDetails }: EscalationCardProps) {
  const [showHistoryDialog, setShowHistoryDialog] = useState(false);
  const [showExtendDialog, setShowExtendDialog] = useState(false);
  const [showResolveDialog, setShowResolveDialog] = useState(false);
  const [showAssignDialog, setShowAssignDialog] = useState(false);
  const [showNotifyDialog, setShowNotifyDialog] = useState(false);
  
  // Form state
  const [selectedRepEmail, setSelectedRepEmail] = useState('');
  const [assignmentNote, setAssignmentNote] = useState('');
  const [newDeadline, setNewDeadline] = useState('');
  const [extendReason, setExtendReason] = useState('');
  const [resolutionNote, setResolutionNote] = useState('');
  const [customerPhone, setCustomerPhone] = useState('');
  const [customerMessage, setCustomerMessage] = useState('');

  const urgency = calculateSalesAidUrgency(escalation.rep72HourDeadline);

  const getCategoryIcon = (category: PCEscalationCategory) => {
    switch (category) {
      case 'mmu_required': return Zap;
      case 'rep_promises': return MessageSquareText;
      case 'hoa_issues': return Home;
      case 'financing_issues': return DollarSign;
      case 'customer_complaints': return AlertCircle;
      default: return AlertCircle;
    }
  };

  const getCategoryColor = (category: PCEscalationCategory): string => {
    switch (category) {
      case 'mmu_required': return 'border-l-blue-500';
      case 'rep_promises': return 'border-l-orange-500';
      case 'hoa_issues': return 'border-l-green-500';
      case 'financing_issues': return 'border-l-yellow-500';
      case 'customer_complaints': return 'border-l-red-500';
      default: return 'border-l-gray-500';
    }
  };

  const formatTimeWaiting = (dateCreated: string): string => {
    const now = new Date();
    const created = new Date(dateCreated);
    const diffDays = Math.floor((now.getTime() - created.getTime()) / (1000 * 60 * 60 * 24));
    
    if (diffDays === 0) return 'Escalated today';
    if (diffDays === 1) return 'Escalated 1 day ago';
    return `Escalated ${diffDays} days ago`;
  };

  const CategoryIcon = getCategoryIcon(escalation.category);

  return (
    <Card className={`p-3 md:p-4 hover:bg-gray-50 transition-colors ${getCategoryColor(escalation.category)}`}>
      <div className="flex items-start gap-4">
        {/* Left section: Category icon and urgency indicator */}
        <div className="flex flex-col items-center gap-2">
          <div className="p-2 rounded-full bg-gray-100">
            <CategoryIcon className="h-5 w-5 text-gray-600" />
          </div>
          <div className={`w-2 h-2 rounded-full ${
            urgency === 'critical' ? 'bg-red-500' : 
            urgency === 'high' ? 'bg-orange-500' : 
            'bg-blue-500'
          }`} />
        </div>

        {/* Main section: Escalation details */}
        <div className="flex-1 min-w-0">
          <div className="flex items-start justify-between mb-2">
            <div>
              <h3 className="font-semibold text-gray-900 truncate">
                Project #{escalation.relatedProject}
              </h3>
              {escalation.assignedEscalationRep && (
                <p className="text-sm text-gray-600">
                  Assigned to: {escalation.assignedEscalationRep}
                </p>
              )}
            </div>
            <Badge variant="outline" className="text-xs">
              {escalation.category.replace('_', ' ').toUpperCase()}
            </Badge>
          </div>

          <p className="text-sm text-gray-700 mb-2 line-clamp-2">
            {escalation.salesAidReason}
          </p>

          <div className="flex items-center gap-2 mb-2">
            <Badge variant="secondary" className="text-xs">
              {formatTimeWaiting(escalation.escalatedDateTime)}
            </Badge>
            {escalation.assignedEscalationRep && (
              <Badge variant="outline" className="text-xs">
                Assigned
              </Badge>
            )}
          </div>
        </div>

        {/* Timer section */}
        <div className="flex-shrink-0">
          <EscalationTimer 
            deadline={escalation.rep72HourDeadline} 
            urgency={urgency}
            compact={false}
          />
        </div>

        {/* Right section: Action buttons */}
        <div className="flex flex-col gap-2">
          <Button
            variant="outline"
            size="sm"
            onClick={() => setShowAssignDialog(true)}
            className="flex items-center gap-1"
          >
            <UserPlus className="h-4 w-4" />
            Assign
          </Button>

          <Button
            variant="outline"
            size="sm"
            onClick={() => setShowExtendDialog(true)}
            className="flex items-center gap-1"
          >
            <Clock className="h-4 w-4" />
            Extend
          </Button>

          <Button
            variant="default"
            size="sm"
            onClick={() => setShowResolveDialog(true)}
            className="flex items-center gap-1"
          >
            <CheckCircle className="h-4 w-4" />
            Resolve
          </Button>

          <Button
            variant="outline"
            size="sm"
            onClick={() => setShowNotifyDialog(true)}
            className="flex items-center gap-1"
          >
            <MessageSquare className="h-4 w-4" />
            Notify
          </Button>

          <Button
            variant="ghost"
            size="sm"
            onClick={() => setShowHistoryDialog(true)}
            className="flex items-center gap-1"
          >
            <History className="h-4 w-4" />
            History
          </Button>
        </div>
      </div>

      {/* Assign Dialog */}
      <AlertDialog open={showAssignDialog} onOpenChange={setShowAssignDialog}>
        <AlertDialogContent>
          <AlertDialogHeader>
            <AlertDialogTitle>Assign Escalation to Sales Aid</AlertDialogTitle>
          </AlertDialogHeader>
          <div className="space-y-4">
            <div>
              <label className="block text-sm font-medium mb-2">Sales Aid Rep</label>
              <select 
                className="w-full p-2 border rounded-md"
                value={selectedRepEmail}
                onChange={(e) => setSelectedRepEmail(e.target.value)}
              >
                <option value="">Select rep...</option>
                <option value="rep1@example.com">Rep 1</option>
                <option value="rep2@example.com">Rep 2</option>
              </select>
            </div>
            <div>
              <label className="block text-sm font-medium mb-2">Note (optional)</label>
              <textarea 
                className="w-full p-2 border rounded-md"
                rows={3}
                placeholder="Add assignment note..."
                value={assignmentNote}
                onChange={(e) => setAssignmentNote(e.target.value)}
              />
            </div>
            <div className="flex gap-2 justify-end">
              <Button variant="outline" onClick={() => setShowAssignDialog(false)}>
                Cancel
              </Button>
              <Button onClick={() => {
                onAction(escalation.recordId, 'assign', { 
                  repEmail: selectedRepEmail,
                  note: assignmentNote 
                });
                setShowAssignDialog(false);
              }}>
                Assign
              </Button>
            </div>
          </div>
        </AlertDialogContent>
      </AlertDialog>

      {/* Extend Grace Dialog */}
      <AlertDialog open={showExtendDialog} onOpenChange={setShowExtendDialog}>
        <AlertDialogContent>
          <AlertDialogHeader>
            <AlertDialogTitle>Extend Grace Period</AlertDialogTitle>
          </AlertDialogHeader>
          <div className="space-y-4">
            <div>
              <label className="block text-sm font-medium mb-2">New Deadline</label>
              <input 
                type="datetime-local"
                className="w-full p-2 border rounded-md"
                min={new Date().toISOString().slice(0, 16)}
                value={newDeadline}
                onChange={(e) => setNewDeadline(e.target.value)}
              />
            </div>
            <div>
              <label className="block text-sm font-medium mb-2">Reason</label>
              <textarea 
                className="w-full p-2 border rounded-md"
                rows={3}
                placeholder="Why is the grace period being extended?"
                required
                value={extendReason}
                onChange={(e) => setExtendReason(e.target.value)}
              />
            </div>
            <div className="flex gap-2 justify-end">
              <Button variant="outline" onClick={() => setShowExtendDialog(false)}>
                Cancel
              </Button>
              <Button onClick={() => {
                onAction(escalation.recordId, 'extend_grace', { 
                  newDeadline: newDeadline || new Date(Date.now() + 7 * 24 * 60 * 60 * 1000).toISOString(),
                  reason: extendReason || 'Additional time needed'
                });
                setShowExtendDialog(false);
              }}>
                Extend
              </Button>
            </div>
          </div>
        </AlertDialogContent>
      </AlertDialog>

      {/* Resolve Dialog */}
      <AlertDialog open={showResolveDialog} onOpenChange={setShowResolveDialog}>
        <AlertDialogContent>
          <AlertDialogHeader>
            <AlertDialogTitle>Resolve Escalation</AlertDialogTitle>
          </AlertDialogHeader>
          <div className="space-y-4">
            <div>
              <label className="block text-sm font-medium mb-2">Resolution Note</label>
              <textarea 
                className="w-full p-2 border rounded-md"
                rows={4}
                placeholder="Describe how the escalation was resolved..."
                required
                value={resolutionNote}
                onChange={(e) => setResolutionNote(e.target.value)}
              />
            </div>
            <div className="flex gap-2 justify-end">
              <Button variant="outline" onClick={() => setShowResolveDialog(false)}>
                Cancel
              </Button>
              <Button onClick={() => {
                onAction(escalation.recordId, 'resolve', { note: resolutionNote || 'Resolved via customer communication' });
                setShowResolveDialog(false);
              }}>
                Resolve
              </Button>
            </div>
          </div>
        </AlertDialogContent>
      </AlertDialog>

      {/* History Dialog */}
      <Dialog open={showHistoryDialog} onOpenChange={setShowHistoryDialog}>
        <DialogContent className="max-w-2xl">
          <DialogHeader>
            <DialogTitle>Escalation History</DialogTitle>
          </DialogHeader>
          <div className="space-y-4">
            {escalation.history.length > 0 ? (
              <div className="space-y-3">
                {escalation.history.map((item, index) => (
                  <div key={index} className="flex items-start gap-3 p-3 bg-gray-50 rounded-lg">
                    <div className="w-2 h-2 bg-blue-500 rounded-full mt-2" />
                    <div className="flex-1">
                      <div className="flex items-center gap-2 mb-1">
                        <span className="font-medium text-sm">{item.action}</span>
                        <span className="text-xs text-gray-500">
                          {new Date(item.timestamp).toLocaleString()}
                        </span>
                      </div>
                      <p className="text-sm text-gray-700">{item.details}</p>
                      <p className="text-xs text-gray-500">by {item.performedBy}</p>
                    </div>
                  </div>
                ))}
              </div>
            ) : (
              <p className="text-gray-500 text-center py-4">No history available</p>
            )}
          </div>
        </DialogContent>
      </Dialog>

      {/* Notify Customer Dialog */}
      <AlertDialog open={showNotifyDialog} onOpenChange={setShowNotifyDialog}>
        <AlertDialogContent>
          <AlertDialogHeader>
            <AlertDialogTitle>Notify Customer</AlertDialogTitle>
          </AlertDialogHeader>
          <div className="space-y-4">
            <div>
              <label className="block text-sm font-medium mb-2">Customer Phone</label>
              <input 
                type="tel"
                className="w-full p-2 border rounded-md"
                placeholder="Enter customer phone number"
                value={customerPhone}
                onChange={(e) => setCustomerPhone(e.target.value)}
                required
              />
            </div>
            <div>
              <label className="block text-sm font-medium mb-2">Message</label>
              <textarea 
                className="w-full p-2 border rounded-md"
                rows={4}
                placeholder="Enter message to send to customer..."
                value={customerMessage}
                onChange={(e) => setCustomerMessage(e.target.value)}
                required
              />
            </div>
            <div className="flex gap-2 justify-end">
              <Button variant="outline" onClick={() => setShowNotifyDialog(false)}>
                Cancel
              </Button>
              <Button onClick={() => {
                onAction(escalation.recordId, 'notify_customer', { 
                  customerPhone: customerPhone,
                  message: customerMessage 
                });
                setShowNotifyDialog(false);
              }}>
                Send Notification
              </Button>
            </div>
          </div>
        </AlertDialogContent>
      </AlertDialog>
    </Card>
  );
}

export function EscalationCardSkeleton() {
  return (
    <Card className="p-3 md:p-4">
      <div className="flex items-start gap-4">
        <Skeleton className="w-8 h-8 rounded-full" />
        <div className="flex-1 space-y-2">
          <Skeleton className="h-4 w-32" />
          <Skeleton className="h-3 w-48" />
          <Skeleton className="h-3 w-24" />
        </div>
        <div className="flex flex-col gap-2">
          <Skeleton className="h-8 w-16" />
          <Skeleton className="h-8 w-16" />
          <Skeleton className="h-8 w-16" />
        </div>
      </div>
    </Card>
  );
}
