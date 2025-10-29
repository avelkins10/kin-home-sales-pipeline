'use client'

import { useState } from 'react'
import { useMutation, useQueryClient } from '@tanstack/react-query'
import { toast } from 'sonner'
import { AlertTriangle, Save, X, Loader2, Calendar, FileText } from 'lucide-react'
import { Button } from '@/components/ui/button'
import { Textarea } from '@/components/ui/textarea'
import { Label } from '@/components/ui/label'
import { Alert, AlertDescription } from '@/components/ui/alert'
import { cn } from '@/lib/utils/cn'

interface PendingCancelTaskCardProps {
  task: {
    projectId: number
    projectName: string
    customerName: string
    projectStatus: string
    closerName?: string | null
    salesOffice?: string | null
    cancelReason?: string | null
    dateMovedToPendingCancel?: string | null
  }
  className?: string
}

export function PendingCancelTaskCard({ task, className }: PendingCancelTaskCardProps) {
  const [showActions, setShowActions] = useState(false)
  const [notes, setNotes] = useState('')
  const [isSubmitting, setIsSubmitting] = useState(false)
  const queryClient = useQueryClient()

  // Check if project is already cancelled (not pending)
  const isPendingCancel = typeof task.projectStatus === 'string' && 
    task.projectStatus.toLowerCase().includes('pending cancel')
  const isCancelled = typeof task.projectStatus === 'string' && 
    (task.projectStatus.toLowerCase().includes('cancel') || task.projectStatus === 'Cancelled') &&
    !task.projectStatus.toLowerCase().includes('pending')

  const saveCustomerMutation = useMutation({
    mutationFn: async (data: { action: 'save' | 'cancel'; notes: string }) => {
      const response = await fetch(`/api/projects/${task.projectId}/save-or-cancel`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(data)
      })
      if (!response.ok) {
        const error = await response.json()
        throw new Error(error.message || 'Failed to update project')
      }
      return response.json()
    },
    onSuccess: (data, variables) => {
      toast.success(
        variables.action === 'save' 
          ? 'Customer saved! Project moved back to Active.'
          : 'Project cancelled successfully.'
      )
      setShowActions(false)
      setNotes('')
      // Invalidate tasks query to refresh the list
      queryClient.invalidateQueries({ queryKey: ['tasks', 'all'] })
      // Also invalidate projects query
      queryClient.invalidateQueries({ queryKey: ['projects'] })
    },
    onError: (error: Error) => {
      toast.error(error.message || 'Failed to update project')
      setIsSubmitting(false)
    }
  })

  const handleSave = async () => {
    if (!notes.trim()) {
      toast.error('Please provide notes explaining how you saved the customer')
      return
    }
    setIsSubmitting(true)
    saveCustomerMutation.mutate({ action: 'save', notes: notes.trim() })
  }

  const handleCancel = async () => {
    setIsSubmitting(true)
    saveCustomerMutation.mutate({ action: 'cancel', notes: notes.trim() })
  }

  return (
    <div className={cn(
      'rounded-lg border-2 border-orange-300 bg-orange-50 space-y-4',
      className
    )}>
      <Alert className={cn(
        isCancelled ? 'border-red-300 bg-red-100' : 'border-orange-300 bg-orange-100'
      )}>
        <AlertTriangle className={cn(
          'h-5 w-5',
          isCancelled ? 'text-red-600' : 'text-orange-600'
        )} />
        <AlertDescription className={cn(
          'font-medium',
          isCancelled ? 'text-red-900' : 'text-orange-900'
        )}>
          {isCancelled 
            ? `Project "${task.customerName}" has been cancelled`
            : `Project "${task.customerName}" is pending cancellation`
          }
        </AlertDescription>
      </Alert>

      <div className="px-4 pb-4 space-y-4">
        {isCancelled ? (
          <div>
            <h4 className="font-semibold text-gray-900 mb-1">Project Cancelled</h4>
            <p className="text-sm text-gray-600">
              This project has been officially cancelled. No further action is required.
            </p>
          </div>
        ) : (
          <>
            <div>
              <h4 className="font-semibold text-gray-900 mb-1">Save Customer or Cancel Project</h4>
              <p className="text-sm text-gray-600">
                This project is marked as "Pending Cancel". You can either save the customer (move back to Active) 
                or officially cancel the project.
              </p>
            </div>

            {/* Cancellation Details - Always show if pending cancel */}
            <div className="bg-white rounded-lg p-3 border border-gray-200 space-y-2">
              {task.dateMovedToPendingCancel ? (
                <div className="flex items-start gap-2 text-sm">
                  <Calendar className="w-4 h-4 text-gray-500 mt-0.5 flex-shrink-0" />
                  <div>
                    <span className="font-medium text-gray-700">Moved to Pending Cancel:</span>{' '}
                    <span className="text-gray-600">
                      {(() => {
                        try {
                          const dateValue = task.dateMovedToPendingCancel
                          if (!dateValue) return 'Date unavailable'
                          
                          // Handle QuickBase date format (MM-DD-YYYY or ISO string)
                          const date = typeof dateValue === 'string' && dateValue.includes('-') && !dateValue.includes('T')
                            ? new Date(dateValue.split(' ')[0].split('-').reverse().join('-')) // MM-DD-YYYY -> YYYY-MM-DD
                            : new Date(dateValue)
                          
                          if (isNaN(date.getTime())) return 'Date unavailable'
                          return date.toLocaleDateString('en-US', {
                            month: 'short',
                            day: 'numeric',
                            year: 'numeric',
                            hour: 'numeric',
                            minute: '2-digit'
                          })
                        } catch {
                          return 'Date unavailable'
                        }
                      })()}
                    </span>
                  </div>
                </div>
              ) : (
                <div className="flex items-start gap-2 text-sm text-gray-500">
                  <Calendar className="w-4 h-4 text-gray-400 mt-0.5 flex-shrink-0" />
                  <span>Date moved to pending cancel not available</span>
                </div>
              )}
              {task.cancelReason && (
                <div className="flex items-start gap-2 text-sm mt-2">
                  <FileText className="w-4 h-4 text-gray-500 mt-0.5 flex-shrink-0" />
                  <div>
                    <span className="font-medium text-gray-700">Cancellation Reason:</span>
                    <p className="text-gray-600 mt-1 whitespace-pre-wrap">{task.cancelReason}</p>
                  </div>
                </div>
              )}
            </div>

            {!showActions ? (
              <div className="flex gap-2">
                <Button
                  onClick={() => setShowActions(true)}
                  className="bg-green-600 hover:bg-green-700"
                >
                  <Save className="w-4 h-4 mr-2" />
                  Take Action
                </Button>
              </div>
            ) : (
          <div className="space-y-4 bg-white rounded-lg p-4 border border-gray-200">
            <div>
              <Label htmlFor="notes" className="text-sm font-medium text-gray-700">
                Notes {task.projectStatus?.toLowerCase().includes('pending cancel') && '(Required for Save)'}
              </Label>
              <Textarea
                id="notes"
                value={notes}
                onChange={(e) => setNotes(e.target.value)}
                placeholder={
                  task.projectStatus?.toLowerCase().includes('pending cancel')
                    ? 'Explain how you saved the customer or why the project is being cancelled...'
                    : 'Optional notes about the cancellation...'
                }
                className="mt-1 min-h-[100px]"
                disabled={isSubmitting}
              />
            </div>

            <div className="flex gap-2">
              <Button
                onClick={handleSave}
                disabled={isSubmitting || !notes.trim()}
                className="bg-green-600 hover:bg-green-700 flex-1"
              >
                {isSubmitting && saveCustomerMutation.variables?.action === 'save' ? (
                  <Loader2 className="w-4 h-4 mr-2 animate-spin" />
                ) : (
                  <Save className="w-4 h-4 mr-2" />
                )}
                Save Customer
              </Button>
              <Button
                onClick={handleCancel}
                disabled={isSubmitting}
                variant="destructive"
                className="flex-1"
              >
                {isSubmitting && saveCustomerMutation.variables?.action === 'cancel' ? (
                  <Loader2 className="w-4 h-4 mr-2 animate-spin" />
                ) : (
                  <X className="w-4 h-4 mr-2" />
                )}
                Cancel Project
              </Button>
            </div>

            <Button
              onClick={() => {
                setShowActions(false)
                setNotes('')
              }}
              variant="outline"
              className="w-full"
              disabled={isSubmitting}
            >
              Cancel
            </Button>
          </div>
            )}
          </>
        )}

        {/* Project Info */}
        <div className="text-xs text-gray-600 space-y-1 pt-2 border-t border-gray-200">
          <div><strong>Project:</strong> {task.projectName}</div>
          {task.closerName && <div><strong>Closer:</strong> {task.closerName}</div>}
          {task.salesOffice && <div><strong>Office:</strong> {task.salesOffice}</div>}
        </div>
      </div>
    </div>
  )
}

