'use client'

import { useState, useEffect } from 'react'
import { useMutation, useQueryClient } from '@tanstack/react-query'
import { AlertTriangle, CheckCircle, X, WifiOff, Wifi } from 'lucide-react'
import { Card, CardHeader, CardTitle, CardContent } from '@/components/ui/card'
import { Button } from '@/components/ui/button'
import { Textarea } from '@/components/ui/textarea'
import { Dialog, DialogContent, DialogHeader, DialogTitle, DialogTrigger, DialogFooter } from '@/components/ui/dialog'
import { Badge } from '@/components/ui/badge'
import { Alert, AlertDescription } from '@/components/ui/alert'
import { PROJECT_FIELDS } from '@/lib/constants/fieldIds'
import { QuickbaseProject } from '@/lib/types/project'
import { formatDate, formatDaysAgo } from '@/lib/utils/formatters'
import { toast } from 'sonner'
import { updateProjectHoldOffline, getPendingHoldUpdates } from '@/lib/offline/offlineMutations'

interface HoldManagementCardProps {
  project: QuickbaseProject
}

// Helper function to clean and parse hold reason from QuickBase
function parseHoldReason(rawReason: string): string {
  if (!rawReason) return ''

  // Known hold types to look for
  const holdTypes = [
    'Finance Hold',
    'Roof Hold',
    'HOA Hold',
    'Customer Hold',
    'Site Survey Hold',
    'Engineering Hold',
    'Permitting Hold',
    'Intake Hold',
    'Welcome Call Hold'
  ]

  // Check if any known hold type is in the text
  for (const type of holdTypes) {
    if (rawReason.includes(type)) {
      return type
    }
  }

  // Strip out URLs, placeholders, and common QuickBase artifacts
  let cleaned = rawReason
    .replace(/Placeholder:\/\/[^\s]+/gi, '') // Remove Placeholder:// URLs
    .replace(/https?:\/\/[^\s]+/gi, '') // Remove regular URLs
    .replace(/by:[\s\S]*$/i, '') // Remove "by: ..." signatures
    .split('\n')[0] // Take first line only
    .trim()

  // If what's left is just a number (record ID), return generic message
  if (/^\d+$/.test(cleaned)) {
    return 'Project on hold'
  }

  return cleaned || 'Project on hold'
}

export function HoldManagementCard({ project }: HoldManagementCardProps) {
  const queryClient = useQueryClient()

  // State
  const [holdReason, setHoldReason] = useState('')
  const [blockReason, setBlockReason] = useState('')
  const [isDialogOpen, setIsDialogOpen] = useState(false)
  const [isOffline, setIsOffline] = useState(!navigator.onLine)
  const [pendingCount, setPendingCount] = useState(0)
  const [isSubmitting, setIsSubmitting] = useState(false)

  // Extract project data
  const recordId = project[PROJECT_FIELDS.RECORD_ID]?.value
  const projectStatus = project[PROJECT_FIELDS.PROJECT_STATUS]?.value || ''
  const rawHoldReason = project[PROJECT_FIELDS.HOLD_REASON]?.value || ''
  const currentHoldReason = parseHoldReason(rawHoldReason) // Clean the hold reason
  const currentBlockReason = project[PROJECT_FIELDS.BLOCK_REASON]?.value || ''
  const dateOnHold = project[PROJECT_FIELDS.DATE_ON_HOLD]?.value
  const userWhoPlacedHold = project[PROJECT_FIELDS.USER_PLACED_ON_HOLD]?.value || ''

  // Check if project is in a terminal state (cannot be modified)
  const terminalStates = ['Rejected', 'Cancelled', 'Completed', 'Complete']
  const isTerminalState = terminalStates.includes(projectStatus)

  // Project is on hold if checkbox is checked OR there's a hold reason
  // This catches holds like "Finance Hold", "Roof Hold", "HOA Hold", "Customer Hold", etc.
  const onHold = project[PROJECT_FIELDS.ON_HOLD]?.value === 'Yes' || !!rawHoldReason

  // Online/offline listeners
  useEffect(() => {
    const handleOnline = () => setIsOffline(false)
    const handleOffline = () => setIsOffline(true)
    
    window.addEventListener('online', handleOnline)
    window.addEventListener('offline', handleOffline)
    
    // Update pending count
    getPendingHoldUpdates().then(setPendingCount)
    
    return () => {
      window.removeEventListener('online', handleOnline)
      window.removeEventListener('offline', handleOffline)
    }
  }, [])

  // Mutation for updating hold status
  const holdMutation = useMutation({
    mutationFn: async (data: { onHold: boolean; holdReason: string; blockReason: string }) => {
      setIsSubmitting(true)
      try {
        return await updateProjectHoldOffline(recordId, data)
      } finally {
        setIsSubmitting(false)
      }
    },
    onMutate: async (data) => {
      // Optimistic update
      const previousProject = queryClient.getQueryData(['project', recordId])
      
      if (previousProject) {
        const optimisticProject = {
          ...previousProject,
          [PROJECT_FIELDS.ON_HOLD]: { value: data.onHold ? 'Yes' : 'No' },
          [PROJECT_FIELDS.HOLD_REASON]: { value: data.holdReason },
          [PROJECT_FIELDS.BLOCK_REASON]: { value: data.blockReason },
          [PROJECT_FIELDS.DATE_ON_HOLD]: { value: data.onHold ? new Date().toISOString() : '' },
        }
        
        queryClient.setQueryData(['project', recordId], optimisticProject)
      }
      
      return { previousProject }
    },
    onSuccess: (result) => {
      // Invalidate queries to refetch data
      queryClient.invalidateQueries({ queryKey: ['projects'] })
      queryClient.invalidateQueries({ queryKey: ['project', recordId] })
      
      // Update pending count
      getPendingHoldUpdates().then(setPendingCount)
      
      if (result.queued) {
        toast.info('queued for sync')
      } else {
        toast.success('Hold status updated successfully')
      }
      
      setIsDialogOpen(false)
      setHoldReason('')
      setBlockReason('')
    },
    onError: (error: Error, data, context) => {
      // Rollback optimistic update
      if (context?.previousProject) {
        queryClient.setQueryData(['project', recordId], context.previousProject)
      }
      
      setIsSubmitting(false)
      toast.error(error.message || 'Failed to update hold status')
    },
  })

  const handlePlaceOnHold = () => {
    if (!holdReason.trim()) {
      toast.error('Hold reason is required')
      return
    }

    holdMutation.mutate({
      onHold: true,
      holdReason: holdReason.trim(),
      blockReason: blockReason.trim(),
    })
  }

  const handleReleaseHold = () => {
    holdMutation.mutate({
      onHold: false,
      holdReason: '',
      blockReason: '',
    })
  }

  const handleUpdateReason = () => {
    if (!holdReason.trim()) {
      toast.error('Hold reason is required')
      return
    }

    holdMutation.mutate({
      onHold: true,
      holdReason: holdReason.trim(),
      blockReason: blockReason.trim(),
    })
  }

  const openDialog = (isUpdate = false) => {
    if (isUpdate) {
      // When updating, use raw reason so user can see/edit the full original text
      setHoldReason(rawHoldReason)
      setBlockReason(currentBlockReason)
    } else {
      setHoldReason('')
      setBlockReason('')
    }
    setIsDialogOpen(true)
  }

  return (
    <Card id="hold-management">
      <CardHeader>
        <div className="flex items-center justify-between">
          <CardTitle className="text-xl font-semibold">Hold Management</CardTitle>
          <Badge
            variant={
              isTerminalState ? 'secondary' :
              onHold ? 'destructive' : 'secondary'
            }
            className={
              projectStatus === 'Rejected' ? 'bg-red-100 text-red-700 hover:bg-red-100' :
              projectStatus === 'Cancelled' ? 'bg-gray-400 text-gray-800 hover:bg-gray-400' :
              projectStatus === 'Completed' || projectStatus === 'Complete' ? 'bg-green-100 text-green-700 hover:bg-green-100' :
              ''
            }
          >
            {isTerminalState ? projectStatus : (onHold ? 'On Hold' : 'Active')}
          </Badge>
        </div>
      </CardHeader>
      <CardContent>
        {/* Offline indicator */}
        {isOffline && (
          <Alert className="mb-4 bg-amber-50 border-amber-200">
            <WifiOff className="h-4 w-4 text-amber-600" />
            <AlertDescription className="text-amber-800">
              You are offline. Changes will sync when reconnected.
            </AlertDescription>
          </Alert>
        )}
        
        {/* Pending updates indicator */}
        {pendingCount > 0 && (
          <Alert className="mb-4 bg-blue-50 border-blue-200">
            <Wifi className="h-4 w-4 text-blue-600" />
            <AlertDescription className="text-blue-800">
              {pendingCount} update(s) pending sync
            </AlertDescription>
          </Alert>
        )}

        {/* Terminal state message */}
        {isTerminalState ? (
          <div className="space-y-4">
            <div className={`rounded-lg border p-4 ${
              projectStatus === 'Rejected' ? 'bg-red-50 border-red-200' :
              projectStatus === 'Cancelled' ? 'bg-gray-50 border-gray-200' :
              'bg-green-50 border-green-200'
            }`}>
              <div className="flex items-start gap-3">
                <AlertTriangle className={`w-5 h-5 mt-0.5 flex-shrink-0 ${
                  projectStatus === 'Rejected' ? 'text-red-600' :
                  projectStatus === 'Cancelled' ? 'text-gray-600' :
                  'text-green-600'
                }`} />
                <div className="flex-1">
                  <h3 className={`text-sm font-medium ${
                    projectStatus === 'Rejected' ? 'text-red-800' :
                    projectStatus === 'Cancelled' ? 'text-gray-800' :
                    'text-green-800'
                  }`}>
                    Project {projectStatus}
                  </h3>
                  <p className={`text-sm mt-1 ${
                    projectStatus === 'Rejected' ? 'text-red-700' :
                    projectStatus === 'Cancelled' ? 'text-gray-700' :
                    'text-green-700'
                  }`}>
                    Hold management is not available for {projectStatus.toLowerCase()} projects.
                  </p>
                </div>
              </div>
            </div>

            {/* Show rejection reason if available */}
            {projectStatus === 'Rejected' && currentHoldReason && (
              <div className="text-sm space-y-2">
                <div className="flex justify-between">
                  <span className="text-gray-600">Reason:</span>
                  <span className="text-gray-900 font-medium">{currentHoldReason}</span>
                </div>
              </div>
            )}
          </div>
        ) : onHold ? (
          <div className="space-y-4">
            {/* Hold Alert */}
            <div className="bg-red-50 border border-red-200 rounded-lg p-4">
              <div className="flex items-start gap-3">
                <AlertTriangle className="w-5 h-5 text-red-600 mt-0.5 flex-shrink-0" />
                <div className="flex-1">
                  <h3 className="text-sm font-medium text-red-800">Project on Hold</h3>
                  <p className="text-sm text-red-700 mt-1 font-medium">
                    {currentHoldReason}
                  </p>
                  {currentBlockReason && (
                    <p className="text-sm text-red-600 mt-1">
                      {currentBlockReason}
                    </p>
                  )}
                </div>
              </div>
            </div>

            {/* Hold Details */}
            <div className="space-y-2 text-sm">
              {dateOnHold && (
                <div className="flex justify-between">
                  <span className="text-gray-600">Date placed on hold:</span>
                  <span className="text-gray-900">{formatDate(new Date(dateOnHold))}</span>
                </div>
              )}
              {dateOnHold && (
                <div className="flex justify-between">
                  <span className="text-gray-600">Days on hold:</span>
                  <span className="text-gray-900">{formatDaysAgo(new Date(dateOnHold))}</span>
                </div>
              )}
              {userWhoPlacedHold && (
                <div className="flex justify-between">
                  <span className="text-gray-600">Placed by:</span>
                  <span className="text-gray-900">{userWhoPlacedHold}</span>
                </div>
              )}
            </div>

            {/* Action Buttons */}
            <div className="flex flex-col sm:flex-row gap-2">
              <Button
                onClick={handleReleaseHold}
                disabled={isSubmitting}
                aria-busy={isSubmitting}
                className={`flex-1 bg-green-600 hover:bg-green-700 ${isOffline ? 'opacity-75' : ''}`}
              >
                <CheckCircle className="w-4 h-4 mr-2" />
                {isOffline ? 'Queue Release' : 'Release Hold'}
              </Button>
              <Button
                variant="outline"
                onClick={() => openDialog(true)}
                disabled={isSubmitting}
                aria-busy={isSubmitting}
                className={`flex-1 ${isOffline ? 'opacity-75' : ''}`}
              >
                Update Reason
              </Button>
            </div>
          </div>
        ) : (
          <div className="space-y-4">
            {/* Active Status */}
            <div className="text-center py-4">
              <CheckCircle className="w-12 h-12 text-green-600 mx-auto mb-2" />
              <p className="text-sm text-gray-600">Project is currently active</p>
            </div>

            {/* Place on Hold Button */}
            <Dialog open={isDialogOpen} onOpenChange={setIsDialogOpen}>
              <DialogTrigger asChild>
                <Button
                  variant="destructive"
                  onClick={() => openDialog(false)}
                  className={`w-full ${isOffline ? 'opacity-75' : ''}`}
                >
                  <AlertTriangle className="w-4 h-4 mr-2" />
                  {isOffline ? 'Queue Hold' : 'Place on Hold'}
                </Button>
              </DialogTrigger>
              <DialogContent className="max-w-md">
                <DialogHeader>
                  <DialogTitle>Place Project on Hold</DialogTitle>
                </DialogHeader>
                <div className="space-y-4">
                  <div>
                    <label htmlFor="hold-reason" className="block text-sm font-medium text-gray-700 mb-1">
                      Hold Reason *
                    </label>
                    <Textarea
                      id="hold-reason"
                      placeholder="Enter reason for placing project on hold..."
                      value={holdReason}
                      onChange={(e) => setHoldReason(e.target.value)}
                      className="min-h-[100px] resize-y"
                    />
                  </div>
                  <div>
                    <label htmlFor="block-reason" className="block text-sm font-medium text-gray-700 mb-1">
                      Block Reason (Optional)
                    </label>
                    <Textarea
                      id="block-reason"
                      placeholder="Enter additional blocking details..."
                      value={blockReason}
                      onChange={(e) => setBlockReason(e.target.value)}
                      className="min-h-[80px] resize-y"
                    />
                  </div>
                </div>
                <DialogFooter>
                  <Button
                    variant="outline"
                    onClick={() => setIsDialogOpen(false)}
                    disabled={isSubmitting}
                  >
                    Cancel
                  </Button>
                  <Button
                    onClick={handlePlaceOnHold}
                    disabled={isSubmitting || !holdReason.trim()}
                    aria-busy={isSubmitting}
                  >
                    {isSubmitting ? 'Placing...' : (isOffline ? 'Queue Hold' : 'Place on Hold')}
                  </Button>
                </DialogFooter>
              </DialogContent>
            </Dialog>
          </div>
        )}

        {/* Update Reason Dialog */}
        {onHold && (
          <Dialog open={isDialogOpen} onOpenChange={setIsDialogOpen}>
            <DialogContent className="max-w-md">
              <DialogHeader>
                <DialogTitle>Update Hold Reason</DialogTitle>
              </DialogHeader>
              <div className="space-y-4">
                <div>
                  <label htmlFor="update-hold-reason" className="block text-sm font-medium text-gray-700 mb-1">
                    Hold Reason *
                  </label>
                  <Textarea
                    id="update-hold-reason"
                    placeholder="Enter updated hold reason..."
                    value={holdReason}
                    onChange={(e) => setHoldReason(e.target.value)}
                    className="min-h-[100px] resize-y"
                  />
                </div>
                <div>
                  <label htmlFor="update-block-reason" className="block text-sm font-medium text-gray-700 mb-1">
                    Block Reason (Optional)
                  </label>
                  <Textarea
                    id="update-block-reason"
                    placeholder="Enter updated blocking details..."
                    value={blockReason}
                    onChange={(e) => setBlockReason(e.target.value)}
                    className="min-h-[80px] resize-y"
                  />
                </div>
              </div>
              <DialogFooter>
                <Button
                  variant="outline"
                  onClick={() => setIsDialogOpen(false)}
                  disabled={isSubmitting}
                >
                  Cancel
                </Button>
                <Button
                  onClick={handleUpdateReason}
                  disabled={isSubmitting || !holdReason.trim()}
                  aria-busy={isSubmitting}
                >
                  {isSubmitting ? 'Updating...' : (isOffline ? 'Queue Update' : 'Update Reason')}
                </Button>
              </DialogFooter>
            </DialogContent>
          </Dialog>
        )}
      </CardContent>
    </Card>
  )
}
