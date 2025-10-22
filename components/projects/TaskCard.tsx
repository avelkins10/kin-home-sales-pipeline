'use client'

import React, { useState } from 'react'
import { useMutation, useQueryClient } from '@tanstack/react-query'
import { toast } from 'sonner'
import { CheckCircle, AlertTriangle, Clock, FileText, ChevronDown, ChevronUp, Upload, Loader2 } from 'lucide-react'
import { Task, TaskSubmission } from '@/lib/types/task'
import { formatDate } from '@/lib/utils/formatters'
import { TaskStatusBadge } from './TaskStatusBadge'
import { TaskCardSkeleton } from './TaskCardSkeleton'
import { FileUpload } from '@/components/ui/FileUpload'
import { Button } from '@/components/ui/button'
import { Textarea } from '@/components/ui/textarea'
import { Label } from '@/components/ui/label'
import { Alert, AlertDescription } from '@/components/ui/alert'
import { tasksKey, projectKey } from '@/lib/queryKeys'
import { cn } from '@/lib/utils/cn'
import { useIsMobile } from '@/lib/hooks/useMediaQuery'
import { getTaskRequirements, shouldShowFileUpload, getFileUploadLabel } from '@/lib/utils/task-requirements'

interface TaskCardProps {
  task: Task
  projectId: string | number
  className?: string
  isLoading?: boolean
}

export function TaskCard({ task, projectId, className, isLoading = false }: TaskCardProps) {
  const queryClient = useQueryClient()
  const isMobile = useIsMobile()
  const [isFormExpanded, setIsFormExpanded] = useState(false)
  const [selectedFile, setSelectedFile] = useState<File | null>(null)
  const [notes, setNotes] = useState('')
  const [uploadProgress, setUploadProgress] = useState(0)
  const [isSubmitting, setIsSubmitting] = useState(false)
  const [lastSubmissionError, setLastSubmissionError] = useState<string | null>(null)

  // Calculate task requirements based on name/category
  const taskRequirements = getTaskRequirements(task.name, task.category)
  const showFileUpload = shouldShowFileUpload(taskRequirements)
  const fileUploadLabel = getFileUploadLabel(taskRequirements)

  // Helper logic
  const canSubmit = task.status !== 'Complete'
  const needsRevisionSubmission = task.submissions?.find(
    sub => sub.opsDisposition === 'Needs Revision'
  )
  const needsResubmission = task.status === 'In Progress' && needsRevisionSubmission !== undefined
  const hasNeverSubmitted = !task.submissions || task.submissions.length === 0
  const latestSubmission = task.submissions?.[0]

  // Submission mutation
  const submitTaskMutation = useMutation({
    mutationFn: async () => {
      // Validate file requirement
      if (taskRequirements.requiresFile && !selectedFile) {
        throw new Error('Please select a file to upload')
      }

      const formData = new FormData()
      if (selectedFile) {
        formData.append('file', selectedFile)
      }
      if (notes.trim()) {
        formData.append('notes', notes.trim())
      }

      setIsSubmitting(true)
      setUploadProgress(0)

      try {
        const response = await fetch(`/api/projects/${projectId}/tasks/${task.recordId}/submit`, {
          method: 'POST',
          body: formData,
        })

        if (!response.ok) {
          const errorData = await response.json().catch(() => ({}))
          throw new Error(errorData.message || `HTTP ${response.status}: ${response.statusText}`)
        }

        const result = await response.json()
        setUploadProgress(100)
        return result
      } finally {
        setIsSubmitting(false)
      }
    },
    onMutate: async () => {
      // Cancel any outgoing refetches
      await queryClient.cancelQueries({ queryKey: tasksKey(projectId) })

      // Snapshot the previous value
      const previousTasks = queryClient.getQueryData(tasksKey(projectId))

      // Optimistically update to the new value
      const optimisticSubmission: TaskSubmission = {
        recordId: Date.now(), // Temporary ID
        dateCreated: new Date().toISOString(),
        relatedTask: task.recordId,
        submissionStatus: 'Pending Approval',
        opsDisposition: null,
        opsFeedback: null,
        fileAttachments: selectedFile?.name || '',
      }

      // Create optimistic task update
      const updatedTask = {
        ...task,
        submissions: [optimisticSubmission, ...(task.submissions || [])],
        status: task.status === 'Not Started' ? 'In Progress' : task.status,
      }

      // Update the query data
      queryClient.setQueryData(tasksKey(projectId), (old: any) => {
        if (!old) return old
        return old.map((group: any) => ({
          ...group,
          tasks: group.tasks.map((t: Task) => 
            t.recordId === task.recordId ? updatedTask : t
          ),
        }))
      })

      // Return a context object with the snapshotted value
      return { previousTasks }
    },
    onSuccess: () => {
      // Invalidate and refetch
      queryClient.invalidateQueries({ queryKey: tasksKey(projectId) })
      queryClient.invalidateQueries({ queryKey: projectKey(projectId) })
      
      // Show success toast
      toast.success('Task submitted successfully! Awaiting approval.')
      
      // Reset form state
      setSelectedFile(null)
      setNotes('')
      setIsFormExpanded(false)
      setUploadProgress(0)
      setLastSubmissionError(null)
    },
    onError: (error: Error, variables, context) => {
      // If the mutation fails, use the context returned from onMutate to roll back
      if (context?.previousTasks) {
        queryClient.setQueryData(tasksKey(projectId), context.previousTasks)
      }
      
      // Track the error for inline display
      setLastSubmissionError(error.message || 'Failed to submit task. Please try again.')
      setIsSubmitting(false)
    },
  })

  // Submission handler
  const handleSubmit = () => {
    if (taskRequirements.requiresFile && !selectedFile) {
      toast.error('Please select a file to upload')
      return
    }
    setLastSubmissionError(null)
    submitTaskMutation.mutate()
  }

  // Retry handler
  const handleRetry = () => {
    setLastSubmissionError(null)
    submitTaskMutation.mutate()
  }

  // Form toggle handler
  const handleToggleForm = () => {
    setIsFormExpanded(!isFormExpanded)
    if (isFormExpanded) {
      // Reset form state when collapsing
      setSelectedFile(null)
      setNotes('')
      setUploadProgress(0)
    }
  }

  // Show skeleton if loading
  if (isLoading) {
    return <TaskCardSkeleton className={className} />
  }

  return (
    <div className={cn(
      'rounded-lg border bg-white space-y-3 hover:border-gray-300 transition-colors',
      isMobile ? 'p-3' : 'p-4',
      className
    )}>
      {/* Header */}
      <div className={cn(
        'flex items-start justify-between',
        isMobile ? 'gap-2' : 'gap-3'
      )}>
        <div className="flex-1 min-w-0">
          <h4 
            id={`task-name-${task.recordId}`}
            className={cn(
              'font-medium text-gray-900',
              isMobile ? 'text-xs' : 'text-sm'
            )}
          >
            {task.name}
          </h4>
          {task.category && task.category !== task.name && (
            <p className="text-xs text-gray-500 mt-1">{task.category}</p>
          )}
        </div>
        <TaskStatusBadge status={task.status} />
      </div>

      {/* Submission History */}
      {task.submissions && task.submissions.length > 0 ? (
        <div className="space-y-2" role="region" aria-label="Submission history">
          <h5 className="text-xs font-medium text-gray-600">Submission History</h5>
          <div className="space-y-2">
            {task.submissions.map((submission, index) => (
              <div key={submission.recordId || `submission-${index}`} className="flex items-start gap-2 text-xs" role="listitem">
                <span className="text-gray-500 min-w-0 flex-shrink-0">
                  {formatDate(submission.dateCreated)}
                </span>
                <div className="flex flex-col gap-1">
                  <div className="flex items-center gap-1.5">
                    {submission.opsDisposition === 'Approved' ? (
                      <>
                        <CheckCircle className="w-3 h-3 text-green-600" />
                        <span className="text-green-700">Approved</span>
                      </>
                    ) : submission.opsDisposition === 'Needs Revision' ? (
                      <>
                        <AlertTriangle className="w-3 h-3 text-orange-600" />
                        <span className="text-orange-700">Needs Revision</span>
                      </>
                    ) : submission.submissionStatus === 'Pending Approval' ? (
                      <>
                        <Clock className="w-3 h-3 text-blue-600" />
                        <span className="text-blue-700">Pending Review</span>
                      </>
                    ) : (
                      <>
                        <FileText className="w-3 h-3 text-gray-600" />
                        <span className="text-gray-700">Submitted</span>
                      </>
                    )}
                  </div>
                  {submission.opsDisposition === 'Needs Revision' && submission.opsFeedback && (
                    <p className="text-xs text-orange-600 italic ml-4">
                      {submission.opsFeedback}
                    </p>
                  )}
                </div>
              </div>
            ))}
          </div>
        </div>
      ) : (
        <div className="text-center py-2">
          <FileText className="h-8 w-8 text-gray-300 mx-auto mb-2" />
          <p className="text-xs text-gray-500 italic">No submissions yet</p>
          <p className="text-xs text-gray-400 mt-1">Upload a file to get started</p>
        </div>
      )}

      {/* Ops Feedback */}
      {needsRevisionSubmission && (
        <div className="bg-orange-50 border border-orange-200 rounded-lg p-3">
          <div className="flex items-start gap-2">
            <AlertTriangle className="w-4 h-4 text-orange-600 flex-shrink-0 mt-0.5" />
            <div className="space-y-1">
              <h6 className="text-xs font-medium text-orange-800">Revision Requested</h6>
              {needsRevisionSubmission.opsFeedback && (
                <p className="text-xs text-orange-700 italic">
                  {needsRevisionSubmission.opsFeedback}
                </p>
              )}
            </div>
          </div>
        </div>
      )}

      {/* Task Submission Form */}
      {task.status === 'Complete' ? (
        <div className="bg-green-50 border border-green-200 rounded-lg p-3">
          <div className="flex items-center gap-2">
            <CheckCircle className="w-4 h-4 text-green-600" />
            <span className="text-sm font-medium text-green-800">✓ Task completed and approved</span>
          </div>
        </div>
      ) : canSubmit ? (
        <div className="space-y-3">
          {/* Form Toggle Button */}
          <Button
            onClick={handleToggleForm}
            onKeyDown={(e) => {
              if (e.key === 'Enter' || e.key === ' ') {
                e.preventDefault()
                handleToggleForm()
              }
            }}
            disabled={isSubmitting}
            variant={needsResubmission ? 'destructive' : hasNeverSubmitted ? 'default' : 'outline'}
            className="w-full flex items-center gap-2"
            aria-expanded={isFormExpanded}
            aria-controls={`submission-form-${task.recordId}`}
            aria-describedby={`task-name-${task.recordId}`}
            aria-label={
              isFormExpanded 
                ? `Collapse submission form for ${task.name}` 
                : needsResubmission 
                  ? `Resubmit ${task.name} with revisions` 
                  : `Submit ${task.name}`
            }
          >
            {isFormExpanded ? (
              <ChevronUp className="w-4 h-4" />
            ) : (
              <Upload className="w-4 h-4" />
            )}
            {needsResubmission
              ? 'Resubmit Task'
              : hasNeverSubmitted
              ? 'Submit Task'
              : 'Submit New Version'}
          </Button>

          {/* Collapsible Form */}
          {isFormExpanded && (
            <div
              id={`submission-form-${task.recordId}`}
              role="region"
              aria-label="Task submission form"
              className="bg-gray-50 border border-gray-200 rounded-lg p-4 space-y-4"
            >
              {/* File Upload Section - conditionally shown */}
              {showFileUpload && (
                <div className="space-y-2">
                  <Label htmlFor={`file-upload-${task.recordId}`} className="text-sm font-medium">
                    {fileUploadLabel}
                    {taskRequirements.requiresFile && (
                      <>
                        {' '}<span className="text-red-500">*</span>
                        <span className="sr-only">Required field</span>
                      </>
                    )}
                  </Label>
                  <FileUpload
                    id={`file-upload-${task.recordId}`}
                    onFileSelect={setSelectedFile}
                    uploadProgress={uploadProgress}
                    disabled={isSubmitting}
                  />
                  <p className="text-xs text-gray-500 italic">
                    {taskRequirements.reason}
                  </p>
                </div>
              )}

              {/* Notes Section */}
              <div className="space-y-2">
                <Label htmlFor="submission-notes" className="text-sm font-medium">
                  Notes (Optional)
                </Label>
                <Textarea
                  id="submission-notes"
                  placeholder="Add any notes or context for this submission..."
                  value={notes}
                  onChange={(e) => setNotes(e.target.value)}
                  disabled={isSubmitting}
                  className="min-h-[80px]"
                  aria-describedby="notes-hint"
                />
                <p id="notes-hint" className="text-xs text-gray-500">
                  Provide additional context or explanations for your submission
                </p>
              </div>

              {/* Action Buttons */}
              <div className="flex flex-col sm:flex-row gap-2">
                <Button
                  variant="outline"
                  onClick={handleToggleForm}
                  disabled={isSubmitting}
                  className="flex-1"
                >
                  Cancel
                </Button>
                <Button
                  onClick={handleSubmit}
                  disabled={(taskRequirements.requiresFile && !selectedFile) || isSubmitting}
                  className="flex-1"
                  aria-busy={isSubmitting}
                  aria-live="polite"
                >
                  {isSubmitting && <Loader2 className="h-4 w-4 mr-2 animate-spin" />}
                  {isSubmitting ? 'Submitting...' : 'Submit Task'}
                </Button>
              </div>
            </div>
          )}

          {/* Inline Error Alert */}
          {lastSubmissionError && (
            <Alert variant="destructive" role="alert" className="mt-3">
              <AlertTriangle className="h-4 w-4" />
              <AlertDescription className="flex items-center justify-between">
                <span>{lastSubmissionError}</span>
                <Button
                  variant="outline"
                  size="sm"
                  onClick={handleRetry}
                  disabled={isSubmitting}
                  className="ml-3"
                >
                  Retry
                </Button>
              </AlertDescription>
            </Alert>
          )}
        </div>
      ) : null}
    </div>
  )
}
