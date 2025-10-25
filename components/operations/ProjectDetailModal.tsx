'use client'

import { useState } from 'react'
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query'
import { useSession } from 'next-auth/react'
import { toast } from 'sonner'
import {
  Dialog,
  DialogContent,
  DialogHeader,
  DialogTitle,
} from '@/components/ui/dialog'
import {
  AlertDialog,
  AlertDialogAction,
  AlertDialogCancel,
  AlertDialogContent,
  AlertDialogDescription,
  AlertDialogFooter,
  AlertDialogHeader,
  AlertDialogTitle,
} from '@/components/ui/alert-dialog'
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card'
import { Badge } from '@/components/ui/badge'
import { Avatar, AvatarFallback } from '@/components/ui/avatar'
import { Button } from '@/components/ui/button'
import { Separator } from '@/components/ui/separator'
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs'
import { Textarea } from '@/components/ui/textarea'
import { Label } from '@/components/ui/label'
import { Input } from '@/components/ui/input'
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from '@/components/ui/select'
import { Tooltip, TooltipContent, TooltipProvider, TooltipTrigger } from '@/components/ui/tooltip'
import { getBaseUrl } from '@/lib/utils/baseUrl'
import { getInitials, getAvatarColor, getAvatarTextColor } from '@/lib/utils/avatar'
import { Timeline } from '@/components/milestones/Timeline'
import { CommunicationLog } from './CommunicationLog'
import { TaskAssignment } from './TaskAssignment'
import { MessageThread } from './MessageThread'
import { PROJECT_FIELDS } from '@/lib/constants/fieldIds'
import type { PCProjectDetail, PCProjectAction, PCProjectActionResult, PCEscalationCategory, PCCreateEscalationPayload } from '@/lib/types/operations'
import {
  Phone,
  MessageSquare,
  Mail,
  FileText,
  CheckSquare,
  AlertTriangle,
  X,
  Calendar,
  MapPin,
  User,
  Download,
  Clock,
  CheckCircle,
  XCircle,
  AlertCircle
} from 'lucide-react'

interface ProjectDetailModalProps {
  recordId: number | null
  isOpen: boolean
  onClose: () => void
}

type ActiveTab = 'timeline' | 'communication' | 'team' | 'tasks' | 'documents'

export function ProjectDetailModal({ recordId, isOpen, onClose }: ProjectDetailModalProps) {
  const queryClient = useQueryClient()
  const { data: session } = useSession()
  const [activeTab, setActiveTab] = useState<ActiveTab>('timeline')
  const [showSmsDialog, setShowSmsDialog] = useState(false)
  const [smsMessage, setSmsMessage] = useState('')
  const [showNoteDialog, setShowNoteDialog] = useState(false)
  const [noteContent, setNoteContent] = useState('')
  const [customerPhone, setCustomerPhone] = useState('')
  const [customerName, setCustomerName] = useState('')
  const [showTaskMessages, setShowTaskMessages] = useState(false)
  const [selectedTaskId, setSelectedTaskId] = useState<number | null>(null)
  
  // Escalation state
  const [showEscalateDialog, setShowEscalateDialog] = useState(false)
  const [escalationCategory, setEscalationCategory] = useState<PCEscalationCategory | null>(null)
  const [escalationReason, setEscalationReason] = useState('')
  const [escalationDescription, setEscalationDescription] = useState('')
  const [escalationPriority, setEscalationPriority] = useState<'high' | 'normal'>('normal')

  // Fetch project details
  const { data: projectDetail, isLoading, error } = useQuery({
    queryKey: ['pc-project-detail', recordId],
    queryFn: async () => {
      if (!recordId) return null
      const response = await fetch(`${getBaseUrl()}/api/operations/projects/${recordId}`)
      if (!response.ok) throw new Error('Failed to fetch project details')
      return response.json() as Promise<PCProjectDetail>
    },
    enabled: !!recordId && isOpen,
    refetchOnWindowFocus: true
  })

  // Call mutation
  const callMutation = useMutation({
    mutationFn: async (data: { customerPhone: string; customerName: string }) => {
      const response = await fetch(`${getBaseUrl()}/api/operations/projects/${recordId}/call`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(data)
      })
      if (!response.ok) throw new Error('Failed to initiate call')
      return response.json() as Promise<PCProjectActionResult>
    },
    onSuccess: (result) => {
      toast.success('Call initiated successfully')
      // Invalidate project detail query to refresh data
      queryClient.invalidateQueries({ queryKey: ['pc-project-detail', recordId] })
    },
    onError: (error) => {
      toast.error(error.message || 'Failed to initiate call')
    }
  })

  // SMS mutation
  const smsMutation = useMutation({
    mutationFn: async (data: { customerPhone: string; message: string }) => {
      const response = await fetch(`${getBaseUrl()}/api/operations/projects/${recordId}/sms`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(data)
      })
      if (!response.ok) throw new Error('Failed to send SMS')
      return response.json() as Promise<PCProjectActionResult>
    },
    onSuccess: (result) => {
      toast.success('SMS sent successfully')
      setShowSmsDialog(false)
      setSmsMessage('')
      // Invalidate project detail query to refresh data
      queryClient.invalidateQueries({ queryKey: ['pc-project-detail', recordId] })
    },
    onError: (error) => {
      toast.error(error.message || 'Failed to send SMS')
    }
  })

  // Note mutation
  const noteMutation = useMutation({
    mutationFn: async (data: { noteContent: string }) => {
      const response = await fetch(`${getBaseUrl()}/api/operations/projects/${recordId}/note`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(data)
      })
      if (!response.ok) throw new Error('Failed to add note')
      return response.json() as Promise<PCProjectActionResult>
    },
    onSuccess: (result) => {
      toast.success('Note added successfully')
      setShowNoteDialog(false)
      setNoteContent('')
      // Invalidate project detail query to refresh data
      queryClient.invalidateQueries({ queryKey: ['pc-project-detail', recordId] })
    },
    onError: (error) => {
      toast.error(error.message || 'Failed to add note')
    }
  })

  // Escalation mutation
  const createEscalationMutation = useMutation({
    mutationFn: async (payload: PCCreateEscalationPayload) => {
      const response = await fetch('/api/operations/escalations', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(payload)
      })
      if (!response.ok) throw new Error('Failed to create escalation')
      return response.json()
    },
    onSuccess: () => {
      toast.success('Escalation created successfully')
      setShowEscalateDialog(false)
      // Reset form
      setEscalationCategory(null)
      setEscalationReason('')
      setEscalationDescription('')
      setEscalationPriority('normal')
    },
    onError: (error) => {
      toast.error('Failed to create escalation')
      console.error(error)
    }
  })

  // Handle quick actions
  const handleCall = () => {
    if (!projectDetail?.project) return
    
    const phone = projectDetail.project[PROJECT_FIELDS.CUSTOMER_PHONE] || '' // Customer phone field
    const name = projectDetail.project[PROJECT_FIELDS.CUSTOMER_NAME] || 'Customer' // Customer name field
    
    if (!phone) {
      toast.error('Customer phone number not available')
      return
    }

    setCustomerPhone(phone)
    setCustomerName(name)
    callMutation.mutate({ customerPhone: phone, customerName: name })
  }

  const handleSms = () => {
    if (!projectDetail?.project) return
    
    const phone = projectDetail.project[PROJECT_FIELDS.CUSTOMER_PHONE] || '' // Customer phone field
    const name = projectDetail.project[PROJECT_FIELDS.CUSTOMER_NAME] || 'Customer' // Customer name field
    
    if (!phone) {
      toast.error('Customer phone number not available')
      return
    }

    setCustomerPhone(phone)
    setCustomerName(name)
    setShowSmsDialog(true)
  }

  const handleEmail = () => {
    if (!projectDetail?.project) return
    
    const email = projectDetail.project[PROJECT_FIELDS.CUSTOMER_EMAIL] || '' // Customer email field
    const name = projectDetail.project[PROJECT_FIELDS.CUSTOMER_NAME] || 'Customer' // Customer name field
    
    if (!email) {
      toast.error('Customer email not available')
      return
    }

    window.open(`mailto:${email}?subject=Project Update - ${name}`, '_blank')
  }

  const handleAddNote = () => {
    setShowNoteDialog(true)
  }

  const handleSendSms = () => {
    if (!smsMessage.trim()) {
      toast.error('Message cannot be empty')
      return
    }

    smsMutation.mutate({ customerPhone, message: smsMessage.trim() })
  }

  const handleSaveNote = () => {
    if (!noteContent.trim()) {
      toast.error('Note cannot be empty')
      return
    }

    noteMutation.mutate({ noteContent: noteContent.trim() })
  }

  // Get project status and current stage
  const getProjectStatus = () => {
    if (!projectDetail?.project) return { status: 'Unknown', color: 'gray' }
    
    const status = projectDetail.project[PROJECT_FIELDS.PROJECT_STATUS] || 'Active' // Project status field
    const isOnHold = projectDetail.project[PROJECT_FIELDS.ON_HOLD] // On hold field
    
    if (isOnHold) return { status: 'On Hold', color: 'red' }
    if (status === 'Completed') return { status: 'Completed', color: 'green' }
    return { status: 'Active', color: 'blue' }
  }

  const getCurrentStage = () => {
    if (!projectDetail?.project) return 'Unknown'
    
    // Use milestone engine to determine current stage
    // This would need to be implemented based on the milestone engine
    return projectDetail.project[PROJECT_FIELDS.PROJECT_STAGE] || 'Intake' // Project stage field
  }

  const getDaysInStage = () => {
    if (!projectDetail?.project) return 0
    
    // Calculate days in current stage based on milestone dates
    const project = projectDetail.project
    const currentStage = getCurrentStage()
    const today = new Date()
    
    // Get the start date for the current stage based on milestone completion dates
    let stageStartDate: Date | null = null
    
    switch (currentStage) {
      case 'Intake':
        // Use sales date as start
        stageStartDate = project[PROJECT_FIELDS.SALES_DATE] ? new Date(project[PROJECT_FIELDS.SALES_DATE]) : null
        break
      case 'Survey':
        // Use intake completion date
        stageStartDate = project[PROJECT_FIELDS.INTAKE_COMPLETED_DATE] ? new Date(project[PROJECT_FIELDS.INTAKE_COMPLETED_DATE]) : null
        break
      case 'Design':
        // Use survey completion date
        stageStartDate = project[PROJECT_FIELDS.SURVEY_APPROVED] ? new Date(project[PROJECT_FIELDS.SURVEY_APPROVED]) : null
        break
      case 'NEM':
        // Use design completion date
        stageStartDate = project[PROJECT_FIELDS.DESIGN_COMPLETED] ? new Date(project[PROJECT_FIELDS.DESIGN_COMPLETED]) : null
        break
      case 'Permit':
        // Use NEM completion date
        stageStartDate = project[PROJECT_FIELDS.NEM_APPROVED] ? new Date(project[PROJECT_FIELDS.NEM_APPROVED]) : null
        break
      case 'Install':
        // Use permit completion date
        stageStartDate = project[PROJECT_FIELDS.PERMIT_APPROVED] ? new Date(project[PROJECT_FIELDS.PERMIT_APPROVED]) : null
        break
      case 'Inspection':
        // Use install completion date
        stageStartDate = project[PROJECT_FIELDS.INSTALL_COMPLETED_DATE] ? new Date(project[PROJECT_FIELDS.INSTALL_COMPLETED_DATE]) : null
        break
      case 'PTO':
        // Use inspection completion date
        stageStartDate = project[PROJECT_FIELDS.PASSING_INSPECTION_COMPLETED] ? new Date(project[PROJECT_FIELDS.PASSING_INSPECTION_COMPLETED]) : null
        break
    }
    
    if (!stageStartDate || isNaN(stageStartDate.getTime())) {
      return 0
    }
    
    const diffTime = today.getTime() - stageStartDate.getTime()
    const diffDays = Math.floor(diffTime / (1000 * 60 * 60 * 24))
    return Math.max(0, diffDays)
  }

  const getProjectedCompletion = (): Date | null => {
    if (!projectDetail?.project) return null
    
    const project = projectDetail.project
    const currentStage = getCurrentStage()
    
    // Define typical stage durations in days
    const stageDurations: Record<string, number> = {
      'Intake': 3,
      'Survey': 7,
      'Design': 14,
      'NEM': 21,
      'Permit': 14,
      'Install': 7,
      'Inspection': 3,
      'PTO': 14
    }
    
    // Get the most recent milestone date to calculate from
    let baseDate: Date | null = null
    
    // Find the most recent completed milestone date
    const milestoneDates = [
      project[PROJECT_FIELDS.INTAKE_COMPLETED_DATE],
      project[PROJECT_FIELDS.SURVEY_APPROVED],
      project[PROJECT_FIELDS.DESIGN_COMPLETED],
      project[PROJECT_FIELDS.NEM_APPROVED],
      project[PROJECT_FIELDS.PERMIT_APPROVED],
      project[PROJECT_FIELDS.INSTALL_COMPLETED_DATE],
      project[PROJECT_FIELDS.PASSING_INSPECTION_COMPLETED],
      project[PROJECT_FIELDS.PTO_APPROVED]
    ].filter(Boolean).map(date => new Date(date))
    
    if (milestoneDates.length > 0) {
      // Use the most recent milestone date
      baseDate = new Date(Math.max(...milestoneDates.map(d => d.getTime())))
    } else {
      // Fall back to sales date if no milestones completed
      baseDate = project[PROJECT_FIELDS.SALES_DATE] ? new Date(project[PROJECT_FIELDS.SALES_DATE]) : null
    }
    
    if (!baseDate || isNaN(baseDate.getTime())) {
      return null
    }
    
    // Calculate remaining stages and their durations
    const stageOrder = ['Intake', 'Survey', 'Design', 'NEM', 'Permit', 'Install', 'Inspection', 'PTO']
    const currentStageIndex = stageOrder.indexOf(currentStage)
    
    if (currentStageIndex === -1) {
      return null
    }
    
    // Sum up remaining stage durations
    let remainingDays = 0
    for (let i = currentStageIndex; i < stageOrder.length; i++) {
      const stage = stageOrder[i]
      remainingDays += stageDurations[stage] || 0
    }
    
    // Add some buffer for potential delays
    remainingDays += Math.ceil(remainingDays * 0.2) // 20% buffer
    
    // Calculate projected completion date
    const projectedDate = new Date(baseDate)
    projectedDate.setDate(projectedDate.getDate() + remainingDays)
    
    return projectedDate
  }

  if (isLoading) {
    return (
      <Dialog open={isOpen} onOpenChange={onClose}>
        <DialogContent className="max-w-6xl max-h-[90vh] overflow-y-auto">
          <div className="flex items-center justify-center h-64">
            <div className="animate-spin rounded-full h-8 w-8 border-b-2 border-gray-900"></div>
          </div>
        </DialogContent>
      </Dialog>
    )
  }

  if (error || !projectDetail) {
    return (
      <Dialog open={isOpen} onOpenChange={onClose}>
        <DialogContent className="max-w-6xl max-h-[90vh] overflow-y-auto">
          <div className="flex items-center justify-center h-64">
            <div className="text-center">
              <XCircle className="h-12 w-12 text-red-500 mx-auto mb-4" />
              <h3 className="text-lg font-semibold">Error loading project</h3>
              <p className="text-gray-600">Failed to load project details</p>
            </div>
          </div>
        </DialogContent>
      </Dialog>
    )
  }

  const { project, communications, documents, teamMembers } = projectDetail
  const projectStatus = getProjectStatus()
  const currentStage = getCurrentStage()
  const daysInStage = getDaysInStage()

  return (
    <>
      <Dialog open={isOpen} onOpenChange={onClose}>
        <DialogContent className="max-w-6xl max-h-[90vh] overflow-y-auto">
          <DialogHeader>
            <div className="flex items-center justify-between">
              <div className="flex-1">
                <DialogTitle className="text-2xl font-bold">
                  {project[PROJECT_FIELDS.CUSTOMER_NAME] || 'Unknown Customer'} {/* Customer name */}
                </DialogTitle>
                <div className="flex items-center gap-2 mt-1">
                  <MapPin className="h-4 w-4 text-gray-500" />
                  <span className="text-sm text-gray-600">
                    {project[PROJECT_FIELDS.CUSTOMER_ADDRESS] || 'Address not available'}
                  </span>
                </div>
                <div className="flex items-center gap-4 mt-2">
                  <span className="text-sm text-gray-600">Record ID: {recordId}</span>
                  <Badge variant={projectStatus.color === 'red' ? 'destructive' : projectStatus.color === 'green' ? 'default' : 'secondary'}>
                    {projectStatus.status}
                  </Badge>
                  <Badge variant="outline">{currentStage}</Badge>
                  <span className="text-sm text-gray-600">{daysInStage} days in stage</span>
                </div>
              </div>
              <Button variant="ghost" size="icon" onClick={onClose}>
                <X className="h-4 w-4" />
              </Button>
            </div>
          </DialogHeader>

          {/* Quick Actions Bar */}
          <div className="flex flex-wrap gap-2 py-4 border-b">
            <Button 
              onClick={handleCall}
              disabled={callMutation.isPending}
              className="flex items-center gap-2"
            >
              <Phone className="h-4 w-4" />
              Call
            </Button>
            <Button 
              onClick={handleSms}
              disabled={smsMutation.isPending}
              variant="outline"
              className="flex items-center gap-2"
            >
              <MessageSquare className="h-4 w-4" />
              Text
            </Button>
            <Button 
              onClick={handleEmail}
              variant="outline"
              className="flex items-center gap-2"
            >
              <Mail className="h-4 w-4" />
              Email
            </Button>
            <Button 
              onClick={handleAddNote}
              disabled={noteMutation.isPending}
              variant="outline"
              className="flex items-center gap-2"
            >
              <FileText className="h-4 w-4" />
              Add Note
            </Button>
            <Button 
              variant="outline" 
              className="flex items-center gap-2"
              onClick={() => setActiveTab('tasks')}
            >
              <CheckSquare className="h-4 w-4" />
              Assign Task
            </Button>
            <Button 
              variant="outline" 
              className="flex items-center gap-2"
              onClick={() => setShowEscalateDialog(true)}
            >
              <AlertTriangle className="h-4 w-4" />
              Escalate
            </Button>
          </div>

          {/* Tabs */}
          <Tabs value={activeTab} onValueChange={(value) => setActiveTab(value as ActiveTab)}>
            <TabsList className="grid w-full grid-cols-5">
              <TabsTrigger value="timeline">Timeline</TabsTrigger>
              <TabsTrigger value="communication">Communication</TabsTrigger>
              <TabsTrigger value="team">Team</TabsTrigger>
              <TabsTrigger value="tasks">Tasks</TabsTrigger>
              <TabsTrigger value="documents">Documents</TabsTrigger>
            </TabsList>

            <TabsContent value="timeline" className="mt-6">
              <Card>
                <CardHeader>
                  <CardTitle>Project Timeline</CardTitle>
                </CardHeader>
                <CardContent>
                  {/* Projected Completion Date */}
                  {(() => {
                    const projectedCompletion = getProjectedCompletion()
                    return projectedCompletion ? (
                      <div className="mb-4 p-3 bg-blue-50 border border-blue-200 rounded-lg">
                        <div className="flex items-center gap-2">
                          <Calendar className="h-4 w-4 text-blue-600" />
                          <span className="text-sm font-medium text-blue-900">
                            Projected Completion: {projectedCompletion.toLocaleDateString()}
                          </span>
                        </div>
                      </div>
                    ) : (
                      <div className="mb-4 p-3 bg-gray-50 border border-gray-200 rounded-lg">
                        <div className="flex items-center gap-2">
                          <Clock className="h-4 w-4 text-gray-500" />
                          <span className="text-sm text-gray-600">
                            Projected completion not available
                          </span>
                        </div>
                      </div>
                    )
                  })()}
                  <Timeline project={project} />
                </CardContent>
              </Card>
            </TabsContent>

            <TabsContent value="communication" className="mt-6">
              <Card>
                <CardHeader>
                  <CardTitle>Communication History</CardTitle>
                </CardHeader>
                <CardContent>
                  <CommunicationLog communications={communications} />
                </CardContent>
              </Card>
            </TabsContent>

            <TabsContent value="team" className="mt-6">
              <Card>
                <CardHeader>
                  <CardTitle>Team Members</CardTitle>
                </CardHeader>
                <CardContent>
                  <div className="grid gap-4">
                    {teamMembers.map((member, index) => (
                      <div key={index} className="flex items-center gap-4 p-4 border rounded-lg">
                        <Avatar className="h-10 w-10">
                          <AvatarFallback 
                            className={getAvatarColor(member.name)}
                            style={{ color: getAvatarTextColor(member.name) }}
                          >
                            {getInitials(member.name)}
                          </AvatarFallback>
                        </Avatar>
                        <div className="flex-1">
                          <div className="flex items-center gap-2">
                            <h4 className="font-semibold">{member.name}</h4>
                            <Badge variant="outline">{member.role}</Badge>
                          </div>
                          <p className="text-sm text-gray-600">{member.email}</p>
                          {member.phone && (
                            <p className="text-sm text-gray-600">{member.phone}</p>
                          )}
                        </div>
                        <div className="flex gap-2">
                          {member.phone && (
                            <Button 
                              size="sm" 
                              variant="outline"
                              onClick={(e) => {
                                e.stopPropagation()
                                if (member.phone) {
                                  setCustomerPhone(member.phone)
                                  setCustomerName(member.name)
                                  callMutation.mutate({ customerPhone: member.phone, customerName: member.name })
                                }
                              }}
                              disabled={callMutation.isPending}
                            >
                              <Phone className="h-4 w-4" />
                            </Button>
                          )}
                          <Button 
                            size="sm" 
                            variant="outline"
                            onClick={(e) => {
                              e.stopPropagation()
                              if (member.phone) {
                                setCustomerPhone(member.phone)
                                setCustomerName(member.name)
                                setShowSmsDialog(true)
                              }
                            }}
                            disabled={smsMutation.isPending}
                          >
                            <MessageSquare className="h-4 w-4" />
                          </Button>
                        </div>
                      </div>
                    ))}
                    {teamMembers.length === 0 && (
                      <div className="text-center py-8 text-gray-500">
                        No team members found
                      </div>
                    )}
                  </div>
                </CardContent>
              </Card>
            </TabsContent>

            <TabsContent value="tasks" className="mt-6">
              <TaskAssignment
                projectId={project?.[PROJECT_FIELDS.PROJECT_ID] || ''}
                recordId={recordId}
                customerName={project?.[PROJECT_FIELDS.CUSTOMER_NAME] || 'Customer'}
                salesRepEmail={project?.[PROJECT_FIELDS.CLOSER_EMAIL] || ''}
                salesRepName={project?.[PROJECT_FIELDS.CLOSER_NAME] || 'Sales Rep'}
                onTaskCreated={(taskId) => {
                  toast.success('Task assigned successfully')
                  setSelectedTaskId(taskId)
                  setShowTaskMessages(true)
                }}
              />
              
              {showTaskMessages && selectedTaskId && (
                <div className="mt-6">
                  <Card>
                    <CardHeader>
                      <div className="flex items-center justify-between">
                        <CardTitle>Task Messages</CardTitle>
                        <Button
                          variant="ghost"
                          size="sm"
                          onClick={() => {
                            setShowTaskMessages(false)
                            setSelectedTaskId(null)
                          }}
                        >
                          <X className="h-4 w-4" />
                        </Button>
                      </div>
                    </CardHeader>
                    <CardContent>
                      <MessageThread
                        projectId={project?.[PROJECT_FIELDS.PROJECT_ID] || ''}
                        recordId={recordId}
                        taskId={selectedTaskId}
                        currentUserEmail={session?.user?.email || ''}
                        currentUserRole="pc"
                      />
                    </CardContent>
                  </Card>
                </div>
              )}
            </TabsContent>

            <TabsContent value="documents" className="mt-6">
              <Card>
                <CardHeader>
                  <CardTitle>Documents</CardTitle>
                </CardHeader>
                <CardContent>
                  {documents.length > 0 ? (
                    <div className="grid gap-4">
                      {documents.map((doc, index) => (
                        <div key={index} className="flex items-center gap-4 p-4 border rounded-lg">
                          <div className="flex-1">
                            <h4 className="font-semibold">{doc.fileName}</h4>
                            <p className="text-sm text-gray-600">
                              {doc.documentType} • Uploaded {new Date(doc.uploadDate).toLocaleDateString()} by {doc.uploadedBy}
                            </p>
                          </div>
                          {doc.fileUrl && (
                            <Button size="sm" variant="outline">
                              <Download className="h-4 w-4" />
                            </Button>
                          )}
                        </div>
                      ))}
                    </div>
                  ) : (
                    <div className="text-center py-8 text-gray-500">
                      <FileText className="h-16 w-16 text-gray-400 mx-auto mb-4" />
                      <h3 className="text-lg font-semibold mb-2">No Documents Found</h3>
                      <p className="text-gray-600">
                        Document management coming soon
                      </p>
                    </div>
                  )}
                </CardContent>
              </Card>
            </TabsContent>
          </Tabs>
        </DialogContent>
      </Dialog>

      {/* SMS Dialog */}
      <AlertDialog open={showSmsDialog} onOpenChange={setShowSmsDialog}>
        <AlertDialogContent>
          <AlertDialogHeader>
            <AlertDialogTitle>Send SMS to {customerName}</AlertDialogTitle>
            <AlertDialogDescription>
              Send a text message to {customerPhone}
            </AlertDialogDescription>
          </AlertDialogHeader>
          <div className="space-y-4">
            <div>
              <Label htmlFor="sms-message">Message</Label>
              <Textarea
                id="sms-message"
                value={smsMessage}
                onChange={(e) => setSmsMessage(e.target.value)}
                placeholder="Enter your message..."
                maxLength={1600}
                rows={4}
              />
              <p className="text-sm text-gray-600 mt-1">
                {smsMessage.length}/1600 characters
              </p>
            </div>
          </div>
          <AlertDialogFooter>
            <AlertDialogCancel>Cancel</AlertDialogCancel>
            <AlertDialogAction 
              onClick={handleSendSms}
              disabled={smsMutation.isPending || !smsMessage.trim()}
            >
              {smsMutation.isPending ? 'Sending...' : 'Send SMS'}
            </AlertDialogAction>
          </AlertDialogFooter>
        </AlertDialogContent>
      </AlertDialog>

      {/* Add Note Dialog */}
      <AlertDialog open={showNoteDialog} onOpenChange={setShowNoteDialog}>
        <AlertDialogContent>
          <AlertDialogHeader>
            <AlertDialogTitle>Add Note to Project</AlertDialogTitle>
            <AlertDialogDescription>
              Add a note to this project's communication history
            </AlertDialogDescription>
          </AlertDialogHeader>
          <div className="space-y-4">
            <div>
              <Label htmlFor="note-content">Note</Label>
              <Textarea
                id="note-content"
                value={noteContent}
                onChange={(e) => setNoteContent(e.target.value)}
                placeholder="Enter your note..."
                maxLength={5000}
                rows={4}
              />
              <p className="text-sm text-gray-600 mt-1">
                {noteContent.length}/5000 characters
              </p>
            </div>
          </div>
          <AlertDialogFooter>
            <AlertDialogCancel>Cancel</AlertDialogCancel>
            <AlertDialogAction 
              onClick={handleSaveNote}
              disabled={noteMutation.isPending || !noteContent.trim()}
            >
              {noteMutation.isPending ? 'Saving...' : 'Save Note'}
            </AlertDialogAction>
          </AlertDialogFooter>
        </AlertDialogContent>
      </AlertDialog>

      {/* Escalate Dialog */}
      <AlertDialog open={showEscalateDialog} onOpenChange={setShowEscalateDialog}>
        <AlertDialogContent className="max-w-2xl">
          <AlertDialogHeader>
            <AlertDialogTitle>Create Escalation</AlertDialogTitle>
            <AlertDialogDescription>
              Create a new escalation for this project
            </AlertDialogDescription>
          </AlertDialogHeader>
          <div className="space-y-4">
            <div>
              <Label htmlFor="escalation-category">Category</Label>
              <Select 
                value={escalationCategory || ''} 
                onValueChange={(value) => setEscalationCategory(value as PCEscalationCategory)}
              >
                <SelectTrigger>
                  <SelectValue placeholder="Select category" />
                </SelectTrigger>
                <SelectContent>
                  <SelectItem value="mmu_required">MMU Required</SelectItem>
                  <SelectItem value="rep_promises">Rep Promises</SelectItem>
                  <SelectItem value="hoa_issues">HOA Issues</SelectItem>
                  <SelectItem value="financing_issues">Financing Issues</SelectItem>
                  <SelectItem value="customer_complaints">Customer Complaints</SelectItem>
                </SelectContent>
              </Select>
            </div>

            <div>
              <Label htmlFor="escalation-reason">Reason</Label>
              <Input
                id="escalation-reason"
                value={escalationReason}
                onChange={(e) => setEscalationReason(e.target.value)}
                placeholder="Enter escalation reason"
              />
            </div>

            <div>
              <Label htmlFor="escalation-description">Description</Label>
              <Textarea
                id="escalation-description"
                value={escalationDescription}
                onChange={(e) => setEscalationDescription(e.target.value)}
                placeholder="Describe the escalation in detail..."
                rows={4}
              />
            </div>

            <div>
              <Label>Priority</Label>
              <div className="flex gap-6">
                <div className="flex items-center space-x-2">
                  <input
                    type="radio"
                    id="normal-priority"
                    name="priority"
                    value="normal"
                    checked={escalationPriority === 'normal'}
                    onChange={(e) => setEscalationPriority(e.target.value as 'normal')}
                  />
                  <Label htmlFor="normal-priority">Normal (72 hours)</Label>
                </div>
                <div className="flex items-center space-x-2">
                  <input
                    type="radio"
                    id="high-priority"
                    name="priority"
                    value="high"
                    checked={escalationPriority === 'high'}
                    onChange={(e) => setEscalationPriority(e.target.value as 'high')}
                  />
                  <Label htmlFor="high-priority">High (48 hours)</Label>
                </div>
              </div>
            </div>
          </div>
          <AlertDialogFooter>
            <AlertDialogCancel>Cancel</AlertDialogCancel>
            <AlertDialogAction 
              onClick={() => {
                if (!escalationCategory || !escalationReason || !escalationDescription) {
                  toast.error('Please fill in all required fields')
                  return
                }
                createEscalationMutation.mutate({
                  projectId: recordId?.toString() || '',
                  recordId: recordId || 0,
                  reason: escalationReason,
                  category: escalationCategory,
                  description: escalationDescription,
                  priority: escalationPriority
                })
              }}
              disabled={createEscalationMutation.isPending || !escalationCategory || !escalationReason || !escalationDescription}
            >
              {createEscalationMutation.isPending ? 'Creating...' : 'Create Escalation'}
            </AlertDialogAction>
          </AlertDialogFooter>
        </AlertDialogContent>
      </AlertDialog>
    </>
  )
}
