'use client'

import { useState } from 'react'
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query'
import { useSession } from 'next-auth/react'
import {
  Dialog,
  DialogContent,
  DialogDescription,
  DialogFooter,
  DialogHeader,
  DialogTitle,
} from '@/components/ui/dialog'
import { Button } from '@/components/ui/button'
import { Input } from '@/components/ui/input'
import { Label } from '@/components/ui/label'
import { Switch } from '@/components/ui/switch'
import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from '@/components/ui/select'
import { Alert, AlertDescription } from '@/components/ui/alert'
import { Badge } from '@/components/ui/badge'
import { Copy, Info, Check, X, Search } from 'lucide-react'
import { OfficeMultiSelect } from './OfficeMultiSelect'
import { cn } from '@/lib/utils/cn'
import { toast } from 'sonner'
import { getBaseUrl } from '@/lib/utils/baseUrl'
import { useAvailableOffices } from '@/hooks/useAvailableOffices'

interface InviteUserDialogProps {
  open: boolean
  onOpenChange: (open: boolean) => void
}

interface InviteUserData {
  name: string
  email: string
  role: string
  office: string
  offices: string[]
  sendEmail: boolean
}

export function InviteUserDialog({ open, onOpenChange }: InviteUserDialogProps) {
  const [formData, setFormData] = useState<InviteUserData>({
    name: '',
    email: '',
    role: 'closer',
    office: '',
    offices: [],
    sendEmail: true,
  })
  const [lookupLoading, setLookupLoading] = useState(false)
  const [lookupData, setLookupData] = useState<any>(null)
  const [inviteResult, setInviteResult] = useState<{ inviteLink: string; emailSent: boolean } | null>(null)
  const [manages, setManages] = useState<string[]>([])
  const [copied, setCopied] = useState(false)

  const queryClient = useQueryClient()
  const { data: session } = useSession()
  const { data: availableOffices = [] } = useAvailableOffices()

  const { data: availableUsers = [] } = useQuery({
    queryKey: ['users', 'for-team-lead'],
    queryFn: async () => {
      const response = await fetch(`${getBaseUrl()}/api/admin/users`)
      if (!response.ok) throw new Error('Failed to fetch users')
      const users = await response.json()
      return users.filter((u: any) => ['closer', 'setter'].includes(u.role))
    },
    enabled: formData.role === 'team_lead'
  })

  const inviteMutation = useMutation({
    mutationFn: async (data: InviteUserData) => {
      const response = await fetch(`${getBaseUrl()}/api/admin/users/invite`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(data),
      })

      if (!response.ok) {
        const error = await response.json()
        throw new Error(error.error || 'Failed to send invite')
      }

      return response.json()
    },
    onSuccess: (data) => {
      queryClient.invalidateQueries({ queryKey: ['users'] })
      
      // Store invite result instead of closing dialog
      setInviteResult({
        inviteLink: data.user.inviteLink,
        emailSent: data.user.emailSent
      })
      
      toast.success('Invite sent! Copy the link below.')
    },
    onError: (error: Error) => {
      toast.error('Failed to send invite', {
        description: error.message,
      })
    },
  })

  const handleSubmit = (e: React.FormEvent) => {
    e.preventDefault()
    
    if (!formData.name || !formData.email) {
      toast.error('Name and email are required')
      return
    }
    
    // Validate office-based roles
    if (isOfficeBasedRole(formData.role) && (!formData.offices || formData.offices.length === 0)) {
      toast.error('Office-based roles must have at least one office assigned')
      return
    }
    
    // Team lead validation removed - manages will be assigned elsewhere
    
    // Build submit data based on role type
    const submitData = {
      ...formData,
      ...(isIndividualRole(formData.role) ? { offices: [] } : {}),
    }
    
    inviteMutation.mutate(submitData)
  }

  const handleOfficeChange = (office: string) => {
    setFormData(prev => ({
      ...prev,
      office,
      offices: office ? [office] : []
    }))
  }

  const isOfficeBasedRole = (role: string) => {
    return ['office_leader', 'area_director', 'divisional', 'regional'].includes(role)
  }

  const isTeamLeadRole = (role: string) => {
    return role === 'team_lead'
  }

  const isIndividualRole = (role: string) => {
    return ['closer', 'setter'].includes(role)
  }

  const getAvailableRoles = () => {
    const currentUserRole = session?.user?.role
    if (!currentUserRole) return []
    
    // Super admin can invite anyone
    if (currentUserRole === 'super_admin') {
      return [
        { value: 'closer', label: 'Closer' },
        { value: 'setter', label: 'Setter' },
        { value: 'team_lead', label: 'Team Lead' },
        { value: 'office_leader', label: 'Office Leader' },
        { value: 'area_director', label: 'Area Director' },
        { value: 'divisional', label: 'Divisional' },
        { value: 'regional', label: 'Regional Manager' },
        { value: 'super_admin', label: 'Super Admin' }
      ]
    }
    
    // Office leaders can only invite closers and setters
    if (currentUserRole === 'office_leader') {
      return [
        { value: 'closer', label: 'Closer' },
        { value: 'setter', label: 'Setter' }
      ]
    }
    
    // Other roles cannot invite anyone
    return []
  }

  const getActivityStatus = (lastProjectDate?: string) => {
    if (!lastProjectDate) return 'dormant'
    const date = new Date(lastProjectDate)
    const now = new Date()
    const monthsAgo = (now.getTime() - date.getTime()) / (1000 * 60 * 60 * 24 * 30)
    if (monthsAgo <= 6) return 'active'
    if (monthsAgo <= 12) return 'inactive'
    return 'dormant'
  }

  const formatRelativeDate = (dateString: string) => {
    const date = new Date(dateString)
    const now = new Date()
    const diffTime = Math.abs(now.getTime() - date.getTime())
    const diffDays = Math.ceil(diffTime / (1000 * 60 * 60 * 24))
    const diffMonths = Math.floor(diffDays / 30)
    
    if (diffMonths === 0) return 'less than a month ago'
    if (diffMonths === 1) return '1 month ago'
    return `${diffMonths} months ago`
  }

  const getActivityColor = (status: string) => {
    switch (status) {
      case 'active': return 'bg-green-500'
      case 'inactive': return 'bg-yellow-500'
      case 'dormant': return 'bg-red-500'
      default: return 'bg-gray-500'
    }
  }

  const handleQuickBaseLookup = async () => {
    if (!formData.email) {
      toast.error('Please enter an email address first')
      return
    }
    
    setLookupLoading(true)
    try {
      const response = await fetch(`${getBaseUrl()}/api/admin/users/lookup?search=${encodeURIComponent(formData.email)}`)
      
      if (!response.ok) {
        toast.error('User not found in QuickBase')
        setLookupData(null)
        return
      }
      
      const data = await response.json()
      
      if (data.users && data.users.length > 0) {
        const user = data.users[0]
        setLookupData({ user, activity: { totalProjects: 0, activeProjects: 0 } })
        
        // Auto-fill form data
        setFormData(prev => ({
          ...prev,
          name: user.name || prev.name,
          role: user.role || prev.role,
          office: user.office || prev.office,
          offices: user.office ? [user.office] : prev.offices
        }))
        
        toast.success('User found in QuickBase')
        
        // Fetch detailed activity
        if (user.quickbaseUserId) {
          const activityResponse = await fetch(`${getBaseUrl()}/api/admin/users/lookup?quickbaseId=${user.quickbaseUserId}`)
          if (activityResponse.ok) {
            const activityData = await activityResponse.json()
            setLookupData({ user, activity: activityData.activity })
          }
        }
      } else {
        toast.error('User not found in QuickBase')
        setLookupData(null)
      }
    } catch (error) {
      toast.error('Failed to lookup user in QuickBase')
      setLookupData(null)
    } finally {
      setLookupLoading(false)
    }
  }

  const copyToClipboard = async (text: string) => {
    try {
      await navigator.clipboard.writeText(text)
      setCopied(true)
      toast.success('Invite link copied to clipboard')
      setTimeout(() => setCopied(false), 2000)
      return true
    } catch (error) {
      toast.error('Failed to copy to clipboard')
      return false
    }
  }

  const handleReset = () => {
    setFormData({
      name: '',
      email: '',
      role: 'closer',
      office: '',
      offices: [],
      sendEmail: true,
    })
    setLookupData(null)
    setInviteResult(null)
    setManages([])
    setCopied(false)
  }

  const handleClose = () => {
    handleReset()
    onOpenChange(false)
  }

  return (
    <Dialog open={open} onOpenChange={onOpenChange}>
      <DialogContent className="sm:max-w-2xl max-h-[90vh] overflow-hidden">
        <DialogHeader>
          <DialogTitle>Invite User</DialogTitle>
          <DialogDescription>
            Send an invite to create a new user account (recommended method).
          </DialogDescription>
        </DialogHeader>
        
        <form onSubmit={handleSubmit} className="space-y-4 max-h-[60vh] overflow-y-auto pr-2">
          <div>
            <Label htmlFor="invite-name">Name *</Label>
            <Input
              id="invite-name"
              value={formData.name}
              onChange={(e) => setFormData(prev => ({ ...prev, name: e.target.value }))}
              placeholder="Full name"
              required
            />
          </div>
          
          <div>
            <div className="flex items-center justify-between mb-1">
              <Label htmlFor="invite-email">Email *</Label>
              <Button
                type="button"
                variant="ghost"
                size="sm"
                onClick={handleQuickBaseLookup}
                disabled={!formData.email || lookupLoading}
              >
                {lookupLoading ? (
                  <span className="h-4 w-4 animate-spin">⏳</span>
                ) : (
                  <Search className="h-4 w-4 mr-1" />
                )}
                Look up in QuickBase
              </Button>
            </div>
            <Input
              id="invite-email"
              type="email"
              value={formData.email}
              onChange={(e) => setFormData(prev => ({ ...prev, email: e.target.value }))}
              placeholder="user@kinhome.com"
              required
            />
          </div>
          
          {lookupData && (
            <Alert className="relative">
              <button
                onClick={() => setLookupData(null)}
                className="absolute top-2 right-2 rounded-full p-1 hover:bg-gray-200"
              >
                <X className="h-3 w-3" />
              </button>
              <AlertDescription>
                <div className="flex items-start gap-2">
                  <div className={cn('w-2 h-2 rounded-full mt-1.5', getActivityColor(getActivityStatus(lookupData.activity?.lastProjectDate)))} />
                  <div>
                    <p className="font-medium">✓ Found in QuickBase: {lookupData.user.name}</p>
                    {lookupData.activity?.lastProjectDate && (
                      <p className="text-sm text-gray-600 mt-1">
                        Last project: {formatRelativeDate(lookupData.activity.lastProjectDate)} • 
                        {lookupData.activity.totalProjects || 0} total projects • 
                        {lookupData.activity.activeProjects || 0} active
                      </p>
                    )}
                    {lookupData.activity?.offices && lookupData.activity.offices.length > 0 && (
                      <p className="text-sm text-gray-600 mt-1">
                        Offices: {lookupData.activity.offices.join(', ')}
                      </p>
                    )}
                  </div>
                </div>
              </AlertDescription>
            </Alert>
          )}
          
          <div>
            <Label htmlFor="invite-role">Role *</Label>
            <Select 
              value={formData.role} 
              onValueChange={(value) => setFormData(prev => ({ ...prev, role: value }))}
            >
              <SelectTrigger>
                <SelectValue />
              </SelectTrigger>
              <SelectContent>
                {getAvailableRoles().map((role) => (
                  <SelectItem key={role.value} value={role.value}>
                    {role.label}
                  </SelectItem>
                ))}
              </SelectContent>
            </Select>
          </div>
          
          {/* Role-specific fields */}
          {isIndividualRole(formData.role) && (
            <div>
              <Label htmlFor="invite-office">Office (Optional)</Label>
              <Select 
                value={formData.office} 
                onValueChange={(value) => setFormData(prev => ({ ...prev, office: value, offices: [] }))}
              >
                <SelectTrigger>
                  <SelectValue placeholder="Select office" />
                </SelectTrigger>
                <SelectContent>
                  <SelectItem value="">No office</SelectItem>
                  {availableOffices.map((office: string) => (
                    <SelectItem key={office} value={office}>
                      {office}
                    </SelectItem>
                  ))}
                </SelectContent>
              </Select>
            </div>
          )}

          {isOfficeBasedRole(formData.role) && (
            <div>
              <Label htmlFor="invite-offices">Offices *</Label>
              <OfficeMultiSelect
                value={formData.offices}
                onChange={(offices) => setFormData(prev => ({ ...prev, offices, office: offices[0] || '' }))}
                placeholder="Select offices..."
              />
              <Alert className="mt-2">
                <Info className="h-4 w-4" />
                <AlertDescription className="text-xs">
                  Office-based roles see ALL projects in their assigned offices, including projects from users without active accounts.
                </AlertDescription>
              </Alert>
            </div>
          )}

          {isTeamLeadRole(formData.role) && (
            <div>
              <Label htmlFor="invite-manages">Will Manage (Reps) *</Label>
              <Select 
                value="" 
                onValueChange={(userId) => {
                  if (!manages.includes(userId)) {
                    setManages([...manages, userId])
                  }
                }}
              >
                <SelectTrigger>
                  <SelectValue placeholder="Select reps to manage..." />
                </SelectTrigger>
                <SelectContent>
                  {availableUsers.map((user: any) => (
                    <SelectItem key={user.id} value={user.id}>
                      {user.name} ({user.role})
                    </SelectItem>
                  ))}
                </SelectContent>
              </Select>
              {manages.length > 0 && (
                <div className="flex flex-wrap gap-1 mt-2">
                  {manages.map((userId) => {
                    const user = availableUsers.find((u: any) => u.id === userId)
                    return user ? (
                      <Badge key={userId} variant="secondary" className="gap-1">
                        {user.name}
                        <button
                          onClick={() => setManages(manages.filter(id => id !== userId))}
                          className="ml-1 rounded-full hover:bg-gray-300"
                        >
                          <X className="h-3 w-3" />
                        </button>
                      </Badge>
                    ) : null
                  })}
                </div>
              )}
              <Alert className="mt-2">
                <Info className="h-4 w-4" />
                <AlertDescription className="text-xs">
                  Team leads see projects for their managed reps (both closer and setter projects).
                </AlertDescription>
              </Alert>
            </div>
          )}
          
          <div className="flex items-center space-x-2">
            <Switch
              id="send-email"
              checked={formData.sendEmail}
              onCheckedChange={(checked) => setFormData(prev => ({ ...prev, sendEmail: checked }))}
            />
            <Label htmlFor="send-email">Send invite email</Label>
          </div>
          
          {inviteResult && (
            <div className={`border rounded-lg p-4 space-y-3 ${
              inviteResult.emailSent
                ? 'bg-green-50 border-green-200'
                : 'bg-amber-50 border-amber-300'
            }`}>
              {inviteResult.emailSent ? (
                <>
                  <p className="font-medium text-green-900 flex items-center gap-2">
                    <Check className="h-5 w-5" />
                    Invite Email Sent Successfully!
                  </p>
                  <p className="text-sm text-green-700">
                    An invite email has been sent to <strong>{formData.email}</strong>.
                    They should receive it within a few minutes.
                  </p>
                </>
              ) : (
                <>
                  <p className="font-medium text-amber-900 flex items-center gap-2">
                    <Info className="h-5 w-5" />
                    Invite Created (Email Not Sent)
                  </p>
                  <p className="text-sm text-amber-800">
                    Email is not configured on this system. Copy the invite link below and share it manually with <strong>{formData.email}</strong>.
                  </p>
                </>
              )}
              <div className="space-y-2">
                <Label className="text-xs text-gray-600">Invite Link (valid for 7 days)</Label>
                <div className="flex gap-2">
                  <Input
                    value={inviteResult.inviteLink}
                    readOnly
                    className="flex-1 bg-white font-mono text-sm"
                  />
                  <Button
                    type="button"
                    variant="outline"
                    onClick={() => copyToClipboard(inviteResult.inviteLink)}
                    className="shrink-0"
                  >
                    {copied ? (
                      <>
                        <Check className="h-4 w-4 mr-2" />
                        Copied
                      </>
                    ) : (
                      <>
                        <Copy className="h-4 w-4 mr-2" />
                        Copy
                      </>
                    )}
                  </Button>
                </div>
              </div>
            </div>
          )}
          
          <DialogFooter>
            {inviteResult ? (
              <>
                <Button 
                  type="button" 
                  variant="outline" 
                  onClick={handleReset}
                >
                  Send Another Invite
                </Button>
                <Button 
                  type="button"
                  onClick={handleClose}
                >
                  Done
                </Button>
              </>
            ) : (
              <>
                <Button 
                  type="button" 
                  variant="outline" 
                  onClick={handleClose}
                >
                  Cancel
                </Button>
                <Button 
                  type="submit"
                  disabled={inviteMutation.isPending}
                >
                  {inviteMutation.isPending ? 'Sending...' : 'Send Invite'}
                </Button>
              </>
            )}
          </DialogFooter>
        </form>
      </DialogContent>
    </Dialog>
  )
}
