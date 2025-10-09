'use client'

import { useState } from 'react'
import { useMutation, useQueryClient } from '@tanstack/react-query'
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

  const queryClient = useQueryClient()
  const { data: availableOffices = [] } = useAvailableOffices()

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
      onOpenChange(false)
      setFormData({
        name: '',
        email: '',
        role: 'closer',
        office: '',
        offices: [],
        sendEmail: true,
      })
      
      if (data.user?.inviteLink) {
        toast.success('Invite sent successfully!', {
          description: `Invite link: ${data.user.inviteLink}`,
          duration: 10000,
        })
      } else {
        toast.success('Invite sent successfully!')
      }
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

    inviteMutation.mutate(formData)
  }

  const handleOfficeChange = (office: string) => {
    setFormData(prev => ({
      ...prev,
      office,
      offices: office ? [office] : []
    }))
  }

  return (
    <Dialog open={open} onOpenChange={onOpenChange}>
      <DialogContent className="sm:max-w-md">
        <DialogHeader>
          <DialogTitle>Invite User</DialogTitle>
          <DialogDescription>
            Send an invite to create a new user account (recommended method).
          </DialogDescription>
        </DialogHeader>
        
        <form onSubmit={handleSubmit} className="space-y-4">
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
            <Label htmlFor="invite-email">Email *</Label>
            <Input
              id="invite-email"
              type="email"
              value={formData.email}
              onChange={(e) => setFormData(prev => ({ ...prev, email: e.target.value }))}
              placeholder="user@kinhome.com"
              required
            />
          </div>
          
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
                <SelectItem value="closer">Closer</SelectItem>
                <SelectItem value="setter">Setter</SelectItem>
                <SelectItem value="team_lead">Team Lead</SelectItem>
                <SelectItem value="office_leader">Office Leader</SelectItem>
                <SelectItem value="area_director">Area Director</SelectItem>
                <SelectItem value="divisional">Divisional</SelectItem>
                <SelectItem value="regional">Regional Manager</SelectItem>
                <SelectItem value="super_admin">Super Admin</SelectItem>
              </SelectContent>
            </Select>
          </div>
          
          <div>
            <Label htmlFor="invite-office">Office</Label>
            <Select 
              value={formData.office} 
              onValueChange={handleOfficeChange}
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
          
          <div className="flex items-center space-x-2">
            <Switch
              id="send-email"
              checked={formData.sendEmail}
              onCheckedChange={(checked) => setFormData(prev => ({ ...prev, sendEmail: checked }))}
            />
            <Label htmlFor="send-email">Send invite email</Label>
          </div>
          
          <DialogFooter>
            <Button 
              type="button" 
              variant="outline" 
              onClick={() => onOpenChange(false)}
            >
              Cancel
            </Button>
            <Button 
              type="submit"
              disabled={inviteMutation.isPending}
            >
              {inviteMutation.isPending ? 'Sending...' : 'Send Invite'}
            </Button>
          </DialogFooter>
        </form>
      </DialogContent>
    </Dialog>
  )
}
