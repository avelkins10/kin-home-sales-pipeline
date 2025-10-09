'use client'

import { useState } from 'react'
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query'
import { useRouter } from 'next/navigation'
import { toast } from 'sonner'
import {
  Dialog,
  DialogContent,
  DialogDescription,
  DialogFooter,
  DialogHeader,
  DialogTitle,
} from '@/components/ui/dialog'
import { Button } from '@/components/ui/button'
import { Label } from '@/components/ui/label'
import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from '@/components/ui/select'
import { Badge } from '@/components/ui/badge'
import { X, Users, Building2 } from 'lucide-react'
import { OfficeMultiSelect } from './OfficeMultiSelect'
import { getBaseUrl } from '@/lib/utils/baseUrl'
import { cn } from '@/lib/utils/cn'

interface User {
  id: string
  name: string
  email: string
  role: string
}

interface Office {
  id: string
  name: string
  region: string
  managerCount: number
}

interface BulkAssignOfficesDialogProps {
  open: boolean
  onOpenChange: (open: boolean) => void
}

export default function BulkAssignOfficesDialog({
  open,
  onOpenChange,
}: BulkAssignOfficesDialogProps) {
  const router = useRouter()
  const queryClient = useQueryClient()
  
  const [selectedManagers, setSelectedManagers] = useState<string[]>([])
  const [selectedOffices, setSelectedOffices] = useState<string[]>([])
  const [accessLevel, setAccessLevel] = useState<'view' | 'manage' | 'admin'>('view')

  // Fetch managers with appropriate roles
  const { data: managers = [], isLoading: isLoadingManagers } = useQuery({
    queryKey: ['managers-for-bulk-assign'],
    queryFn: async () => {
      const response = await fetch(`${getBaseUrl()}/api/admin/users?role=office_leader,area_director,divisional,regional,super_admin`)
      if (!response.ok) throw new Error('Failed to fetch managers')
      return response.json() as Promise<User[]>
    },
    enabled: open,
  })

  // Fetch offices
  const { data: offices = [], isLoading: isLoadingOffices } = useQuery({
    queryKey: ['offices'],
    queryFn: async () => {
      const response = await fetch(`${getBaseUrl()}/api/admin/offices`)
      if (!response.ok) throw new Error('Failed to fetch offices')
      return response.json() as Promise<Office[]>
    },
    enabled: open,
  })

  // Bulk assign mutation
  const bulkAssignMutation = useMutation({
    mutationFn: async (data: {
      userIds: string[]
      officeNames: string[]
      accessLevel: 'view' | 'manage' | 'admin'
    }) => {
      const response = await fetch(`${getBaseUrl()}/api/admin/office-assignments/bulk`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify(data),
      })

      if (!response.ok) {
        const error = await response.json()
        throw new Error(error.error || 'Failed to assign offices')
      }

      return response.json()
    },
    onSuccess: (data) => {
      toast.success(`Successfully assigned ${data.total} office assignments`)
      queryClient.invalidateQueries({ queryKey: ['offices'] })
      queryClient.invalidateQueries({ queryKey: ['users'] })
      handleClose()
    },
    onError: (error: Error) => {
      toast.error(error.message)
    },
  })

  const handleClose = () => {
    setSelectedManagers([])
    setSelectedOffices([])
    setAccessLevel('view')
    onOpenChange(false)
  }

  const handleSubmit = () => {
    if (selectedManagers.length === 0 || selectedOffices.length === 0) {
      toast.error('Please select at least one manager and one office')
      return
    }

    bulkAssignMutation.mutate({
      userIds: selectedManagers,
      officeNames: selectedOffices,
      accessLevel,
    })
  }

  const handleManagerToggle = (userId: string) => {
    setSelectedManagers(prev => 
      prev.includes(userId) 
        ? prev.filter(id => id !== userId)
        : [...prev, userId]
    )
  }

  const handleRemoveManager = (userId: string) => {
    setSelectedManagers(prev => prev.filter(id => id !== userId))
  }

  const handleClearAllManagers = () => {
    setSelectedManagers([])
  }

  const isValid = selectedManagers.length > 0 && selectedOffices.length > 0

  return (
    <Dialog open={open} onOpenChange={onOpenChange}>
      <DialogContent className="max-w-2xl">
        <DialogHeader>
          <DialogTitle>Bulk Assign Offices</DialogTitle>
          <DialogDescription>
            Assign multiple offices to multiple managers with the same access level.
          </DialogDescription>
        </DialogHeader>

        <div className="space-y-6">
          {/* Managers Selection */}
          <div className="space-y-3">
            <Label className="flex items-center gap-2">
              <Users className="h-4 w-4" />
              Select Managers
            </Label>
            
            <div className="space-y-2">
              <div className="border rounded-md p-3 max-h-48 overflow-y-auto">
                {isLoadingManagers ? (
                  <div className="text-center text-sm text-gray-500 py-4">
                    Loading managers...
                  </div>
                ) : managers.length === 0 ? (
                  <div className="text-center text-sm text-gray-500 py-4">
                    No managers found
                  </div>
                ) : (
                  <div className="space-y-2">
                    {managers.map((manager) => {
                      const isSelected = selectedManagers.includes(manager.id)
                      return (
                        <button
                          key={manager.id}
                          onClick={() => handleManagerToggle(manager.id)}
                          className={cn(
                            'flex w-full items-center justify-between rounded-sm px-2 py-2 text-sm hover:bg-gray-100',
                            isSelected && 'bg-gray-50'
                          )}
                        >
                          <div className="flex items-center gap-2">
                            <div
                              className={cn(
                                'flex h-4 w-4 items-center justify-center rounded border',
                                isSelected
                                  ? 'border-primary bg-primary text-white'
                                  : 'border-gray-300'
                              )}
                            >
                              {isSelected && <X className="h-3 w-3" />}
                            </div>
                            <div className="text-left">
                              <div className="font-medium">{manager.name}</div>
                              <div className="text-xs text-gray-500">{manager.email}</div>
                            </div>
                          </div>
                          <Badge variant="outline" className="text-xs">
                            {manager.role.replace('_', ' ')}
                          </Badge>
                        </button>
                      )
                    })}
                  </div>
                )}
              </div>

              {selectedManagers.length > 0 && (
                <div className="flex items-center justify-between">
                  <span className="text-sm text-gray-600">
                    {selectedManagers.length} manager{selectedManagers.length !== 1 ? 's' : ''} selected
                  </span>
                  <Button
                    variant="ghost"
                    size="sm"
                    onClick={handleClearAllManagers}
                  >
                    Clear All
                  </Button>
                </div>
              )}

              {/* Selected managers badges */}
              {selectedManagers.length > 0 && (
                <div className="flex flex-wrap gap-1">
                  {selectedManagers.map((userId) => {
                    const manager = managers.find(m => m.id === userId)
                    return (
                      <Badge key={userId} variant="secondary" className="gap-1">
                        {manager?.name}
                        <button
                          onClick={() => handleRemoveManager(userId)}
                          className="ml-1 rounded-full hover:bg-gray-300"
                        >
                          <X className="h-3 w-3" />
                        </button>
                      </Badge>
                    )
                  })}
                </div>
              )}
            </div>
          </div>

          {/* Offices Selection */}
          <div className="space-y-3">
            <Label className="flex items-center gap-2">
              <Building2 className="h-4 w-4" />
              Select Offices
            </Label>
            <OfficeMultiSelect
              value={selectedOffices}
              onChange={setSelectedOffices}
              placeholder="Select offices to assign..."
            />
          </div>

          {/* Access Level */}
          <div className="space-y-3">
            <Label>Access Level</Label>
            <Select value={accessLevel} onValueChange={(value: 'view' | 'manage' | 'admin') => setAccessLevel(value)}>
              <SelectTrigger>
                <SelectValue />
              </SelectTrigger>
              <SelectContent>
                <SelectItem value="view">View - Can view office data</SelectItem>
                <SelectItem value="manage">Manage - Can manage office operations</SelectItem>
                <SelectItem value="admin">Admin - Full administrative access</SelectItem>
              </SelectContent>
            </Select>
          </div>

          {/* Preview */}
          {isValid && (
            <div className="bg-gray-50 rounded-md p-4">
              <h4 className="font-medium text-sm mb-2">Assignment Preview</h4>
              <p className="text-sm text-gray-600">
                You are about to assign <strong>{selectedOffices.length}</strong> office{selectedOffices.length !== 1 ? 's' : ''} to{' '}
                <strong>{selectedManagers.length}</strong> manager{selectedManagers.length !== 1 ? 's' : ''} with{' '}
                <strong>{accessLevel}</strong> access level.
              </p>
              <p className="text-xs text-gray-500 mt-1">
                This will create {selectedManagers.length * selectedOffices.length} total assignments.
              </p>
            </div>
          )}
        </div>

        <DialogFooter>
          <Button variant="outline" onClick={handleClose}>
            Cancel
          </Button>
          <Button
            onClick={handleSubmit}
            disabled={!isValid || bulkAssignMutation.isPending}
          >
            {bulkAssignMutation.isPending ? 'Assigning...' : 'Assign Offices'}
          </Button>
        </DialogFooter>
      </DialogContent>
    </Dialog>
  )
}
