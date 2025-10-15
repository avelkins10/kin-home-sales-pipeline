'use client'

import React, { useState } from 'react'
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query'
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
import { X, Users, UserPlus, Trash2 } from 'lucide-react'
import { getBaseUrl } from '@/lib/utils/baseUrl'
import { cn } from '@/lib/utils/cn'
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

interface User {
  id: string
  name: string
  email: string
  role: string
}

interface AssignedManager {
  userId: string
  userName: string
  userRole: string
  accessLevel: string
}

interface Office {
  id: string
  name: string
  assignedManagers?: AssignedManager[]
}

interface ManageOfficeManagersDialogProps {
  open: boolean
  onOpenChange: (open: boolean) => void
  office: Office | null
}

export default function ManageOfficeManagersDialog({
  open,
  onOpenChange,
  office,
}: ManageOfficeManagersDialogProps) {
  const queryClient = useQueryClient()

  const [selectedManager, setSelectedManager] = useState<string>('')
  const [accessLevel, setAccessLevel] = useState<'view' | 'manage' | 'admin'>('manage')
  const [managerToRemove, setManagerToRemove] = useState<AssignedManager | null>(null)

  // Fetch potential managers
  const { data: managers = [], isLoading: isLoadingManagers } = useQuery({
    queryKey: ['managers-for-office-assign'],
    queryFn: async () => {
      const response = await fetch(`${getBaseUrl()}/api/admin/users?role=office_leader,area_director,divisional,regional,super_admin`)
      if (!response.ok) throw new Error('Failed to fetch managers')
      return response.json() as Promise<User[]>
    },
    enabled: open,
  })

  // Filter out already assigned managers
  const availableManagers = managers.filter(
    manager => !office?.assignedManagers?.some(am => am.userId === manager.id)
  )

  // Add manager mutation
  const addManagerMutation = useMutation({
    mutationFn: async (data: {
      userId: string
      officeName: string
      accessLevel: 'view' | 'manage' | 'admin'
    }) => {
      const response = await fetch(`${getBaseUrl()}/api/admin/office-assignments/bulk`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({
          userIds: [data.userId],
          officeNames: [data.officeName],
          accessLevel: data.accessLevel,
        }),
      })

      if (!response.ok) {
        const error = await response.json()
        throw new Error(error.error || 'Failed to add manager')
      }

      return response.json()
    },
    onSuccess: (data, variables) => {
      const manager = managers.find(m => m.id === variables.userId)
      toast.success('Manager added successfully!', {
        description: `${manager?.name} can now access ${office?.name} projects.`,
      })
      queryClient.invalidateQueries({ queryKey: ['offices'] })
      queryClient.invalidateQueries({ queryKey: ['users'] })
      setSelectedManager('')
      setAccessLevel('manage')
    },
    onError: (error: Error) => {
      toast.error('Failed to add manager', {
        description: error.message,
      })
    },
  })

  // Remove manager mutation
  const removeManagerMutation = useMutation({
    mutationFn: async (data: { userId: string; officeName: string }) => {
      const response = await fetch(`${getBaseUrl()}/api/admin/office-assignments`, {
        method: 'DELETE',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify(data),
      })

      if (!response.ok) {
        const error = await response.json()
        throw new Error(error.error || 'Failed to remove manager')
      }

      return response.json()
    },
    onSuccess: (data, variables) => {
      toast.success('Manager removed successfully!', {
        description: `Manager no longer has access to ${office?.name} projects.`,
      })
      queryClient.invalidateQueries({ queryKey: ['offices'] })
      queryClient.invalidateQueries({ queryKey: ['users'] })
      setManagerToRemove(null)
    },
    onError: (error: Error) => {
      toast.error('Failed to remove manager', {
        description: error.message,
      })
      setManagerToRemove(null)
    },
  })

  const handleAddManager = () => {
    if (!selectedManager || !office) {
      toast.error('Please select a manager')
      return
    }

    addManagerMutation.mutate({
      userId: selectedManager,
      officeName: office.name,
      accessLevel,
    })
  }

  const handleRemoveManager = (manager: AssignedManager) => {
    setManagerToRemove(manager)
  }

  const confirmRemoveManager = () => {
    if (!managerToRemove || !office) return

    removeManagerMutation.mutate({
      userId: managerToRemove.userId,
      officeName: office.name,
    })
  }

  const handleClose = () => {
    setSelectedManager('')
    setAccessLevel('manage')
    setManagerToRemove(null)
    onOpenChange(false)
  }

  if (!office) return null

  return (
    <>
      <Dialog open={open} onOpenChange={handleClose}>
        <DialogContent className="max-w-2xl">
          <DialogHeader>
            <DialogTitle className="flex items-center gap-2">
              <Users className="h-5 w-5" />
              Manage Managers for {office.name}
            </DialogTitle>
            <DialogDescription>
              Add or remove managers who can access this office&apos;s projects.
              All managers see ALL projects in this office, regardless of closer/setter.
            </DialogDescription>
          </DialogHeader>

          <div className="space-y-6">
            {/* Current Managers */}
            <div className="space-y-3">
              <Label className="text-base font-semibold">Current Managers</Label>
              {office.assignedManagers && office.assignedManagers.length > 0 ? (
                <div className="space-y-2">
                  {office.assignedManagers.map((manager) => (
                    <div
                      key={manager.userId}
                      className="flex items-center justify-between p-3 bg-gray-50 rounded-md"
                    >
                      <div className="flex items-center gap-3">
                        <Users className="h-4 w-4 text-gray-500" />
                        <div>
                          <div className="font-medium">{manager.userName}</div>
                          <div className="flex gap-2 mt-1">
                            <Badge variant="outline" className="text-xs">
                              {manager.userRole.replace('_', ' ')}
                            </Badge>
                            <Badge variant="secondary" className="text-xs">
                              {manager.accessLevel}
                            </Badge>
                          </div>
                        </div>
                      </div>
                      <Button
                        variant="ghost"
                        size="sm"
                        onClick={() => handleRemoveManager(manager)}
                        disabled={removeManagerMutation.isPending}
                        className="text-red-600 hover:text-red-700 hover:bg-red-50"
                      >
                        <Trash2 className="h-4 w-4" />
                      </Button>
                    </div>
                  ))}
                </div>
              ) : (
                <div className="text-sm text-gray-500 p-4 bg-gray-50 rounded-md text-center">
                  No managers assigned yet. Add managers below.
                </div>
              )}
            </div>

            {/* Add New Manager */}
            <div className="space-y-3 border-t pt-4">
              <Label className="text-base font-semibold flex items-center gap-2">
                <UserPlus className="h-4 w-4" />
                Add New Manager
              </Label>

              <div className="space-y-3">
                <div>
                  <Label>Select Manager</Label>
                  <Select
                    value={selectedManager}
                    onValueChange={setSelectedManager}
                    disabled={isLoadingManagers || availableManagers.length === 0}
                  >
                    <SelectTrigger>
                      <SelectValue placeholder="Choose a manager..." />
                    </SelectTrigger>
                    <SelectContent>
                      {availableManagers.map((manager) => (
                        <SelectItem key={manager.id} value={manager.id}>
                          <div className="flex items-center gap-2">
                            <span>{manager.name}</span>
                            <Badge variant="outline" className="text-xs">
                              {manager.role.replace('_', ' ')}
                            </Badge>
                          </div>
                        </SelectItem>
                      ))}
                    </SelectContent>
                  </Select>
                  {availableManagers.length === 0 && !isLoadingManagers && (
                    <p className="text-sm text-gray-500 mt-1">
                      All eligible managers are already assigned to this office.
                    </p>
                  )}
                </div>

                <div>
                  <Label>Access Level</Label>
                  <Select
                    value={accessLevel}
                    onValueChange={(value: 'view' | 'manage' | 'admin') => setAccessLevel(value)}
                  >
                    <SelectTrigger>
                      <SelectValue />
                    </SelectTrigger>
                    <SelectContent>
                      <SelectItem value="view">View - Read-only access</SelectItem>
                      <SelectItem value="manage">Manage - Standard access (recommended)</SelectItem>
                      <SelectItem value="admin">Admin - Full access</SelectItem>
                    </SelectContent>
                  </Select>
                  <p className="text-xs text-gray-500 mt-1">
                    Recommended: Use &apos;Manage&apos; for office leaders, &apos;Admin&apos; for area directors and above.
                  </p>
                </div>

                <Button
                  onClick={handleAddManager}
                  disabled={!selectedManager || addManagerMutation.isPending}
                  className="w-full"
                >
                  {addManagerMutation.isPending ? (
                    <>
                      <div className="animate-spin rounded-full h-4 w-4 border-b-2 border-white mr-2"></div>
                      Adding...
                    </>
                  ) : (
                    <>
                      <UserPlus className="h-4 w-4 mr-2" />
                      Add Manager
                    </>
                  )}
                </Button>
              </div>
            </div>
          </div>

          <DialogFooter>
            <Button variant="outline" onClick={handleClose}>
              Done
            </Button>
          </DialogFooter>
        </DialogContent>
      </Dialog>

      {/* Remove Manager Confirmation Dialog */}
      <AlertDialog open={!!managerToRemove} onOpenChange={(open) => !open && setManagerToRemove(null)}>
        <AlertDialogContent>
          <AlertDialogHeader>
            <AlertDialogTitle>Remove Manager Access?</AlertDialogTitle>
            <AlertDialogDescription>
              Are you sure you want to remove <strong>{managerToRemove?.userName}</strong> from{' '}
              <strong>{office.name}</strong>? They will no longer be able to see this office&apos;s projects.
            </AlertDialogDescription>
          </AlertDialogHeader>
          <AlertDialogFooter>
            <AlertDialogCancel disabled={removeManagerMutation.isPending}>
              Cancel
            </AlertDialogCancel>
            <AlertDialogAction
              onClick={confirmRemoveManager}
              disabled={removeManagerMutation.isPending}
              className="bg-red-600 hover:bg-red-700"
            >
              {removeManagerMutation.isPending ? (
                <>
                  <div className="animate-spin rounded-full h-4 w-4 border-b-2 border-white mr-2"></div>
                  Removing...
                </>
              ) : (
                'Remove Manager'
              )}
            </AlertDialogAction>
          </AlertDialogFooter>
        </AlertDialogContent>
      </AlertDialog>
    </>
  )
}
