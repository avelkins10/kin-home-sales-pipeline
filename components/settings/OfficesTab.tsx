'use client'

import { useState } from 'react'
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query'
import {
  Card,
  CardHeader,
  CardTitle,
  CardContent,
} from '@/components/ui/card'
import { Input } from '@/components/ui/input'
import { Button } from '@/components/ui/button'
import { Badge } from '@/components/ui/badge'
import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from '@/components/ui/select'
import {
  Dialog,
  DialogContent,
  DialogDescription,
  DialogFooter,
  DialogHeader,
  DialogTitle,
  DialogTrigger,
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
import { Label } from '@/components/ui/label'
import { Switch } from '@/components/ui/switch'
import { Building2, Search, Users, Briefcase, Edit, Trash2, RefreshCw } from 'lucide-react'
import { toast } from 'sonner'
import { useRouter } from 'next/navigation'
import { Office, CreateOfficeInput, UpdateOfficeInput } from '@/lib/types/office'
import { User } from '@/lib/types/user'
import { getBaseUrl } from '@/lib/utils/baseUrl'
import BulkAssignOfficesDialog from './BulkAssignOfficesDialog'

export default function OfficesTab() {
  const [searchQuery, setSearchQuery] = useState('')
  const [isAddDialogOpen, setIsAddDialogOpen] = useState(false)
  const [isEditDialogOpen, setIsEditDialogOpen] = useState(false)
  const [isBulkAssignDialogOpen, setIsBulkAssignDialogOpen] = useState(false)
  const [officeToDelete, setOfficeToDelete] = useState<Office | null>(null)
  const [officeToEdit, setOfficeToEdit] = useState<Office | null>(null)
  const [newOffice, setNewOffice] = useState<CreateOfficeInput>({
    name: '',
    region: 'southwest',
    leaderId: 'none',
  })
  const [editOffice, setEditOffice] = useState<UpdateOfficeInput>({
    name: '',
    region: 'southwest',
    leaderId: 'none',
    isActive: true,
  })

  const router = useRouter()

  const queryClient = useQueryClient()

  // Fetch offices with search
  const { data: offices = [], isLoading } = useQuery({
    queryKey: ['offices', searchQuery],
    queryFn: async () => {
      const params = new URLSearchParams()
      if (searchQuery) params.append('search', searchQuery)

      const response = await fetch(`${getBaseUrl()}/api/admin/offices?${params}`)
      if (!response.ok) throw new Error('Failed to fetch offices')
      return response.json()
    },
  })

  // Fetch potential leaders
  const { data: potentialLeaders = [] } = useQuery({
    queryKey: ['potential-leaders'],
    queryFn: async () => {
      const response = await fetch(`${getBaseUrl()}/api/admin/users?role=office_leader,regional,super_admin`)
      if (!response.ok) throw new Error('Failed to fetch potential leaders')
      return response.json()
    },
  })

  // Create office mutation
  const createOfficeMutation = useMutation({
    mutationFn: async (officeData: CreateOfficeInput) => {
      const response = await fetch(`${getBaseUrl()}/api/admin/offices`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(officeData),
      })
      if (!response.ok) {
        const error = await response.json()
        throw new Error(error.message || 'Failed to create office')
      }
      return response.json()
    },
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['offices'] })
      setIsAddDialogOpen(false)
      setNewOffice({
        name: '',
        region: 'southwest',
        leaderId: 'none',
      })
      toast.success('Office created successfully')
    },
    onError: (error: Error) => {
      toast.error(error.message)
    },
  })

  // Sync offices from QuickBase mutation
  const syncOfficeMutation = useMutation({
    mutationFn: async () => {
      const response = await fetch(`${getBaseUrl()}/api/admin/offices/sync`, {
        method: 'POST',
      })
      if (!response.ok) {
        const error = await response.json()
        throw new Error(error.error || 'Failed to sync offices')
      }
      return response.json()
    },
    onSuccess: (data) => {
      queryClient.invalidateQueries({ queryKey: ['offices'] })
      toast.success(`Synced: ${data.created} created, ${data.skipped} skipped`)
    },
    onError: (error: Error) => {
      toast.error(error.message)
    },
  })

  // Update office mutation
  const updateOfficeMutation = useMutation({
    mutationFn: async ({ officeId, data }: { officeId: string; data: UpdateOfficeInput }) => {
      const response = await fetch(`${getBaseUrl()}/api/admin/offices/${officeId}`, {
        method: 'PATCH',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(data),
      })
      if (!response.ok) {
        const error = await response.json()
        throw new Error(error.error || 'Failed to update office')
      }
      return response.json()
    },
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['offices'] })
      setIsEditDialogOpen(false)
      setOfficeToEdit(null)
      toast.success('Office updated successfully')
    },
    onError: (error: Error) => {
      toast.error(error.message)
    },
  })

  // Delete office mutation
  const deleteOfficeMutation = useMutation({
    mutationFn: async (officeId: string) => {
      const response = await fetch(`${getBaseUrl()}/api/admin/offices/${officeId}`, {
        method: 'DELETE',
      })
      if (!response.ok) {
        const error = await response.json()
        throw new Error(error.message || 'Failed to delete office')
      }
      return response.json()
    },
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['offices'] })
      setOfficeToDelete(null)
      toast.success('Office deleted successfully')
    },
    onError: (error: Error) => {
      toast.error(error.message)
    },
  })

  const handleCreateOffice = () => {
    if (!newOffice.name || !newOffice.region) {
      toast.error('Name and region are required')
      return
    }
    createOfficeMutation.mutate(newOffice)
  }

  const handleUpdateOffice = () => {
    if (!officeToEdit || !editOffice.name || !editOffice.region) {
      toast.error('Name and region are required')
      return
    }
    updateOfficeMutation.mutate({ officeId: officeToEdit.id, data: editOffice })
  }

  const handleDeleteOffice = () => {
    if (officeToDelete) {
      deleteOfficeMutation.mutate(officeToDelete.id)
    }
  }

  const handleEditOffice = (office: Office) => {
    setOfficeToEdit(office)
    setEditOffice({
      name: office.name,
      region: office.region,
      leaderId: office.leaderId || 'none',
      isActive: office.isActive,
    })
    setIsEditDialogOpen(true)
  }

  return (
    <div className="space-y-6">
      {/* Header */}
      <div className="flex items-center justify-between">
        <div>
          <h2 className="text-2xl font-bold">Office Management</h2>
          <p className="text-gray-600">Manage sales offices and their leadership</p>
        </div>
        <div className="flex items-center space-x-2">
          <Button
            variant="outline"
            onClick={() => syncOfficeMutation.mutate()}
            disabled={syncOfficeMutation.isPending}
          >
            <RefreshCw className={`h-4 w-4 mr-2 ${syncOfficeMutation.isPending ? 'animate-spin' : ''}`} />
            {syncOfficeMutation.isPending ? 'Syncing...' : 'Sync from QuickBase'}
          </Button>
          <Button
            variant="outline"
            onClick={() => setIsBulkAssignDialogOpen(true)}
          >
            <Users className="h-4 w-4 mr-2" />
            Bulk Assign
          </Button>
          <Dialog open={isAddDialogOpen} onOpenChange={setIsAddDialogOpen}>
          <DialogTrigger asChild>
            <Button>
              <Building2 className="h-4 w-4 mr-2" />
              Add Office
            </Button>
          </DialogTrigger>
          <DialogContent className="sm:max-w-md">
            <DialogHeader>
              <DialogTitle>Add New Office</DialogTitle>
              <DialogDescription>
                Create a new sales office with region and leader assignment.
              </DialogDescription>
            </DialogHeader>
            <div className="space-y-4">
              <div>
                <Label htmlFor="officeName">Office Name *</Label>
                <Input
                  id="officeName"
                  value={newOffice.name}
                  onChange={(e) => setNewOffice({ ...newOffice, name: e.target.value })}
                  placeholder="Phoenix Office"
                />
              </div>
              <div>
                <Label htmlFor="region">Region *</Label>
                <Select value={newOffice.region} onValueChange={(value) => setNewOffice({ ...newOffice, region: value })}>
                  <SelectTrigger>
                    <SelectValue />
                  </SelectTrigger>
                  <SelectContent>
                    <SelectItem value="southwest">Southwest</SelectItem>
                    <SelectItem value="southeast">Southeast</SelectItem>
                    <SelectItem value="midwest">Midwest</SelectItem>
                    <SelectItem value="northeast">Northeast</SelectItem>
                    <SelectItem value="west">West</SelectItem>
                  </SelectContent>
                </Select>
              </div>
              <div>
                <Label htmlFor="leader">Office Leader</Label>
                <Select value={newOffice.leaderId || 'none'} onValueChange={(value) => setNewOffice({ ...newOffice, leaderId: value === 'none' ? '' : value })}>
                  <SelectTrigger>
                    <SelectValue placeholder="Select a leader" />
                  </SelectTrigger>
                  <SelectContent>
                    <SelectItem value="none">No leader</SelectItem>
                    {potentialLeaders.map((leader: User) => (
                      <SelectItem key={leader.id} value={leader.id}>
                        {leader.name} ({leader.role.replace('_', ' ')})
                      </SelectItem>
                    ))}
                  </SelectContent>
                </Select>
              </div>
            </div>
            <DialogFooter>
              <Button variant="outline" onClick={() => setIsAddDialogOpen(false)}>
                Cancel
              </Button>
              <Button 
                onClick={handleCreateOffice}
                disabled={createOfficeMutation.isPending}
              >
                {createOfficeMutation.isPending ? 'Creating...' : 'Create Office'}
              </Button>
            </DialogFooter>
          </DialogContent>
          </Dialog>
        </div>
      </div>

      {/* Search */}
      <div className="relative">
        <Search className="absolute left-3 top-1/2 transform -translate-y-1/2 h-4 w-4 text-gray-400" />
        <Input
          placeholder="Search offices by name or region..."
          value={searchQuery}
          onChange={(e) => setSearchQuery(e.target.value)}
          className="pl-10"
        />
      </div>

      {/* Offices Grid */}
      {isLoading ? (
        <div className="text-center py-8">
          Loading offices...
        </div>
      ) : offices.length === 0 ? (
        <div className="text-center py-8">
          <Building2 className="h-12 w-12 mx-auto text-gray-400 mb-4" />
          <h3 className="text-lg font-medium text-gray-900 mb-2">No offices found</h3>
          <p className="text-gray-600 mb-4">Create your first office to get started.</p>
        </div>
      ) : (
        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
          {offices.map((office: Office) => (
            <Card key={office.id} className="relative">
              <CardHeader className="pb-3">
                <div className="flex items-start justify-between">
                  <div className="flex items-center space-x-3">
                    <div className="p-2 bg-blue-100 rounded-full">
                      <Building2 className="h-5 w-5 text-blue-600" />
                    </div>
                    <div>
                      <CardTitle className="text-lg">{office.name}</CardTitle>
                      <Badge variant="outline" className="mt-1">
                        {office.region}
                      </Badge>
                    </div>
                  </div>
                  <div className="flex items-center space-x-1">
                    <Button 
                      variant="ghost" 
                      size="sm"
                      onClick={() => handleEditOffice(office)}
                    >
                      <Edit className="h-4 w-4" />
                    </Button>
                    <Button 
                      variant="ghost" 
                      size="sm"
                      onClick={() => setOfficeToDelete(office)}
                    >
                      <Trash2 className="h-4 w-4" />
                    </Button>
                  </div>
                </div>
              </CardHeader>
              <CardContent className="space-y-3">
                {/* Assigned Managers */}
                {office.assignedManagers && office.assignedManagers.length > 0 ? (
                  <div className="space-y-2">
                    <div className="flex items-center space-x-2">
                      <Users className="h-4 w-4 text-gray-500" />
                      <span className="text-sm font-medium text-gray-600">Assigned Managers:</span>
                    </div>
                    <div className="flex flex-wrap gap-1">
                      {office.assignedManagers.map((manager) => (
                        <Badge 
                          key={manager.userId} 
                          variant="secondary" 
                          className="cursor-pointer hover:bg-gray-200"
                          onClick={() => router.push(`/settings?tab=users&userId=${manager.userId}`)}
                        >
                          {manager.userName}
                          <span className="ml-1 text-xs opacity-75">({manager.accessLevel})</span>
                        </Badge>
                      ))}
                    </div>
                  </div>
                ) : (
                  <div className="flex items-center space-x-2">
                    <Users className="h-4 w-4 text-gray-500" />
                    <span className="text-sm text-gray-600">
                      Leader: {office.leaderName}
                    </span>
                  </div>
                )}
                <div className="flex items-center space-x-2">
                  <Users className="h-4 w-4 text-gray-500" />
                  <span className="text-sm text-gray-600">
                    Managers: {office.managerCount}
                  </span>
                </div>
                <div className="flex items-center space-x-2">
                  <Briefcase className="h-4 w-4 text-gray-500" />
                  <span className="text-sm text-gray-600">
                    Projects: {office.activeProjects} active
                  </span>
                </div>
              </CardContent>
            </Card>
          ))}
        </div>
      )}

      {/* Delete Confirmation Dialog */}
      <AlertDialog open={!!officeToDelete} onOpenChange={() => setOfficeToDelete(null)}>
        <AlertDialogContent>
          <AlertDialogHeader>
            <AlertDialogTitle>Delete Office</AlertDialogTitle>
            <AlertDialogDescription>
              Are you sure you want to delete &quot;{officeToDelete?.name}&quot;? This action cannot be undone.
              <br />
              <br />
              <strong>Warning:</strong> All users in this office will need to be reassigned.
            </AlertDialogDescription>
          </AlertDialogHeader>
          <AlertDialogFooter>
            <AlertDialogCancel>Cancel</AlertDialogCancel>
            <AlertDialogAction
              onClick={handleDeleteOffice}
              className="bg-red-600 hover:bg-red-700"
              disabled={deleteOfficeMutation.isPending}
            >
              {deleteOfficeMutation.isPending ? 'Deleting...' : 'Delete'}
            </AlertDialogAction>
          </AlertDialogFooter>
        </AlertDialogContent>
      </AlertDialog>

      {/* Edit Office Dialog */}
      <Dialog open={isEditDialogOpen} onOpenChange={setIsEditDialogOpen}>
        <DialogContent className="sm:max-w-md">
          <DialogHeader>
            <DialogTitle>Edit Office</DialogTitle>
            <DialogDescription>
              Update office information and leadership.
            </DialogDescription>
          </DialogHeader>
          <div className="space-y-4">
            <div>
              <Label htmlFor="editOfficeName">Office Name *</Label>
              <Input
                id="editOfficeName"
                value={editOffice.name}
                onChange={(e) => setEditOffice({ ...editOffice, name: e.target.value })}
                placeholder="Phoenix Office"
              />
            </div>
            <div>
              <Label htmlFor="editRegion">Region *</Label>
              <Select value={editOffice.region} onValueChange={(value) => setEditOffice({ ...editOffice, region: value })}>
                <SelectTrigger>
                  <SelectValue />
                </SelectTrigger>
                <SelectContent>
                  <SelectItem value="southwest">Southwest</SelectItem>
                  <SelectItem value="southeast">Southeast</SelectItem>
                  <SelectItem value="midwest">Midwest</SelectItem>
                  <SelectItem value="northeast">Northeast</SelectItem>
                  <SelectItem value="west">West</SelectItem>
                </SelectContent>
              </Select>
            </div>
            <div>
              <Label htmlFor="editLeader">Office Leader</Label>
              <Select value={editOffice.leaderId || 'none'} onValueChange={(value) => setEditOffice({ ...editOffice, leaderId: value === 'none' ? '' : value })}>
                <SelectTrigger>
                  <SelectValue placeholder="Select a leader" />
                </SelectTrigger>
                <SelectContent>
                  <SelectItem value="none">No leader</SelectItem>
                  {potentialLeaders.map((leader: User) => (
                    <SelectItem key={leader.id} value={leader.id}>
                      {leader.name} ({leader.role.replace('_', ' ')})
                    </SelectItem>
                  ))}
                </SelectContent>
              </Select>
            </div>
            <div className="flex items-center space-x-2">
              <Switch
                id="editIsActive"
                checked={editOffice.isActive}
                onCheckedChange={(checked) => setEditOffice({ ...editOffice, isActive: checked })}
              />
              <Label htmlFor="editIsActive">Active</Label>
            </div>
          </div>
          <DialogFooter>
            <Button variant="outline" onClick={() => setIsEditDialogOpen(false)}>
              Cancel
            </Button>
            <Button 
              onClick={handleUpdateOffice}
              disabled={updateOfficeMutation.isPending}
            >
              {updateOfficeMutation.isPending ? 'Updating...' : 'Update Office'}
            </Button>
          </DialogFooter>
        </DialogContent>
      </Dialog>

      {/* Bulk Assign Dialog */}
      <BulkAssignOfficesDialog
        open={isBulkAssignDialogOpen}
        onOpenChange={setIsBulkAssignDialogOpen}
      />
    </div>
  )
}
