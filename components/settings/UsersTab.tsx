'use client'

import { useState } from 'react'
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query'
import {
  Table,
  TableBody,
  TableCell,
  TableHead,
  TableHeader,
  TableRow,
} from '@/components/ui/table'
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
import { Label } from '@/components/ui/label'
import { Switch } from '@/components/ui/switch'
import { UserPlus, Search, Edit, Key, Mail, Users, RefreshCw, UserMinus, Eye } from 'lucide-react'
import { toast } from 'sonner'
import { User, CreateUserInput } from '@/lib/types/user'
import { getBaseUrl } from '@/lib/utils/baseUrl'
import { InviteUserDialog } from './InviteUserDialog'
import { QuickBaseLookupDialog } from './QuickBaseLookupDialog'
import { SmartSyncDialog } from './SmartSyncDialog'
import { DeactivateInactiveDialog } from './DeactivateInactiveDialog'
import { HierarchyTreeView } from './HierarchyTreeView'

export default function UsersTab() {
  const [searchQuery, setSearchQuery] = useState('')
  const [roleFilter, setRoleFilter] = useState('all')
  const [officeFilter, setOfficeFilter] = useState('all')
  const [isAddDialogOpen, setIsAddDialogOpen] = useState(false)
  const [isInviteDialogOpen, setIsInviteDialogOpen] = useState(false)
  const [isLookupDialogOpen, setIsLookupDialogOpen] = useState(false)
  const [isSmartSyncDialogOpen, setIsSmartSyncDialogOpen] = useState(false)
  const [isDeactivateDialogOpen, setIsDeactivateDialogOpen] = useState(false)
  const [isHierarchyView, setIsHierarchyView] = useState(false)
  const [newUser, setNewUser] = useState<CreateUserInput>({
    name: '',
    email: '',
    phone: '',
    role: 'closer',
    quickbaseUserId: '',
    office: '',
    region: '',
    temporaryPassword: '',
  })

  const queryClient = useQueryClient()

  // Fetch users with filters
  const { data: users = [], isLoading } = useQuery({
    queryKey: ['users', searchQuery, roleFilter, officeFilter],
    queryFn: async () => {
      const params = new URLSearchParams()
      if (searchQuery) params.append('search', searchQuery)
      if (roleFilter !== 'all') params.append('role', roleFilter)
      if (officeFilter !== 'all') params.append('office', officeFilter)

      const response = await fetch(`${getBaseUrl()}/api/admin/users?${params}`)
      if (!response.ok) throw new Error('Failed to fetch users')
      return response.json()
    },
  })

  // Create user mutation
  const createUserMutation = useMutation({
    mutationFn: async (userData: CreateUserInput) => {
      const response = await fetch(`${getBaseUrl()}/api/admin/users`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(userData),
      })
      if (!response.ok) {
        const error = await response.json()
        throw new Error(error.message || 'Failed to create user')
      }
      return response.json()
    },
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['users'] })
      setIsAddDialogOpen(false)
      setNewUser({
        name: '',
        email: '',
        phone: '',
        role: 'closer',
        quickbaseUserId: '',
        office: '',
        region: '',
        temporaryPassword: '',
      })
      toast.success('User created successfully')
    },
    onError: (error: Error) => {
      toast.error(error.message)
    },
  })

  // Toggle active status mutation
  const toggleActiveMutation = useMutation({
    mutationFn: async ({ userId, isActive }: { userId: string; isActive: boolean }) => {
      const response = await fetch(`${getBaseUrl()}/api/admin/users/${userId}`, {
        method: 'PATCH',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ isActive }),
      })
      if (!response.ok) {
        const error = await response.json()
        throw new Error(error.message || 'Failed to update user')
      }
      return response.json()
    },
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['users'] })
      toast.success('User status updated')
    },
    onError: (error: Error) => {
      toast.error(error.message)
    },
  })

  // Reset password mutation
  const resetPasswordMutation = useMutation({
    mutationFn: async (userId: string) => {
      const response = await fetch(`${getBaseUrl()}/api/admin/users/${userId}/reset-password`, {
        method: 'POST',
      })
      if (!response.ok) {
        const error = await response.json()
        throw new Error(error.message || 'Failed to reset password')
      }
      return response.json()
    },
    onSuccess: (data) => {
      toast.success(`Temp password: ${data.temporaryPassword}`)
    },
    onError: (error: Error) => {
      toast.error(error.message)
    },
  })

  const getRoleBadgeVariant = (role: string) => {
    switch (role) {
      case 'super_admin':
        return 'destructive'
      case 'regional':
        return 'default'
      case 'office_leader':
        return 'secondary'
      default:
        return 'outline'
    }
  }

  const handleCreateUser = () => {
    if (!newUser.name || !newUser.email || !newUser.quickbaseUserId || !newUser.temporaryPassword) {
      toast.error('Name, email, Quickbase User ID, and temporary password are required')
      return
    }
    createUserMutation.mutate(newUser)
  }

  return (
    <div className="space-y-6">
      {/* Header */}
      <div className="flex items-center justify-between">
        <div>
          <h2 className="text-2xl font-bold">User Management</h2>
          <p className="text-gray-600">Manage user accounts and permissions</p>
        </div>
        <div className="flex items-center gap-2">
          <Button
            variant="outline"
            onClick={() => setIsHierarchyView(!isHierarchyView)}
          >
            <Eye className="h-4 w-4 mr-2" />
            {isHierarchyView ? 'Table View' : 'Hierarchy View'}
          </Button>
          <Button
            variant="outline"
            onClick={() => setIsSmartSyncDialogOpen(true)}
          >
            <RefreshCw className="h-4 w-4 mr-2" />
            Smart Sync
          </Button>
          <Button
            variant="outline"
            onClick={() => setIsDeactivateDialogOpen(true)}
          >
            <UserMinus className="h-4 w-4 mr-2" />
            Deactivate Inactive
          </Button>
          <Button 
            variant="outline"
            onClick={() => setIsLookupDialogOpen(true)}
          >
            <Search className="h-4 w-4 mr-2" />
            Add from QuickBase
          </Button>
          <Button 
            variant="outline"
            onClick={() => setIsInviteDialogOpen(true)}
          >
            <Mail className="h-4 w-4 mr-2" />
            Invite
          </Button>
          <Dialog open={isAddDialogOpen} onOpenChange={setIsAddDialogOpen}>
            <DialogTrigger asChild>
              <Button>
                <UserPlus className="h-4 w-4 mr-2" />
                Add User
              </Button>
            </DialogTrigger>
          <DialogContent className="sm:max-w-md">
            <DialogHeader>
              <DialogTitle>Add New User</DialogTitle>
              <DialogDescription>
                Create a new user account with role and office assignment.
              </DialogDescription>
            </DialogHeader>
            <div className="space-y-4">
              <div>
                <Label htmlFor="name">Name *</Label>
                <Input
                  id="name"
                  value={newUser.name}
                  onChange={(e) => setNewUser({ ...newUser, name: e.target.value })}
                  placeholder="Full name"
                />
              </div>
              <div>
                <Label htmlFor="email">Email *</Label>
                <Input
                  id="email"
                  type="email"
                  value={newUser.email}
                  onChange={(e) => setNewUser({ ...newUser, email: e.target.value })}
                  placeholder="user@kinhome.com"
                />
              </div>
              <div>
                <Label htmlFor="phone">Phone</Label>
                <Input
                  id="phone"
                  value={newUser.phone || ''}
                  onChange={(e) => setNewUser({ ...newUser, phone: e.target.value })}
                  placeholder="555-1234"
                />
              </div>
              <div>
                <Label htmlFor="role">Role *</Label>
                <Select value={newUser.role} onValueChange={(value) => setNewUser({ ...newUser, role: value })}>
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
                <Label htmlFor="quickbaseUserId">Quickbase User ID *</Label>
                <Input
                  id="quickbaseUserId"
                  value={newUser.quickbaseUserId}
                  onChange={(e) => setNewUser({ ...newUser, quickbaseUserId: e.target.value })}
                  placeholder="Quickbase user ID"
                />
              </div>
              <div>
                <Label htmlFor="office">Office</Label>
                <Input
                  id="office"
                  value={newUser.office || ''}
                  onChange={(e) => setNewUser({ ...newUser, office: e.target.value })}
                  placeholder="Phoenix"
                />
              </div>
              <div>
                <Label htmlFor="temporaryPassword">Temporary Password *</Label>
                <Input
                  id="temporaryPassword"
                  type="password"
                  value={newUser.temporaryPassword}
                  onChange={(e) => setNewUser({ ...newUser, temporaryPassword: e.target.value })}
                  placeholder="Minimum 8 characters"
                />
              </div>
            </div>
            <DialogFooter>
              <Button variant="outline" onClick={() => setIsAddDialogOpen(false)}>
                Cancel
              </Button>
              <Button 
                onClick={handleCreateUser}
                disabled={createUserMutation.isPending}
              >
                {createUserMutation.isPending ? 'Creating...' : 'Create User'}
              </Button>
            </DialogFooter>
          </DialogContent>
        </Dialog>
      </div>

      {/* Filters */}
      <div className="flex items-center gap-4">
        <div className="relative flex-1">
          <Search className="absolute left-3 top-1/2 transform -translate-y-1/2 h-4 w-4 text-gray-400" />
          <Input
            placeholder="Search users by name or email..."
            value={searchQuery}
            onChange={(e) => setSearchQuery(e.target.value)}
            className="pl-10"
          />
        </div>
        <Select value={roleFilter} onValueChange={setRoleFilter}>
          <SelectTrigger className="w-48">
            <SelectValue placeholder="Filter by role" />
          </SelectTrigger>
          <SelectContent>
            <SelectItem value="all">All Roles</SelectItem>
            <SelectItem value="closer">Closers</SelectItem>
            <SelectItem value="setter">Setters</SelectItem>
            <SelectItem value="team_lead">Team Leads</SelectItem>
            <SelectItem value="office_leader">Office Leaders</SelectItem>
            <SelectItem value="area_director">Area Directors</SelectItem>
            <SelectItem value="divisional">Divisional</SelectItem>
            <SelectItem value="regional">Regional</SelectItem>
            <SelectItem value="super_admin">Super Admin</SelectItem>
          </SelectContent>
        </Select>
        <Select value={officeFilter} onValueChange={setOfficeFilter}>
          <SelectTrigger className="w-48">
            <SelectValue placeholder="Filter by office" />
          </SelectTrigger>
          <SelectContent>
            <SelectItem value="all">All Offices</SelectItem>
            <SelectItem value="Phoenix">Phoenix</SelectItem>
            <SelectItem value="Austin">Austin</SelectItem>
            <SelectItem value="Dallas">Dallas</SelectItem>
            <SelectItem value="Houston">Houston</SelectItem>
          </SelectContent>
        </Select>
      </div>

      {/* Main Content */}
      {isHierarchyView ? (
        <HierarchyTreeView />
      ) : (
        /* Users Table */
        <div className="border rounded-lg">
          <Table>
            <TableHeader>
              <TableRow>
                <TableHead>Name</TableHead>
                <TableHead>Email</TableHead>
                <TableHead>Role</TableHead>
                <TableHead>Office</TableHead>
                <TableHead>Status</TableHead>
                <TableHead>Last Login</TableHead>
                <TableHead>Actions</TableHead>
              </TableRow>
            </TableHeader>
            <TableBody>
              {isLoading ? (
                <TableRow>
                  <TableCell colSpan={7} className="text-center py-8">
                    Loading users...
                  </TableCell>
                </TableRow>
              ) : users.length === 0 ? (
                <TableRow>
                  <TableCell colSpan={7} className="text-center py-8">
                    No users found
                  </TableCell>
                </TableRow>
              ) : (
                users.map((user: User) => (
                  <TableRow key={user.id}>
                    <TableCell className="font-medium">{user.name}</TableCell>
                    <TableCell>{user.email}</TableCell>
                    <TableCell>
                      <Badge variant={getRoleBadgeVariant(user.role)}>
                        {user.role.replace('_', ' ')}
                      </Badge>
                    </TableCell>
                    <TableCell>{user.office || '-'}</TableCell>
                    <TableCell>
                      <Switch
                        checked={user.isActive}
                        onCheckedChange={(checked) =>
                          toggleActiveMutation.mutate({ userId: user.id, isActive: checked })
                        }
                      />
                    </TableCell>
                    <TableCell>
                      {user.lastLoginAt 
                        ? new Date(user.lastLoginAt).toLocaleDateString()
                        : 'Never'
                      }
                    </TableCell>
                    <TableCell>
                      <div className="flex items-center gap-2">
                        <Button variant="ghost" size="sm">
                          <Edit className="h-4 w-4" />
                        </Button>
                        <Button
                          variant="ghost"
                          size="sm"
                          onClick={() => resetPasswordMutation.mutate(user.id)}
                          disabled={resetPasswordMutation.isPending}
                        >
                          <Key className="h-4 w-4" />
                        </Button>
                      </div>
                    </TableCell>
                  </TableRow>
                ))
              )}
            </TableBody>
          </Table>
        </div>
      )}

      {/* Dialog Components */}
      <InviteUserDialog 
        open={isInviteDialogOpen} 
        onOpenChange={setIsInviteDialogOpen} 
      />
      
      <QuickBaseLookupDialog 
        open={isLookupDialogOpen} 
        onOpenChange={setIsLookupDialogOpen} 
      />
      
      <SmartSyncDialog 
        open={isSmartSyncDialogOpen} 
        onOpenChange={setIsSmartSyncDialogOpen} 
      />
      
      <DeactivateInactiveDialog 
        open={isDeactivateDialogOpen} 
        onOpenChange={setIsDeactivateDialogOpen} 
      />
    </div>
  )
}
