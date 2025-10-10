'use client'

import { useState, useMemo } from 'react'
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
  Popover,
  PopoverContent,
  PopoverTrigger,
} from '@/components/ui/popover'
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
import { UserPlus, Search, Edit, Key, Mail, Users, RefreshCw, UserMinus, Eye, Info, X } from 'lucide-react'
import { toast } from 'sonner'
import { User, CreateUserInput } from '@/lib/types/user'
import { getBaseUrl } from '@/lib/utils/baseUrl'
import { getRoleBadgeVariant, getRoleDisplayName } from '@/lib/utils/roles'
import { InviteUserDialog } from './InviteUserDialog'
import { QuickBaseLookupDialog } from './QuickBaseLookupDialog'
import { SmartSyncDialog } from './SmartSyncDialog'
import { DeactivateInactiveDialog } from './DeactivateInactiveDialog'
import { HierarchyTreeView } from './HierarchyTreeView'
import { OfficeMultiSelect } from './OfficeMultiSelect'

export default function UsersTab() {
  const [searchQuery, setSearchQuery] = useState('')
  const [roleFilter, setRoleFilter] = useState('all')
  const [officeFilter, setOfficeFilter] = useState('all')
  const [activityFilter, setActivityFilter] = useState('all')
  const [managedByFilter, setManagedByFilter] = useState<string | null>(null)
  const [isAddDialogOpen, setIsAddDialogOpen] = useState(false)
  const [isInviteDialogOpen, setIsInviteDialogOpen] = useState(false)
  const [isLookupDialogOpen, setIsLookupDialogOpen] = useState(false)
  const [isSmartSyncDialogOpen, setIsSmartSyncDialogOpen] = useState(false)
  const [isDeactivateDialogOpen, setIsDeactivateDialogOpen] = useState(false)
  const [isHierarchyView, setIsHierarchyView] = useState(false)
  const [newUser, setNewUser] = useState<CreateUserInput & { 
    offices?: string[]
    managedBy?: string
    manages?: string[]
    officeAccess?: Array<{ officeName: string; accessLevel: string }>
  }>({
    name: '',
    email: '',
    phone: '',
    role: 'closer',
    quickbaseUserId: '',
    office: '',
    offices: [],
    region: '',
    temporaryPassword: '',
    managedBy: undefined,
    manages: [],
    officeAccess: [],
  })

  const queryClient = useQueryClient()

  // Fetch users with filters
  const { data: users = [], isLoading } = useQuery({
    queryKey: ['users', searchQuery, roleFilter, officeFilter, activityFilter, managedByFilter],
    queryFn: async () => {
      const params = new URLSearchParams()
      if (searchQuery) params.append('search', searchQuery)
      if (roleFilter !== 'all') params.append('role', roleFilter)
      if (officeFilter !== 'all') params.append('office', officeFilter)
      if (managedByFilter) params.append('managedBy', managedByFilter)
      
      // For activity filtering, we'll fetch a wide window and filter client-side
      // This ensures dormant users (12+ months) are properly included
      if (activityFilter === 'active') {
        params.append('activeOnly', 'true')
        params.append('monthsBack', '6')
      } else if (activityFilter === 'inactive') {
        params.append('activeOnly', 'true')
        params.append('monthsBack', '12')
      }
      // For 'dormant' and 'all', we fetch all users and filter client-side
      
      const response = await fetch(`${getBaseUrl()}/api/admin/users?${params}`)
      if (!response.ok) throw new Error('Failed to fetch users')
      return response.json()
    },
  })

  // Create usersById map for efficient lookups
  const usersById = useMemo(() => {
    const map = new Map<string, User>()
    users.forEach((user: User) => {
      map.set(user.id, user)
    })
    return map
  }, [users])

  // Client-side activity filtering
  const displayedUsers = useMemo(() => {
    if (activityFilter === 'all') return users
    
    return users.filter((user: User) => {
      const activityStatus = getActivityStatus(user.lastProjectDate)
      return activityStatus === activityFilter
    })
  }, [users, activityFilter])

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
        offices: [],
        region: '',
        temporaryPassword: '',
        managedBy: undefined,
        manages: [],
        officeAccess: [],
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


  const handleCreateUser = () => {
    if (!newUser.name || !newUser.email || !newUser.temporaryPassword) {
      toast.error('Name, email, and temporary password are required')
      return
    }
    
    // Validate office assignments for office-based roles
    if (isOfficeBasedRole(newUser.role) && (!newUser.offices || newUser.offices.length === 0)) {
      toast.error('Office-based roles must have at least one office assigned')
      return
    }
    
    // Team lead validation - allow creation without initial managed reps
    if (isTeamLeadRole(newUser.role) && (!newUser.manages || newUser.manages.length === 0)) {
      // Show informational toast instead of error
      toast.info('Team lead created without initial managed reps. You can assign reps later.')
    }
    
    createUserMutation.mutate(newUser)
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

  const getActivityColor = (status: string) => {
    switch (status) {
      case 'active': return 'bg-green-500'
      case 'inactive': return 'bg-yellow-500'
      case 'dormant': return 'bg-red-500'
      default: return 'bg-gray-500'
    }
  }

  const isOfficeBasedRole = (role: string) => {
    return ['office_leader', 'area_director', 'divisional', 'regional'].includes(role)
  }

  const isTeamLeadRole = (role: string) => {
    return role === 'team_lead'
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
                <Label htmlFor="quickbaseUserId">Quickbase User ID (Optional)</Label>
                <Input
                  id="quickbaseUserId"
                  value={newUser.quickbaseUserId}
                  onChange={(e) => setNewUser({ ...newUser, quickbaseUserId: e.target.value })}
                  placeholder="Leave blank for admin/ops users"
                />
                <p className="text-xs text-gray-500 mt-1">
                  Only required for sales team members with QuickBase access
                </p>
              </div>
              <div>
                <Label htmlFor="offices">Offices {isOfficeBasedRole(newUser.role) && '*'}</Label>
                <OfficeMultiSelect
                  value={newUser.offices || []}
                  onChange={(offices) => {
                    setNewUser({ 
                      ...newUser, 
                      offices,
                      office: offices[0] || '', // Set first office as primary
                      officeAccess: offices.map(name => ({
                        officeName: name,
                        accessLevel: 'manage' as const
                      }))
                    })
                  }}
                  placeholder="Select offices..."
                />
                {isOfficeBasedRole(newUser.role) && (
                  <div className="flex items-start gap-2 mt-2 p-2 bg-blue-50 rounded-md">
                    <Info className="h-4 w-4 text-blue-600 mt-0.5 flex-shrink-0" />
                    <p className="text-xs text-blue-700">
                      Office-based roles see ALL projects in their assigned offices, including projects from users without active accounts.
                    </p>
                  </div>
                )}
              </div>
              {/* Team Lead: Select users to manage */}
              {isTeamLeadRole(newUser.role) && (
                <div>
                  <Label htmlFor="manages">Manages (Reps)</Label>
                  <Select 
                    value="" 
                    onValueChange={(userId) => {
                      if (!newUser.manages?.includes(userId)) {
                        setNewUser({ 
                          ...newUser, 
                          manages: [...(newUser.manages || []), userId]
                        })
                      }
                    }}
                  >
                    <SelectTrigger>
                      <SelectValue placeholder="Select reps to manage..." />
                    </SelectTrigger>
                    <SelectContent>
                      {users
                        .filter((u: User) => ['closer', 'setter'].includes(u.role))
                        .map((u: User) => (
                          <SelectItem key={u.id} value={u.id}>
                            {u.name} ({u.role})
                          </SelectItem>
                        ))}
                    </SelectContent>
                  </Select>
                  {newUser.manages && newUser.manages.length > 0 && (
                    <div className="flex flex-wrap gap-1 mt-2">
                      {newUser.manages.map((userId) => {
                        const user = users.find((u: User) => u.id === userId)
                        return user ? (
                          <Badge key={userId} variant="secondary" className="gap-1">
                            {user.name}
                            <button
                              onClick={() => {
                                setNewUser({
                                  ...newUser,
                                  manages: newUser.manages?.filter(id => id !== userId)
                                })
                              }}
                              className="ml-1 rounded-full hover:bg-gray-300"
                            >
                              <X className="h-3 w-3" />
                            </button>
                          </Badge>
                        ) : null
                      })}
                    </div>
                  )}
                  <div className="flex items-start gap-2 mt-2 p-2 bg-cyan-50 rounded-md">
                    <Info className="h-4 w-4 text-cyan-600 mt-0.5 flex-shrink-0" />
                    <p className="text-xs text-cyan-700">
                      Team leads see projects for their managed reps (both closer and setter projects).
                    </p>
                  </div>
                </div>
              )}

              {/* Closers/Setters: Select team lead */}
              {['closer', 'setter'].includes(newUser.role) && (
                <div>
                  <Label htmlFor="managedBy">Managed By (Team Lead)</Label>
                  <Select 
                    value={newUser.managedBy ?? 'none'} 
                    onValueChange={(value) => setNewUser({ ...newUser, managedBy: value === 'none' ? undefined : value })}
                  >
                    <SelectTrigger>
                      <SelectValue placeholder="Select team lead (optional)..." />
                    </SelectTrigger>
                    <SelectContent>
                      <SelectItem value="none">None</SelectItem>
                      {users
                        .filter((u: User) => ['team_lead', 'office_leader', 'area_director', 'divisional', 'regional', 'super_admin'].includes(u.role))
                        .map((u: User) => (
                          <SelectItem key={u.id} value={u.id}>
                            {u.name} ({getRoleDisplayName(u.role)})
                          </SelectItem>
                        ))}
                    </SelectContent>
                  </Select>
                </div>
              )}

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
        <Select value={activityFilter} onValueChange={setActivityFilter}>
          <SelectTrigger className="w-48">
            <SelectValue placeholder="Filter by activity" />
          </SelectTrigger>
          <SelectContent>
            <SelectItem value="all">All Users</SelectItem>
            <SelectItem value="active">Active (6 months)</SelectItem>
            <SelectItem value="inactive">Inactive (6-12 months)</SelectItem>
            <SelectItem value="dormant">Dormant (12+ months)</SelectItem>
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
                <TableHead>Offices</TableHead>
                <TableHead>Managed By</TableHead>
                <TableHead>Manages</TableHead>
                <TableHead>Status</TableHead>
                <TableHead>Last Login</TableHead>
                <TableHead>Actions</TableHead>
              </TableRow>
            </TableHeader>
            <TableBody>
              {isLoading ? (
                <TableRow>
                  <TableCell colSpan={9} className="text-center py-8">
                    Loading users...
                  </TableCell>
                </TableRow>
              ) : users.length === 0 ? (
                <TableRow>
                  <TableCell colSpan={9} className="text-center py-8">
                    No users found
                  </TableCell>
                </TableRow>
              ) : (
                displayedUsers.map((user: User) => (
                  <TableRow key={user.id}>
                    <TableCell className="font-medium">
                      <div className="flex items-center gap-2">
                        <div className={`w-2 h-2 rounded-full ${getActivityColor(getActivityStatus(user.lastProjectDate))}`} />
                        {user.name}
                      </div>
                    </TableCell>
                    <TableCell>{user.email}</TableCell>
                    <TableCell>
                      <Badge variant={getRoleBadgeVariant(user.role)}>
                        {getRoleDisplayName(user.role)}
                      </Badge>
                    </TableCell>
                    <TableCell>
                      {user.officeAccess && user.officeAccess.length > 0 ? (
                        <Popover>
                          <PopoverTrigger asChild>
                            <Button variant="ghost" size="sm" className="h-auto p-1">
                              <Badge variant="secondary">
                                {user.officeAccess.length} {user.officeAccess.length === 1 ? 'office' : 'offices'}
                              </Badge>
                            </Button>
                          </PopoverTrigger>
                          <PopoverContent className="w-64">
                            <div className="space-y-2">
                              <h4 className="font-semibold text-sm">Assigned Offices</h4>
                              <div className="space-y-1">
                                {user.officeAccess.map((access: any, idx: number) => (
                                  <div key={idx} className="flex items-center justify-between text-sm">
                                    <span>{access.officeName}</span>
                                    <Badge variant="outline" className="text-xs">{access.accessLevel}</Badge>
                                  </div>
                                ))}
                              </div>
                            </div>
                          </PopoverContent>
                        </Popover>
                      ) : user.salesOffice && user.salesOffice.length > 0 ? (
                        <span className="text-sm">{user.salesOffice.join(', ')}</span>
                      ) : (
                        <span className="text-gray-400">—</span>
                      )}
                    </TableCell>
                    <TableCell>
                      {user.managedBy ? (
                        <div className="flex items-center gap-2">
                          <Button 
                            variant="ghost" 
                            size="sm" 
                            className="h-auto p-1 text-blue-600 hover:text-blue-800"
                            onClick={() => {
                              setManagedByFilter(user.managedBy!)
                            }}
                          >
                            {usersById.get(user.managedBy!)?.name || user.managedBy}
                          </Button>
                          {managedByFilter === user.managedBy && (
                            <Button
                              variant="ghost"
                              size="sm"
                              className="h-auto p-1 text-gray-400 hover:text-gray-600"
                              onClick={() => setManagedByFilter(null)}
                            >
                              <X className="h-3 w-3" />
                            </Button>
                          )}
                        </div>
                      ) : (
                        <span className="text-gray-400">—</span>
                      )}
                    </TableCell>
                    <TableCell>
                      {user.manages && user.manages.length > 0 ? (
                        <Popover>
                          <PopoverTrigger asChild>
                            <Button variant="ghost" size="sm" className="h-auto p-1">
                              <Badge variant="secondary">
                                {user.manages.length} {user.manages.length === 1 ? 'rep' : 'reps'}
                              </Badge>
                            </Button>
                          </PopoverTrigger>
                          <PopoverContent className="w-64">
                            <div className="space-y-2">
                              <h4 className="font-semibold text-sm">Managed Users</h4>
                              <div className="space-y-1">
                                {user.manages.map((userId: string) => {
                                  const managedUser = usersById.get(userId)
                                  return (
                                    <div key={userId} className="text-sm">
                                      {managedUser ? `${managedUser.name} (${managedUser.role})` : userId}
                                    </div>
                                  )
                                })}
                              </div>
                            </div>
                          </PopoverContent>
                        </Popover>
                      ) : (
                        <span className="text-gray-400">—</span>
                      )}
                    </TableCell>
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
