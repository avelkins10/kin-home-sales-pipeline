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
import { UserPlus, Search, Edit, Key, Mail, Users, RefreshCw, UserMinus, Eye, Info, X, Trash2, Copy } from 'lucide-react'
import { toast } from 'sonner'
import { User, CreateUserInput } from '@/lib/types/user'
import { getBaseUrl } from '@/lib/utils/baseUrl'
import { getRoleBadgeVariant, getRoleDisplayName } from '@/lib/utils/roles'
import { InviteUserDialog } from './InviteUserDialog'
import { SmartSyncDialog } from './SmartSyncDialog'
import { DeactivateInactiveDialog } from './DeactivateInactiveDialog'
import { HierarchyTreeView } from './HierarchyTreeView'
import { OfficeMultiSelect } from './OfficeMultiSelect'
import { EmailConfigBanner } from './EmailConfigBanner'

export default function UsersTab() {
  const [searchQuery, setSearchQuery] = useState('')
  const [roleFilter, setRoleFilter] = useState('all')
  const [officeFilter, setOfficeFilter] = useState('all')
  const [activityFilter, setActivityFilter] = useState('all')
  const [managedByFilter, setManagedByFilter] = useState<string | null>(null)
  const [isInviteDialogOpen, setIsInviteDialogOpen] = useState(false)
  const [isSmartSyncDialogOpen, setIsSmartSyncDialogOpen] = useState(false)
  const [isDeactivateDialogOpen, setIsDeactivateDialogOpen] = useState(false)
  const [isHierarchyView, setIsHierarchyView] = useState(false)
  const [isEditDialogOpen, setIsEditDialogOpen] = useState(false)
  const [editingUser, setEditingUser] = useState<User | null>(null)
  const [isDeleteDialogOpen, setIsDeleteDialogOpen] = useState(false)
  const [userToDelete, setUserToDelete] = useState<User | null>(null)
  const [newPassword, setNewPassword] = useState<string | null>(null)
  const [passwordResetUser, setPasswordResetUser] = useState<User | null>(null)

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
    onSuccess: (data, userId) => {
      const user = users.find((u: User) => u.id === userId)
      setNewPassword(data.temporaryPassword)
      setPasswordResetUser(user || null)
      toast.success('Password reset successfully!')
    },
    onError: (error: Error) => {
      toast.error(error.message)
    },
  })

  // Update user mutation
  const updateUserMutation = useMutation({
    mutationFn: async ({ userId, updates }: { userId: string; updates: Partial<User> }) => {
      const response = await fetch(`${getBaseUrl()}/api/admin/users/${userId}`, {
        method: 'PATCH',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(updates),
      })
      if (!response.ok) {
        const error = await response.json()
        throw new Error(error.message || 'Failed to update user')
      }
      return response.json()
    },
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['users'] })
      setIsEditDialogOpen(false)
      setEditingUser(null)
      toast.success('User updated successfully')
    },
    onError: (error: Error) => {
      toast.error(error.message)
    },
  })

  // Delete user mutation
  const deleteUserMutation = useMutation({
    mutationFn: async (userId: string) => {
      const response = await fetch(`${getBaseUrl()}/api/admin/users/${userId}`, {
        method: 'DELETE',
      })
      if (!response.ok) {
        const error = await response.json()
        throw new Error(error.message || 'Failed to delete user')
      }
      return response.json()
    },
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['users'] })
      setIsDeleteDialogOpen(false)
      setUserToDelete(null)
      toast.success('User deleted successfully')
    },
    onError: (error: Error) => {
      toast.error(error.message)
    },
  })

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

  const copyToClipboard = (text: string) => {
    navigator.clipboard.writeText(text)
    toast.success('Password copied to clipboard')
  }

  return (
    <div className="space-y-6">
      {/* Email Configuration Banner */}
      <EmailConfigBanner />

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
            onClick={() => setIsInviteDialogOpen(true)}
          >
            <UserPlus className="h-4 w-4 mr-2" />
            Invite User
          </Button>
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
        <div className="border rounded-lg overflow-x-auto">
          <Table>
            <TableHeader>
              <TableRow>
                <TableHead className="min-w-[200px]">Name / Status</TableHead>
                <TableHead className="min-w-[220px]">Email / Role</TableHead>
                <TableHead>Managed By</TableHead>
                <TableHead>Manages</TableHead>
                <TableHead>Last Login</TableHead>
                <TableHead className="text-right">Actions</TableHead>
              </TableRow>
            </TableHeader>
            <TableBody>
              {isLoading ? (
                <TableRow>
                  <TableCell colSpan={6} className="text-center py-8">
                    Loading users...
                  </TableCell>
                </TableRow>
              ) : users.length === 0 ? (
                <TableRow>
                  <TableCell colSpan={6} className="text-center py-8">
                    No users found
                  </TableCell>
                </TableRow>
              ) : (
                displayedUsers.map((user: User) => (
                  <TableRow key={user.id}>
                    <TableCell className="font-medium">
                      <div className="flex items-center gap-3">
                        <div className={`w-2 h-2 rounded-full flex-shrink-0 ${getActivityColor(getActivityStatus(user.lastProjectDate))}`} />
                        <div className="flex flex-col gap-1">
                          <div className="font-medium">{user.name}</div>
                          {user.inviteToken && !user.inviteAcceptedAt ? (
                            <Badge variant="secondary" className="bg-amber-100 text-amber-800 border-amber-300 w-fit">
                              Pending Invite
                            </Badge>
                          ) : user.isActive ? (
                            <Badge variant="secondary" className="bg-green-100 text-green-800 border-green-300 w-fit">
                              Active
                            </Badge>
                          ) : (
                            <Badge variant="secondary" className="bg-gray-100 text-gray-600 border-gray-300 w-fit">
                              Inactive
                            </Badge>
                          )}
                        </div>
                      </div>
                    </TableCell>
                    <TableCell>
                      <div className="flex flex-col gap-1">
                        <div className="text-sm">{user.email}</div>
                        <Badge variant={getRoleBadgeVariant(user.role)} className="w-fit">
                          {getRoleDisplayName(user.role)}
                        </Badge>
                      </div>
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
                      {user.inviteToken && !user.inviteAcceptedAt ? (
                        <div className="text-sm">
                          <span className="text-gray-500">Invited </span>
                          <span className="text-gray-700">
                            {user.invitedAt && Math.floor((Date.now() - new Date(user.invitedAt).getTime()) / (1000 * 60 * 60 * 24))} days ago
                          </span>
                        </div>
                      ) : user.lastLoginAt ? (
                        new Date(user.lastLoginAt).toLocaleDateString()
                      ) : (
                        <span className="text-gray-400">Never</span>
                      )}
                    </TableCell>
                    <TableCell className="text-right">
                      <div className="flex items-center justify-end gap-2">
                        <Button
                          variant="ghost"
                          size="sm"
                          onClick={() => {
                            setEditingUser(user)
                            setIsEditDialogOpen(true)
                          }}
                        >
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
                        <Button
                          variant="ghost"
                          size="sm"
                          onClick={() => {
                            setUserToDelete(user)
                            setIsDeleteDialogOpen(true)
                          }}
                          className="text-red-600 hover:text-red-700 hover:bg-red-50"
                        >
                          <Trash2 className="h-4 w-4" />
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

      <SmartSyncDialog
        open={isSmartSyncDialogOpen}
        onOpenChange={setIsSmartSyncDialogOpen}
      />

      <DeactivateInactiveDialog
        open={isDeactivateDialogOpen}
        onOpenChange={setIsDeactivateDialogOpen}
      />

      {/* Edit User Dialog */}
      {editingUser && (
        <Dialog open={isEditDialogOpen} onOpenChange={setIsEditDialogOpen}>
          <DialogContent className="sm:max-w-md">
            <DialogHeader>
              <DialogTitle>Edit User</DialogTitle>
              <DialogDescription>
                Update user information and settings.
              </DialogDescription>
            </DialogHeader>
            <div className="space-y-4">
              <div>
                <Label htmlFor="edit-name">Name</Label>
                <Input
                  id="edit-name"
                  value={editingUser.name}
                  onChange={(e) => setEditingUser({ ...editingUser, name: e.target.value })}
                  placeholder="Full name"
                />
              </div>
              <div>
                <Label htmlFor="edit-email">Email (read-only)</Label>
                <Input
                  id="edit-email"
                  value={editingUser.email}
                  disabled
                  className="bg-gray-50"
                />
              </div>
              <div>
                <Label htmlFor="edit-phone">Phone</Label>
                <Input
                  id="edit-phone"
                  value={editingUser.phone || ''}
                  onChange={(e) => setEditingUser({ ...editingUser, phone: e.target.value })}
                  placeholder="555-1234"
                />
              </div>
              <div>
                <Label htmlFor="edit-role">Role</Label>
                <Select
                  value={editingUser.role}
                  onValueChange={(value) => setEditingUser({ ...editingUser, role: value as User['role'] })}
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
                    <SelectItem value="coordinator">Coordinator</SelectItem>
                  </SelectContent>
                </Select>
              </div>
              <div>
                <Label htmlFor="edit-quickbaseUserId">QuickBase User ID</Label>
                <Input
                  id="edit-quickbaseUserId"
                  value={editingUser.quickbaseUserId || ''}
                  onChange={(e) => setEditingUser({ ...editingUser, quickbaseUserId: e.target.value })}
                  placeholder="Leave blank for admin/ops users"
                />
                <p className="text-xs text-gray-500 mt-1">
                  Only required for sales team members with QuickBase access
                </p>
              </div>
              <div className="flex items-center space-x-2 pt-2">
                <Switch
                  id="edit-isActive"
                  checked={editingUser.isActive}
                  onCheckedChange={(checked) => setEditingUser({ ...editingUser, isActive: checked })}
                />
                <Label htmlFor="edit-isActive" className="cursor-pointer">
                  Active Account
                </Label>
              </div>
            </div>
            <DialogFooter className="flex-col sm:flex-row gap-2">
              <Button
                variant="outline"
                onClick={() => resetPasswordMutation.mutate(editingUser.id)}
                disabled={resetPasswordMutation.isPending}
                className="sm:mr-auto"
              >
                <Key className="h-4 w-4 mr-2" />
                {resetPasswordMutation.isPending ? 'Resetting...' : 'Reset Password'}
              </Button>
              <div className="flex gap-2">
                <Button
                  variant="outline"
                  onClick={() => {
                    setIsEditDialogOpen(false)
                    setEditingUser(null)
                  }}
                >
                  Cancel
                </Button>
                <Button
                  onClick={() => {
                    if (!editingUser.name) {
                      toast.error('Name is required')
                      return
                    }
                    updateUserMutation.mutate({
                      userId: editingUser.id,
                      updates: {
                        name: editingUser.name,
                        phone: editingUser.phone || undefined,
                        quickbaseUserId: editingUser.quickbaseUserId || undefined,
                        role: editingUser.role,
                        isActive: editingUser.isActive,
                      },
                    })
                  }}
                  disabled={updateUserMutation.isPending}
                >
                  {updateUserMutation.isPending ? 'Saving...' : 'Save Changes'}
                </Button>
              </div>
            </DialogFooter>
          </DialogContent>
        </Dialog>
      )}

      {/* Delete Confirmation Dialog */}
      <AlertDialog open={isDeleteDialogOpen} onOpenChange={setIsDeleteDialogOpen}>
        <AlertDialogContent>
          <AlertDialogHeader>
            <AlertDialogTitle>Delete User</AlertDialogTitle>
            <AlertDialogDescription>
              Are you sure you want to delete <strong>{userToDelete?.name}</strong> ({userToDelete?.email})?
              This action cannot be undone and will remove all associated data including office assignments and hierarchy relationships.
            </AlertDialogDescription>
          </AlertDialogHeader>
          <AlertDialogFooter>
            <AlertDialogCancel onClick={() => setUserToDelete(null)}>
              Cancel
            </AlertDialogCancel>
            <AlertDialogAction
              onClick={() => {
                if (userToDelete) {
                  deleteUserMutation.mutate(userToDelete.id)
                }
              }}
              className="bg-red-600 hover:bg-red-700"
              disabled={deleteUserMutation.isPending}
            >
              {deleteUserMutation.isPending ? 'Deleting...' : 'Delete User'}
            </AlertDialogAction>
          </AlertDialogFooter>
        </AlertDialogContent>
      </AlertDialog>

      {/* Password Reset Success Dialog */}
      <AlertDialog open={!!newPassword} onOpenChange={(open) => !open && setNewPassword(null)}>
        <AlertDialogContent>
          <AlertDialogHeader>
            <AlertDialogTitle>Password Reset Successful</AlertDialogTitle>
            <AlertDialogDescription>
              The new temporary password for <strong>{passwordResetUser?.name}</strong> is:
            </AlertDialogDescription>
          </AlertDialogHeader>
          <div className="bg-gray-100 p-4 rounded-md">
            <div className="flex items-center justify-between">
              <code className="text-lg font-mono font-bold">{newPassword}</code>
              <Button
                variant="outline"
                size="sm"
                onClick={() => {
                  if (newPassword) {
                    copyToClipboard(newPassword)
                  }
                }}
              >
                <Copy className="h-4 w-4 mr-2" />
                Copy
              </Button>
            </div>
          </div>
          <AlertDialogDescription className="text-amber-600 mt-4">
            ⚠️ <strong>Important:</strong> This password will only be shown once.
            Make sure to copy it and share it with {passwordResetUser?.name} securely.
            You can now use this password to log in as them for testing.
          </AlertDialogDescription>
          <AlertDialogFooter>
            <AlertDialogAction onClick={() => setNewPassword(null)}>
              Done
            </AlertDialogAction>
          </AlertDialogFooter>
        </AlertDialogContent>
      </AlertDialog>
    </div>
  )
}
