'use client'

import { useState } from 'react'
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query'
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card'
import { Button } from '@/components/ui/button'
import { Badge } from '@/components/ui/badge'
import { Input } from '@/components/ui/input'
import { Label } from '@/components/ui/label'
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
} from '@/components/ui/dialog'
import { toast } from 'sonner'
import { getBaseUrl } from '@/lib/utils/baseUrl'
import { getRoleBadgeVariant, getRoleDisplayName } from '@/lib/utils/roles'
import type { UserRole } from '@/lib/types/project'
import { 
  Users, 
  UserPlus, 
  UserMinus, 
  ChevronDown, 
  ChevronRight, 
  Crown, 
  Shield,
  Building,
  MapPin
} from 'lucide-react'

interface HierarchyTreeViewProps {
  className?: string
}

interface User {
  id: string
  name: string
  email: string
  role: UserRole
  office?: string
  is_active: boolean
  manages?: string[]
  managed_by?: string
}

interface Hierarchy {
  id: string
  manager_id: string
  user_id: string
  created_at: string
  updated_at: string
  manager_name: string
  manager_email: string
  manager_role: string
  user_name: string
  user_email: string
  user_role: string
  user_is_active: boolean
}

interface HierarchyNode {
  user: User
  children: HierarchyNode[]
  level: number
}

export function HierarchyTreeView({ className }: HierarchyTreeViewProps) {
  const [expandedNodes, setExpandedNodes] = useState<Set<string>>(new Set())
  const [assignDialogOpen, setAssignDialogOpen] = useState(false)
  const [selectedManager, setSelectedManager] = useState<string>('')
  const [selectedUsers, setSelectedUsers] = useState<string[]>([])
  const [removeDialogOpen, setRemoveDialogOpen] = useState(false)
  const [removalTarget, setRemovalTarget] = useState<{ managerId: string; userId: string } | null>(null)

  const queryClient = useQueryClient()

  // Fetch all users
  const { data: users = [] } = useQuery({
    queryKey: ['users'],
    queryFn: async () => {
      const response = await fetch(`${getBaseUrl()}/api/admin/users`)
      if (!response.ok) throw new Error('Failed to fetch users')
      return response.json()
    },
  })

  // Fetch hierarchies
  const { data: hierarchies = [] } = useQuery({
    queryKey: ['hierarchies'],
    queryFn: async () => {
      const response = await fetch(`${getBaseUrl()}/api/admin/hierarchies`)
      if (!response.ok) throw new Error('Failed to fetch hierarchies')
      return response.json()
    },
  })

  // Build hierarchy tree
  const buildHierarchyTree = (): HierarchyNode[] => {
    const userMap = new Map<string, User>()
    const childrenMap = new Map<string, string[]>()
    const managerMap = new Map<string, string>()

    // Build user map
    users.forEach((user: User) => {
      userMap.set(user.id, user)
    })

    // Build relationships from hierarchies
    hierarchies.forEach((hierarchy: Hierarchy) => {
      if (!childrenMap.has(hierarchy.manager_id)) {
        childrenMap.set(hierarchy.manager_id, [])
      }
      childrenMap.get(hierarchy.manager_id)!.push(hierarchy.user_id)
      managerMap.set(hierarchy.user_id, hierarchy.manager_id)
    })

    // Find root nodes (users with no manager)
    const rootUsers = users.filter((user: User) => !managerMap.has(user.id))

    const buildNode = (user: User, level: number): HierarchyNode => {
      const children = childrenMap.get(user.id) || []
      return {
        user,
        children: children.map(childId => {
          const childUser = userMap.get(childId)
          return childUser ? buildNode(childUser, level + 1) : null
        }).filter(Boolean) as HierarchyNode[],
        level
      }
    }

    return rootUsers.map(user => buildNode(user, 0))
  }

  // Assign users mutation
  const assignMutation = useMutation({
    mutationFn: async ({ managerId, userIds }: { managerId: string; userIds: string[] }) => {
      const response = await fetch(`${getBaseUrl()}/api/admin/hierarchies`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ managerId, userIds }),
      })

      if (!response.ok) {
        const error = await response.json()
        throw new Error(error.error || 'Failed to assign users')
      }

      return response.json()
    },
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['hierarchies'] })
      setAssignDialogOpen(false)
      setSelectedManager('')
      setSelectedUsers([])
      toast.success('Users assigned successfully')
    },
    onError: (error: Error) => {
      toast.error('Failed to assign users', {
        description: error.message,
      })
    },
  })

  // Remove assignment mutation
  const removeMutation = useMutation({
    mutationFn: async ({ managerId, userId }: { managerId: string; userId: string }) => {
      const response = await fetch(
        `${getBaseUrl()}/api/admin/hierarchies?managerId=${managerId}&userId=${userId}`,
        { method: 'DELETE' }
      )

      if (!response.ok) {
        const error = await response.json()
        throw new Error(error.error || 'Failed to remove assignment')
      }

      return response.json()
    },
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['hierarchies'] })
      setRemoveDialogOpen(false)
      setRemovalTarget(null)
      toast.success('Assignment removed successfully')
    },
    onError: (error: Error) => {
      toast.error('Failed to remove assignment', {
        description: error.message,
      })
    },
  })

  const toggleNode = (userId: string) => {
    const newExpanded = new Set(expandedNodes)
    if (newExpanded.has(userId)) {
      newExpanded.delete(userId)
    } else {
      newExpanded.add(userId)
    }
    setExpandedNodes(newExpanded)
  }

  const getRoleIcon = (role: UserRole) => {
    switch (role) {
      case 'super_admin':
        return <Crown className="h-4 w-4 text-red-500" />
      case 'regional':
      case 'divisional':
      case 'area_director':
        return <Shield className="h-4 w-4 text-blue-500" />
      case 'office_leader':
        return <Building className="h-4 w-4 text-green-500" />
      default:
        return <Users className="h-4 w-4 text-gray-500" />
    }
  }


  const renderNode = (node: HierarchyNode) => {
    const isExpanded = expandedNodes.has(node.user.id)
    const hasChildren = node.children.length > 0

    return (
      <div key={node.user.id} className="ml-4">
        <Card className={`mb-2 ${node.level > 0 ? 'ml-6' : ''}`}>
          <CardContent className="p-4">
            <div className="flex items-center justify-between">
              <div className="flex items-center gap-3">
                {hasChildren && (
                  <Button
                    variant="ghost"
                    size="sm"
                    onClick={() => toggleNode(node.user.id)}
                    className="h-6 w-6 p-0"
                  >
                    {isExpanded ? (
                      <ChevronDown className="h-4 w-4" />
                    ) : (
                      <ChevronRight className="h-4 w-4" />
                    )}
                  </Button>
                )}
                {!hasChildren && <div className="w-6" />}
                
                <div className="flex items-center gap-2">
                  {getRoleIcon(node.user.role)}
                  <div>
                    <div className="flex items-center gap-2">
                      <h4 className="font-medium">{node.user.name}</h4>
                      <Badge variant={getRoleBadgeVariant(node.user.role)}>
                        {getRoleDisplayName(node.user.role)}
                      </Badge>
                      {!node.user.is_active && (
                        <Badge variant="outline" className="text-gray-500">
                          Inactive
                        </Badge>
                      )}
                    </div>
                    <div className="flex items-center gap-4 text-sm text-gray-600">
                      <span>{node.user.email}</span>
                      {node.user.office && (
                        <div className="flex items-center gap-1">
                          <MapPin className="h-3 w-3" />
                          {node.user.office}
                        </div>
                      )}
                    </div>
                  </div>
                </div>
              </div>
              
              <div className="flex items-center gap-2">
                <Button
                  variant="outline"
                  size="sm"
                  onClick={() => {
                    setSelectedManager(node.user.id)
                    setAssignDialogOpen(true)
                  }}
                  disabled={!(['team_lead', 'office_leader', 'area_director', 'divisional', 'regional', 'super_admin'] as const satisfies readonly UserRole[]).includes(node.user.role)}
                >
                  <UserPlus className="h-4 w-4 mr-1" />
                  Assign
                </Button>
                {node.level > 0 && (
                  <Button
                    variant="outline"
                    size="sm"
                    onClick={() => {
                      const managerId = hierarchies.find((h: Hierarchy) => h.user_id === node.user.id)?.manager_id
                      if (managerId) {
                        setRemovalTarget({ managerId, userId: node.user.id })
                        setRemoveDialogOpen(true)
                      }
                    }}
                  >
                    <UserMinus className="h-4 w-4 mr-1" />
                    Remove
                  </Button>
                )}
              </div>
            </div>
          </CardContent>
        </Card>
        
        {isExpanded && hasChildren && (
          <div>
            {node.children.map(child => renderNode(child))}
          </div>
        )}
      </div>
    )
  }

  const hierarchyTree = buildHierarchyTree()
  const ROLE_MANAGERS: readonly UserRole[] = ['team_lead', 'office_leader', 'area_director', 'divisional', 'regional', 'super_admin'] as const
  const availableManagers = users.filter((user: User) => 
    ROLE_MANAGERS.includes(user.role)
  )
  const availableUsers = users.filter((user: User) => 
    !(['super_admin'] as const satisfies readonly UserRole[]).includes(user.role) && user.is_active
  )

  return (
    <div className={className}>
      <div className="mb-6">
        <h3 className="text-lg font-semibold mb-2">User Hierarchy</h3>
        <p className="text-sm text-gray-600">
          Manage team lead assignments and organizational structure.
        </p>
      </div>

      {hierarchyTree.length === 0 ? (
        <Card>
          <CardContent className="p-8 text-center">
            <Users className="h-12 w-12 mx-auto text-gray-400 mb-4" />
            <h3 className="text-lg font-medium mb-2">No hierarchy data</h3>
            <p className="text-gray-600 mb-4">
              Start by assigning users to managers to build your organizational structure.
            </p>
            <Button onClick={() => setAssignDialogOpen(true)}>
              <UserPlus className="h-4 w-4 mr-2" />
              Create First Assignment
            </Button>
          </CardContent>
        </Card>
      ) : (
        <div className="space-y-2">
          {hierarchyTree.map(node => renderNode(node))}
        </div>
      )}

      {/* Assign Users Dialog */}
      <Dialog open={assignDialogOpen} onOpenChange={setAssignDialogOpen}>
        <DialogContent className="sm:max-w-md">
          <DialogHeader>
            <DialogTitle>Assign Users to Manager</DialogTitle>
            <DialogDescription>
              Select a manager and users to assign to them.
            </DialogDescription>
          </DialogHeader>
          
          <div className="space-y-4">
            <div>
              <Label htmlFor="manager-select">Manager</Label>
              <Select 
                value={selectedManager} 
                onValueChange={setSelectedManager}
              >
                <SelectTrigger>
                  <SelectValue placeholder="Select manager" />
                </SelectTrigger>
                <SelectContent>
                  {availableManagers.map((manager) => (
                    <SelectItem key={manager.id} value={manager.id}>
                      {manager.name} ({getRoleDisplayName(manager.role)})
                    </SelectItem>
                  ))}
                </SelectContent>
              </Select>
            </div>

            <div>
              <Label htmlFor="users-select">Users to Assign</Label>
              <Select 
                value={selectedUsers[0] || ''} 
                onValueChange={(value) => setSelectedUsers([value])}
              >
                <SelectTrigger>
                  <SelectValue placeholder="Select user" />
                </SelectTrigger>
                <SelectContent>
                  {availableUsers
                    .filter(user => user.id !== selectedManager)
                    .map((user) => (
                      <SelectItem key={user.id} value={user.id}>
                        {user.name} ({getRoleDisplayName(user.role)})
                      </SelectItem>
                    ))}
                </SelectContent>
              </Select>
            </div>
          </div>
          
          <DialogFooter>
            <Button variant="outline" onClick={() => setAssignDialogOpen(false)}>
              Cancel
            </Button>
            <Button 
              onClick={() => {
                if (selectedManager && selectedUsers.length > 0) {
                  assignMutation.mutate({ managerId: selectedManager, userIds: selectedUsers })
                }
              }}
              disabled={!selectedManager || selectedUsers.length === 0 || assignMutation.isPending}
            >
              {assignMutation.isPending ? 'Assigning...' : 'Assign Users'}
            </Button>
          </DialogFooter>
        </DialogContent>
      </Dialog>

      {/* Remove Assignment Dialog */}
      <Dialog open={removeDialogOpen} onOpenChange={setRemoveDialogOpen}>
        <DialogContent className="sm:max-w-md">
          <DialogHeader>
            <DialogTitle>Remove Assignment</DialogTitle>
            <DialogDescription>
              Are you sure you want to remove this user from their manager's team?
            </DialogDescription>
          </DialogHeader>
          
          <DialogFooter>
            <Button variant="outline" onClick={() => setRemoveDialogOpen(false)}>
              Cancel
            </Button>
            <Button 
              variant="destructive"
              onClick={() => {
                if (removalTarget) {
                  removeMutation.mutate(removalTarget)
                }
              }}
              disabled={removeMutation.isPending}
            >
              {removeMutation.isPending ? 'Removing...' : 'Remove Assignment'}
            </Button>
          </DialogFooter>
        </DialogContent>
      </Dialog>
    </div>
  )
}
