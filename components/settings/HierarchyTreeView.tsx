'use client'

import { useState, useMemo, useEffect } from 'react'
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query'
import { useDebounce } from 'use-debounce'
import { DndContext, DragOverlay, useDraggable, useDroppable, closestCenter, KeyboardSensor, PointerSensor, useSensor, useSensors } from '@dnd-kit/core'
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card'
import { Button } from '@/components/ui/button'
import { Badge } from '@/components/ui/badge'
import { Input } from '@/components/ui/input'
import { Label } from '@/components/ui/label'
import { Avatar, AvatarFallback } from '@/components/ui/avatar'
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
import {
  Popover,
  PopoverContent,
  PopoverTrigger,
} from '@/components/ui/popover'
import {
  ContextMenu,
  ContextMenuContent,
  ContextMenuItem,
  ContextMenuSeparator,
  ContextMenuTrigger,
} from '@/components/ui/context-menu'
import { toast } from 'sonner'
import { getBaseUrl } from '@/lib/utils/baseUrl'
import { getRoleBadgeVariant, getRoleDisplayName } from '@/lib/utils/roles'
import { getActivityStatus, getActivityColor, getActivityLabel, formatLastActivity } from '@/lib/utils/activity'
import { getInitials, getAvatarColor, getAvatarTextColor } from '@/lib/utils/avatar'
import type { UserRole } from '@/lib/types/project'
import { UserDetailsDialog } from './UserDetailsDialog'
import { 
  Users, 
  UserPlus, 
  UserMinus, 
  ChevronDown, 
  ChevronRight, 
  Crown, 
  Shield,
  Building,
  MapPin,
  Search,
  Filter,
  X,
  Copy,
  Edit,
  UserCheck,
  UserX,
  Eye,
  MoreHorizontal
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
  salesOffice?: string[]
  officeAccess?: Array<{
    officeName: string
    accessLevel: 'view' | 'manage' | 'admin'
  }>
  is_active: boolean
  manages?: string[]
  managed_by?: string
  lastProjectDate?: string
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
  const [userDetailsOpen, setUserDetailsOpen] = useState(false)
  const [selectedUserId, setSelectedUserId] = useState<string | null>(null)
  
  // Search and filter state
  const [searchTerm, setSearchTerm] = useState('')
  const [roleFilter, setRoleFilter] = useState<string>('all')
  const [activityFilter, setActivityFilter] = useState<string>('all')
  const [officeFilter, setOfficeFilter] = useState<string>('all')
  const [debouncedSearchTerm] = useDebounce(searchTerm, 300)
  
  // Drag and drop state
  const [activeId, setActiveId] = useState<string | null>(null)
  const sensors = useSensors(
    useSensor(PointerSensor),
    useSensor(KeyboardSensor)
  )

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


  // Build hierarchy tree from all users, then apply filtering
  const buildHierarchyTree = (): HierarchyNode[] => {
    const userMap = new Map<string, User>()
    const childrenMap = new Map<string, string[]>()
    const managerMap = new Map<string, string>()

    // Build user map from ALL users
    users.forEach((user: User) => {
      userMap.set(user.id, user)
    })

    // Build relationships from hierarchies (for ALL users)
    hierarchies.forEach((hierarchy: Hierarchy) => {
      if (userMap.has(hierarchy.manager_id) && userMap.has(hierarchy.user_id)) {
        if (!childrenMap.has(hierarchy.manager_id)) {
          childrenMap.set(hierarchy.manager_id, [])
        }
        childrenMap.get(hierarchy.manager_id)!.push(hierarchy.user_id)
        managerMap.set(hierarchy.user_id, hierarchy.manager_id)
      }
    })

    // Find root nodes (users with no manager AND roles super_admin or regional)
    const rootUsers = users.filter((user: User) => 
      !managerMap.has(user.id) && ['super_admin', 'regional'].includes(user.role)
    )

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

    // Build complete tree from all users
    const completeTree = rootUsers.map((user: User) => buildNode(user, 0))

    // Apply recursive filter that preserves parents of matching children
    const matches = (user: User): boolean => {
      // Search filter
      if (debouncedSearchTerm) {
        const searchLower = debouncedSearchTerm.toLowerCase()
        const matchesSearch = 
          user.name.toLowerCase().includes(searchLower) ||
          user.email.toLowerCase().includes(searchLower)
        if (!matchesSearch) return false
      }

      // Role filter
      if (roleFilter !== 'all' && user.role !== roleFilter) {
        return false
      }

      // Activity filter
      if (activityFilter !== 'all') {
        const activityStatus = getActivityStatus(user.lastProjectDate)
        if (activityStatus !== activityFilter) {
          return false
        }
      }

      // Office filter
      if (officeFilter !== 'all') {
        const userOffices = [
          user.office,
          ...(user.salesOffice || []),
          ...(user.officeAccess?.map(oa => oa.officeName) || [])
        ].filter(Boolean)
        
        if (!userOffices.includes(officeFilter)) {
          return false
        }
      }

      return true
    }

    const filterTree = (nodes: HierarchyNode[]): HierarchyNode[] => {
      return nodes.map(node => {
        const filteredChildren = filterTree(node.children)
        const userMatches = matches(node.user)
        
        // Include node if it matches OR has matching children
        if (userMatches || filteredChildren.length > 0) {
          return {
            ...node,
            children: filteredChildren
          }
        }
        return null
      }).filter(Boolean) as HierarchyNode[]
    }

    return filterTree(completeTree)
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

  const openUserDetails = (userId: string) => {
    setSelectedUserId(userId)
    setUserDetailsOpen(true)
  }

  // Highlight search terms in text
  const highlightSearchTerm = (text: string, searchTerm: string) => {
    if (!searchTerm) return text
    
    const regex = new RegExp(`(${searchTerm})`, 'gi')
    const parts = text.split(regex)
    
    return parts.map((part, index) => 
      regex.test(part) ? (
        <mark key={index} className="bg-yellow-200 px-1 rounded">
          {part}
        </mark>
      ) : part
    )
  }

  // Check if a drop is valid based on role hierarchy
  const canDrop = (dragRole: UserRole, targetRole: UserRole): boolean => {
    // Only allow dragging for setter, closer, team_lead
    if (!['setter', 'closer', 'team_lead'].includes(dragRole)) {
      return false
    }
    
    // Restrict drops: setter|closer -> team_lead and team_lead -> office_leader
    if (['setter', 'closer'].includes(dragRole) && targetRole === 'team_lead') {
      return true
    }
    if (dragRole === 'team_lead' && targetRole === 'office_leader') {
      return true
    }
    
    return false
  }

  // Drag and drop handlers
  const handleDragStart = (event: any) => {
    setActiveId(event.active.id)
  }

  const handleDragEnd = (event: any) => {
    const { active, over } = event
    setActiveId(null)

    if (!over || active.id === over.id) return

    const draggedUser = users.find((u: User) => u.id === active.id)
    const targetManager = users.find((u: User) => u.id === over.id)

    if (!draggedUser || !targetManager) return

    // Check if drop is valid based on role hierarchy
    if (!canDrop(draggedUser.role, targetManager.role)) {
      toast.error('Invalid role assignment')
      return
    }

    // Prevent self-assignment
    if (draggedUser.id === targetManager.id) {
      toast.error('Cannot assign user to themselves')
      return
    }

    // Prevent circular hierarchy by checking if target is a descendant
    const getDescendants = (userId: string): string[] => {
      const descendants: string[] = []
      const directChildren = hierarchies
        .filter((h: Hierarchy) => h.manager_id === userId)
        .map((h: Hierarchy) => h.user_id)
      
      directChildren.forEach((childId: string) => {
        descendants.push(childId)
        descendants.push(...getDescendants(childId))
      })
      
      return descendants
    }

    const descendants = getDescendants(draggedUser.id)
    if (descendants.includes(targetManager.id)) {
      toast.error('Cannot create circular hierarchy')
      return
    }

    assignMutation.mutate({ 
      managerId: targetManager.id, 
      userIds: [draggedUser.id] 
    })
  }

  // Clear all filters
  const clearFilters = () => {
    setSearchTerm('')
    setRoleFilter('all')
    setActivityFilter('all')
    setOfficeFilter('all')
  }

  // Get unique offices for filter
  const uniqueOffices = useMemo(() => {
    const offices = new Set<string>()
    users.forEach((user: User) => {
      if (user.office) offices.add(user.office)
      if (user.salesOffice) user.salesOffice.forEach(office => offices.add(office))
      if (user.officeAccess) user.officeAccess.forEach(oa => offices.add(oa.officeName))
    })
    return Array.from(offices).sort()
  }, [users])

  // Check if any filters are active
  const hasActiveFilters = searchTerm || roleFilter !== 'all' || activityFilter !== 'all' || officeFilter !== 'all'

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

  // Draggable User Node Component
  function DraggableUserNode({ node, children }: { node: HierarchyNode; children: React.ReactNode }) {
    const canDrag = ['setter', 'closer', 'team_lead'].includes(node.user.role)
    const { attributes, listeners, setNodeRef, transform, isDragging } = useDraggable({
      id: node.user.id,
      disabled: !canDrag
    })

    const style = transform ? {
      transform: `translate3d(${transform.x}px, ${transform.y}px, 0)`,
    } : undefined

    return (
      <div
        ref={setNodeRef}
        style={style}
        {...attributes}
        {...listeners}
        className={`${isDragging ? 'opacity-50' : ''} ${canDrag ? 'cursor-grab active:cursor-grabbing' : 'cursor-not-allowed'}`}
      >
        {children}
      </div>
    )
  }

  // Droppable Manager Node Component
  function DroppableManagerNode({ node, children }: { node: HierarchyNode; children: React.ReactNode }) {
    const { isOver, setNodeRef } = useDroppable({
      id: node.user.id
    })

    // Check if current drag would be valid
    const isDragValid = activeId ? (() => {
      const draggedUser = users.find((u: User) => u.id === activeId)
      return draggedUser ? canDrop(draggedUser.role, node.user.role) : false
    })() : false

    const getDropStyles = () => {
      if (!isOver) return ''
      if (isDragValid) {
        return 'ring-2 ring-green-500 ring-opacity-50'
      } else {
        return 'ring-2 ring-red-500 ring-opacity-50 cursor-not-allowed'
      }
    }

    return (
      <div
        ref={setNodeRef}
        className={getDropStyles()}
      >
        {children}
      </div>
    )
  }


  const renderNode = (node: HierarchyNode) => {
    const isExpanded = expandedNodes.has(node.user.id)
    const hasChildren = node.children.length > 0
    const activityStatus = getActivityStatus(node.user.lastProjectDate)
    const activityColor = getActivityColor(activityStatus)
    const activityLabel = getActivityLabel(activityStatus)
    const lastActivity = formatLastActivity(node.user.lastProjectDate)
    const initials = getInitials(node.user.name)
    const avatarColor = getAvatarColor(node.user.role)
    const textColor = getAvatarTextColor()

    // Get managed user count from constructed hierarchy
    const managedCount = node.children.length

    // Get office information
    const allOffices = [
      ...(node.user.salesOffice || []),
      ...(node.user.officeAccess?.map(oa => oa.officeName) || [])
    ].filter(Boolean)
    const hasMultipleOffices = allOffices.length > 1

    const nodeContent = (
      <ContextMenu>
        <ContextMenuTrigger asChild>
          <Card 
            className={`mb-2 ${node.level > 0 ? 'ml-6' : ''} transition-all hover:shadow-md cursor-pointer`}
            onClick={() => openUserDetails(node.user.id)}
          >
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
                  
                  <div className="flex items-center gap-3">
                    {/* Avatar with activity indicator */}
                    <div className="relative">
                      <Avatar size="md" className={`${avatarColor} ${textColor}`}>
                        <AvatarFallback size="md">
                          {initials}
                        </AvatarFallback>
                      </Avatar>
                      {/* Activity indicator dot */}
                      <div 
                        className={`absolute -top-1 -right-1 w-3 h-3 rounded-full ${activityColor} border-2 border-white`}
                        title={`${activityLabel} - ${lastActivity}`}
                      />
                    </div>

                    <div className="flex-1">
                      <div className="flex items-center gap-2 flex-wrap">
                        <h4 className="font-medium">{highlightSearchTerm(node.user.name, debouncedSearchTerm)}</h4>
                        <Badge variant={getRoleBadgeVariant(node.user.role)}>
                          {getRoleDisplayName(node.user.role)}
                        </Badge>
                        {!node.user.is_active && (
                          <Badge variant="outline" className="text-gray-500">
                            Inactive
                          </Badge>
                        )}
                      </div>
                      
                      <div className="flex items-center gap-4 text-sm text-gray-600 mt-1">
                        <span>{highlightSearchTerm(node.user.email, debouncedSearchTerm)}</span>
                        
                        {/* Office display */}
                        {node.user.office && (
                          <div className="flex items-center gap-1">
                            <MapPin className="h-3 w-3" />
                            {node.user.office}
                          </div>
                        )}
                        
                        {/* Multiple offices badge */}
                        {hasMultipleOffices && (
                          <Popover>
                            <PopoverTrigger asChild>
                              <Badge variant="secondary" className="cursor-pointer hover:bg-secondary/80">
                                {allOffices.length} offices
                              </Badge>
                            </PopoverTrigger>
                            <PopoverContent className="w-64">
                              <div className="space-y-2">
                                <h4 className="font-medium">Offices</h4>
                                {allOffices.map((office, index) => (
                                  <div key={index} className="flex items-center gap-2 text-sm">
                                    <MapPin className="h-3 w-3" />
                                    {office}
                                  </div>
                                ))}
                              </div>
                            </PopoverContent>
                          </Popover>
                        )}

                        {/* Managed users count */}
                        {managedCount > 0 && (
                          <Badge variant="outline" className="cursor-pointer hover:bg-accent">
                            <Users className="h-3 w-3 mr-1" />
                            Manages {managedCount} {managedCount === 1 ? 'user' : 'users'}
                          </Badge>
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
                    disabled={!(['team_lead', 'office_leader', 'area_director', 'divisional', 'regional', 'super_admin'] as const).includes(node.user.role as any)}
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
        </ContextMenuTrigger>
        
        <ContextMenuContent>
          <ContextMenuItem onClick={() => openUserDetails(node.user.id)}>
            <Eye className="h-4 w-4 mr-2" />
            View Profile
          </ContextMenuItem>
          
          {(['team_lead', 'office_leader', 'area_director', 'divisional', 'regional', 'super_admin'] as const).includes(node.user.role as any) && (
            <ContextMenuItem onClick={() => {
              setSelectedManager(node.user.id)
              setAssignDialogOpen(true)
            }}>
              <UserPlus className="h-4 w-4 mr-2" />
              Assign Users
            </ContextMenuItem>
          )}
          
          {node.level > 0 && (
            <ContextMenuItem onClick={() => {
              const managerId = hierarchies.find((h: Hierarchy) => h.user_id === node.user.id)?.manager_id
              if (managerId) {
                setRemovalTarget({ managerId, userId: node.user.id })
                setRemoveDialogOpen(true)
              }
            }}>
              <UserMinus className="h-4 w-4 mr-2" />
              Remove from Team
            </ContextMenuItem>
          )}
          
          <ContextMenuSeparator />
          
          <ContextMenuItem onClick={() => {
            // Navigate to edit user page
            toast.info('Edit user not implemented yet')
          }}>
            <Edit className="h-4 w-4 mr-2" />
            Edit User
          </ContextMenuItem>
          
          <ContextMenuItem onClick={() => {
            // Toggle active status
            toast.info('Toggle user status not implemented yet')
          }}>
            {node.user.is_active ? (
              <>
                <UserX className="h-4 w-4 mr-2" />
                Deactivate User
              </>
            ) : (
              <>
                <UserCheck className="h-4 w-4 mr-2" />
                Activate User
              </>
            )}
          </ContextMenuItem>
          
          <ContextMenuSeparator />
          
          <ContextMenuItem onClick={() => {
            navigator.clipboard.writeText(node.user.id)
            toast.success('User ID copied to clipboard')
          }}>
            <Copy className="h-4 w-4 mr-2" />
            Copy User ID
          </ContextMenuItem>
          
          <ContextMenuItem onClick={() => {
            navigator.clipboard.writeText(node.user.email)
            toast.success('Email copied to clipboard')
          }}>
            <Copy className="h-4 w-4 mr-2" />
            Copy Email
          </ContextMenuItem>
        </ContextMenuContent>
      </ContextMenu>
    )

    return (
      <div key={node.user.id} className="ml-4">
        <DroppableManagerNode node={node}>
          <DraggableUserNode node={node}>
            {nodeContent}
          </DraggableUserNode>
        </DroppableManagerNode>
        
        {isExpanded && hasChildren && (
          <div>
            {node.children.map(child => renderNode(child))}
          </div>
        )}
      </div>
    )
  }

  const hierarchyTree = buildHierarchyTree()

  // Auto-expand nodes when filters change
  useEffect(() => {
    if (hasActiveFilters) {
      const newExpanded = new Set<string>()
      
      // Find all nodes that should be expanded (those with matching children)
      const findNodesToExpand = (nodes: HierarchyNode[]) => {
        nodes.forEach(node => {
          // Check if this node or any of its descendants match the filter
          const hasMatchingDescendants = (n: HierarchyNode): boolean => {
            if (n.children.length === 0) return false
            return n.children.some(child => hasMatchingDescendants(child)) || 
                   n.children.some(child => {
                     // Check if child matches current filters
                     const matches = (user: User): boolean => {
                       if (debouncedSearchTerm) {
                         const searchLower = debouncedSearchTerm.toLowerCase()
                         const matchesSearch = 
                           user.name.toLowerCase().includes(searchLower) ||
                           user.email.toLowerCase().includes(searchLower)
                         if (!matchesSearch) return false
                       }
                       if (roleFilter !== 'all' && user.role !== roleFilter) return false
                       if (activityFilter !== 'all') {
                         const activityStatus = getActivityStatus(user.lastProjectDate)
                         if (activityStatus !== activityFilter) return false
                       }
                       if (officeFilter !== 'all') {
                         const userOffices = [
                           user.office,
                           ...(user.salesOffice || []),
                           ...(user.officeAccess?.map(oa => oa.officeName) || [])
                         ].filter(Boolean)
                         if (!userOffices.includes(officeFilter)) return false
                       }
                       return true
                     }
                     return matches(child.user)
                   })
          }
          
          if (hasMatchingDescendants(node)) {
            newExpanded.add(node.user.id)
            findNodesToExpand(node.children)
          }
        })
      }
      
      findNodesToExpand(hierarchyTree)
      setExpandedNodes(newExpanded)
    }
  }, [debouncedSearchTerm, roleFilter, activityFilter, officeFilter, hierarchyTree, hasActiveFilters])

  const ROLE_MANAGERS: readonly UserRole[] = ['team_lead', 'office_leader', 'area_director', 'divisional', 'regional', 'super_admin'] as const
  const availableManagers = users.filter((user: User) => 
    ROLE_MANAGERS.includes(user.role)
  )
  const availableUsers = users.filter((user: User) =>
    !(['super_admin'] as const).includes(user.role as any) && user.is_active
  )

  return (
    <div className={className}>
      <div className="mb-6">
        <h3 className="text-lg font-semibold mb-2">User Hierarchy</h3>
        <p className="text-sm text-gray-600">
          Manage team lead assignments and organizational structure. Drag users to reassign, right-click for more options.
        </p>
      </div>

      {/* Search and Filter UI */}
      <Card className="mb-6">
        <CardContent className="p-4">
          <div className="space-y-4">
            {/* Search Bar */}
            <div className="relative">
              <Search className="absolute left-3 top-1/2 transform -translate-y-1/2 h-4 w-4 text-gray-400" />
              <Input
                placeholder="Search by name or email..."
                value={searchTerm}
                onChange={(e) => setSearchTerm(e.target.value)}
                className="pl-10"
              />
            </div>

            {/* Filters */}
            <div className="grid grid-cols-1 md:grid-cols-4 gap-4">
              <div>
                <Label htmlFor="role-filter">Role</Label>
                <Select value={roleFilter} onValueChange={setRoleFilter}>
                  <SelectTrigger>
                    <SelectValue placeholder="All roles" />
                  </SelectTrigger>
                  <SelectContent>
                    <SelectItem value="all">All roles</SelectItem>
                    <SelectItem value="super_admin">Super Admin</SelectItem>
                    <SelectItem value="regional">Regional</SelectItem>
                    <SelectItem value="divisional">Divisional</SelectItem>
                    <SelectItem value="area_director">Area Director</SelectItem>
                    <SelectItem value="office_leader">Office Leader</SelectItem>
                    <SelectItem value="team_lead">Team Lead</SelectItem>
                    <SelectItem value="closer">Closer</SelectItem>
                    <SelectItem value="setter">Setter</SelectItem>
                  </SelectContent>
                </Select>
              </div>

              <div>
                <Label htmlFor="activity-filter">Activity</Label>
                <Select value={activityFilter} onValueChange={setActivityFilter}>
                  <SelectTrigger>
                    <SelectValue placeholder="All activity" />
                  </SelectTrigger>
                  <SelectContent>
                    <SelectItem value="all">All activity</SelectItem>
                    <SelectItem value="active">Active (0-6 months)</SelectItem>
                    <SelectItem value="inactive">Inactive (6-12 months)</SelectItem>
                    <SelectItem value="dormant">Dormant (12+ months)</SelectItem>
                    <SelectItem value="unknown">No activity data</SelectItem>
                  </SelectContent>
                </Select>
              </div>

              <div>
                <Label htmlFor="office-filter">Office</Label>
                <Select value={officeFilter} onValueChange={setOfficeFilter}>
                  <SelectTrigger>
                    <SelectValue placeholder="All offices" />
                  </SelectTrigger>
                  <SelectContent>
                    <SelectItem value="all">All offices</SelectItem>
                    {uniqueOffices.map(office => (
                      <SelectItem key={office} value={office}>{office}</SelectItem>
                    ))}
                  </SelectContent>
                </Select>
              </div>

              <div className="flex items-end">
                {hasActiveFilters && (
                  <Button variant="outline" onClick={clearFilters} className="w-full">
                    <X className="h-4 w-4 mr-2" />
                    Clear Filters
                  </Button>
                )}
              </div>
            </div>

            {/* Results count */}
            <div className="text-sm text-gray-600">
              Showing {hierarchyTree.length} of {users.length} users
            </div>
          </div>
        </CardContent>
      </Card>

      <DndContext
        sensors={sensors}
        collisionDetection={closestCenter}
        onDragStart={handleDragStart}
        onDragEnd={handleDragEnd}
      >
        {hierarchyTree.length === 0 ? (
          <Card>
            <CardContent className="p-8 text-center">
              <Users className="h-12 w-12 mx-auto text-gray-400 mb-4" />
              <h3 className="text-lg font-medium mb-2">
                {hasActiveFilters ? 'No users found' : 'No hierarchy data'}
              </h3>
              <p className="text-gray-600 mb-4">
                {hasActiveFilters 
                  ? 'No users match the selected filters. Try adjusting your search criteria.'
                  : 'Start by assigning users to managers to build your organizational structure.'
                }
              </p>
              {hasActiveFilters ? (
                <Button onClick={clearFilters}>
                  <X className="h-4 w-4 mr-2" />
                  Clear Filters
                </Button>
              ) : (
                <Button onClick={() => setAssignDialogOpen(true)}>
                  <UserPlus className="h-4 w-4 mr-2" />
                  Create First Assignment
                </Button>
              )}
            </CardContent>
          </Card>
        ) : (
          <div className="space-y-2">
            {hierarchyTree.map(node => renderNode(node))}
          </div>
        )}

        <DragOverlay>
          {activeId ? (
            <Card className="opacity-90 shadow-lg">
              <CardContent className="p-4">
                <div className="flex items-center gap-3">
                  <Avatar size="md" className="bg-blue-500 text-white">
                    <AvatarFallback size="md">
                      {getInitials(users.find((u: User) => u.id === activeId)?.name || '')}
                    </AvatarFallback>
                  </Avatar>
                  <div>
                    <h4 className="font-medium">
                      {users.find((u: User) => u.id === activeId)?.name}
                    </h4>
                    <p className="text-sm text-gray-600">
                      {users.find((u: User) => u.id === activeId)?.email}
                    </p>
                  </div>
                </div>
              </CardContent>
            </Card>
          ) : null}
        </DragOverlay>
      </DndContext>

      {/* Assign Users Dialog */}
      <Dialog open={assignDialogOpen} onOpenChange={setAssignDialogOpen}>
        <DialogContent className="sm:max-w-md">
          <DialogHeader>
            <DialogTitle>Assign Users to Manager</DialogTitle>
            <DialogDescription>
              Select a manager and users to assign to them. You can also drag and drop users directly onto managers in the tree view.
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
                  {availableManagers.map((manager: User) => (
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
                    .filter((user: User) => user.id !== selectedManager)
                    .map((user: User) => (
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
              Are you sure you want to remove this user from their manager&apos;s team?
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

      {/* User Details Dialog */}
      <UserDetailsDialog
        userId={selectedUserId}
        isOpen={userDetailsOpen}
        onClose={() => {
          setUserDetailsOpen(false)
          setSelectedUserId(null)
        }}
      />
    </div>
  )
}
