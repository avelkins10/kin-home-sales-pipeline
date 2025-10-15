'use client'

import { useState, useMemo, useEffect } from 'react'
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query'
import { useDebounce } from 'use-debounce'
import { useSession } from 'next-auth/react'
import { DndContext, DragOverlay, useDraggable, useDroppable, closestCenter, KeyboardSensor, PointerSensor, useSensor, useSensors } from '@dnd-kit/core'
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card'
import { Button } from '@/components/ui/button'
import { Badge } from '@/components/ui/badge'
import { Input } from '@/components/ui/input'
import { Label } from '@/components/ui/label'
import { Avatar, AvatarFallback } from '@/components/ui/avatar'
import { Checkbox } from '@/components/ui/checkbox'
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
  AlertDialog,
  AlertDialogAction,
  AlertDialogCancel,
  AlertDialogContent,
  AlertDialogDescription,
  AlertDialogFooter,
  AlertDialogHeader,
  AlertDialogTitle,
} from '@/components/ui/alert-dialog'
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
import { UserMultiSelect } from './UserMultiSelect'
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
  Eye,
  MoreHorizontal,
  CheckSquare,
  AlertCircle,
  AlertTriangle,
  TrendingUp,
  UserX as UserXIcon,
  Expand,
  Minimize2,
  Download,
  RefreshCw,
  BarChart3,
  HelpCircle
} from 'lucide-react'
import { Alert, AlertDescription, AlertTitle } from '@/components/ui/alert'
import { Skeleton } from '@/components/ui/skeleton'
import {
  Tooltip,
  TooltipContent,
  TooltipProvider,
  TooltipTrigger,
} from '@/components/ui/tooltip'
import { 
  validateManagerRole,
  validateCircularHierarchy,
  validateUserAssignments,
  validateBulkAssignmentSize,
  getUnassignedUsers,
  getTeamSize,
  formatHierarchyError,
  canManageRole,
  getManagerRoleConstraints,
  getDescendants,
  type ValidationResult
} from '@/lib/utils/hierarchy-helpers'

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

// Draggable User Node Component (extracted to module level to avoid hooks violations)
interface DraggableUserNodeProps {
  node: HierarchyNode
  children: React.ReactNode
  bulkSelectionMode: boolean
  canManageHierarchies: boolean
}

function DraggableUserNode({ node, children, bulkSelectionMode, canManageHierarchies }: DraggableUserNodeProps) {
  const canDrag = ['setter', 'closer', 'team_lead'].includes(node.user.role) && !bulkSelectionMode && canManageHierarchies
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

// Droppable Manager Node Component (extracted to module level to avoid hooks violations)
interface DroppableManagerNodeProps {
  node: HierarchyNode
  children: React.ReactNode
  activeId: string | null
  users: User[]
  hierarchies: Hierarchy[]
  canDrop: (dragRole: UserRole, targetRole: UserRole) => boolean
}

function DroppableManagerNode({
  node,
  children,
  activeId,
  users,
  hierarchies,
  canDrop
}: DroppableManagerNodeProps) {
  const { isOver, setNodeRef } = useDroppable({
    id: node.user.id
  })

  // Compute validation result and message locally (don't set state during render)
  const validationResult = useMemo(() => {
    if (!activeId) return { isValid: false, message: null }

    const draggedUser = users.find((u: User) => u.id === activeId)
    if (!draggedUser) return { isValid: false, message: 'User not found' }

    // Check for self-assignment
    if (draggedUser.id === node.user.id) {
      return { isValid: false, message: 'Cannot assign to self' }
    }

    // Check for circular hierarchy
    const descendants = getDescendants(draggedUser.id, hierarchies)
    if (descendants.includes(node.user.id)) {
      return { isValid: false, message: 'Would create circular hierarchy' }
    }

    // Check role compatibility
    if (!canDrop(draggedUser.role, node.user.role)) {
      return { isValid: false, message: `${node.user.role} cannot manage ${draggedUser.role}` }
    }

    return { isValid: true, message: null }
  }, [activeId, users, node.user.id, hierarchies, canDrop])

  const getDropStyles = () => {
    if (!isOver) return ''
    if (validationResult.isValid) {
      return 'ring-2 ring-green-500 ring-opacity-50'
    } else {
      return 'ring-2 ring-red-500 ring-opacity-50 cursor-not-allowed'
    }
  }

  return (
    <div
      ref={setNodeRef}
      className={`relative ${getDropStyles()}`}
    >
      {children}
      {/* Droppable hover tooltip */}
      {isOver && activeId && (
        <div
          className="absolute -top-10 left-1/2 transform -translate-x-1/2 bg-gray-900 text-white text-xs px-2 py-1 rounded shadow-lg z-50 whitespace-nowrap"
          aria-live="polite"
        >
          {validationResult.isValid ? (
            `Drop here to assign ${users.find((u: User) => u.id === activeId)?.name} to ${node.user.name}`
          ) : (
            validationResult.message || 'Cannot drop here'
          )}
        </div>
      )}
    </div>
  )
}

export function HierarchyTreeView({ className }: HierarchyTreeViewProps) {
  const { data: session } = useSession()
  const [expandedNodes, setExpandedNodes] = useState<Set<string>>(new Set())
  const [assignDialogOpen, setAssignDialogOpen] = useState(false)
  const [selectedManager, setSelectedManager] = useState<string>('')
  const [selectedUsers, setSelectedUsers] = useState<string[]>([])
  const [removeDialogOpen, setRemoveDialogOpen] = useState(false)
  const [removalTarget, setRemovalTarget] = useState<{ managerId: string; userId: string } | null>(null)
  const [userDetailsOpen, setUserDetailsOpen] = useState(false)
  const [selectedUserId, setSelectedUserId] = useState<string | null>(null)

  // Client-only rendering flag to prevent hydration mismatch
  const [isMounted, setIsMounted] = useState(false)

  // Search and filter state
  const [searchTerm, setSearchTerm] = useState('')
  const [roleFilter, setRoleFilter] = useState<string>('all')
  const [activityFilter, setActivityFilter] = useState<string>('all')
  const [officeFilter, setOfficeFilter] = useState<string>('all')
  const [debouncedSearchTerm] = useDebounce(searchTerm, 300)

  // Enhanced state for new features
  const [bulkSelectionMode, setBulkSelectionMode] = useState(false)
  const [bulkSelectedUsers, setBulkSelectedUsers] = useState<string[]>([])
  const [expandedOffices, setExpandedOffices] = useState<Set<string>>(new Set())
  const [expandedTeamInsights, setExpandedTeamInsights] = useState<Set<string>>(new Set())
  const [validationErrors, setValidationErrors] = useState<string[]>([])
  const [validationWarnings, setValidationWarnings] = useState<string[]>([])
  const [showToolbar, setShowToolbar] = useState(true)
  const [dragValidationMessage, setDragValidationMessage] = useState<string | null>(null)
  const [bulkConfirmDialogOpen, setBulkConfirmDialogOpen] = useState(false)
  const [helpDialogOpen, setHelpDialogOpen] = useState(false)
  const [showUnassignedOnly, setShowUnassignedOnly] = useState(false)

  // Drag and drop state
  const [activeId, setActiveId] = useState<string | null>(null)
  const sensors = useSensors(
    useSensor(PointerSensor),
    useSensor(KeyboardSensor)
  )

  const queryClient = useQueryClient()

  // Check if current user has permission to manage hierarchies
  const canManageHierarchies = session?.user?.role === 'super_admin'

  // Set mounted flag after initial render to prevent hydration mismatch
  useEffect(() => {
    setIsMounted(true)
  }, [])

  // Keyboard shortcuts
  useEffect(() => {
    const handleKeyDown = (e: KeyboardEvent) => {
      if ((e.metaKey || e.ctrlKey) && e.key === 'k') {
        e.preventDefault();
        document.getElementById('hierarchy-search')?.focus();
      }
      if ((e.metaKey || e.ctrlKey) && e.key === 'e') {
        e.preventDefault();
        expandAll();
        toast.success('All nodes expanded');
      }
      if ((e.metaKey || e.ctrlKey) && e.shiftKey && e.key.toLowerCase() === 'e') {
        e.preventDefault();
        collapseAll();
        toast.success('All nodes collapsed');
      }
      if ((e.metaKey || e.ctrlKey) && e.key === 'b') {
        e.preventDefault();
        setBulkSelectionMode(!bulkSelectionMode);
      }
      if ((e.metaKey || e.ctrlKey) && e.key === 'r') {
        e.preventDefault();
        queryClient.invalidateQueries({ queryKey: ['users'] });
        queryClient.invalidateQueries({ queryKey: ['hierarchies'] });
      }
    };
    window.addEventListener('keydown', handleKeyDown);
    return () => window.removeEventListener('keydown', handleKeyDown);
  }, [bulkSelectionMode, queryClient]);

  // Fetch all users
  const { data: users = [], isLoading: isLoadingUsers, error: usersError } = useQuery({
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
  const buildHierarchyTree = useMemo((): HierarchyNode[] => {
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
      // Unassigned filter
      if (showUnassignedOnly) {
        const isUnassigned = !managerMap.has(user.id)
        if (!isUnassigned) return false
      }

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
  }, [users, hierarchies, debouncedSearchTerm, roleFilter, activityFilter, officeFilter, showUnassignedOnly])

  // Assign users mutation
  const assignMutation = useMutation({
    mutationFn: async ({ managerId, userIds }: { managerId: string; userIds: string[] }) => {
      const response = await fetch(`${getBaseUrl()}/api/admin/hierarchies`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ managerId, userIds }),
      })

      if (!response.ok) {
        if (response.status === 401) {
          throw new Error('Authentication required. Please log in again.')
        } else if (response.status === 403) {
          throw new Error('Insufficient permissions. Only super admins can assign users.')
        }
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
      // Parse backend error variants to show specific guidance
      const errorMessage = error.message.toLowerCase()
      
      if (errorMessage.includes('circular')) {
        toast.error('Circular hierarchy detected', {
          description: 'Cannot assign user as it would create a circular management chain. Please remove conflicting relationships first.',
        })
      } else if (errorMessage.includes('already managed')) {
        toast.error('User already managed', {
          description: 'One or more users are already managed by someone else. Please remove them from their current manager first.',
        })
      } else if (errorMessage.includes('validation failed')) {
        toast.error('Validation failed', {
          description: error.message,
        })
      } else if (errorMessage.includes('manager not found')) {
        toast.error('Manager not found', {
          description: 'The selected manager could not be found. Please refresh and try again.',
        })
      } else if (errorMessage.includes('role is not appropriate')) {
        toast.error('Invalid manager role', {
          description: 'The selected user does not have a role that can manage others.',
        })
      } else {
        toast.error('Failed to assign users', {
          description: error.message,
        })
      }
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
        if (response.status === 401) {
          throw new Error('Authentication required. Please log in again.')
        } else if (response.status === 403) {
          throw new Error('Insufficient permissions. Only super admins can remove assignments.')
        }
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

  const expandAll = () => {
    const allUserIds = new Set<string>();
    const collectIds = (nodes: HierarchyNode[]) => {
      nodes.forEach(node => {
        if (node.children.length > 0) {
          allUserIds.add(node.user.id);
          collectIds(node.children);
        }
      });
    };
    collectIds(hierarchyTree);
    setExpandedNodes(allUserIds);
  };

  const collapseAll = () => {
    setExpandedNodes(new Set());
  };

  const toggleOfficeExpansion = (userId: string) => {
    const newExpanded = new Set(expandedOffices);
    if (newExpanded.has(userId)) {
      newExpanded.delete(userId);
    } else {
      newExpanded.add(userId);
    }
    setExpandedOffices(newExpanded);
  };

  const toggleTeamInsights = (userId: string) => {
    const newExpanded = new Set(expandedTeamInsights);
    if (newExpanded.has(userId)) {
      newExpanded.delete(userId);
    } else {
      newExpanded.add(userId);
    }
    setExpandedTeamInsights(newExpanded);
  };

  // Calculate hierarchy statistics
  const hierarchyStats = useMemo(() => {
    const stats = {
      totalUsers: users.length,
      totalManagers: users.filter((u: User) => ['team_lead', 'office_leader', 'area_director', 'divisional', 'regional', 'super_admin'].includes(u.role)).length,
      totalReps: users.filter((u: User) => ['closer', 'setter'].includes(u.role)).length,
      unassignedUsers: getUnassignedUsers(users, hierarchies).length,
      inactiveUsers: users.filter((u: User) => !u.is_active).length,
      averageTeamSize: 0,
      largestTeam: { managerId: '', managerName: '', size: 0 },
    };
    
    // Calculate average team size and largest team
    const managerTeamSizes = new Map<string, number>();
    hierarchies.forEach((h: Hierarchy) => {
      managerTeamSizes.set(h.manager_id, (managerTeamSizes.get(h.manager_id) || 0) + 1);
    });
    
    if (managerTeamSizes.size > 0) {
      const sizes = Array.from(managerTeamSizes.values());
      stats.averageTeamSize = sizes.reduce((a, b) => a + b, 0) / sizes.length;
      
      const largest = Array.from(managerTeamSizes.entries())
        .sort((a, b) => b[1] - a[1])[0];
      if (largest) {
        const manager = users.find((u: User) => u.id === largest[0]);
        stats.largestTeam = {
          managerId: largest[0],
          managerName: manager?.name || 'Unknown',
          size: largest[1],
        };
      }
    }
    
    return stats;
  }, [users, hierarchies]);

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
    // Only allow dragging for assignable roles
    if (!['setter', 'closer', 'team_lead'].includes(dragRole)) {
      return false
    }
    
    // Use canManageRole from hierarchy-helpers to check if target can manage drag role
    return canManageRole(targetRole, dragRole)
  }

  // Drag and drop handlers
  const handleDragStart = (event: any) => {
    setActiveId(event.active.id)
  }

  const handleDragEnd = (event: any) => {
    const { active, over } = event
    setActiveId(null)
    setDragValidationMessage(null)

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
    const descendants = getDescendants(draggedUser.id, hierarchies)
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
    setShowUnassignedOnly(false)
  }

  // Handle statistics tile clicks
  const handleUnassignedClick = () => {
    setShowUnassignedOnly(!showUnassignedOnly)
    if (!showUnassignedOnly) {
      // Clear other filters when showing unassigned only
      setSearchTerm('')
      setRoleFilter('all')
      setActivityFilter('all')
      setOfficeFilter('all')
    }
  }

  const handleInactiveClick = () => {
    setActivityFilter(activityFilter === 'inactive' ? 'all' : 'inactive')
  }

  const handleLargestTeamClick = () => {
    if (hierarchyStats.largestTeam.managerId) {
      // Expand the largest team manager node
      setExpandedNodes(prev => new Set([...Array.from(prev), hierarchyStats.largestTeam.managerId]))
      // Scroll to the manager (this would need a ref in a real implementation)
      toast.success(`Expanded ${hierarchyStats.largestTeam.managerName}'s team`)
    }
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
  const hasActiveFilters = searchTerm || roleFilter !== 'all' || activityFilter !== 'all' || officeFilter !== 'all' || showUnassignedOnly

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

  const renderNode = (node: HierarchyNode, isFirstChild = false, isLastChild = false) => {
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
    const personalOffice = node.user.office
    const managedOffices = node.user.officeAccess || []
    const allOffices = [
      ...(node.user.salesOffice || []),
      ...(node.user.officeAccess?.map(oa => oa.officeName) || [])
    ].filter(Boolean)
    const hasMultipleOffices = allOffices.length > 1
    const isOfficeExpanded = expandedOffices.has(node.user.id)
    const isTeamInsightsExpanded = expandedTeamInsights.has(node.user.id)

    const nodeContent = (
      <ContextMenu>
        <ContextMenuTrigger asChild>
          <div className={`relative ${node.level > 0 ? 'ml-12' : ''}`}>
            {/* Tree connector lines - using CSS pseudo-elements for proper alignment */}
            {node.level > 0 && (
              <div className={`tree-node tree-connector ${
                isFirstChild ? 'tree-first-child' : ''
              } ${isLastChild ? 'tree-last-child' : ''}`} />
            )}
            
            <Card 
              className="mb-2 pl-8 transition-all hover:shadow-md cursor-pointer"
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
                        suppressHydrationWarning
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
                        
                        {/* Personal office display */}
                        {personalOffice && (
                          <div className="flex items-center gap-1">
                            <MapPin className="h-3 w-3" />
                            {personalOffice}
                          </div>
                        )}
                        
                        {/* Managed offices badge */}
                        {managedOffices.length > 0 && (
                          <Badge 
                            variant="secondary" 
                            className="cursor-pointer hover:bg-secondary/80"
                            onClick={(e) => {
                              e.stopPropagation();
                              toggleOfficeExpansion(node.user.id);
                            }}
                          >
                            <Building className="h-3 w-3 mr-1" />
                            Manages {managedOffices.length} office{managedOffices.length !== 1 ? 's' : ''}
                            {isOfficeExpanded ? <ChevronDown className="h-3 w-3 ml-1" /> : <ChevronRight className="h-3 w-3 ml-1" />}
                          </Badge>
                        )}
                        
                        {/* Multiple offices badge (legacy) */}
                        {hasMultipleOffices && managedOffices.length === 0 && (
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
                          <Badge 
                            variant="outline" 
                            className="cursor-pointer hover:bg-accent"
                            onClick={(e) => {
                              e.stopPropagation();
                              toggleTeamInsights(node.user.id);
                            }}
                          >
                            <Users className="h-3 w-3 mr-1" />
                            Manages {managedCount} {managedCount === 1 ? 'user' : 'users'}
                            {isTeamInsightsExpanded ? <ChevronDown className="h-3 w-3 ml-1" /> : <ChevronRight className="h-3 w-3 ml-1" />}
                          </Badge>
                        )}
                      </div>
                      
                      {/* Expanded office section */}
                      {isOfficeExpanded && managedOffices.length > 0 && (
                        <div className="mt-3 p-3 bg-gray-50 rounded-md">
                          <h5 className="text-sm font-medium mb-2">Managed Offices</h5>
                          <div className="space-y-2">
                            {managedOffices.map((office, index) => (
                              <div key={index} className="flex items-center justify-between text-sm">
                                <div className="flex items-center gap-2">
                                  <Building className="h-3 w-3" />
                                  {office.officeName}
                                </div>
                                <Badge 
                                  variant={
                                    office.accessLevel === 'admin' ? 'destructive' :
                                    office.accessLevel === 'manage' ? 'default' : 'secondary'
                                  }
                                  className="text-xs"
                                >
                                  {office.accessLevel}
                                </Badge>
                              </div>
                            ))}
                          </div>
                        </div>
                      )}
                      
                      {/* Expanded team insights section */}
                      {isTeamInsightsExpanded && managedCount > 0 && (
                        <div className="mt-3 p-3 bg-gray-50 rounded-md">
                          <h5 className="text-sm font-medium mb-2">Team Insights</h5>
                          <div className="grid grid-cols-2 gap-4 text-sm">
                            <div>
                              <span className="text-gray-600">Team Size:</span>
                              <span className="ml-1 font-medium">{managedCount}</span>
                            </div>
                            <div>
                              <span className="text-gray-600">Active:</span>
                              <span className="ml-1 font-medium">
                                {node.children.filter(child => child.user.is_active).length}
                              </span>
                            </div>
                          </div>
                        </div>
                      )}
                    </div>
                  </div>
                </div>
                
                <div className="flex items-center gap-2">
                  {bulkSelectionMode && canManageHierarchies && ['setter', 'closer', 'team_lead'].includes(node.user.role) && (
                    <Checkbox
                      checked={bulkSelectedUsers.includes(node.user.id)}
                      onCheckedChange={(checked) => {
                        if (checked) {
                          setBulkSelectedUsers([...bulkSelectedUsers, node.user.id])
                        } else {
                          setBulkSelectedUsers(bulkSelectedUsers.filter(id => id !== node.user.id))
                        }
                      }}
                    />
                  )}
                  {!bulkSelectionMode && canManageHierarchies && (
                    <>
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
                    </>
                  )}
                  {!bulkSelectionMode && !canManageHierarchies && (
                    <div className="text-xs text-gray-500 px-2 py-1">
                      Super admin required
                    </div>
                  )}
                </div>
              </div>
            </CardContent>
          </Card>
          </div>
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
                <UserXIcon className="h-4 w-4 mr-2" />
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
      <div key={node.user.id}>
        <DroppableManagerNode
          node={node}
          activeId={activeId}
          users={users}
          hierarchies={hierarchies}
          canDrop={canDrop}
        >
          <DraggableUserNode
            node={node}
            bulkSelectionMode={bulkSelectionMode}
            canManageHierarchies={canManageHierarchies}
          >
            {nodeContent}
          </DraggableUserNode>
        </DroppableManagerNode>

        {isExpanded && hasChildren && (
          <div>
            {node.children.map((child, index) =>
              renderNode(child, index === 0, index === node.children.length - 1)
            )}
          </div>
        )}
      </div>
    )
  }

  const hierarchyTree = buildHierarchyTree

  // Check for inactive manager warning
  useEffect(() => {
    const warnings: string[] = []
    if (selectedManager) {
      const mgr = users.find((u: User) => u.id === selectedManager)
      if (mgr && !mgr.is_active) {
        warnings.push(`${mgr.name || mgr.email} is inactive; assignments may have limited effect`)
      }
    }
    setValidationWarnings(warnings)
  }, [selectedManager, users])

  // Validation logic for assign dialog
  useEffect(() => {
    if (selectedManager && selectedUsers.length > 0) {
      const manager = users.find((u: User) => u.id === selectedManager);
      const selectedUserObjects = users.filter((u: User) => selectedUsers.includes(u.id));
      
      const errors: string[] = [];
      const warnings: string[] = [];
      
      // Check if manager is inactive
      if (manager && !manager.is_active) {
        warnings.push(`${manager.name} is inactive and may not be able to manage users effectively`);
      }
      
      // Validate manager role
      const managerValidation = validateManagerRole(manager, selectedUserObjects);
      if (!managerValidation.valid) {
        errors.push(managerValidation.error || 'Invalid manager role');
      }
      
      // Validate user assignments
      const userValidation = validateUserAssignments(selectedUsers, users, hierarchies);
      if (!userValidation.valid) {
        errors.push(...(userValidation.errors || []));
      }
      if (userValidation.warnings) {
        warnings.push(...userValidation.warnings);
      }
      
      // Validate circular hierarchy
      const circularValidation = validateCircularHierarchy(selectedManager, selectedUsers, hierarchies);
      if (!circularValidation.valid) {
        errors.push(circularValidation.error || 'Circular hierarchy detected');
      }
      
      // Validate bulk assignment size
      const sizeValidation = validateBulkAssignmentSize(selectedUsers.length);
      if (!sizeValidation.valid) {
        errors.push(sizeValidation.error || 'Too many users selected');
      }
      if (sizeValidation.warnings) {
        warnings.push(...sizeValidation.warnings);
      }
      
      setValidationErrors(errors);
      // Merge with existing warnings from inactive manager check
      setValidationWarnings(prev => [...prev, ...warnings]);
    } else {
      setValidationErrors([]);
      // Don't clear warnings here - let the inactive manager effect handle it
    }
  }, [selectedManager, selectedUsers, users, hierarchies]);

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
  const availableManagers = useMemo(() => 
    users.filter((user: User) => ROLE_MANAGERS.includes(user.role)),
    [users]
  )
  const availableUsers = useMemo(() =>
    users.filter((user: User) =>
      !(['super_admin'] as const).includes(user.role as any) && user.is_active
    ),
    [users]
  )

  // Compute role filter for UserMultiSelect based on selected manager
  const userRoleFilterForManager = useMemo(() => {
    if (!selectedManager) return undefined
    const manager = users.find((u: User) => u.id === selectedManager)
    if (!manager) return undefined
    return getManagerRoleConstraints(manager.role).canManage
  }, [selectedManager, users])

  // Loading state or not mounted (prevents hydration mismatch)
  if (isLoadingUsers || !isMounted) {
    return (
      <div className={className}>
        <Card>
          <CardContent className="p-8">
            <div className="space-y-4">
              <Skeleton className="h-8 w-64" />
              <Skeleton className="h-12 w-full" />
              <Skeleton className="h-32 w-full" />
              <Skeleton className="h-32 w-full" />
            </div>
          </CardContent>
        </Card>
      </div>
    );
  }

  // Error state
  if (usersError) {
    return (
      <Alert variant="destructive">
        <AlertCircle className="h-4 w-4" />
        <AlertTitle>Failed to Load Hierarchy</AlertTitle>
        <AlertDescription>
          {usersError.message}
          <Button onClick={() => queryClient.invalidateQueries()} className="mt-2">
            Retry
          </Button>
        </AlertDescription>
      </Alert>
    );
  }

  return (
    <div className={className}>
      <div className="mb-6">
        <div className="flex items-center justify-between">
          <h3 className="text-lg font-semibold">User Hierarchy</h3>
          <Button 
            variant="ghost" 
            size="sm" 
            className="h-8 w-8 p-0"
            onClick={() => setHelpDialogOpen(true)}
            aria-label="Open help"
          >
            <HelpCircle className="h-4 w-4" />
          </Button>
        </div>
        <p className="text-sm text-gray-600">
          Manage team lead assignments and organizational structure. Drag users to reassign, right-click for more options.
        </p>
      </div>

      {/* Statistics Panel */}
      <Card className="mb-6">
        <CardHeader className="pb-3">
          <CardTitle className="text-base flex items-center gap-2">
            <BarChart3 className="h-4 w-4" />
            Hierarchy Overview
          </CardTitle>
        </CardHeader>
        <CardContent>
          <div className="grid grid-cols-2 md:grid-cols-4 lg:grid-cols-7 gap-4">
            <TooltipProvider>
              <Tooltip>
                <TooltipTrigger asChild>
                  <div className="text-center cursor-help">
                    <div className="text-2xl font-bold text-blue-600">{hierarchyStats.totalUsers}</div>
                    <div className="text-xs text-gray-600 flex items-center justify-center gap-1">
                      Total Users
                      <HelpCircle className="h-3 w-3" />
                    </div>
                  </div>
                </TooltipTrigger>
                <TooltipContent>
                  <p>Total number of users in the system, including active and inactive</p>
                </TooltipContent>
              </Tooltip>
            </TooltipProvider>
            
            <TooltipProvider>
              <Tooltip>
                <TooltipTrigger asChild>
                  <div className="text-center cursor-help">
                    <div className="text-2xl font-bold text-green-600">{hierarchyStats.totalManagers}</div>
                    <div className="text-xs text-gray-600 flex items-center justify-center gap-1">
                      Managers
                      <HelpCircle className="h-3 w-3" />
                    </div>
                  </div>
                </TooltipTrigger>
                <TooltipContent>
                  <p>Users with manager roles: team_lead, office_leader, area_director, divisional, regional, super_admin</p>
                </TooltipContent>
              </Tooltip>
            </TooltipProvider>
            
            <TooltipProvider>
              <Tooltip>
                <TooltipTrigger asChild>
                  <div className="text-center cursor-help">
                    <div className="text-2xl font-bold text-purple-600">{hierarchyStats.totalReps}</div>
                    <div className="text-xs text-gray-600 flex items-center justify-center gap-1">
                      Reps
                      <HelpCircle className="h-3 w-3" />
                    </div>
                  </div>
                </TooltipTrigger>
                <TooltipContent>
                  <p>Users with rep roles: closer, setter</p>
                </TooltipContent>
              </Tooltip>
            </TooltipProvider>
            
            <TooltipProvider>
              <Tooltip>
                <TooltipTrigger asChild>
                  <div 
                    className={`text-center cursor-pointer transition-colors hover:bg-gray-50 rounded-lg p-2 ${
                      showUnassignedOnly ? 'bg-blue-50 border border-blue-200' : ''
                    }`}
                    onClick={handleUnassignedClick}
                    onKeyDown={(e) => {
                      if (e.key === 'Enter' || e.key === ' ') {
                        e.preventDefault()
                        handleUnassignedClick()
                      }
                    }}
                    role="button"
                    tabIndex={0}
                    aria-label="Filter unassigned users"
                  >
                    <div className={`text-2xl font-bold ${hierarchyStats.unassignedUsers > 0 ? 'text-red-600' : 'text-gray-600'}`}>
                      {hierarchyStats.unassignedUsers}
                    </div>
                    <div className="text-xs text-gray-600 flex items-center justify-center gap-1">
                      Unassigned
                      <HelpCircle className="h-3 w-3" />
                    </div>
                  </div>
                </TooltipTrigger>
                <TooltipContent>
                  <p>Users without a manager assignment. Click to filter and view unassigned users.</p>
                </TooltipContent>
              </Tooltip>
            </TooltipProvider>
            
            <TooltipProvider>
              <Tooltip>
                <TooltipTrigger asChild>
                  <div 
                    className={`text-center cursor-pointer transition-colors hover:bg-gray-50 rounded-lg p-2 ${
                      activityFilter === 'inactive' ? 'bg-blue-50 border border-blue-200' : ''
                    }`}
                    onClick={handleInactiveClick}
                    onKeyDown={(e) => {
                      if (e.key === 'Enter' || e.key === ' ') {
                        e.preventDefault()
                        handleInactiveClick()
                      }
                    }}
                    role="button"
                    tabIndex={0}
                    aria-label="Filter inactive users"
                  >
                    <div className={`text-2xl font-bold ${hierarchyStats.inactiveUsers > 0 ? 'text-yellow-600' : 'text-gray-600'}`}>
                      {hierarchyStats.inactiveUsers}
                    </div>
                    <div className="text-xs text-gray-600 flex items-center justify-center gap-1">
                      Inactive
                      <HelpCircle className="h-3 w-3" />
                    </div>
                  </div>
                </TooltipTrigger>
                <TooltipContent>
                  <p>Users with is_active = false. These users cannot log in but their projects remain visible to managers.</p>
                </TooltipContent>
              </Tooltip>
            </TooltipProvider>
            
            <TooltipProvider>
              <Tooltip>
                <TooltipTrigger asChild>
                  <div className="text-center cursor-help">
                    <div className="text-2xl font-bold text-indigo-600" suppressHydrationWarning>
                      {hierarchyStats.averageTeamSize.toFixed(1)}
                    </div>
                    <div className="text-xs text-gray-600 flex items-center justify-center gap-1">
                      Avg Team Size
                      <HelpCircle className="h-3 w-3" />
                    </div>
                  </div>
                </TooltipTrigger>
                <TooltipContent>
                  <p>Average number of direct reports per manager. Recommended: 5-10 for team leads.</p>
                </TooltipContent>
              </Tooltip>
            </TooltipProvider>
            
            <TooltipProvider>
              <Tooltip>
                <TooltipTrigger asChild>
                  <div 
                    className="text-center cursor-pointer transition-colors hover:bg-gray-50 rounded-lg p-2"
                    onClick={handleLargestTeamClick}
                    onKeyDown={(e) => {
                      if (e.key === 'Enter' || e.key === ' ') {
                        e.preventDefault()
                        handleLargestTeamClick()
                      }
                    }}
                    role="button"
                    tabIndex={0}
                    aria-label="Expand largest team"
                  >
                    <div className="text-2xl font-bold text-orange-600">
                      {hierarchyStats.largestTeam.size}
                    </div>
                    <div className="text-xs text-gray-600 flex items-center justify-center gap-1">
                      Largest Team
                      <HelpCircle className="h-3 w-3" />
                    </div>
                  </div>
                </TooltipTrigger>
                <TooltipContent>
                  <p>Manager with the most direct reports: {hierarchyStats.largestTeam.managerName} ({hierarchyStats.largestTeam.size} users). Click to view their team.</p>
                </TooltipContent>
              </Tooltip>
            </TooltipProvider>
          </div>
        </CardContent>
      </Card>

      {/* Toolbar */}
      {showToolbar && (
        <Card className="mb-6">
          <CardContent className="p-4">
            <div className="flex flex-wrap gap-2">
              <TooltipProvider>
                <Tooltip>
                  <TooltipTrigger asChild>
                    <Button
                      variant="outline"
                      size="sm"
                      onClick={() => setBulkSelectionMode(!bulkSelectionMode)}
                      disabled={!canManageHierarchies}
                      className={bulkSelectionMode ? 'bg-blue-50 border-blue-200' : ''}
                    >
                      <CheckSquare className="h-4 w-4 mr-2" />
                      Bulk Select
                    </Button>
                  </TooltipTrigger>
                  <TooltipContent>
                    <p>Enable bulk selection mode to select multiple users with checkboxes. Keyboard shortcut: ⌘B</p>
                  </TooltipContent>
                </Tooltip>
              </TooltipProvider>
              
              <TooltipProvider>
                <Tooltip>
                  <TooltipTrigger asChild>
                    <Button variant="outline" size="sm" onClick={expandAll}>
                      <Expand className="h-4 w-4 mr-2" />
                      Expand All
                    </Button>
                  </TooltipTrigger>
                  <TooltipContent>
                    <p>Expand all manager nodes to show their teams. Keyboard shortcut: ⌘E</p>
                  </TooltipContent>
                </Tooltip>
              </TooltipProvider>
              
              <TooltipProvider>
                <Tooltip>
                  <TooltipTrigger asChild>
                    <Button variant="outline" size="sm" onClick={collapseAll}>
                      <Minimize2 className="h-4 w-4 mr-2" />
                      Collapse All
                    </Button>
                  </TooltipTrigger>
                  <TooltipContent>
                    <p>Collapse all manager nodes to hide their teams. Keyboard shortcut: ⌘⇧E</p>
                  </TooltipContent>
                </Tooltip>
              </TooltipProvider>
              
              <TooltipProvider>
                <Tooltip>
                  <TooltipTrigger asChild>
                    <Button 
                      variant="outline" 
                      size="sm" 
                      onClick={() => {
                        queryClient.invalidateQueries({ queryKey: ['users'] });
                        queryClient.invalidateQueries({ queryKey: ['hierarchies'] });
                      }}
                    >
                      <RefreshCw className="h-4 w-4 mr-2" />
                      Refresh
                    </Button>
                  </TooltipTrigger>
                  <TooltipContent>
                    <p>Reload hierarchy data from the database. Keyboard shortcut: ⌘R</p>
                  </TooltipContent>
                </Tooltip>
              </TooltipProvider>
              <TooltipProvider>
                <Tooltip>
                  <TooltipTrigger asChild>
                    <Button variant="outline" size="sm" onClick={() => {
                      // Enhanced export functionality - flatten hierarchy with all descendants
                      const flattenHierarchy = (nodes: HierarchyNode[], managerName = ''): Array<{
                        name: string;
                        email: string;
                        role: string;
                        manager: string;
                        office: string;
                        managedUsers: string;
                        status: string;
                      }> => {
                        const result: Array<{
                          name: string;
                          email: string;
                          role: string;
                          manager: string;
                          office: string;
                          managedUsers: string;
                          status: string;
                        }> = [];
                        
                        nodes.forEach(node => {
                          // Add current node
                          result.push({
                            name: node.user.name,
                            email: node.user.email,
                            role: node.user.role,
                            manager: managerName,
                            office: node.user.office || '',
                            managedUsers: node.children.map(child => child.user.name).join('; '),
                            status: node.user.is_active ? 'Active' : 'Inactive'
                          });
                          
                          // Recursively add children
                          if (node.children.length > 0) {
                            result.push(...flattenHierarchy(node.children, node.user.name));
                          }
                        });
                        
                        return result;
                      };
                      
                      const csvData = flattenHierarchy(hierarchyTree);
                      const csv = 'Name,Email,Role,Manager,Office,Managed Users,Status\n' + 
                        csvData.map(row => Object.values(row).map(val => `"${val}"`).join(',')).join('\n');
                      const blob = new Blob([csv], { type: 'text/csv' });
                      const url = URL.createObjectURL(blob);
                      const a = document.createElement('a');
                      a.href = url;
                      a.download = 'hierarchy-export.csv';
                      a.click();
                      URL.revokeObjectURL(url);
                    }}>
                      <Download className="h-4 w-4 mr-2" />
                      Export
                    </Button>
                  </TooltipTrigger>
                  <TooltipContent>
                    <p>Export current hierarchy view as CSV file. Respects active filters.</p>
                  </TooltipContent>
                </Tooltip>
              </TooltipProvider>
            </div>
          </CardContent>
        </Card>
      )}

      {/* Search and Filter UI */}
      <Card className="mb-6">
        <CardContent className="p-4">
          <div className="space-y-4">
            {/* Search Bar */}
            <div className="relative">
              <Search className="absolute left-3 top-1/2 transform -translate-y-1/2 h-4 w-4 text-gray-400" />
              <Input
                id="hierarchy-search"
                placeholder="Search by name or email... (⌘K)"
                value={searchTerm}
                onChange={(e) => setSearchTerm(e.target.value)}
                className="pl-10"
              />
              <TooltipProvider>
                <Tooltip>
                  <TooltipTrigger asChild>
                    <HelpCircle className="absolute right-3 top-1/2 transform -translate-y-1/2 h-4 w-4 text-gray-400 cursor-help" />
                  </TooltipTrigger>
                  <TooltipContent>
                    <p>Search by name or email. Press ⌘K to focus. Search is case-insensitive and searches both fields.</p>
                  </TooltipContent>
                </Tooltip>
              </TooltipProvider>
            </div>

            {/* Filters */}
            <div className="grid grid-cols-1 md:grid-cols-4 gap-4">
              <div>
                <div className="flex items-center gap-1 mb-1">
                  <Label htmlFor="role-filter">Role</Label>
                  <TooltipProvider>
                    <Tooltip>
                      <TooltipTrigger asChild>
                        <HelpCircle className="h-3 w-3 text-gray-400 cursor-help" />
                      </TooltipTrigger>
                      <TooltipContent>
                        <p>Filter users by role. Managers include: team_lead, office_leader, area_director, divisional, regional, super_admin. Reps include: closer, setter.</p>
                      </TooltipContent>
                    </Tooltip>
                  </TooltipProvider>
                </div>
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
                <div className="flex items-center gap-1 mb-1">
                  <Label htmlFor="activity-filter">Activity</Label>
                  <TooltipProvider>
                    <Tooltip>
                      <TooltipTrigger asChild>
                        <HelpCircle className="h-3 w-3 text-gray-400 cursor-help" />
                      </TooltipTrigger>
                      <TooltipContent>
                        <p>Filter by last project activity date. Active = 0-6 months, Inactive = 6-12 months, Dormant = 12+ months.</p>
                      </TooltipContent>
                    </Tooltip>
                  </TooltipProvider>
                </div>
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
                <div className="flex items-center gap-1 mb-1">
                  <Label htmlFor="office-filter">Office</Label>
                  <TooltipProvider>
                    <Tooltip>
                      <TooltipTrigger asChild>
                        <HelpCircle className="h-3 w-3 text-gray-400 cursor-help" />
                      </TooltipTrigger>
                      <TooltipContent>
                        <p>Filter users by their personal office. For managers, this shows their primary office, not managed offices.</p>
                      </TooltipContent>
                    </Tooltip>
                  </TooltipProvider>
                </div>
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
          <div className="space-y-4">
            <div className="space-y-2">
              {hierarchyTree.map(node => renderNode(node))}
            </div>
            
            {/* Unassigned Users Section */}
            {(() => {
              const unassignedUsers = getUnassignedUsers(users, hierarchies);
              if (unassignedUsers.length > 0) {
                return (
                  <Card>
                    <CardHeader className="pb-3">
                      <CardTitle className="text-base flex items-center gap-2">
                        <UserXIcon className="h-4 w-4" />
                        Unassigned Users ({unassignedUsers.length})
                        <TooltipProvider>
                          <Tooltip>
                            <TooltipTrigger asChild>
                              <HelpCircle className="h-4 w-4 text-gray-400 cursor-help" aria-label="Unassigned users help" />
                            </TooltipTrigger>
                            <TooltipContent>
                              <p>Users without a manager assignment. These users can still log in and see their own projects, but no manager can see their projects. Assign them to a manager to enable team visibility.</p>
                            </TooltipContent>
                          </Tooltip>
                        </TooltipProvider>
                      </CardTitle>
                    </CardHeader>
                    <CardContent>
                      <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-3">
                        {unassignedUsers.map(user => (
                          <Card key={user.id} className="p-3">
                            <div className="flex items-center gap-3">
                              <Avatar size="sm" className={`${getAvatarColor(user.role)} ${getAvatarTextColor()}`}>
                                <AvatarFallback size="sm">
                                  {getInitials(user.name)}
                                </AvatarFallback>
                              </Avatar>
                              <div className="flex-1 min-w-0">
                                <div className="font-medium text-sm truncate">{user.name}</div>
                                <div className="text-xs text-gray-600 truncate">{user.email}</div>
                                <Badge variant={getRoleBadgeVariant(user.role)} className="text-xs mt-1">
                                  {getRoleDisplayName(user.role)}
                                </Badge>
                              </div>
                              <TooltipProvider>
                                <Tooltip>
                                  <TooltipTrigger asChild>
                                    <Button
                                      variant="outline"
                                      size="sm"
                                      onClick={() => {
                                        setSelectedManager('');
                                        setSelectedUsers([user.id]);
                                        setAssignDialogOpen(true);
                                      }}
                                      aria-label="Assign user to manager"
                                    >
                                      <UserPlus className="h-3 w-3" />
                                    </Button>
                                  </TooltipTrigger>
                                  <TooltipContent>
                                    <p>Assign this user to a manager. You can also drag this card onto a manager card.</p>
                                  </TooltipContent>
                                </Tooltip>
                              </TooltipProvider>
                            </div>
                          </Card>
                        ))}
                      </div>
                    </CardContent>
                  </Card>
                );
              }
              return null;
            })()}
          </div>
        )}

        <DragOverlay>
          {activeId ? (
            <div className="relative">
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
              {dragValidationMessage && (
                <div className="absolute -top-2 -right-2 bg-red-500 text-white text-xs px-2 py-1 rounded-full shadow-lg">
                  {dragValidationMessage}
                </div>
              )}
            </div>
          ) : null}
        </DragOverlay>
      </DndContext>

      {/* Assign Users Dialog */}
      <Dialog open={assignDialogOpen} onOpenChange={setAssignDialogOpen}>
        <DialogContent className="sm:max-w-lg">
          <DialogHeader>
            <DialogTitle>Bulk Assign Users to Manager</DialogTitle>
            <DialogDescription>
              Select a manager and multiple users to assign to them. You can assign up to 50 users at once.
            </DialogDescription>
          </DialogHeader>
          
          <div className="space-y-4">
            <div>
              <div className="flex items-center gap-2 mb-1">
                <Label htmlFor="manager-select">Manager</Label>
                <TooltipProvider>
                  <Tooltip>
                    <TooltipTrigger asChild>
                      <HelpCircle className="h-4 w-4 text-gray-400 cursor-help" aria-label="Manager field help" />
                    </TooltipTrigger>
                    <TooltipContent>
                      <p>Select the manager who will oversee these users. Only users with manager roles are shown.</p>
                    </TooltipContent>
                  </Tooltip>
                </TooltipProvider>
              </div>
              <Select 
                value={selectedManager} 
                onValueChange={(value) => {
                  setSelectedManager(value);
                  setValidationErrors([]);
                  setValidationWarnings([]);
                }}
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
              <div className="flex items-center gap-2 mb-1">
                <Label htmlFor="users-select">Users to Assign</Label>
                <TooltipProvider>
                  <Tooltip>
                    <TooltipTrigger asChild>
                      <HelpCircle className="h-4 w-4 text-gray-400 cursor-help" aria-label="Users field help" />
                    </TooltipTrigger>
                    <TooltipContent>
                      <p>Select one or more users. Users already managed by someone else will show a warning.</p>
                    </TooltipContent>
                  </Tooltip>
                </TooltipProvider>
              </div>
              <UserMultiSelect
                value={selectedUsers}
                onChange={(userIds) => {
                  setSelectedUsers(userIds);
                  setValidationErrors([]);
                  setValidationWarnings([]);
                }}
                users={availableUsers}
                excludeUserIds={[selectedManager]}
                roleFilter={userRoleFilterForManager}
                maxSelections={50}
                placeholder="Select users to assign..."
              />
            </div>

            {/* Validation Messages */}
            {(validationErrors.length > 0 || validationWarnings.length > 0) && (
              <div>
                <div className="flex items-center gap-2 mb-2">
                  <h4 className="text-sm font-medium">Validation</h4>
                  <TooltipProvider>
                    <Tooltip>
                      <TooltipTrigger asChild>
                        <HelpCircle className="h-3 w-3 text-gray-400 cursor-help" aria-label="Validation help" />
                      </TooltipTrigger>
                      <TooltipContent>
                        <p>These checks ensure the assignment is valid. Fix any errors before submitting.</p>
                      </TooltipContent>
                    </Tooltip>
                  </TooltipProvider>
                </div>
                
                {validationErrors.length > 0 && (
                  <Alert variant="destructive">
                    <AlertCircle className="h-4 w-4" />
                    <AlertDescription>
                      <ul className="list-disc list-inside">
                        {validationErrors.map((error, i) => <li key={i}>{error}</li>)}
                      </ul>
                    </AlertDescription>
                  </Alert>
                )}

                {validationWarnings.length > 0 && (
                  <Alert variant="default" className="border-yellow-200 bg-yellow-50">
                    <AlertTriangle className="h-4 w-4" />
                    <AlertDescription>
                      <ul className="list-disc list-inside">
                        {validationWarnings.map((warning, i) => <li key={i}>{warning}</li>)}
                      </ul>
                    </AlertDescription>
                  </Alert>
                )}
              </div>
            )}

            {/* Assignment Preview */}
            {selectedManager && selectedUsers.length > 0 && (
              <Alert>
                <AlertDescription>
                  You are about to assign {selectedUsers.length} user{selectedUsers.length !== 1 ? 's' : ''} to{' '}
                  {users.find((u: User) => u.id === selectedManager)?.name} ({getRoleDisplayName(users.find((u: User) => u.id === selectedManager)?.role || '')})
                </AlertDescription>
              </Alert>
            )}
          </div>
          
          <DialogFooter>
            <Button variant="outline" onClick={() => {
              setAssignDialogOpen(false);
              setSelectedManager('');
              setSelectedUsers([]);
              setValidationErrors([]);
              setValidationWarnings([]);
            }}>
              Cancel
            </Button>
            <Button 
              onClick={() => {
                if (selectedManager && selectedUsers.length > 0) {
                  if (selectedUsers.length > 10) {
                    setBulkConfirmDialogOpen(true)
                  } else {
                    assignMutation.mutate({ managerId: selectedManager, userIds: selectedUsers })
                  }
                }
              }}
              disabled={!selectedManager || selectedUsers.length === 0 || assignMutation.isPending || validationErrors.length > 0}
            >
              {assignMutation.isPending ? 'Assigning...' : `Assign ${selectedUsers.length} User${selectedUsers.length !== 1 ? 's' : ''}`}
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
              This action will have the following impacts:
            </DialogDescription>
          </DialogHeader>
          
          <div className="space-y-3">
            <ul className="text-sm text-gray-600 space-y-2">
              <li className="flex items-start gap-2">
                <span className="text-red-500 mt-1">•</span>
                <span>Remove user from manager&apos;s team in the hierarchy</span>
              </li>
              <li className="flex items-start gap-2">
                <span className="text-red-500 mt-1">•</span>
                <span>Manager will no longer see this user&apos;s projects</span>
              </li>
              <li className="flex items-start gap-2">
                <span className="text-gray-500 mt-1">•</span>
                <span>User still sees their own projects</span>
              </li>
              <li className="flex items-start gap-2">
                <span className="text-blue-500 mt-1">•</span>
                <span>User can be assigned to a different manager afterward</span>
              </li>
            </ul>
          </div>
          
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

      {/* Bulk Assignment Confirmation Dialog */}
      <AlertDialog open={bulkConfirmDialogOpen} onOpenChange={setBulkConfirmDialogOpen}>
        <AlertDialogContent>
          <AlertDialogHeader>
            <AlertDialogTitle>Confirm Bulk Assignment</AlertDialogTitle>
            <AlertDialogDescription>
              You are about to assign {selectedUsers.length} users to{' '}
              {users.find((u: User) => u.id === selectedManager)?.name} ({getRoleDisplayName(users.find((u: User) => u.id === selectedManager)?.role || '')}).
              This is a large operation that may take a moment to complete.
            </AlertDialogDescription>
          </AlertDialogHeader>
          <AlertDialogFooter>
            <AlertDialogCancel>Cancel</AlertDialogCancel>
            <AlertDialogAction
              onClick={() => {
                if (selectedManager && selectedUsers.length > 0) {
                  assignMutation.mutate({ managerId: selectedManager, userIds: selectedUsers })
                  setBulkConfirmDialogOpen(false)
                }
              }}
            >
              Confirm Assignment
            </AlertDialogAction>
          </AlertDialogFooter>
        </AlertDialogContent>
      </AlertDialog>

      {/* Bulk Selection Action Bar */}
      {bulkSelectionMode && bulkSelectedUsers.length > 0 && (
        <div className="fixed bottom-4 left-1/2 transform -translate-x-1/2 bg-white border border-gray-200 rounded-lg shadow-lg p-4 z-50">
          <div className="flex items-center gap-4">
            <span className="text-sm font-medium">
              {bulkSelectedUsers.length} user{bulkSelectedUsers.length !== 1 ? 's' : ''} selected
            </span>
            <Button
              size="sm"
              onClick={() => {
                setSelectedUsers(bulkSelectedUsers)
                setSelectedManager('')
                setAssignDialogOpen(true)
              }}
            >
              <UserPlus className="h-4 w-4 mr-2" />
              Assign Selected
            </Button>
            <Button
              variant="outline"
              size="sm"
              onClick={() => {
                setBulkSelectedUsers([])
                setBulkSelectionMode(false)
              }}
            >
              Cancel
            </Button>
          </div>
        </div>
      )}

      {/* Help Dialog */}
      <Dialog open={helpDialogOpen} onOpenChange={setHelpDialogOpen}>
        <DialogContent className="max-w-2xl">
          <DialogHeader>
            <DialogTitle>User Hierarchy Help</DialogTitle>
            <DialogDescription>
              Learn how to manage your organizational structure and user assignments.
            </DialogDescription>
          </DialogHeader>
          
          <div className="space-y-6">
            {/* Getting Started */}
            <div>
              <h3 className="text-lg font-semibold mb-3">Getting Started</h3>
              <div className="space-y-2">
                <div className="flex items-start gap-3">
                  <div className="flex-shrink-0 w-6 h-6 bg-blue-100 text-blue-600 rounded-full flex items-center justify-center text-sm font-medium">1</div>
                  <div>
                    <p className="font-medium">Find User</p>
                    <p className="text-sm text-gray-600">Use the search bar (⌘K) or filters to locate the user you want to assign.</p>
                  </div>
                </div>
                <div className="flex items-start gap-3">
                  <div className="flex-shrink-0 w-6 h-6 bg-blue-100 text-blue-600 rounded-full flex items-center justify-center text-sm font-medium">2</div>
                  <div>
                    <p className="font-medium">Drag to Manager</p>
                    <p className="text-sm text-gray-600">Drag the user card onto a manager card to assign them. Only valid role combinations are allowed.</p>
                  </div>
                </div>
                <div className="flex items-start gap-3">
                  <div className="flex-shrink-0 w-6 h-6 bg-blue-100 text-blue-600 rounded-full flex items-center justify-center text-sm font-medium">3</div>
                  <div>
                    <p className="font-medium">Verify</p>
                    <p className="text-sm text-gray-600">Check the hierarchy tree to confirm the assignment was successful.</p>
                  </div>
                </div>
              </div>
            </div>

            {/* Keyboard Shortcuts */}
            <div>
              <h3 className="text-lg font-semibold mb-3">Keyboard Shortcuts</h3>
              <div className="grid grid-cols-1 md:grid-cols-2 gap-3">
                <div className="flex items-center justify-between p-2 bg-gray-50 rounded">
                  <span className="text-sm">Search</span>
                  <kbd className="px-2 py-1 bg-white border rounded text-xs">⌘/Ctrl + K</kbd>
                </div>
                <div className="flex items-center justify-between p-2 bg-gray-50 rounded">
                  <span className="text-sm">Expand All</span>
                  <kbd className="px-2 py-1 bg-white border rounded text-xs">⌘/Ctrl + E</kbd>
                </div>
                <div className="flex items-center justify-between p-2 bg-gray-50 rounded">
                  <span className="text-sm">Collapse All</span>
                  <kbd className="px-2 py-1 bg-white border rounded text-xs">⌘/Ctrl + ⇧ + E</kbd>
                </div>
                <div className="flex items-center justify-between p-2 bg-gray-50 rounded">
                  <span className="text-sm">Bulk Select</span>
                  <kbd className="px-2 py-1 bg-white border rounded text-xs">⌘/Ctrl + B</kbd>
                </div>
                <div className="flex items-center justify-between p-2 bg-gray-50 rounded">
                  <span className="text-sm">Refresh Data</span>
                  <kbd className="px-2 py-1 bg-white border rounded text-xs">⌘/Ctrl + R</kbd>
                </div>
              </div>
            </div>

            {/* Documentation Links */}
            <div>
              <h3 className="text-lg font-semibold mb-3">Documentation</h3>
              <div className="space-y-2">
                <a 
                  href="https://github.com/your-org/rep-dashboard/blob/main/docs/MANAGER-SETUP-GUIDE.md" 
                  target="_blank" 
                  rel="noopener noreferrer"
                  className="flex items-center gap-2 text-blue-600 hover:text-blue-800 underline"
                >
                  <span>Manager Setup Guide</span>
                </a>
                <a 
                  href="https://github.com/your-org/rep-dashboard/blob/main/docs/HIERARCHY-TROUBLESHOOTING.md" 
                  target="_blank" 
                  rel="noopener noreferrer"
                  className="flex items-center gap-2 text-blue-600 hover:text-blue-800 underline"
                >
                  <span>Hierarchy Troubleshooting</span>
                </a>
              </div>
            </div>

            {/* Support Info */}
            <div>
              <h3 className="text-lg font-semibold mb-3">Support</h3>
              <div className="space-y-2 text-sm text-gray-600">
                <p>• Email: support@company.com</p>
                <p>• Slack: #rep-dashboard-support</p>
                <p>• For urgent issues, contact your system administrator</p>
              </div>
            </div>
          </div>
          
          <DialogFooter>
            <Button onClick={() => setHelpDialogOpen(false)}>
              Close
            </Button>
          </DialogFooter>
        </DialogContent>
      </Dialog>
    </div>
  )
}
