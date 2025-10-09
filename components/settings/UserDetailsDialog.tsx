'use client'

import { useState } from 'react'
import { useQuery } from '@tanstack/react-query'
import {
  Dialog,
  DialogContent,
  DialogDescription,
  DialogHeader,
  DialogTitle,
} from '@/components/ui/dialog'
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card'
import { Badge } from '@/components/ui/badge'
import { Avatar, AvatarFallback } from '@/components/ui/avatar'
import { Button } from '@/components/ui/button'
import { Separator } from '@/components/ui/separator'
import { getBaseUrl } from '@/lib/utils/baseUrl'
import { getRoleBadgeVariant, getRoleDisplayName } from '@/lib/utils/roles'
import { getActivityStatus, getActivityColor, getActivityLabel, formatLastActivity } from '@/lib/utils/activity'
import { getInitials, getAvatarColor, getAvatarTextColor } from '@/lib/utils/avatar'
import type { UserRole } from '@/lib/types/project'
import { 
  Users, 
  MapPin,
  Calendar,
  Mail,
  Phone,
  Building,
  Shield,
  Crown,
  UserCheck,
  UserX,
  Copy,
  Edit
} from 'lucide-react'
import { toast } from 'sonner'

interface UserDetailsDialogProps {
  userId: string | null
  isOpen: boolean
  onClose: () => void
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
  phone?: string
  created_at?: string
  updated_at?: string
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

export function UserDetailsDialog({ userId, isOpen, onClose }: UserDetailsDialogProps) {
  const [copiedField, setCopiedField] = useState<string | null>(null)

  // Fetch user details
  const { data: user, isLoading: userLoading } = useQuery({
    queryKey: ['user', userId],
    queryFn: async () => {
      if (!userId) return null
      const response = await fetch(`${getBaseUrl()}/api/admin/users/${userId}`)
      if (!response.ok) throw new Error('Failed to fetch user')
      return response.json()
    },
    enabled: !!userId && isOpen,
  })

  // Fetch hierarchies for this user
  const { data: hierarchies = [] } = useQuery({
    queryKey: ['hierarchies'],
    queryFn: async () => {
      const response = await fetch(`${getBaseUrl()}/api/admin/hierarchies`)
      if (!response.ok) throw new Error('Failed to fetch hierarchies')
      return response.json()
    },
    enabled: isOpen,
  })

  // Fetch all users for hierarchy context
  const { data: allUsers = [] } = useQuery({
    queryKey: ['users'],
    queryFn: async () => {
      const response = await fetch(`${getBaseUrl()}/api/admin/users`)
      if (!response.ok) throw new Error('Failed to fetch users')
      return response.json()
    },
    enabled: isOpen,
  })

  const copyToClipboard = (text: string, field: string) => {
    navigator.clipboard.writeText(text)
    setCopiedField(field)
    toast.success(`${field} copied to clipboard`)
    setTimeout(() => setCopiedField(null), 2000)
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

  if (!user && !userLoading) {
    return null
  }

  const userHierarchies = hierarchies.filter((h: Hierarchy) => h.user_id === userId)
  const managedUsers = hierarchies.filter((h: Hierarchy) => h.manager_id === userId)
  const manager = user?.managed_by ? allUsers.find((u: User) => u.id === user.managed_by) : null

  const activityStatus = user ? getActivityStatus(user.lastProjectDate) : null
  const activityColor = activityStatus ? getActivityColor(activityStatus) : ''
  const activityLabel = activityStatus ? getActivityLabel(activityStatus) : ''
  const lastActivity = user ? formatLastActivity(user.lastProjectDate) : ''

  return (
    <Dialog open={isOpen} onOpenChange={onClose}>
      <DialogContent className="sm:max-w-2xl max-h-[90vh] overflow-y-auto">
        <DialogHeader>
          <DialogTitle className="flex items-center gap-3">
            {user && (
              <>
                <Avatar size="lg" className={`${getAvatarColor(user.role)} ${getAvatarTextColor()}`}>
                  <AvatarFallback size="lg">
                    {getInitials(user.name)}
                  </AvatarFallback>
                </Avatar>
                <div>
                  <div className="flex items-center gap-2">
                    <h2 className="text-xl font-semibold">{user.name}</h2>
                    {getRoleIcon(user.role)}
                  </div>
                  <p className="text-sm text-gray-600">{user.email}</p>
                </div>
              </>
            )}
          </DialogTitle>
          <DialogDescription>
            View detailed information about this user including hierarchy relationships and activity.
          </DialogDescription>
        </DialogHeader>

        {userLoading ? (
          <div className="space-y-4">
            <div className="h-4 bg-gray-200 rounded animate-pulse" />
            <div className="h-4 bg-gray-200 rounded animate-pulse" />
            <div className="h-4 bg-gray-200 rounded animate-pulse" />
          </div>
        ) : user ? (
          <div className="space-y-6">
            {/* Basic Information */}
            <Card>
              <CardHeader>
                <CardTitle className="text-lg">Basic Information</CardTitle>
              </CardHeader>
              <CardContent className="space-y-4">
                <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                  <div className="space-y-2">
                    <div className="flex items-center gap-2">
                      <Mail className="h-4 w-4 text-gray-500" />
                      <span className="text-sm font-medium">Email</span>
                      <Button
                        variant="ghost"
                        size="sm"
                        onClick={() => copyToClipboard(user.email, 'Email')}
                        className="h-6 w-6 p-0"
                      >
                        <Copy className="h-3 w-3" />
                      </Button>
                    </div>
                    <p className="text-sm text-gray-600 ml-6">{user.email}</p>
                  </div>

                  {user.phone && (
                    <div className="space-y-2">
                      <div className="flex items-center gap-2">
                        <Phone className="h-4 w-4 text-gray-500" />
                        <span className="text-sm font-medium">Phone</span>
                        <Button
                          variant="ghost"
                          size="sm"
                          onClick={() => copyToClipboard(user.phone!, 'Phone')}
                          className="h-6 w-6 p-0"
                        >
                          <Copy className="h-3 w-3" />
                        </Button>
                      </div>
                      <p className="text-sm text-gray-600 ml-6">{user.phone}</p>
                    </div>
                  )}

                  <div className="space-y-2">
                    <div className="flex items-center gap-2">
                      <Shield className="h-4 w-4 text-gray-500" />
                      <span className="text-sm font-medium">Role</span>
                    </div>
                    <div className="ml-6">
                      <Badge variant={getRoleBadgeVariant(user.role)}>
                        {getRoleDisplayName(user.role)}
                      </Badge>
                    </div>
                  </div>

                  <div className="space-y-2">
                    <div className="flex items-center gap-2">
                      {user.is_active ? (
                        <UserCheck className="h-4 w-4 text-green-500" />
                      ) : (
                        <UserX className="h-4 w-4 text-red-500" />
                      )}
                      <span className="text-sm font-medium">Status</span>
                    </div>
                    <div className="ml-6">
                      <Badge variant={user.is_active ? "default" : "outline"}>
                        {user.is_active ? 'Active' : 'Inactive'}
                      </Badge>
                    </div>
                  </div>
                </div>

                <Separator />

                <div className="space-y-2">
                  <div className="flex items-center gap-2">
                    <Calendar className="h-4 w-4 text-gray-500" />
                    <span className="text-sm font-medium">Last Activity</span>
                    <div 
                      className={`w-3 h-3 rounded-full ${activityColor} border-2 border-white`}
                      title={`${activityLabel} - ${lastActivity}`}
                    />
                  </div>
                  <p className="text-sm text-gray-600 ml-6">
                    {lastActivity || 'No activity data'}
                  </p>
                </div>
              </CardContent>
            </Card>

            {/* Office Information */}
            <Card>
              <CardHeader>
                <CardTitle className="text-lg">Office Information</CardTitle>
              </CardHeader>
              <CardContent className="space-y-4">
                {user.office && (
                  <div className="space-y-2">
                    <div className="flex items-center gap-2">
                      <MapPin className="h-4 w-4 text-gray-500" />
                      <span className="text-sm font-medium">Primary Office</span>
                    </div>
                    <p className="text-sm text-gray-600 ml-6">{user.office}</p>
                  </div>
                )}

                {user.salesOffice && user.salesOffice.length > 0 && (
                  <div className="space-y-2">
                    <div className="flex items-center gap-2">
                      <Building className="h-4 w-4 text-gray-500" />
                      <span className="text-sm font-medium">Sales Offices</span>
                    </div>
                    <div className="ml-6 space-y-1">
                      {user.salesOffice.map((office: string, index: number) => (
                        <div key={index} className="text-sm text-gray-600">
                          {office}
                        </div>
                      ))}
                    </div>
                  </div>
                )}

                {user.officeAccess && user.officeAccess.length > 0 && (
                  <div className="space-y-2">
                    <div className="flex items-center gap-2">
                      <Shield className="h-4 w-4 text-gray-500" />
                      <span className="text-sm font-medium">Office Access</span>
                    </div>
                    <div className="ml-6 space-y-2">
                      {user.officeAccess.map((access: { officeName: string; accessLevel: 'view' | 'manage' | 'admin' }, index: number) => (
                        <div key={index} className="flex items-center gap-2">
                          <span className="text-sm text-gray-600">{access.officeName}</span>
                          <Badge variant="outline" className="text-xs">
                            {access.accessLevel}
                          </Badge>
                        </div>
                      ))}
                    </div>
                  </div>
                )}
              </CardContent>
            </Card>

            {/* Hierarchy Relationships */}
            <Card>
              <CardHeader>
                <CardTitle className="text-lg">Hierarchy Relationships</CardTitle>
              </CardHeader>
              <CardContent className="space-y-4">
                {manager && (
                  <div className="space-y-2">
                    <div className="flex items-center gap-2">
                      <Users className="h-4 w-4 text-gray-500" />
                      <span className="text-sm font-medium">Reports to</span>
                    </div>
                    <div className="ml-6 flex items-center gap-2">
                      <Avatar size="sm" className={`${getAvatarColor(manager.role)} ${getAvatarTextColor()}`}>
                        <AvatarFallback size="sm">
                          {getInitials(manager.name)}
                        </AvatarFallback>
                      </Avatar>
                      <div>
                        <p className="text-sm font-medium">{manager.name}</p>
                        <p className="text-xs text-gray-600">{getRoleDisplayName(manager.role)}</p>
                      </div>
                    </div>
                  </div>
                )}

                {managedUsers.length > 0 && (
                  <div className="space-y-2">
                    <div className="flex items-center gap-2">
                      <Users className="h-4 w-4 text-gray-500" />
                      <span className="text-sm font-medium">Manages ({managedUsers.length})</span>
                    </div>
                    <div className="ml-6 space-y-2">
                      {managedUsers.map((hierarchy: Hierarchy) => {
                        const managedUser = allUsers.find((u: User) => u.id === hierarchy.user_id)
                        if (!managedUser) return null
                        
                        return (
                          <div key={hierarchy.id} className="flex items-center gap-2">
                            <Avatar size="sm" className={`${getAvatarColor(managedUser.role)} ${getAvatarTextColor()}`}>
                              <AvatarFallback size="sm">
                                {getInitials(managedUser.name)}
                              </AvatarFallback>
                            </Avatar>
                            <div>
                              <p className="text-sm font-medium">{managedUser.name}</p>
                              <p className="text-xs text-gray-600">{getRoleDisplayName(managedUser.role)}</p>
                            </div>
                          </div>
                        )
                      })}
                    </div>
                  </div>
                )}

                {!manager && managedUsers.length === 0 && (
                  <p className="text-sm text-gray-500 text-center py-4">
                    No hierarchy relationships found
                  </p>
                )}
              </CardContent>
            </Card>

            {/* Actions */}
            <div className="flex justify-end gap-2">
              <Button variant="outline" onClick={onClose}>
                Close
              </Button>
              <Button variant="outline">
                <Edit className="h-4 w-4 mr-2" />
                Edit User
              </Button>
            </div>
          </div>
        ) : null}
      </DialogContent>
    </Dialog>
  )
}
