'use client'

import { useState, useEffect } from 'react'
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query'
import {
  Dialog,
  DialogContent,
  DialogDescription,
  DialogFooter,
  DialogHeader,
  DialogTitle,
} from '@/components/ui/dialog'
import { Button } from '@/components/ui/button'
import { Input } from '@/components/ui/input'
import { Label } from '@/components/ui/label'
import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from '@/components/ui/select'
import { Badge } from '@/components/ui/badge'
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card'
import { Alert, AlertDescription } from '@/components/ui/alert'
import { Checkbox } from '@/components/ui/checkbox'
import { Table, TableBody, TableCell, TableHead, TableHeader, TableRow } from '@/components/ui/table'
import { toast } from 'sonner'
import { getBaseUrl } from '@/lib/utils/baseUrl'
import { useAvailableOffices } from '@/hooks/useAvailableOffices'
import { cn } from '@/lib/utils/cn'
import { Search, UserPlus, Mail, Phone, MapPin, Info, AlertCircle, Calendar, TrendingUp } from 'lucide-react'

interface QuickBaseLookupDialogProps {
  open: boolean
  onOpenChange: (open: boolean) => void
}

interface QuickBaseUser {
  quickbaseUserId: string
  name: string
  email: string
  phone?: string
  role: string
  office?: string
  offices?: string[]
  lastProjectDate?: string
  projectCount?: number
  activeProjectCount?: number
}

interface CreateUserData {
  name: string
  email: string
  phone: string
  role: string
  quickbaseUserId: string
  office: string
  region: string
  temporaryPassword: string
}

export function QuickBaseLookupDialog({ open, onOpenChange }: QuickBaseLookupDialogProps) {
  const [searchQuery, setSearchQuery] = useState('')
  const [monthsBack, setMonthsBack] = useState(6)
  const [activeOnly, setActiveOnly] = useState(true)
  const [selectedUser, setSelectedUser] = useState<QuickBaseUser | null>(null)
  const [activityStats, setActivityStats] = useState<any>(null)
  const [createUserData, setCreateUserData] = useState<CreateUserData>({
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
  const { data: availableOffices = [] } = useAvailableOffices()

  // Activity status helper functions
  const getActivityStatus = (lastProjectDate?: string) => {
    if (!lastProjectDate) return 'dormant'
    const date = new Date(lastProjectDate)
    const now = new Date()
    const monthsAgo = (now.getTime() - date.getTime()) / (1000 * 60 * 60 * 24 * 30)
    if (monthsAgo <= 6) return 'active'
    if (monthsAgo <= 12) return 'inactive'
    return 'dormant'
  }

  const isOlderThanMonths = (dateStr?: string, months = 6) => {
    if (!dateStr) return true
    const d = new Date(dateStr)
    const now = new Date()
    const monthsDiff = (now.getTime() - d.getTime()) / (1000 * 60 * 60 * 24 * 30)
    return monthsDiff > months
  }

  const formatRelativeDate = (dateString: string) => {
    const date = new Date(dateString)
    const now = new Date()
    const diffTime = Math.abs(now.getTime() - date.getTime())
    const diffDays = Math.ceil(diffTime / (1000 * 60 * 60 * 24))
    const diffMonths = Math.floor(diffDays / 30)
    
    if (diffMonths === 0) return 'less than a month ago'
    if (diffMonths === 1) return '1 month ago'
    return `${diffMonths} months ago`
  }

  const getActivityColor = (status: string) => {
    switch (status) {
      case 'active': return 'bg-green-500'
      case 'inactive': return 'bg-yellow-500'
      case 'dormant': return 'bg-red-500'
      default: return 'bg-gray-500'
    }
  }

  // Search QuickBase users
  const { data: searchResults, isLoading: isSearching } = useQuery({
    queryKey: ['quickbase-users', searchQuery, monthsBack, activeOnly],
    queryFn: async () => {
      if (!searchQuery || searchQuery.length < 2) return { users: [], total: 0 }
      
      const response = await fetch(
        `${getBaseUrl()}/api/admin/users/lookup?search=${encodeURIComponent(searchQuery)}&activeOnly=${activeOnly}&monthsBack=${monthsBack}`
      )
      
      if (!response.ok) {
        throw new Error('Failed to search QuickBase users')
      }
      
      return response.json()
    },
    enabled: searchQuery.length >= 2,
  })

  // Create user mutation
  const createUserMutation = useMutation({
    mutationFn: async (data: CreateUserData) => {
      const response = await fetch(`${getBaseUrl()}/api/admin/users`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(data),
      })

      if (!response.ok) {
        const error = await response.json()
        throw new Error(error.error || 'Failed to create user')
      }

      return response.json()
    },
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['users'] })
      onOpenChange(false)
      setSearchQuery('')
      setSelectedUser(null)
      setCreateUserData({
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
      toast.error('Failed to create user', {
        description: error.message,
      })
    },
  })

  const handleUserSelect = async (user: QuickBaseUser) => {
    setSelectedUser(user)
    setCreateUserData({
      name: user.name,
      email: user.email,
      phone: user.phone || '',
      role: user.role,
      quickbaseUserId: user.quickbaseUserId,
      office: user.office || '',
      region: '',
      temporaryPassword: Math.random().toString(36).slice(-12), // Generate temp password
    })

    // Fetch detailed activity stats
    try {
      const response = await fetch(`${getBaseUrl()}/api/admin/users/lookup?quickbaseId=${user.quickbaseUserId}`)
      if (response.ok) {
        const data = await response.json()
        setActivityStats(data.activity)
      }
    } catch (error) {
      console.error('Failed to fetch activity stats:', error)
      setActivityStats(null)
    }
  }

  const handleSubmit = (e: React.FormEvent) => {
    e.preventDefault()
    
    if (!createUserData.name || !createUserData.email || !createUserData.quickbaseUserId || !createUserData.temporaryPassword) {
      toast.error('Name, email, Quickbase User ID, and temporary password are required')
      return
    }

    createUserMutation.mutate(createUserData)
  }

  const handleOpenChange = (nextOpen: boolean) => {
    if (!nextOpen) {
      setSearchQuery('')
      setMonthsBack(6)
      setActiveOnly(true)
      setSelectedUser(null)
      setActivityStats(null)
      setCreateUserData({
        name: '',
        email: '',
        phone: '',
        role: 'closer',
        quickbaseUserId: '',
        office: '',
        region: '',
        temporaryPassword: '',
      })
    }
    onOpenChange(nextOpen)
  }

  return (
    <Dialog open={open} onOpenChange={handleOpenChange}>
      <DialogContent className="sm:max-w-4xl max-h-[80vh] overflow-y-auto">
        <DialogHeader>
          <DialogTitle>Add from QuickBase</DialogTitle>
          <DialogDescription>
            Search and add users from QuickBase with auto-filled data.
          </DialogDescription>
        </DialogHeader>
        
        <div className="space-y-6">
          {/* Search Section */}
          <div className="space-y-3">
            <div className="flex items-center gap-4">
              <div className="flex-1">
                <Label htmlFor="activity-timeframe">Activity Timeframe</Label>
                <Select value={monthsBack.toString()} onValueChange={(value) => setMonthsBack(parseInt(value))}>
                  <SelectTrigger>
                    <SelectValue />
                  </SelectTrigger>
                  <SelectContent>
                    <SelectItem value="3">3 months</SelectItem>
                    <SelectItem value="6">6 months</SelectItem>
                    <SelectItem value="12">12 months</SelectItem>
                  </SelectContent>
                </Select>
              </div>
              <div className="flex items-center gap-2">
                <Checkbox id="active-only" checked={activeOnly} onCheckedChange={(v) => setActiveOnly(Boolean(v))} />
                <Label htmlFor="active-only">Active users only</Label>
              </div>
            </div>
            <div>
              <Label htmlFor="lookup-search">Search QuickBase Users</Label>
              <div className="relative">
                <Search className="absolute left-3 top-1/2 transform -translate-y-1/2 h-4 w-4 text-gray-400" />
                <Input
                  id="lookup-search"
                  placeholder="Search by name or email..."
                  value={searchQuery}
                  onChange={(e) => setSearchQuery(e.target.value)}
                  className="pl-10"
                />
              </div>
            </div>
          </div>

          {/* Search Results */}
          {searchQuery.length >= 2 && (
            <div>
              <h3 className="text-sm font-medium mb-3">Search Results</h3>
              {isSearching ? (
                <div className="text-center py-4">Searching...</div>
              ) : searchResults?.users?.length > 0 ? (
                <div className="max-h-60 overflow-y-auto border rounded-md">
                  <Table>
                    <TableHeader>
                      <TableRow>
                        <TableHead>Name</TableHead>
                        <TableHead>Email</TableHead>
                        <TableHead>Role</TableHead>
                        <TableHead>Last Project</TableHead>
                        <TableHead>Projects</TableHead>
                        <TableHead>Offices</TableHead>
                        <TableHead className="text-right">Action</TableHead>
                      </TableRow>
                    </TableHeader>
                    <TableBody>
                      {searchResults.users.map((user: QuickBaseUser) => {
                        const activityStatus = getActivityStatus(user.lastProjectDate)
                        const officesText = Array.isArray(user.offices) ? user.offices.join(', ') : (user.office || undefined)
                        return (
                          <TableRow 
                            key={user.quickbaseUserId} 
                            className={cn(
                              selectedUser?.quickbaseUserId === user.quickbaseUserId && 'ring-2 ring-blue-500'
                            )}
                          >
                            <TableCell>
                              <div className="flex items-center gap-2">
                                <span className={cn('w-2 h-2 rounded-full', getActivityColor(activityStatus))} />
                                {user.name}
                              </div>
                            </TableCell>
                            <TableCell>{user.email}</TableCell>
                            <TableCell><Badge variant="outline">{user.role}</Badge></TableCell>
                            <TableCell>{user.lastProjectDate ? formatRelativeDate(user.lastProjectDate) : '-'}</TableCell>
                            <TableCell>
                              {user.projectCount !== undefined && (
                                <Badge variant="secondary" className="gap-1">
                                  <TrendingUp className="h-3 w-3" />
                                  {user.projectCount} projects
                                </Badge>
                              )}
                            </TableCell>
                            <TableCell>{officesText || '-'}</TableCell>
                            <TableCell className="text-right">
                              <Button size="sm" onClick={() => handleUserSelect(user)}>Select</Button>
                            </TableCell>
                          </TableRow>
                        )
                      })}
                    </TableBody>
                  </Table>
                </div>
              ) : (
                <div className="text-center py-4 text-gray-500">
                  No users found for &quot;{searchQuery}&quot;
                </div>
              )}
            </div>
          )}

          {/* User Creation Form */}
          {selectedUser && (
            <div>
              <h3 className="text-sm font-medium mb-3">Create User Account</h3>
              
              {/* Activity Stats Display */}
              {activityStats && (
                <Alert className="mb-4">
                  <AlertDescription>
                    <div className="flex items-start gap-2">
                      <div className={cn('w-2 h-2 rounded-full mt-1.5', getActivityColor(getActivityStatus(activityStats.lastProjectDate)))} />
                      <div>
                        <p className="font-medium">✓ Found in QuickBase: {selectedUser.name}</p>
                        {activityStats.lastProjectDate && (
                          <p className="text-sm text-gray-600 mt-1">
                            Last project: {formatRelativeDate(activityStats.lastProjectDate)} • 
                            {activityStats.totalProjects || 0} total projects • 
                            {activityStats.activeProjects || 0} active
                          </p>
                        )}
                        {activityStats.offices && activityStats.offices.length > 0 && (
                          <p className="text-sm text-gray-600 mt-1">
                            Offices: {activityStats.offices.join(', ')}
                          </p>
                        )}
                      </div>
                    </div>
                  </AlertDescription>
                </Alert>
              )}

              {/* Warning for dormant users */}
              {activityStats && isOlderThanMonths(activityStats.lastProjectDate, monthsBack) && (
                <Alert className="mb-4 border-yellow-200 bg-yellow-50">
                  <AlertCircle className="h-4 w-4 text-yellow-600" />
                  <AlertDescription className="text-yellow-800">
                    This user has no projects in the last {monthsBack} months. Create account anyway?
                  </AlertDescription>
                </Alert>
              )}

              <form onSubmit={handleSubmit} className="space-y-4">
                <div className="grid grid-cols-2 gap-4">
                  <div>
                    <Label htmlFor="create-name">Name *</Label>
                    <Input
                      id="create-name"
                      value={createUserData.name}
                      onChange={(e) => setCreateUserData(prev => ({ ...prev, name: e.target.value }))}
                      required
                    />
                  </div>
                  <div>
                    <Label htmlFor="create-email">Email *</Label>
                    <Input
                      id="create-email"
                      type="email"
                      value={createUserData.email}
                      onChange={(e) => setCreateUserData(prev => ({ ...prev, email: e.target.value }))}
                      required
                    />
                  </div>
                </div>

                <div className="grid grid-cols-2 gap-4">
                  <div>
                    <Label htmlFor="create-phone">Phone</Label>
                    <Input
                      id="create-phone"
                      value={createUserData.phone}
                      onChange={(e) => setCreateUserData(prev => ({ ...prev, phone: e.target.value }))}
                    />
                  </div>
                  <div>
                    <Label htmlFor="create-role">Role *</Label>
                    <Select 
                      value={createUserData.role} 
                      onValueChange={(value) => setCreateUserData(prev => ({ ...prev, role: value }))}
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
                      </SelectContent>
                    </Select>
                  </div>
                </div>

                {/* Info message for office-based roles */}
                {['office_leader', 'area_director', 'divisional', 'regional'].includes(createUserData.role) && (
                  <Alert>
                    <Info className="h-4 w-4" />
                    <AlertDescription className="text-xs">
                      Office-based roles see ALL projects in their assigned offices, including projects from users without active accounts.
                    </AlertDescription>
                  </Alert>
                )}

                <div className="grid grid-cols-2 gap-4">
                  <div>
                    <Label htmlFor="create-office">Office</Label>
                    <Select 
                      value={createUserData.office} 
                      onValueChange={(value) => setCreateUserData(prev => ({ ...prev, office: value }))}
                    >
                      <SelectTrigger>
                        <SelectValue placeholder="Select office" />
                      </SelectTrigger>
                      <SelectContent>
                        <SelectItem value="">No office</SelectItem>
                        {availableOffices.map((office: string) => (
                          <SelectItem key={office} value={office}>
                            {office}
                          </SelectItem>
                        ))}
                      </SelectContent>
                    </Select>
                  </div>
                  <div>
                    <Label htmlFor="create-region">Region</Label>
                    <Input
                      id="create-region"
                      value={createUserData.region}
                      onChange={(e) => setCreateUserData(prev => ({ ...prev, region: e.target.value }))}
                    />
                  </div>
                </div>

                <div className="grid grid-cols-2 gap-4">
                  <div>
                    <Label htmlFor="create-quickbase-id">Quickbase User ID *</Label>
                    <Input
                      id="create-quickbase-id"
                      value={createUserData.quickbaseUserId}
                      onChange={(e) => setCreateUserData(prev => ({ ...prev, quickbaseUserId: e.target.value }))}
                      required
                    />
                  </div>
                  <div>
                    <Label htmlFor="create-password">Temporary Password *</Label>
                    <Input
                      id="create-password"
                      type="password"
                      value={createUserData.temporaryPassword}
                      onChange={(e) => setCreateUserData(prev => ({ ...prev, temporaryPassword: e.target.value }))}
                      required
                    />
                  </div>
                </div>
              </form>
            </div>
          )}
        </div>
        
        <DialogFooter>
          <Button variant="outline" onClick={() => handleOpenChange(false)}>
            Cancel
          </Button>
          {selectedUser && (
            <Button 
              onClick={handleSubmit}
              disabled={createUserMutation.isPending}
            >
              {createUserMutation.isPending ? 'Creating...' : 'Create User'}
            </Button>
          )}
        </DialogFooter>
      </DialogContent>
    </Dialog>
  )
}
