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
import { toast } from 'sonner'
import { getBaseUrl } from '@/lib/utils/baseUrl'
import { useAvailableOffices } from '@/hooks/useAvailableOffices'
import { Search, UserPlus, Mail, Phone, MapPin } from 'lucide-react'

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
  lastProjectDate?: string
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
  const [selectedUser, setSelectedUser] = useState<QuickBaseUser | null>(null)
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

  // Search QuickBase users
  const { data: searchResults, isLoading: isSearching } = useQuery({
    queryKey: ['quickbase-users', searchQuery],
    queryFn: async () => {
      if (!searchQuery || searchQuery.length < 2) return { users: [], total: 0 }
      
      const response = await fetch(
        `${getBaseUrl()}/api/admin/users/lookup?search=${encodeURIComponent(searchQuery)}&activeOnly=true&monthsBack=6`
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

  const handleUserSelect = (user: QuickBaseUser) => {
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
  }

  const handleSubmit = (e: React.FormEvent) => {
    e.preventDefault()
    
    if (!createUserData.name || !createUserData.email || !createUserData.quickbaseUserId || !createUserData.temporaryPassword) {
      toast.error('Name, email, Quickbase User ID, and temporary password are required')
      return
    }

    createUserMutation.mutate(createUserData)
  }

  const handleClose = () => {
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
  }

  return (
    <Dialog open={open} onOpenChange={handleClose}>
      <DialogContent className="sm:max-w-4xl max-h-[80vh] overflow-y-auto">
        <DialogHeader>
          <DialogTitle>Add from QuickBase</DialogTitle>
          <DialogDescription>
            Search and add users from QuickBase with auto-filled data.
          </DialogDescription>
        </DialogHeader>
        
        <div className="space-y-6">
          {/* Search Section */}
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

          {/* Search Results */}
          {searchQuery.length >= 2 && (
            <div>
              <h3 className="text-sm font-medium mb-3">Search Results</h3>
              {isSearching ? (
                <div className="text-center py-4">Searching...</div>
              ) : searchResults?.users?.length > 0 ? (
                <div className="grid gap-3 max-h-60 overflow-y-auto">
                  {searchResults.users.map((user: QuickBaseUser) => (
                    <Card 
                      key={user.quickbaseUserId} 
                      className={`cursor-pointer transition-colors ${
                        selectedUser?.quickbaseUserId === user.quickbaseUserId 
                          ? 'ring-2 ring-blue-500' 
                          : 'hover:bg-gray-50'
                      }`}
                      onClick={() => handleUserSelect(user)}
                    >
                      <CardContent className="p-4">
                        <div className="flex items-center justify-between">
                          <div className="flex-1">
                            <div className="flex items-center gap-2 mb-1">
                              <h4 className="font-medium">{user.name}</h4>
                              <Badge variant="outline">{user.role}</Badge>
                            </div>
                            <div className="flex items-center gap-4 text-sm text-gray-600">
                              {user.email && (
                                <div className="flex items-center gap-1">
                                  <Mail className="h-3 w-3" />
                                  {user.email}
                                </div>
                              )}
                              {user.phone && (
                                <div className="flex items-center gap-1">
                                  <Phone className="h-3 w-3" />
                                  {user.phone}
                                </div>
                              )}
                              {user.office && (
                                <div className="flex items-center gap-1">
                                  <MapPin className="h-3 w-3" />
                                  {user.office}
                                </div>
                              )}
                            </div>
                          </div>
                          <UserPlus className="h-5 w-5 text-gray-400" />
                        </div>
                      </CardContent>
                    </Card>
                  ))}
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
          <Button variant="outline" onClick={handleClose}>
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
