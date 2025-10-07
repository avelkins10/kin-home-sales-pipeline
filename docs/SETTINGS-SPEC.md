# Settings Page - Complete Specification

**Last Updated**: 2025-01-02
**Status**: Not implemented - Phased approach for Traycer/Cursor
**Route**: `/app/(dashboard)/settings/page.tsx`
**Access**: Role-based (super_admin gets full access, others get limited)

---

## Table of Contents
1. [Overview](#1-overview)
2. [Role-Based Access](#2-role-based-access)
3. [Page Structure](#3-page-structure)
4. [Phase 5A: Basic Settings (Essential)](#phase-5a-basic-settings-essential)
5. [Phase 5B: User Management (Admin)](#phase-5b-user-management-admin)
6. [Phase 5C: System Configuration (Advanced)](#phase-5c-system-configuration-advanced)
7. [Phase 5D: Analytics & Audit (Enterprise)](#phase-5d-analytics--audit-enterprise)
8. [Data Schema](#8-data-schema)
9. [API Endpoints](#9-api-endpoints)
10. [Implementation Prompts](#10-implementation-prompts)

---

## 1. Overview

The Settings page provides role-based configuration and management capabilities for the Kin Home Rep Dashboard. It follows a progressive disclosure pattern where users see only what's relevant to their role.

### Design Principles
- **Role-based visibility**: Show only features available to user's role
- **Clear hierarchy**: Tabs/sections organized by function
- **Immediate feedback**: Real-time validation and status updates
- **Safe operations**: Confirmations for destructive actions
- **Audit trail**: All changes logged for super_admins

---

## 2. Role-Based Access

### Super Admin (`super_admin`)
**Full Access** - All settings visible
- User management (CRUD)
- Office/region management
- System configuration
- Quickbase integration settings
- Milestone timeline configuration
- Notification rule management
- Audit logs
- Data export

### Regional Manager (`regional`)
**Limited Management**
- View users in their region
- Update their profile
- View team performance settings
- Configure regional notification preferences
- View (not edit) office assignments

### Office Leader (`office_leader`)
**Office Management**
- View users in their office
- Update their profile
- Configure office notification preferences
- View team settings

### Closer/Setter (`closer`, `setter`)
**Personal Settings Only**
- Update their profile (name, email, phone)
- Change password
- Notification preferences (email alerts on/off)
- Theme preferences (light/dark - future)

---

## 3. Page Structure

### Layout: Tabbed Interface

```typescript
<Tabs defaultValue="profile">
  <TabsList>
    <TabsTrigger value="profile">Profile</TabsTrigger>
    <TabsTrigger value="notifications">Notifications</TabsTrigger>

    {/* Office Leaders & Above */}
    {hasTeamAccess && <TabsTrigger value="team">Team</TabsTrigger>}

    {/* Super Admin Only */}
    {isSuperAdmin && <TabsTrigger value="users">Users</TabsTrigger>}
    {isSuperAdmin && <TabsTrigger value="offices">Offices</TabsTrigger>}
    {isSuperAdmin && <TabsTrigger value="system">System</TabsTrigger>}
    {isSuperAdmin && <TabsTrigger value="quickbase">Quickbase</TabsTrigger>}
    {isSuperAdmin && <TabsTrigger value="audit">Audit Logs</TabsTrigger>}
  </TabsList>

  {/* Tab contents... */}
</Tabs>
```

---

## Phase 5A: Basic Settings (Essential)

**Time Estimate**: 2-3 hours
**Priority**: HIGH - Build this first
**Who Gets It**: All users

### Tab 1: Profile

#### Components Needed
```
ProfileTab
├── ProfileHeader (avatar, name, role badge)
├── ProfileForm
│   ├── Name field (editable)
│   ├── Email field (editable)
│   ├── Phone field (editable)
│   ├── Quickbase User ID (read-only, display only)
│   ├── Role badge (read-only)
│   ├── Office (read-only)
│   └── Save button
├── PasswordChangeSection
│   ├── Current password
│   ├── New password
│   ├── Confirm password
│   └── Change password button
└── DangerZone
    └── Sign out all sessions button
```

#### Implementation

**File**: `components/settings/ProfileTab.tsx`

```typescript
'use client'

import { useState } from 'react'
import { useMutation, useQueryClient } from '@tanstack/react-query'
import { Card, CardHeader, CardTitle, CardContent } from '@/components/ui/card'
import { Input } from '@/components/ui/input'
import { Button } from '@/components/ui/button'
import { Badge } from '@/components/ui/badge'
import { Label } from '@/components/ui/label'
import { User, Mail, Phone, Shield, AlertTriangle } from 'lucide-react'
import { toast } from 'sonner'
import type { User as UserType } from '@/lib/types/user'

interface ProfileTabProps {
  user: UserType
}

export function ProfileTab({ user }: ProfileTabProps) {
  const queryClient = useQueryClient()

  // Form state
  const [name, setName] = useState(user.name || '')
  const [email, setEmail] = useState(user.email || '')
  const [phone, setPhone] = useState(user.phone || '')

  // Password change state
  const [currentPassword, setCurrentPassword] = useState('')
  const [newPassword, setNewPassword] = useState('')
  const [confirmPassword, setConfirmPassword] = useState('')

  // Update profile mutation
  const profileMutation = useMutation({
    mutationFn: async (data: { name: string; email: string; phone: string }) => {
      const response = await fetch('/api/user/profile', {
        method: 'PUT',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(data),
      })
      if (!response.ok) throw new Error('Failed to update profile')
      return response.json()
    },
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['user'] })
      toast.success('Profile updated successfully')
    },
    onError: () => toast.error('Failed to update profile'),
  })

  // Change password mutation
  const passwordMutation = useMutation({
    mutationFn: async (data: { currentPassword: string; newPassword: string }) => {
      const response = await fetch('/api/user/password', {
        method: 'PUT',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(data),
      })
      if (!response.ok) {
        const error = await response.json()
        throw new Error(error.message || 'Failed to change password')
      }
      return response.json()
    },
    onSuccess: () => {
      setCurrentPassword('')
      setNewPassword('')
      setConfirmPassword('')
      toast.success('Password changed successfully')
    },
    onError: (error: Error) => toast.error(error.message),
  })

  const handleProfileSave = () => {
    if (!name.trim() || !email.trim()) {
      toast.error('Name and email are required')
      return
    }
    profileMutation.mutate({ name: name.trim(), email: email.trim(), phone: phone.trim() })
  }

  const handlePasswordChange = () => {
    if (!currentPassword || !newPassword || !confirmPassword) {
      toast.error('All password fields are required')
      return
    }
    if (newPassword !== confirmPassword) {
      toast.error('New passwords do not match')
      return
    }
    if (newPassword.length < 8) {
      toast.error('Password must be at least 8 characters')
      return
    }
    passwordMutation.mutate({ currentPassword, newPassword })
  }

  const getRoleBadgeVariant = (role: string) => {
    switch (role) {
      case 'super_admin': return 'destructive'
      case 'regional': return 'default'
      case 'office_leader': return 'secondary'
      default: return 'outline'
    }
  }

  const getRoleDisplayName = (role: string) => {
    switch (role) {
      case 'super_admin': return 'Super Admin'
      case 'regional': return 'Regional Manager'
      case 'office_leader': return 'Office Leader'
      case 'setter': return 'Setter'
      case 'closer': return 'Closer'
      default: return role
    }
  }

  return (
    <div className="space-y-6">
      {/* Profile Information */}
      <Card>
        <CardHeader>
          <CardTitle>Profile Information</CardTitle>
        </CardHeader>
        <CardContent className="space-y-4">
          {/* Avatar & Role */}
          <div className="flex items-center gap-4 pb-4 border-b">
            <div className="w-16 h-16 rounded-full bg-blue-100 flex items-center justify-center">
              <User className="w-8 h-8 text-blue-600" />
            </div>
            <div>
              <h3 className="text-xl font-semibold">{user.name}</h3>
              <Badge variant={getRoleBadgeVariant(user.role)}>
                {getRoleDisplayName(user.role)}
              </Badge>
            </div>
          </div>

          {/* Name */}
          <div className="space-y-2">
            <Label htmlFor="name">Full Name</Label>
            <Input
              id="name"
              value={name}
              onChange={(e) => setName(e.target.value)}
              placeholder="Enter your full name"
            />
          </div>

          {/* Email */}
          <div className="space-y-2">
            <Label htmlFor="email">Email Address</Label>
            <div className="relative">
              <Mail className="absolute left-3 top-1/2 -translate-y-1/2 w-4 h-4 text-gray-400" />
              <Input
                id="email"
                type="email"
                value={email}
                onChange={(e) => setEmail(e.target.value)}
                placeholder="your.email@kinhome.com"
                className="pl-10"
              />
            </div>
          </div>

          {/* Phone */}
          <div className="space-y-2">
            <Label htmlFor="phone">Phone Number</Label>
            <div className="relative">
              <Phone className="absolute left-3 top-1/2 -translate-y-1/2 w-4 h-4 text-gray-400" />
              <Input
                id="phone"
                type="tel"
                value={phone}
                onChange={(e) => setPhone(e.target.value)}
                placeholder="(555) 123-4567"
                className="pl-10"
              />
            </div>
          </div>

          {/* Read-only fields */}
          <div className="grid grid-cols-2 gap-4 pt-4 border-t">
            <div className="space-y-2">
              <Label className="text-gray-500">Quickbase User ID</Label>
              <p className="text-sm font-mono text-gray-700">{user.quickbaseUserId}</p>
            </div>
            <div className="space-y-2">
              <Label className="text-gray-500">Office</Label>
              <p className="text-sm text-gray-700">{user.office || 'Not assigned'}</p>
            </div>
          </div>

          {/* Save Button */}
          <Button
            onClick={handleProfileSave}
            disabled={profileMutation.isPending}
            className="w-full"
          >
            {profileMutation.isPending ? 'Saving...' : 'Save Profile'}
          </Button>
        </CardContent>
      </Card>

      {/* Change Password */}
      <Card>
        <CardHeader>
          <CardTitle>Change Password</CardTitle>
        </CardHeader>
        <CardContent className="space-y-4">
          <div className="space-y-2">
            <Label htmlFor="current-password">Current Password</Label>
            <Input
              id="current-password"
              type="password"
              value={currentPassword}
              onChange={(e) => setCurrentPassword(e.target.value)}
              placeholder="Enter current password"
            />
          </div>

          <div className="space-y-2">
            <Label htmlFor="new-password">New Password</Label>
            <Input
              id="new-password"
              type="password"
              value={newPassword}
              onChange={(e) => setNewPassword(e.target.value)}
              placeholder="Enter new password (min 8 characters)"
            />
          </div>

          <div className="space-y-2">
            <Label htmlFor="confirm-password">Confirm New Password</Label>
            <Input
              id="confirm-password"
              type="password"
              value={confirmPassword}
              onChange={(e) => setConfirmPassword(e.target.value)}
              placeholder="Confirm new password"
            />
          </div>

          <Button
            onClick={handlePasswordChange}
            disabled={passwordMutation.isPending}
            className="w-full"
          >
            {passwordMutation.isPending ? 'Changing...' : 'Change Password'}
          </Button>
        </CardContent>
      </Card>

      {/* Danger Zone */}
      <Card className="border-red-200">
        <CardHeader>
          <CardTitle className="text-red-700 flex items-center gap-2">
            <AlertTriangle className="w-5 h-5" />
            Danger Zone
          </CardTitle>
        </CardHeader>
        <CardContent>
          <p className="text-sm text-gray-600 mb-4">
            Sign out from all devices and sessions. You'll need to log in again everywhere.
          </p>
          <Button variant="destructive" className="w-full">
            Sign Out All Sessions
          </Button>
        </CardContent>
      </Card>
    </div>
  )
}
```

### Tab 2: Notifications

#### Components Needed
```
NotificationsTab
├── EmailNotificationsSection
│   ├── Enable/disable email notifications toggle
│   ├── Urgent alerts toggle (projects on hold > X days)
│   ├── Daily digest toggle
│   └── Weekly summary toggle
├── AlertThresholdsSection (Office Leaders & Above)
│   ├── Days on hold before urgent (number input)
│   ├── Project age warning threshold (number input)
│   └── Install overdue threshold (number input)
└── NotificationPreview
    └── Test notification button
```

#### Implementation

**File**: `components/settings/NotificationsTab.tsx`

```typescript
'use client'

import { useState } from 'react'
import { useMutation, useQueryClient } from '@tanstack/react-query'
import { Card, CardHeader, CardTitle, CardContent, CardDescription } from '@/components/ui/card'
import { Switch } from '@/components/ui/switch'
import { Input } from '@/components/ui/input'
import { Button } from '@/components/ui/button'
import { Label } from '@/components/ui/label'
import { Bell, Mail, AlertCircle } from 'lucide-react'
import { toast } from 'sonner'

interface NotificationsTabProps {
  userRole: string
  currentSettings: NotificationSettings
}

interface NotificationSettings {
  emailEnabled: boolean
  urgentAlerts: boolean
  dailyDigest: boolean
  weeklySummary: boolean
  holdThreshold: number
  ageWarningThreshold: number
  installOverdueThreshold: number
}

export function NotificationsTab({ userRole, currentSettings }: NotificationsTabProps) {
  const queryClient = useQueryClient()

  const [settings, setSettings] = useState<NotificationSettings>(currentSettings)

  const updateMutation = useMutation({
    mutationFn: async (data: NotificationSettings) => {
      const response = await fetch('/api/user/notifications', {
        method: 'PUT',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(data),
      })
      if (!response.ok) throw new Error('Failed to update notifications')
      return response.json()
    },
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['user-settings'] })
      toast.success('Notification settings updated')
    },
    onError: () => toast.error('Failed to update settings'),
  })

  const handleSave = () => {
    updateMutation.mutate(settings)
  }

  const sendTestNotification = async () => {
    try {
      const response = await fetch('/api/user/test-notification', {
        method: 'POST',
      })
      if (!response.ok) throw new Error('Failed to send test')
      toast.success('Test notification sent! Check your email.')
    } catch (error) {
      toast.error('Failed to send test notification')
    }
  }

  const hasTeamAccess = ['office_leader', 'regional', 'super_admin'].includes(userRole)

  return (
    <div className="space-y-6">
      {/* Email Notifications */}
      <Card>
        <CardHeader>
          <CardTitle className="flex items-center gap-2">
            <Mail className="w-5 h-5" />
            Email Notifications
          </CardTitle>
          <CardDescription>
            Manage how you receive updates and alerts via email
          </CardDescription>
        </CardHeader>
        <CardContent className="space-y-4">
          {/* Master toggle */}
          <div className="flex items-center justify-between">
            <div className="space-y-0.5">
              <Label htmlFor="email-enabled">Enable Email Notifications</Label>
              <p className="text-sm text-gray-500">
                Receive all email notifications from the dashboard
              </p>
            </div>
            <Switch
              id="email-enabled"
              checked={settings.emailEnabled}
              onCheckedChange={(checked) => setSettings({ ...settings, emailEnabled: checked })}
            />
          </div>

          {/* Urgent Alerts */}
          <div className="flex items-center justify-between">
            <div className="space-y-0.5">
              <Label htmlFor="urgent-alerts">Urgent Alerts</Label>
              <p className="text-sm text-gray-500">
                Get notified when projects need immediate attention
              </p>
            </div>
            <Switch
              id="urgent-alerts"
              checked={settings.urgentAlerts}
              onCheckedChange={(checked) => setSettings({ ...settings, urgentAlerts: checked })}
              disabled={!settings.emailEnabled}
            />
          </div>

          {/* Daily Digest */}
          <div className="flex items-center justify-between">
            <div className="space-y-0.5">
              <Label htmlFor="daily-digest">Daily Digest</Label>
              <p className="text-sm text-gray-500">
                Receive a summary of your projects every morning at 8 AM
              </p>
            </div>
            <Switch
              id="daily-digest"
              checked={settings.dailyDigest}
              onCheckedChange={(checked) => setSettings({ ...settings, dailyDigest: checked })}
              disabled={!settings.emailEnabled}
            />
          </div>

          {/* Weekly Summary */}
          <div className="flex items-center justify-between">
            <div className="space-y-0.5">
              <Label htmlFor="weekly-summary">Weekly Summary</Label>
              <p className="text-sm text-gray-500">
                Get a weekly performance report every Monday at 9 AM
              </p>
            </div>
            <Switch
              id="weekly-summary"
              checked={settings.weeklySummary}
              onCheckedChange={(checked) => setSettings({ ...settings, weeklySummary: checked })}
              disabled={!settings.emailEnabled}
            />
          </div>
        </CardContent>
      </Card>

      {/* Alert Thresholds - Office Leaders & Above */}
      {hasTeamAccess && (
        <Card>
          <CardHeader>
            <CardTitle className="flex items-center gap-2">
              <AlertCircle className="w-5 h-5" />
              Alert Thresholds
            </CardTitle>
            <CardDescription>
              Configure when you receive urgent alerts based on project status
            </CardDescription>
          </CardHeader>
          <CardContent className="space-y-4">
            <div className="space-y-2">
              <Label htmlFor="hold-threshold">
                Days on Hold Before Urgent Alert
              </Label>
              <Input
                id="hold-threshold"
                type="number"
                min="1"
                max="30"
                value={settings.holdThreshold}
                onChange={(e) => setSettings({ ...settings, holdThreshold: parseInt(e.target.value) || 7 })}
              />
              <p className="text-xs text-gray-500">
                Receive alerts when projects are on hold for this many days (default: 7)
              </p>
            </div>

            <div className="space-y-2">
              <Label htmlFor="age-warning">
                Project Age Warning Threshold (days)
              </Label>
              <Input
                id="age-warning"
                type="number"
                min="30"
                max="180"
                value={settings.ageWarningThreshold}
                onChange={(e) => setSettings({ ...settings, ageWarningThreshold: parseInt(e.target.value) || 90 })}
              />
              <p className="text-xs text-gray-500">
                Warn when projects exceed this age without completion (default: 90)
              </p>
            </div>

            <div className="space-y-2">
              <Label htmlFor="install-overdue">
                Install Overdue Threshold (days)
              </Label>
              <Input
                id="install-overdue"
                type="number"
                min="7"
                max="60"
                value={settings.installOverdueThreshold}
                onChange={(e) => setSettings({ ...settings, installOverdueThreshold: parseInt(e.target.value) || 14 })}
              />
              <p className="text-xs text-gray-500">
                Alert when install is scheduled but not completed within this timeframe (default: 14)
              </p>
            </div>
          </CardContent>
        </Card>
      )}

      {/* Test Notification */}
      <Card>
        <CardHeader>
          <CardTitle>Test Notifications</CardTitle>
          <CardDescription>
            Send a test email to verify your notification settings
          </CardDescription>
        </CardHeader>
        <CardContent>
          <Button
            variant="outline"
            onClick={sendTestNotification}
            className="w-full"
          >
            <Bell className="w-4 h-4 mr-2" />
            Send Test Email
          </Button>
        </CardContent>
      </Card>

      {/* Save Button */}
      <Button
        onClick={handleSave}
        disabled={updateMutation.isPending}
        className="w-full"
        size="lg"
      >
        {updateMutation.isPending ? 'Saving...' : 'Save Notification Settings'}
      </Button>
    </div>
  )
}
```

---

## Phase 5B: User Management (Admin)

**Time Estimate**: 3-4 hours
**Priority**: MEDIUM - Build after Phase 5A
**Who Gets It**: Super Admin only

### Tab 3: Users (Super Admin Only)

#### Components Needed
```
UsersTab
├── UsersTableHeader
│   ├── Search users input
│   ├── Filter by role dropdown
│   ├── Filter by office dropdown
│   └── Add new user button
├── UsersTable
│   ├── User row (name, email, role, office, status, actions)
│   ├── Edit user button
│   ├── Deactivate/activate toggle
│   └── Reset password button
└── AddUserDialog
    ├── Name input
    ├── Email input
    ├── Phone input
    ├── Role selector
    ├── Office selector
    ├── Quickbase User ID input
    └── Create button
```

#### User Schema

```typescript
// lib/types/user.ts
export interface User {
  id: string
  name: string
  email: string
  phone: string
  role: 'closer' | 'setter' | 'office_leader' | 'regional' | 'super_admin'
  quickbaseUserId: string
  office: string | null
  region: string | null
  isActive: boolean
  createdAt: string
  updatedAt: string
  lastLoginAt: string | null
}

export interface CreateUserInput {
  name: string
  email: string
  phone: string
  role: string
  quickbaseUserId: string
  office?: string
  region?: string
  temporaryPassword: string
}

export interface UpdateUserInput {
  name?: string
  email?: string
  phone?: string
  role?: string
  office?: string
  region?: string
  isActive?: boolean
}
```

#### Implementation

**File**: `components/settings/UsersTab.tsx`

```typescript
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
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from '@/components/ui/select'
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
import { UserPlus, Search, MoreVertical, Edit, Trash2, Key } from 'lucide-react'
import { toast } from 'sonner'
import type { User, CreateUserInput } from '@/lib/types/user'

export function UsersTab() {
  const queryClient = useQueryClient()

  const [searchQuery, setSearchQuery] = useState('')
  const [roleFilter, setRoleFilter] = useState<string>('all')
  const [officeFilter, setOfficeFilter] = useState<string>('all')
  const [isAddDialogOpen, setIsAddDialogOpen] = useState(false)

  // New user form state
  const [newUser, setNewUser] = useState<CreateUserInput>({
    name: '',
    email: '',
    phone: '',
    role: 'closer',
    quickbaseUserId: '',
    office: '',
    temporaryPassword: '',
  })

  // Fetch users
  const { data: users, isLoading } = useQuery({
    queryKey: ['users', searchQuery, roleFilter, officeFilter],
    queryFn: async () => {
      const params = new URLSearchParams()
      if (searchQuery) params.set('search', searchQuery)
      if (roleFilter !== 'all') params.set('role', roleFilter)
      if (officeFilter !== 'all') params.set('office', officeFilter)

      const response = await fetch(`/api/admin/users?${params.toString()}`)
      if (!response.ok) throw new Error('Failed to fetch users')
      return response.json() as Promise<User[]>
    },
  })

  // Create user mutation
  const createUserMutation = useMutation({
    mutationFn: async (data: CreateUserInput) => {
      const response = await fetch('/api/admin/users', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(data),
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
        temporaryPassword: '',
      })
      toast.success('User created successfully')
    },
    onError: (error: Error) => toast.error(error.message),
  })

  // Toggle user active status
  const toggleActiveMutation = useMutation({
    mutationFn: async ({ userId, isActive }: { userId: string; isActive: boolean }) => {
      const response = await fetch(`/api/admin/users/${userId}`, {
        method: 'PATCH',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ isActive }),
      })
      if (!response.ok) throw new Error('Failed to update user')
      return response.json()
    },
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['users'] })
      toast.success('User status updated')
    },
    onError: () => toast.error('Failed to update user status'),
  })

  // Reset password mutation
  const resetPasswordMutation = useMutation({
    mutationFn: async (userId: string) => {
      const response = await fetch(`/api/admin/users/${userId}/reset-password`, {
        method: 'POST',
      })
      if (!response.ok) throw new Error('Failed to reset password')
      return response.json()
    },
    onSuccess: (data) => {
      toast.success(`Password reset. Temp password: ${data.temporaryPassword}`)
    },
    onError: () => toast.error('Failed to reset password'),
  })

  const handleCreateUser = () => {
    if (!newUser.name || !newUser.email || !newUser.quickbaseUserId) {
      toast.error('Name, email, and Quickbase User ID are required')
      return
    }
    createUserMutation.mutate(newUser)
  }

  const getRoleBadgeVariant = (role: string) => {
    switch (role) {
      case 'super_admin': return 'destructive'
      case 'regional': return 'default'
      case 'office_leader': return 'secondary'
      default: return 'outline'
    }
  }

  return (
    <div className="space-y-6">
      {/* Header & Actions */}
      <div className="flex items-center justify-between">
        <div>
          <h2 className="text-2xl font-bold">User Management</h2>
          <p className="text-gray-600">Manage user accounts and permissions</p>
        </div>
        <Dialog open={isAddDialogOpen} onOpenChange={setIsAddDialogOpen}>
          <DialogTrigger asChild>
            <Button>
              <UserPlus className="w-4 h-4 mr-2" />
              Add User
            </Button>
          </DialogTrigger>
          <DialogContent className="max-w-md">
            <DialogHeader>
              <DialogTitle>Create New User</DialogTitle>
              <DialogDescription>
                Add a new user to the system. They'll receive login instructions via email.
              </DialogDescription>
            </DialogHeader>
            <div className="space-y-4">
              <div className="space-y-2">
                <Label htmlFor="new-name">Full Name *</Label>
                <Input
                  id="new-name"
                  value={newUser.name}
                  onChange={(e) => setNewUser({ ...newUser, name: e.target.value })}
                  placeholder="John Doe"
                />
              </div>

              <div className="space-y-2">
                <Label htmlFor="new-email">Email *</Label>
                <Input
                  id="new-email"
                  type="email"
                  value={newUser.email}
                  onChange={(e) => setNewUser({ ...newUser, email: e.target.value })}
                  placeholder="john.doe@kinhome.com"
                />
              </div>

              <div className="space-y-2">
                <Label htmlFor="new-phone">Phone</Label>
                <Input
                  id="new-phone"
                  type="tel"
                  value={newUser.phone}
                  onChange={(e) => setNewUser({ ...newUser, phone: e.target.value })}
                  placeholder="(555) 123-4567"
                />
              </div>

              <div className="space-y-2">
                <Label htmlFor="new-role">Role *</Label>
                <Select
                  value={newUser.role}
                  onValueChange={(value) => setNewUser({ ...newUser, role: value })}
                >
                  <SelectTrigger id="new-role">
                    <SelectValue />
                  </SelectTrigger>
                  <SelectContent>
                    <SelectItem value="closer">Closer</SelectItem>
                    <SelectItem value="setter">Setter</SelectItem>
                    <SelectItem value="office_leader">Office Leader</SelectItem>
                    <SelectItem value="regional">Regional Manager</SelectItem>
                    <SelectItem value="super_admin">Super Admin</SelectItem>
                  </SelectContent>
                </Select>
              </div>

              <div className="space-y-2">
                <Label htmlFor="new-quickbase-id">Quickbase User ID *</Label>
                <Input
                  id="new-quickbase-id"
                  value={newUser.quickbaseUserId}
                  onChange={(e) => setNewUser({ ...newUser, quickbaseUserId: e.target.value })}
                  placeholder="12345678"
                />
              </div>

              <div className="space-y-2">
                <Label htmlFor="new-office">Office</Label>
                <Input
                  id="new-office"
                  value={newUser.office}
                  onChange={(e) => setNewUser({ ...newUser, office: e.target.value })}
                  placeholder="Austin, TX"
                />
              </div>

              <div className="space-y-2">
                <Label htmlFor="new-password">Temporary Password *</Label>
                <Input
                  id="new-password"
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
          <Search className="absolute left-3 top-1/2 -translate-y-1/2 w-4 h-4 text-gray-400" />
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
            <SelectItem value="office_leader">Office Leaders</SelectItem>
            <SelectItem value="regional">Regional Managers</SelectItem>
            <SelectItem value="super_admin">Super Admins</SelectItem>
          </SelectContent>
        </Select>
      </div>

      {/* Users Table */}
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
              <TableHead className="text-right">Actions</TableHead>
            </TableRow>
          </TableHeader>
          <TableBody>
            {isLoading ? (
              <TableRow>
                <TableCell colSpan={7} className="text-center py-8">
                  Loading users...
                </TableCell>
              </TableRow>
            ) : users && users.length > 0 ? (
              users.map((user) => (
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
                      : 'Never'}
                  </TableCell>
                  <TableCell className="text-right">
                    <div className="flex items-center justify-end gap-2">
                      <Button variant="ghost" size="sm">
                        <Edit className="w-4 h-4" />
                      </Button>
                      <Button
                        variant="ghost"
                        size="sm"
                        onClick={() => resetPasswordMutation.mutate(user.id)}
                      >
                        <Key className="w-4 h-4" />
                      </Button>
                    </div>
                  </TableCell>
                </TableRow>
              ))
            ) : (
              <TableRow>
                <TableCell colSpan={7} className="text-center py-8">
                  No users found
                </TableCell>
              </TableRow>
            )}
          </TableBody>
        </Table>
      </div>
    </div>
  )
}
```

### Tab 4: Offices (Super Admin Only)

#### Components Needed
```
OfficesTab
├── OfficesHeader
│   ├── Search offices
│   └── Add new office button
├── OfficesGrid
│   ├── Office card (name, leader, region, user count)
│   ├── Edit office button
│   └── Delete office button (with confirmation)
└── AddOfficeDialog
    ├── Office name input
    ├── Region selector
    ├── Office leader selector
    └── Create button
```

#### Office Schema

```typescript
// lib/types/office.ts
export interface Office {
  id: string
  name: string
  region: string
  leaderId: string
  leaderName: string
  userCount: number
  activeProjects: number
  createdAt: string
  updatedAt: string
}

export interface CreateOfficeInput {
  name: string
  region: string
  leaderId: string
}

export interface UpdateOfficeInput {
  name?: string
  region?: string
  leaderId?: string
}
```

#### Implementation

**File**: `components/settings/OfficesTab.tsx`

```typescript
'use client'

import { useState } from 'react'
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query'
import { Card, CardHeader, CardTitle, CardContent } from '@/components/ui/card'
import { Input } from '@/components/ui/input'
import { Button } from '@/components/ui/button'
import { Badge } from '@/components/ui/badge'
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from '@/components/ui/select'
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
import { Building2, Search, Users, Briefcase, Edit, Trash2 } from 'lucide-react'
import { toast } from 'sonner'
import type { Office, CreateOfficeInput } from '@/lib/types/office'
import type { User } from '@/lib/types/user'

export function OfficesTab() {
  const queryClient = useQueryClient()

  const [searchQuery, setSearchQuery] = useState('')
  const [isAddDialogOpen, setIsAddDialogOpen] = useState(false)
  const [officeToDelete, setOfficeToDelete] = useState<Office | null>(null)

  // New office form state
  const [newOffice, setNewOffice] = useState<CreateOfficeInput>({
    name: '',
    region: '',
    leaderId: '',
  })

  // Fetch offices
  const { data: offices, isLoading: officesLoading } = useQuery({
    queryKey: ['offices', searchQuery],
    queryFn: async () => {
      const params = new URLSearchParams()
      if (searchQuery) params.set('search', searchQuery)

      const response = await fetch(`/api/admin/offices?${params.toString()}`)
      if (!response.ok) throw new Error('Failed to fetch offices')
      return response.json() as Promise<Office[]>
    },
  })

  // Fetch potential office leaders (office_leader role and above)
  const { data: potentialLeaders } = useQuery({
    queryKey: ['potential-leaders'],
    queryFn: async () => {
      const response = await fetch('/api/admin/users?role=office_leader,regional,super_admin')
      if (!response.ok) throw new Error('Failed to fetch leaders')
      return response.json() as Promise<User[]>
    },
  })

  // Create office mutation
  const createOfficeMutation = useMutation({
    mutationFn: async (data: CreateOfficeInput) => {
      const response = await fetch('/api/admin/offices', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(data),
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
      setNewOffice({ name: '', region: '', leaderId: '' })
      toast.success('Office created successfully')
    },
    onError: (error: Error) => toast.error(error.message),
  })

  // Delete office mutation
  const deleteOfficeMutation = useMutation({
    mutationFn: async (officeId: string) => {
      const response = await fetch(`/api/admin/offices/${officeId}`, {
        method: 'DELETE',
      })
      if (!response.ok) throw new Error('Failed to delete office')
      return response.json()
    },
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['offices'] })
      setOfficeToDelete(null)
      toast.success('Office deleted successfully')
    },
    onError: () => toast.error('Failed to delete office'),
  })

  const handleCreateOffice = () => {
    if (!newOffice.name || !newOffice.region || !newOffice.leaderId) {
      toast.error('All fields are required')
      return
    }
    createOfficeMutation.mutate(newOffice)
  }

  const handleDeleteOffice = () => {
    if (officeToDelete) {
      deleteOfficeMutation.mutate(officeToDelete.id)
    }
  }

  return (
    <div className="space-y-6">
      {/* Header & Actions */}
      <div className="flex items-center justify-between">
        <div>
          <h2 className="text-2xl font-bold">Office Management</h2>
          <p className="text-gray-600">Manage sales offices and their leadership</p>
        </div>
        <Dialog open={isAddDialogOpen} onOpenChange={setIsAddDialogOpen}>
          <DialogTrigger asChild>
            <Button>
              <Building2 className="w-4 h-4 mr-2" />
              Add Office
            </Button>
          </DialogTrigger>
          <DialogContent className="max-w-md">
            <DialogHeader>
              <DialogTitle>Create New Office</DialogTitle>
              <DialogDescription>
                Add a new sales office to the system
              </DialogDescription>
            </DialogHeader>
            <div className="space-y-4">
              <div className="space-y-2">
                <Label htmlFor="office-name">Office Name *</Label>
                <Input
                  id="office-name"
                  value={newOffice.name}
                  onChange={(e) => setNewOffice({ ...newOffice, name: e.target.value })}
                  placeholder="Austin, TX"
                />
              </div>

              <div className="space-y-2">
                <Label htmlFor="office-region">Region *</Label>
                <Select
                  value={newOffice.region}
                  onValueChange={(value) => setNewOffice({ ...newOffice, region: value })}
                >
                  <SelectTrigger id="office-region">
                    <SelectValue placeholder="Select region" />
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

              <div className="space-y-2">
                <Label htmlFor="office-leader">Office Leader *</Label>
                <Select
                  value={newOffice.leaderId}
                  onValueChange={(value) => setNewOffice({ ...newOffice, leaderId: value })}
                >
                  <SelectTrigger id="office-leader">
                    <SelectValue placeholder="Select office leader" />
                  </SelectTrigger>
                  <SelectContent>
                    {potentialLeaders?.map((leader) => (
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

      {/* Search */}
      <div className="relative">
        <Search className="absolute left-3 top-1/2 -translate-y-1/2 w-4 h-4 text-gray-400" />
        <Input
          placeholder="Search offices by name or region..."
          value={searchQuery}
          onChange={(e) => setSearchQuery(e.target.value)}
          className="pl-10"
        />
      </div>

      {/* Offices Grid */}
      {officesLoading ? (
        <div className="text-center py-12">Loading offices...</div>
      ) : offices && offices.length > 0 ? (
        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
          {offices.map((office) => (
            <Card key={office.id}>
              <CardHeader>
                <div className="flex items-start justify-between">
                  <div className="flex items-center gap-3">
                    <div className="w-10 h-10 rounded-lg bg-blue-100 flex items-center justify-center">
                      <Building2 className="w-5 h-5 text-blue-600" />
                    </div>
                    <div>
                      <CardTitle className="text-lg">{office.name}</CardTitle>
                      <Badge variant="outline" className="mt-1">
                        {office.region}
                      </Badge>
                    </div>
                  </div>
                  <div className="flex gap-1">
                    <Button variant="ghost" size="sm">
                      <Edit className="w-4 h-4" />
                    </Button>
                    <Button
                      variant="ghost"
                      size="sm"
                      onClick={() => setOfficeToDelete(office)}
                    >
                      <Trash2 className="w-4 h-4 text-red-600" />
                    </Button>
                  </div>
                </div>
              </CardHeader>
              <CardContent className="space-y-3">
                <div className="flex items-center gap-2 text-sm">
                  <Users className="w-4 h-4 text-gray-400" />
                  <span className="text-gray-600">Office Leader:</span>
                  <span className="font-medium">{office.leaderName}</span>
                </div>
                <div className="flex items-center gap-2 text-sm">
                  <Users className="w-4 h-4 text-gray-400" />
                  <span className="text-gray-600">Team Members:</span>
                  <span className="font-medium">{office.userCount}</span>
                </div>
                <div className="flex items-center gap-2 text-sm">
                  <Briefcase className="w-4 h-4 text-gray-400" />
                  <span className="text-gray-600">Active Projects:</span>
                  <span className="font-medium">{office.activeProjects}</span>
                </div>
              </CardContent>
            </Card>
          ))}
        </div>
      ) : (
        <div className="text-center py-12 text-gray-600">
          No offices found. Create your first office to get started.
        </div>
      )}

      {/* Delete Confirmation Dialog */}
      <AlertDialog open={!!officeToDelete} onOpenChange={() => setOfficeToDelete(null)}>
        <AlertDialogContent>
          <AlertDialogHeader>
            <AlertDialogTitle>Delete Office</AlertDialogTitle>
            <AlertDialogDescription>
              Are you sure you want to delete <strong>{officeToDelete?.name}</strong>?
              This action cannot be undone. All users in this office will need to be reassigned.
            </AlertDialogDescription>
          </AlertDialogHeader>
          <AlertDialogFooter>
            <AlertDialogCancel>Cancel</AlertDialogCancel>
            <AlertDialogAction
              onClick={handleDeleteOffice}
              className="bg-red-600 hover:bg-red-700"
            >
              Delete Office
            </AlertDialogAction>
          </AlertDialogFooter>
        </AlertDialogContent>
      </AlertDialog>
    </div>
  )
}
```

---

## Phase 5C: System Configuration (Advanced)

**Time Estimate**: 4-5 hours
**Priority**: LOW - Build after 5A and 5B
**Who Gets It**: Super Admin only

### Tab 5: System Settings (Super Admin Only)

#### Components Needed
```
SystemTab
├── QuickbaseConnectionCard
│   ├── Realm hostname input
│   ├── User token (masked)
│   ├── Test connection button
│   └── Connection status indicator
├── MilestoneDefaultsCard
│   ├── Default SLA days for each milestone
│   ├── Warning threshold (% of SLA)
│   └── Critical threshold (% of SLA)
├── HoldTypesCard
│   ├── List of hold reasons (editable)
│   ├── Add custom hold reason
│   └── Remove custom hold reason
└── GeneralSettingsCard
    ├── Date format selector
    ├── Timezone selector
    └── Session timeout (minutes)
```

#### Implementation

**File**: `components/settings/SystemTab.tsx`

```typescript
'use client'

import { useState } from 'react'
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query'
import { Card, CardHeader, CardTitle, CardContent, CardDescription } from '@/components/ui/card'
import { Input } from '@/components/ui/input'
import { Button } from '@/components/ui/button'
import { Label } from '@/components/ui/label'
import { Badge } from '@/components/ui/badge'
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from '@/components/ui/select'
import { Database, Settings, Clock, AlertCircle, CheckCircle, XCircle } from 'lucide-react'
import { toast } from 'sonner'

interface SystemSettings {
  quickbaseRealm: string
  quickbaseToken: string
  milestoneSLA: {
    survey: number
    design: number
    permit: number
    nem: number
    install: number
    inspection: number
    pto: number
  }
  warningThreshold: number
  criticalThreshold: number
  holdReasons: string[]
  dateFormat: string
  timezone: string
  sessionTimeout: number
}

export function SystemTab() {
  const queryClient = useQueryClient()
  const [connectionStatus, setConnectionStatus] = useState<'idle' | 'testing' | 'success' | 'error'>('idle')

  // Fetch current settings
  const { data: settings, isLoading } = useQuery<SystemSettings>({
    queryKey: ['system-settings'],
    queryFn: async () => {
      const response = await fetch('/api/admin/system/settings')
      if (!response.ok) throw new Error('Failed to fetch settings')
      return response.json()
    },
  })

  const [localSettings, setLocalSettings] = useState<SystemSettings | null>(null)

  // Use local state or fallback to fetched settings
  const currentSettings = localSettings || settings

  // Update settings mutation
  const updateMutation = useMutation({
    mutationFn: async (data: Partial<SystemSettings>) => {
      const response = await fetch('/api/admin/system/settings', {
        method: 'PUT',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(data),
      })
      if (!response.ok) throw new Error('Failed to update settings')
      return response.json()
    },
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['system-settings'] })
      toast.success('System settings updated')
    },
    onError: () => toast.error('Failed to update settings'),
  })

  const testQuickbaseConnection = async () => {
    if (!currentSettings) return

    setConnectionStatus('testing')
    try {
      const response = await fetch('/api/admin/system/test-connection', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          realm: currentSettings.quickbaseRealm,
          token: currentSettings.quickbaseToken,
        }),
      })

      if (!response.ok) throw new Error('Connection failed')

      setConnectionStatus('success')
      toast.success('Quickbase connection successful')
    } catch (error) {
      setConnectionStatus('error')
      toast.error('Quickbase connection failed')
    }
  }

  const handleSave = () => {
    if (currentSettings) {
      updateMutation.mutate(currentSettings)
    }
  }

  const addHoldReason = (reason: string) => {
    if (!currentSettings) return
    if (currentSettings.holdReasons.includes(reason)) {
      toast.error('Hold reason already exists')
      return
    }
    setLocalSettings({
      ...currentSettings,
      holdReasons: [...currentSettings.holdReasons, reason],
    })
  }

  const removeHoldReason = (reason: string) => {
    if (!currentSettings) return
    setLocalSettings({
      ...currentSettings,
      holdReasons: currentSettings.holdReasons.filter((r) => r !== reason),
    })
  }

  if (isLoading || !currentSettings) {
    return <div className="text-center py-12">Loading system settings...</div>
  }

  return (
    <div className="space-y-6">
      {/* Quickbase Connection */}
      <Card>
        <CardHeader>
          <CardTitle className="flex items-center gap-2">
            <Database className="w-5 h-5" />
            Quickbase Connection
          </CardTitle>
          <CardDescription>
            Configure connection to Quickbase for project data sync
          </CardDescription>
        </CardHeader>
        <CardContent className="space-y-4">
          <div className="space-y-2">
            <Label htmlFor="qb-realm">Realm Hostname</Label>
            <Input
              id="qb-realm"
              value={currentSettings.quickbaseRealm}
              onChange={(e) =>
                setLocalSettings({ ...currentSettings, quickbaseRealm: e.target.value })
              }
              placeholder="example.quickbase.com"
            />
          </div>

          <div className="space-y-2">
            <Label htmlFor="qb-token">User Token</Label>
            <Input
              id="qb-token"
              type="password"
              value={currentSettings.quickbaseToken}
              onChange={(e) =>
                setLocalSettings({ ...currentSettings, quickbaseToken: e.target.value })
              }
              placeholder="b6_xxxx_xxxx_xxxx"
            />
          </div>

          <div className="flex items-center gap-3">
            <Button
              variant="outline"
              onClick={testQuickbaseConnection}
              disabled={connectionStatus === 'testing'}
            >
              {connectionStatus === 'testing' ? 'Testing...' : 'Test Connection'}
            </Button>

            {connectionStatus === 'success' && (
              <Badge variant="default" className="bg-green-600">
                <CheckCircle className="w-3 h-3 mr-1" />
                Connected
              </Badge>
            )}
            {connectionStatus === 'error' && (
              <Badge variant="destructive">
                <XCircle className="w-3 h-3 mr-1" />
                Failed
              </Badge>
            )}
          </div>
        </CardContent>
      </Card>

      {/* Milestone SLA Defaults */}
      <Card>
        <CardHeader>
          <CardTitle className="flex items-center gap-2">
            <Clock className="w-5 h-5" />
            Milestone SLA Defaults
          </CardTitle>
          <CardDescription>
            Set default number of days expected for each milestone
          </CardDescription>
        </CardHeader>
        <CardContent className="space-y-4">
          <div className="grid grid-cols-2 gap-4">
            <div className="space-y-2">
              <Label htmlFor="sla-survey">Survey (days)</Label>
              <Input
                id="sla-survey"
                type="number"
                min="1"
                value={currentSettings.milestoneSLA.survey}
                onChange={(e) =>
                  setLocalSettings({
                    ...currentSettings,
                    milestoneSLA: {
                      ...currentSettings.milestoneSLA,
                      survey: parseInt(e.target.value) || 7,
                    },
                  })
                }
              />
            </div>

            <div className="space-y-2">
              <Label htmlFor="sla-design">Design (days)</Label>
              <Input
                id="sla-design"
                type="number"
                min="1"
                value={currentSettings.milestoneSLA.design}
                onChange={(e) =>
                  setLocalSettings({
                    ...currentSettings,
                    milestoneSLA: {
                      ...currentSettings.milestoneSLA,
                      design: parseInt(e.target.value) || 10,
                    },
                  })
                }
              />
            </div>

            <div className="space-y-2">
              <Label htmlFor="sla-permit">Permit (days)</Label>
              <Input
                id="sla-permit"
                type="number"
                min="1"
                value={currentSettings.milestoneSLA.permit}
                onChange={(e) =>
                  setLocalSettings({
                    ...currentSettings,
                    milestoneSLA: {
                      ...currentSettings.milestoneSLA,
                      permit: parseInt(e.target.value) || 21,
                    },
                  })
                }
              />
            </div>

            <div className="space-y-2">
              <Label htmlFor="sla-nem">NEM (days)</Label>
              <Input
                id="sla-nem"
                type="number"
                min="1"
                value={currentSettings.milestoneSLA.nem}
                onChange={(e) =>
                  setLocalSettings({
                    ...currentSettings,
                    milestoneSLA: {
                      ...currentSettings.milestoneSLA,
                      nem: parseInt(e.target.value) || 14,
                    },
                  })
                }
              />
            </div>

            <div className="space-y-2">
              <Label htmlFor="sla-install">Install (days)</Label>
              <Input
                id="sla-install"
                type="number"
                min="1"
                value={currentSettings.milestoneSLA.install}
                onChange={(e) =>
                  setLocalSettings({
                    ...currentSettings,
                    milestoneSLA: {
                      ...currentSettings.milestoneSLA,
                      install: parseInt(e.target.value) || 7,
                    },
                  })
                }
              />
            </div>

            <div className="space-y-2">
              <Label htmlFor="sla-inspection">Inspection (days)</Label>
              <Input
                id="sla-inspection"
                type="number"
                min="1"
                value={currentSettings.milestoneSLA.inspection}
                onChange={(e) =>
                  setLocalSettings({
                    ...currentSettings,
                    milestoneSLA: {
                      ...currentSettings.milestoneSLA,
                      inspection: parseInt(e.target.value) || 5,
                    },
                  })
                }
              />
            </div>

            <div className="space-y-2">
              <Label htmlFor="sla-pto">PTO (days)</Label>
              <Input
                id="sla-pto"
                type="number"
                min="1"
                value={currentSettings.milestoneSLA.pto}
                onChange={(e) =>
                  setLocalSettings({
                    ...currentSettings,
                    milestoneSLA: {
                      ...currentSettings.milestoneSLA,
                      pto: parseInt(e.target.value) || 10,
                    },
                  })
                }
              />
            </div>
          </div>

          <div className="grid grid-cols-2 gap-4 pt-4 border-t">
            <div className="space-y-2">
              <Label htmlFor="warning-threshold">Warning Threshold (%)</Label>
              <Input
                id="warning-threshold"
                type="number"
                min="50"
                max="100"
                value={currentSettings.warningThreshold}
                onChange={(e) =>
                  setLocalSettings({
                    ...currentSettings,
                    warningThreshold: parseInt(e.target.value) || 75,
                  })
                }
              />
              <p className="text-xs text-gray-500">
                Show yellow warning at this % of SLA (default: 75%)
              </p>
            </div>

            <div className="space-y-2">
              <Label htmlFor="critical-threshold">Critical Threshold (%)</Label>
              <Input
                id="critical-threshold"
                type="number"
                min="50"
                max="150"
                value={currentSettings.criticalThreshold}
                onChange={(e) =>
                  setLocalSettings({
                    ...currentSettings,
                    criticalThreshold: parseInt(e.target.value) || 100,
                  })
                }
              />
              <p className="text-xs text-gray-500">
                Show red alert at this % of SLA (default: 100%)
              </p>
            </div>
          </div>
        </CardContent>
      </Card>

      {/* Hold Reasons Management */}
      <Card>
        <CardHeader>
          <CardTitle className="flex items-center gap-2">
            <AlertCircle className="w-5 h-5" />
            Hold Reasons
          </CardTitle>
          <CardDescription>
            Manage available hold reasons for projects
          </CardDescription>
        </CardHeader>
        <CardContent className="space-y-4">
          <div className="flex flex-wrap gap-2">
            {currentSettings.holdReasons.map((reason) => (
              <Badge
                key={reason}
                variant="secondary"
                className="cursor-pointer hover:bg-red-100"
                onClick={() => removeHoldReason(reason)}
              >
                {reason}
                <XCircle className="w-3 h-3 ml-2" />
              </Badge>
            ))}
          </div>

          <div className="flex gap-2">
            <Input
              placeholder="Add new hold reason..."
              onKeyDown={(e) => {
                if (e.key === 'Enter') {
                  addHoldReason(e.currentTarget.value)
                  e.currentTarget.value = ''
                }
              }}
            />
            <Button
              variant="outline"
              onClick={(e) => {
                const input = e.currentTarget.previousSibling as HTMLInputElement
                addHoldReason(input.value)
                input.value = ''
              }}
            >
              Add
            </Button>
          </div>
        </CardContent>
      </Card>

      {/* General Settings */}
      <Card>
        <CardHeader>
          <CardTitle className="flex items-center gap-2">
            <Settings className="w-5 h-5" />
            General Settings
          </CardTitle>
        </CardHeader>
        <CardContent className="space-y-4">
          <div className="grid grid-cols-2 gap-4">
            <div className="space-y-2">
              <Label htmlFor="date-format">Date Format</Label>
              <Select
                value={currentSettings.dateFormat}
                onValueChange={(value) =>
                  setLocalSettings({ ...currentSettings, dateFormat: value })
                }
              >
                <SelectTrigger id="date-format">
                  <SelectValue />
                </SelectTrigger>
                <SelectContent>
                  <SelectItem value="MM/DD/YYYY">MM/DD/YYYY</SelectItem>
                  <SelectItem value="DD/MM/YYYY">DD/MM/YYYY</SelectItem>
                  <SelectItem value="YYYY-MM-DD">YYYY-MM-DD</SelectItem>
                </SelectContent>
              </Select>
            </div>

            <div className="space-y-2">
              <Label htmlFor="timezone">Timezone</Label>
              <Select
                value={currentSettings.timezone}
                onValueChange={(value) =>
                  setLocalSettings({ ...currentSettings, timezone: value })
                }
              >
                <SelectTrigger id="timezone">
                  <SelectValue />
                </SelectTrigger>
                <SelectContent>
                  <SelectItem value="America/New_York">Eastern (ET)</SelectItem>
                  <SelectItem value="America/Chicago">Central (CT)</SelectItem>
                  <SelectItem value="America/Denver">Mountain (MT)</SelectItem>
                  <SelectItem value="America/Los_Angeles">Pacific (PT)</SelectItem>
                </SelectContent>
              </Select>
            </div>

            <div className="space-y-2">
              <Label htmlFor="session-timeout">Session Timeout (minutes)</Label>
              <Input
                id="session-timeout"
                type="number"
                min="15"
                max="480"
                value={currentSettings.sessionTimeout}
                onChange={(e) =>
                  setLocalSettings({
                    ...currentSettings,
                    sessionTimeout: parseInt(e.target.value) || 60,
                  })
                }
              />
            </div>
          </div>
        </CardContent>
      </Card>

      {/* Save Button */}
      <Button
        onClick={handleSave}
        disabled={updateMutation.isPending}
        className="w-full"
        size="lg"
      >
        {updateMutation.isPending ? 'Saving...' : 'Save System Settings'}
      </Button>
    </div>
  )
}
```

---

## Phase 5D: Analytics & Audit (Enterprise)

**Time Estimate**: 3-4 hours
**Priority**: LOWEST - Build last
**Who Gets It**: Super Admin only

### Tab 6: Audit Logs (Super Admin Only)

#### Components Needed
```
AuditLogsTab
├── FilterBar
│   ├── Date range picker
│   ├── User filter
│   ├── Action type filter (Login, Update, Delete, etc.)
│   └── Export to CSV button
├── AuditLogsTable
│   ├── Timestamp
│   ├── User
│   ├── Action
│   ├── Resource (what was changed)
│   ├── Changes (JSON diff)
│   └── IP Address
└── Pagination
```

#### Audit Log Schema

```typescript
// lib/types/audit.ts
export interface AuditLog {
  id: string
  timestamp: string
  userId: string
  userName: string
  action: 'login' | 'logout' | 'create' | 'update' | 'delete' | 'export'
  resource: string
  resourceId: string
  changes: Record<string, { old: any; new: any }>
  ipAddress: string
  userAgent: string
}
```

#### Implementation

**File**: `components/settings/AuditLogsTab.tsx`

```typescript
'use client'

import { useState } from 'react'
import { useQuery } from '@tanstack/react-query'
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
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from '@/components/ui/select'
import { Calendar } from '@/components/ui/calendar'
import { Popover, PopoverContent, PopoverTrigger } from '@/components/ui/popover'
import {
  Dialog,
  DialogContent,
  DialogDescription,
  DialogHeader,
  DialogTitle,
} from '@/components/ui/dialog'
import { FileDown, Calendar as CalendarIcon, Search, Eye } from 'lucide-react'
import { format } from 'date-fns'
import { toast } from 'sonner'
import type { AuditLog } from '@/lib/types/audit'

export function AuditLogsTab() {
  const [dateRange, setDateRange] = useState<{ from: Date; to: Date }>({
    from: new Date(Date.now() - 30 * 24 * 60 * 60 * 1000), // 30 days ago
    to: new Date(),
  })
  const [actionFilter, setActionFilter] = useState<string>('all')
  const [searchQuery, setSearchQuery] = useState('')
  const [selectedLog, setSelectedLog] = useState<AuditLog | null>(null)
  const [page, setPage] = useState(1)

  // Fetch audit logs
  const { data: logsData, isLoading } = useQuery({
    queryKey: ['audit-logs', dateRange, actionFilter, searchQuery, page],
    queryFn: async () => {
      const params = new URLSearchParams({
        from: dateRange.from.toISOString(),
        to: dateRange.to.toISOString(),
        page: page.toString(),
        limit: '50',
      })

      if (actionFilter !== 'all') params.set('action', actionFilter)
      if (searchQuery) params.set('search', searchQuery)

      const response = await fetch(`/api/admin/audit-logs?${params.toString()}`)
      if (!response.ok) throw new Error('Failed to fetch audit logs')
      return response.json() as Promise<{ logs: AuditLog[]; total: number; pages: number }>
    },
  })

  const exportToCSV = async () => {
    try {
      const params = new URLSearchParams({
        from: dateRange.from.toISOString(),
        to: dateRange.to.toISOString(),
        format: 'csv',
      })

      if (actionFilter !== 'all') params.set('action', actionFilter)
      if (searchQuery) params.set('search', searchQuery)

      const response = await fetch(`/api/admin/audit-logs/export?${params.toString()}`)
      if (!response.ok) throw new Error('Export failed')

      const blob = await response.blob()
      const url = window.URL.createObjectURL(blob)
      const a = document.createElement('a')
      a.href = url
      a.download = `audit-logs-${format(new Date(), 'yyyy-MM-dd')}.csv`
      a.click()
      window.URL.revokeObjectURL(url)

      toast.success('Audit logs exported successfully')
    } catch (error) {
      toast.error('Failed to export audit logs')
    }
  }

  const getActionBadgeVariant = (action: string) => {
    switch (action) {
      case 'delete':
        return 'destructive'
      case 'create':
        return 'default'
      case 'update':
        return 'secondary'
      default:
        return 'outline'
    }
  }

  return (
    <div className="space-y-6">
      {/* Header */}
      <div>
        <h2 className="text-2xl font-bold">Audit Logs</h2>
        <p className="text-gray-600">View all system activity and user actions</p>
      </div>

      {/* Filters */}
      <div className="flex flex-wrap items-center gap-4">
        {/* Date Range Picker */}
        <Popover>
          <PopoverTrigger asChild>
            <Button variant="outline" className="w-64">
              <CalendarIcon className="w-4 h-4 mr-2" />
              {format(dateRange.from, 'MMM dd')} - {format(dateRange.to, 'MMM dd, yyyy')}
            </Button>
          </PopoverTrigger>
          <PopoverContent className="w-auto p-0" align="start">
            <Calendar
              mode="range"
              selected={{ from: dateRange.from, to: dateRange.to }}
              onSelect={(range) => {
                if (range?.from && range?.to) {
                  setDateRange({ from: range.from, to: range.to })
                }
              }}
            />
          </PopoverContent>
        </Popover>

        {/* Action Filter */}
        <Select value={actionFilter} onValueChange={setActionFilter}>
          <SelectTrigger className="w-48">
            <SelectValue placeholder="Filter by action" />
          </SelectTrigger>
          <SelectContent>
            <SelectItem value="all">All Actions</SelectItem>
            <SelectItem value="login">Login</SelectItem>
            <SelectItem value="logout">Logout</SelectItem>
            <SelectItem value="create">Create</SelectItem>
            <SelectItem value="update">Update</SelectItem>
            <SelectItem value="delete">Delete</SelectItem>
            <SelectItem value="export">Export</SelectItem>
          </SelectContent>
        </Select>

        {/* Search */}
        <div className="relative flex-1">
          <Search className="absolute left-3 top-1/2 -translate-y-1/2 w-4 h-4 text-gray-400" />
          <Input
            placeholder="Search by user, resource, or IP..."
            value={searchQuery}
            onChange={(e) => setSearchQuery(e.target.value)}
            className="pl-10"
          />
        </div>

        {/* Export Button */}
        <Button variant="outline" onClick={exportToCSV}>
          <FileDown className="w-4 h-4 mr-2" />
          Export CSV
        </Button>
      </div>

      {/* Audit Logs Table */}
      <div className="border rounded-lg">
        <Table>
          <TableHeader>
            <TableRow>
              <TableHead>Timestamp</TableHead>
              <TableHead>User</TableHead>
              <TableHead>Action</TableHead>
              <TableHead>Resource</TableHead>
              <TableHead>IP Address</TableHead>
              <TableHead className="text-right">Details</TableHead>
            </TableRow>
          </TableHeader>
          <TableBody>
            {isLoading ? (
              <TableRow>
                <TableCell colSpan={6} className="text-center py-8">
                  Loading audit logs...
                </TableCell>
              </TableRow>
            ) : logsData && logsData.logs.length > 0 ? (
              logsData.logs.map((log) => (
                <TableRow key={log.id}>
                  <TableCell className="font-mono text-sm">
                    {format(new Date(log.timestamp), 'MMM dd, HH:mm:ss')}
                  </TableCell>
                  <TableCell className="font-medium">{log.userName}</TableCell>
                  <TableCell>
                    <Badge variant={getActionBadgeVariant(log.action)}>
                      {log.action}
                    </Badge>
                  </TableCell>
                  <TableCell>
                    {log.resource}
                    {log.resourceId && (
                      <span className="text-xs text-gray-500 ml-1">#{log.resourceId}</span>
                    )}
                  </TableCell>
                  <TableCell className="font-mono text-xs">{log.ipAddress}</TableCell>
                  <TableCell className="text-right">
                    <Button
                      variant="ghost"
                      size="sm"
                      onClick={() => setSelectedLog(log)}
                    >
                      <Eye className="w-4 h-4" />
                    </Button>
                  </TableCell>
                </TableRow>
              ))
            ) : (
              <TableRow>
                <TableCell colSpan={6} className="text-center py-8">
                  No audit logs found
                </TableCell>
              </TableRow>
            )}
          </TableBody>
        </Table>
      </div>

      {/* Pagination */}
      {logsData && logsData.pages > 1 && (
        <div className="flex items-center justify-between">
          <p className="text-sm text-gray-600">
            Showing page {page} of {logsData.pages} ({logsData.total} total logs)
          </p>
          <div className="flex gap-2">
            <Button
              variant="outline"
              onClick={() => setPage(page - 1)}
              disabled={page === 1}
            >
              Previous
            </Button>
            <Button
              variant="outline"
              onClick={() => setPage(page + 1)}
              disabled={page === logsData.pages}
            >
              Next
            </Button>
          </div>
        </div>
      )}

      {/* Detail Dialog */}
      <Dialog open={!!selectedLog} onOpenChange={() => setSelectedLog(null)}>
        <DialogContent className="max-w-2xl">
          <DialogHeader>
            <DialogTitle>Audit Log Details</DialogTitle>
            <DialogDescription>
              {selectedLog && format(new Date(selectedLog.timestamp), 'PPpp')}
            </DialogDescription>
          </DialogHeader>
          {selectedLog && (
            <div className="space-y-4">
              <div className="grid grid-cols-2 gap-4 text-sm">
                <div>
                  <span className="text-gray-600">User:</span>
                  <p className="font-medium">{selectedLog.userName}</p>
                </div>
                <div>
                  <span className="text-gray-600">Action:</span>
                  <p className="font-medium">{selectedLog.action}</p>
                </div>
                <div>
                  <span className="text-gray-600">Resource:</span>
                  <p className="font-medium">{selectedLog.resource}</p>
                </div>
                <div>
                  <span className="text-gray-600">IP Address:</span>
                  <p className="font-mono">{selectedLog.ipAddress}</p>
                </div>
              </div>

              {Object.keys(selectedLog.changes).length > 0 && (
                <div>
                  <h4 className="font-medium mb-2">Changes:</h4>
                  <div className="bg-gray-50 rounded-lg p-4 space-y-2">
                    {Object.entries(selectedLog.changes).map(([field, change]) => (
                      <div key={field} className="text-sm">
                        <span className="font-medium">{field}:</span>
                        <div className="flex gap-2 mt-1">
                          <div className="flex-1">
                            <span className="text-xs text-gray-500">Old:</span>
                            <p className="text-red-600">{JSON.stringify(change.old)}</p>
                          </div>
                          <div className="flex-1">
                            <span className="text-xs text-gray-500">New:</span>
                            <p className="text-green-600">{JSON.stringify(change.new)}</p>
                          </div>
                        </div>
                      </div>
                    ))}
                  </div>
                </div>
              )}

              <div className="text-xs text-gray-500">
                <span className="font-medium">User Agent:</span>
                <p className="mt-1 font-mono">{selectedLog.userAgent}</p>
              </div>
            </div>
          )}
        </DialogContent>
      </Dialog>
    </div>
  )
}
```

---

## 8. Data Schema

### Complete TypeScript Interfaces

**File**: `lib/types/user.ts`

```typescript
export type UserRole = 'closer' | 'setter' | 'office_leader' | 'regional' | 'super_admin'

export interface User {
  id: string
  name: string
  email: string
  phone: string
  role: UserRole
  quickbaseUserId: string
  office: string | null
  region: string | null
  isActive: boolean
  createdAt: string
  updatedAt: string
  lastLoginAt: string | null
}

export interface CreateUserInput {
  name: string
  email: string
  phone: string
  role: UserRole
  quickbaseUserId: string
  office?: string
  region?: string
  temporaryPassword: string
}

export interface UpdateUserInput {
  name?: string
  email?: string
  phone?: string
  role?: UserRole
  office?: string
  region?: string
  isActive?: boolean
}

export interface NotificationSettings {
  emailEnabled: boolean
  urgentAlerts: boolean
  dailyDigest: boolean
  weeklySummary: boolean
  holdThreshold: number
  ageWarningThreshold: number
  installOverdueThreshold: number
}
```

**File**: `lib/types/office.ts`

```typescript
export interface Office {
  id: string
  name: string
  region: string
  leaderId: string
  leaderName: string
  userCount: number
  activeProjects: number
  createdAt: string
  updatedAt: string
}

export interface CreateOfficeInput {
  name: string
  region: string
  leaderId: string
}

export interface UpdateOfficeInput {
  name?: string
  region?: string
  leaderId?: string
}
```

**File**: `lib/types/audit.ts`

```typescript
export type AuditAction = 'login' | 'logout' | 'create' | 'update' | 'delete' | 'export'

export interface AuditLog {
  id: string
  timestamp: string
  userId: string
  userName: string
  action: AuditAction
  resource: string
  resourceId: string
  changes: Record<string, { old: any; new: any }>
  ipAddress: string
  userAgent: string
}
```

**File**: `lib/types/settings.ts`

```typescript
export interface SystemSettings {
  quickbaseRealm: string
  quickbaseToken: string
  milestoneSLA: {
    survey: number
    design: number
    permit: number
    nem: number
    install: number
    inspection: number
    pto: number
  }
  warningThreshold: number
  criticalThreshold: number
  holdReasons: string[]
  dateFormat: 'MM/DD/YYYY' | 'DD/MM/YYYY' | 'YYYY-MM-DD'
  timezone: string
  sessionTimeout: number
}
```

---

## 9. API Endpoints

### User Management

#### `GET /api/admin/users`
**Access**: Super Admin only
**Query Parameters**:
- `search` (optional): Search by name or email
- `role` (optional): Filter by role
- `office` (optional): Filter by office

**Response**: `User[]`

---

#### `POST /api/admin/users`
**Access**: Super Admin only
**Body**: `CreateUserInput`
**Response**: `User`

**Validation**:
- Name, email, and quickbaseUserId are required
- Email must be unique
- Temporary password must be at least 8 characters

---

#### `PATCH /api/admin/users/:userId`
**Access**: Super Admin only
**Body**: `UpdateUserInput`
**Response**: `User`

---

#### `POST /api/admin/users/:userId/reset-password`
**Access**: Super Admin only
**Response**: `{ temporaryPassword: string }`

---

### Office Management

#### `GET /api/admin/offices`
**Access**: Super Admin only
**Query Parameters**:
- `search` (optional): Search by name or region

**Response**: `Office[]`

---

#### `POST /api/admin/offices`
**Access**: Super Admin only
**Body**: `CreateOfficeInput`
**Response**: `Office`

---

#### `PATCH /api/admin/offices/:officeId`
**Access**: Super Admin only
**Body**: `UpdateOfficeInput`
**Response**: `Office`

---

#### `DELETE /api/admin/offices/:officeId`
**Access**: Super Admin only
**Response**: `{ success: boolean }`

---

### User Profile

#### `GET /api/user/profile`
**Access**: All authenticated users
**Response**: `User`

---

#### `PUT /api/user/profile`
**Access**: All authenticated users
**Body**: `{ name: string; email: string; phone: string }`
**Response**: `User`

---

#### `PUT /api/user/password`
**Access**: All authenticated users
**Body**: `{ currentPassword: string; newPassword: string }`
**Response**: `{ success: boolean }`

---

#### `GET /api/user/notifications`
**Access**: All authenticated users
**Response**: `NotificationSettings`

---

#### `PUT /api/user/notifications`
**Access**: All authenticated users
**Body**: `NotificationSettings`
**Response**: `NotificationSettings`

---

#### `POST /api/user/test-notification`
**Access**: All authenticated users
**Response**: `{ success: boolean }`

---

### System Settings

#### `GET /api/admin/system/settings`
**Access**: Super Admin only
**Response**: `SystemSettings`

---

#### `PUT /api/admin/system/settings`
**Access**: Super Admin only
**Body**: `Partial<SystemSettings>`
**Response**: `SystemSettings`

---

#### `POST /api/admin/system/test-connection`
**Access**: Super Admin only
**Body**: `{ realm: string; token: string }`
**Response**: `{ success: boolean; message: string }`

---

### Audit Logs

#### `GET /api/admin/audit-logs`
**Access**: Super Admin only
**Query Parameters**:
- `from` (required): Start date (ISO string)
- `to` (required): End date (ISO string)
- `action` (optional): Filter by action type
- `search` (optional): Search by user, resource, or IP
- `page` (optional): Page number (default: 1)
- `limit` (optional): Results per page (default: 50)

**Response**: `{ logs: AuditLog[]; total: number; pages: number }`

---

#### `GET /api/admin/audit-logs/export`
**Access**: Super Admin only
**Query Parameters**: Same as GET /api/admin/audit-logs
**Additional**: `format=csv`

**Response**: CSV file download

---

## 10. Implementation Prompts

### For Traycer/Cursor: Phase 5A (Basic Settings)

```
Build the Settings page Phase 5A: Basic Settings (Profile + Notifications).

CONTEXT:
- Main settings page route: /app/(dashboard)/settings/page.tsx
- This is Phase 5A focusing on essential settings for all users
- Use role-based access control from session
- Reference existing: HoldManagementCard for mutation patterns

COMPONENTS TO CREATE:

1. app/(dashboard)/settings/page.tsx
   - Server component that fetches session
   - Redirects if not authenticated
   - Renders <Tabs> component with conditional tabs based on role
   - All users see: Profile, Notifications
   - Office leaders+ see: Team (future)
   - Super admin sees: Users, Offices, System, Quickbase, Audit (future phases)

2. components/settings/ProfileTab.tsx
   - Copy implementation from docs/SETTINGS-SPEC.md lines 134-406
   - Displays user profile with avatar placeholder
   - Editable fields: name, email, phone
   - Read-only fields: quickbaseUserId, role, office
   - Password change section
   - Danger zone (sign out all sessions)
   - Uses TanStack Query mutations for updates
   - Toast notifications for success/error

3. components/settings/NotificationsTab.tsx
   - Copy implementation from docs/SETTINGS-SPEC.md lines 430-676
   - Email notification toggles (master, urgent, daily, weekly)
   - Alert thresholds (visible to office_leader+)
   - Test notification button
   - Uses TanStack Query for settings updates

4. lib/types/user.ts
   - Export User interface (if not exists)
   - Export NotificationSettings interface

API ENDPOINTS TO CREATE:

1. app/api/user/profile/route.ts
   - GET: Return current user from session
   - PUT: Update user profile (name, email, phone)
   - Validate email uniqueness
   - Return updated user object

2. app/api/user/password/route.ts
   - PUT: Change password
   - Verify current password
   - Hash new password with bcrypt
   - Return success message

3. app/api/user/notifications/route.ts
   - GET: Return user's notification settings
   - PUT: Update notification settings
   - Return updated settings

4. app/api/user/test-notification/route.ts
   - POST: Send test email to user
   - Use existing email service
   - Return success message

STYLING:
- Use shadcn/ui components (Card, Input, Button, Badge, Switch, Tabs)
- Responsive: mobile-first with iPad Pro breakpoints
- Match existing HoldManagementCard visual style

ACCEPTANCE CRITERIA:
- All users can update their profile
- All users can change their password
- Office leaders+ can configure alert thresholds
- All users can send test email notification
- Toast notifications work for all actions
- Optimistic updates where appropriate
- Form validation prevents empty submissions
```

---

### For Traycer/Cursor: Phase 5B (User Management)

```
Build the Settings page Phase 5B: User Management (Users + Offices tabs).

CONTEXT:
- This is Phase 5B focusing on admin features for super_admin role only
- Extends Phase 5A settings page
- Uses role-based access control from session

COMPONENTS TO CREATE:

1. components/settings/UsersTab.tsx
   - Copy implementation from docs/SETTINGS-SPEC.md lines 756-1124
   - Table view of all users with search and filters
   - Add new user dialog with form
   - Toggle active/inactive status
   - Reset password button (generates temp password)
   - Edit user button (future enhancement)
   - Role badge color coding

2. components/settings/OfficesTab.tsx
   - Copy implementation from docs/SETTINGS-SPEC.md lines 1145-END of OfficesTab
   - Grid view of office cards
   - Add new office dialog
   - Edit office details
   - Delete office (with confirmation)
   - Show office stats: leader, user count, active projects

3. lib/types/office.ts
   - Export Office interface
   - Export CreateOfficeInput interface
   - Export UpdateOfficeInput interface

API ENDPOINTS TO CREATE:

1. app/api/admin/users/route.ts
   - GET: List all users with filters (search, role, office)
   - POST: Create new user
   - Validate super_admin access
   - Generate temporary password
   - Send welcome email

2. app/api/admin/users/[userId]/route.ts
   - PATCH: Update user (name, email, role, office, isActive)
   - Validate super_admin access
   - Prevent self-deactivation

3. app/api/admin/users/[userId]/reset-password/route.ts
   - POST: Generate new temporary password
   - Send password reset email
   - Return temporary password (for display once)

4. app/api/admin/offices/route.ts
   - GET: List all offices with search
   - POST: Create new office
   - Validate super_admin access

5. app/api/admin/offices/[officeId]/route.ts
   - PATCH: Update office details
   - DELETE: Delete office (check for assigned users first)
   - Validate super_admin access

MIDDLEWARE:
- Add middleware to check super_admin role for all /api/admin/* routes
- Return 403 Forbidden if not authorized

ACCEPTANCE CRITERIA:
- Only super_admin can access Users and Offices tabs
- Can create, update, and deactivate users
- Can reset user passwords
- Can create, update, and delete offices
- Deletion blocked if office has assigned users
- All actions have confirmation dialogs for destructive operations
- Search and filters work correctly
```

---

### For Traycer/Cursor: Phase 5C (System Configuration)

```
Build the Settings page Phase 5C: System Configuration (System tab).

CONTEXT:
- This is Phase 5C focusing on advanced system settings
- Super admin only
- Configures Quickbase connection, SLA defaults, hold reasons

COMPONENTS TO CREATE:

1. components/settings/SystemTab.tsx
   - Copy full implementation from docs/SETTINGS-SPEC.md SystemTab section
   - Quickbase connection settings with test button
   - Milestone SLA defaults (7 milestones)
   - Warning/critical threshold percentages
   - Hold reasons management (add/remove custom reasons)
   - General settings (date format, timezone, session timeout)

2. lib/types/settings.ts
   - Export SystemSettings interface

API ENDPOINTS TO CREATE:

1. app/api/admin/system/settings/route.ts
   - GET: Return current system settings
   - PUT: Update system settings
   - Validate super_admin access
   - Store in database or config file

2. app/api/admin/system/test-connection/route.ts
   - POST: Test Quickbase connection
   - Validate realm and token
   - Attempt API call to Quickbase
   - Return success/failure

DATABASE:
- Create system_settings table (or use JSON config file)
- Schema: id, quickbase_realm, quickbase_token (encrypted), milestone_sla (JSON), thresholds (JSON), hold_reasons (JSON), general_settings (JSON)
- Seed with defaults on first run

ACCEPTANCE CRITERIA:
- Only super_admin can access System tab
- Quickbase connection test works correctly
- All settings persist to database
- SLA defaults apply to new projects
- Custom hold reasons appear in hold dropdown throughout app
- Changes to date format/timezone apply globally
```

---

### For Traycer/Cursor: Phase 5D (Audit Logs)

```
Build the Settings page Phase 5D: Audit Logs tab.

CONTEXT:
- This is Phase 5D focusing on audit trail and compliance
- Super admin only
- Tracks all user actions in the system

COMPONENTS TO CREATE:

1. components/settings/AuditLogsTab.tsx
   - Copy full implementation from docs/SETTINGS-SPEC.md AuditLogsTab section
   - Table view with filters (date range, action type, search)
   - Pagination (50 per page)
   - Detail dialog showing JSON diff of changes
   - Export to CSV button

2. lib/types/audit.ts
   - Export AuditLog interface
   - Export AuditAction type

API ENDPOINTS TO CREATE:

1. app/api/admin/audit-logs/route.ts
   - GET: List audit logs with filters
   - Query parameters: from, to, action, search, page, limit
   - Return paginated results
   - Validate super_admin access

2. app/api/admin/audit-logs/export/route.ts
   - GET: Export audit logs as CSV
   - Same filters as list endpoint
   - Return CSV file download
   - Validate super_admin access

AUDIT LOGGING MIDDLEWARE:
Create lib/middleware/auditLogger.ts
- Function: logAudit(userId, action, resource, resourceId, changes, req)
- Captures: timestamp, user, action, resource, changes (JSON diff), IP, user agent
- Stores in audit_logs table
- Called from all mutations (create, update, delete)

DATABASE:
- Create audit_logs table
- Schema: id, timestamp, user_id, user_name, action, resource, resource_id, changes (JSON), ip_address, user_agent
- Index on: timestamp, user_id, action, resource
- Retention policy: 1 year (configurable)

INTEGRATION POINTS:
Add audit logging to:
- User login/logout
- Project hold/release
- User CRUD operations
- Office CRUD operations
- System settings changes
- Data exports

ACCEPTANCE CRITERIA:
- All user actions logged automatically
- Audit logs searchable and filterable
- CSV export works with all filters
- Detail view shows before/after changes
- Logs retained for configured period
- Only super_admin can view logs
```

---

## Summary

This complete Settings specification provides Traycer with:

1. **Phase 5A** (2-3 hours): Basic profile and notification settings for all users
2. **Phase 5B** (3-4 hours): User and office management for super admins
3. **Phase 5C** (4-5 hours): System configuration for Quickbase, SLAs, and general settings
4. **Phase 5D** (3-4 hours): Audit logs for compliance and tracking

**Total Implementation Time**: 12-16 hours (or 4-6 hours with AI assistance)

**Recommended Build Order**:
1. Phase 5A first (essential for all users)
2. Phase 5B second (enables team management)
3. Phase 5C third (system configuration)
4. Phase 5D last (compliance feature)

All components include:
- Full TypeScript implementations
- TanStack Query for data fetching
- Optimistic updates where appropriate
- Error handling and validation
- Toast notifications
- Role-based access control
- Responsive design for iPad Pro
